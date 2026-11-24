# -----------------------------------------------------------------------------
# Program    : hta_subgroups.R
# Study      : HTA part (hta/), any ADTTE-like data
# Purpose    : One call that returns the subgroup table and the forest plot
#              for any time-to-event parameter: hazard ratio per subgroup
#              level, and the likelihood-ratio test of treatment-by-subgroup
#              interaction for each subgroup variable
# Inputs     : a data frame with PARAMCD, AVAL, CNSR, a treatment variable
#              and the subgroup variables
# Outputs    : function hta_subgroups(), returning a list with
#              $overall (one row), $table (one row per subgroup level) and
#              $plot (ggplot forest plot)
# Author     : Ignacio Garcia Ribelles
# Created    : 2026-11-25
# Change log : 2026-11-25  IGR  Initial version
# Notes      : Cox model with treatment only within each level (Efron ties,
#              the survival::coxph default). {tern} offers the same analysis
#              in extract_survival_subgroups() and g_forest().
#              tests/testthat/test-hta-subgroups.R checks this function
#              against direct coxph() calls and against {tern}.
#              IQWiG General Methods 7.0, section 9.3.9: subgroup results
#              need at least 10 patients per subgroup and at least 10 events
#              in one of the subgroups. The table flags levels below that.
# -----------------------------------------------------------------------------

hta_subgroups <- function(data, paramcd, active, control = "Placebo",
                          subgroups, trt_var = "TRTA", time = "AVAL",
                          cnsr = "CNSR", ref_line_label = "") {
  stopifnot(is.character(paramcd), length(paramcd) == 1,
            all(names(subgroups) %in% names(data)))
  d <- data[data$PARAMCD == paramcd & data[[trt_var]] %in% c(control, active), ]
  if (nrow(d) == 0) stop("No records for PARAMCD = ", paramcd, " and the two arms.")
  d$.time  <- d[[time]]
  d$.event <- 1 - d[[cnsr]]
  d$.act   <- as.integer(d[[trt_var]] == active)

  # Hazard ratio (active vs control) from a Cox model with treatment only.
  fit_hr <- function(x) {
    if (sum(x$.event) == 0 || length(unique(x$.act)) < 2) {
      return(c(hr = NA, lo = NA, hi = NA))
    }
    ci <- summary(survival::coxph(survival::Surv(.time, .event) ~ .act, data = x))$conf.int
    c(hr = ci[1, 1], lo = ci[1, 3], hi = ci[1, 4])
  }
  count_row <- function(x) {
    data.frame(n = nrow(x),
               n_active = sum(x$.act == 1), events_active = sum(x$.event[x$.act == 1]),
               n_control = sum(x$.act == 0), events_control = sum(x$.event[x$.act == 0]))
  }

  overall <- cbind(data.frame(variable = "All patients", level = ""),
                   count_row(d), t(fit_hr(d)), p_int = NA)

  tab <- do.call(rbind, lapply(names(subgroups), function(v) {
    x <- d[!is.na(d[[v]]), ]
    x$.sub <- factor(x[[v]])
    # Likelihood-ratio test of the treatment-by-subgroup interaction.
    m0 <- survival::coxph(survival::Surv(.time, .event) ~ .act + .sub, data = x)
    m1 <- survival::coxph(survival::Surv(.time, .event) ~ .act * .sub, data = x)
    p_int <- stats::anova(m0, m1)[2, "Pr(>|Chi|)"]
    do.call(rbind, lapply(levels(x$.sub), function(l) {
      xl <- x[x$.sub == l, ]
      cbind(data.frame(variable = subgroups[[v]], level = l), count_row(xl),
            t(fit_hr(xl)), p_int = p_int)
    }))
  }))
  # IQWiG 9.3.9: at least 10 patients per level and at least 10 events in
  # one of the levels of the same subgroup variable.
  ev <- tab$events_active + tab$events_control
  max_ev <- stats::ave(ev, tab$variable, FUN = max)
  tab$iqwig_size_ok <- tab$n >= 10 & max_ev >= 10
  rownames(tab) <- NULL

  forest <- rbind(overall[, c("variable", "level", "hr", "lo", "hi")],
                  tab[, c("variable", "level", "hr", "lo", "hi")])
  forest$label <- ifelse(forest$level == "", forest$variable,
                         paste0(forest$variable, ": ", forest$level))
  forest$label <- factor(forest$label, levels = rev(forest$label))
  forest$overall <- forest$level == ""

  plot <- ggplot2::ggplot(forest, ggplot2::aes(hr, label)) +
    ggplot2::geom_vline(xintercept = 1, colour = "grey40") +
    ggplot2::geom_vline(xintercept = overall$hr, linetype = "dashed", colour = "#D55E00") +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = lo, xmax = hi), width = 0.2, orientation = "y",
                           na.rm = TRUE) +
    ggplot2::geom_point(ggplot2::aes(shape = overall), size = 2.5, na.rm = TRUE) +
    ggplot2::scale_shape_manual(values = c(`FALSE` = 15, `TRUE` = 18), guide = "none") +
    ggplot2::scale_x_log10() +
    ggplot2::labs(x = paste0("Hazard ratio, ", active, " vs ", control, " (log scale)"),
                  y = NULL, title = paste0("PARAMCD = ", paramcd),
                  caption = paste("Dashed line: overall hazard ratio.", ref_line_label)) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   plot.caption = ggplot2::element_text(hjust = 0, colour = "grey40"))

  list(overall = overall, table = tab, plot = plot)
}
