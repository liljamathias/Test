# Interactive Kaplan-Meier survival curves.
#
# Returns a plotly widget: hovering on the curve reveals the point estimate
# S(t) and its 95% CI at each step vertex. Designed to be dropped straight
# into a Shiny app via plotlyOutput / renderPlotly.

#' Interactive Kaplan-Meier plot
#'
#' @param data A data frame.
#' @param time Column name (string) of follow-up time.
#' @param event Column name (string) of the 0/1 event indicator (1 = event, 0 = censored).
#' @param strata Optional column name (string) to stratify by. If NULL, a single curve is drawn.
#' @param conf.level Confidence level for the CI ribbon (default 0.95).
#' @param xlab,ylab Axis labels.
#' @param risk_table If TRUE, a number-at-risk table is stacked below the plot.
#' @return A plotly htmlwidget.
interactive_km <- function(data,
                           time,
                           event,
                           strata     = NULL,
                           conf.level = 0.95,
                           xlab       = "Time",
                           ylab       = "Survival probability",
                           risk_table = TRUE) {

  stopifnot(is.data.frame(data), is.character(time), is.character(event))
  has_strata <- !is.null(strata) && nzchar(strata)

  df <- data
  if (has_strata) {
    df[[strata]] <- as.factor(df[[strata]])
    rhs <- strata
  } else {
    rhs <- "1"
  }

  fit_formula <- stats::as.formula(
    sprintf("survival::Surv(`%s`, `%s`) ~ %s", time, event, rhs)
  )
  fit <- survival::survfit(fit_formula, data = df, conf.int = conf.level)

  tidy <- ggsurvfit::tidy_survfit(fit)
  # tidy_survfit columns: time, n.risk, n.event, n.censor, estimate,
  # std.error, conf.low, conf.high, strata (if stratified)

  if (has_strata) {
    tidy$.group <- factor(tidy$strata)
    tidy$tooltip <- sprintf(
      "Time: %.2f\nS(t): %.3f\n95%% CI: [%.3f, %.3f]\nGroup: %s",
      tidy$time, tidy$estimate, tidy$conf.low, tidy$conf.high, as.character(tidy$.group)
    )
  } else {
    tidy$.group <- factor("All")
    tidy$tooltip <- sprintf(
      "Time: %.2f\nS(t): %.3f\n95%% CI: [%.3f, %.3f]",
      tidy$time, tidy$estimate, tidy$conf.low, tidy$conf.high
    )
  }

  p <- ggplot2::ggplot(
    tidy,
    ggplot2::aes(x = time, y = estimate, group = .group, colour = .group, fill = .group)
  ) +
    pammtools::geom_stepribbon(
      ggplot2::aes(ymin = conf.low, ymax = conf.high),
      alpha = 0.2, colour = NA
    ) +
    ggplot2::geom_step(linewidth = 0.9) +
    ggplot2::geom_point(
      ggplot2::aes(text = tooltip),
      size = 0.01, alpha = 0
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1), labels = scales::percent_format(accuracy = 1)
    ) +
    ggplot2::labs(x = xlab, y = ylab, colour = NULL, fill = NULL) +
    ggplot2::theme_minimal()

  if (!has_strata) {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  km_plotly <- plotly::ggplotly(p, tooltip = "text")

  if (!risk_table) return(km_plotly)

  breaks <- pretty(range(tidy$time, na.rm = TRUE), n = 8)
  risk_df <- lapply(split(tidy, tidy$.group), function(g) {
    g <- g[order(g$time), ]
    n_at <- vapply(breaks, function(b) {
      idx <- which(g$time <= b)
      if (length(idx) == 0) max(g$n.risk, na.rm = TRUE) else g$n.risk[max(idx)]
    }, numeric(1))
    data.frame(group = unique(as.character(g$.group)), time = breaks, n.risk = n_at)
  })
  risk_df <- do.call(rbind, risk_df)
  risk_df$group <- factor(risk_df$group, levels = levels(tidy$.group))

  rt <- ggplot2::ggplot(
    risk_df,
    ggplot2::aes(x = time, y = group, label = n.risk, colour = group)
  ) +
    ggplot2::geom_text(size = 3.5) +
    ggplot2::scale_y_discrete(limits = rev(levels(risk_df$group))) +
    ggplot2::labs(x = xlab, y = NULL, title = "Number at risk") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      panel.grid      = ggplot2::element_blank(),
      plot.title      = ggplot2::element_text(size = 10)
    )

  risk_plotly <- plotly::ggplotly(rt, tooltip = NULL)

  plotly::subplot(
    km_plotly, risk_plotly,
    nrows   = 2,
    heights = c(0.75, 0.25),
    shareX  = TRUE,
    titleY  = TRUE
  )
}
