#' Forest plots for brmsfit objects
#'
#' This function draws a forest plot for a brmsfit model;
#' especially useful for random-effects meta-analytic models.
#'
#' @param model A brmsfit object.
#' @param pars Parameters to plot, defaults to all (NA).
#' @param level The "Confidence" level for the Credible Intervals.
#' Defaults to 0.95.
#' @param av_name Name of average parameter (e.g. `"Meta-Analytic\\nestimate"`)
#' @param show_data Logical; whether to show the observed effect size
#' and standard error below the meta-analytic estimates. Defaults to FALSE.
#' @param sort Logical; whether to sort the estimates in ascending
#' order of magnitude from bottom to top. Defaults to FALSE.
#' @param fill_ridge String; color to fill the densities. Defaults to "grey65".
#' @param col_ridge String; color for the outlines of the densities. Default: NA.
#' @param density Logical; should densities be shown?
#' @param text Logical; should estimates' numerical summaries be included?
#' @param rel_min_height Passed to [ggridges::geom_density_ridges()].
#' @param scale Passed to [ggridges::geom_density_ridges()].
#' @param digits Digits to display in numerical summaries.
#' @param theme_forest Use [brmstools::theme_forest()] ggplot2 theme?
#'
#' @return a ggplot
#'
#' @importFrom stats coef
#' @export
forest <- function(model,
                   pars = NA,
                   level = .95,
                   av_name = "Average",
                   sort = TRUE,
                   show_data = FALSE,
                   col_ridge = NA,
                   fill_ridge = "grey75",
                   density = TRUE,
                   text = TRUE,
                   rel_min_height = .01,
                   scale = 0.9,
                   digits = 2,
                   theme_forest = TRUE) {

  # Requires the ggridges package
  if (!requireNamespace("ggridges", quietly = TRUE)) {
    stop(
      "ggridges package needed for this function. Please install it.",
      call. = FALSE
    )
  }

  grouping <- unique(model$ranef$group)
  if (length(grouping) > 1) stop("More than 1 grouping factor.", call. = F)
  probs <- c(.5 - level / 2, .5 + level / 2)
  lwr <- paste0(probs[1]*100, "%ile")
  upr <- paste0(probs[2]*100, "%ile")

  samples <- tidycoef(model, pars=pars)
  samples_sum <- tidycoef(model, pars=pars, summary = T, level = level)

  # Rename average effects
  samples[[grouping]] <- ifelse(is.na(samples[[grouping]]),
                                av_name,
                                samples[[grouping]])
  samples_sum[[grouping]] <- ifelse(is.na(samples_sum[[grouping]]),
                                    av_name,
                                    samples_sum[[grouping]])
  # Create text intervals
  samples_sum[["Interval"]] <- paste0(
    round(samples_sum[["Estimate"]], digits),
    " [",
    round(samples_sum[[lwr]], digits),
    ", ",
    round(samples_sum[[upr]], digits), "]"
  )
  # Order effects
  if (sort) samples_sum <- dplyr::arrange_(samples_sum, "type", "Parameter", "Estimate")
  samples_sum[["order"]] <- forcats::fct_inorder(
    paste0(samples_sum[["type"]],
           samples_sum[[grouping]],
           samples_sum[["Parameter"]])
  )
  samples <- dplyr::left_join(
    samples,
    samples_sum[, c(grouping, "Parameter", "order")],
    by = c(grouping, "Parameter")
  )
  # Create graph
  g <- ggplot(samples_sum, aes_string("Estimate", "order")) +
    scale_y_discrete(labels = samples_sum[[grouping]],
                     breaks = samples_sum[["order"]]) +
    geom_point()
  if (density) {
    g <- g + ggridges::geom_density_ridges(
      data = samples,
      aes_string(x="value"),
      rel_min_height = rel_min_height,
      scale = scale,
      col = col_ridge,
      fill = fill_ridge
    ) +
      geom_point()
  }
  g <- g + geom_segment(
    aes_(
      y = ~order,
      yend = ~order,
      x = as.name(lwr),
      xend = as.name(upr)
    )
  )
  if (text) {
    g <- g +
      geom_text(
        data = samples_sum[samples_sum[["type"]] == "b", ],
        aes_string(label = "Interval", x = "Inf"),
        hjust = "inward", size = 3, fontface = "bold"
      ) +
      geom_text(
        data = samples_sum[samples_sum[["type"]] == "r", ],
        aes_string(label = "Interval", x = "Inf"),
        hjust = "inward", size = 3
      )
  }
  if (show_data & length(unique(samples_sum[["Parameter"]])) == 1) {
    tmp <- dplyr::left_join(model$data, samples_sum[, c(grouping, "order")])
    g <- g + geom_point(
      data = tmp,
      aes_string(
        attr(
          attr(model$data, "terms"),
          "term.labels"
        )[1],
        "order"
      ),
      shape = 8
    )
  }
  g <- g + facet_wrap("Parameter", scales="free", strip.position = "bottom")
  if (theme_forest) g <- g + theme_forest()
  g
}
