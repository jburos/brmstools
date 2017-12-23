#' Meta-analysis forest plots from brms output
#'
#' This function draws a forest plot from a random-effects meta-analysis
#' model fitted with brms.
#'
#' @param model A meta-analytic model estimated with brms.
#' @param level The "Confidence" level for the Credible Intervals.
#' Defaults to 0.95.
#' @param show_data Logical; whether to show the observed effect size
#' and standard error below the meta-analytic estimates. Defaults to FALSE.
#' @param sort Logical; whether to sort the estimates in ascending
#' order of magnitude from bottom to top. Defaults to FALSE.
#' @param fill_ridge String; color to fill the densities. Defaults to "grey65".
#' @param col_ridge String; color for the outlines of the densities. Default: NA.
#'
#' @details
#'
#' \subsection{Requirements for brms model}{
#'
#' The meta-analytic model must be fitted with the following brms formula:
#' \code{yi | se(sei) ~ 1 + (1|study)}
#'
#' }
#'
#' @examples
#' \dontrun{
#' ## Use a data frame called d
#' fit <- brm(yi | se(sei) ~ 1 + (1|study), data = d)
#' forest(fit)
#' }
#' @importFrom stats coef
#' @export
forest <- function(model,
                   level = .95,
                   sort = TRUE,
                   show_data = FALSE,
                   col_ridge = NA,
                   fill_ridge = "grey65") {

  # Requires the qgraph package
  if (!requireNamespace("ggridges", quietly = TRUE)) {
    stop(
      "ggridges package needed for this function. Please install it.",
      call. = FALSE
    )
  }

  grouping <- model$ranef$group
  parameter <- model$ranef$coef
  limits <- c(.5 - level / 2, .5 + level / 2)

  # Obtain samples of each varying intercept
  samples_r <- coef(model, summary = FALSE)[[grouping]][, , parameter]
  samples_r <- tidyr::gather_(
    as.data.frame(samples_r),
    key_col = grouping,
    value_col = parameter,
    gather_cols = model$data[[grouping]]
  )

  # Add samples of population-level intercept
  samples_f <- as.data.frame(fixef(model, summary = FALSE))
  samples_f[[grouping]] <- "ME"

  # Combine
  samples <- rbind(samples_f, samples_r)

  # Summaries
  sum_r <- coef(model, probs = limits)[[grouping]][, , parameter]
  sum_r <- as.data.frame(sum_r)
  sum_r[[grouping]] <- row.names(sum_r)
  sum_f <- fixef(model, probs = limits)
  # Limit labels
  lwr <- colnames(sum_f)[3]
  upr <- colnames(sum_f)[4]
  sum_f <- as.data.frame(sum_f)
  sum_f[[grouping]] <- "ME"
  samples_sum <- rbind(sum_r, sum_f)
  samples_sum[["Interval"]] <- paste0(
    round(samples_sum[["Estimate"]], 2),
    " [",
    round(samples_sum[[lwr]], 2),
    ", ",
    round(samples_sum[[upr]], 2), "]"
  )

  # Order effects
  if (sort) samples_sum <- dplyr::arrange(samples_sum, Estimate)
  samples_sum[["order"]] <- 1:nrow(samples_sum)
  # Put ME in bottom
  samples_sum[["order"]] <- ifelse(samples_sum[[grouping]] == "ME",
                                   -1,
                                   samples_sum[["order"]])
  samples_sum[[grouping]] <- stats::reorder(
    samples_sum[[grouping]],
    as.numeric(samples_sum[["order"]])
  )

  # Create graph
  g <- ggplot(samples_sum, aes_string(parameter, grouping)) +
    geom_text(
      data = samples_sum,
      aes(label = Interval, x = max(samples[[parameter]])),
      hjust = "right", size = 3
    ) +
    ggridges::geom_density_ridges(data = samples,
                                  rel_min_height = 0.01,
                                  scale = 0.95,
                                  col = col_ridge,
                                  fill = fill_ridge) +
    geom_point(data = samples_sum, aes(Estimate)) +
    geom_segment(
      data = samples_sum,
      aes_(
        y = as.name(grouping),
        yend = as.name(grouping),
        x = as.name(lwr),
        xend = as.name(upr)
      )
  )
  if (show_data) {
    g <- g + geom_point(data = model$data,
                        aes_string(attr(attr(model$data, "terms"),
                                        "term.labels")[1],
                                   grouping),
                        shape = 8)
  }
  g
}
