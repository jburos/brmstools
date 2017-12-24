#' Forest plots for brmsfit objects
#'
#' This function draws a forest plot for a brmsfit model;
#' especially useful for random-effects meta-analytic models.
#'
#' @param model A brmsfit object.
#' @param level The "Confidence" level for the Credible Intervals.
#' Defaults to 0.95.
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
#'
#' @importFrom stats coef
#' @export
forest <- function(model,
                   level = .95,
                   sort = TRUE,
                   show_data = FALSE,
                   col_ridge = NA,
                   density = TRUE,
                   text = TRUE,
                   rel_min_height = .01,
                   scale = 0.9,
                   fill_ridge = "grey75",
                   digits = 2) {

  # Requires the ggridges package
  if (!requireNamespace("ggridges", quietly = TRUE)) {
    stop(
      "ggridges package needed for this function. Please install it.",
      call. = FALSE
    )
  }

  limits <- c(.5 - level / 2, .5 + level / 2)
  grouping <- unique(model$ranef$group)
  if (length(grouping) > 1) stop("More than 1 grouping factor.", call. = F)
  parameters <- dimnames(coef(model)[[grouping]])[[3]]

  out <- vector("list", length(parameters))
  names(out) <- parameters
  for (parameter in parameters) {

    # Posterior samples
    # Varying
    samples_r <- coef(model, summary = FALSE)[[grouping]][, , parameter]
    samples_r <- tidyr::gather_(
      as.data.frame(samples_r),
      key_col = grouping,
      value_col = parameter,
      gather_cols = as.character(model$data[[grouping]])
    )
    # Population-level
    samples_f <- as.data.frame(fixef(model, summary = FALSE))
    samples_f[[grouping]] <- "Average"
    samples_f <- samples_f[, c(grouping, parameter)]
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
    sum_f[[grouping]] <- "Average"
    # Take current parameter
    sum_f <- sum_f[rownames(sum_f) == parameter, ]
    # Combine
    samples_sum <- rbind(sum_r, sum_f, make.row.names = F)
    samples_sum[["Interval"]] <- paste0(
      round(samples_sum[["Estimate"]], digits),
      " [",
      round(samples_sum[[lwr]], digits),
      ", ",
      round(samples_sum[[upr]], digits), "]"
    )

    # Order effects
    if (sort) samples_sum <- dplyr::arrange_(samples_sum, "Estimate")
    samples_sum[["order"]] <- 1:nrow(samples_sum)
    # Put ME in bottom
    samples_sum[["order"]] <- ifelse(samples_sum[[grouping]] == "Average",
      -.5,
      samples_sum[["order"]]
    )
    dplyr::left_join(
      samples,
      samples_sum[, c(grouping, "order")],
      by = grouping
    )
    samples_sum[[grouping]] <- stats::reorder(
      samples_sum[[grouping]],
      as.numeric(samples_sum[["order"]])
    )

    # Create graph
    g <- ggplot(samples_sum, aes_string(parameter, grouping)) +
      geom_point(aes_string(x = "Estimate"))
    if (density) {
      g <- g + ggridges::geom_density_ridges(
        data = samples,
        rel_min_height = rel_min_height,
        scale = scale,
        col = col_ridge,
        fill = fill_ridge
      ) +
        geom_point(aes_string(x = "Estimate"))
    }
    g <- g + geom_segment(
      aes_(
        y = as.name(grouping),
        yend = as.name(grouping),
        x = as.name(lwr),
        xend = as.name(upr)
      )
    )
    if (text) {
      g <- g +
        geom_text(
          data = samples_sum[samples_sum[[grouping]] == "Average", ],
          aes_string(label = "Interval", x = "Inf"),
          hjust = "inward", size = 3, fontface = "bold"
        ) +
        geom_text(
          data = samples_sum[samples_sum[[grouping]] != "Average", ],
          aes_string(label = "Interval", x = "Inf"),
          hjust = "inward", size = 3
        )
    }
    if (show_data & length(parameters) == 1) {
      g <- g + geom_point(
        data = model$data,
        aes_string(
          attr(
            attr(model$data, "terms"),
            "term.labels"
          )[1],
          grouping
        ),
        shape = 8
      )
    }
    out[[parameter]] <- g
  }
  out
}
