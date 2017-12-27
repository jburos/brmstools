#' Panel plots for brmsfit models
#'
#' Panel plots display fitted values for each grouping factor in a
#' separate panel.
#'
#' @param model A brmsfit model
#' @param level For limits of credible intervals
#' @param data Should data be shown?
#' @param sort Sort panels based on a variable
#' @param xvar What variable should be on the x-axis
#' @param line_col Line color
#' @param line_lty Line type
#' @param ribbon_col Ribbon color
#' @param ribbon_fill Ribbon fill
#' @param ribbon_alpha Ribbon transparency
#' @param ... Passed to [tidyfitted()]
#'
#' @return a ggplot
#' @export
panels <- function(model,
                   level = .95,
                   data = T,
                   sort = NA,
                   xvar = NA,
                   line_col = "black",
                   line_lty = 1,
                   ribbon_col = NA,
                   ribbon_fill = "grey60",
                   ribbon_alpha = .2,
                   ...) {

  d <- tidyfitted(model, level = level, ...)
  d <- d[d[["type"]]=="r",]
  grouping <- unique(model$ranef$group)

  # Order panels on desired parameter
  if (!is.na(sort)) {
    cfs <- tidycoef(model, pars = sort, level = .95, summary=T)
    cfs <- cfs[cfs[["type"]]=="r",]
    cfs <- dplyr::arrange_(cfs, "type", "Parameter", "Estimate")
    cfs[["order"]] <- forcats::fct_inorder(cfs[[grouping]])
    d <- dplyr::left_join(
      d,
      cfs[, c(grouping, "order")],
      by = grouping
    )
    d[[grouping]] <- d[["order"]]
  }

  probs <- c(.5 - level / 2, .5 + level / 2)
  lwr <- paste0(probs[1]*100, "%ile")
  upr <- paste0(probs[2]*100, "%ile")
  g <- ggplot(d, aes_string(x=xvar, y="Estimate")) +
    geom_ribbon(aes_(ymin=as.name(lwr), ymax=as.name(upr)),
                alpha = ribbon_alpha,
                col = ribbon_col,
                fill = ribbon_fill) +
    geom_line(col = line_col, lty = line_lty) +
    facet_wrap(grouping)

  if (data) {
    resp <- all.vars(model$formula$formula)[[1]]
    g <- g + geom_point(data=model$data, aes_string(x = xvar, y = resp))
  }
  g
}
