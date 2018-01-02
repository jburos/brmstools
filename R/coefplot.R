#' Coefficient plots for brms models
#'
#' A more traditional version of [bayesplot::mcmc_intervals()], additionally
#' showing parameter means (and CIs) for each grouping factor.
#'
#' @param model A brmsfit model
#' @param grouping Name of grouping variable (e.g. `(1 | grouping)`). Defaults
#' to `NA` which returns the unique / first grouping factor in model.
#' @param level For limits of credible intervals
#' @param pars Which parameters to show (defaults to NA (all parameters)).
#' @param type Show population level ("b"), group-specific ("r"), or
#' both types ("br", default) of parameters.
#' @param interval_col Color of intervals
#' @param interval_h Height of bars at credible interval limits
#' @param point_col Color of points
#' @param point_size Size of points
#' @param r_col Color of points for group-specific posterior means
#' @param r_intervals Show credible intervals for group-specific parameters?
#' @param r_alpha Transparency of group-specific estimates
#' @param ... Passed to [tidyfitted()]
#'
#' @return a ggplot
#' @export
coefplot <- function(model,
                     grouping = NA,
                     level = .95,
                     pars = NA,
                     type = "br",
                     interval_col = "black",
                     interval_h = 0.1,
                     point_col = "black",
                     point_size = 3,
                     r_col = "black",
                     r_intervals = FALSE,
                     r_alpha = .5,
                     ...) {

  grouping <- get_grouping(model, grouping)
  d <- tidycoef(model,
                grouping = grouping,
                level = level,
                pars = pars,
                summary = T)
  b <- d[d[["type"]]=="b", ]
  r <- d[d[["type"]]=="r", ]
  probs <- c(.5 - level / 2, .5 + level / 2)
  lwr <- paste0(probs[1]*100, "%ile")
  upr <- paste0(probs[2]*100, "%ile")

  g <- ggplot(b, aes_string(x="Estimate", y="Parameter"))
  if (grepl("r", type)) {
    if (!r_intervals) r_lwr <- r_upr <- "Estimate"
    else { r_lwr <- lwr; r_upr <- upr }
    g <- g + ggstance::geom_pointrangeh(
      data=r,
      aes_(xmin=as.name(r_lwr), xmax=as.name(r_upr), group = grouping),
      shape = 1,
      col = r_col,
      alpha = r_alpha,
      size = point_size/8,
      fatten = 2,
      position = position_jitter(0, interval_h/2.1))
  }
  if (grepl("b", type)) {
    g <- g + geom_errorbarh(aes_(xmin=as.name(lwr), xmax=as.name(upr)),
                            col = interval_col, height = interval_h)
    g <- g + geom_point(size = point_size)
  }
  g
}
