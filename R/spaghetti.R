#' Spaghetti plots for brmsfit objects
#'
#' Spaghetti plots display fitted values for each grouping factor
#' in a single panel with (optional) the average fitted line.
#'
#' @param model A brmsfit model
#' @param grouping Name of grouping variable (e.g. `(1 | grouping)`). Defaults
#' to `NA` which returns the unique / first grouping factor in model.
#' @param level For limits of credible intervals
#' @param xvar What variable should be on the x-axis
#' @param line_col Line color
#' @param line_lty Line type
#' @param ribbon_col Ribbon color
#' @param ribbon_fill Ribbon fill
#' @param ribbon_alpha Ribbon transparency
#' @param average Show average fitted values?
#' @param ... Passed to [tidyfitted()]
#'
#' @return a ggplot
#' @export
spaghetti <- function(model,
                      grouping = NA,
                      level = .95,
                      xvar = NA,
                      line_col = "black",
                      line_lty = 1,
                      ribbon_col = NA,
                      ribbon_fill = "grey60",
                      ribbon_alpha = .3,
                      average = T,
                      ...) {

  grouping <- get_grouping(model, grouping)
  d <- tidyfitted(model, level = level, grouping = grouping)
  probs <- c(.5 - level / 2, .5 + level / 2)
  lwr <- paste0("Q", probs[1]*100)
  upr <- paste0("Q", probs[2]*100)

  g <- ggplot(d[d[["type"]]=="r",],
              aes_string(x=xvar, y="Estimate", group=grouping)) +
    geom_line(col = line_col, lty = line_lty)
  if (average) {
    g <- g + geom_line(data=d[d[["type"]]=="b",], size = 2)
    g <- g + geom_ribbon(data=d[d[["type"]]=="b",],
                         aes_(ymin=as.name(lwr), ymax=as.name(upr)),
                         alpha = ribbon_alpha,
                         col = ribbon_col,
                         fill = ribbon_fill)
  }
  g
}
