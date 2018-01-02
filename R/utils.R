#' Get grouping factors for brmsfit model
#'
#' Used internally by brmstools
#'
#' @param model a brmsfit model
#' @param grouping name of grouping factor or `NA`.
#'
#' @return character
get_grouping <- function(model, grouping){

  if (is.na(grouping)) {
    out <- unique(model$ranef$group)[1]
    # Message if automatically using one of many grouping factors
    if (length(unique(model$ranef$group)) > 1) {
      msg <- paste("Using", out, "as grouping factor. Change with 'grouping' argument.")
      message(msg)
    }
  } else { out <- grouping }

  out
}
