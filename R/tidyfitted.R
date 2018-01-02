#' Tidy fitted values from brmsfit models
#'
#' This function extracts fitted values in a format expected by other
#' plotting functions
#'
#' @param model a brmsfit model
#' @param grouping Name of grouping variable (e.g. `(1 | grouping)`). Defaults
#' to `NA` which returns the unique / first grouping factor in model.
#' @param xvar Predictor variable to evaluate fitted values on
#' @param level For credible interval limits.
#' @param ... Passed to [brms::fitted.brmsfit()]
#'
#' @return a tibble
#' @export
tidyfitted <- function(model,
                       grouping = NA,
                       xvar = NA,
                       level = .95, ...) {

  grouping <- get_grouping(model, grouping)

  # By default, use all predictor terms
  predictors <- all.vars(model$formula$formula[[3]])
  predictors <- predictors[!grepl(grouping, predictors)]

  probs <- c(.5 - level / 2, .5 + level / 2)

  r <- dplyr::distinct_(model$data, grouping, predictors)
  r[["type"]] <- "r"
  ry <- tibble::as_tibble(
    cbind(r, fitted(model, newdata = r, probs = probs, ...))
    )

  b <- dplyr::distinct_(model$data, predictors)
  b[[grouping]] <- NA
  b[["type"]] <- "b"
  by <- tibble::as_tibble(
    cbind(b, fitted(model, newdata = b, re_formula = NA, probs = probs, ...))
    )
  out <- dplyr::bind_rows(ry, by)
  out
}
