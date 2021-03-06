% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidycoef.R
\name{tidycoef}
\alias{tidycoef}
\title{Tidy parameters from brmsfit objects}
\usage{
tidycoef(model, grouping = NA, pars = NA, summary = FALSE,
  level = 0.95)
}
\arguments{
\item{model}{A brmsfit.}

\item{grouping}{Name of grouping variable (e.g. \code{(1 | grouping)}). Defaults
to \code{NA} which returns the unique / first grouping factor in model.}

\item{pars}{Parameters to extract; must match exactly. Combine many with
\code{c(...)}.}

\item{summary}{Should summary statistics be returned instead of raw values.}

\item{level}{For interval limits.}
}
\value{
a tibble
}
\description{
Extract brmsfit parameter (summaries) into a tidy tibble. Returns a tibble
where the type of parameter (varying / average) is represented separately
from the parameter. Especially useful for plots where the "fixed"
parameters should be shown next to their "random" counterparts.
}
