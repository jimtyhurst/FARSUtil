% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_util.R
\name{fars_map_state}
\alias{fars_map_state}
\title{Plots accidents on an outline of a state map.}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{integer identifier of a state in the
United States.}

\item{year}{integer year for which to plot accidents.}
}
\value{
NULL, but has side-effect of creating a plot.
}
\description{
Plots dots on an outline of a state map for accidents
    in the give state during the given year.
}
