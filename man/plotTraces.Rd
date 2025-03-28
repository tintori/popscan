% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotTraces.R
\name{plotTraces}
\alias{plotTraces}
\title{Takes a growth curve table and meta_with_minmax table and outputs an assortment of plots}
\usage{
plotTraces(
  sd.table,
  exp.design,
  save.to.folder = "./tmp_plots",
  plot.untrimmed.traces = T,
  plot.trimmed.traces = T,
  plot.ghost.curve = T,
  plot.time.to.starve = T,
  p.width = NA,
  p.height = NA,
  p.suffix = ""
)
}
\arguments{
\item{sd.table}{A table with one row per image captured, with standard deviation of pixel intensity already calculated.}

\item{exp.design}{A table with one row per culture/plate/petri dish. Each column adds info about how to group, facet, color, etc the data points.}

\item{save.to.folder}{The folder where images will be saved. Defaults to "./tmp_plots".}

\item{plot.untrimmed.traces}{Plot each entire growth curve. Defaults to TRUE.}

\item{plot.trimmed.traces}{Plot just the lowest point to the highest point of each growth curve. Defaults to TRUE.}

\item{plot.ghost.curve}{Plot the trimmed curves on top of the faded out full curves. Defaults to TRUE.}

\item{plot.time.to.starve}{Plot just the number of hours from time fed to time starved. Defaults to TRUE.}

\item{p.width}{Force plot width. Defaults to NA.}

\item{p.height}{Force plot height. Defaults to NA.}

\item{p.suffix}{If running this multiple ways, include a suffix for the exported files, to distinguish them from each other. Defaults to "" (nothing).}
}
\description{
Takes a growth curve table and meta_with_minmax table and outputs an assortment of plots
}
\examples{
plotTraces()
}
