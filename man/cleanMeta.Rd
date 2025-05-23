% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleanMeta.R
\name{cleanMeta}
\alias{cleanMeta}
\title{Parsing meta file}
\usage{
cleanMeta(
  in.table,
  scan.prefix = "scan",
  scandir = NA,
  crop.hist.dir = NA,
  scan.position = NA,
  group.factors = c(),
  facet.wrap.factor = c(),
  facet.grid.factors = list(),
  color.factor = NA,
  linetype.factor = NA,
  filters = c(),
  timefed = "time_fed",
  trimbefore = NA,
  trimafter = NA,
  additional.columns = NA
)
}
\arguments{
\item{in.table}{A table with one row per culture/plate/petri dish. Each column adds info about how to group, facet, color, etc the data points.}

\item{scan.prefix}{Which column specifies the unique name for each timecourse of images. Defaults to "scan".}

\item{scan.position}{If there are multiple plates per scan, this column specifies which crop on the original scan corresponds to the sample. Defaults to "position", but "", NA, NULL, "none" are also good options}

\item{group.factors}{Column(s) to group images by such that each group corresponds to a single sample. For example, c("strain", "dose", "replicate").}

\item{facet.wrap.factor}{Column(s) to facet wrap the plots by, if any}

\item{facet.grid.factors}{A list of two vectors of column names, the first specifying facet rows and the second specifying facet columns.}

\item{color.factor}{Column to color plots by, if any.}

\item{linetype.factor}{Column to set linetype by, if any.}

\item{filters}{Logical columns(s) to filter samples based on, if any.}

\item{timefed}{Column that specifies the worms met food on each plate. Date format = yy-mm-dd_HH_MM}

\item{trimbefore}{Column that specifies the timepoint before which data for plate should be ignored. Date format = yy-mm-dd_HH_MM}

\item{trimafter}{Column that specifies the timepoint after which data for plate should be ignored. Date format = yy-mm-dd_HH_MM}

\item{scan.dir}{FULL path to the directory containing the cropped images and/or histogram tables, if it exists.}
}
\description{
This function cleans up the metadata table, and allows you to specify roles for columns.
}
\examples{
cleanMeta()
}
