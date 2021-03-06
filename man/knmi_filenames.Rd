% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/knmi.R
\name{knmi_filenames}
\alias{knmi_filenames}
\title{Get KNMI image filenames.}
\usage{
knmi_filenames(dataset_name, dataset_version, after = NULL, max_keys = 6)
}
\arguments{
\item{dataset_name}{dataset name.}

\item{dataset_version}{dataset version.}

\item{after}{return images dated after this time (POSIXct), if not provided then defaults to one hour ago.}

\item{max_keys}{maximum number of keys to return.}
}
\value{
Filenames.
}
\description{
Get KNMI image filenames.
}
