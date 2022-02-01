############################################
#' @title AQEval Example data
############################################
#'
#' @name AQEval.data
#' @aliases aq.my1 aq.ea2 aq.kc1 met.lhr
#' @description Data packaged with AQEval for
#' use with example code.
#' @usage
#' aq.my1
#' aq.ea2
#' aq.kc1
#' met.lhr
#' @format 4 (70128x6) 'tbl_df' objects
#' \describe{
#'   \item{date}{Time-series of POSIX class date and time records.}
#'   \item{site}{For \code{aq...} datasets, air quality
#'   monitoring site name.}
#'   \item{station}{For \code{met...} dataset, meteorological
#'   monitoring station name.}
#'   \item{code}{Monitoring site identifer code, see below.}
#'   \item{no2}{For \code{aq...} datasets, time-series of
#'   nitrogen dioxide measurements.}
#'   \item{nox}{For \code{aq...} datasets, time-series of
#'   oxides of nitrogen measurements.}
#'   \item{o3}{For \code{aq...} datasets, time-series of
#'   ozone measurements.}
#'   \item{ws}{\code{met...} dataset, time-series of
#'   wind speed measurements.}
#'   \item{wd}{\code{met...} dataset, time-series of
#'   wind direction measurements.}
#'   \item{air_temp}{\code{met...} dataset, time-series
#'   of air temperature measurements.}
#' }
#' @source Air quality and meteorological data packaged
#' for use with AQEval Examples.
#'
#' aq.my1 air quality data from Marylebone Road AURN
#' monitoring station.
#'
#' aq.ea2 and aq.kc1 air quality data from Earling and
#' Knightsbridge KCL network monitoring stations.
#'
#' met.lhr meteorological data from London Heathrow NOAA
#' archive.
#'
#' Data access from associated archives using openair
#' functions importAURN and importKCL and worldmet
#' function importNOAA.
#'
"aq.my1"
"aq.ea2"
"aq.kc1"
"met.lhr"
