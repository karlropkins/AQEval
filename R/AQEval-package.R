##################################################
#' Air Quality Evaluation
##################################################
#'
#' R AQEval: R code for the analysis of discrete
#' change in Air Quality time-series.
#'
#' @section AQEval:
#' \code{AQEval} was developed for use by those tasked with
#' the routine detection, characterisation and quantification
#' of discrete changes in air quality time-series.
#'
#' The main functions, \code{\link{quantBreakPoints}}
#' and \code{\link{quantBreakSegments}}, use
#' break-point/segment (BP/S) methods
#' based on the consecutive use of methods in the
#' \code{strucchange} and \code{segmented} \code{R} packages
#' to first detection (as break-points) and then characterise
#' and quantify (as segments), discrete changes in
#' air-quality time-series.
#'
#' \code{AQEval} functions adopt an \code{openair}-friendly
#' approach using function and data structures that many
#' in the air quality research community are already familiar
#' with.
#' Most notably, most functions expect supplied data
#' to be time-series, to be supplied as a single
#' \code{data.frame} (or similar R object), and for
#' time-series to be identified by column names.
#' The main functions are typically structured expect
#' first the \code{data.frame}, then the name of the
#' pollutant to be used, then other arguments:
#'
#' \code{function(data, "polluant.name", ...)}
#'
#' \code{output <- function(data, "polluant.name", ...)}
#'
#' @seealso
#'
#' For more about data structure and an example data set,
#' see \code{\link{AQEval.data}}
#'
#' For more about the main functions, see
#' \code{\link{quantBreakPoints}}
#' and \code{\link{quantBreakSegments}}
#' @author Karl Ropkins
#' @references
#' Ropkins et al (In Prep).
#'
#' @docType package
#' @name AQEval
NULL
