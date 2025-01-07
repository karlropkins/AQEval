############################################
#' @title quantify break-point/segments
############################################

#' @name quantify.breaks
#' @aliases quantBreakPoints quantBreakSegments
#' @description Quantify either break-points or
#' break-segment methods for pollutant time-series
#' @param data Data source, typically a data.frame or similar,
#' containing data-series to model and a paired time-stamp
#' data-series, named date.
#' @param pollutant The name of the data-series to
#' break-point or break-segment model.
#' @param breaks (Optional) The break-points and
#' confidence intervals to use when building either
#' break-point or break-segment models. If not supplied
#' these are build using \code{\link{findBreakPoints}}
#' and supplied arguments.
#' @param ylab Y-label term, by default pollutant.
#' @param xlab X-label term, by default date.
#' @param pt.col Point fill and line colours for plot,
#' defaults lightgrey and darkgrey.
#' @param line.col Line colour for plot, default red.
#' @param break.col Break-point/segment colour for plot, default
#' blue.
#' @param event An optional list of plot terms for an event
#' marker, applied to a vertical line and text label. List
#' items include: \code{x} the event date (YYYY-MM-DD format)
#' require for both line and label; \code{y} by default 0.9 x
#' y-plot range; \code{label} the label text, required for
#' label; \code{line.size} the line width, by default 0.5;
#' \code{font.size} the text size, by default 5; and,
#' \code{hjust} the label left/right justification, 0 left,
#' 0.5 centre, 1 right (default). See also examples below.
#' @param show What to show before returning the break-point
#' quantification mode, by default plot and report.
#' @param ... other parameters
#' @param seg.method (\code{quantBreakSegments} only) the
#' break-segment fitting method to use.
#' @param seg.seed (\code{quantBreakSegments} only) the
#' seed setting to use when fitting break-segments, default
#' \code{12345}.
#' @details \code{quantBreakPoints} and
#' \code{quantBreakSegments} both use
#' \code{strucchange} methods to identify potential
#' break-points in time-series, and then quantify
#' these as conventional break-points or break-segments,
#' respectively:
#' \itemize{
#'   \item \strong{Finding Break-points} Using the
#'   \code{strucchange} methods of Zeileis and colleagues
#'   and independent change detection model, the functions
#'   apply a rolling-window approach, assuming the first
#'   window (or data subset) is without change, building a
#'   statistical model of that, advancing the window,
#'   building a second model and comparing these, and so on,
#'   to identify the most likely points of change in a
#'   larger data-series. See also \code{\link{findBreakPoints}}
#'   \item \strong{Quantifying Break-points} Using the
#'   supplied break-points to build a break-point model.
#'   \item \strong{Quantifying Break-segments} Using the
#'   confidence regions for the supplied break-points as the
#'   starting points to build a break-segment model.
#' }
#' @author Karl Ropkins
#' @references
#' Regarding \code{strucchange} methods see in-package
#' documentation, e.g. \code{\link[strucchange]{breakpoints}},
#' and:
#'
#' Achim Zeileis, Friedrich Leisch, Kurt Hornik and Christian Kleiber
#' (2002). strucchange: An R Package for Testing for Structural Change
#' in Linear Regression Models. Journal of Statistical Software, 7(2),
#' 1-38. URL \url{https://www.jstatsoft.org/v07/i02/}.
#'
#' Achim Zeileis, Christian Kleiber, Walter Kraemer and Kurt Hornik
#' (2003). Testing and Dating of Structural Changes in Practice.
#' Computational Statistics & Data Analysis, 44, 109-123.
#' DOI \doi{10.1016/S0167-9473(03)00030-6}.
#'
#' Regarding \code{segmented} methods see in-package
#' documentation, e.g.
#' \code{\link[segmented]{segmented}}, and:
#'
#'   Vito M. R. Muggeo (2003). Estimating regression models
#'   with unknown break-points. Statistics in Medicine, 22,
#'   3055-3071. DOI 10.1002/sim.1545.
#'
#'   Vito M. R. Muggeo (2008). segmented: an R Package to
#'   Fit Regression Models with Broken-Line Relationships.
#'   R News, 8/1, 20-25.
#'   URL \url{https://cran.r-project.org/doc/Rnews/}.
#'
#'   Vito M. R. Muggeo (2016). Testing with a nuisance
#'   parameter present only under the alternative: a
#'   score-based approach with application to segmented
#'   modelling. J of Statistical Computation and Simulation,
#'   86, 3059-3067.
#'   DOI 10.1080/00949655.2016.1149855.
#'
#'   Vito M. R. Muggeo (2017). Interval estimation for the
#'   breakpoint in segmented regression: a smoothed
#'   score-based approach. Australian & New Zealand Journal
#'   of Statistics, 59, 311-322.
#'   DOI 10.1111/anzs.12200.
#'
#' Regarding break-points/segment methods, see:
#'
#' Ropkins, K., Walker, A., Philips, I., Rushton, C., Clark, T. and
#' Tate, J., Change Detection of Air Quality Time-Series Using the
#' R Package AEQval. Available at SSRN 4267722.
#' https://ssrn.com/abstract=4267722 or http://dx.doi.org/10.2139/ssrn.4267722
#' Also at: https://karlropkins.github.io/AQEval/articles/AQEval_Intro_Preprint.pdf
#' @seealso
#' \code{\link{timeAverage}} in \code{openair},
#' \code{\link{breakpoints}} in \code{strucchange}, and
#' \code{\link{segmented}} in \code{segmented}.
#'
#' @returns Both functions use the \code{show} argument
#' to control which elements of the functions outputs
#' are shown but also invisible return a \code{list}
#' of all outputs which can caught using, e.g.:
#'
#' \code{brk.mod <- quantBreakPoints(data, pollutant)}
#' @note \code{AQEval} function \code{quantBreakSegments}
#' is currently running \code{segmented v.1.3-4} while we
#' evaluate latest version, \code{v.1.4-0}.
#' @examples
#' #using openair timeAverage to covert 1-hour data to 1-day averages
#'
#' temp <- openair::timeAverage(aq.data, "1 day")
#'
#' #break-points
#'
#' quantBreakPoints(temp, "no2", h=0.3)
#'
#' #break-segments
#'
#' quantBreakSegments(temp, "no2", h=0.3)
#'
#' #addition examples (not run)
#' \dontrun{
#' #in-call plot modification
#' #removing x axis label
#' #recolouring break line and
#' #adding an event marker
#' quantBreakPoints(temp, "no2", h=0.3,
#'        xlab="", break.col = "red",
#'        event=list(label="Event expected here",
#'                  x="2002-08-01", col="grey"))
#'}

#quantBreakPoints
################################
#need to tidy
#    most local functions using data2 rather than data
#recent rebuild to isolate model prediction


#splatted function
#' @rdname quantify.breaks
#' @export
quantBreakPoints <-
  function (data, pollutant, breaks, ylab = NULL,
            xlab = NULL, pt.col = c("lightgrey", "darkgrey"),
            line.col = "red", break.col ="blue",
            event = NULL, show = c("plot", "report"), ...)
  {
##########################
#need a checker for date and pollutant
#does breaks default want to be NA
#some code in next section could move into subfunctions
##########################
    data <- aqe_RowNameFix(data)
    name.pol <- pollutant
    pollutant <- data[, name.pol]
    if(missing(breaks)){
      #if breaks missing build assuming
      #what sent plus defaults
      breaks <- aqe_buildBreaks(data, name.pol, ...)
    }
    x.args <- list(...)
#print("fit")
################
    data2 <- data[,c("date", name.pol)]
    mod <- aqe_fitBreakPointsModel(data2, name.pol, breaks)
################
    data2$pred <- rep(NA, nrow(data2))
    data2$err <- data2$pred
    ans <- predict(mod, se.fit = TRUE)
    data2$pred[as.numeric(names(ans$fit))] <- ans$fit
    data2$err[as.numeric(names(ans$fit))] <- ans$se.fit
#print("report")
################
    report <- aqe_makeBreakPointsReport(data2, breaks)
#################
#################
    if ("report" %in% show) {
      aqe_summariseBreakPointsReport(report)
    }
#print("plot")
######
    ####################
    #not sure this is best way to handle
    #auto.text=FALSE...
    #####################
    auto.text <- if("auto.text" %in% names(x.args)){
      x.args$auto.text
    } else { TRUE }
    plt <- aqe_plotQuantBreakPoints(data2, name.pol, breaks,
                  xlab=xlab, ylab=ylab, pt.col=pt.col,
                  line.col=line.col, break.col=break.col,
                  event=event, auto.text=auto.text, ...)
    if ("plot" %in% show) {
      plot(plt)
    }
    return(invisible(list(data = data, breaks = breaks, data2 = data2,
                          plot = plt, report = report, model = mod)))
  }


#splatted function
#' @rdname quantify.breaks
#' @export
quantBreakSegments <-
  function (data, pollutant, breaks, ylab = NULL,
            xlab = NULL, pt.col = c("lightgrey", "darkgrey"),
            line.col = "red", break.col ="blue",
            event = NULL, seg.method = 2, seg.seed = 12345,
            show = c("plot", "report"), ...)
  {
    ##########################
    #need a checker for date and pollutant like quantBreakPoints
    #does breaks default want to be NA
    #need to rationalise seg.method = 1/2 once method finalised
    #see CRAN feedback regard set.seed
    ##########################

    #breaks setup
    #buildBreaks in quantBreakPoints
    data <- aqe_RowNameFix(data)
    if(missing(breaks)){
      breaks <- aqe_buildBreaks(data, pollutant,...)
    }
    x.args <- list(...)

    #model
    if(!seg.method %in% 1:2){
      stop("Unknown seg.method requested", call.=FALSE)
    }
    #######################################
    #should be able to simplify this or
    #drop it once seg.models confirmed
    #see optimisation notes
    #######################################
    if(seg.method==1){
      data <- aqe_RowNameFix(data)
      mod <- aqe_fitBreakSegmentsModel01(data2, pollutant, breaks)
      data2 <- data[,c("date", pollutant)]
      data2$pred <- rep(NA, nrow(data2))
      data2$err <- data2$pred
      ans <- predict(mod, se.fit = TRUE)
      data2$pred[as.numeric(names(ans$fit))] <- ans$fit
      data2$err[as.numeric(names(ans$fit))] <- ans$se.fit
      #segments
      #this needs to be tidied once methods finalised
      segments <- aqe_makeSegmentsFromBreaks01(breaks)
      #report
      #this needs to be tidied once methods finalised
      report <- aqe_makeBreakSegmentsReport(data2, segments)
      if ("report" %in% show) {
        aqe_summariseBreakSegmentsReport(report)
      }
      #plot
      #this needs to be tidied once methods finalised
      auto.text <- if("auto.text" %in% names(x.args)){
        x.args$auto.text
      } else { TRUE }
      plt <- aqe_plotQuantBreakSegments01(data2, pollutant,
                                          segments,
                                          pt.col=pt.col,
                                          break.col = break.col, line.col = line.col,
                                          ylab=ylab, xlab=xlab,
                                          event=event,
                                          auto.text = auto.text, ...)
      if ("plot" %in% show) {
        plot(plt)
      }
    }
    if(seg.method==2){
      data2 <- data[,c("date", pollutant)]
#print("fit")
#################
      data2 <- aqe_RowNameFix(data2)
      ls.mod <- aqe_fitBreakSegmentsModel02(data2, pollutant, breaks,
                                            seg.seed = seg.seed)
#print("after.fit")
      mod <- ls.mod$mod
      data2$pred <- rep(NA, nrow(data2))
      data2$err <- data2$pred
      ##########################
      #not sure if I need all this
      ##########################
      ans <- if("segmented" %in% class(mod)){
        segmented::predict.segmented(mod, se.fit = TRUE)
      } else {
        predict(mod, se.fit=TRUE)
      }
      ##############################
      data2$pred[as.numeric(names(ans$fit))] <- ans$fit
      data2$err[as.numeric(names(ans$fit))] <- ans$se.fit
      #segments
      #this needs to be tidied once methods finalised
      segments <- ls.mod$segments

      #report
      #this needs to be tidied once methods finalised
#print("report")
###################
      report <- aqe_makeBreakSegmentsReport(data2, segments)
      if ("report" %in% show) {
        aqe_summariseBreakSegmentsReport(report)
      }

      #plot
      #this needs to be tidied once methods finalised
      ##plt <- NULL
      auto.text <- if("auto.text" %in% names(x.args)){
        x.args$auto.text
      } else { TRUE }
#print("plot")
#######################
      plt <- aqe_plotQuantBreakSegments02(data2, pollutant,
                                          segments,
                                          pt.col=pt.col,
                                          break.col = break.col, line.col = line.col,
                                          ylab=ylab, xlab=xlab,
                                          event=event,
                                          auto.text = auto.text, ...)
      if ("plot" %in% show) {
        plot(plt)
      }
    }

    #output
    return(invisible(list(data = data, segments = segments, data2 = data2,
                          plot = plt, report = report, model = mod)))
  }


#################################
#removed
#################################

## @export
##quantBreaks02 <- function(...) quantBreakPoints(...)

## from days of quantBreaks01 and 02
## nobody should be using either these days

