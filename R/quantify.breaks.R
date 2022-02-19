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
#' @param breaks (Optional) The break-points to
#' and confidence intervals to use when build either
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
#' @param show What to show before returning the break-point
#' quantification mode, by default plot and report.
#' @param ... other parameters
#' @param seg.method (\code{quantBreakSegments} only) the
#' break-segment fitting method to use.
#' @details \code{quantBreakPoints} and
#' \code{quantBreakSegments} both use
#' \code{strucchange} methods to identify potential
#' break-points in time-series, and then quantify
#' these as conventional break-points or break-segments,
#' respectively.
#' @author Karl Ropkins
#' @references
#' Ropkins et al (In Prep).
#' @seealso
#' \code{\link{timeAverage}} in \code{openair}.
#' @returns Both functions use the \code{show} argument
#' to control which elements of the functions outputs
#' are shown but also invisible return a \code{list}
#' of all outputs which can caught using, e.g.:
#'
#' \code{brk.mod <- quantBreakPoints(data, pollutant)}
#' @examples
#' #fitting a simple deseasonalisation, deweathering
#' #and background correction (dswb) model to no2:
#'
#' aq.data$dswb.no2 <- isolateContribution(aq.data,
#'                         "no2", background="bg.no2")
#'
#' #compare at 7 day resolution:
#' temp <- openair::timeAverage(aq.data, "7 day")
#'
#' #without dswb
#' quantBreakPoints(temp, "no2", test=FALSE, h=0.1)
#'
#' with dswb
#' quantBreakPoints(temp, "dswb.no2", test=FALSE, h=0.1)

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
            show = c("plot", "report"), ...)
  {
##########################
#need a checker for date and pollutant
#does breaks default want to be NA
#some code in next section could move into subfunctions
##########################
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
                  auto.text=auto.text)
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
            seg.method = 2,
            show = c("plot", "report"), ...)
  {
    ##########################
    #need a checker for date and pollutant like quantBreakPoints
    #does breaks default want to be NA
    #need to rationalise seg.method = 1/2 once method finalised
    ##########################

    #breaks setup
    #buildBreaks in quantBreakPoints
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
      mod <- aqe_fitBreakSegmentsModel01(data, pollutant, breaks)
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
                                          auto.text = auto.text)
      if ("plot" %in% show) {
        plot(plt)
      }
    }
    if(seg.method==2){
      data2 <- data[,c("date", pollutant)]
      ls.mod <- aqe_fitBreakSegmentsModel02(data2, pollutant, breaks)
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
      plt <- aqe_plotQuantBreakSegments02(data2, pollutant,
                                          segments,
                                          pt.col=pt.col,
                                          break.col = break.col, line.col = line.col,
                                          ylab=ylab, xlab=xlab,
                                          auto.text = auto.text)
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

