############################################
#' @title quantBreakSegments
############################################

#' @name quantBreakSegments
#' @description Assumes model based on general trend plus
#' transitions about declared break points in supplied data.
#' @param data Data source, typically a data.frame or similar,
#' containing data-series to model and a paired timestamp
#' data-series, named date.
#' @param pollutant The data-series to model.
#' @param breaks The break points and confidence intervals,
#' typically a findBreakPoints function output.
#' @param ylab Y-label term, by default pollutant.
#' @param xlab X-label term, by default date.
#' @param pt.col Point fill and line colours for plot,
#' defaults lightgrey and darkgrey.
#' @param line.col Line colour for plot, default red.
#' @param break.col Break transition colour for plot, default
#' blue.
#' @param seg.method Method to use to fit segments: 1 back-stop;
#' 2 soft segmented (default).
#' @param show What to show before returning the brak-point
#' quantification mode, by default plot and report.
#' @param ... other parameters
#' @note quantBreakSegments is the default version of this function;
#' quantSegments01 and quantSegmentss02 were early versions used in testing
#' and optimisation work.

#quantBreakSegments
#################################
#to do
#################################
#possible error catchers
#     missing time-series in user call
#     expected data missing, date plus user defined
#tidy code
#recent rebuild to isolate model prediction
#     most local functions using data2 rather than data
#diagnostic plot needed
#subfunctions structure to setup?
#calculate uncertainties and report these?


#splatted function
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

  #model
  if(!seg.method %in% 1:2){
    stop("Unknown seg.method requested", call.=FALSE)
  }
  #######################################
  #should be able to simplify this once
  #seg.models confirmed
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
    plt <- aqe_plotQuantBreakSegments01(data2, pollutant,
                                    segments,
                                    pt.col=pt.col,
                                    break.col = break.col, line.col = line.col,
                                    ylab=ylab, xlab=xlab)
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
    plt <- aqe_plotQuantBreakSegments02(data2, pollutant,
                                         segments,
                                         pt.col=pt.col,
                                         break.col = break.col, line.col = line.col,
                                         ylab=ylab, xlab=xlab)
    if ("plot" %in% show) {
      plot(plt)
    }
  }

  #output
  return(invisible(list(data = data, segments = segments, data2 = data2,
                        plot = plt, report = report, model = mod)))
}

aqe_makeBreakSegmentsReport <- function(data, segments){
  if(!is.null(segments) && nrow(segments)>0){
    #this needs tidying
    #err ranges need calculating
    data.frame(s1.date1 = data$date[segments[, 2]],
               s1.date2 = data$date[segments[, 5]],
               s1.date.delta = data$date[segments[, 5]] -
                 data$date[segments[, 2]],
               s1.c0 = data$pred[segments[, 2]],
               s1.c1 = data$pred[segments[, 5]],
               s1.c.delta = data$pred[segments[, 5]] -
                 data$pred[segments[, 2]],
               s1.per.delta = (data$pred[segments[, 5]] -
                                 data$pred[segments[, 2]]) /
                 data$pred[segments[, 2]] * 100,
               #################################
               #to do
               #################################
               #diffs to calculate/report
               #confidences to report
               stringsAsFactors = FALSE)
  } else { NULL }
}



aqe_plotQuantBreakSegments01 <- function(data, name.pol, segments,
                                            ylab = NULL, xlab = NULL,
                                            pt.col = c("lightgrey", "darkgrey"),
                                            line.col = "red", break.col ="blue",
                                            scalelabs = c("data", "trend",
                                                          "change")){
  #using plotQuantBreakPoints
  if(!is.null(segments) && nrow(segments)>0){
    temp <- segments[,4:6]
    names(temp) <- names(segments[,1:3])
    segments <- rbind(segments[,1:3], temp)
    names(segments) <- c("lower", "bpt", "upper")
    if(any(segments<2)){
      segments[segments<2] <-2
      warning("quantBreakSegments: break(s) too near '", name.pol,
              "' start...\n\tTried to fix but strongly recommend re-running ",
              "\n\tfindBreakPoints with different range\n",
              call.=FALSE)
    }
  }
  aqe_plotQuantBreakPoints(data, name.pol, segments,
                              ylab = ylab, xlab = xlab,
                              pt.col = pt.col, line.col = line.col,
                              break.col = break.col,
                              scalelabs = scalelabs)
}



aqe_plotQuantBreakSegments02 <- function(data, name.pol, segments,
                                        ylab = NULL, xlab = NULL,
                                        pt.col = c("lightgrey", "darkgrey"),
                                        line.col = "red", break.col ="blue",
                                        scalelabs = c("data", "trend",
                                                      "change")){
  #using plotQuantBreakPoints
  if(!is.null(segments) && nrow(segments)>0){
    segments <- segments[2:nrow(segments),1:3]
    names(segments) <- c("lower", "bpt", "upper")
    if(any(is.na(segments))) {
      if(any(is.na(segments[,1]))){
        segments[,1] <- ifelse(is.na(segments[,1]), 1, segments[,1])
      }
      if(any(is.na(segments[, ncol(segments)]))){
        segments[,ncol(segments)] <- ifelse(is.na(segments[,ncol(segments)]),
                                            nrow(data),
                                            segments[,ncol(segments)])
      }
      if(any(is.na(segments)))
        warning("quantBreakSegments: found suspect segment(s)...'",
                "\n\tTried to fix but failed...\n",
                call.=FALSE)
    }
    if(any(segments<2)){
      segments[segments<2] <-2
      warning("quantBreakSegments: break(s) too near '", name.pol,
              "' start...\n\tTried to fix but strongly recommend re-running ",
              "\n\tfindBreakPoints with different range\n",
              call.=FALSE)
    }
  }
  aqe_plotQuantBreakPoints(data, name.pol, segments,
                              ylab = ylab, xlab = xlab,
                              pt.col = pt.col, line.col = line.col,
                              break.col = break.col,
                              scalelabs = scalelabs)
}


aqe_summariseBreakSegmentsReport <- function(report){
  if (is.null(report)) {
    cat("no change ranges declared...\n")
  }
  else {
    for (i in 1:nrow(report)) {
      cat("\n", as.character(report[i, 1]), " to ",
          as.character(report[i, 2]), " (",
          as.character(report[i, 3]), ")\n",
          sep = "")
      cat(report[i, 4], "->", report[i, 5], ";",
          report[i, 6], " (", report[i, 7], "%)\n", sep = "")
      #########################
      #to do
      #########################
      #report confidences and diffs?
      #when calculated
    }
  }
}


aqe_fitBreakSegmentsModel01 <- function(data, name.pol, breaks){
  #function in quantBreakPoints
  mod <- aqe_fitBreakPointsModel(data, name.pol, breaks)
  data$pred <- rep(NA, nrow(data))
  data$err <- data$pred
  ans <- predict(mod, se.fit = TRUE)
  data$pred[as.numeric(names(ans$fit))] <- ans$fit
  data$err[as.numeric(names(ans$fit))] <- ans$se.fit
  if(!is.null(breaks) && nrow(breaks)>0){
    for(i in 1:nrow(breaks)){
      temp <- data$pred[c(breaks[i,1]:breaks[i,3])]
      data$pred[c(breaks[i,1]:breaks[i,3])] <-
        seq(temp[1], temp[length(temp)], length.out = length(temp))
    }
  }
  data$count <- as.numeric(data$date)
  data$count <- (data$count - min(data$count, na.rm=TRUE)) + 1
  ff <- as.formula(paste(name.pol, "~pred", sep=""))
  lm(ff, data=data)
}

aqe_makeSegmentsFromBreaks01 <- function(breaks){

  #make segments
  segments <- NULL
  if(!is.null(breaks) && nrow(breaks)>0){
    segments <- data.frame(seg.1.low=breaks$lower -
                                 (breaks$bpt-breaks$lower),
                           seg.1=breaks$lower,
                           seg.1.high=breaks$bpt,
                           seg.2.low=breaks$bpt,
                           seg.2=breaks$upper,
                           seg.2.high=breaks$upper +
                                 (breaks$upper-breaks$bpt),
                           stringsAsFactors = FALSE)
  }
  segments
}

#fitBreakSegmentsModel02
aqe_fitBreakSegmentsModel02 <- function(data, name.pol, breaks){
  #reinstate iterative segmented as method 02
  #(needs extra stops for out of range cases)
  #(might need change of outputs for method 01)
  temp <- as.data.frame(data)[c("date", name.pol)]
  #not happy with this naming strategy
  temp$..ref <- 1:nrow(temp)
  temp$..d.prop <- as.numeric(temp$date)
  temp$..d.prop <- temp$..d.prop - min(temp$..d.prop,
                                       na.rm=TRUE)
  temp$..d.prop <- temp$..d.prop/max(temp$..d.prop,
                                    na.rm=TRUE)
  ff <- as.formula(paste(name.pol, "..d.prop", sep="~"))
  mod <- lm(ff, temp)
  #####################
  #send back if not breaks
  #####################
  if(is.null(breaks) || nrow(breaks)<1){
    return(list(mod=mod, segments=NULL))
  }
  #####################
  #segs0 <- temp$..d.prop[sort(as.vector(unlist(breaks[rep(c(2),2)])))]
  #segsd <- temp$..d.prop[sort(as.vector(unlist(breaks[c(1,3)])))]
  segs0 <- temp$..d.prop[as.vector(unlist(breaks[rep(c(2),2)]))]
  segsd <- temp$..d.prop[as.vector(unlist(breaks[c(1,3)]))]
  segs0 <- segs0[order(segsd)]
  segsd <- segsd[order(segsd)]
  segsd <- segs0-segsd
  test <- do.call(expand.grid, rep(list(c(0.25,1,1.75)),
                                   length(segs0)))
  ref <- -10
  smod <- NULL
  for(i in 1:nrow(test)){
    ttest <- as.vector(unlist(test[i,]))
    segs <- segs0 + (segsd * ttest)
    segs <- segs[order(segs)]
    #segs <- sort(segs)
    if(any(segs<0.01)){
      segs[segs<0.01] <- 0.01
    }
    if(any(segs>0.99)){
      segs[segs>0.99] <- 0.99
    }

    log <- capture.output({
    tmod <- try(suppressWarnings(
      segmented::segmented(mod, seg.Z=~..d.prop, psi=segs,
                           control=segmented::seg.control(it.max=1, n.boot=0))
    ), silent=TRUE)
    })
    if(class(tmod)[1]!="try-error"){
      ans <- try(segmented::summary.segmented(tmod)$adj.r.squared,
                 silent=TRUE)
      if(class(ans)[1]!="try-error" && ans>ref){
        ref <- ans
        smod <- tmod
      }
    }
  }
  #if no segmented model built...
  #fault 1: sot, thomas lewin, bpt 4 at end...
  if(is.null(smod)) {
    stop(paste("quantBreakSegments(): segmented model trips",
               "\n\t(close to end break?)", sep=""),
         call. = TRUE)
    #return(list(mod=mod, segments=NULL))
  }
  segs <- segmented::confint.segmented(smod)
  segs.low <- round(approx(temp$..d.prop, temp$..ref, segs[,2])$y)
  segs.hi <- round(approx(temp$..d.prop, temp$..ref, segs[,3])$y)
  segs <- round(approx(temp$..d.prop, temp$..ref, segs[,1])$y)

  segments <- data.frame(
    seg.str.low = c(1, segs.low),
    seg.str = c(1, segs),
    seg.str.hi = c(1, segs.hi),
    seg.end.low = c(segs.low, nrow(data)),
    seg.end = c(segs, nrow(data)),
    seg.end.hi = c(segs.hi, nrow(data))
  )
  #this returns mod and segments
  list(mod=smod, segments=segments)
}
