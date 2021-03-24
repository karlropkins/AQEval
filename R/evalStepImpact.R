############################################
#' @title evalStepImpact
############################################

#' @name evalStepImpact
#' @description Estimates time period impact of a step-change.
#' @param mod Data source, typically a quantBreakSegments output.
#' @param pollutant the pollutant to build the step impact estimate
#' for, if not the one the break-point/segment model was built for.
#' This needs a WARNING...
#' @param start Start of time period impact of step-change to be
#' evaluated for (YYYY-MM-DD), by default from start of data
#' range.
#' @param end End of time period impact of step-change to be
#' evaluated for (YYYY-MM-DD), by default from end of data
#' range.
#' @param breaks The break-points to evaluate impact of.
#' @param segments The segments to evaluate impact of.
#' @param ... Other arguments, currently ignored

#evalStepImpact 09/06/2020
################################
#requested by defra
################################
#finish documentation

#splatted function
#' @export
evalStepImpact <-
  function (mod, pollutant = NULL, start = NULL, end = NULL,
            breaks = NULL, segments=NULL, ...)
  {
    warning("FUNCTION IN VERY EARLY DEVELOPMENT")
#check mod is what we expect
#pollutant handling
    start <- if(is.null(start)){
      as.Date(min(mod$data$date, na.rm=TRUE))
      #not date[1] might be NA, might not be in order
    } else {
      as.Date(start)
    }
    if(start < as.Date(min(mod$data$date, na.rm=TRUE))){
      stop("Can foreshorten start but not back-extrapolate")
    }
    end <- if(is.null(end)){
      as.Date(max(mod$data$date, na.rm=TRUE))
    } else {
      as.Date(end)
    }
    if(is.null(breaks) & is.null(segments)){
      stop("Need one of breaks or segments...")
    }
    #######################
    #need to add err.ex
    #######################
    #need to code for breaks rather than segments
    #when we do move the out and make selector break or segment
    #what this does at min to max of segments
    #need to think about 1st and last segment
    #need to think about pred and err
    #######################
    e.segs <- mod$segments[segments,]
#print(e.segs)
    exclusion <- c(min(e.segs[,c(2,5)], na.rm=TRUE),
                   max(e.segs[,c(2,5)], na.rm=TRUE))
#print(exclusion)
    out <-mod$data2
    out$pred.ex <- NA
    out$err.ex <- NA
    #data to exclusion
    if(exclusion[1]>1){
      out$pred.ex[1:exclusion[1]] <- out$pred[1:exclusion[1]]
      out$err.ex[1:exclusion[1]] <- out$err[1:exclusion[1]]
    }
#lines(out$date, out$pred.ex, col="red")
    #extrapolate into exclusion
#print(exclusion)
    if(exclusion[2]-exclusion[1]>1){
      #get last segment range
      temp <- as.vector(unlist(mod$segments[min(segments)-1,c(2,5)]))
      t.mod <- lm(pred~date, data=out[c(temp[1]:temp[2]),])
      temp <- predict(t.mod, newdata=out[c(exclusion[1]:exclusion[2]),])
      out[c(exclusion[1]:exclusion[2]), "pred.ex"] <- temp
#lines(out$date, out$pred.ex+10, col="red")
#print(temp)
    }
    #post-segment range
    if(exclusion[2]<nrow(out)){
      temp <- out$pred[exclusion[2]] - out$pred.ex[exclusion[2]]
#print(temp)
      out$pred.ex[c(exclusion[2]:nrow(out))] <-
        out$pred[c(exclusion[2]:nrow(out))]-temp
    }
    #quantification range
    ########################
    temp <- names(out)[2]
    out$..temp.. <- out[[temp]] - out$pred + out$pred.ex
    names(out)[names(out)=="..temp.."] <- paste(temp, ".ex", sep="")
    if(!is.null(pollutant)){
#warning regarding bad use
      out$..temp.. <- mod$data[[pollutant]]
      out$..temp2.. <- out$..temp.. - out$pred + out$pred.ex
      names(out)[names(out)=="..temp.."] <- pollutant
      names(out)[names(out)=="..temp2.."] <- paste(pollutant,
                                                   ".ex", sep="")
    }
    #extrapolate forward if needed
    if(end > as.Date(max(mod$data$date, na.rm=TRUE))){
      #extrapolate

    }

#warning regarding range actual versus predicted


plot(out$date, out$pred, lty=2, ylim=c(0,100), type="l") #temp plot
lines(out$date, out$pred.ex, col="red")
plot(out$date, out[["no2"]], lty=2, ylim=c(0,100), type="l") #temp plot
lines(out$date, out$no2.ex, col="red")
plot(out$date, out[["no2"]] - out$no2.ex, lty=2, ylim=c(0,100), type="l")
#temp plot

plot(out$date, out[["nox"]], lty=2, ylim=c(0,400), type="l") #temp plot
lines(out$date, out$nox.ex, col="red")


  out
  }

