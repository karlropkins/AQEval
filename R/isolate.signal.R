############################################
#' @title isolateContribution
############################################

#' @name isolate.signal
#' @aliases isolateContribution
#' @description Environmental time-series signal processing:
#' Contribution isolation based on background subtraction,
#' deseasonalisation and/or deweathering.
#' @param data Data source, typically \code{data.frame}
#' (or similar), containing all time-series of interest.
#' @param pollutant The column name of the \code{data}
#' time-series to be signal processed.
#' @param background (optional) if supplied, the background
#' time-series to use as a background correction.
#' See below.
#' @param deseason logical or character vector, if
#' \code{TRUE} (default), the \code{pollutant} is
#' deseasonalised using \code{day.hour} and \code{year.day}
#' frequency terms, all calculate from the \code{data}
#' time stamp, assumed to be \code{date} in \code{data}.
#' Other options: \code{FALSE} to turn off
#' deseasonalisation; or a character vector of frequency
#' terms if user-defining. See below.
#' @param deweather logical or character vector, if
#' \code{TRUE} (default), the data is deweathered using
#' wind speed and direction, assumed to be \code{ws}
#' and \code{wd} in \code{data}). Other options: \code{FALSE}
#' to turn off deweathering; or a character vector of
#' \code{data} column names if user-defining. See below.
#' @param method numeric, contribution isolation method
#' (default 2). See Note.
#' @param add.term extra terms to add to the contribution
#' isolation model; ignore for now (in development).
#' @param formula (optional) Signal isolate model formula;
#' this allows user to set the signal isolation model formula
#' directly, but means other formula terms (\code{background},
#' \code{deseason} and \code{deweather}) will be ignored.
#' @param output output options; ignore for now (in development)
#' @param ... other arguments; ignore for now (in development)
#' @author Karl Ropkins
#' @returns \code{isolateContribution} returns a vector of
#' predictions of the \code{pollutant} time-series after
#' the requested signal isolation.
#' @details \code{isolateContribution} estimates and
#' subtracts \code{pollutant} variance associated with
#' factors that may hinder break-point/segment analysis:
#' \itemize{
#'  \item{\strong{Background Correction}}{ If applied, this fits
#'  the supplied \code{background} time-series as a
#'  spline term: \code{s(background)}.}
#'  \item{\strong{Seasonality}}{ If applied, this fits regular
#'  frequency terms, e.g. \code{day.hour}, \code{year.day},
#'  as spline terms, default TRUE is equivalent to
#'  \code{s(day.hour)} and \code{s(year.day)}. All terms are
#'  calculated from \code{date} column in \code{data}.}
#'  \item{\strong{Weather}}{ If applied, this fits time-series of
#'  identified meteorological measurements, e.g. wind speed
#'  and direction (\code{ws} and \code{wd} in \code{data}).
#'  If both \code{ws} and \code{wd} are present these are
#'  fitted as a tensor term \code{te(ws, wd)}. Other
#'  \code{deweather}ing terms, if included, are fitted
#'  as spline term \code{s(term)}. The default \code{TRUE}
#'  is equivalent to \code{te(ws, wd)}.}
#' }
#' Using the supplied arguments, it builds a signal
#' (\code{\link{mgcv}}) GAM model, calculates,
#' and returns the mean-centred residuals as an
#' estimate of the isolated local contribution.
#' @note \code{method} was included as part of method
#' development and testing work, and retained for now.
#' Please ignore for now.
#' @references
#' Regarding \code{\link{mgcv}} GAM fitting methods, see
#' Wood (2017) for general introduction and package
#' documentation regarding coding (\code{\link{mgcv}}):
#'
#' Wood, S.N. (2017) Generalized Additive Models:
#' an introduction with R (2nd edition), CRC, DOI:
#' \url{https://doi.org/10.1201/9781420010404}.
#'
#' Regarding \code{isolateContribution}, see:
#'
#' Ropkins et al (In Prep).
#' @seealso
#' \code{\link{mgcv}}, \code{\link{gam}}.
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
#' #with dswb
#' quantBreakPoints(temp, "dswb.no2", test=FALSE, h=0.1)




#isolateContribtion function
##################################
#to do
##################################
#test
#tidy code - quick package for work Anthony is doing
#     move any common elements to sub functions
#check alternative to mgcv - might be stuck with it but
#possible error catchers?
#     bad call - missing time-series
#     missing expected time-series, date, ws, wd
#document methods
#diagnostic plot
#     marginal effect partial plot of model?
#     nb: this needs to be sub function (in case plotting package changes...)
##################################
#notes
##################################
#this currently works on openair-like data 1hr resolution data.frame
#     might need to think about other time-scales...
#this currently applies minimal dw and ds model
#     might need to think about expanding modelling term...
#think about diagnostic options following T-IRP feedback...
#think about import
#think about making some of this internal functions
#     for example the dS and dW fomula build...


#splatted function
#' @import mgcv
#' @rdname isolate.signal
#' @export
isolateContribution <-
function(data, pollutant, background = NULL,
         deseason = TRUE, deweather = TRUE,
         method = 2, add.term = NULL,
         formula = NULL,
         output = "mean",
         ...){

    #####################
    #setup
    #####################
    if(is.null(formula)){
     #don't need any of this if formula is set!

      deweather <- if(is.logical(deweather)){
        if(deweather) c("ws", "wd") else character()
      } else {
        deweather
      }

      deseason <- if(is.logical(deseason)){
        if(deseason) c("day.hour", "year.day") else character()
      } else {
        deseason
      }

      #check that all expected columns are there

      if(is.null(background) & length(deseason)<1 & length(deweather)<1){
        stop("Need (at least) one of 'background', 'deseason' or 'deweather'")
      }
      ref <- c("date", pollutant, background)
      if(length(deweather)>1){
        ref <- unique(c(ref, deweather))
      }
      if(!is.null(add.term)){
        ref <- unique(c(ref, add.term))
      }
    } else {
      ref <- unique(all.vars(formula))
      if(length(formula)==2){
        ############################
        #this will error if formula has no y term
        #and pollutant missing
        ##########################
        ref <- unique(c(pollutant, ref))
      }
    }
    temp <- ref[!ref %in% names(data)]
    ####################
    #untidy error message
    ###################
    if(length(temp)>0){
        stop(paste("Missing data-series: ", paste(temp, collapse=", "),
                   "\n", sep=""))
    }
    d1 <- data[ref]                  #currently not used...
    d1$..counter <- 1:nrow(d1)       #...
    d1 <- as.data.frame(na.omit(d1)) #...
    ####################
    #build model formula
    #or use formula
    ####################
    if(!is.null(formula)){
      if(length(formula)==3){
        ff <- formula
        pollutant <- all.vars(ff[[1]])
      } else {
        ff <- paste(pollutant, as.character(formula),
                    sep=" ~ ")
        ff <- as.formula(ff)
      }
    } else {
      ff <- ""
######################
#method 3
#log(y)~ (and e^y)
#does not seem to be better
#test again/makes no sense...
######################
      if(!is.null(background)){
        ff <- paste(ff, "+s(", background, ")")
      }
      if(length(deweather)>0){
        if("wd" %in% deweather & "ws" %in% deweather){
          ff <- paste(ff, "+te(wd,ws)", sep="")
          deweather <- deweather[!deweather %in% c("wd", "ws")]
        }
        if(length(deweather)>0){
          fff <- paste("s(", deweather, ")", sep="", collapse = "+")
          ff <- paste(ff, "+", fff, sep="")
        }

        }
        temp <- TRUE

      if(length(deseason)>0){
############################
#like to look at this again
############################
#method 2
#fit a spline better than factor...
############################
#add more seasonal terms
# week.day
# month.day??
# week.hour??
############################
#do we need to set d1 and data?
# unless we want to return data
# it seems unlikely
############################
        if(method==2){
          if("year.day" %in% deseason){
            d1$year.day <- as.numeric(format(d1$date, "%j"))
            data$year.day <- as.numeric(format(data$date, "%j"))
            ff <- paste(ff, "+s(year.day)", sep="")
          }
          if("day.hour" %in% deseason){
            d1$day.hour <- as.numeric(format(d1$date, "%H"))
            data$day.hour <- as.numeric(format(data$date, "%H"))
            ff <- paste(ff, "+s(day.hour)", sep="")
          }
        } else{
          #hold for testing...
          if("year.day" %in% deseason){
            d1$year.day <- as.numeric(format(d1$date, "%j"))
            data$year.day <- as.numeric(format(data$date, "%j"))
            ff <- paste(ff, "+year.day", sep="")
          }
          if("day.hour" %in% deseason){
            d1$day.hour <- as.numeric(format(d1$date, "%H"))
            data$day.hour <- as.numeric(format(data$date, "%H"))
            ff <- paste(ff, "+day.hour", sep="")
          }
        }
        if(!is.null(add.term)){
          #this currently only allows you to add one term
          ff <- paste(ff, paste("+s(", add.term, ")",
                                sep="", collapse = ""))
        }
      }
      ff <- gsub('^[+]','', ff)
      ff <- paste(pollutant, " ~ ", ff, sep="")
      ff <- as.formula(ff)
    }
#think about how we do/document this...
cat(paste(as.character(ff)[c(2,1,3)],
            sep="", collapse = " "))
    ############################
    #model
    ############################
    mod <- gam(ff, data=data)
    if("model" %in% output)
      return(mod)
    temp <- predict.gam(mod)
    ans <- rep(NA, length=nrow(data))
    ans[as.numeric(names(temp))] <- temp
    ################################
    #scaling
    ################################
    if("mean" %in% output){
        ans <- data[[pollutant]] - ans
        scale <- mean(data[[pollutant]], na.rm=TRUE)
    } else {
      #residual, might want to add warning?
      ans <- data[[pollutant]] - ans
      scale <- 0
    }
    ans <- ans + scale
    ########################
    #this returns vector for data.frames and tbf_df...
    ########################
    return(ans)
}
