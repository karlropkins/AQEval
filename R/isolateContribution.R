############################################
#' @title isolateContribution
############################################

#' @name isolateContribution
#' @description time-series contribution isolation based on
#' background subtraction, deseasonalisation and deweathering
#' @param data data source, typically data.frame, containing time-series
#' @param pollutant the time-series
#' @param background (optimal) if supplied, the background time-series to
#' subtract
#' @param deseason logical, if TRUE (default) the data is deseasonalised as
#' part of contribution isolation
#' @param deweather logical, if TRUE (default) the data is deweathered as
#' part of contribution isolation
#' @param method numeric, contribution isolation method (default 2)
#' @param add.term extra terms to add to the contribution isolation
#' model; ignore for now (in development)
#' @param formula model formula to use; ignore for now (in development)
#' @param output output options; ignore for now (in development)
#' @param ... other arguments; ignore for now (in development)

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
#this currently works on opeanir-like data 1hr resolution data.frame
#     might need to think about other time-scales...
#this applies miminal dw and ds model
#     might need to think about expanding modelling term...
#think about diagnostic out following T-IRP feedback...

#splatted function
#' @import mgcv
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
    #needs check that all expected columns are there
    if(is.null(formula)){
      if(is.null(background) & !deseason & !deweather){
        stop("Need (at least) one of 'background', 'deseason' or 'deweather'")
      }
      ref <- c("date", pollutant, background)
      if(deweather){
        ref <- unique(c(ref, "wd", "ws"))
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
      ff <- paste(pollutant, " ~ ", sep="")
######################
#to think about
######################
#method 3
#log(y)~
######################
      temp <- FALSE
      if(!is.null(background)){
        ff <- paste(ff, "s(", background, ")")
        temp <- TRUE
      }
      if(deweather){
        if(temp){
            ff <- paste(ff, " + ", sep="")
        }
        ff <- paste(ff, "te(wd, ws)", sep="")
        temp <- TRUE
      }
      if(deseason){
        if(temp){
            ff <- paste(ff, " + ", sep="")
        }
############################
#to think about
############################
#method 2
#fit a spline rather than factor...
############################
        if(method==2){
          d1$jd <- as.numeric(format(d1$date, "%j"))
          d1$dh <- as.numeric(format(d1$date, "%H"))
          data$jd <- as.numeric(format(data$date, "%j"))
          data$dh <- as.numeric(format(data$date, "%H"))
          ff <- paste(ff, "s(jd) + s(dh)", sep="")
          temp <- TRUE
        } else{
          d1$jd <- format(d1$date, "%j")
          d1$dh <- format(d1$date, "%H")
          data$jd <- format(data$date, "%j")
          data$dh <- format(data$date, "%H")
          ff <- paste(ff, "jd + dh", sep="")
          temp <- TRUE
        }
        if(!is.null(add.term)){
          ff <- paste(ff, paste(" + s(", add.term, ")",
                                sep="", collapse = ""))
        }
      }
      ff <- as.formula(ff)
    }
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
        scale <- -((summary.gam(mod)$r.sq)^0.5)*mean(data[[pollutant]],
                                                         na.rm=TRUE)
    }
    ans <- ans + scale
    ########################
    #this returns vector for data.frames and tbf_df...
    ########################
    return(ans)
}
