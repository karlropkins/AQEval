############################################
#' @title Other Air Quality Models
############################################

#' @name other.aq.models
#' @rdname fitNearSiteModel
#' @description Other packaged Air Quality Models.
#' @param data \code{data.frame} (or similar) containing data-series
#' to be modelled; this is expected to contain 'date', 'site'
#' and pollutant of interest data-series.
#' @param pollutant The name of the \code{pollutant} (in
#' \code{data}) to model, by default 'NO2'.
#' @param y The name of the monitor site to be modelled,
#' assumed to be one several names in the \code{site} column of
#' \code{data}.
#' @param x The other sites to use when building the model, the
#' default 'rest' uses all supplied sites except 'y'.
#' @param elements The number of inputs to use in the
#' site models, can be any number up to length of x or
#' combination thereof; by default this is set as
#' \code{length(x):1}
#' @details \code{fitNearSiteModel} builds an air quality
#' model for one location using air quality data from nearby
#' sites.

## #' @references TO DO...

#fitNearSiteModel
##############################################
#v0.0.2 22/02/2020
#fit model based on near site data

#to think about
#################################
#name of function
#merge this and findNearSites function
#    documentation
#

#this NEEDS tidyr at moment
#this NEEDS mgcv at moment

#' @import mgcv
#' @export
fitNearSiteModel <-
  function(data, pollutant = "no2", y, x = "rest",
           elements = NULL, ...){
    #setup
    x.args <- list(...)
    data <- aqe_checkData(data,
                          unique(c("date", "code",
                                  pollutant)),
                          "fitNearSiteModel(data,...)")
    data <- tidyr::spread(data, "code", pollutant)
    #think I have to do this to make gams work...
    #tibbles names confusing things
    data <- as.data.frame(data)
    names(data) <- make.names(names(data))
    y <- make.names(y)
    x <- make.names(x)
    #
    names <- names(data)[names(data)!="date"]
    ################
    if(!y %in% names){
      stop("fitNearSiteModel(data,...)\n\t",
           "Expected data for site '", y, "'",
           call. = FALSE)
    }
    if(is.character(x) && length(x)==1 &&
       tolower(x)=="rest"){
      x <- names[names!=y]
    }
    temp <- x[!x %in% names]
    if(length(temp)>0){
      stop("fitNearSiteModel(data,...)\n\t",
           "Missing data for expected site(s):\n\t",
           paste(x, collapse = ", "), call. = FALSE)
    }
    #elements
    if(is.null(elements)){
#might need more here
      elements <- length(x):1
    }
#this is not tidy
#also want pollutant name here...
    ans <- data.frame(date=data$date, ref=data[,y],
                      pred.temp=NA, pred=NA, model=NA)
#need option to add elements of own?
    elements <- lapply(elements, function(element){
      combn(x, element, simplify=FALSE)
    })
    for(i in 1:length(elements)){
      #do for each level of elements
      #if not already filled...
      if(any(is.na(ans$pred))){
        #build all models
        mods <- elements[[i]]
        mod.ls <- lapply(1:length(mods), function(j){
          mod <- mods[[j]]
          form <- paste("s(", mod, ")", sep="")
          form <- paste(form, collapse = "+")
          form <- paste(y, "~", form, sep="")
          form <- as.formula(form)
          mod <- paste(mod, collapse="+")
          mod.ans <- gam(form, data = data)
          list(mod=mod, gam=mod.ans, r=summary(mod.ans)$r.sq)
        })
        #reorder mod.ls
        mod.ls <- mod.ls[rev(order(sapply(mod.ls,
                              function(x) x$r)))]
        #add prediction to data to ans
        #if not already filled
        for(j in 1:length(mod.ls)){
          if(any(is.na(ans$pred))){
            #pred and add to ans as pred
            #track mod in ans as model
            mod.y <- predict(mod.ls[[j]]$gam,
                             newdata = data)
            ans$pred.temp <- NA
            ans$pred.temp[as.numeric(names(mod.y))] <-
              mod.y
#extra linear model step here in original method
#test if needed...
            ans$model[is.na(ans$pred) &
                        !is.na(ans$pred.temp)] <-
              mod.ls[[j]]$mod
            ans$pred[is.na(ans$pred) &
                        !is.na(ans$pred.temp)] <-
              ans$pred.temp[is.na(ans$pred) &
                         !is.na(ans$pred.temp)]
#######################
#TO HERE
#######################
          }
        }
      }
    }
    #remove pred.temp
    ans <- ans[names(ans)!="pred.temp"]
    ans
  }


aqe_checkData <-
  function(data, names, fun.name = "AQEval", tidy = TRUE){
    temp <- names[!names %in% names(data)]
    if(length(temp)>0){
      stop(fun.name, "\n\tRequires missing data column(s):\n\t",
           paste(temp, collapse = ", "), call.=FALSE)
    }
    if(tidy) data <- data[names]
    data
  }
