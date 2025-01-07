#################################################
#' @title Some functions to calculate statistics
#################################################

#' @name calculate.stats
#' @aliases calcDateRangeStat calcRollingDateRangeStat
#' @description Calculate data set statistics for
#' selected time intervals.
#' @param data (data.frame, tibble, etc) Data set containing
#' data statistic to be calculated for, and \code{date}
#' column of date/time records.
#' @param pollutant (character) The name(s) of data-series to
#' analyse in \code{data}, by default all columns in
#' supplied data except \code{date}.
#' @param ... extra arguments.
#' @param method (numeric) Method to use when calculating
#' statistic. Currently 1 (using base R), 2 (using dplyr),
#' 3 (using data.table), and 4 (using dplyr and purrr)
#' @param from (various) Start date(s) to subsample from when
#' calculating statistic, by default end of supplied
#' \code{data} date range.
#' @param to (various) End date(s) to subsample to when
#' calculating statistic, by default end of supplied
#' \code{data} date range.
#' @param stat (function) Statistic to be applied to selected
#' data, by default \code{mean(pollutant, na.rm=TRUE)}. NB:
#' This should be a function that works on vectors in the form
#' \code{function(x)}.
#' @param range (character) For \code{calcRollingDateRange},
#' the range the rolling date windows, by default
#' \code{'year'} for annual statistic calculations.
#' @param res (character) For \code{calcRollingDateRange},
#' the resolution to calculate the rolling statistic at, by
#' default \code{'day'} to calculate this once per day.
#' @note These functions are in development and likely to change
#' significantly in future versions, please handle with
#' care.
#' @returns These functions return \code{data.frame}s of function
#' outputs.

#kr version 0.4 (first packaged) 2020/11/02
#   version 0.5 2020/12/16

# added openair.like via ...
#   wd/wd handling like in openair
#   maybe move forward if keeping
#   also document properly if keeping

#to think about/do
##################################
#error handle - for bad inputs
#faster options - using method to test these...
#statistic as arg, option?
#time window and output resolution?
#better option/control than only.last...

#doc sources
#also see
#https://stackoverflow.com/questions/10803010/how-to-calculate-rolling-average-of-past-1-month-in-r
#regarding rolling windows...

#(not a fan of importing lubridate in full...)
#(maybe try dplyr approach?)
#' @import lubridate

# ref = seq(as.Date("2001-01-01"), as.Date("2004-01-01"), length.out=100)
# stat = function(x) { if (is.numeric(x)){ mean(x, na.rm = TRUE) } else { x[1] }}
# calcDateRangeStat(aq.data, from=ref, to=ref+100, method=4, stat=stat)
#

#' @rdname calculate.stats
#' @export
calcDateRangeStat <-
  function(data, from = NULL, to = NULL,
           stat = NULL, pollutant = NULL,
           ..., method=2){
    #calculates stat for all data in date range

    #setup
    .xargs <- list(...)
    .d.cls <- class(data)
    if(!method %in% 1:4){
      stop("Unknown method requested", call.=FALSE)
    }

    #to to from
    # currently no sub- grouping
    from <- aqe_prepFromDate(from, data)
    to <- aqe_prepToDate(to, data)
    dates <- data.frame(start.date = from, end.date = to,
                        stringsAsFactors = FALSE)
    if(is.null(stat)){
      stat <- function(x){
        if(is.numeric(x)) mean(x, na.rm=TRUE) else NA
      }
    }

    ###################
    # openair-like handling of ws/wd if there...
    ###################
    #not tested or documented...
    if(!is.null(.xargs[["openair.like"]])){
      if(is.logical(.xargs$openair.like)){
        .xargs$openair.like <- if(.xargs$openair.like){
          "standard"
        } else {
          NULL
        }
      }
    }
    if(!is.null(.xargs[["openair.like"]])){
      if ("wd" %in% names(data)) {
        if (is.numeric(data$wd) && "ws" %in% names(data)) {
          data$.Uu <- data$ws * sin(2 * pi * data$wd/360)
          data$.Vv <- data$ws * cos(2 * pi * data$wd/360)
        }
        if (is.numeric(data$wd) && !"ws" %in% names(data)) {
          data$.Uu <- sin(2 * pi * data$wd/360)
          data$.Vv <- cos(2 * pi * data$wd/360)
        }
      }
    }

    test <- if(is.null(pollutant)){
      names(data)[names(data)!="date"]
    } else {
      pollutant
    }

    #base method
    if(method==1){
      #previous 1 within the t(sapply)... with died with columns were different classes
      ans <- lapply(1:nrow(dates), function(x){
        temp <- data[data$date >= dates$start.date[x] & data$date < dates$end.date[x], test]
        temp <- as.data.frame(lapply(temp, stat))
        if(nrow(temp)>0){
          data.frame(start.date=dates$start.date[x],
                     end.date=dates$end.date[x],
                     temp)
        } else {
          NULL
        }
      })
      df.stat <- do.call(rbind, ans)
      if(class(data)[1] %in% c("tbl", "tbl_df")){
        df.stat <- dplyr::as_tibble(df.stat)
      }
    }
    if(method==2){
      #method 2 dplyr and base::Reduce...
      ans <- lapply(test,
                    function(x){
                      data$.rstat <- data[[x]]
                      #removed pipe from next section of code
                      #(not sure about trade-offs)
                      df.stat <- dplyr::group_by(dates, start.date,
                                                 end.date)
                      df.stat <- dplyr::summarize(df.stat,
                                                  rstat = stat(data$.rstat[data$date >= start.date &
                                                                             data$date < end.date]))
                      names(df.stat)[names(df.stat)=="rstat"] <- x
                      df.stat
                    })
      df.stat <- Reduce(function(x,y) {dplyr::full_join(x,y, by=c("start.date", "end.date"))},
                        ans)
    }
    if(method==3){
      #data.table
      ans <- lapply(1:nrow(dates), function(x){
        temp <- data[data$date >= dates$start.date[x] & data$date < dates$end.date[x], test]
        temp <- data.table::as.data.table(temp)
        temp <- temp[, (test) := lapply(.SD, stat), .SDcols = test]

        temp <- as.data.frame(lapply(temp, stat))
        if(nrow(temp)>0){
          data.frame(start.date=dates$start.date[x],
                     end.date=dates$end.date[x],
                     temp)
        } else {
          NULL
        }
      })
      df.stat <- do.call(rbind, ans)
      if(class(data)[1] %in% c("tbl", "tbl_df")){
        df.stat <- dplyr::as_tibble(df.stat)
      }
    }
    if(method==4){
      #method 4, dplyr and purrr::reduce
      #          gave up trying to pass args to purrr...
      ans <- lapply(test,
                    function(x){
                      data$.rstat <- data[[x]]
                      #removed pipe from next section of code
                      #(not sure about trade-offs)
                      df.stat <- dplyr::group_by(dates, start.date,
                                                 end.date)
                      df.stat <- dplyr::summarize(df.stat,
                                                  rstat = stat(data$.rstat[data$date >= start.date &
                                                                             data$date < end.date]))
                      names(df.stat)[names(df.stat)=="rstat"] <- x
                      df.stat
                    })
      df.stat <- suppressMessages(purrr::reduce(ans,  dplyr::full_join))
    }

    #if openair.like
    if(is.character(.xargs[["openair.like"]])){
      ####################
      #reset of ws/wd handers if there
      if ("wd" %in% names(df.stat)) {
        if (is.numeric(df.stat$wd)) {
          df.stat$wd <- as.vector(atan2(df.stat$.Uu, df.stat$.Vv) * 360/2/pi)
          ids <- which(df.stat$wd < 0)
          df.stat$wd[ids] <- df.stat$wd[ids] + 360
          if ("ws" %in% names(df.stat)) {
            if("vector.ws" %in% .xargs$openair.like){
              df.stat$ws <- (df.stat$.Uu^2 + df.stat$.Vv^2)^0.5
            }
          }
        }
        df.stat <- df.stat[!names(df.stat) %in% c(".Uu", ".Vv")]
      }
    }
    #looking for faster ways of doing this...

    if(.d.cls[1] %in% c("tbl", "tbl_df")){
      df.stat <- dplyr::as_tibble(df.stat)
    }
    df.stat
  }


#' @rdname calculate.stats
#' @export
calcRollingDateRangeStat <-
  function(data,
           range = "year", res = "day",
           stat = NULL, pollutant = NULL,
           from = NULL, to = NULL,
           ..., method=2){
    #calculates stat for rolling date range
    #think about to and from?
    #ignores all sub-grouping (see calcDateRangeStat)

    #works at res(olution) at moment because it is tidier
    #might want option to turn this off?
    temp <- strsplit(res, " ")[[1]]
    temp <- temp[length(temp)]
    start <- round.POSIXt(min(data$date, na.rm=TRUE), units=temp)
    end <- round.POSIXt(max(data$date, na.rm=TRUE), units=temp)

    #calculating at end of range
    end <- seq(start, end, by=res)

    #using lubridate because of leap years, etc...
    if(length(grep(" ", range))==1){
      temp <- strsplit(range, " ")[[1]]
      step <- period(as.numeric(temp[1]), temp[[2]])
    } else {
      step <- period(1, range)
    }
    start <- end %m-% step

    from <- aqe_prepFromDate(from, data)
    to <- aqe_prepToDate(to, data)

    dates <- data.frame(start = start, end = end)
    dates <- dplyr::filter(dates,
                           start >= from,
                           end <= to)

    calcDateRangeStat(data, dates$start, dates$end, stat,
                      pollutant,
                      method=method, ...)
  }



############################################
#unexported code

#could be using a few of these elsewhere?
#would standardise handling of same name args...

aqe_prepFromDate <- function(from=NULL, data){
  if(is.null(from)){
    from <- min(data$date, na.rm=TRUE)
  }
  if(is.numeric(from)){
    #allowed numeric year
    from <- paste(from, "-01-01", sep="")
  }
  #assuming character from here
  test <- attributes(data$date)$tzone
  from <- if(is.null(test)){
    as.POSIXct(from, "%Y-%m-%d")
  } else {
    as.POSIXct(from, "%Y-%m-%d", tz=test)
  }
  from
}

aqe_prepToDate <- function(to=NULL, data){
  if(is.null(to)){
    to <- max(data$date, na.rm=TRUE)
  }
  if(is.numeric(to)){
    #allowed numeric year
    to <- paste(to, "-12-31 23:59:59", sep="")
  }
  #assuming character from here
  test <- attributes(data$date)$tzone
  to <- if(is.null(test)){
    as.POSIXct(to, "%Y-%m-%d %H:%M:%S")
  } else {
    as.POSIXct(to, "%Y-%m-%d %H:%M:%S", tz=test)
  }
  to
}

aqe_prepXArgs <- function(x.args, data, mod){
  if(is.null(x.args)){
    x.args <- if(is.null(mod)){
      c("date", "wd", "ws", "air_temp")
    } else {
      temp <- as.formula(paste("~", mod, sep=""))
      temp <- all.vars(temp[[2]])
      temp <- temp[!temp %in% c("year.day", "week.day",
                                "hour.day", "count")]
      temp <- unique(c("date", temp))
      temp
    }
  }
  x.args
}

aqe_prepPadData <- function(pad.data, data){
  if(is.null(pad.data)){
    stop("missing pad.data")
  }
  if(is.numeric(pad.data)){
    #allowed numeric year taken from data
    pad.data <- data[format(data$date, "%Y") %in%
                       pad.data,]
  }
  pad.data
}

aqe_padData <- function(from, to, data, pad.data, x.args,
                        diagnostic=FALSE){
  #x.args
  local.args <- x.args[x.args %in% names(pad.data)]
  #could check length of x.args?
  pad.data <- pad.data[local.args[local.args %in%
                                    names(pad.data)]]
  pad.data$padded <- TRUE
  data$padded <- FALSE
  #to do
  ####################################
  #pad from

  #pad to
  #note
  #this does not work if ref data is one year but not 2...
  #sort or or prevent?
  if(to > max(data$date, na.rm=TRUE)){
    if(diagnostic){
      print("padding end")
    }
    .year <- as.numeric(format(max(data$date,
                                   na.rm=TRUE),
                               "%Y"))
    pad <- TRUE
    while(as.numeric(pad)==1){

      ref <- pad.data
      t2 <- as.numeric(pad.data$date)
      t2 <- t2 - min(t2, na.rm=TRUE)
      t1 <- as.POSIXct(paste(.year, "-",
                             format(pad.data$date[1],
                                    "%m-%d %H:%M:%S"),
                             sep=""),
                       "%Y-%m-%d %H:%M:%S",
                       tz= attr(pad.data$date[1], "tzone"))
      ref$date <- t1 + t2
      ref0 <- filter(ref, date > max(data$date,
                                     na.rm=TRUE))
      #print(dim(ref))
      if(nrow(ref0)>0){
        data <- bind_rows(data, ref0)
      }
      if(to < max(data$date, na.rm=TRUE)){
        data <- filter(data, date <= to)
      }
      .year <- .year + 1
      test <- as.numeric(format(max(data$date,
                                    na.rm=TRUE),
                                "%Y"))
      if(test > .year) .year <- test

      if(to < max(ref$date, na.rm=TRUE)){
        pad <- FALSE
      }
    }
  }
  #out
  data
}

