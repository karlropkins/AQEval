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
#' statistic.
#' @param from (various) Start date(s) to subsample from when
#' calculating statistic, by default end of supplied
#' \code{data} date range.
#' @param to (various) End date(s) to subsample to when
#' calculating statistic, by default end of supplied
#' \code{data} date range.
#' @param stat (function) Statistic to be applied to selected
#' data, by default \code{mean(pollutant, na.rm=TRUE)}.
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


#' @rdname calculate.stats
#' @export
calcDateRangeStat <-
  function(data, from = NULL, to = NULL,
           stat = NULL, pollutant = NULL,
           ..., method=2){
    #calculates stat for all data in date range
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

    test <- if(is.null(pollutant)){
      names(data)[names(data)!="date"]
    } else {
      pollutant
    }

    #method 1 faster than 2/but neither great
    if(method==1){
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
      df.stat <- purrr::reduce(ans, dplyr::full_join)
    }
    if(method==2){
      ans <- lapply(1:nrow(dates), function(x){
        temp <- data[data$date >= dates$start.date[x] & data$date < dates$end.date[x], test]
        x.df <- sapply(temp,FUN=function(x) stat(x))
        data.frame(start.date=dates$start.date[x],
                   end.date=dates$end.date[x],
                   t(x.df))
      })
      df.stat <- dplyr::bind_rows(ans)
      #think about this
      #or making same class as input?
      ##df.stat <- dplyr::as_tibble(df.stat)
      ##eval(parse(text = paste0("as.", class, "(df.stat, ...)")))
      if(class(data)[1] %in% c("tbl", "tbl_df")){
        df.stat <- dplyr::as_tibble(df.stat)
      }
    }
    #looking for faster ways of doing this...
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

