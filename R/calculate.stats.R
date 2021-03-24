#################################################
#' @title Some functions to calculate statistics
#################################################

#' @name calculate.stats
#' @aliases calcDateRangeStat calcRollingDateRangeStat
#' @description Calculate data set statistics for
#' selected time intervals.
#' @param data (data.frame, tibble, etc) Data set containing
#' data statistic to be calculated for and \code{date}
#' column of date/time records.
#' @param pollutant (character) The name(s) of data-series to
#' analyse in \code{data}, by default all columns in
#' supplied data except \code{date}.
#' @param only.last (logical) Just calculate last statistic.
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
    start <- round(min(data$date, na.rm=TRUE), units=temp)
    end <- round(max(data$date, na.rm=TRUE), units=temp)

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
