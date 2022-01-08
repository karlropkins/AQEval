############################################
#' @title findBreakPoints
############################################

#' @name findBreakPoints
#' @description find break points in a time-series using
#' strucchange methods
#' @param data dataset containing time-series to be tested,
#' identified using pollutant, and a reference time-stamp
#' called 'date'
#' @param pollutant name of time-series
#' @param h data window to use as reference when looking for
#' breaks from previous trends, expressed as proportion of
#' time-series (0-1)
#' @param ... other parameters
#' @note This uses methods from strucchange package (see references) and
#' modified as suggested by the main author of strucchange to handle
#' missing cases. URL:
#' https://stackoverflow.com/questions/43243548/strucchange-not-reporting-breakdates.
#' @references Strucchange:
#'
#' Achim Zeileis, Friedrich Leisch, Kurt Hornik and Christian Kleiber
#' (2002). strucchange: An R Package for Testing for Structural Change
#' in Linear Regression Models. Journal of Statistical Software, 7(2),
#' 1-38. URL http://www.jstatsoft.org/v07/i02/.
#'
#' Achim Zeileis, Christian Kleiber, Walter Kraemer and Kurt Hornik
#' (2003). Testing and Dating of Structural Changes in Practice.
#' Computational Statistics & Data Analysis, 44, 109-123.

#findBreakPoints
#############################
#to do
#check NA handling and non-regular data handling
#    added an initial 'fix' but needs work
#diagnostic option for this function???
#need to check nothing in strucchange output
#    currently dropped is worth keeping...
#error catchers for?
#    expected time-series, date
#    bad calls, missing time-series
#    aqeval_... checkPrep
#finish documenting
#think about rename/manage/set data columns local function
#    aqeval_renameDataColumns?


#splatted function
#' @import strucchange
#' @export
findBreakPoints <-
  function (data, pollutant, h = 0.15, ...)
  {
    #require(strucchange)
    name.pol <- pollutant
    #NA handling based on
    #https://stackoverflow.com/questions/43243548/strucchange-not-reporting-breakdates
    #package author recommendation
    #but should look into better option...
    d <- na.omit(data.frame(ans = data[, name.pol],
                            date = data$date,
                            ..ref = 1:nrow(data)))
    names(d) <- c("ans", "date", "..ref")
    bp <- breakpoints(ans ~ 1, data = d, h = h, ...)
    if(is.logical(bp$breakpoints)) return(NULL)
    temp <- suppressWarnings(confint(bp)$confint)
    if(any(is.na(temp))){
      #print(dimnames(temp))
      fix <- c(t(temp))
      #https://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value
      fix <- fix[which(!is.na(fix))[c(1,1:sum(!is.na(fix)))][cumsum(!is.na(fix))+1]]
      temp <- matrix(fix, ncol=ncol(temp),
                     dimnames = dimnames(temp))
      #print(temp)
      warning("findBreakPoints: some suspect confidences",
              "\n\tTried to fix but strongly recommend",
              "\n\tre-running with different range or 'h'\n",
              call.=FALSE)
    }
    if(any(temp<2)){
      #not sure about this...
      #but may kill later code if ignored...
      temp[temp<2] <- 2
      warning("findBreakPoints: break(s) too near '", pollutant,
              "' start...\n\tTried to fix but strongly recommend",
              "\n\tre-running with different range or 'h'\n", call.=FALSE)
    }
    if(any(temp>(nrow(d)-1))){
      #not sure about this...
      #but may kill later code if ignored...
      temp[temp>(nrow(d)-1)] <- (nrow(d)-1)
      warning("findBreakPoints: break(s) too near '", pollutant,
              "' end...\n\tTried to fix but strongly recommend",
              "\n\tre-running with different range or 'h'\n",
              call.=FALSE)
    }
    #my.bp <- d$date[temp]
    #ans <- as.data.frame(t(matrix(which(data$date %in% my.bp),
    #                                  ncol(temp))))
    #print(temp)
    my.bp <- d$..ref[temp]
    ans <- as.data.frame(t(matrix(my.bp, ncol(temp), byrow = TRUE)))
    names(ans) <- c("lower", "bpt", "upper")
    ans
  }
## wrapper so code using earlier name still works
## currently not exporting
## @export
#findBreakPoints01 <- function(...) findBreakPoints(...)

