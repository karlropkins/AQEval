############################################
#' @title find and test break-points
############################################

#' @name find.breaks
#' @aliases findBreakPoints textBreakSegments
#' @description Finding and testing break-points in
#' conventionally formatted air quality data sets.
#' @param data Data source, typically a \code{data.frame}
#' or similar, containing data-series to apply function to
#' and a paired time-stamped data-series, called \code{date}.
#' @param pollutant Name of time-series, assumed to be
#' a column in \code{date}.
#' @param h (\code{findBreakPoints} only) The data/time window
#' size to use when looking for breaks in a supplied time-series,
#' expressed as proportion of time-series (0-1), default 0.15.
#' @param ... other parameters
#' @param breaks (\code{testBreakPoints} only) \code{data.frame}
#' of The break-points and confidence intervals, typically a
#' \code{findBreakPoints} output.
#' @details  \code{findBreakPoints} uses methods from
#' \code{strucchange} package (see references) and
#' modifications as suggested by the main author of
#' \code{strucchange} to handle missing cases to find
#' potential breaks-points in a supplied time-series.
#'
#' \code{testBreakPoints} tests and identifies most likely
#' break-points using methods proposed for use with
#' \code{quantBreakPoints} and \code{quantBreakSegments}
#' and conventionally formatted air quality data sets.
#' @returns \code{findBreakPoints} returns a \code{data.frame}
#' of found break-points.
#'
#' \code{testBreakPoints} return a likely break-point/segment
#' report.
#' @references
#' Regarding \code{strucchange} methods see
#' \code{\link[strucchange]{breakpoints}}, and:
#'
#' Achim Zeileis, Friedrich Leisch, Kurt Hornik and Christian Kleiber
#' (2002). strucchange: An R Package for Testing for Structural Change
#' in Linear Regression Models. Journal of Statistical Software, 7(2),
#' 1-38. URL \url{https://www.jstatsoft.org/v07/i02/}.
#'
#' Achim Zeileis, Christian Kleiber, Walter Kraemer and Kurt Hornik
#' (2003). Testing and Dating of Structural Changes in Practice.
#' Computational Statistics & Data Analysis, 44, 109-123.
#'
#' Regarding missing data handling, see:
#'
#' URL:
#' \url{https://stackoverflow.com/questions/43243548/strucchange-not-reporting-breakdates}.
#'
#' Regarding \code{testBreakPoints}, see:
#'
#' Ropkins, K., Walker, A., Philips, I., Rushton, C., Clark, T. and
#' Tate, J., Change Detection of Air Quality Time-Series Using the
#' R Package AEQval. Available at SSRN 4267722.
#' https://ssrn.com/abstract=4267722 or http://dx.doi.org/10.2139/ssrn.4267722
#' Also at: https://karlropkins.github.io/AQEval/articles/AQEval_Intro_Preprint.pdf
#' @seealso
#' \code{\link{find.breaks}}.


#############################
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
#' @importFrom stats approx as.formula confint filter lm na.omit pchisq predict qchisq spec.pgram
#' @rdname find.breaks
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


#testBreakPoints
#kr v.0.0.1
#############################
#to do

#' @export
#' @rdname find.breaks
#' @importFrom dplyr bind_rows
testBreakPoints <-
  function (data, pollutant, breaks, ...)
  {
    #test identified break points
    #test zero breaks model...
    data <- aqe_RowNameFix(data)
    mod <- aqe_fitBreakPointsModel(data, pollutant,
                                   breaks=NULL)
    ans <- aqe_makeBreakPointTestReport(mod, 0, NA)
    #if breaks check each model
    #print("here")
    if(!is.null(breaks) && nrow(breaks)>0){
      for(i in 1:nrow(breaks)){
        temp <- combn(1:nrow(breaks), i,
                      simplify = FALSE)
        for(j in 1:length(temp)){
          mod <- aqe_fitBreakPointsModel(data,
                                         pollutant, breaks=breaks[temp[[j]],])
          ans <- bind_rows(aqe_makeBreakPointTestReport(
            mod, i, temp[[j]]), ans)
        }
      }
    }
    ans$suggest <- if(all(is.na(ans$adj.r.sq))){
      ""
    } else {
      ifelse(!is.na(ans$adj.r.sq) &
               ans$adj.r.sq == max(ans$adj.r.sq, na.rm=TRUE),
             "(<-)", "")
    }
    ans
  }


#local function
aqe_makeBreakPointTestReport <-
  function(mod, elements, breaks){
    signif <- all(summary(mod)$coefficients[,4]<0.05)
    breaks <- paste(breaks, sep="", collapse = "+")
    if(!signif){
      data.frame(elements = elements, breaks = breaks,
                 signif = signif, adj.r.sq = NA,
                 stringsAsFactors = FALSE)
    } else {
      adj.r.sq <- summary(mod)$adj.r.squared
      data.frame(elements = elements, breaks = breaks,
                 signif = signif, adj.r.sq = adj.r.sq,
                 stringsAsFactors = FALSE)
    }
  }

