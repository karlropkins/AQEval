############################################
#' @title testBreakPoints
############################################

#' @name testBreakPoints
#' @description test break points identified using
#' findBreakPoints
#' @param data dataset containing time-series to be tested,
#' and a reference time-stamp called 'date'
#' @param pollutant name of time-series to be tested
#' @param breaks The break points and confidence intervals,
#' typically a findBreakPoints function output
#' @param ... other parameters

#testBreakPoints
#kr v.0.0.1
#############################
#to do

#' @export
#' @importFrom dplyr bind_rows
testBreakPoints <-
  function (data, pollutant, breaks, ...)
  {
    #test identified break points
    #test zero breaks model...
    mod <- aqeval_fitBreakPointsModel(data, pollutant,
                                      breaks=NULL)
    ans <- aqeval_makeBreakPointTestReport(mod, 0, NA)
    #if breaks check each model
#print("here")
    if(!is.null(breaks) && nrow(breaks)>0){
      for(i in 1:nrow(breaks)){
        temp <- combn(1:nrow(breaks), i,
                      simplify = FALSE)
        for(j in 1:length(temp)){
          mod <- aqeval_fitBreakPointsModel(data,
                      pollutant, breaks=breaks[temp[[j]],])
          ans <- bind_rows(aqeval_makeBreakPointTestReport(
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
aqeval_makeBreakPointTestReport <-
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
