#' @name aqeval.generics
#' @title aqeval.generics
#' @aliases aqe.generics
#' @description Generic functions for use with \code{aqe} object class for
#' \code{AQEval} outputs.
#'
#' @param x the \code{aqe}
#' object to be printed, plotted, etc.
#' @param ... additional arguments, typically passed on to next method or
#' ignored

#############################################

# Didn't want to do this
# and might not keep it if I cannot sort the + ggplot object option...

#' @rdname aqeval.generics
#' @method print aqe
#' @export

print.aqe <- function(x, ...){

  if("breaks" %in% names(x)){
     message("AQEval: breaks")
     aqe_summariseBreakPointsReport(x$report)
  }
  if("segments" %in% names(x)){
    message("AQEval: segments")
    aqe_summariseBreakSegmentsReport(x$report)
  }
  plot(x$plot)
  return(invisible(x))
}

#' @rdname aqeval.generics
#' @method plot aqe
#' @export

plot.aqe <- function(x, ...){
  x$plot
}


# #' @rdname aqeval.generics
# #' @method + aqe
# #' @export

# `+.aqe` <-
# function (e1, e2)
# {
#   temp <- e1$plot
#   print(class(temp))
#   return(e1)
# }
