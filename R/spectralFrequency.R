############################################
#' @title Spectral Analysis
############################################

#' @name spectralFrequency
#' @description Time-series spectral frequency analysis.
#' @param data data.frame holding data to be analysed,
#' expected to contain a timestamp data-series called
#' date and a measurement time-series to be analysed
#' identified using the pollutant argument
#' @param pollutant the name of the time-series,
#' typically pollutant measurements, to be analysed
#' @param ... extra arguments
#' @returns \code{spectralFrequency} uses the \code{show} argument
#' to control which elements of the functions outputs
#' are shown but also invisible return a \code{list}
#' of all outputs which can caught using, e.g.:
#'
#' \code{sfa.mod <- spectralFrequency(data, pollutant)}
#' @details \code{spectralFrequency} producing a
#' time frequency analysis of the requested
#' \code{pollutant}.
#' @examples
#' #no2 spectral frequency
#' spectralFrequency(aq.data, "no2")

#splatted function
#spectralFrequency
#kr v.0.0.1  2019/11/02
#kr v.0.0.2  2019/11/15 (speed up regularization)
#kr v.0.0.3  2020/03/26 (moved plot to ggplot)
#(in development)
#args need tidying
#know issues/work to do
#look at regularization/hole filling - lomb?
#group handling
##think about colour scheme for stacking (yellow in middle)
#think about normalise
#think about ci
#labelling for key and facets via quickText
#   needs replacing with local version

#' @rdname spectral.analysis
#' @export
spectralFrequency <-
  function(data, pollutant, ...){
    ################
    #default settings plus user updates via '...'
    x.args <- loa::listUpdate(list(show = "plot",
                                   auto.text=TRUE,
                                   stack=FALSE, xlab="",
                                   log.x = TRUE,
                                   ylab=paste("spec(",
                                        paste(pollutant,
                                              collapse = ","),
                                        ")", sep="")
                                   ),
                              list(...))
#######################
#current bit
#######################
    if(!"col" %in% names(x.args)){
      temp <- length(pollutant)
      x.args$col <- loa::colHandler(z=1:(if(temp==1) 2 else temp),
                                    col.regions = loa::colHandler(1:4, col.regions = "Spectral"))
      if(temp==1){
        x.args$col <- x.args$col[2]
      } else {
        if("stack" %in% names(x.args) && x.args$stack){
          x.args$col <- rep(x.args$col[length(x.args$col)],
                            temp)
        }
      }
    }
######################
    ################
    #cut data back to just needed data
    data <- aqe_prepData(data, pollutant)
    #tidy data (regularise and holefill)
    data <- aqe_tidySpectralData(data)
    #not tracking by at moment
    mypol <- unlist(data[, pollutant])
    #could use align to do this - might be faster but ws/wd bad?
    #nicer way of getting mypol?

    ######################
    #spectral analysis
    #(don't want to spec.pgram plot)
    #(don't want to pass spec.pgram any lattice user settings)
    temp <- loa::listUpdate(list(plot=FALSE), x.args,
                            use.b=names(formals(spec.pgram)),
                            ignore.b=c("x", "plot"))
    spec.df <- lapply(pollutant, function(y){
      temp$x <- unlist(data[, y])
      spec.raw <- do.call(spec.pgram, temp)
      ans <- data.frame(freq = spec.raw$freq, spec = spec.raw$spec,
                        pollutant = y, stringsAsFactors = FALSE)
      #normalise
      ans$spec <- ans$spec/sum(ans$spec)
      ans
    })
    spec.df <- do.call(rbind, spec.df)
    spec.df$pollutant <- factor(spec.df$pollutant, levels=pollutant,
                                ordered=TRUE)
    #stripping arguments sent to spec.pgram
    temp <- names(temp)[names(temp)!="plot"]
    x.args <- x.args[!names(x.args) %in% temp]
    #currently not allowing grouping/subsetting...

    #############################
    #plot prep
    #############################
    #x-axis
    #periods and labels
    #units are in hours (assuming openair-like data)
    #(this could be better - currently assumes data-like timescale)
    period <- rev(c(1/(24*365*10), 1/(24*365*5), 1/(24*365),
                    1/(24*30*6), 1/(24*30), 1/(24*7), 1/24,
                    1/12, 1/6, 1/3, 1))
    labels <- rev(c("10 years", "5 years", "   1 year", "6 months",
                    "1 month", "  1 week", "    1 day", "12 hours",
                    "  6 hours", "  3 hours", "  1 hour"))
    spec.df$period <- 1/spec.df$freq

    if("auto.text" %in% names(x.args) && x.args$auto.text){
      x.args$xlab <- openair::quickText(x.args$xlab)
      x.args$ylab <- openair::quickText(x.args$ylab)
    }

    plt <- ggplot2::ggplot(data = subset(spec.df)) +
      ggplot2::geom_line(ggplot2::aes(x = freq, y = spec,
                                      col = pollutant)) +
      ggplot2::xlab(x.args$xlab) +
      ggplot2::ylab(x.args$ylab)

    if("stack" %in% names(x.args) && x.args$stack){
      #condition by pollutant
      plt <- plt +
        ggplot2::facet_grid(ggplot2::vars(pollutant))
    }

    if("log.x" %in% names(x.args) && x.args$log.x){
      plt <- plt +
        ggplot2::scale_x_log10(breaks = period,
                      labels = labels)
    } else {
      plt <- plt +
        ggplot2::scale_x_continuous()
    }
    if("log.y" %in% names(x.args) && x.args$log.y){
      plt <- plt +
        ggplot2::scale_y_log10()
    } else {
      plt <- plt +
        ggplot2::scale_y_continuous()
    }

    plt <- plt +
      ggplot2::scale_colour_manual(
        values = x.args$col) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                      hjust=1.1, vjust=1.1))

    if(length(pollutant)==1 || x.args$stack){
      plt <- plt +
        ggplot2::theme(legend.position = "none")
    }

    #####################
    #output
    if ("show" %in% names(x.args) &&
        "plot" %in% x.args$show) plot(plt)
    output <- list(plot = plt, data = spec.df,
                   call = match.call())
    #class(output) <- "openair"
    #need to decide what to do about class
    invisible(output)
}


#######################
#internal functions
#######################

#aqe_prepData
#kr v.0.0.1  2019/11/15
#checks all expected data there
#stops if not
#send back only what is needed
aqe_prepData<- function(data, pollutant, ...){
   temp <- unique(c("date", pollutant))
   if(!all(temp %in% names(data))){
     stop(paste("Expected data missing (",
                paste(temp[!temp %in% names(data)], collapse=", "),
                ")", sep=""),
          call.=FALSE)
   }
   data[temp]
}


#aqe_tidySpectralData
#kr v.0.0.1  2019/11/02
#might be better option via align
#but need to think about ws,wd handling
aqe_tidySpectralData<- function(data, by = "hour", ...){
  ######################
  #to think about
  #this currently holefills/regularises all columns
  ##(alternative? regularise in align?)
  ##################
  #version 1
  ###################
  #(very slow because of timeAverage)
  #ind <- openair:::find.time.interval(data$date)
  #temp <- openair::timeAverage(data, ind)
  #ref <- names(temp)[names(temp)!="date"]
  #for(i in ref){
  #  test <- unlist(temp[,i])
  #  if(any(is.na(test)))
  #    temp[,i] <- approx(temp$date, test, temp$date, rule=2)$y
  #}
  ts <- seq(data$date[1], data$date[nrow(data)], by = by)
  ref <- names(data)[names(data)!="date"]
  temp <- lapply(ref, function(x){
    approx(data$date, unlist(data[,x]), ts, rule=2)$y
  })
  temp <- as.data.frame(temp, stringsAsFactors = FALSE,
                        col.names = ref)
  temp$date <- ts
  temp[unique(c("date", ref))]
}


######################
#work of others
#from https://stats.stackexchange.com/questions/26244/what-is-the-confidence-interval-calculated-in-a-spectral-density-periodogram-in
#not sure what this is...
spec.ci <- function(spec.obj, coverage = 0.95) {
  if (coverage < 0 || coverage >= 1)
    stop("coverage probability out of range [0,1)")
  tail <- (1 - coverage)
  df <- spec.obj$df
  upper.quantile <- 1 - tail * pchisq(df, df, lower.tail = FALSE)
  lower.quantile <- tail * pchisq(df, df)
  1/(qchisq(c(upper.quantile, lower.quantile), df)/df)
}

#also method in plot.spec.coherence
#that might be worth looking at...
#draws bands...
