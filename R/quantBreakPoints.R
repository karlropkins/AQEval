############################################
#' @title quantBreakPoints
############################################

#' @name quantBreakPoints
#' @description Quantifies Break Points, assuming a
#' model based on general trend plus any supplied break
#' points.
#' @param data Data source, typically a data.frame or similar,
#' containing data-series to model and a paired timestamp
#' data-series, named date.
#' @param pollutant The data-series to model.
#' @param breaks The break points and confidence intervals,
#' typically a findBreakPoints function output.
#' @param ylab Y-label term, by default pollutant.
#' @param xlab X-label term, by default date.
#' @param pt.col Point fill and line colours for plot,
#' defaults lightgrey and darkgrey.
#' @param line.col Line colour for plot, default red.
#' @param break.col Break point colour for plot, default
#' blue.
#' @param show What to show before returning the break-point
#' quantification mode, by default plot and report.
#' @param ... other parameters
#' @note quantBreakPoints is the default version of this function;
#' quantBreaks01 and quantBreaks02, now removed, were early versions used in
#' testing and optimisation work.
#' @examples
#' #using openair timeAverage
#' temp <- openair::timeAverage(aq.my1, "4 day")
#' quantBreakPoints(temp, "no2")

#quantBreakPoints
################################
#need to tidy
#    most local functions using data2 rather than data
#recent rebuild to isolate model prediction


#splatted function
#' @export
quantBreakPoints <-
  function (data, pollutant, breaks, ylab = NULL,
            xlab = NULL, pt.col = c("lightgrey", "darkgrey"),
            line.col = "red", break.col ="blue",
            show = c("plot", "report"), ...)
  {
##########################
#need a checker for date and pollutant
#does breaks default want to be NA
#some code in next section could move into subfunctions
##########################
    name.pol <- pollutant
    pollutant <- data[, name.pol]
    if(missing(breaks)){
      #if breaks missing build assuming
      #what sent plus defaults
      breaks <- aqe_buildBreaks(data, name.pol, ...)
    }
    x.args <- list(...)
#print("fit")
################
    data2 <- data[,c("date", name.pol)]
    mod <- aqe_fitBreakPointsModel(data2, name.pol, breaks)
################
    data2$pred <- rep(NA, nrow(data2))
    data2$err <- data2$pred
    ans <- predict(mod, se.fit = TRUE)
    data2$pred[as.numeric(names(ans$fit))] <- ans$fit
    data2$err[as.numeric(names(ans$fit))] <- ans$se.fit
#print("report")
################
    report <- aqe_makeBreakPointsReport(data2, breaks)
#################
#################
    if ("report" %in% show) {
      aqe_summariseBreakPointsReport(report)
    }
#print("plot")
######
    ####################
    #not sure this is best way to handle
    #auto.text=FALSE...
    #####################
    auto.text <- if("auto.text" %in% names(x.args)){
      x.args$auto.text
    } else { TRUE }
    plt <- aqe_plotQuantBreakPoints(data2, name.pol, breaks,
                  xlab=xlab, ylab=ylab, pt.col=pt.col,
                  line.col=line.col, break.col=break.col,
                  auto.text=auto.text)
    if ("plot" %in% show) {
      plot(plt)
    }
    return(invisible(list(data = data, breaks = breaks, data2 = data2,
                          plot = plt, report = report, model = mod)))
  }

#wrappers for above
#not exporting...

## @export
##quantBreaks02 <- function(...) quantBreakPoints(...)

#sub functions
#not exported
#may change if methods change

aqe_buildBreaks <- function(data, name.pol, ...){
  breaks <- findBreakPoints(data, name.pol, ...)
  x.args <- list(...)
  if("test" %in% names(x.args) && !x.args$test){
    cat("Using all ", nrow(breaks), " suggested breaks\n",
        sep="")
    breaks
  } else {
    test <- testBreakPoints(data, name.pol, breaks)
    temp <- which(test$suggest=="(<-)")
    if(length(temp)<1) return(breaks)
    if(all(test$breaks[temp]=="NA")) return(NULL)
    temp <- as.numeric(strsplit(test$breaks[temp],
                        "[+]")[[1]])
    cat("Using ", length(temp), " of ", nrow(breaks),
        " suggested breaks: ", paste(temp, collapse = ",",
                                   sep=","), "\n", sep="")
    breaks[temp,]
  }
}


#this needs tidying
aqe_plotQuantBreakPoints <- function(data, name.pol, breaks,
                                        ylab = NULL, xlab = NULL,
                                        pt.col = c("lightgrey", "darkgrey"),
                                        line.col = "red", break.col ="blue",
                                        scalelabs =c("data", "trend",
                                                     "break"),
                                        auto.text = TRUE){
  #think about default declarations...
  #   have to match these and those in main function at moment...
  if(is.null(ylab)) ylab <- name.pol
  if(is.null(xlab)) xlab <- "date"
  if(length(pt.col)<2) pt.col <- rep(pt.col, 2)
  #plot
  temp <- data
  names(temp)[names(temp) == name.pol] <- "my.y"

  plt <- ggplot2::ggplot(data = temp,
                       ggplot2::aes(x = date, y = my.y,
                                    ymin = pred -(1.96 * err),
                                    ymax = pred + (1.96 * err))) +
  ggplot2::geom_point(col = pt.col[2],
                      fill = pt.col[1],
                      ggplot2::aes(pch = "data"),
                      na.rm = TRUE)

  if(is.data.frame(breaks) && nrow(breaks)>0){
    plt <- plt +
      ggplot2::geom_vline(data=breaks,
                        ggplot2::aes(xintercept = temp$date[breaks[,1]],
                                     #col="confidence",
                                     linetype = "confidence"),
                        col=break.col) +
      ggplot2::geom_vline(data=breaks,
                        ggplot2::aes(xintercept = temp$date[breaks[,2]],
                                     #col="break",
                                     linetype="break"),
                        col=break.col) +
      ggplot2::geom_vline(data=breaks,
                        ggplot2::aes(xintercept = temp$date[breaks[,3]],
                                     #col="confidence",
                                     linetype = "confidence"),
                        col=break.col)
  }
  plt <- plt +
    ggplot2::geom_ribbon(ggplot2::aes(fill = "  confidence"),
                       alpha = 0.25) +
    ggplot2::geom_path(ggplot2::aes(y = pred, col = "  trend")) +
    ggplot2::ylab(aqe_quickText(ylab, auto.text)) +
    ggplot2::xlab(aqe_quickText(xlab, auto.text)) +
    ggplot2::scale_shape_manual(name="",
                              values=c(21),
                              labels=c(scalelabs[1]))+
    ggplot2::scale_color_manual(name="",
                              values=c(line.col),
                              labels=c(paste("  ", scalelabs[2], sep=""))) +
    ggplot2::scale_fill_manual(name="",
                             values=c(line.col),
                             labels=c("  confidence")) +
    ggplot2::scale_linetype_manual(name="",
                                 values=c("solid", "dotted"),
                                 labels=c(scalelabs[3], "confidence"))+
    #ggplot2::scale_color_manual(name="breaks",
    #                            values=c(break.col, break.col),
    #                            labels=c("break", "confidence")) +

    ggplot2::guides(
      shape = ggplot2::guide_legend(order = 1),
      color = ggplot2::guide_legend(order = 2),
      fill = ggplot2::guide_legend(order = 3)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position="top",
                 legend.spacing.x = ggplot2::unit(0, 'cm'),
                 axis.title.y = ggtext::element_markdown(),
                 axis.title.x = ggtext::element_markdown())
  #output
  plt
}


aqe_fitBreakPointsModel <- function(data, name.pol, breaks){
  bpts <- breaks$bpt
  #############################
  #this needs thinking about
  #assumes data regular???
  #############################
  counter <- 1:nrow(data)
  counter <- ifelse(counter %in% bpts, 1, 0)
  counter <- factor(paste("SGM", cumsum(counter) + 1, sep = ""))
  data$counter <- counter
  counter2 <- 1:length(levels(counter))
  ref <- paste("x", counter2, sep = "")
  for (i in 1:length(counter2)) {
    ii <- levels(counter)[i]
    p <- rep(0, nrow(data))
    p[data$counter == ii] <- data$date[data$counter == ii]
    data[, ref[i]] <- p
  }
  ff <- paste(name.pol, "~", sep = "")
  ff <- as.formula(paste(ff, paste(ref, collapse = "+"), sep = ""))
  #output
  lm(ff, data = data)
}


aqe_makeBreakPointsReport <- function(data, breaks){
  bpts <- breaks$bpt
  if (length(bpts) > 0) {
    ls <- lapply(1:length(bpts), function(i) {
      temp <- data[(bpts[i] - 1):(bpts[i]), c("pred", "err")]
      temp <- as.data.frame(temp)
      out <- data.frame(date = data$date[bpts[i]],
                        date.low = data$date[breaks[i,1]],
                        date.high = data$date[breaks[i,3]],
                        c0 = signif(temp[1,1], 4),
                        c1 = signif(temp[2, 1], 4),
                        c.delta = signif(temp[2,1] - temp[1, 1], 4),
                        per.delta = signif(((temp[2,1] - temp[1, 1])/temp[1, 1]) * 100, 2))
      temp[1, 1] <- temp[1, 1] + (1.96 * temp[1, 2])
      temp[2, 1] <- temp[2, 1] - (1.96 * temp[2, 2])
      out <- cbind(out, data.frame(upper.c0 = signif(temp[1, 1], 4),
                                   upper.c1 = signif(temp[2, 1], 4),
                                   upper.c.delta = signif(temp[2, 1] - temp[1, 1], 4),
                                   upper.per.delta = signif(((temp[2, 1] - temp[1, 1])/temp[1, 1]) * 100, 2)))
      temp <- data[(bpts[i] - 1):(bpts[i]), c("pred", "err")]
      temp <- as.data.frame(temp)
      temp[1, 1] <- temp[1, 1] - (1.96 * temp[1, 2])
      temp[2, 1] <- temp[2, 1] + (1.96 * temp[2, 2])
      cbind(out, data.frame(lower.c0 = signif(temp[1, 1], 4),
                            lower.c1 = signif(temp[2, 1], 4),
                            lower.c.delta = signif(temp[2, 1] - temp[1, 1], 4),
                            lower.per.delta = signif(((temp[2, 1] - temp[1, 1])/temp[1, 1]) * 100, 2)))
    })
    do.call(rbind, ls)
  }
  else {
    NULL
  }
}


aqe_summariseBreakPointsReport <- function(report){
  if (is.null(report)) {
    cat("no break points declared...\n")
  }
  else {
    for (i in 1:nrow(report)) {
      cat("\n", as.character(report[i, 1]), " (",
          as.character(report[i, 2]), " to ",
          as.character(report[i, 3]), ")\n",
          sep = "")
      cat(report[i, 4], "->", report[i, 5], ";",
          report[i, 6], " (", report[i, 7], "%)\n", sep = "")
      #########################
      #to do
      #########################
      #include option for old update
    }
  }
}

#this is older version of above
#replace to make outputs more consistent
#but could re-introduce but would probably
#want to have similar option for quantBreakSegments
aqe_summariseBreakPointsReport.old <- function(report){
  if (is.null(report)) {
    cat("no breakpoints declared...\n")
  }
  else {
    for (i in 1:nrow(report)) {
      cat("\n", as.character(report[i, 1]), "(",
          as.character(report[i, 2]), "->",
          as.character(report[i, 3]), ")\n",
          sep = "")
      cat(report[i, 4], "->", report[i, 5], ";",
          report[i, 6], " (", report[i, 7], "%)\n", sep = "")
      cat("[Upper] ", report[i, 8], "->", report[i, 9],
          ";", report[i, 10], " (", report[i, 11],
          "%)\n", sep = "")
      cat("[Lower]", report[i, 12], "->",
          report[i, 13], ";", report[i, 14], " (",
          report[i, 15],
          "%)\n", sep = "")
    }
  }
}


##############################
#new / not fully tested
#this is local alternative to my openair::quickText
#needed this because that does not play nicely with
#multi-line axis labels and ggplot2
#(folks seem to want both...)
#
#notes
#this needs ggtext::element_markdown() in ggplot theme
#or similar..
#seems to be a character spacing issue in rstudio
#console outputs but not r console or studio
#markdown...
#raised with ggtext admin
#[link]
#needs graphics drivers sorted... IT for admin systems...
##############################

aqe_quickText <- function (text, auto.text = TRUE)
{

  #openair::quicktext alternative for aqe

  #needed because (1) openair quicktext cannot cope
  #with super and subscripts and multiple lines of text
  #in ggplots...
  #and we are regularly going to three lines for
  #some report figures

  #this uses ggtext
#need to add ggtext to imports...

  #chasing something like...
  #plt + xlab("") +
  #   ylab("jjjjjj<br>NO<sub>2</sub><br>[&mu;g.m<sup>-3</sup>]") +
  #   theme(axis.title.y = element_markdown())


    if (!auto.text)
      return(ans <- text)
    #currently based on openair quicktext
    ans <- text
    ans <- gsub("NO2", "NO<sub>2</sub>", ans)
    ans <- gsub("no2", "NO<sub>2</sub>", ans)
    ans <- gsub("NOX", "NO<sub>x</sub>", ans)
    ans <- gsub("nox", "NO<sub>x</sub>", ans)
    ans <- gsub("NOx", "NO<sub>x</sub>", ans)
    ans <- gsub("NH3", "NH<sub>3</sub>", ans)
    ans <- gsub("nh3", "NH<sub>3</sub>", ans)
    ans <- gsub("co ", "CO ", ans)
    ans <- gsub("co,", "CO,", ans)
    ans <- gsub("nmhc", "NHHC", ans)
    ans <- if (nchar(as.character(text)) == 2 && length(grep("ws",
                                                             text)) > 0) {
      gsub("ws", "wind spd.", ans)
    }
    else {
      ans
    }
    ans <- gsub("wd", "wind dir.", ans)
    ans <- gsub("rh ", "relative humidity ",
                ans)
    ans <- gsub("PM10", "PM<sub>10</sub>", ans)
    ans <- gsub("pm10", "PM<sub>10</sub>", ans)
    ans <- gsub("pm1", "PM<sub>1</sub>", ans)
    ans <- gsub("PM1", "PM<sub>1</sub>", ans)
    ans <- gsub("PM4", "PM<sub>4</sub>", ans)
    ans <- gsub("pm4", "PM<sub>4</sub>", ans)
    ans <- gsub("PMtot", "PM<sub>total</sub>", ans)
    ans <- gsub("pmtot", "PM<sub>total</sub>", ans)
    ans <- gsub("pmc", "PM<sub>coarse</sub>", ans)
    ans <- gsub("pmcoarse", "PM<sub>coarse</sub>",
                ans)
    ans <- gsub("PMc", "PM<sub>coarse</sub>", ans)
    ans <- gsub("PMcoarse", "PM<sub>coarse</sub>",
                ans)
    ans <- gsub("pmf", "PM<sub>fine</sub>", ans)
    ans <- gsub("pmfine", "PM<sub>fine</sub>", ans)
    ans <- gsub("PMf", "PM<sub>fine</sub>", ans)
    ans <- gsub("PMfine", "PM<sub>fine</sub>", ans)
    ans <- gsub("PM2.5", "PM<sub>2.5</sub>", ans)
    ans <- gsub("pm2.5", "PM<sub>2.5</sub>", ans)
    ans <- gsub("pm25", "PM<sub>2.5</sub>", ans)
    ans <- gsub("PM2.5", "PM<sub>2.5</sub>", ans)
    ans <- gsub("PM25", "PM<sub>2.5</sub>", ans)
    ans <- gsub("pm25", "PM<sub>2.5</sub>", ans)
    ans <- gsub("O3", "O<sub>3</sub>", ans)
    ans <- gsub("o3", "O<sub>3</sub>", ans)
    ans <- gsub("ozone", "O<sub>3</sub>", ans)
    ans <- gsub("CO2", "CO<sub>2</sub>", ans)
    ans <- gsub("co2", "CO<sub>2</sub>", ans)
    ans <- gsub("SO2", "SO<sub>2</sub>", ans)
    ans <- gsub("so2", "SO<sub>2</sub>", ans)
    ans <- gsub("H2S", "H<sub>2</sub>S", ans)
    ans <- gsub("h2s", "H<sub>2</sub>S", ans)
    ans <- gsub("CH4", "CH<sub>4</sub>", ans)
    ans <- gsub("ch4", "CH<sub>4</sub>", ans)
    ans <- gsub("dgrC", "<sup>o</sup>C", ans)
    ans <- gsub("degreeC", "<sup>o</sup>C",
                ans)
    ans <- gsub("deg. C", "<sup>o</sup>C", ans)
    ans <- gsub("degreesC", "<sup>o</sup>C",
                ans)
#    ans <- gsub("degrees", "' * degree *'", ans)
#    ans <- gsub("Delta", "' * Delta *'", ans)
#    ans <- gsub("delta", "' * Delta *'", ans)
    ans <- gsub("ug/m3", "&mu;g.m<sup>-3</sup>",
                ans)
    ans <- gsub("ug.m-3", "&mu;g.m<sup>-3</sup>",
                ans)
    ans <- gsub("ug m-3", "&mu;g.m<sup>-3</sup>",
                ans)
    ans <- gsub("ugm-3", "&mu;g.m<sup>-3</sup>",
                ans)
    ans <- gsub("mg/m3", "mg.m<sup>-3</sup>",
                ans)
    ans <- gsub("mg.m-3", "mg.m<sup>-3</sup>",
                ans)
    ans <- gsub("mg m-3", "mg.m<sup>-3</sup>",
                ans)
    ans <- gsub("mgm-3", "mg.m<sup>-3</sup>",
                ans)
    ans <- gsub("ng/m3", "ng.m<sup>-3</sup>",
                ans)
    ans <- gsub("ng.m-3", "ng.m<sup>-3</sup>",
                ans)
    ans <- gsub("ng m-3", "ng.m<sup>-3</sup>",
                ans)
    ans <- gsub("ngm-3", "ng.m<sup>-3</sup>",
                ans)
    ans <- gsub("m/s2", "m.s<sup>-2</sup>", ans)
    ans <- gsub("m/s", "m.s<sup>-1</sup>", ans)
    ans <- gsub("m.s-1", "m.s<sup>-1</sup>", ans)
    ans <- gsub("m s-1", "m.s<sup>-1</sup>", ans)
    ans <- gsub("g/km", "g.km<sup>-1</sup>", ans)
    ans <- gsub("g/s", "g.s<sup>-1</sup>", ans)
    ans <- gsub("kW/t", "kW.t<sup>-1</sup>", ans)
    ans <- gsub("g/hour", "g.hour<sup>-1</sup>", ans)
    ans <- gsub("g/hr", "g.hour<sup>-1</sup>", ans)
    ans <- gsub("g/m3", "g.m<sup>-3</sup>", ans)
    ans <- gsub("g/kg", "g.kg<sup>-1</sup>", ans)
    ans <- gsub("km/hr/s", "km.hour<sup>-1</sup>s<sup>-1</sup>",
                ans)
    ans <- gsub("km/hour/s", "km.hour<sup>-1</sup>s<sup>-1</sup>",
                ans)
    ans <- gsub("km/h/s", "km.hour<sup>-1</sup>s<sup>-1</sup>",
                ans)
    ans <- gsub("km/hr", "km.hour<sup>-1", ans)
    ans <- gsub("km/h", "km.hour<sup>-1", ans)
    ans <- gsub("km/hour", "km.hour<sup>-1", ans)
    ans <- gsub("r2", "R<sup>2", ans)
    ans <- gsub("R2", "R<sup>2", ans)
    #ans <- gsub("tau ", "' * tau * '", ans)
    #ans <- gsub("umol/m2/s", "' * mu * 'mol m' ^-2 * ' s' ^-1 *'",
    #            ans)
    #ans <- gsub("umol/m2", "' * mu * 'mol m' ^-2 *'",
    #            ans)
    ans <- gsub("\n", "<br>", ans)
    ans
  }


