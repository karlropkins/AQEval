#############################################
#misc set up
#should tidy this...
#############################################
#defined globals
#' @importFrom utils capture.output combn flush.console
#' @importFrom grDevices grey
#' @importFrom graphics abline lines par points segments
#' @importFrom stats coef dnorm fitted formula is.empty.model
#' lm.fit lm.wfit model.matrix model.offset model.response
#' model.weights pnorm pt qnorm qt quantile residuals runif
#' sd splinefun summary.glm summary.lm update update.formula
#' vcov weights

#############################################
#undefined globals
utils::globalVariables(c("end.date", "err", "freq", "my.y",
                         "pred", "spec", "start.date",
                         "splineDesign", "spline.des", "segmented",
                         "seg.control", "seg.lm.fit"))


##############################################
#misc sub functions
#not exported
#may change if methods change
##############################################

aqe_RowNameFix <- function(data){
  data <- as.data.frame(data)
  row.names(data) <- 1:nrow(data)
  data
}



aqe_buildBreaks <- function(data, name.pol, ...){
  breaks <- findBreakPoints(data, name.pol, ...)
  x.args <- list(...)
  if("test" %in% names(x.args) && !x.args$test){
    message("Using all ", nrow(breaks), " suggested breaks",
        sep="")
    breaks
  } else {
    test <- testBreakPoints(data, name.pol, breaks)
    temp <- which(test$suggest=="(<-)")
    if(length(temp)<1) return(breaks)
    if(all(test$breaks[temp]=="NA")) return(NULL)
    temp <- as.numeric(strsplit(test$breaks[temp],
                                "[+]")[[1]])
    message("Using ", length(temp), " of ", nrow(breaks),
        " suggested breaks: ", paste(temp, collapse = ",",
                                     sep=","), sep="")
    breaks[temp,]
  }
}


#this needs tidying
aqe_plotQuantBreakPoints <- function(data, name.pol, breaks,
                                     ylab = NULL, xlab = NULL,
                                     pt.col = c("lightgrey", "darkgrey"),
                                     line.col = "red", break.col ="blue",
                                     scalelabs = c("data", "trend", "break"),
                                     event = NULL, auto.text = TRUE, ...){
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
  if(!is.null(event)){
    #requested intervention marker
    #(not a fan of this)
    if(!is.list(event)){
      warning("expecting event to be list; ignoring; see help")
    } else {
      event <- loa::listUpdate(list(label=NA, y=NA, x=NA, col="black",
                                  hjust=1, line.size=0.5,
                                  font.size=5),
                               event)
      if(is.na(event$y)) {
        ref <- ggplot2::layer_scales(plt)$y$range$range
        event$y <- ((ref[2] - ref[1])*0.9) + ref[1]
      }
      if(is.character(event$x)){
        event$x <- as.POSIXct(event$x)
      }
      if(!is.na(event$x)){
        plt <- plt + ggplot2::geom_vline(xintercept=event$x, col=event$col,
                                       size=event$line.size)
        if(!is.na(event$label)){
          #this will not work line very thick...
          if(event$hjust==0) {
            event$label <- paste(" ", event$label, sep="")
            event$label <- gsub("\n", "\n ", event$label)
          }
          if(event$hjust==1) {
            event$label <- paste(event$label, " ", sep="")
            event$label <- gsub("\n", " \n", event$label)
          }
          plt <- plt + ggplot2::geom_text(label=event$label, x=event$x,
                                          y=event$y, col=event$col,
                                          hjust = event$hjust, size=event$font.size)
        }
      }
    }
  }
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
    message("no break points declared...")
  }
  else {
    for (i in 1:nrow(report)) {
      message("\n", as.character(report[i, 1]), " (",
          as.character(report[i, 2]), " to ",
          as.character(report[i, 3]), ")",
          sep = "")
      message(report[i, 4], "->", report[i, 5], ";",
          report[i, 6], " (", report[i, 7], "%)", sep = "")
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
    message("no breakpoints declared...")
  }
  else {
    for (i in 1:nrow(report)) {
      message("\n", as.character(report[i, 1]), "(",
          as.character(report[i, 2]), "->",
          as.character(report[i, 3]), ")",
          sep = "")
      message(report[i, 4], "->", report[i, 5], ";",
          report[i, 6], " (", report[i, 7], "%)\n", sep = "")
      message("[Upper] ", report[i, 8], "->", report[i, 9],
          ";", report[i, 10], " (", report[i, 11],
          "%)", sep = "")
      message("[Lower]", report[i, 12], "->",
          report[i, 13], ";", report[i, 14], " (",
          report[i, 15],
          "%)", sep = "")
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
  #   ylab("top line<br>NO<sub>2</sub><br>[&mu;g.m<sup>-3</sup>]") +
  #   theme(axis.title.y = element_markdown())


  if (!auto.text)
    return(ans <- text)
  #currently based on openair quicktext
  #check can we make gsub case non-sensitive
  #without big speed penalty???
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

##############################
#break-segments
##############################

aqe_makeBreakSegmentsReport <- function(data, segments){
  if(!is.null(segments) && nrow(segments)>0){
    #this needs tidying
    #err ranges need calculating
    data.frame(s1.date1 = data$date[segments[, 2]],
               s1.date2 = data$date[segments[, 5]],
               s1.date.delta = data$date[segments[, 5]] -
                 data$date[segments[, 2]],
               s1.c0 = data$pred[segments[, 2]],
               s1.c1 = data$pred[segments[, 5]],
               s1.c.delta = data$pred[segments[, 5]] -
                 data$pred[segments[, 2]],
               s1.per.delta = (data$pred[segments[, 5]] -
                                 data$pred[segments[, 2]]) /
                 data$pred[segments[, 2]] * 100,
               #################################
               #to do
               #################################
               #diffs to calculate/report
               #confidences to report
               stringsAsFactors = FALSE)
  } else { NULL }
}



aqe_plotQuantBreakSegments01 <- function(data, name.pol, segments,
                                            ylab = NULL, xlab = NULL,
                                            pt.col = c("lightgrey", "darkgrey"),
                                            line.col = "red", break.col ="blue",
                                            scalelabs = c("data", "trend",
                                                          "change"),
                                         event = NULL,
                                         auto.text=TRUE, ...){
  #using plotQuantBreakPoints
  if(!is.null(segments) && nrow(segments)>0){
    temp <- segments[,4:6]
    names(temp) <- names(segments[,1:3])
    segments <- rbind(segments[,1:3], temp)
    names(segments) <- c("lower", "bpt", "upper")
    if(any(segments<2)){
      segments[segments<2] <-2
      warning("quantBreakSegments: break(s) too near '", name.pol,
              "' start...\n\tTried to fix but strongly recommend re-running ",
              "\n\tfindBreakPoints with different range\n",
              call.=FALSE)
    }
  }
  aqe_plotQuantBreakPoints(data, name.pol, segments,
                              ylab = ylab, xlab = xlab,
                              pt.col = pt.col, line.col = line.col,
                              break.col = break.col,
                              scalelabs = scalelabs,
                           event=event,
                           auto.text=auto.text, ...)
}



aqe_plotQuantBreakSegments02 <- function(data, name.pol, segments,
                                        ylab = NULL, xlab = NULL,
                                        pt.col = c("lightgrey", "darkgrey"),
                                        line.col = "red", break.col ="blue",
                                        scalelabs = c("data", "trend",
                                                      "change"),
                                        event=NULL,
                                        auto.text=TRUE, ...){
  #using plotQuantBreakPoints
  if(!is.null(segments) && nrow(segments)>0){
    segments <- segments[2:nrow(segments),1:3]
    names(segments) <- c("lower", "bpt", "upper")
    if(any(is.na(segments))) {
      if(any(is.na(segments[,1]))){
        segments[,1] <- ifelse(is.na(segments[,1]), 1, segments[,1])
      }
      if(any(is.na(segments[, ncol(segments)]))){
        segments[,ncol(segments)] <- ifelse(is.na(segments[,ncol(segments)]),
                                            nrow(data),
                                            segments[,ncol(segments)])
      }
      if(any(is.na(segments)))
        warning("quantBreakSegments: found suspect segment(s)...'",
                "\n\tTried to fix but failed...\n",
                call.=FALSE)
    }
    if(any(segments<2)){
      segments[segments<2] <-2
      warning("quantBreakSegments: break(s) too near '", name.pol,
              "' start...\n\tTried to fix but strongly recommend re-running ",
              "\n\tfindBreakPoints with different range\n",
              call.=FALSE)
    }
  }
  aqe_plotQuantBreakPoints(data, name.pol, segments,
                              ylab = ylab, xlab = xlab,
                              pt.col = pt.col, line.col = line.col,
                              break.col = break.col,
                              scalelabs = scalelabs,
                           event=event,
                           auto.text=auto.text)
}


aqe_summariseBreakSegmentsReport <- function(report){
  if (is.null(report)) {
    message("no change ranges declared...")
  }
  else {
    message("building ", nrow(report), " segments")
    for (i in 1:nrow(report)) {
      message("\n", as.character(report[i, 1]), " to ",
          as.character(report[i, 2]), " (",
          as.character(report[i, 3]), ")",
          sep = "")
      message(signif(report[i, 4], 4), "->",
          signif(report[i, 5], 4), ";",
          signif(report[i, 6], 4), " (",
          signif(report[i, 7], 4), "%)", sep = "")
      #########################
      #to do
      #########################
      #report confidences and diffs?
      #need to agree method...
    }
  }
}


aqe_fitBreakSegmentsModel01 <- function(data, name.pol, breaks){
  #function in quantBreakPoints
  mod <- aqe_fitBreakPointsModel(data, name.pol, breaks)
  data$pred <- rep(NA, nrow(data))
  data$err <- data$pred
  ans <- predict(mod, se.fit = TRUE)
  data$pred[as.numeric(names(ans$fit))] <- ans$fit
  data$err[as.numeric(names(ans$fit))] <- ans$se.fit
  if(!is.null(breaks) && nrow(breaks)>0){
    for(i in 1:nrow(breaks)){
      temp <- data$pred[c(breaks[i,1]:breaks[i,3])]
      data$pred[c(breaks[i,1]:breaks[i,3])] <-
        seq(temp[1], temp[length(temp)], length.out = length(temp))
    }
  }
  data$count <- as.numeric(data$date)
  data$count <- (data$count - min(data$count, na.rm=TRUE)) + 1
  ff <- as.formula(paste(name.pol, "~pred", sep=""))
  lm(ff, data=data)
}

aqe_makeSegmentsFromBreaks01 <- function(breaks){

  #make segments
  segments <- NULL
  if(!is.null(breaks) && nrow(breaks)>0){
    segments <- data.frame(seg.1.low=breaks$lower -
                                 (breaks$bpt-breaks$lower),
                           seg.1=breaks$lower,
                           seg.1.high=breaks$bpt,
                           seg.2.low=breaks$bpt,
                           seg.2=breaks$upper,
                           seg.2.high=breaks$upper +
                                 (breaks$upper-breaks$bpt),
                           stringsAsFactors = FALSE)
  }
  segments
}

#fitBreakSegmentsModel02
aqe_fitBreakSegmentsModel02 <- function(data, name.pol, breaks,
                                        seg.seed = 12345){
  #reinstate iterative segmented as method 02
  #(needs extra stops for out of range cases)
  #(might need change of outputs for method 01)
  temp <- as.data.frame(data)[c("date", name.pol)]
  #not happy with this naming strategy
  temp$..ref <- 1:nrow(temp)
  temp$..d.prop <- as.numeric(temp$date)
  temp$..d.prop <- temp$..d.prop - min(temp$..d.prop,
                                       na.rm=TRUE)
  temp$..d.prop <- temp$..d.prop/max(temp$..d.prop,
                                    na.rm=TRUE)
  ff <- as.formula(paste(name.pol, "..d.prop", sep="~"))
#print("fit")
#############################
  mod <- lm(ff, temp)
#print("post-fit")

  #####################
  #send back if not breaks
  #####################
  if(is.null(breaks) || nrow(breaks)<1){
    return(list(mod=mod, segments=NULL))
  }
  #####################
  #segs0 <- temp$..d.prop[sort(as.vector(unlist(breaks[rep(c(2),2)])))]
  #segsd <- temp$..d.prop[sort(as.vector(unlist(breaks[c(1,3)])))]
  segs0 <- temp$..d.prop[as.vector(unlist(breaks[rep(c(2),2)]))]
  segsd <- temp$..d.prop[as.vector(unlist(breaks[c(1,3)]))]
  segs0 <- segs0[order(segsd)]
  segsd <- segsd[order(segsd)]
  segsd <- segs0-segsd
  test <- do.call(expand.grid, rep(list(c(0.25,1,1.75)),
                                   length(segs0)))
  ref <- -10
  smod <- NULL
  for(i in 1:nrow(test)){
#print("start")
#######################
    ttest <- as.vector(unlist(test[i,]))
    segs <- segs0 + (segsd * ttest)
    segs <- segs[order(segs)]
    #segs <- sort(segs)
    if(any(segs<0.01)){
      segs[segs<0.01] <- 0.01
    }
    if(any(segs>0.99)){
      segs[segs>0.99] <- 0.99
    }
#print("this fit")
#######################
    log <- capture.output({
    #tmod <- try(suppressWarnings(
    #  local_segmented(mod, seg.Z=~..d.prop, psi=segs,
    #                       control=local_seg.control(it.max=1, n.boot=0))
    #  ), silent=FALSE)

    tmod <- try(suppressWarnings(
        local_segmented(mod, seg.Z=~..d.prop, psi=segs,
                      control=local_seg.control(it.max=1, n.boot=0,
                                                seed=seg.seed))
      ), silent=TRUE)
    })
#print(class(tmod))
###########################
    if(class(tmod)[1]!="try-error"){
      ans <- try(suppressWarnings(local_summary.segmented(tmod)$adj.r.squared),
                 silent=TRUE)

#print(ans)
#print(ref)
############################
      if(class(ans)[1]!="try-error" && ans>ref){
        ref <- ans
        smod <- tmod
      }
    }
#print("end")
#######################
  }
  #if no segmented model built...
  #fault 1: sot, thomas lewin, bpt 4 at end...
  if(is.null(smod)) {
    stop(paste("quantBreakSegments(): segmented model trips",
               "\n\t(close to end break?)", sep=""),
         call. = TRUE)
    #return(list(mod=mod, segments=NULL))
  }
  segs <- local_confint.segmented(smod)
  segs.low <- round(approx(temp$..d.prop, temp$..ref, segs[,2])$y)
  segs.hi <- round(approx(temp$..d.prop, temp$..ref, segs[,3])$y)
  segs <- round(approx(temp$..d.prop, temp$..ref, segs[,1])$y)

  segments <- data.frame(
    seg.str.low = c(1, segs.low),
    seg.str = c(1, segs),
    seg.str.hi = c(1, segs.hi),
    seg.end.low = c(segs.low, nrow(data)),
    seg.end = c(segs, nrow(data)),
    seg.end.hi = c(segs.hi, nrow(data))
  )
  #this returns mod and segments
#print("here")
#######################

  list(mod=smod, segments=segments)
}

local_summary.segmented <-
  function (object, short = FALSE, var.diff = FALSE, p.df = "p",
            .vcov = NULL, ...)
  {
#print("lss start")
#######################

    if (is.null(object$psi))
      object <- object[[length(object)]]
    if (!is.null(.vcov))
      var.diff <- FALSE
    if (var.diff && length(object$nameUV$Z) > 1) {
      var.diff <- FALSE
      warning(" 'var.diff' set to FALSE with multiple segmented variables",
              call. = FALSE)
    }
    nomiU <- object$nameUV$U
    nomiV <- object$nameUV$V
    idU <- match(nomiU, names(coef(object)[!is.na(coef(object))]))
    idV <- match(nomiV, names(coef(object)[!is.na(coef(object))]))
    beta.c <- coef(object)[nomiU]
#print("lss model")
#################
    if ("segmented.default" == as.character(object$call)[1]) {
      summ <- c(summary(object, ...), object["psi"])
      summ[c("it", "epsilon")] <- object[c("it", "epsilon")]
      return(summ)
    }
    if ("lm" %in% class(object) && !"glm" %in% class(object)) {
      summ <- c(summary.lm(object, ...), object["psi"])
      summ$Ttable <- summ$coefficients
      if (var.diff) {
        Qr <- object$qr
        p <- object$rank
        p1 <- 1L:p
        inv.XtX <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
        X <- qr.X(Qr, FALSE)
        attr(X, "assign") <- NULL
        K <- length(unique(object$id.group))
        dev.new <- tapply(object$residuals, object$id.group,
                          function(.x) {
                            sum(.x^2)
                          })
        summ$df.new <- tapply(object$residuals, object$id.group,
                              function(.x) {
                                (length(.x) - eval(parse(text = p.df)))
                              })
        if (any(summ$df.new <= 0))
          stop("nonpositive df when computig the group-specific variances.. reduce 'p.df'?",
               call. = FALSE)
        summ$sigma.new <- sqrt(dev.new/summ$df.new)
        sigma.i <- rowSums(model.matrix(~0 + factor(object$id.group)) %*%
                             diag(summ$sigma.new))
        var.b <- inv.XtX %*% crossprod(X * sigma.i) %*%
          inv.XtX
        dimnames(var.b) <- dimnames(summ$cov.unscaled)
        summ$cov.var.diff <- var.b
        summ$Ttable[, 2] <- sqrt(diag(var.b))
        summ$Ttable[, 3] <- summ$Ttable[, 1]/summ$Ttable[,
                                                         2]
        summ$Ttable[, 4] <- 2 * pt(abs(summ$Ttable[, 3]),
                                   df = object$df.residual, lower.tail = FALSE)
        dimnames(summ$Ttable) <- list(names(object$coefficients)[Qr$pivot[p1]],
                                      c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
      }
      if (!is.null(.vcov)) {
        summ$Ttable[, 2] <- sqrt(diag(.vcov))
        summ$Ttable[, 3] <- summ$Ttable[, 1]/summ$Ttable[,
                                                         2]
        summ$Ttable[, 4] <- 2 * pt(abs(summ$Ttable[, 3]),
                                   df = object$df.residual, lower.tail = FALSE)
      }
      summ$Ttable[idU, 4] <- NA
      summ$Ttable <- summ$Ttable[-idV, ]
      summ[c("it", "epsilon", "conv.warn")] <- object[c("it",
                                                        "epsilon", "id.warn")]
      summ$var.diff <- var.diff
      summ$short <- short
      class(summ) <- c("summary.segmented", "summary.lm")
#print("lss stop")
#######################

      return(summ)
    }
    if (inherits(object, "glm")) {
      summ <- c(summary.glm(object, ...), object["psi"])
      summ$Ttable <- summ$coefficients[-idV, ]
      summ$Ttable[idU, 4] <- NA
      summ[c("it", "epsilon", "conv.warn")] <- object[c("it",
                                                        "epsilon", "id.warn")]
      summ$short <- short
      class(summ) <- c("summary.segmented", "summary.glm")
      return(summ)
    }
    if ("Arima" %in% class(object)) {
      coeff <- object$coef
      v <- sqrt(diag(object$var.coef))
      Ttable <- cbind(coeff[-idV], v[-idV], coeff[-idV]/v[-idV])
      colnames(Ttable) <- c("Estimate", "Std. Error", "t value")
      object$Ttable <- Ttable
      object$short <- short
      summ <- object
      summ[c("it", "epsilon", "conv.warn")] <- object[c("it",
                                                        "epsilon", "id.warn")]
      class(summ) <- c("summary.segmented", "summary.Arima")
      return(summ)
    }
  }


#######################################
#local versions of segmented functions
#######################################
#added while looking at changes
#segmented 1.3 -> 1.4
######################################
#note:
#segmented sets seed which R core do
#not like...
#######################################

#source:
#https://CRAN.R-project.org/package=segmented

local_segmented  <-
  function (obj, seg.Z, psi, npsi, fixed.psi = NULL, control = local_seg.control(),
            model = TRUE, keep.class = FALSE, ...)
  {
#print("ls start")
#######################
    build.all.psi <- function(psi, fixed.psi) {
      all.names.psi <- union(names(psi), names(fixed.psi))
      all.psi <- vector("list", length = length(all.names.psi))
      names(all.psi) <- all.names.psi
      for (i in names(all.psi)) {
        if (!is.null(psi[[i]])) {
          psi[[i]] <- sort(psi[[i]])
          names(psi[[i]]) <- paste("U", 1:length(psi[[i]]),
                                   ".", i, sep = "")
        }
        if (!is.null(fixed.psi[[i]])) {
          fixed.psi[[i]] <- sort(fixed.psi[[i]])
          names(fixed.psi[[i]]) <- paste("U", 1:length(fixed.psi[[i]]),
                                         ".fixed.", i, sep = "")
        }
        all.psi[[i]] <- sort
      }
      return(all.psi)
    }
    if (missing(seg.Z)) {
      if (length(all.vars(formula(obj))) == 2)
        seg.Z <- as.formula(paste("~", all.vars(formula(obj))[2]))
      else stop("please specify 'seg.Z'")
    }
    n.Seg <- length(all.vars(seg.Z))
    id.npsi <- FALSE
    if ("V" %in% sub("V[1-9]*[0-9]", "V", c(all.vars(seg.Z),
                                            all.vars(formula(obj))[-1])))
      stop("variable names 'V', 'V1', .. are not allowed")
    if ("U" %in% sub("U[1-9]*[0-9]", "U", c(all.vars(seg.Z),
                                            all.vars(formula(obj))[-1])))
      stop("variable names 'U', 'U1', .. are not allowed")
    if (any(c("$", "[") %in% all.names(seg.Z)))
      stop(" '$' or '[' not allowed in 'seg.Z' ")
    if (missing(psi)) {
      if (n.Seg == 1) {
        if (missing(npsi))
          npsi <- 1
        npsi <- lapply(npsi, function(.x) .x)
        if (length(npsi) != length(all.vars(seg.Z)))
          stop("seg.Z and npsi do not match")
        names(npsi) <- all.vars(seg.Z)
      }
      else {
        if (missing(npsi)) {
          npsi <- rep(1, n.Seg)
          names(npsi) <- all.vars(seg.Z)
        }
        if (length(npsi) != n.Seg)
          stop(" 'npsi' and seg.Z should have the same length")
        if (!all(names(npsi) %in% all.vars(seg.Z)))
          stop(" names in 'npsi' and 'seg.Z' do not match")
      }
      psi <- lapply(npsi, function(.x) rep(NA, .x))
      id.npsi <- TRUE
    }
    else {
      if (n.Seg == 1) {
        if (!is.list(psi)) {
          psi <- list(psi)
          names(psi) <- all.vars(seg.Z)
        }
      }
      else {
        if (!is.list(psi))
          stop("with multiple terms in `seg.Z', `psi' should be a named list")
        if (n.Seg != length(psi))
          stop("A wrong number of terms in `seg.Z' or `psi'")
        if (!all(names(psi) %in% all.vars(seg.Z)))
          stop("Names in `seg.Z' and `psi' do not match")
      }
    }
    fc <- min(max(abs(control$fc), 0.8), 1)
    min.step <- control$min.step
    alpha <- control$alpha
    it.max <- old.it.max <- control$it.max
    digits <- control$digits
    toll <- control$toll
    if (toll < 0)
      stop("Negative tolerance ('tol' in seg.control()) is meaningless",
           call. = FALSE)
    visual <- control$visual
    stop.if.error <- control$stop.if.error
    fix.npsi <- fix.npsi <- control$fix.npsi
    if (!is.null(stop.if.error)) {
      warning(" Argument 'stop.if.error' is working, but will be removed in the next releases. Please use 'fix.npsi' for the future..")
    }
    else {
      stop.if.error <- fix.npsi
    }
    break.boot = control$break.boot
    n.boot <- control$n.boot
    size.boot <- control$size.boot
    gap <- control$gap
    random <- control$random
    pow <- control$pow
    conv.psi <- control$conv.psi
    visualBoot <- FALSE
    if (n.boot > 0) {
      if (!is.null(control$seed)) {
        set.seed(control$seed)
        employed.Random.seed <- control$seed
      }
      else {
        employed.Random.seed <- eval(parse(text = paste(sample(0:9,
                                                               size = 6), collapse = "")))
        set.seed(employed.Random.seed)
      }
      if (visual) {
        visual <- FALSE
        visualBoot <- TRUE
      }
    }
    last <- control$last
    K <- control$K
    h <- control$h
    orig.call <- Call <- mf <- obj$call
    orig.call$formula <- mf$formula <- formula(obj)
    m <- match(c("formula", "data", "subset", "weights", "na.action",
                 "offset"), names(mf), 0L)
    mf <- mf[c(1, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- as.name("model.frame")
    if (class(mf$formula)[1] == "name" && !"~" %in% paste(mf$formula))
      mf$formula <- eval(mf$formula)
    mfExt <- mf
    mf$formula <- update.formula(mf$formula, paste(seg.Z, collapse = ".+"))
    if (!is.null(obj$call$offset) || !is.null(obj$call$weights) ||
        !is.null(obj$call$subset)) {
      mfExt$formula <- update.formula(mf$formula, paste(".~.+",
                                                        paste(c(all.vars(obj$call$offset), all.vars(obj$call$weights),
                                                                all.vars(obj$call$subset)), collapse = "+")))
    }
    mf <- eval(mf, parent.frame())
    n <- nrow(mf)
    nomiOff <- setdiff(all.vars(formula(obj)), names(mf))
    if (length(nomiOff) >= 1)
      mfExt$formula <- update.formula(mfExt$formula, paste(".~.+",
                                                           paste(nomiOff, collapse = "+"), sep = ""))
    nomiTUTTI <- all.vars(mfExt$formula)
    nomiNO <- NULL
    for (i in nomiTUTTI) {
      r <- try(eval(parse(text = i), parent.frame()), silent = TRUE)
      if (class(r)[1] != "try-error" && length(r) == 1 && !is.function(r) &&
          !i %in% names(mf))
        nomiNO[[length(nomiNO) + 1]] <- i
    }
    if (!is.null(nomiNO))
      mfExt$formula <- update.formula(mfExt$formula, paste(".~.-",
                                                           paste(nomiNO, collapse = "-"), sep = ""))
    mfExt <- eval(mfExt, parent.frame())
    weights <- as.vector(model.weights(mf))
    offs <- as.vector(model.offset(mf))
    mt <- attr(mf, "terms")
    interc <- attr(mt, "intercept")
    y <- model.response(mf, "any")
    XREG <- if (!is.empty.model(mt))
      model.matrix(mt, mf, obj$contrasts)
    namesXREG0 <- colnames(XREG)
    nameLeftSlopeZero <- setdiff(all.vars(seg.Z), names(coef(obj)))
    namesXREG0 <- setdiff(namesXREG0, nameLeftSlopeZero)
    id.duplic <- match(all.vars(formula(obj)), all.vars(seg.Z),
                       nomatch = 0) > 0
    if (any(id.duplic)) {
      new.mf <- mf[, all.vars(formula(obj))[id.duplic], drop = FALSE]
      new.XREGseg <- data.matrix(new.mf)
      XREG <- cbind(XREG, new.XREGseg)
    }
    n.psi <- length(unlist(psi))
    id.n.Seg <- (ncol(XREG) - n.Seg + 1):ncol(XREG)
    XREGseg <- XREG[, id.n.Seg, drop = FALSE]
    XREG <- XREG[, match(c("(Intercept)", namesXREG0), colnames(XREG),
                         nomatch = 0), drop = FALSE]
    XREG <- XREG[, unique(colnames(XREG)), drop = FALSE]
    n <- nrow(XREG)
    Z <- lapply(apply(XREGseg, 2, list), unlist)
    name.Z <- names(Z) <- colnames(XREGseg)
    if (length(Z) == 1 && is.vector(psi) && (is.numeric(psi) ||
                                             is.na(psi))) {
      psi <- list(as.numeric(psi))
      names(psi) <- name.Z
    }
    if (!is.list(Z) || !is.list(psi) || is.null(names(Z)) ||
        is.null(names(psi)))
      stop("Z and psi have to be *named* list")
    id.nomiZpsi <- match(names(Z), names(psi))
    if ((length(Z) != length(psi)) || any(is.na(id.nomiZpsi)))
      stop("Length or names of Z and psi do not match")
    nome <- names(psi)[id.nomiZpsi]
    psi <- psi[nome]
    if (id.npsi) {
      for (i in 1:length(psi)) {
        K <- length(psi[[i]])
        if (any(is.na(psi[[i]])))
          psi[[i]] <- if (control$quant) {
            quantile(Z[[i]], prob = seq(0, 1, l = K +
                                          2)[-c(1, K + 2)], names = FALSE)
          }
        else {
          (min(Z[[i]]) + diff(range(Z[[i]])) * (1:K)/(K +
                                                        1))
        }
      }
    }
    else {
      for (i in 1:length(psi)) {
        if (any(is.na(psi[[i]])))
          psi[[i]] <- if (control$quant) {
            quantile(Z[[i]], prob = seq(0, 1, l = K +
                                          2)[-c(1, K + 2)], names = FALSE)
          }
        else {
          (min(Z[[i]]) + diff(range(Z[[i]])) * (1:K)/(K +
                                                        1))
        }
      }
    }
    id.psi.fixed <- FALSE
    if (!is.null(fixed.psi)) {
      id.psi.fixed <- TRUE
      if (is.numeric(fixed.psi) && n.Seg == 1) {
        fixed.psi <- list(fixed.psi)
        names(fixed.psi) <- all.vars(seg.Z)
      }
      if (is.list(fixed.psi)) {
        if (!(names(fixed.psi) %in% all.vars(seg.Z)))
          stop("names(fixed.psi) is not a subset of variables in 'seg.Z' ")
      }
      else {
        stop(" 'fixed.psi' has to be a named list ")
      }
      fixed.psi <- lapply(fixed.psi, sort)
      Zfixed <- matrix(unlist(mapply(function(x, y) rep(x,
                                                        y), Z[names(fixed.psi)], sapply(fixed.psi, length),
                                     SIMPLIFY = TRUE)), nrow = n)
      n.fixed.psi <- sapply(fixed.psi, length)
      rip.nomi <- rep(names(fixed.psi), n.fixed.psi)
      rip.numeri <- unlist(lapply(n.fixed.psi, function(.x) 1:.x))
      colnames(Zfixed) <- paste("U", rip.numeri, ".fixed.",
                                rip.nomi, sep = "")
      PSI <- matrix(unlist(fixed.psi), ncol = ncol(Zfixed),
                    nrow = n, byrow = TRUE)
      fixedU <- (Zfixed - PSI) * (Zfixed > PSI)
      XREG <- cbind(XREG, fixedU)
    }
    initial.psi <- psi
    a <- sapply(psi, length)
    id.psi.group <- rep(1:length(a), times = a)
    Z <- matrix(unlist(mapply(function(x, y) rep(x, y), Z, a,
                              SIMPLIFY = TRUE)), nrow = n)
    psi <- unlist(psi)
    psi <- unlist(tapply(psi, id.psi.group, sort))
    k <- ncol(Z)
    PSI <- matrix(rep(psi, rep(n, k)), ncol = k)
    c1 <- apply((Z <= PSI), 2, all)
    c2 <- apply((Z >= PSI), 2, all)
    if (sum(c1 + c2) != 0 || is.na(sum(c1 + c2)))
      stop("starting psi out of the admissible range")
    colnames(Z) <- nomiZ <- rep(nome, times = a)
    ripetizioni <- as.numeric(unlist(sapply(table(nomiZ)[order(unique(nomiZ))],
                                            function(.x) {
                                              1:.x
                                            })))
    nomiU <- paste("U", ripetizioni, sep = "")
    nomiU <- paste(nomiU, nomiZ, sep = ".")
    nomiV <- paste("V", ripetizioni, sep = "")
    nomiV <- paste(nomiV, nomiZ, sep = ".")
    if (it.max == 0) {
      U <- (Z > PSI) * (Z - PSI)
      colnames(U) <- paste(ripetizioni, nomiZ, sep = ".")
      nomiU <- paste("U", colnames(U), sep = "")
      for (i in 1:ncol(U)) mfExt[nomiU[i]] <- mf[nomiU[i]] <- U[,
                                                                i]
      Fo <- update.formula(formula(obj), as.formula(paste(".~.+",
                                                          paste(nomiU, collapse = "+"))))
      obj <- update(obj, formula = Fo, evaluate = FALSE, data = mfExt)
      if (!is.null(obj[["subset"]]))
        obj[["subset"]] <- NULL
      obj <- eval(obj, envir = mfExt)
      if (model)
        obj$model <- mf
      psi <- cbind(psi, psi, 0)
      rownames(psi) <- paste(paste("psi", ripetizioni, sep = ""),
                             nomiZ, sep = ".")
      colnames(psi) <- c("Initial", "Est.", "St.Err")
      obj$psi <- psi
      return(obj)
    }
    if (is.null(weights))
      weights <- rep(1, n)
    if (is.null(offs))
      offs <- rep(0, n)
    initial <- psi
    obj0 <- obj
    dev0 <- sum(obj$residuals^2)
    list.obj <- list(obj)
    nomiOK <- nomiU
    invXtX <- if (!is.null(obj$qr))
      chol2inv(qr.R(obj$qr))
    else NULL
    Xty <- crossprod(XREG, y)
    opz <- list(toll = toll, h = h, stop.if.error = stop.if.error,
                dev0 = dev0, visual = visual, it.max = it.max, nomiOK = nomiOK,
                id.psi.group = id.psi.group, gap = gap, visualBoot = visualBoot,
                pow = pow, digits = digits, invXtX = invXtX, Xty = Xty,
                conv.psi = conv.psi, alpha = alpha, fix.npsi = fix.npsi,
                min.step = min.step, fc = fc)
    if (n.boot <= 0) {
#print("here?")
      obj <- local_seg.lm.fit(y, XREG, Z, PSI, weights, offs, opz)
    }
    else {
      obj <- local_seg.lm.fit.boot(y, XREG, Z, PSI, weights, offs,
                             opz, n.boot = n.boot, size.boot = size.boot, random = random,
                             break.boot = break.boot)
    }
    if (!is.list(obj)) {
      warning("No breakpoint estimated", call. = FALSE)
      return(obj0)
    }
    if (obj$obj$df.residual == 0)
      warning("no residual degrees of freedom (other warnings expected)",
              call. = FALSE)
    id.psi.group <- obj$id.psi.group
    nomiOK <- obj$nomiOK
    nomiFINALI <- unique(sub("U[1-9]*[0-9].", "", nomiOK))
    nomiSenzaPSI <- setdiff(name.Z, nomiFINALI)
    if (length(nomiSenzaPSI) >= 1)
      warning("no breakpoints found for: ", paste(nomiSenzaPSI,
                                                  " "), call. = FALSE)
    it <- obj$it
    psi <- obj$psi
    psi.values <- if (n.boot <= 0)
      obj$psi.values
    else obj$boot.restart
    U <- obj$U
    V <- obj$V
    id.warn <- obj$id.warn
    rangeZ <- obj$rangeZ
    obj <- obj$obj
    k <- length(psi)
    beta.c <- coef(obj)[paste("U", 1:ncol(U), sep = "")]
    Vxb <- V %*% diag(beta.c, ncol = length(beta.c))
    length.psi <- tapply(as.numeric(as.character(names(psi))),
                         as.numeric(as.character(names(psi))), length)
    forma.nomiU <- function(xx, yy) paste("U", 1:xx, ".", yy,
                                          sep = "")
    forma.nomiVxb <- function(xx, yy) paste("psi", 1:xx, ".",
                                            yy, sep = "")
    nomiU <- unlist(mapply(forma.nomiU, length.psi, nomiFINALI))
    nomiVxb <- unlist(mapply(forma.nomiVxb, length.psi, nomiFINALI))
    psi.list <- vector("list", length = length(unique(nomiZ)))
    names(psi.list) <- unique(nomiZ)
    names(psi) <- rep(nomiFINALI, length.psi)
    for (i in names(psi.list)) {
      psi.list[[i]] <- psi[names(psi) == i]
    }
    for (i in 1:ncol(U)) {
      mfExt[nomiU[i]] <- mf[nomiU[i]] <- U[, i]
      mfExt[nomiVxb[i]] <- mf[nomiVxb[i]] <- Vxb[, i]
    }
    nnomi <- c(nomiU, nomiVxb)
    Fo <- update.formula(formula(obj0), as.formula(paste(".~.+",
                                                         paste(nnomi, collapse = "+"))))
    if (id.psi.fixed) {
      for (i in 1:ncol(fixedU)) mfExt[colnames(fixedU)[i]] <- mf[colnames(fixedU)[i]] <- fixedU[,
                                                                                                i]
      Fo <- update.formula(Fo, paste(c("~.", colnames(fixedU)),
                                     collapse = "+"))
    }
    objF <- update(obj0, formula = Fo, evaluate = FALSE, data = mfExt)
    if (!is.null(objF[["subset"]]))
      objF[["subset"]] <- NULL
    objF <- eval(objF, envir = mfExt)
    objF$offset <- obj0$offset
    isNAcoef <- any(is.na(objF$coefficients))
    if (isNAcoef) {
      if (stop.if.error) {
        message("breakpoint estimate(s):", as.vector(psi))
        stop("at least one coef is NA: breakpoint(s) at the boundary? (possibly with many x-values replicated)",
             call. = FALSE)
      }
      else {
        warning("some estimate is NA: too many breakpoints? 'var(hat.psi)' cannot be computed \n ..returning a 'lm' model",
                call. = FALSE)
        Fo <- update.formula(formula(obj0), as.formula(paste(".~.+",
                                                             paste(nomiU, collapse = "+"))))
        objF <- update(obj0, formula = Fo, evaluate = TRUE,
                       data = mfExt)
        names(psi) <- nomiVxb
        objF$psi <- psi
        return(objF)
      }
    }
    if (!gap) {
      names.coef <- names(objF$coefficients)
      names(obj$coefficients)[match(c(paste("U", 1:k, sep = ""),
                                      paste("V", 1:k, sep = "")), names(coef(obj)))] <- nnomi
      objF$coefficients[names.coef] <- obj$coefficients[names.coef]
      objF$fitted.values <- obj$fitted.values
      objF$residuals <- obj$residuals
    }
    Cov <- vcov(objF)
    id <- match(nomiVxb, names(coef(objF)))
    vv <- if (length(id) == 1)
      Cov[id, id]
    else diag(Cov[id, id])
    a <- tapply(id.psi.group, id.psi.group, length)
    ris.psi <- matrix(NA, length(psi), 3)
    colnames(ris.psi) <- c("Initial", "Est.", "St.Err")
    rownames(ris.psi) <- nomiVxb
    ris.psi[, 2] <- psi
    ris.psi[, 3] <- sqrt(vv)
    a.ok <- NULL
    for (j in name.Z) {
      if (j %in% nomiFINALI) {
        a.ok[length(a.ok) + 1] <- a[1]
        a <- a[-1]
      }
      else {
        a.ok[length(a.ok) + 1] <- 0
      }
    }
    initial <- unlist(mapply(function(x, y) {
      if (is.na(x)[1])
        rep(x, y)
      else x
    }, initial.psi[nomiFINALI], a.ok[a.ok != 0], SIMPLIFY = TRUE))
    if (stop.if.error)
      ris.psi[, 1] <- initial
    objF$rangeZ <- rangeZ
    objF$psi.history <- psi.values
    objF$psi <- ris.psi
    objF$it <- it
    objF$epsilon <- obj$epsilon
    objF$call <- match.call()
    objF$nameUV <- list(U = drop(nomiU), V = rownames(ris.psi),
                        Z = nomiFINALI)
    objF$id.group <- if (length(name.Z) <= 1)
      -rowSums(as.matrix(V))
    objF$id.psi.group <- id.psi.group
    objF$id.warn <- id.warn
    objF$orig.call <- orig.call
    objF$indexU <- build.all.psi(psi.list, fixed.psi)
    if (model)
      objF$model <- mf
    if (n.boot > 0)
      objF$seed <- employed.Random.seed
    class(objF) <- c("segmented", class(obj0))
    list.obj[[length(list.obj) + 1]] <- objF
    class(list.obj) <- "segmented"
#print("end")
#######################

    if (last)
      list.obj <- list.obj[[length(list.obj)]]
    return(list.obj)
  }


local_seg.lm.fit <-
  function (y, XREG, Z, PSI, w, offs, opz, return.all.sol = FALSE)
  {
#print("start")
#######################

    useExp.k = TRUE
    est.k <- function(x1, y1, L0) {
      ax <- log(x1)
      .x <- cbind(1, ax, ax^2)
      b <- drop(solve(crossprod(.x), crossprod(.x, y1)))
      const <- b[1] - L0
      DD <- sqrt(b[2]^2 - 4 * const * b[3])
      kk <- exp((-b[2] + DD)/(2 * b[3]))
      return(round(kk))
    }
    dpmax <- function(x, y, pow = 1) {
      if (pow == 1)
        -(x > y)
      else -pow * ((x - y) * (x > y))^(pow - 1)
    }
    mylm <- function(x, y, w, offs = rep(0, length(y))) {
      x1 <- x * sqrt(w)
      y <- y - offs
      y1 <- y * sqrt(w)
      b <- drop(solve(crossprod(x1), crossprod(x1, y1)))
      fit <- drop(tcrossprod(x, t(b)))
      r <- y - fit
      o <- list(coefficients = b, fitted.values = fit, residuals = r,
                df.residual = length(y) - length(b))
      o
    }
    mylmADD <- function(invXtX, X, v, Xty, y) {
      vtv <- sum(v^2)
      Xtv <- crossprod(X, v)
      m <- invXtX %*% Xtv
      d <- drop(1/(vtv - t(Xtv) %*% m))
      r <- -d * m
      invF <- invXtX + d * tcrossprod(m)
      newINV <- rbind(cbind(invF, r), c(t(r), d))
      b <- crossprod(newINV, c(Xty, sum(v * y)))
      fit <- tcrossprod(cbind(X, v), t(b))
      r <- y - fit
      o <- list(coefficients = b, fitted.values = fit, residuals = r)
      o
    }
    in.psi <- function(LIM, PSI, ret.id = TRUE) {
      a <- PSI[1, ] <= LIM[1, ]
      b <- PSI[1, ] >= LIM[2, ]
      is.ok <- !a & !b
      if (ret.id)
        return(is.ok)
      isOK <- all(is.ok) && all(!is.na(is.ok))
      isOK
    }
    far.psi <- function(Z, PSI, id.psi.group, ret.id = TRUE,
                        fc = 0.93) {
      nSeg <- length(unique(id.psi.group))
      npsij <- tapply(id.psi.group, id.psi.group, length)
      nj <- sapply(unique(id.psi.group), function(.x) {
        tabulate(rowSums((Z > PSI)[, id.psi.group == .x,
                                   drop = FALSE]) + 1)
      }, simplify = FALSE)
      ff <- id.far.ok <- vector("list", length = nSeg)
      for (i in 1:nSeg) {
        if (length(nj[[i]]) != npsij[i] + 1)
          nj[[i]] <- tabulate(rowSums((Z >= PSI)[, id.psi.group ==
                                                   i, drop = FALSE]) + 1)
        id.ok <- (nj[[i]] >= 2)
        id.far.ok[[i]] <- id.ok[-length(id.ok)] & id.ok[-1]
        ff[[i]] <- ifelse(diff(nj[[i]]) > 0, 1/fc, fc)
      }
      id.far.ok <- unlist(id.far.ok)
      ff <- unlist(ff)
      if (!ret.id) {
        return(all(id.far.ok))
      }
      else {
        attr(id.far.ok, "factor") <- ff
        return(id.far.ok)
      }
    }
    adj.psi <- function(psii, LIM) {
      pmin(pmax(LIM[1, ], psii), LIM[2, ])
    }
    n <- length(y)
    min.step <- opz$min.step
    rangeZ <- apply(Z, 2, range)
    alpha <- opz$alpha
    limZ <- apply(Z, 2, quantile, names = FALSE, probs = c(alpha,
                                                           1 - alpha))
    psi <- PSI[1, ]
    id.psi.group <- opz$id.psi.group
    conv.psi <- opz$conv.psi
    h <- opz$h
    digits <- opz$digits
    pow <- opz$pow
    nomiOK <- opz$nomiOK
    toll <- opz$toll
    h <- opz$h
    gap <- opz$gap
    fix.npsi <- opz$stop.if.error
    dev.new <- opz$dev0
    visual <- opz$visual
    it.max <- old.it.max <- opz$it.max
    fc <- opz$fc
    names(psi) <- id.psi.group
    it <- 0
    epsilon <- 10
    k.values <- dev.values <- NULL
    psi.values <- list()
    psi.values[[length(psi.values) + 1]] <- NA
    sel.col.XREG <- unique(sapply(colnames(XREG), function(x) match(x,
                                                                    colnames(XREG))))
    if (is.numeric(sel.col.XREG))
      XREG <- XREG[, sel.col.XREG, drop = FALSE]
    invXtX <- opz$invXtX
    Xty <- opz$Xty
    if (!in.psi(limZ, PSI, FALSE))
      stop("starting psi out of the range.. see 'alpha' in seg.control.",
           call. = FALSE)
    if (!far.psi(Z, PSI, id.psi.group, FALSE))
      stop("psi values too close each other. Please change (decreases number of) starting values",
           call. = FALSE)
    n.psi1 <- ncol(Z)
    U <- ((Z - PSI) * (Z > PSI))
    if (pow[1] != 1)
      U <- U^pow[1]
    obj0 <- try(mylm(cbind(XREG, U), y, w, offs), silent = TRUE)
    if (class(obj0)[1] == "try-error")
      obj0 <- lm.wfit(cbind(XREG, U), y, w, offs)
    L0 <- sum(obj0$residuals^2 * w)
    n.intDev0 <- nchar(strsplit(as.character(L0), "\\.")[[1]][1])
    dev.values[length(dev.values) + 1] <- opz$dev0
    dev.values[length(dev.values) + 1] <- L0
    psi.values[[length(psi.values) + 1]] <- psi
    if (visual) {
      message(paste("iter = ", sprintf("%2.0f", 0), "  dev = ",
                sprintf(paste("%", n.intDev0 + 6, ".5f", sep = ""),
                        L0), "  k = ", sprintf("%2.0f", NA), "  n.psi = ",
                formatC(length(unlist(psi)), digits = 0, format = "f"),
                "  ini.psi = ", paste(formatC(unlist(psi), digits = 3,
                                              format = "f"), collapse = "  "), sep = ""),
          "\n")
    }
    id.warn <- FALSE
    id.psi.changed <- rep(FALSE, it.max)
    while (abs(epsilon) > toll) {
      it <- it + 1
      n.psi0 <- n.psi1
      n.psi1 <- ncol(Z)
      if (n.psi1 != n.psi0) {
        U <- ((Z - PSI) * (Z > PSI))
        if (pow[1] != 1)
          U <- U^pow[1]
        obj0 <- try(mylm(cbind(XREG, U), y, w, offs), silent = TRUE)
        if (class(obj0)[1] == "try-error")
          obj0 <- lm.wfit(cbind(XREG, U), y, w, offs)
        L0 <- sum(obj0$residuals^2 * w)
      }
      V <- dpmax(Z, PSI, pow = pow[2])
      X <- cbind(XREG, U, V)
      rownames(X) <- NULL
      colnames(X)[(ncol(XREG) + 1):ncol(X)] <- c(paste("U",
                                                       1:ncol(U), sep = ""), paste("V", 1:ncol(V), sep = ""))
      obj <- lm.wfit(x = X, y = y, w = w, offset = offs)
      beta.c <- coef(obj)[paste("U", 1:ncol(U), sep = "")]
      gamma.c <- coef(obj)[paste("V", 1:ncol(V), sep = "")]
      if (any(is.na(c(beta.c, gamma.c)))) {
        if (fix.npsi) {
          if (return.all.sol)
            return(list(dev.values, psi.values))
          else stop("breakpoint estimate too close or at the boundary causing NA estimates.. too many breakpoints being estimated?",
                    call. = FALSE)
        }
        else {
          id.coef.ok <- !is.na(gamma.c)
          psi <- psi[id.coef.ok]
          if (length(psi) <= 0) {
            warning(paste("All breakpoints have been removed after",
                          it, "iterations.. returning 0"), call. = FALSE)
            return(0)
          }
          gamma.c <- gamma.c[id.coef.ok]
          beta.c <- beta.c[id.coef.ok]
          Z <- Z[, id.coef.ok, drop = FALSE]
          rangeZ <- rangeZ[, id.coef.ok, drop = FALSE]
          limZ <- limZ[, id.coef.ok, drop = FALSE]
          nomiOK <- nomiOK[id.coef.ok]
          id.psi.group <- id.psi.group[id.coef.ok]
          names(psi) <- id.psi.group
        }
      }
      psi.old <- psi
      psi <- psi.old + gamma.c/beta.c
      if (!is.null(digits))
        psi <- round(psi, digits)
      PSI <- matrix(rep(psi, rep(n, length(psi))), ncol = length(psi))
      U1 <- (Z - PSI) * (Z > PSI)
      if (pow[1] != 1)
        U1 <- U1^pow[1]
      obj1 <- try(mylm(cbind(XREG, U1), y, w, offs), silent = TRUE)
      if (class(obj1)[1] == "try-error")
        obj1 <- try(lm.wfit(cbind(XREG, U1), y, w, offs),
                    silent = TRUE)
      L1 <- if (class(obj1)[1] == "try-error")
        L0 + 10
      else sum(obj1$residuals^2 * w)
      use.k <- k <- 1
      L1.k <- NULL
      L1.k[length(L1.k) + 1] <- L1
      while (L1 > L0) {
        k <- k + 1
        use.k <- if (useExp.k)
          2^(k - 1)
        else k
        psi <- psi.old + (gamma.c/beta.c)/(use.k * h)
        if (!is.null(digits))
          psi <- round(psi, digits)
        PSI <- matrix(rep(psi, rep(n, length(psi))), ncol = length(psi))
        U1 <- (Z - PSI) * (Z > PSI)
        if (pow[1] != 1)
          U1 <- U1^pow[1]
        obj1 <- try(mylm(cbind(XREG, U1), y, w, offs), silent = TRUE)
        if (class(obj1)[1] == "try-error")
          obj1 <- lm.wfit(cbind(XREG, U1), y, w, offs)
        L1 <- if (class(obj1)[1] == "try-error")
          L0 + 10
        else sum(obj1$residuals^2 * w)
        L1.k[length(L1.k) + 1] <- L1
        if (1/(use.k * h) < min.step) {
          break
        }
      }
      if (visual) {
        flush.console()
        message(paste("iter = ", sprintf("%2.0f", it), "  dev = ",
                  sprintf(paste("%", n.intDev0 + 6, ".5f", sep = ""),
                          L1), "  k = ", sprintf("%2.0f", k), "  n.psi = ",
                  formatC(length(unlist(psi)), digits = 0, format = "f"),
                  "  est.psi = ", paste(formatC(unlist(psi), digits = 3,
                                                format = "f"), collapse = "  "), sep = ""),
            "\n")
      }
      epsilon <- if (conv.psi)
        max(abs((psi - psi.old)/psi.old))
      else (L0 - L1)/(abs(L0) + 0.1)
      L0 <- L1
      U <- U1
      k.values[length(k.values) + 1] <- use.k
      psi.values[[length(psi.values) + 1]] <- psi
      dev.values[length(dev.values) + 1] <- L0
      id.psi.far <- far.psi(Z, PSI, id.psi.group, TRUE, fc = opz$fc)
      id.psi.in <- in.psi(limZ, PSI, TRUE)
      id.psi.ok <- id.psi.in & id.psi.far
      if (!all(id.psi.ok)) {
        if (fix.npsi) {
          psi <- psi * ifelse(id.psi.far, 1, attr(id.psi.far,
                                                  "factor"))
          PSI <- matrix(rep(psi, rep(nrow(Z), length(psi))),
                        ncol = length(psi))
          id.psi.changed[it] <- TRUE
        }
        else {
          Z <- Z[, id.psi.ok, drop = FALSE]
          PSI <- PSI[, id.psi.ok, drop = FALSE]
          rangeZ <- rangeZ[, id.psi.ok, drop = FALSE]
          limZ <- limZ[, id.psi.ok, drop = FALSE]
          nomiOK <- nomiOK[id.psi.ok]
          id.psi.group <- id.psi.group[id.psi.ok]
          psi.old <- psi.old[id.psi.ok]
          psi <- psi[id.psi.ok]
          names(psi) <- id.psi.group
          if (ncol(PSI) <= 0) {
            warning(paste("All breakpoints have been removed after",
                          it, "iterations.. returning 0"), call. = FALSE)
            return(0)
          }
        }
      }
      if (it >= it.max) {
        id.warn <- TRUE
        break
      }
    }
    if (id.psi.changed[length(id.psi.changed)])
      warning(paste("Some psi (", (1:length(psi))[!id.psi.far],
                    ") changed after the last iter.", sep = ""), call. = FALSE)
    if (id.warn)
      warning(paste("max number of iterations (", it, ") attained",
                    sep = ""), call. = FALSE)
    attr(psi.values, "dev") <- dev.values
    attr(psi.values, "k") <- k.values
    psi <- unlist(tapply(psi, id.psi.group, sort))
    names(psi) <- id.psi.group
    names.coef <- names(obj$coefficients)
    PSI.old <- PSI
    PSI <- matrix(rep(psi, rep(nrow(Z), length(psi))), ncol = length(psi))
    if (sd(PSI - PSI.old) > 0 || id.psi.changed[length(id.psi.changed)]) {
      U <- (Z - PSI) * (Z > PSI)
      colnames(U) <- paste("U", 1:ncol(U), sep = "")
      V <- -(Z > PSI)
      colnames(V) <- paste("V", 1:ncol(V), sep = "")
      obj <- lm.wfit(x = cbind(XREG, U), y = y, w = w, offset = offs)
      L1 <- sum(obj$residuals^2 * w)
    }
    else {
      obj <- obj1
    }
    obj$coefficients <- c(obj$coefficients, rep(0, ncol(V)))
    names(obj$coefficients) <- names.coef
    obj$epsilon <- epsilon
    obj$it <- it
    obj <- list(obj = obj, it = it, psi = psi, psi.values = psi.values,
                U = U, V = V, rangeZ = rangeZ, epsilon = epsilon, nomiOK = nomiOK,
                SumSquares.no.gap = L1, id.psi.group = id.psi.group,
                id.warn = id.warn)
    return(obj)
  }



local_seg.lm.fit.boot <-
  function (y, XREG, Z, PSI, w, offs, opz, n.boot = 10, size.boot = NULL,
            jt = FALSE, nonParam = TRUE, random = FALSE, break.boot = n.boot)
  {
    extract.psi <- function(lista) {
      dev.values <- lista[[1]][-1]
      psi.values <- lista[[2]][-1]
      dev.ok <- min(dev.values)
      id.dev.ok <- which.min(dev.values)
      if (is.list(psi.values))
        psi.values <- matrix(unlist(psi.values), nrow = length(dev.values),
                             byrow = TRUE)
      if (!is.matrix(psi.values))
        psi.values <- matrix(psi.values)
      psi.ok <- psi.values[id.dev.ok, ]
      r <- list(SumSquares.no.gap = dev.ok, psi = psi.ok)
      r
    }
    visualBoot <- opz$visualBoot
    opz.boot <- opz
    opz.boot$pow = c(1, 1)
    opz1 <- opz
    opz1$it.max <- 1
    n <- length(y)
    o0 <- try(suppressWarnings(seg.lm.fit(y, XREG, Z, PSI, w,
                                          offs, opz, return.all.sol = FALSE)), silent = TRUE)
    rangeZ <- apply(Z, 2, range)
    if (!is.list(o0)) {
      o0 <- suppressWarnings(seg.lm.fit(y, XREG, Z, PSI, w,
                                        offs, opz, return.all.sol = TRUE))
      o0 <- extract.psi(o0)
      ss00 <- opz$dev0
      if (!nonParam) {
        warning("using nonparametric boot")
        nonParam <- TRUE
      }
    }
    if (is.list(o0)) {
      est.psi00 <- est.psi0 <- o0$psi
      ss00 <- o0$SumSquares.no.gap
      if (!nonParam)
        fitted.ok <- fitted(o0)
    }
    else {
      if (!nonParam)
        stop("the first fit failed and I cannot extract fitted values for the semipar boot")
      if (random) {
        est.psi00 <- est.psi0 <- apply(rangeZ, 2, function(r) runif(1,
                                                                    r[1], r[2]))
        PSI1 <- matrix(rep(est.psi0, rep(nrow(Z), length(est.psi0))),
                       ncol = length(est.psi0))
        o0 <- try(suppressWarnings(seg.lm.fit(y, XREG, Z,
                                              PSI1, w, offs, opz1)), silent = TRUE)
        ss00 <- o0$SumSquares.no.gap
      }
      else {
        est.psi00 <- est.psi0 <- apply(PSI, 2, mean)
        ss00 <- opz$dev0
      }
    }
    n.intDev0 <- nchar(strsplit(as.character(ss00), "\\.")[[1]][1])
    all.est.psi.boot <- all.selected.psi <- all.est.psi <- matrix(NA,
                                                                  nrow = n.boot, ncol = length(est.psi0))
    all.ss <- all.selected.ss <- rep(NA, n.boot)
    if (is.null(size.boot))
      size.boot <- n
    Z.orig <- Z
    count.random <- 0
    id.uguali <- 0
    k.psi.change <- 1
    alpha <- 0.1
    for (k in seq(n.boot)) {
      n.boot.rev <- 3
      diff.selected.ss <- rev(diff(na.omit(all.selected.ss)))
      if (length(diff.selected.ss) >= (n.boot.rev - 1) &&
          all(round(diff.selected.ss[1:(n.boot.rev - 1)],
                    6) == 0)) {
        qpsi <- sapply(1:ncol(Z), function(i) mean(est.psi0[i] >=
                                                     Z[, i]))
        qpsi <- ifelse(abs(qpsi - 0.5) < 0.1, alpha, qpsi)
        alpha <- 1 - alpha
        est.psi0 <- sapply(1:ncol(Z), function(i) quantile(Z[,
                                                             i], probs = 1 - qpsi[i], names = FALSE))
      }
      PSI <- matrix(rep(est.psi0, rep(nrow(Z), length(est.psi0))),
                    ncol = length(est.psi0))
      if (jt)
        Z <- apply(Z.orig, 2, jitter)
      if (nonParam) {
        id <- sample(n, size = size.boot, replace = TRUE)
        o.boot <- try(suppressWarnings(seg.lm.fit(y[id],
                                                  XREG[id, , drop = FALSE], Z[id, , drop = FALSE],
                                                  PSI[id, , drop = FALSE], w[id], offs[id], opz.boot)),
                      silent = TRUE)
      }
      else {
        yy <- fitted.ok + sample(residuals(o0), size = n,
                                 replace = TRUE)
        o.boot <- try(suppressWarnings(seg.lm.fit(yy, XREG,
                                                  Z.orig, PSI, weights, offs, opz.boot)), silent = TRUE)
      }
      if (is.list(o.boot)) {
        all.est.psi.boot[k, ] <- est.psi.boot <- o.boot$psi
      }
      else {
        est.psi.boot <- apply(rangeZ, 2, function(r) runif(1,
                                                           r[1], r[2]))
      }
      PSI <- matrix(rep(est.psi.boot, rep(nrow(Z), length(est.psi.boot))),
                    ncol = length(est.psi.boot))
      opz$h <- max(opz$h * 0.9, 0.2)
      opz$it.max <- opz$it.max + 1
      o <- try(suppressWarnings(seg.lm.fit(y, XREG, Z.orig,
                                           PSI, w, offs, opz, return.all.sol = TRUE)), silent = TRUE)
      if (!is.list(o) && random) {
        est.psi0 <- apply(rangeZ, 2, function(r) runif(1,
                                                       r[1], r[2]))
        PSI1 <- matrix(rep(est.psi0, rep(nrow(Z), length(est.psi0))),
                       ncol = length(est.psi0))
        o <- try(suppressWarnings(seg.lm.fit(y, XREG, Z,
                                             PSI1, w, offs, opz1)), silent = TRUE)
        count.random <- count.random + 1
      }
      if (is.list(o)) {
        if (!"coefficients" %in% names(o$obj))
          o <- extract.psi(o)
        all.est.psi[k, ] <- o$psi
        all.ss[k] <- o$SumSquares.no.gap
        if (o$SumSquares.no.gap <= ifelse(is.list(o0), o0$SumSquares.no.gap,
                                          10^12)) {
          o0 <- o
          k.psi.change <- k
        }
        est.psi0 <- o0$psi
        all.selected.psi[k, ] <- est.psi0
        all.selected.ss[k] <- o0$SumSquares.no.gap
      }
      if (visualBoot) {
        flush.console()
        message(paste("boot sample = ", sprintf("%2.0f", k),
                  "  opt.dev = ", sprintf(paste("%", n.intDev0 +
                                                  6, ".5f", sep = ""), o0$SumSquares.no.gap),
                  "  n.psi = ", formatC(length(unlist(est.psi0)),
                                        digits = 0, format = "f"), "  est.psi = ",
                  paste(formatC(unlist(est.psi0), digits = 3,
                                format = "f"), collapse = "  "), sep = ""),
            "\n")
      }
      asss <- na.omit(all.selected.ss)
      if (length(asss) > break.boot) {
        if (all(rev(round(diff(asss), 6))[1:(break.boot -
                                             1)] == 0))
          break
      }
    }
    all.selected.psi <- rbind(est.psi00, all.selected.psi)
    all.selected.ss <- c(ss00, all.selected.ss)
    ris <- list(all.selected.psi = drop(all.selected.psi), all.selected.ss = all.selected.ss,
                all.psi = all.est.psi, all.ss = all.ss)
    if (is.null(o0$obj)) {
      PSI1 <- matrix(rep(est.psi0, rep(nrow(Z), length(est.psi0))),
                     ncol = length(est.psi0))
      o0 <- try(seg.lm.fit(y, XREG, Z, PSI1, w, offs, opz1),
                silent = TRUE)
    }
    if (!is.list(o0))
      return(0)
    o0$boot.restart <- ris
    #rm(.Random.seed, envir = globalenv())
    return(o0)
  }


local_seg.control <-
  function (n.boot = 10, display = FALSE, tol = 1e-05, it.max = 30,
            fix.npsi = TRUE, K = 10, quant = TRUE, maxit.glm = 25, h = 1,
            break.boot = 5, size.boot = NULL, jt = FALSE, nonParam = TRUE,
            random = TRUE, seed = 12345, fn.obj = NULL, digits = NULL,
            conv.psi = FALSE, alpha = 0.02, min.step = 1e-04,
            powers = c(1,1), last = TRUE, stop.if.error = NULL, gap = FALSE,
            fc = 0.95)
  {
print("control")
####################
    list(toll = tol, it.max = it.max, visual = display, stop.if.error = stop.if.error,
         K = K, last = last, maxit.glm = maxit.glm, h = h, n.boot = n.boot,
         size.boot = size.boot, gap = gap, jt = jt, break.boot = break.boot,
         nonParam = nonParam, random = random, pow = powers,
         seed = seed, quant = quant, fn.obj = fn.obj, digits = digits,
         conv.psi = conv.psi, alpha = alpha, fix.npsi = fix.npsi,
         min.step = min.step, fc = fc)
  }


local_confint.segmented <-
  function (object, parm, level = 0.95, method = c("delta", "score",
                                                   "gradient"), rev.sgn = FALSE, var.diff = FALSE, is = FALSE,
            digits = max(4, getOption("digits") - 1), .coef = NULL,
            .vcov = NULL, ...)
  {
    method <- match.arg(method)
    cls <- class(object)
    if (length(cls) == 1)
      cls <- c(cls, cls)
    if (method %in% c("score", "gradient") && !all(cls[1:2] ==
                                                   c("segmented", "lm")))
      stop("Score- or Gradient-based CI only work with segmented lm models")
    estcoef <- if (is.null(.coef))
      coef(object)
    else .coef
    COV <- if (is.null(.vcov))
      vcov(object, var.diff = var.diff, is = is, ...)
    else .vcov
    confintSegDelta <- function(object, parm, level = 0.95,
                                rev.sgn = FALSE, var.diff = FALSE, is = FALSE, ...) {
      f.U <- function(nomiU, term = NULL) {
        k <- length(nomiU)
        nomiUsenzaU <- strsplit(nomiU, "\\.")
        nomiU.ok <- vector(length = k)
        for (i in 1:k) {
          nomi.i <- nomiUsenzaU[[i]][-1]
          if (length(nomi.i) > 1)
            nomi.i <- paste(nomi.i, collapse = ".")
          nomiU.ok[i] <- nomi.i
        }
        if (!is.null(term))
          nomiU.ok <- (1:k)[nomiU.ok %in% term]
        return(nomiU.ok)
      }
      if (var.diff && length(object$nameUV$Z) > 1) {
        var.diff <- FALSE
        warning(" 'var.diff' set to FALSE with multiple segmented variables",
                call. = FALSE)
      }
      if (missing(parm)) {
        nomeZ <- object$nameUV$Z
        if (length(rev.sgn) == 1)
          rev.sgn <- rep(rev.sgn, length(nomeZ))
      }
      else {
        if (!all(parm %in% object$nameUV$Z)) {
          stop("invalid 'parm' name", call. = FALSE)
        }
        else {
          nomeZ <- parm
        }
      }
      if (length(nomeZ) > 1) {
        warning("There are multiple segmented terms. The first is taken",
                call. = FALSE, immediate. = TRUE)
        nomeZ <- nomeZ[1]
      }
      if (length(rev.sgn) != length(nomeZ))
        rev.sgn <- rep(rev.sgn, length.out = length(nomeZ))
      rr <- list()
      z <- if ("lm" %in% class(object))
        abs(qt((1 - level)/2, df = object$df.residual))
      else abs(qnorm((1 - level)/2))
      for (i in 1:length(nomeZ)) {
        nomi.U <- object$nameUV$U[f.U(object$nameUV$U, nomeZ[i])]
        nomi.V <- object$nameUV$V[f.U(object$nameUV$V, nomeZ[i])]
        m <- matrix(, length(nomi.U), 3)
        colnames(m) <- c("Est.", paste("CI", "(", level *
                                         100, "%", ")", c(".low", ".up"), sep = ""))
        for (j in 1:length(nomi.U)) {
          sel <- c(nomi.V[j], nomi.U[j])
          V <- COV[sel, sel]
          b <- estcoef[sel[2]]
          th <- c(b, 1)
          orig.coef <- drop(diag(th) %*% estcoef[sel])
          gammma <- orig.coef[1]
          est.psi <- object$psi[sel[1], 2]
          V <- diag(th) %*% V %*% diag(th)
          se.psi <- sqrt((V[1, 1] + V[2, 2] * (gammma/b)^2 -
                            2 * V[1, 2] * (gammma/b))/b^2)
          r <- c(est.psi, est.psi - z * se.psi, est.psi +
                   z * se.psi)
          if (rev.sgn[i])
            r <- c(-r[1], rev(-r[2:3]))
          m[j, ] <- r
        }
        m <- m[order(m[, 1]), , drop = FALSE]
        rownames(m) <- nomi.V
        if (rev.sgn[i]) {
          rownames(m) <- rev(rownames(m))
        }
        rr[[length(rr) + 1]] <- m
      }
      names(rr) <- nomeZ
      return(rr[[1]])
    }
    confintSegIS <- function(obj, parm, d.h = 1.5, h = 2.5,
                             conf.level = level, ...) {
      ci.IS <- function(obj.seg, nomeZ, nomeUj, stat = c("score",
                                                         "gradient"), transf = FALSE, h = -1, sigma, conf.level = 0.95,
                        use.z = FALSE, is = TRUE, fit.is = TRUE, var.is = TRUE,
                        bw = NULL, smooth = 0, msgWarn = FALSE, n.values = 50,
                        altro = FALSE, cadj = FALSE, plot = FALSE, add = FALSE,
                        agg = FALSE, raw = FALSE, useSeg = FALSE) {
        u.psiX <- function(psi, sigma, x, y, XREG = NULL,
                           scale = FALSE, est.psi = NULL, interc = FALSE,
                           pow = c(1, 1), lag = 0, robust = FALSE, GS = FALSE,
                           is = FALSE, se.psi, var.is = TRUE, which.return = 3,
                           fit.is = FALSE, altro = FALSE, cadj = FALSE,
                           transf = FALSE) {
          varUpsi.fn <- function(X, sigma = 1, r = NULL) {
            INF <- crossprod(X)/(sigma^2)
            if (is.null(r)) {
              vv <- INF[1, 1] - (INF[1, -1] %*% solve(INF[-1,
                                                          -1], INF[-1, 1]))
            }
            else {
              u <- X * r/(sigma^2)
              V <- crossprod(u)
              I22 <- solve(INF[-1, -1])
              vv <- V[1, 1] - INF[1, -1] %*% I22 %*% V[1,
                                                       -1] - V[1, -1] %*% I22 %*% INF[-1, 1] +
                INF[1, -1] %*% I22 %*% V[-1, -1] %*% I22 %*%
                INF[-1, 1]
            }
            return(vv)
          }
          dpmax <- function(x, y, pow = 1) {
            if (pow == 1)
              -(x > y)
            else -pow * (x > y) * (x - y)^(pow - 1)
          }
          if (cadj && which.return != 3)
            stop("cadj=TRUE can return only the studentized score")
          if (is && missing(se.psi))
            stop("is=TRUE needs se.psi")
          if (interc)
            XREG <- cbind(rep(1, length(y)), XREG)
          if (fit.is) {
            XX <- if (altro)
              cbind((x - psi) * pnorm((x - psi)/se.psi) +
                      se.psi * dnorm((x - psi)/se.psi), XREG)
            else cbind((x - psi) * pnorm((x - psi)/se.psi),
                       XREG)
            o <- lm.fit(x = XX, y = y)
          }
          else {
            .U <- (x > psi) * (x - psi)
            if (pow[1] != 1)
              .U <- .U^pow[1]
            XX <- cbind(.U, XREG)
            o <- lm.fit(x = XX, y = y)
          }
          b <- o$coef[1]
          mu <- o$fitted.values
          n <- length(mu)
          V <- NULL
          if (GS) {
            if (is.null(est.psi))
              stop("'GS=TRUE' needs 'est.psi'")
            gs <- b * (sum((y - mu) * V)/(sigma^2)) *
              (est.psi - psi)
            gs <- sqrt(pmax(gs, 0)) * sign(est.psi - psi)
            return(gs)
          }
          if (is) {
            r <- -b * sum(((y - mu) * pnorm((x - psi)/se.psi)))/sigma^2
            XX <- if (var.is)
              cbind(-b * pnorm((x - psi)/se.psi), XX)
            else cbind(-b * I(x > psi), XX)
          }
          else {
            r <- -b * sum((y - mu) * I(x > psi))/sigma^2
            XX <- cbind(-b * I(x > psi), XX)
          }
          if (scale) {
            if (!is.null(est.psi)) {
              mu <- attr(est.psi, "fitted")
              est.b <- attr(est.psi, "b")
              est.psi <- as.numeric(est.psi)
              if (is) {
                XX <- if (var.is)
                  cbind(-est.b * pnorm((x - est.psi)/se.psi),
                        XX[, -1])
                else cbind(-est.b * I(x > est.psi), XX[,
                                                       -1])
              }
              else {
                XX <- cbind(-est.b * I(x > est.psi), XX[,
                                                        -1])
              }
            }
            rr <- if (robust)
              (y - mu)
            else NULL
            v.Upsi <- try(varUpsi.fn(XX, sigma, r = rr),
                          silent = TRUE)
            if (!is.numeric(v.Upsi))
              return(NA)
            if (v.Upsi <= 0)
              return(NA)
          }
          names(r) <- NULL
          r <- c(r, v.Upsi, r/sqrt(max(v.Upsi, 0)))
          r <- r[which.return]
          if (cadj)
            r <- sign(r) * sqrt((r^2) * (1 - (3 - (r^2))/(2 *
                                                            n)))
          r
        }
        u.psiXV <- Vectorize(u.psiX, vectorize.args = "psi",
                             USE.NAMES = FALSE)
        gs.fn <- function(x, y, estpsi, sigma2, psivalue,
                          pow = c(1, 1), adj = 1, is = FALSE, sepsi, XREG = NULL,
                          fit.is = FALSE, altro = FALSE, transf = FALSE) {
          logitDeriv <- function(kappa) exp(kappa) * diff(intv)/((1 +
                                                                    exp(kappa))^2)
          logit <- function(psi) log((psi - min(intv))/(max(intv) -
                                                          psi))
          logitInv <- function(kappa) (min(intv) + max(intv) *
                                         exp(kappa))/(1 + exp(kappa))
          intv <- quantile(x, probs = c(0.02, 0.98), names = FALSE)
          if (is && missing(sepsi))
            stop("SE(psi) is requested when is=TRUE")
          k <- length(psivalue)
          r <- vector(length = k)
          for (i in 1:k) {
            psii <- psivalue[i]
            if (fit.is) {
              X <- if (altro)
                cbind(1, x, (x - psii) * pnorm((x - psii)/sepsi) +
                        sepsi * dnorm((x - psii)/sepsi), XREG)
              else cbind(1, x, (x - psii) * pnorm((x -
                                                     psii)/sepsi), XREG)
            }
            else {
              .U <- (x - psii) * (x > psii)
              if (pow[1] != 1)
                .U <- .U^pow[1]
              X <- cbind(1, x, .U, XREG)
            }
            o <- lm.fit(y = y, x = X)
            b <- o$coef[3]
            if (is) {
              v <- pnorm((x - psii)/sepsi)
            }
            else {
              v <- if (pow[2] == 1)
                I(x > psii)
              else pow[2] * pmax(x - psii, 0)^(pow[2] -
                                                 1)
            }
            if (transf)
              v <- v * logitDeriv(logit(psii))
            r[i] <- -(b/sigma2) * sum((y - o$fitted) *
                                        v)
            r[i] <- if (!transf)
              r[i] * (estpsi - psii)
            else r[i] * (logit(estpsi) - logit(psii))
            if (altro && fit.is)
              r[i] <- r[i] + (estpsi - psii) * ((b * sepsi *
                                                   sum(dnorm((x - psii)/sepsi))) * (b/sigma2))
          }
          if (adj > 0) {
            r <- if (adj == 1)
              pmax(r, 0)
            else abs(r)
          }
          if (transf)
            psivalue <- logit(psivalue)
          segni <- if (transf)
            sign(logit(estpsi) - psivalue)
          else sign(estpsi - psivalue)
          r <- cbind(psi = psivalue, gs.Chi = r, gs.Norm = sqrt(r) *
                       segni)
          r
        }
        monotSmooth <- function(xx, yy, hat.psi, k = 20,
                                w = 0) {
          bspline <- function(x, ndx, xlr = NULL, knots,
                              deg = 3, deriv = 0) {
            if (missing(knots)) {
              if (is.null(xlr)) {
                xl <- min(x) - 0.01 * diff(range(x))
                xr <- max(x) + 0.01 * diff(range(x))
              }
              else {
                if (length(xlr) != 2)
                  stop("quando fornito, xlr deve avere due componenti")
                xl <- xlr[1]
                xr <- xlr[2]
              }
              dx <- (xr - xl)/ndx
              knots <- seq(xl - deg * dx, xr + deg * dx,
                           by = dx)
            }
            B <- splineDesign(knots, x, ord = deg + 1,
                              derivs = rep(deriv, length(x)))
            r <- list(B = B, degree = deg, knots = knots)
            r
          }
          if (length(k) == 1)
            r <- bspline(xx, ndx = k)
          else r <- bspline(xx, knots = k)
          B <- r$B
          knots <- r$knots
          degree <- r$degree
          D1 <- diff(diag(ncol(B)), diff = 1)
          d <- drop(solve(crossprod(B), crossprod(B, yy)))
          B0 <- spline.des(knots, c(min(xx), hat.psi,
                                    max(xx)), degree + 1)$design
          P <- tcrossprod(B0[2, ]) * 10^12
          e <- rep(1, length(d))
          ww <- (1/(abs(xx - hat.psi) + diff(range(xx))/100))^w
          it <- 0
          while (!isTRUE(all.equal(e, rep(0, length(e))))) {
            v <- 1 * I(diff(d) > 0)
            E <- (10^12) * crossprod(D1 * sqrt(v))
            d.old <- d
            M <- crossprod(B * sqrt(ww)) + E + P
            d <- drop(solve(M + 0.001 * diag(ncol(M)),
                            crossprod(B, ww * yy)))
            e <- d - d.old
            it <- it + 1
            if (it >= 20)
              break
          }
          fit <- drop(B %*% d)
          return(fit)
        }
        miop <- function(x, y, xs = x, ys = y, h = FALSE,
                         v = FALSE, only.lines = FALSE, top = TRUE, right = TRUE,
                         col.h = grey(0.6), col.v = col.h, ...) {
          if (only.lines)
            h <- v <- TRUE
          if (!only.lines)
            plot(x, y, type = "l", ...)
          if (v) {
            y0 <- if (top)
              par()$usr[4]
            else par()$usr[3]
            segments(xs, y0, xs, ys, col = col.v, lty = 3)
          }
          if (h) {
            x0 <- if (right)
              par()$usr[2]
            else par()$usr[1]
            segments(xs, ys, x0, ys, col = col.h, lty = 3,
                     lwd = 1.2)
          }
          invisible(NULL)
        }
        f.Left <- function(x, y) {
          yy <- rev(y)
          xx <- rev(x)
          idList <- NULL
          while (any(diff(yy) < 0)) {
            id <- which(diff(yy) < 0)[1]
            idList[length(idList) + 1] <- id + 1
            yy <- yy[-(id + 1)]
            xx <- xx[-(id + 1)]
          }
          r <- cbind(xx, yy)
          r
        }
        f.Right <- function(x, y) {
          xx <- x
          yy <- y
          idList <- NULL
          while (any(diff(yy) > 0)) {
            id <- which(diff(yy) > 0)[1]
            idList[length(idList) + 1] <- id + 1
            yy <- yy[-(id + 1)]
            xx <- xx[-(id + 1)]
          }
          r <- cbind(xx, yy)
          r
        }
        stat <- match.arg(stat)
        if (missing(sigma))
          sigma <- summary.lm(obj.seg)$sigma
        if (cadj)
          use.z = TRUE
        zalpha <- if (use.z)
          -qnorm((1 - conf.level)/2)
        else -qt((1 - conf.level)/2, df = obj.seg$df.residual)
        if (!is.numeric(h))
          stop(" 'h' should be numeric")
        if (sign(h) >= 0)
          h <- abs(h[1])
        Y <- obj.seg$model[, 1]
        X <- obj.seg$model[, nomeZ]
        formula.lin <- update.formula(formula(obj.seg),
                                      paste(".~.", paste("-", paste(obj.seg$nameUV$V,
                                                                    collapse = "-"))))
        formula.lin <- update.formula(formula.lin, paste(".~.-",
                                                         nomeUj))
        XREG <- model.matrix(formula.lin, data = obj.seg$model)
        if (ncol(XREG) == 0)
          XREG <- NULL
        nomePsij <- sub("U", "psi", nomeUj)
        est.psi <- obj.seg$psi[nomePsij, "Est."]
        se.psi <- obj.seg$psi[nomePsij, "St.Err"]
        if (any(h < 0)) {
          all.range <- TRUE
          valori <- seq(quantile(X, probs = 0.05, names = FALSE),
                        quantile(X, probs = 0.95, names = FALSE),
                        l = n.values)
        }
        else {
          all.range <- FALSE
          valori <- seq(max(quantile(X, probs = 0.05,
                                     names = FALSE), est.psi - h * se.psi), min(quantile(X,
                                                                                         probs = 0.95, names = FALSE), est.psi + h *
                                                                                  se.psi), l = n.values)
        }
        n <- length(Y)
        min.X <- min(X)
        max.X <- max(X)
        if (!is.null(bw))
          se.psi <- eval(parse(text = bw))
        if (stat == "score") {
          U.valori <- u.psiXV(psi = valori, sigma = sigma,
                              x = X, y = Y, XREG = XREG, is = is, se.psi = se.psi,
                              scale = TRUE, pow = c(1, 1), fit.is = fit.is,
                              altro = altro, cadj = cadj, var.is = var.is,
                              transf = transf)
          statlab <- "Score statistic"
          if (plot && raw)
            U.raw <- u.psiXV(valori, sigma, X, Y, XREG,
                             is = FALSE, scale = TRUE, pow = c(1, 1),
                             fit.is = FALSE, altro = altro, cadj = cadj,
                             var.is = FALSE, transf = transf)
        }
        else {
          U.valori <- gs.fn(X, Y, est.psi, sigma^2, valori,
                            is = is, sepsi = se.psi, XREG = XREG, fit.is = fit.is,
                            altro = altro, transf = transf, pow = c(1,
                                                                    1))[, 3]
          statlab <- "Gradient statistic"
          if (plot && raw)
            U.raw <- gs.fn(X, Y, est.psi, sigma^2, valori,
                           is = FALSE, XREG = XREG, fit.is = FALSE,
                           altro = altro, transf = transf)[, 3]
        }
        if (any(is.na(U.valori))) {
          warning("removing NA in the statistic values")
          valori <- valori[!is.na(U.valori)]
          U.valori <- U.valori[!is.na(U.valori)]
        }
        logit <- function(psi) log((psi - min(intv))/(max(intv) -
                                                        psi))
        logitInv <- function(kappa) (min(intv) + max(intv) *
                                       exp(kappa))/(1 + exp(kappa))
        intv <- quantile(X, probs = c(0.02, 0.98), names = FALSE)
        if (stat == "gradient" && transf) {
          est.psi <- logit(est.psi)
          valori <- logit(valori)
          x.lab <- "kappa"
        }
        if (plot && !add) {
          x.lab <- "psi"
          if (raw) {
            plot(valori, U.raw, xlab = x.lab, ylab = statlab,
                 type = "l")
            points(valori, U.valori, xlab = x.lab, ylab = statlab,
                   type = "l", col = 2)
          }
          else {
            plot(valori, U.valori, xlab = x.lab, ylab = statlab,
                 type = "l", col = 2)
          }
          abline(h = 0, lty = 3)
          segments(est.psi, 0, est.psi, -20, lty = 2)
        }
        if (prod(range(U.valori)) >= 0)
          stop("the signs of stat at extremes are not discordant, increase 'h' o set 'h=-1' ")
        if (smooth == 0) {
          valoriLeft <- valori[valori <= est.psi]
          UvaloriLeft <- U.valori[valori <= est.psi]
          vLeft <- f.Left(valoriLeft, UvaloriLeft)
          valori.ok <- vLeft[, 1]
          Uvalori.ok <- vLeft[, 2]
          f.interpL <- splinefun(Uvalori.ok, valori.ok,
                                 method = "mono", ties = min)
          valoriRight <- valori[valori >= est.psi]
          UvaloriRight <- U.valori[valori >= est.psi]
          vRight <- f.Right(valoriRight, UvaloriRight)
          valori.ok <- vRight[, 1]
          Uvalori.ok <- vRight[, 2]
          f.interpR <- splinefun(Uvalori.ok, valori.ok,
                                 method = "mono", ties = min)
        }
        else {
          if (useSeg) {
            oseg <- try(suppressWarnings(segmented(lm(U.valori ~
                                                        valori), ~valori, psi = quantile(valori,
                                                                                         c(0.25, 0.75), names = FALSE), control = seg.control(n.boot = 0,
                                                                                                                                              stop.if.error = F))), silent = TRUE)
            if (class(oseg)[1] == "try-error") {
              oseg <- try(suppressWarnings(segmented(lm(U.valori ~
                                                          valori), ~valori, psi = quantile(valori,
                                                                                           0.5, names = FALSE), control = seg.control(n.boot = 0))),
                          silent = TRUE)
            }
            if (class(oseg)[1] == "segmented") {
              if (plot)
                lines(valori, oseg$fitted, lty = 3, lwd = 1.5)
              soglie <- oseg$psi[, 2]
              iid <- cut(valori, c(min(valori) - 1000,
                                   soglie, max(valori) + 1000), labels = FALSE)
              slopes <- cumsum(oseg$coef[2:(length(oseg$coef) -
                                              length(soglie))])
              slopes <- rep(slopes, table(iid))
              valori <- valori[slopes <= 0]
              U.valori <- U.valori[slopes <= 0]
            }
          }
          fr <- monotSmooth(valori, U.valori, est.psi,
                            k = 7)
          fr <- fr - (0.2/diff(range(valori))) * (valori -
                                                    mean(valori))
          vLeft <- cbind(valori[valori <= est.psi], fr[valori <=
                                                         est.psi])
          vRight <- cbind(valori[valori >= est.psi], fr[valori >=
                                                          est.psi])
          if (!all.range) {
            if ((min(valori) > intv[1]) && (fr[1] < max(zalpha)))
              return("errLeft")
            if ((max(valori) < intv[2]) && (fr[length(fr)] >
                                            min(-zalpha)))
              return("errRight")
          }
          f.interpL <- f.interpR <- splinefun(fr, valori,
                                              "m", ties = min)
        }
        L <- f.interpL(zalpha)
        U <- f.interpR(-zalpha)
        delta <- est.psi - f.interpL(0)
        if (plot) {
          if (!agg)
            delta <- 0
          lines(vLeft, col = 3)
          lines(vRight, col = 3)
          vv <- seq(0, zalpha * 1.2, l = 50)
          lines(f.interpL(vv) + delta, vv, col = grey(0.8,
                                                      alpha = 0.6), lwd = 4)
          vv <- seq(0, -zalpha * 1.2, l = 50)
          lines(f.interpR(vv) + delta, vv, col = grey(0.8,
                                                      alpha = 0.6), lwd = 4)
          points(est.psi, 0, pch = 19)
          miop(c(L, U) + delta, c(zalpha, -zalpha), only.lines = TRUE,
               top = FALSE, right = FALSE)
        }
        if (stat == "gradient" && transf) {
          L <- logitInv(L)
          U <- logitInv(U)
        }
        L <- pmax(L, quantile(X, probs = 0.02))
        U <- pmin(U, quantile(X, probs = 0.98))
        r <- c(est.psi, L, U)
        return(r)
      }
      if (!all(class(obj) == c("segmented", "lm")))
        stop("A segmented lm object is requested")
      if (missing(parm)) {
        nomeZ <- parm <- obj$nameUV$Z
      }
      else {
        if (!all(parm %in% obj$nameUV$Z))
          stop("invalid 'parm' ")
        nomeZ <- parm
      }
      if (length(parm) > 1) {
        warning("There are multiple segmented terms. The first is taken",
                call. = FALSE, immediate. = TRUE)
        nomeZ <- parm[1]
      }
      nomiU.term <- grep(nomeZ, obj$nameUV$U, value = TRUE)
      ra <- matrix(NA, length(nomiU.term), 3)
      rownames(ra) <- nomiU.term
      for (U.j in nomiU.term) {
        if (any(c(d.h, h) < 0)) {
          ra[U.j, ] <- ci.IS(obj, nomeZ, U.j, h = -1,
                             conf.level = level, ...)
        }
        d.h <- min(max(d.h, 1.5), 10)
        a <- "start"
        it <- 0
        while (is.character(a)) {
          a <- try(ci.IS(obj, nomeZ, U.j, h = h, conf.level = level,
                         ...), silent = TRUE)
          h <- h * d.h
          it <- it + 1
          if (it >= 20)
            break
        }
        ra[U.j, ] <- a
      }
      colnames(ra) <- c("Est.", paste("CI", "(", level * 100,
                                      "%", ")", c(".low", ".up"), sep = ""))
      rownames(ra) <- sub("U", "psi", nomiU.term)
      ra
    }
    if (method == "delta") {
      r <- confintSegDelta(object, parm, level, rev.sgn, var.diff,
                           is, ...)
    }
    else {
      r <- confintSegIS(object, parm, stat = method, conf.level = level,
                        ...)
    }
    r <- signif(r, digits)
    return(r)
  }
