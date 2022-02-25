
##############################################
#misc sub functions
#not exported
#may change if methods change
##############################################

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
                                         auto.text=TRUE){
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
                           auto.text=auto.text)
}



aqe_plotQuantBreakSegments02 <- function(data, name.pol, segments,
                                        ylab = NULL, xlab = NULL,
                                        pt.col = c("lightgrey", "darkgrey"),
                                        line.col = "red", break.col ="blue",
                                        scalelabs = c("data", "trend",
                                                      "change"),
                                        auto.text=TRUE){
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
                           auto.text=auto.text)
}


aqe_summariseBreakSegmentsReport <- function(report){
  if (is.null(report)) {
    cat("no change ranges declared...\n")
  }
  else {
    cat("building ", nrow(report), " segments\n")
    for (i in 1:nrow(report)) {
      cat("\n", as.character(report[i, 1]), " to ",
          as.character(report[i, 2]), " (",
          as.character(report[i, 3]), ")\n",
          sep = "")
      cat(signif(report[i, 4], 4), "->",
          signif(report[i, 5], 4), ";",
          signif(report[i, 6], 4), " (",
          signif(report[i, 7], 4), "%)\n", sep = "")
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
aqe_fitBreakSegmentsModel02 <- function(data, name.pol, breaks){
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
  mod <- lm(ff, temp)
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

    log <- capture.output({
    tmod <- try(suppressWarnings(
      segmented::segmented(mod, seg.Z=~..d.prop, psi=segs,
                           control=segmented::seg.control(it.max=1, n.boot=0))
    ), silent=TRUE)
    })
    if(class(tmod)[1]!="try-error"){
      ans <- try(segmented::summary.segmented(tmod)$adj.r.squared,
                 silent=TRUE)
      if(class(ans)[1]!="try-error" && ans>ref){
        ref <- ans
        smod <- tmod
      }
    }
  }
  #if no segmented model built...
  #fault 1: sot, thomas lewin, bpt 4 at end...
  if(is.null(smod)) {
    stop(paste("quantBreakSegments(): segmented model trips",
               "\n\t(close to end break?)", sep=""),
         call. = TRUE)
    #return(list(mod=mod, segments=NULL))
  }
  segs <- segmented::confint.segmented(smod)
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
  list(mod=smod, segments=segments)
}
