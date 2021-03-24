############################################
#' @title Air Quality Red Amber Green (RAG)
############################################

#' @name aqRAG
#' @aliases fitDSWModel RAG03
#' @description working function used in the development
#' of the RAG methods.
#' @param data (data.frame or tibble) data source.
#' @param pollutant (character) name of species RAG model
#' is to be built for.
#' @param to (various) date to extrapolate model to, by
#' default end of supplied data date range.
#' @param from (various) date to extrapolate model from,
#' by default start of supplied data date range.
#' @param mod (character) the left-hand-side of the RAG
#' model if not using standard formula.
#' @param pad.data (various) data to pad extrapolated
#' regions with.
#' @param x.args (character vector) Names of x terms to
#' use in model.
#' @param ... Extra arguments.
#' @param only.last (logical) Option to just report final
#' value when calculation rolling annual mean.
#' @param out (vector) Function output type, by default
#' all model outputs.
#' @param int.descr (vector) predicted intervention
#' impacts. (NEEDS DOC)
#' @param intervention (data.frame or similar) time-series
#' of intervention contributions.
#' @note this is a work function, not intended to be final
#' version and not fully document - please handle with
#' care.

###################################
#to do
###################################
#remove rag03?
#confirm naming convention for unexported functions?
#check mgcv gam + predict handling for unexported
#     function aqe_fitRAGModel

#' @import dplyr
#' @import ggplot2
#will want to tidy this







#splatted function
#old method - written as part of code development
#' @rdname aqRAG
#' @export
RAG03 <-
function(data, pollutant="no2",
                  to=NULL, from=NULL, mod=NULL,
                  pad.data=NULL, x.args=NULL,
                  ..., only.last=TRUE, out="all"){
  #pad x.args in data to from and to using pad.data

  #to think about
  ####################################
  #should to and from be first handled as just years
  #and then filtered?

  #see also notes in code,
  #work to do if others want to use this differently

  #setup
  from <- aqe_prepFromDate(from, data)
  to <- aqe_prepToDate(to, data)
  x.args <- aqe_prepXArgs(x.args, data, mod)
  #check if padding needed
  if(to <= max(data$date, na.rm=TRUE) &
     from >= min(data$date, na.rm=TRUE)){
     p.data <- data
     p.data$padded <- FALSE
  } else {
    pad.data <- aqe_prepPadData(pad.data, data)
    p.data <- aqe_padData(from, to, data, pad.data, x.args)
  }
  if(out=="padded.data") return(p.data)
  #build model
  out <- aqe_fitRAGModel(p.data, pollutant, x.args, mod)
  #calculate rolling averages
  temp <- c("date", pollutant, "rag.pred", "rag.upper",
            "rag.lower")
  temp <- temp[temp %in% names(out$results)]
  temp <- out$results[temp]
  out$rmean <- calcRollingTimeStat(temp,
                                   only.last = only.last)
  #final out
  out
}


#' @rdname aqRAG
#' @export
aqiRAG <- function(data, pollutant="no2",
                   intervention=NULL,
                   rag = NULL, mrag = NULL,
                   threshold = 40,
                   to=NULL, mod=NULL, ...,
                   output=c("report", "scores")){

  #note
  ###############################
  #this only tracks range BUT
  #applies for calc args in ...
  x.args <- list(...)
  range <- if("range" %in% names(x.args)){
    x.args$range
  } else {
    "year"
  }
  pad <- data[1:round((nrow(data))/2),]
  if("report" %in% output) {
    cat("building first RAG...\n")
  }
  mod.1 <- fitDSWModel(data, pollutant,
                       to=mrag, pad.data=pad,
                       mod=mod)
  ans.1 <- aqe_buildDSWModelRDR(mod.1, pollutant)
  ######################
  #new
  #  ans.1$int <- 0
  #  ans.1[ans.1$date >= int.date,]$int <- int.val
  #  ans.1$rag.pred <- ans.1$rag.pred + ans.1$int
  #  ans.1$rag.upper <- ans.1$rag.upper + ans.1$int
  #  ans.1$rag.lower <- ans.1$rag.lower + ans.1$int
  ###############################
  ans.1 <- calcRollingDateRangeStat(ans.1, method=2,
                                    ...)
  pad <- data[round((nrow(data)/2)):nrow(data),]
  if("report" %in% output) {
    cat("building second RAG...\n")
  }
  mod.2 <- fitDSWModel(data, pollutant,
                       to=mrag, pad.data=pad,
                       mod=mod)
  ans.2 <- aqe_buildDSWModelRDR(mod.2, pollutant)
  ######################
  #new
  #  ans.2$int <- 0
  #  ans.2[ans.2$date >= int.date,]$int <- int.val
  #  ans.2$rag.pred <- ans.2$rag.pred + ans.2$int
  #  ans.2$rag.upper <- ans.2$rag.upper + ans.2$int
  #  ans.2$rag.lower <- ans.2$rag.lower + ans.2$int
  ###############################

  ans.2 <- calcRollingDateRangeStat(ans.2, method=2,
                                    ...)

  if("report" %in% output) {
    cat("merging RAGs...\n")
  }
  names(ans.1)[names(ans.1)==pollutant] <- ".pol"
  names(ans.2)[names(ans.2)==pollutant] <- ".pol"
  ans <- bind_rows(ans.1, ans.2) %>%
    group_by(start.date, end.date) %>%
    summarise(.pol = mean(.pol, na.rm=TRUE),
              rag.pred = mean(rag.pred, na.rm=TRUE),
              rag.upper = max(rag.upper, na.rm=TRUE),
              rag.lower = min(rag.lower, na.rm=TRUE),
              rag.m.pred = mean(rag.m.pred, na.rm=TRUE),
              rag.m.upper = max(rag.m.upper, na.rm=TRUE),
              rag.m.lower = min(rag.m.lower, na.rm=TRUE))
  names(ans)[names(ans)==".pol"] <- pollutant
  #############################
  #new
  if(!is.null(intervention)){
    temp <- calcRollingDateRangeStat(intervention, method=2,
                                     ...)
    ans <- left_join(ans, temp, by=c("start.date", "end.date"))
  }
  ###############################
  #build results
  temp <- filter(ans, end.date <
                   AQEval:::aqe_prepToDate(rag, data))
  temp <- ans[nrow(temp), c("rag.pred", "rag.upper",
                            "rag.lower")]
  names(temp) <- paste(names(temp), rag, sep=".")
  results <- temp
  temp <- filter(ans, end.date <
                   AQEval:::aqe_prepToDate(mrag, data))
  temp <- ans[nrow(temp), c("rag.pred", "rag.upper",
                            "rag.lower")]
  names(temp) <- paste(names(temp), mrag, sep=".")
  results <- bind_cols(results, temp)
  #reset second
  temp <- results[,4]-results[,6]
  temp <- temp * 1.5
  results[,6] <- results[,4] - temp
  temp <- results[,5]-results[,4]
  temp <- temp * 1.5
  results[,5] <- results[,4] + temp

  ref <- calcRollingDateRangeStat(data, pollutant = pollutant,
                                  ...)

  out <- list(data=data, pollutant=pollutant,
              ans=ans, results=results, ref=ref,
              threshold=threshold, range=range)

  if("scores" %in% output){
    plot(aqe_plotRAGScores(out))
  }
  invisible(out)
}



#' @rdname aqRAG
#' @export
fitDSWModel <-
  function(data, pollutant="no2",
           to=NULL, from=NULL, mod=NULL,
           pad.data=NULL, x.args=NULL,
           ..., only.last=TRUE, out="all"){
    #pad x.args in data to from and to using pad.data

    #to think about
    ####################################
    #should to and from be first handled as just years
    #and then filtered?

    #see also notes in code,
    #work to do if others want to use this differently

    #setup
    from <- aqe_prepFromDate(from, data)
    to <- aqe_prepToDate(to, data)
    x.args <- aqe_prepXArgs(x.args, data, mod)
    #check if padding needed
    if(to <= max(data$date, na.rm=TRUE) &
       from >= min(data$date, na.rm=TRUE)){
      p.data <- data
      p.data$padded <- FALSE
    } else {
      pad.data <- aqe_prepPadData(pad.data, data)
      p.data <- aqe_padData(from, to, data, pad.data, x.args)
    }
    if(out=="padded.data") return(p.data)
    #build model
    out <- aqe_fitDSWModel(p.data, pollutant, x.args, mod)
    #add rollingmeans?
    out
  }


#' @rdname aqRAG
#' @export
makeIntTimeSeries <- function(data, from=NULL, to=NULL,
                              int.descr = NULL,
                              ...){
  #make dataset
  from <- aqe_prepFromDate(from, data)
  to <- aqe_prepToDate(to, data)
  tz <- attributes(data$date)$tzone
  #from <- as.POSIXct(from, "%Y-%m-%d", tz=tz)
  #to <- as.POSIXct(to, "%Y-%m-%d %H:%M:%S", tz=tz)
  date <- seq(from, to, by="day") #day hard-coded here
  d1 <- dplyr::as_tibble(data.frame(date=date,
                             rag.int=0))
  if(!is.null(int.descr)){
    for(i in int.descr){
      temp <- strsplit(i, " ")[[1]]
      if(length(temp)==2){
        d1[d1$date >= temp[1],]$rag.int <-
          as.numeric(temp[2])
      }
    }
  }
  d1
}





################################
#unexported functions
################################

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

aqe_fitDSWModel <- function(data, pollutant, x.args,
                         mod = NULL, ...){

  #to think about
  ################################
  #model/x.args handling
  #formula as arg...
  #put results in out data frame
  #so users using same names not an issue?

  #text data, pollutant, etc...
  if(!pollutant %in% names(data)){
    stop("pollutant not in data!")
  }

  #build mod.data
  #check columns there?
  m.data <- data[c(pollutant, x.args, "padded")]
  m.data$year.day <- as.numeric(format(m.data$date, "%j"))
  m.data$hour.day <- as.numeric(format(m.data$date, "%H"))
  m.data$week.day <- as.POSIXlt(m.data$date)$wday
  #need to build year.day for leap years...
  #error check above
  m.data$count <- as.numeric(m.data$date -
                               min(m.data$date, na.rm=TRUE))
  if(is.null(mod)){
    mod <- "s(wd,ws) + s(air_temp) + s(year.day) + s(week.day, k=5) + s(hour.day) + s(count, k=1)"
  }
  if(is.character(mod)){
    mod <- as.formula(paste(pollutant, " ~ ", mod,
                            sep=""))
  }

  mod <- gam(mod, data=m.data[!m.data$padded,])
  #move data filtering control to arguments

  #model build
  #(could make separate data frame?)
  temp <- predict(mod, newdata = m.data,
                  se.fit = TRUE)
  m.data$rag.pred <- temp$fit
  m.data$rag.upper <- temp$fit + (temp$se.fit * 1.96)
  m.data$rag.lower <- temp$fit - (temp$se.fit * 1.96)

  #out
  list(source=data, y=pollutant, mod=mod, results=m.data)
}


aqe_buildDSWModelRDR <- function(dsw, pollutant){
  temp <- dsw$results
  temp$rag.m.pred <- ifelse(temp$padded, temp$rag.pred,
                            temp[[pollutant]])
  temp$rag.m.lower <- ifelse(temp$padded, temp$rag.lower,
                             temp[[pollutant]])
  temp$rag.m.upper <- ifelse(temp$padded, temp$rag.upper,
                             temp[[pollutant]])
  temp <- temp[c("date", pollutant, "rag.pred",
                 "rag.lower", "rag.upper", "rag.m.pred",
                 "rag.m.lower", "rag.m.upper")]
  temp
}

aqe_plotRAGScores <- function(rag, threshold = NULL){

  results <- rag$results
  if(is.null(threshold)) {
    threshold <- rag$threshold
  }
  names(results) <- c("rag.pred.1", "rag.upper.1",
                      "rag.lower.1", "rag.pred.2",
                      "rag.upper.2", "rag.lower.2")

  r <- 2 / 2
  tt <- seq(0,2*pi,length.out = 100)
  xx <- 1 + r * cos(tt)
  yy <- 1 + r * sin(tt)
  dat <- data.frame(x = xx, y = yy)

  n <- threshold
  rag.col <- "red"
  if(results$rag.upper.1 > n &
     results$rag.lower.1 <= n) rag.col <- "orange"
  if(results$rag.upper.1 < n) rag.col <- "green"

  mrag.col <- "red"
  if(results$rag.upper.2 > n &
     results$rag.lower.2 <= n) mrag.col <- "orange"
  if(results$rag.upper.2 < n) mrag.col <- "green"

  ggplot(results, aes(x=1, y=1)) +
    geom_point() +
    geom_polygon(data=dat, aes(x,y), col=NA, fill=rag.col,
                 size=2) +
    geom_label(aes(label=round(rag.pred.1, digits=2)),
               size=16) +
    geom_label(aes(y=1.6, label=round(rag.upper.1,
                                      digits=2)),
               size=8) +
    geom_label(aes(y=0.4, label=round(rag.lower.1,
                                      digits=2)),
               size=8) +
    geom_text(aes(y=2.2, label=paste("RAG(", threshold, ")",
                                     sep="")),
              size=8) +

    #ylim(1.025, 0.975) +
    #  geom_polygon(data=dat, aes(x+2.5,y), fill="black") +
    geom_polygon(data=dat, aes(x+2.5,y), col=NA, fill=mrag.col,
                 size=2) +
    geom_label(aes(x = 3.5, y = 1,
                   label=round(rag.pred.2, digits=2)),
               size=16) +
    geom_label(aes(x = 3.5, y=1.6, label=round(rag.upper.2,
                                               digits=2)),
               size=8) +
    geom_label(aes(x=3.5, y=0.4, label=round(rag.lower.2,
                                             digits=2)),
               size=8) +
    geom_text(aes(x=3.5, y=2.2,
                  label=paste("mRAG(", threshold, ")",
                              sep="")),
              size=8) +
    coord_fixed() +
    theme_void()

}

aqe_plotRAG01 <- function(rag, ref=NULL){
  if(is.null(ref))
    ref <- rag$ref
  model <- rag$ans
  temp <- rag$pollutant
  names(ref)[names(ref)==temp] <- "..pol"
  range <- rag$range
  temp <- paste(temp, " Rolling ", range,
  " Mean", sep="")
  temp <- openair::quickText(temp)

  ggplot() +
    geom_path(data=model,
              aes(end.date, rag.pred, col="model")) +
    geom_path(data=model,
              aes(end.date, rag.lower, col="model"),
              linetype="dashed") +
    geom_path(data=model,
              aes(end.date, rag.upper, col="model"),
              linetype="dashed") +
    geom_path(data=ref,
              aes(end.date, ..pol, col="ambient")) +
    xlab("") +
    ylab(temp) +
    scale_color_discrete(name="") +
    theme_bw() +
    theme(legend.position = "top")
}

aqe_plotRAG02 <- function(rag, ref=NULL){
  if(is.null(ref))
    ref <- rag$ref
  model <- rag$ans
  temp <- rag$pollutant
  names(ref)[names(ref)==temp] <- "..pol"
  range <- rag$range
  temp <- paste(temp, " Rolling ", range,
                " Mean", sep="")
  temp <- openair::quickText(temp)

  ggplot() +
    geom_path(data=model,
              aes(end.date, rag.m.pred, col="model2")) +
    geom_path(data=model,
              aes(end.date, rag.m.lower, col="model2"),
              linetype="dashed") +
    geom_path(data=model,
              aes(end.date, rag.m.upper, col="model2"),
              linetype="dashed") +
    geom_path(data=ref,
              aes(end.date, ..pol, col="ambient")) +
    xlab("") +
    ylab(temp) +
    scale_color_discrete(name="") +
    theme_bw() +
    theme(legend.position = "top")
}



aqe_plotRAG03 <- function(rag, ref=NULL){

  if(is.null(ref))
    ref <- rag$ref
  model <- rag$ans

  if("rag.int" %in% names(model)){
    model$int.pred <- model$rag.pred+model$rag.int
    model$int.lower <- model$rag.lower+model$rag.int
    model$int.upper <- model$rag.upper+model$rag.int
  }

  temp <- rag$pollutant
  names(ref)[names(ref)==temp] <- "..pol"
  range <- rag$range
  temp <- paste(temp, " Rolling ", range,
                " Mean", sep="")
  temp <- openair::quickText(temp)

  pp <- ggplot()

  #remove last (suspect) row
  model <- model[-nrow(model),]


  if("rag.int" %in% names(model)){
    pp <- pp +
      geom_path(data=model,
                aes(end.date, int.pred, col="model+int")) +
      geom_path(data=model,
                aes(end.date, int.lower, col="model+int"),
                linetype="dashed") +
      geom_path(data=model,
                aes(end.date, int.upper, col="model+int"),
                linetype="dashed")
  }
  pp +
    geom_path(data=model,
              aes(end.date, rag.pred, col="model")) +
    geom_path(data=model,
              aes(end.date, rag.lower, col="model"),
              linetype="dashed") +
    geom_path(data=model,
              aes(end.date, rag.upper, col="model"),
              linetype="dashed") +
    geom_path(data=ref,
              aes(end.date, ..pol, col="ambient")) +
    xlab("") +
    ylab(temp) +
    scale_color_discrete(name="") +
    theme_bw() +
    theme(legend.position = "top")
}



aqe_plotRAG04 <- function(rag, ref=NULL){

  if(is.null(ref))
    ref <- rag$ref
  model <- rag$ans

  if("rag.int" %in% names(model)){
    model$int.pred <- model$rag.m.pred+model$rag.int
    model$int.lower <- model$rag.m.lower+model$rag.int
    model$int.upper <- model$rag.m.upper+model$rag.int
  }

  temp <- rag$pollutant
  names(ref)[names(ref)==temp] <- "..pol"
  range <- rag$range
  temp <- paste(temp, " Rolling ", range,
                " Mean", sep="")
  temp <- openair::quickText(temp)

  #remove last (suspect) row
  model <- model[-nrow(model),]

  pp <- ggplot()

  if("rag.int" %in% names(model)){
    pp <- pp +
      geom_path(data=model,
                aes(end.date, int.pred, col="model2+int")) +
      geom_path(data=model,
                aes(end.date, int.lower, col="model2+int"),
                linetype="dashed") +
      geom_path(data=model,
                aes(end.date, int.upper, col="model2+int"),
                linetype="dashed")
  }
  pp +
    geom_path(data=model,
              aes(end.date, rag.m.pred, col="model2")) +
    geom_path(data=model,
              aes(end.date, rag.m.lower, col="model2"),
              linetype="dashed") +
    geom_path(data=model,
              aes(end.date, rag.m.upper, col="model2"),
              linetype="dashed") +
    geom_path(data=ref,
              aes(end.date, ..pol, col="ambient")) +
    xlab("") +
    ylab(temp) +
    scale_color_discrete(name="") +
    theme_bw() +
    theme(legend.position = "top")
}


