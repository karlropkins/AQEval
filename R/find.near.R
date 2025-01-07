############################################
#' @title find nearby sites
############################################

#' @name find.near
#' @aliases findNearSites findNearLatLon
#' @description Function to find nearest locations in a
#' reference by latitude and longitude.
#' @param lat,lon (numeric) The supplied latitude and
#' longitude.
#' @param nmax (numeric) The maximum number of nearest sites
#' to report, by default 10.
#' @param ... Other parameters, mostly ignored.
#' @param ref (\code{data.frame} or similar) The look-up table to
#' use when identifying nearby locations, and expected to
#' contain latitude, longitude and any required location
#' identifier data-series. By default, \code{findNearSites}
#' uses openair \code{importMeta} output if this is not
#' supplied but this is a required input for
#' \code{findNearLatLon}.
#' @param units (character) The units to use when reporting
#' distances to near locations; current options m.
#' @param pollutant (character) For \code{findNearSites}
#' only, the pollutant of interest, by default NO2.
#' @param site.type (character) For \code{findNearSites}
#' only, the monitoring site type, by default Rural
#' Background.
#' @details
#' If investigating air quality in a particular location,
#' for example a UK Clean Air Zone
#' (\url{https://www.gov.uk/guidance/driving-in-a-clean-air-zone}),
#' you may wish to locate an appropriate rural background air quality
#' monitoring station. \code{findNearSites} locates air quality monitoring
#' sites with openly available data such as that available from the UK AURN
#' network (\url{https://uk-air.defra.gov.uk/networks/network-info?view=aurn})
#' @note This function uses haversine formula to account
#' to the Earth's surface curvature, and uses 6371 km as
#' the radius of earth.
#' @returns \code{find.near} returns \code{data.frame} of near site meta
#' data.
#' @examples
#' #find rural background NO2 monitoring sites
#' #near latitude = 50, longitude = -1
#'
#' #not run: requires internet
#' \dontrun{
#' findNearSites(lat = 50, lon = -1)
#' }

#findNear...
#############################
#main function findNearLatLon
#############################

# should findNearSite be a wrapper for findNearLatLon ???

# think about rename/manage/set data columns local function
#    aqeval_renameDataColumns?

# better error catchers/messages
#    for unexported lat long name handler
#    expected time-series
#    bad calls, missing time-series
#    aqeval_... checkPrep?

# finish documenting
#    rename.lat, rename.lon, rename.ref.lat, rename.ref.lon (if they stay?)

# add references

# ref=openair::importMeta(all=TRUE)
# ref = ref[ref$variable=="NO2" & ref$site_type=="Rural Background", ]
# findNearLatLon(lat=50, lon=1, ref=ref)
# names(ref)[names(ref)=="longitude"] <- "ick"
# findNearLatLon(lat=50, lon=1, ref=ref, rename.ref.lon="ick")

## #' @references need to reference formula
## #' @need to add dont run examples


#' @rdname find.near
#' @export
findNearLatLon <-
  function(lat, lon = NULL,
           nmax = 10, ...,
           ref = NULL, units = "m")
  {
    #find near locations in a reference dataset
    #replaces findNearSite
    x.args <- loa::listUpdate(list(rename.lat=c("latitude", "lat"),
                                   rename.lon=c("longitude", "long", "lon"),
                                   rename.ref.lat=c("latitude", "lat"),
                                   rename.ref.lon=c("longitude", "long", "lon")),
                              list(...))
    #if no ref stop
    if(is.null(ref)){
      stop("findNearLatLon halted, ref not supplied.\n\t(See ?findNearLatLon)",
           call.=FALSE)
    }

    #if data.frame supplied as lat
    #look in this for latitude and longitude
    if(is.data.frame(lat) & is.null(lon)){
      #this assumes lat and long are names
      lon <- aqe_getXFromData(x.args$rename.lon,
                              lat)
      lat <- aqe_getXFromData(x.args$rename.lat, lat)
    }

    #stop if lat and lon not supplied
    #stop if NULLs supplied
    #handle NAs and multiple lat/lon
    if(any(missing(lat), missing(lon))){
      stop("need both lat and lon")
    }
    lat <- unique(lat[!is.na(lat)])
    lon <- unique(lon[!is.na(lon)])
    if(any(length(lat)<1, length(lon)<1)){
      stop("need both lat and lon")
    }
    if(any(length(lat)>1, length(lon)>1)){
      warning("multiple lat/lon supplied; only using first")
      lat <- lat[1]
      lon <- lon[1]
    }

    #this expects ref to be there
    #setup for haversine calcs
    deg2rad <- function(x) x * (pi/180)
    rad2deg <- function(x) x * (180/pi)

    #lat1 <- ref$latitude
    lat1 <- aqe_getXFromData(x.args$rename.ref.lat,
                             ref)
    #lon1 <- ref$longitude
    lon1 <- aqe_getXFromData(x.args$rename.ref.lon,
                             ref)
    lat0 <- rep(lat, length(lat1))
    lon0 <- rep(lon, length(lon1))

    #using The haversine formula
    dLat <- deg2rad(lat1-lat0)
    dLon <- deg2rad(lon1-lon0)
    lat0 <- deg2rad(lat0)
    lat1 <- deg2rad(lat1)

    #the square of half the chord length between the points
    a <- sin(dLat/2) * sin(dLat/2) +
      sin(dLon/2) * sin(dLon/2) * cos(lat0) * cos(lat1)

    #the angular distance in radians
    c <- 2 * atan2(sqrt(a), sqrt(1-a))

    #radius of earth, km
    R <- 6371

    #scaling for output
    sc <- NULL
    if(units == "km") sc <- 1
    if(units == "m") sc <- 1000
    if(is.null(sc)){
      stop("supplied units not recognised")
    }
    #############
    #needs handling for unrecognised units
    #############

    #see previous version findNearLatLon
    #or smoothLatLonPath handling
    #   in old grey.area

    #output in requested scale
    ref$distance <- R * c * sc
    ref <- ref[order(ref$distance),]
    if(nrow(ref)>nmax)
      ref <- ref[1:nmax,]
    #ref <- ref[,unique(c("code", "site", "distance",
    #                    names(ref)))]
    names(ref)[names(ref)=="distance"] <-
      paste("distance", units, sep=".")
    ref
  }



## findNearSites is earlier version of findNear...
## probably going
## splatted function (first time around)

#' @rdname find.near
#' @import openair
#' @export
findNearSites <-
  function(lat, lon, pollutant = "no2",
           site.type = "rural background",
           nmax = 10, ...,
           ref = NULL, units = "m")
  {
    #find near AQ monitoring sites
    #uses openair importMeta

    #setup
    x.args <- list(...)
    #stop if lat and lon not supplied
    #stop if NULLs supplied
    #handle NAs and multiple lat/lon
    if(any(missing(lat), missing(lon))){
      stop("need both lat and lon")
    }
    lat <- unique(lat[!is.na(lat)])
    lon <- unique(lon[!is.na(lon)])
    if(any(length(lat)<1, length(lon)<1)){
      stop("need both lat and lon")
    }
    if(any(length(lat)>1, length(lon)>1)){
      warning("multiple lat/lon supplied; only using first")
      lat <- lat[1]
      lon <- lon[1]
    }
    #get reference if missing
    #this currently only sources data from AURN
    if(is.null(ref)){
      ref <- importMeta(source="aurn", all=TRUE)
    }
    #limit search to sites monitoring pollutant of interest
    ref <- ref[tolower(ref$variable) %in%
                 tolower(pollutant),]
    #limit search to sites of type
    #extra option all rather than have to write lot in full
    #NB: only handle one species at moment
    if(tolower(site.type)!="all"){
      ref <- ref[tolower(ref$site_type) %in%
                   tolower(site.type),]
    }
    #limit search to date ranges
    #trickier than you think...
    #needs very careful documentation
    ##################################
    if("date.range" %in% names(x.args)){
      ############################
      #next bit will error if not
      #formatted correctly
      ############################
      #start: max of mins...
      temp <- as.Date(ref$start_date)
      t1 <- ifelse(temp > min(as.Date(x.args$date.range),
                                                na.rm=TRUE),
                     temp,
                     min(as.Date(x.args$date.range), na.rm=TRUE))
      #end: min of maxs
      temp <- as.Date(gsub("ongoing", Sys.Date(), ref$end_date))
      t2 <- ifelse(temp < max(as.Date(x.args$date.range),
                                               na.rm=TRUE),
                   temp,
                   max(as.Date(x.args$date.range), na.rm=TRUE))
      if("date.overlap" %in% names(x.args)){
        temp <- strsplit(as.character(x.args$date.overlap), " ")[[1]]
        if(length(temp)<2)
          stop("need units if applying date.overlap")
        ############################
        #next bit will error if not
        #formatted correctly
        t2 <- t2 - as.difftime(as.numeric(temp[1]),
                               units=temp[2])
      }
      ref <- ref[t2>t1,]
    }

    #setup for haversine calcs
    deg2rad <- function(x) x * (pi/180)
    rad2deg <- function(x) x * (180/pi)
    lat1 <- ref$latitude
    lon1 <- ref$longitude
    lat0 <- rep(lat, length(lat1))
    lon0 <- rep(lon, length(lon1))

    #using The haversine formula
    dLat <- deg2rad(lat1-lat0)
    dLon <- deg2rad(lon1-lon0)
    lat0 <- deg2rad(lat0)
    lat1 <- deg2rad(lat1)

    #the square of half the chord length between the points
    a <- sin(dLat/2) * sin(dLat/2) +
      sin(dLon/2) * sin(dLon/2) * cos(lat0) * cos(lat1)

    #the angular distance in radians
    c <- 2 * atan2(sqrt(a), sqrt(1-a))

    #radius of earth, km
    R <- 6371

    #scaling for output
    sc <- NULL
    if(units == "km") sc <- 1
    if(units == "m") sc <- 1000

    #############
    #handling for unrecognised units
    #############

    #could make option to replace na's with 0
    #see previous version below
    #or smoothLatLonPath handling

    #output in requested scale
    ref$distance <- R * c * sc
    ref <- ref[order(ref$distance),]
    if(nrow(ref)>nmax)
      ref <- ref[1:nmax,]
    ref <- ref[,unique(c("code", "site", "distance",
                        names(ref)))]
    names(ref)[names(ref)=="distance"] <- paste("distance",
                                                units,
                                                sep=".")
    ref
  }




####################################
#unexported functions
####################################

#used by findNearLatLon
aqe_getXFromData <- function(x, data, ...){
  #consider tolower
  temp <- x[x %in% names(data)]
  if(length(temp)<1){
    stop("'x' not found")
  }
  data[[temp[1]]]
}


#think about
#from
#https://stackoverflow.com/questions/53639265/how-to-use-dynamic-arguments-in-a-dplyr-filter-within-function
#my_func = function(dat, ...){
#  args <- rlang::enexprs(...)
#  dat %>%
#    filter(!!! args)
#}

#my_func = function(dat, ...){
#  args <- enquos(...)
#  ex_args <- unname(imap(args, function(expr, name) quo(!!sym(name)==!!expr)))
#
#  dat %>% filter(!!!ex_args)
#}

#my_func(example_data, seg1 = 'b', seg2 = 'd')
