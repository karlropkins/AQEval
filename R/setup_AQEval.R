############################################
## #' @title setup_AQEval
############################################

##################################
#remove before public release
#if not using/recommending
##################################

## #' @name setup_AQEval
## #' @description Attempts to load packages needed to run
## #' \code{AQEval}.
## #' @param pkg (character) Package name, default
## #' 'AQEval'.

#Hoping remotes::install_local works and
#currently not export this



#splatted function
#currently not exporting
## #' @export
setup_AQEval <-
function(pkg = "AQEval"){
    #load package depends, etc.
    #plist <- c("sf", "tmap", "openair", "lattice", "loa", "readr",
    #          "ggplot2", "strucchange", "segmented")
    ###################################
    #not tracking packages in local library
    ###################################
    #if(length(plist)>0){
    #    pnew <- plist[!(plist %in% installed.packages()[,"Package"])]
    #    if(length(pnew)>0) install.packages(pnew)
    #}
    install.packages(c("openair", "loa", "mgcv", "dplyr", "tidyr",
              "ggplot2", "strucchange", "segmented"), dependencies = TRUE)
}
