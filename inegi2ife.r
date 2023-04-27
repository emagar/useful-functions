##########################################################################
## FUNCTION TO CONVERT IFE TO INEGI MUNICIPAL CODES AND VICE VERSA      ##
## Input is a vector                                                    ##
## Output is a same-length same-order vector                            ##
## Problem: values not in i2i map remain unchanged (good) but mixed     ##
## among changed values (bad); not a problem if all values are present. ##
##                                                                      ##
## Usage:                                                               ##
##     inegi2ife(3008)                                                  ##
##     > 3004                                                           ##
##     ife2inegi(3004)                                                  ##
##     > 3008                                                           ##
##########################################################################

inegi2ife <- function(v=NA, inegi_to_ife=TRUE, ife_to_inegi=FALSE){
    ## Usage:                                                              
    ## inegi2ife(3008)
    ## > 3004                                                          
    ## ife2inegi(3004)
    ## > 3008
    require(plyr)
    if (is.vector(v)==FALSE) stop("Error: v must be a vector")
    ##
    ## THE SOURCE MAP IS THIS:
    tmp <- "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/ancillary/mun.yrs.csv"
    i2i <- read.csv(file=tmp)
    i2i <- i2i[,c("inegi","ife","mun","edon")] # drop unneded columns
    ## Drop municipios with codes pending from map
    ##table(is.na(i2i$ife))           # debug
    ##table(is.na(i2i$inegi))         # debug
    drop.r <- which(is.na(i2i$ife))
    ##i2i[drop.r,]                    # debug
    if (length(drop.r)>0) i2i <- i2i[-drop.r,]
                                        #
    ##v <- d$ife # debug
    zdata <- data.frame(inp=v)
    zdata$ord <- 1:nrow(zdata) # keep original order
    zdata$outp <- mapvalues(zdata$inp, from = i2i$inegi, to = i2i$ife,   warn_missing=FALSE)
    ##
    zdata <- zdata[order(zdata$ord),] # re-sort into original order 
    return(zdata$outp)
}

ife2inegi <- function(v=NA, inegi_to_ife=TRUE, ife_to_inegi=FALSE){
    ## Usage:                                                              
    ## inegi2ife(3008)
    ## > 3004                                                          
    ## ife2inegi(3004)
    ## > 3008
    require(plyr)
    if (is.vector(v)==FALSE) stop("Error: v must be a vector")
    ##
    ## THE SOURCE MAP IS THIS:
    tmp <- "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/ancillary/mun.yrs.csv"
    i2i <- read.csv(file=tmp)
    i2i <- i2i[,c("inegi","ife","mun","edon")] # drop unneded columns
    ## Drop municipios with codes pending from map
    ##table(is.na(i2i$ife))           # debug
    ##table(is.na(i2i$inegi))         # debug
    drop.r <- which(is.na(i2i$ife))
    ##i2i[drop.r,]                    # debug
    if (length(drop.r)>0) i2i <- i2i[-drop.r,]
                                        #
    ##v <- d$ife # debug
    zdata <- data.frame(inp=v)
    zdata$ord <- 1:nrow(zdata) # keep original order
    ##
    zdata$outp <- plyr::mapvalues(zdata$inp, from = i2i$ife, to = i2i$inegi, warn_missing=FALSE)
    ##
    zdata <- zdata[order(zdata$ord),] # re-sort into original order 
    return(zdata$outp)
}
