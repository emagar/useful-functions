###########################################################################
## función para tabular frequencias absolutas and relativas intercaladas ##
## (takes a vector, reports freqs)                                       ##
##                                                                       ##
## Author: Eric Magar emagar at itam dot mx                              ##
## Created: 9abr2022                                                     ##
## Revised: 9abr2022                                                     ##
###########################################################################

tab.ar <- function(vec, rd=0){
    tmpa <-        table(vec, useNA = "ifany")                       # absol freqs
    tmpr <- round( table(vec, useNA = "ifany") *100 / nrow(dat), rd) # relat freqs
    #
    tmp <- table(1:(2*dim(tmpa)))         # prep empty, twice-long table
    #
    for (i in 1:dim(tmpa)){
        target.a <- i+(i-1)
        target.r <- 2*i
        tmp[target.a] <- tmpa[i]
        tmp[target.r] <- tmpr[i]
        names(tmp)[target.a] <- names(tmpa)[i]
        names(tmp)[target.r] <- names(tmpr)[i]
    }
    return(tmp)
}
