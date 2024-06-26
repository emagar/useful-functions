###################################################
## Handy function to make contingency tables     ##
## of another, dimensionally-matching data frame ##
##                                               ##
## Author: Eric Magar emagar at itam dot mx      ##
## Created: 28may2021                            ##
## Revised: 18apr2024                            ##
###################################################

myxtab <- function(r=NA, c=NA, margin=1, rel=TRUE, pct=FALSE, marginals=TRUE, digits=1){
    ## Tabulates with marginal quantities
    ## Usage eg.: myxtab(r=d$nribs, c=d$sex, margin=2, rel=TRUE, pct=FALSE, marginals=TRUE)
    ##
    tmp <- table(r,c)
    pct <- ifelse(pct==FALSE | rel==FALSE, 1, 100)
    if (margin==1 & rel==TRUE  & marginals==TRUE)  tmp <- cbind(round(prop.table(tmp, margin)*pct, digits), tot=rep(pct, nrow(tmp)), N=margin.table(tmp, margin));
    if (margin==1 & rel==TRUE  & marginals==FALSE) tmp <-       round(prop.table(tmp, margin)*pct, digits);
    if (margin==1 & rel==FALSE & marginals==TRUE)  tmp <- cbind(                 tmp,                       tot=rowSums(tmp));
    if (margin==1 & rel==FALSE & marginals==FALSE) tmp <-                        tmp;
    #
    if (margin==2 & rel==TRUE  & marginals==TRUE)  tmp <- rbind(round(prop.table(tmp, margin)*pct, digits), tot=rep(pct, ncol(tmp)), N=margin.table(tmp, margin));
    if (margin==2 & rel==TRUE  & marginals==FALSE) tmp <-       round(prop.table(tmp, margin)*pct, digits);
    if (margin==2 & rel==FALSE & marginals==TRUE)  tmp <- rbind(                 tmp,                       tot=rowSums(tmp));
    if (margin==2 & rel==FALSE & marginals==FALSE) tmp <-                        tmp;
    return(tmp)
}


