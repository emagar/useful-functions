###########################################################
## Handy function to sort one data frame's rows by order ##
## of another, dimensionally-matching data frame         ##
##                                                       ##
## Author: Eric Magar emagar at itam dot mx              ##
## Created: 16mar2021                                    ##
## Revised: 16mar2021                                    ##
###########################################################

sortBy <- function(target, By){
    t <- target; b <- By;
    do.call(rbind, lapply(seq_len(nrow(b)), 
            function(i) as.character(unlist(t[i,])[order(unlist(-b[i,]))]))) # change to -b for decreasing order
}

#############
## example ##
#############
## v1 <- data.frame(c1=c(30,15,3), c2=c(10,25,2), c3=c(20,35,4))
## l1 <- data.frame(c1=c("thirty","fifteen","three"), c2=c("ten","twenty-five","two"), c3=c("twenty","thirty-five","four"))
## v1.sorted <- t(apply(v1, 1, function(x) sort(x, decreasing = TRUE))) # sort each row of df -- http://stackoverflow.com/questions/6063881/sorting-rows-alphabetically
## l1.sorted <- sortBy(target = l1, By = v1)
## sortBy(target = v1, By = v1)
 
#############
## comment ##
#############
## this sorts matrix rows faster than function above
## vot <- t(apply(v1, 1, function(x) sort(x, decreasing = TRUE)))


