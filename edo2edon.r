require(plyr)
##
edo2edon <- function(x){
    edon <- mapvalues(
        x,
        from = c("ags", "bc",  "bcs", "cam", "coa", "col", "cps", "cua", "df",  "dgo", "gua", "gue", "hgo", "jal", "mex", "mic",
                 "mor", "nay", "nl",  "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac"),
        to   = 1:32,
        warn_missing=FALSE
    )
    edon <- as.numeric(edon)
    return(edon)
}
##
##
edon2edo <- function(x){
    edo <- mapvalues(
        x,
        from = 1:32,
        to   = c("ags", "bc",  "bcs", "cam", "coa", "col", "cps", "cua", "df",  "dgo", "gua", "gue", "hgo", "jal", "mex", "mic",
                 "mor", "nay", "nl",  "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac"),
        warn_missing=FALSE
    )
    return(edo)
}
