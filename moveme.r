#############################################################################################
## Function to move columns of a dataframe easily from                                     ##
## stackoverflow.com/questions/3369959/moving-columns-within-a-data-frame-without-retyping ##
##                                                                                         ##
## Usage:                                                                                  ##
##   df[moveme(names(df), "g first")]                                                      ##
##   df[moveme(names(df), "g first; a last; e before c")]                                  ##
##                                                                                         ##
## The basic options are:                                                                  ##
## first                                                                                   ##
## last                                                                                    ##
## before                                                                                  ##
## after                                                                                   ##
## Compounded moves are separated by a semicolon.                                          ##
#############################################################################################

moveme <- function (invec, movecommand) {
    ## Usage:                                                 ##
    ##   df[moveme(names(df), "g first")]                     ##
    ##   df[moveme(names(df), "g first; a last; e before c")] ##
    ##                                                        ##
    ## The basic options are:                                 ##
    ## - first                                                ##
    ## - last                                                 ##
    ## - before                                               ##
    ## - after                                                ##
    ## Compounded moves are separated by a semicolon.         ##
  movecommand <- lapply(strsplit(strsplit(movecommand, ";")[[1]], 
                                 ",|\\s+"), function(x) x[x != ""])
  movelist <- lapply(movecommand, function(x) {
    Where <- x[which(x %in% c("before", "after", "first", 
                              "last")):length(x)]
    ToMove <- setdiff(x, Where)
    list(ToMove, Where)
  })
  myVec <- invec
  for (i in seq_along(movelist)) {
    temp <- setdiff(myVec, movelist[[i]][[1]])
    A <- movelist[[i]][[2]][1]
    if (A %in% c("before", "after")) {
      ba <- movelist[[i]][[2]][2]
      if (A == "before") {
        after <- match(ba, temp) - 1
      }
      else if (A == "after") {
        after <- match(ba, temp)
      }
    }
    else if (A == "first") {
      after <- 0
    }
    else if (A == "last") {
      after <- length(myVec)
    }
    myVec <- append(temp, values = movelist[[i]][[1]], after = after)
  }
  myVec
}
