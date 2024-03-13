read.excel <- function(header=TRUE,...) {
    ## From https://www.r-bloggers.com/2013/02/copying-data-from-excel-to-r-and-back/
    ## Usage: copy from excel into clipboard then dat <- read.excel()
    read.table("clipboard",sep="\t",header=header,...)
}

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
    ## From https://www.r-bloggers.com/2013/02/copying-data-from-excel-to-r-and-back/
    ## Usage: write.excel(dat)
    write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

