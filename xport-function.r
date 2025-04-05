####################################################################
## Script to export single state-year returns of municipal races. ##
## Invoked from extract-state-yr-mu-returns.r in                  ##
## https://github.com/emagar/elecRetrns.git                       ##
##                                                                ##
## Author: Eric Magar emagar at itam dot mx                       ##
## Created:      13-mar-2021                                      ##
## Last revised: 4-apr-2025                                       ##
####################################################################

xport <- function(e = NA, y = NA, dat = c("aymu1970-on.coalAgg.csv", "aymu1970-on.coalSplit.csv")[1], write.to.file=FALSE){
    ## Function will take municipal data specified in dat
    ## and export it (or write it to file if write.to.file set to TRUE),
    ## re-arranging the data frame so that vote returns appear in columns named after the corresponding party/coalition.
    ## Choose a state (eg. e=2 or e="bc" for Baja California) and a known electoral year (eg. y=2019) to output a dataframe
    ## with municipalities reported in each row. 
    ##
    ## state abbreviations
    edos <- c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac")
    estados <- c("Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua", "Distrito Federal/CDMX", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "México (Estado de)", "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas")
    ##
    #e <- 2; y <- 2019; dat <- c("aymu1970-on.coalAgg.csv", "aymu1970-on.coalSplit.csv")[1] # debug
    ## ## check if state requested in nim abbrev or full name
    ## q <- e %in% c(edos, 1:32) # e==edon??
    ## tmp <- grep(e, edos); if (length(tmp)>0) e <- grep(e, edos) # if char turn to num equivalent (edon)
    ## ## this needs debuging
    ## if (q==FALSE) e <- menu(choices = edos, title = "Select a state") # prompt for state
    ## q <- y %in% as.numeric(names(table(dat$yr[dat$edon==e], useNA = "ifany")))
    ## if (q==FALSE){                                                    # prompt for year
    ##     y <- menu(choices = names(table(dat$yr[dat$edon==e], useNA = "ifany")),
    ##               title = "Choose municipal election year to export")
    ##     y <- as.numeric(names(table(dat$yr[dat$edon==e], useNA = "ifany"))[y])
    ## }
    #
    ## # set data directory here --- SHOULD BE DONE OUTSIDE FUNCTION
    ## dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
    ## # read data with coalition aggregates file
    ## dat <- read.csv(file = paste(dd, "aymu1989-present.coalAgg.csv", sep = ""), stringsAsFactors = FALSE)
    #
    # read data (default is with split coalitions)
    dat <- read.csv(file = dat, stringsAsFactors = FALSE)
    ##
    ## subset data to selection
    dat <- dat[dat$edon==e & dat$yr==y,]
    ## check dim
    if (nrow(dat)==0){
        print("No rows in data, did you select a proper electoral year?")
        break
    }
    ##
    ## extract votes and labels columns as separate objects
    sel.col <- grep("v[0-9]{2}", colnames(dat))
    v <- dat[, sel.col]
    sel.col <- grep("l[0-9]{2}", colnames(dat))
    l <- dat[, sel.col]
    ##
    ## replace - with . in labels (colnames can't have -)
    tmp <- gsub("-", ".", as.vector(t(l)))
    tmp <- data.frame(matrix(tmp, nrow = nrow(l), ncol = ncol(l), byrow = TRUE), stringsAsFactors = FALSE)
    colnames(tmp) <- colnames(l)
    l <- tmp
    ##
    ## generate vl = empty votes columns with party labels as names
    cols <- unique(as.vector(t(l)))
    if (length(which(cols=="0"))>0) cols <- cols[-which(cols=="0")]
    vl <- matrix(0, ncol = length(cols), nrow = nrow(dat))
    vl <- as.data.frame(vl)
    colnames(vl)  <- cols
    ##
    ## loop over vl's columns to fill votes accordingly
    for (c in cols){
        #c <- cols[4] # debug
        # target column indices
        index <- apply(l, 1, FUN = function(x){
            tmp <- which(x==c);
            tmp <- ifelse(length(tmp)==0, 0, tmp)
            return(tmp)
        })
        # rows and col to manipulate in vl
        a <- which(index!=0)        # vl rows to manipulate
        b <- which(colnames(vl)==c) # vl col  to manipulate
        #
        # add index to v as last col
        v <- cbind(v, index=index)
        #
        # manipulate vl accordingly
        vl[,b] <- apply(v, 1, FUN = function(x){ifelse (x["index"]==0, 0, x[x["index"]])})
        #
        v["index"] <- NULL # clean v for next round
        #
        # clean
        rm(index, a, b)
    }
    #
    ## # verify efec
    ## rowSums(vl)==dat$efec
    #
    # break pre- and post votes labels columns to use with manipulated vl 
    sel.col <- grep("[vl][0-9]{2}", colnames(dat))
    dat1 <- dat[, 1:(min(sel.col)-1)]         # columns before votes and labels
    dat2 <- dat[, (max(sel.col)+1):ncol(dat)] # columns after  votes and labels
    #
    ##  manipulated data
    export <- cbind(dat1, vl, dat2)
    ##
    if (write.to.file==FALSE) print(export)
    if (write.to.file==TRUE){
        write.csv(export, file = paste0("xport/", edos[e], y, "aymu.csv"), row.names = FALSE)
        ## Announce all went well
        paste(y, "municipal vote returns for", estados[e], "exported as", paste0("xport/", edos[e], y, "aymu.csv"))
    }
    #
    ## # clean
    ## ls()
}

