# function renaming a column in an R data frame without knowing its index number
# USAGE: my.df <- col.rename(my.df, "old", "new") 
# adapted from https://stackoverflow.com/questions/7531868/how-to-rename-a-single-column-in-a-data-frame
col.rename <- function(df=NA, old.var.name=NA, new.var.name=NA){
    df <- df # duplicate data frame
    names(df)[names(df) == old.var.name] <- new.var.name
    return(df)
}
