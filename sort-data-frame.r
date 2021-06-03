# Solution to sorting data frames
# By http://www.markvanderloo.eu/yaRb/2014/08/15/sort-data-frame/

sort.data.frame <- function(x, decreasing=FALSE, by=1, ... ){
  f <- function(...) order(...,decreasing=decreasing)
  i <- do.call(f,x[by])
  x[i,,drop=FALSE]
}

# #examples
#sort(iris) # default is by 1st column
#sort(iris, by="Sepal.Length")
#sort(iris, by=c("Species","Sepal.Length"))
#sort(iris, by=1:2)
#sort(iris, by="Sepal.Length",decreasing=TRUE)
