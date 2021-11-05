
read.clipboard <- function(sep=","){
     con <- pipe("xclip -selection clipboard -i", open="r")
     read.table(con, header = TRUE)
     close(con)
}


d <- read.clipboard()
