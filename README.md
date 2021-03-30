
# Table of Contents

1.  [Description](#org14181a3)
2.  [Usage in R](#org99db7ad)


<a id="org14181a3"></a>

# Description

Scripts containing (mostly) R code with functions useful for analysis


<a id="org99db7ad"></a>

# Usage in R

Set a path to either web or disk where source files reside

    pth <- ifelse (Sys.info()["user"] %in% c("eric", "magar"),
              "~/Dropbox/data/useful-functions",
              "https://raw.githubusercontent.com/emagar/useful-functions/master"
    )

then read desired function

    # Reads sortBy function
    source( paste(pth, "sortBy.r", sep = "/") )

