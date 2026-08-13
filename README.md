- [Description](#org3be18e1)
- [Usage in R](#orga95e6f6)


<a id="org3be18e1"></a>

# Description

Scripts containing (mostly) R code with functions useful for analysis


<a id="orga95e6f6"></a>

# Usage in R

Set a path to either web or disk where source files reside

```r-base
pth <- ifelse (Sys.info()["user"] %in% c("eric", "magar"),
          "~/Dropbox/data/useful-functions",
          "https://raw.githubusercontent.com/emagar/useful-functions/master"
)
```

then read desired function (eg. function `sortBy.r`)

```r-base
# Reads sortBy function
source( paste(pth, "sortBy.r", sep = "/") )
```
