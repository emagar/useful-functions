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
##
##
edon2estado <- function(x){
    edo <- mapvalues(
        x,
        from = 1:32,
        to   = c("Aguascalientes", "Baja California",  "Baja California Sur", "Campeche", "Coahuila", "Colima", "Chiapas",
                 "Chihuahua", "DF/CdMx",  "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "México", "Michoacán",
                 "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa",
                 "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas"),
        warn_missing=FALSE
    )
    return(edo)
}
