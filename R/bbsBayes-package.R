.onLoad <- function(libname, pkgname)
{
  # Eventually this might just be a web scraper from USGS site
  data("birds", package = pkgname, envir = parent.env(environment()))
  data("datacount.sp", package = pkgname, envir = parent.env(environment()))
  data("route", package = pkgname, envir = parent.env(environment()))
  data("sp", package = pkgname, envir = parent.env(environment()))
  data("st.areas", package = pkgname, envir = parent.env(environment()))
  data("midyear", package = pkgname, envir = parent.env(environment()))
}
