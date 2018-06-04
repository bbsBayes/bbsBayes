.onLoad <- function(libname, pkgname)
{
  # Eventually this might just be a web scraper from USGS site
  data("bbs", package = pkgname, envir = parent.env(environment()))
}
