.onLoad <- function(libname, pkgname)
{
  assign("models",
         list(standard = "standard.jags",
              fd = "first-difference.jags",
              gam = "gam.jags",
              gamye = "gam-ye.jags"),
         parent.env(environment()))

  assign("strata",
         list(bbs_usgs = "strat.csv",
              bbs_cws = "stratcan.csv",
              state = "stateprov.csv",
              bcr = "bcr.csv",
              latlong = "db.csv"),
         parent.env(environment()))
}
