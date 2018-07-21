.onLoad <- function(libname, pkgname)
{
  assign("models",
         list(standard = "standard.jags",
              s = "standard.jags",
              firstdifference = "first-difference.jags",
              fd = "first-difference.jags",
              f = "first-difference.jags",
              gam = "gam.jags",
              g = "gam.jags"),
         parent.env(environment()))

  assign("strata",
         list(bbs = "strat.csv",
              bbs_can = "stratcan.csv",
              state = "stateprov.csv",
              bcr = "bcr.csv",
              latlong = "db.csv"),
         parent.env(environment()))
}
