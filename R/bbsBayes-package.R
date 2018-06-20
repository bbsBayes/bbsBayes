.onLoad <- function(libname, pkgname)
{
  assign("models",
         list(standard = "standard.jags",
              s = "standard.jags",
              first-difference = "first-difference.jags",
              first-diff = "first-difference.jags",
              fd = "first-difference.jags",
              f = "first-difference.jags",
              gam = "gam.jags",
              g = "gam.jags"),
         parent.env(environment()))

  assign("strata",
         list(bbs = "bbs.csv",
              state = "state-prov.csv",
              bcr = "bcr.csv",
              latlong = "degree-blocks.csv"),
         parent.env(environment()))
}
