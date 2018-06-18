.onLoad <- function(libname, pkgname)
{
  assign("models",
         list(standard = "standard.jags",
                 fd = "first-difference.jags"),
         parent.env(environment()))

  assign("strata",
         list(bbs = "bbs.csv",
              state = "state-prov.csv",
              bcr = "bcr.csv",
              latlong = "degree-blocks.csv"),
         parent.env(environment()))
}
