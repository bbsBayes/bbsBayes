.onLoad <- function(libname, pkgname)
{
  assign("maps",
         list(bbs_usgs = "BBS_USGS_strata",
              bbs_cws = "BBS_CWS_strata",
              state = "BBS_ProvState_strata",
              bcr = "BBS_BCR_strata",
              latlong = "BBS_LatLong_strata"),
         parent.env(environment()))

  assign("models",
         list(slope = "slope.jags",
              firstdiff = "first-difference.jags",
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
