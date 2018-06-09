.onLoad <- function(libname, pkgname)
{
  assign("models",
         list(standard = "standard.jags",
                 fd = "first-difference.jags"),
         parent.env(environment()))
}
