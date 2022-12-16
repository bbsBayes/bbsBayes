#' @keywords internal
"_PACKAGE"

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "bbsBayes v", utils::packageVersion("bbsBayes"), "\n",
    "Note: version 3+ represents a major shift in functionality, noteably:\n",
    " - The Bayesian modelling engine has switched from *JAGS* to *Stan*\n",
    " - The workflow has been streamlined, resulting in deprecated/renamed\n",
    "   function arguments\n",
    "See the documentation for more details: ",
    "https://BrandonEdwards.github.io/bbsBayes"
    )
}


# Dealing with CRAN Notes due to Non-standard evaluation
.onLoad <- function(libname = find.package("bbsBayes"),
                    pkgname = "bbsBayes"){
  if(getRversion() >= "2.15.1")
    utils::globalVariables(
      # Vars used in Non-Standard Evaluations, declare here to
      # avoid CRAN warnings
      c("." # piping requires '.' at times
      )
    )
  backports::import(pkgname, "R_user_dir", force = TRUE)
}
