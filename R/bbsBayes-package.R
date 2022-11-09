#' @keywords internal
"_PACKAGE"

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "bbsBayes v", utils::packageVersion("bbsBayes"), "\n",
    "Note: version 3+ represents a major shift in functionality, noteably:\n",
    " - The underlying Bayesian modelling engine has switched from *JAGS* to *Stan*\n",
    " - The workflow has been streamlined, resulting in many deprecated/renamed\n",
    "   function arguments\n",
    "See the documentation for more details: https://BrandonEdwards.github.io/bbsBayes"
    )
}
