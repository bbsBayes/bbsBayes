#' Get BBS numerical code of a species
#'
#' Simply return the 4-5 digit BBS code for the given species.
#'
#' @param species Species List
#' @param sp_eng English name of species
#'
#' @return 4-5 digit AOU code for the given species, -1 if not found
#'
#' @noRd

get_species_aou <- function(species, sp_eng) {
  aou <- species$sp.bbs[species$english == sp_eng]
  if (length(aou) != 1) aou <- -1
  aou
}
