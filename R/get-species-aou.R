#' Get BBS numerical code of a species
#'
#' Simply return the 4-5 digit BBS code for the given species.
#'
#' @param species Species List
#' @param sp_eng English name of species
#'
#' @return 4-5 digit AOU code for the given species, -1 if not found
#'
#' @keywords internal
#' @export
#'

get_species_aou <- function(species, sp_eng)
{
  aou <- NULL
  if (length(aou <- species[species$english == sp_eng, "sp.bbs"]) == 1)
  {
    return(aou)
  }else
  {
    return(-1)
  }
}
