get_species_aou <- function(species, sp_eng)
{
  aou <- NULL
  if (length(aou <- species[species$english == sp_eng, "sp.bbs"]) == 1)
  {
    return(aou)
  }
  else if (length(aou <- species[species$french == sp_eng, "sp.bbs"]) == 1)
  {
    return(aou)
  }
  else if (length(aou <- species[species$spanish == sp_eng, "sp.bbs"]) == 1)
  {
    return(aou)
  }
  else
  {
    return(-1)
  }

}
