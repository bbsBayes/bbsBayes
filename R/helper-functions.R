getSpeciesAOU <- function(species, sp.eng)
{
  return(species[species$english == sp.eng, "sp.bbs"])
}
