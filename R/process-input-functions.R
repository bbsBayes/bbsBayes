process_auto_bbs_input <- function(species_list,
                         model, output_dir)
{
  process_species_list(species_list)
  process_model(model)
  process_output_dir(output_dir)
}

process_species_list <- function(species_list)
{
  # Check for at least 1 species to test.
  if (is.null(species_list))
  {
    stop("no species were provided to auto_bbs().\n")
  }
}

process_model <- function(model)
{
  # Check for at least 1 model to test
  if (is.null(model))
  {
    stop("no model was specified to auto_bbs().\n")
  }
  else
  {
    if ((tolower(model) %in% c("standard", "fd")) == FALSE)
    {
      stop(paste(model, "is not a valid model.\n"))
    }
  }
}

process_output_dir <- function(output_dir)
{
  # Check for output directory
  if (is.null(output_dir))
  {
    warning(paste("no output directory was specified.",
                  "Sending output to", getwd()))
  }
}
