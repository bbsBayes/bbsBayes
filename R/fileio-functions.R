create_run_directory <- function(output_dir, species_to_run, model)
{
  dir1 <- output_dir
  if (is.null(output_dir))
  {
    dir1 <- getwd()
  }
  dir_spsp <- paste(dir1, "/",
                    species_to_run,
                    "-",
                    model,
                    "-",
                    format(Sys.Date(), format="%Y-%m-%d"),
                    "-",
                    format(Sys.time(), format="%H%M%S"), sep = "")

  dir.create(dir1, showWarnings = FALSE)
  dir.create(dir_spsp)

  return(dir_spsp)
}

