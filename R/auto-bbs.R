#' Automatically run a BBS analysis for a given species
#'
#' \code{auto_bbs} generates MCMC chains for a list of species using BBS
#'   data.
#'
#' @param species Character strings or vector of character strings of what
#'   species are wanting to be analysed
#' @param model Character strings or vector of character strings of what
#'   species are wanting to be analysed.
#' @param bbs_data Large list of BBS data as downloaded using \code{fetch_bbs_data}.
#'   If this argument is left blank, \code{auto_bbs} will default to downloading
#'   the latest data from the USGS website.
#' @param stratify_by Character string of how to stratify the BBS data
#' @param generate_report Should \code{auto_bbs} generate an RMarkdown report?
#' @param output_dir Path to directory where RMarkdown report should be sent to
#' @param quiet Should all console output be suppressed?
#' @param ... Optional arguments for JAGS data prepping, modelling, or plotting.
#' See \code{run_model}, \code{prepare_jags_data}, and \code{generate_trend_plots}
#' for other argument details
#'
#' @return Large list. Each entry is a list for unique species-model combination
#'   based on the user's input to \code{species} and \code{model}. So, if 2 species
#'   are entered and 2 models are entered, \code{auto_bbs} returns a list of 4 lists.
#'   Each of these list contains:
#'   \item{model}{The model that was used}
#'   \item{jags_data}{The data that was prepared and used as input for JAGS}
#'   \item{jags_job}{A jagsUI object containing posterior samples from JAGS}
#'   \item{plots}{Plots generated for the species}
#'
#' @examples
#'
#' \dontrun{
#' # Run BBS analysis for Barn Swallow using the standard model
#' # and stratifying by state/province
#' bbs_run <- auto_bbs(species = "Barn Swallow",
#'                     model = "standard",
#'                     stratify_by = "state")
#'
#' # This will produce a list of list entries, with the name of each entry
#' # following the format of species_model, with all whitespace replaced with "_"
#' names(bbs_run)
#' [1] "Barn_Swallow_standard"
#'
#' # We can run more than one species, and more than one model
#' # The following will produce a list with 9 entries for each species X model combination
#' bbs_run <- auto_bbs(species = c("Barn Swallow", "Common Merganser", "Dunlin"),
#'                     model = c("standard", "fd", "gam"),
#'                     stratify_by = "state")
#'
#' # We can specify information about how the JAGS models should be run
#' bbs_run <- auto_bbs(species = "Barn Swallow",
#'                     model = "standard",
#'                     stratify_by = "bbs_usgs",
#'                     n_burnin = 10000,
#'                     n_iter = 5000,
#'                     variable_names = c("n", "sdobs"))
#'
#' }
#'
#' @importFrom stringr str_replace_all
#'
#' @export
#'
auto_bbs <- function(species = NULL,
                    model = NULL,
                    bbs_data = NULL,
                    stratify_by = NULL,
                    generate_report = FALSE,
                    output_dir = NULL,
                    quiet = FALSE,
                    ...)
{
  if (isTRUE(generate_report))
  {
    process_auto_bbs_input(species, model, output_dir)
  }

  if (is.null(bbs_data))
  {
    bbs_data <- fetch_bbs_data(quiet = quiet)
  }

  to_return <- list()

  data_strat <- stratify(bbs_data, stratify_by, quiet = quiet)

  sp_num <- 1
  total_sp <- length(species)
  for (sp in species)
  {
    for (mod in model)
    {
      if (!isTRUE(quiet))
      {
        cat(paste("Species ",
                  sp_num,
                  " of ",
                  total_sp,
                  ": ",
                  sp,
                  " - ",
                  mod,
                  " model\n",
                  sep = ""))
      }

      data_jags <- prepare_jags_data(strat_data = data_strat,
                                     species_to_run = sp,
                                     model = mod,
                                     quiet = quiet,
                                     ...)

      jagsjob <- run_model(jags_data = data_jags,
                           quiet = quiet,
                           ...)

      plots <- plots <- generate_trend_plot(jags_mod = jagsjob,
                                            ...)

      sp_output <- list(model = mod,
                        jags_data = data_jags,
                        jags_job = jagsjob,
                        plots = plots)

      to_return[[str_replace_all(paste(sp, mod, sep = "_"),
                                 "[[:punct:]\\s]+",
                                 "_")]] <- sp_output
    }
    sp_num <- sp_num + 1
  }

  return(to_return)
}
