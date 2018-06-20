#' Automatically run a BBS analysis for a given species
#'
#' \code{auto_bbs} generates MCMC chains for a list of species using BBS
#'   data.
#'
#' @param species Character strings or vector of character strings of what
#'   species are wanting to be analysed
#' @param model Character strings or vector of character strings of what
#'   species are wanting to be analysed.
#' @param bbs_data Large list of BBS data as downloaded using \code{fetchBBSdata()}.
#'   If this argument is left blank, \code{autoBBS()} will default to downloading
#'   the latest data from the USGS website.
#' @param stratify_by Character string of how to stratify the BBS data
#' @param output_dir Path to directory where output should be directed to.
#'   Defaults to current directory.
#' @param ... Optional arguments for JAGS data prepping or modelling.
#' See \code{run_model} and \code{prepare_jags_data} for other argument details
#'
#' @return Some sort of output with cool stuff. Don't know yet
#'
#' @export
#'
auto_bbs <- function(species = NULL,
                    model = NULL,
                    bbs_data = NULL,
                    stratify_by = "bbs",
                    output_dir = NULL,
                    ...)
{
  process_auto_bbs_input(species, model, output_dir)

  if (is.null(bbs_data))
  {
    bbs_data <- fetch_bbs_data()
  }
  species_list <- bbs_data$species

  data_strat <- stratify(bbs_data, stratify_by)

  sp_num <- 1
  total_sp <- length(species)
  for (sp in species)
  {
    cat(paste("Prepping data for species ",
              sp_num,
              " of ",
              total_sp,
              ": ",
              sp,
              "\n",
              sep = ""))
    #cat(paste(sp, model, date(),"\n")) # output information about run and time
    data_jags <- prepare_jags_data(data = data_strat,
                                 species_to_run = sp,
                                 model = model,
                                 ...)

    cat("Beginning JAGS model.\n")
    jagsjob <- run_model(data = data_jags,
                         ...)
    sp_num <- sp_num + 1
  }
}
