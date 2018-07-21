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
#' @param ... Optional arguments for JAGS data prepping, modelling, or plotting.
#' See \code{run_model}, \code{prepare_jags_data}, and \code{generate_trend_plots}
#' for other argument details
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

  data_strat <- stratify(bbs_data, stratify_by)

  sp_num <- 1
  total_sp <- length(species)
  for (sp in species)
  {
    cat(paste("Species ",
              sp_num,
              " of ",
              total_sp,
              ": ",
              sp,
              "\n",
              sep = ""))

    cat("Runnings JAGS model\n")
    data_jags <- prepare_jags_data(strat_data = data_strat,
                                 species_to_run = sp,
                                 model = model,
                                 ...)

    jagsjob <- run_model(jags_data = data_jags,
                         ...)

    plots <- plots <- generate_trend_plot(jags_mod = jagsjob,
                                          ...)
    sp_num <- sp_num + 1
  }

  return(list(jags_data = data_jags,
              jags_model = jagsjob,
              plots = plots))
}
