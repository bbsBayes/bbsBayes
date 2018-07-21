#' Stratify raw Breeding Bird Survey data
#'
#' Assigns each bird count data point and each route a strata
#'   based on its geographic location and the stratification
#'   as specified by the user.
#'
#' @param bbs_data Large list of raw BBS data. Can be obtained using
#'   the \code{fetchBBSdata()} function.
#' @param stratify_by String argument of stratification type. Defaults
#'   to "bbs", which is the standard BCR X State used in BBS data analysis.
#'   Other options are "state", "bcr", "degree_block"
#' @param quiet Should progress bars be suppressed?
#'
#' @return Large list (3 elements) consisting of:
#'   \item{bird_strat}{Dataframe of stratified bird data}
#'   \item{route_strat}{Dataframe of stratified route data}
#'   \item{species_strat}{Dataframe of stratified species data}
#'   \item{stratify_by}{Argument used for stratification}
#'
#' @examples
#'
#' \dontrun{
#'
#' # Download BBS data and stratify by traditional BBS stratifications.
#' bbs <- fetch_bbs_data()
#' data_strat <- stratify(bbs_data = bbs, stratify_by = "bbs")
#'
#' # Stratify by Bird Conservation Regions only
#' # If you don't need the raw BBS data, you can call fetch_bbs_data()
#' #   directly in the stratify command.
#' data_strat <- stratify(bbs_data = fetch_bbs_data(), stratify_by = "bcr")
#'
#' # Stratify by State/Province/Territory only
#' data_strat <- stratify(bbs_data = fetch_bbs_data(), stratify_by = "state")
#'
#' # Stratify by blocks of 1 degree of latitude X 1 degree of longitude
#' data_strat <- stratify(bbs_data = fetch_bbs_data(), stratify_bs = "degree_block")
#'
#' # You can choose not to specify a stratification type and default to BBS stratification
#' data_strat <- stratify(bbs_data = fetch_bbs_data())
#' }
#'
#' @importFrom utils read.csv
#' @importFrom progress progress_bar
#' @export
#'

stratify <- function(bbs_data,
                     stratify_by = "bbs",
                     quiet = FALSE)
{
  if (!isTRUE(quiet))
  {
    pb <- progress_bar$new(
      format = "Stratifying data   [:bar] :percent eta: :eta",
      clear = FALSE,
      total = 7,
      width = 80)
    pb$tick(0)
  }

  bird <- bbs_data$bird; if (!isTRUE(quiet)){pb$tick()}
  route <- bbs_data$route; if (!isTRUE(quiet)){pb$tick()}

  if (stratify_by == "bbs")
  {
    route[,"strat_name"] <- paste(route[,"Country"],
                                  route[,"St_Abrev"],
                                  route[,"BCR"],
                                  sep = "-")
  }else if (stratify_by == "state")
  {
    route[,"strat_name"] <- paste(route[,"St_Abrev"],
                                  sep = "")
  }else if (stratify_by == "bcr")
  {
    route[,"strat_name"] <- paste("BCR",
                                  route[,"BCR"],
                                  sep = "")
  }else if (stratify_by == "latlong")
  {
    route[,"strat_name"] <- paste(trunc(route[,"Latitude"]),
                                  trunc(route[,"Longitude"]),
                                  sep = "_")
  }else if (stratify_by == "bbs_can")
  {
    # Combine all BCR 7
    route[which(route$BCR == 7),"St_Abrev"] <- "BCR7"
    route[which(route$BCR == 7), "Route"] <- 7
    bird[which(bird$BCR == 7), "Route"] <- 7
    route[which(route$BCR == 7), "statenum"] <- 777
    bird[which(bird$BCR == 7), "statenum"] <- 777

    # Combine NS and PEI
    route[which(route$State == "Nova Scotia"), "State"] <- "Nova Scotia Prince Edward Island"
    route[which(route$State == "Prince Edward Island"), "State"] <- "Nova Scotia Prince Edward Island"
    route[which(route$State == "Nova Scotia Prince Edward Island"), "St_Abrev"] <- "NSPE"
    route[which(route$statenum == 65), "Route"] <- 65
    bird[which(bird$statenum == 65), "Route"] <- 65
    route[which(route$statenum == 75), "Route"] <- 75
    bird[which(bird$statenum == 75), "Route"] <- 75
    route[which(route$St_Abrev == "NSPE"), "statenum"] <- 765
    bird[which(bird$statenum == 65), "statenum"] <- 765
    bird[which(bird$statenum == 75), "statenum"] <- 765

    route[,"strat_name"] <- paste(route[,"Country"],
                                  route[,"St_Abrev"],
                                  route[,"BCR"],
                                  sep = "-")

  }; if (!isTRUE(quiet)){pb$tick()}

  route[,"rt.uni"] <- paste(route[,"statenum"],route[,"Route"],sep = "-"); if (!isTRUE(quiet)){pb$tick()} # regenerates the rt.uni value with newly defined combined states
  bird[,"rt.uni"] <- paste(bird[,"statenum"],bird[,"Route"],sep = "-"); if (!isTRUE(quiet)){pb$tick()}

  route[,"rt.uni.y"] <- paste(route[,"statenum"],route[,"Route"],route[,"Year"],sep = "-"); if (!isTRUE(quiet)){pb$tick()}  # regenerates the rt.uni.y value with newly defined combined states
  bird[,"rt.uni.y"] <- paste(bird[,"statenum"],bird[,"Route"],bird[,"Year"],sep = "-"); if (!isTRUE(quiet)){pb$tick()}


  return(list(bird_strat = bird,
              route_strat = route,
              species_strat = bbs_data$species,
              stratify_by = stratify_by))
}
