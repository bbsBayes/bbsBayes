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
#'   Other options are "state", "bcr", "latlong"
#'
#' @return Large list (4 elements) containing stratified point count data,
#' route data, and species data, as well as a data frame containing area
#' information about each strata.
#'
#' @export
#'
#' @examples
#' data.strat <- stratify(bbs_data = data, stratify_by = "bbs")
#' data.strat <- stratify(fetchBBSdata())
#'
stratify <- function(bbs_data, stratify_by = "bbs")
{
  bird <- bbs_data$bird
  route <- bbs_data$route

  s_area <- read.csv(system.file("strata",
                                 "bbs.csv",
                                 package="bbsBayes"),
                     stringsAsFactors = F)

  regs <- read.csv(system.file("helper",
                               "RegionCodes.csv",
                               package="bbsBayes"),
                   stringsAsFactors = F)

  abrev <- read.csv(system.file("helper",
                                "state_abrev.csv",
                                package="bbsBayes"),
                    stringsAsFactors = F)

  pb <- progress_bar$new(
    format = "Stratifying data   [:bar] :percent eta: :eta",
    clear = FALSE,
    total = nrow(s_area) + 32,
    width = 80)
  pb$tick(0)

  # area weights, strata names in st files
  st1 <- merge(s_area,abrev,by.x = c("country","prov"),by.y = c("country","code"),all = T); pb$tick()

  # These also may be a product of just this particular stratifcation
  st1[which(st1$prov == "NS" & st1$country == "CA"),"State"] <- "Nova Scotia Prince Edward Island"; pb$tick()
  st1[which(st1$prov == "NS" & st1$country == "CA"),"prov"] <- "NSPE"; pb$tick()
  if (stratify_by != "state")
  {
    st1[which(st1$prov == "BCR7"),"State"] <- "BCR-7"
  }; pb$tick()

  st1 <- st1[which(!is.na(st1$State)),]
  st1 <- st1[which(!is.na(st1$St_12)),]; pb$tick()

  if (stratify_by == "bbs")
  {
    st1[,"strat.name"] <- paste(st1[,"State"],"-BCR",st1[,"bcr"],sep = "")
  }else if (stratify_by == "state")
  {
    st1[,"strat.name"] <- paste(st1[,"State"],sep = "")
  }else if (stratify_by == "bcr")
  {
    st1[,"strat.name"] <- paste("BCR",st1[,"bcr"],sep = "")
  }; pb$tick()

  st2 <- merge(st1,regs,by.x = "State",by.y = "State.Prov.TerrName", all.x = T); pb$tick()

  if (stratify_by != "state")
  {
    st2[which(st2$prov == "BCR7"),"countrynum"] <- 124
    st2[which(st2$prov == "BCR7"),"RegionCode"] <- 777
  }; pb$tick()

  st2[which(st2$prov == "NSPE"),"RegionCode"] <- 765; pb$tick()
  st2[which(st2$prov == "NSPE"),"countrynum"] <- 124; pb$tick()

  st_areas <- st2; pb$tick()
  for (i in 1:nrow(st_areas)) {
    pb$tick()
    if (nchar(st_areas[i,"RegionCode"]) == 1 ) {
      st_areas[i,"state"] <- paste("0",st_areas[i,"RegionCode"],sep = "")
    }else{
      st_areas[i,"state"] <- paste(st_areas[i,"RegionCode"],sep = "")
    }
  }

  # fix the strata designations on route file
  route$route <- route$Route; pb$tick()

  js <- which(route$statenum == 75)
  route[js,"route"] <- paste(route[js,"Route"],"75",sep = ""); pb$tick()


  js <- which(route$statenum == 65)
  route[js,"route"] <- paste(route[js,"Route"],"65",sep = ""); pb$tick()

  if (stratify_by != "state")
  {
    js <- which(route$BCR == 7)
    route[js,"route"] <- paste(route[js,"Route"],"7",sep = ""); pb$tick()
  }

  bird[,"sp.bbs"] <- as.integer(as.character(bird[,"AOU"])); pb$tick()

  tmp <- unique(route[,c("BCR","statenum","Route","countrynum")]); pb$tick() # all unique routes by BCR and state

  bird2 <- merge(bird,tmp,by = c("statenum","Route","countrynum")); pb$tick()  ## this merge removes bird data from unacceptable routes (i.e., routes that are unacceptable in all years) and adds the BCR and countrynum info

  bird2$route <- bird2$Route; pb$tick()

  js <- which(bird2$statenum == 75)
  bird2[js,"route"] <- paste(bird2[js,"Route"],"75",sep = ""); pb$tick()

  js <- which(bird2$statenum == 65)
  bird2[js,"route"] <- paste(bird2[js,"Route"],"65",sep = ""); pb$tick()

  if (stratify_by != "state")
  {
    js <- which(bird2$BCR == 7)
    bird2[js,"route"] <- paste(bird2[js,"Route"],"7",sep = ""); pb$tick()
  }

  bird2[,"state"] <- as.character(bird2[,"statenum"]); pb$tick()

  bird2[which(bird2$state == "75"),"state"] <- "765"  # combined PEI and NS
  bird2[which(bird2$state == "65"),"state"] <- "765"; pb$tick()

  if (stratify_by != "state")
  {
    bird2[which(bird2$BCR == 7),"state"] <- "777"
  }; pb$tick()

  route[,"state"] <- as.character(route[,"statenum"]); pb$tick()

  route[which(route$state == "75"),"state"] <- "765" # combined PEI and NS
  route[which(route$state == "65"),"state"] <- "765"; pb$tick()

  if (stratify_by != "state")
  {
    route[which(route$BCR == 7),"state"] <- "777"
  }; pb$tick()

  route[,"rt.uni"] <- paste(route[,"state"],route[,"route"],sep = "-")  # regenerates the rt.uni value with newly defined combined states
  bird2[,"rt.uni"] <- paste(bird2[,"state"],bird2[,"route"],sep = "-"); pb$tick()

  route[,"rt.uni.y"] <- paste(route[,"state"],route[,"route"],route[,"Year"],sep = "-")  # regenerates the rt.uni.y value with newly defined combined states
  bird2[,"rt.uni.y"] <- paste(bird2[,"state"],bird2[,"route"],bird2[,"Year"],sep = "-"); pb$tick()

  birds <- bird2; pb$tick()

  birds$runyear <- birds$Year
  route$runyear <- route$Year; pb$tick()

  return(list(bird_strat = birds,
              route_strat = route,
              species_strat = bbs_data$species,
              strata = st_areas))
}
