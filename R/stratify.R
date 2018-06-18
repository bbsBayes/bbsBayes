stratify <- function(bird, route, routes, stratify.by = "bbs")
{
  s.area <- read.csv(system.file("strata",
                                 strata[[stratify.by]],
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

  # area weights, strata names in st files
  st1 <- merge(s.area,abrev,by.x = c("country","prov"),by.y = c("country","code"),all = T)

  # These also may be a product of just this particular stratifcation
  st1[which(st1$prov == "NS" & st1$country == "CA"),"State"] <- "Nova Scotia Prince Edward Island"
  st1[which(st1$prov == "NS" & st1$country == "CA"),"prov"] <- "NSPE"
  st1[which(st1$prov == "BCR7"),"State"] <- "BCR-7"

  st1 <- st1[which(!is.na(st1$St_12)),]

  if (stratify.by == "bbs")
  {
    st1[,"strat.name"] <- paste(st1[,"State"],"-BCR",st1[,"bcr"],sep = "")
  }
  else if (stratify.by == "state")
  {
    st1[,"strat.name"] <- paste(st1[,"State"],sep = "")
  }

  st2 <- merge(st1,regs,by.x = "State",by.y = "State.Prov.TerrName", all.x = T)
  st2[which(st2$prov == "BCR7"),"countrynum"] <- 124
  st2[which(st2$prov == "BCR7"),"RegionCode"] <- 777


  st2[which(st2$prov == "NSPE"),"RegionCode"] <- 765
  st2[which(st2$prov == "NSPE"),"countrynum"] <- 124

  st.areas <- st2
  for (i in 1:nrow(st.areas)) {
    if (nchar(st.areas[i,"RegionCode"]) == 1 ) {
      st.areas[i,"state"] <- paste("0",st.areas[i,"RegionCode"],sep = "")
    }else{
      st.areas[i,"state"] <- paste(st.areas[i,"RegionCode"],sep = "")
    }
  }

  # fix the strata designations on route file
  route$route <- route$Route

  js <- which(route$statenum == 75)
  route[js,"route"] <- paste(route[js,"Route"],"75",sep = "")


  js <- which(route$statenum == 65)
  route[js,"route"] <- paste(route[js,"Route"],"65",sep = "")


  js <- which(route$BCR == 7)
  route[js,"route"] <- paste(route[js,"Route"],"7",sep = "")

  bird[,"sp.bbs"] <- as.integer(as.character(bird[,"AOU"]))

  tmp <- unique(route[,c("BCR","statenum","Route","countrynum")]) # all unique routes by BCR and state

  bird2 <- merge(bird,tmp,by = c("statenum","Route","countrynum"))  ## this merge removes bird data from unacceptable routes (i.e., routes that are unacceptable in all years) and adds the BCR and countrynum info

  bird2$route <- bird2$Route

  js <- which(bird2$statenum == 75)
  bird2[js,"route"] <- paste(bird2[js,"Route"],"75",sep = "")

  js <- which(bird2$statenum == 65)
  bird2[js,"route"] <- paste(bird2[js,"Route"],"65",sep = "")

  js <- which(bird2$BCR == 7)
  bird2[js,"route"] <- paste(bird2[js,"Route"],"7",sep = "")

  bird2[,"state"] <- as.character(bird2[,"statenum"])

  bird2[which(bird2$state == "75"),"state"] <- "765"  # combined PEI and NS
  bird2[which(bird2$state == "65"),"state"] <- "765"

  bird2[which(bird2$BCR == 7),"state"] <- "777"

  route[,"state"] <- as.character(route[,"statenum"])

  route[which(route$state == "75"),"state"] <- "765" # combined PEI and NS
  route[which(route$state == "65"),"state"] <- "765"

  route[which(route$BCR == 7),"state"] <- "777"


  route[,"rt.uni"] <- paste(route[,"state"],route[,"route"],sep = "-")  # regenerates the rt.uni value with newly defined combined states
  bird2[,"rt.uni"] <- paste(bird2[,"state"],bird2[,"route"],sep = "-")

  route[,"rt.uni.y"] <- paste(route[,"state"],route[,"route"],route[,"Year"],sep = "-")  # regenerates the rt.uni.y value with newly defined combined states
  bird2[,"rt.uni.y"] <- paste(bird2[,"state"],bird2[,"route"],bird2[,"Year"],sep = "-")

  birds <- bird2

  birds$runyear <- birds$Year
  route$runyear <- route$Year

  return(list(birds = birds,
              route = route,
              st.areas = st.areas))
}
