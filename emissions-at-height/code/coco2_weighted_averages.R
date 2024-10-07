library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)

# Set working directory
setwd("C:/Users/done231/OneDrive - PNNL/Documents/GitHub/emissions-at-height")

# Create combined VP + catalogue file, if not already created
if (!file.exists(file.path("code","vp_catalogue_combined.csv"))) {
  # Load in values from csv
  vp_extrap <- read.csv(file.path("output","vertical_profiles_extrapolated.csv"))
  vertical_profiles <- read.csv(file.path("coco2_ps_database_v2_0","coco2_ps_vertical_profiles_v2.0.csv"))
  catalogue <- read.csv(file.path("coco2_ps_database_v2_0","coco2_ps_catalogue_v2.0.csv"))
  
  # Get values from catalogue for each vp
  vp_catalogue <- data.frame()
  for (i in 1:nrow(catalogue)) {
    # Select matching profile from catalogue
    id <- catalogue[i,]$ID_VertProf
    vp_row <- vertical_profiles[vertical_profiles$ID_VertProf == id,]
    combined_row <- left_join(catalogue[i,],vp_row)
    vp_catalogue <- bind_rows(vp_catalogue, combined_row)
    vp_catalogue$X <- NULL
  }
  
  # Save as csv
  write.csv(vp_catalogue,file.path("code","vp_catalogue_combined.csv"))
} else {
  # Load in combined VP + catalogue file
  vp_catalogue <- read.csv(file.path("code","vp_catalogue_combined.csv"))
  vp_catalogue$X <- NULL
}

# Convert fractions to total values
co2_catalogue <- vp_catalogue 
nox_catalogue <- vp_catalogue
sox_catalogue <- vp_catalogue
co_catalogue <- vp_catalogue
ch4_catalogue <- vp_catalogue

for (i in 1:nrow(vp_catalogue)) {
  co2 <- vp_catalogue[i,]$co2_emis_ty
  nox <- vp_catalogue[i,]$nox_emis_ty
  sox <- vp_catalogue[i,]$sox_emis_ty
  co <- vp_catalogue[i,]$co_emis_ty
  ch4 <- vp_catalogue[i,]$ch4_emis_ty
  
  co2_catalogue[i,15:31] <- co2_catalogue[i,15:31]*co2
  nox_catalogue[i,15:31] <- nox_catalogue[i,15:31]*nox
  sox_catalogue[i,15:31] <- sox_catalogue[i,15:31]*sox
  co_catalogue[i,15:31] <- co_catalogue[i,15:31]*co
  ch4_catalogue[i,15:31] <- ch4_catalogue[i,15:31]*ch4
}

co2_catalogue <- co2_catalogue %>% subset(select=-c(nox_emis_ty,sox_emis_ty,co_emis_ty,ch4_emis_ty))
nox_catalogue <- nox_catalogue %>% subset(select=-c(co2_emis_ty,sox_emis_ty,co_emis_ty,ch4_emis_ty))
sox_catalogue <- sox_catalogue %>% subset(select=-c(co2_emis_ty,nox_emis_ty,co_emis_ty,ch4_emis_ty))
co_catalogue <- co_catalogue %>% subset(select=-c(co2_emis_ty,nox_emis_ty,sox_emis_ty,ch4_emis_ty))
ch4_catalogue <- ch4_catalogue %>% subset(select=-c(co2_emis_ty,nox_emis_ty,sox_emis_ty,co_emis_ty))

colnames(co2_catalogue)[6] <- "emis_ty"
colnames(nox_catalogue)[6] <- "emis_ty"
colnames(sox_catalogue)[6] <- "emis_ty"
colnames(co_catalogue)[6] <- "emis_ty"
colnames(ch4_catalogue)[6] <- "emis_ty"

# Get data in long format
get_long_catalogue <- function(catalogue) {
  long_catalogue <- catalogue %>%
    pivot_longer(
      cols = starts_with("r"),
      names_to = "range",
      values_to = "value") %>%
    subset(select = -c(co2_emis_ty,nox_emis_ty,sox_emis_ty,co_emis_ty,ch4_emis_ty))
  return(long_catalogue)
}

co2_long <- get_long_catalogue(co2_catalogue)
nox_long <- get_long_catalogue(nox_catalogue)
sox_long <- get_long_catalogue(sox_catalogue)
co_long <- get_long_catalogue(co_catalogue)
ch4_long <- get_long_catalogue(ch4_catalogue)

## Get national weighted averages
get_national_weighted_avg <- function(catalogue) {
  national_catalogue <- catalogue %>%
    group_by(ISO3) %>%
    summarize(across(starts_with("r"), sum))
  national_catalogue[,2:18] <- national_catalogue[,2:18]/national_catalogue$rTOT
  # Keep total emission column
  national_catalogue$emis_ty <- (catalogue %>% 
                                   group_by(ISO3) %>% 
                                   summarize(emis_ty = sum(emis_ty)))$emis_ty
  return(national_catalogue)
}

co2_national <- get_national_weighted_avg(co2_catalogue)
nox_national <- get_national_weighted_avg(nox_catalogue)
sox_national <- get_national_weighted_avg(sox_catalogue)
co_national <- get_national_weighted_avg(co_catalogue)
ch4_national <- get_national_weighted_avg(ch4_catalogue)

# Save to csv
write.csv(co2_national,file.path("output","co2_national_weighted_averages.csv"))
write.csv(nox_national,file.path("output","nox_national_weighted_averages.csv"))
write.csv(sox_national,file.path("output","sox_national_weighted_averages.csv"))
write.csv(co_national,file.path("output","co_national_weighted_averages.csv"))
write.csv(ch4_national,file.path("output","ch4_national_weighted_averages.csv"))

## Get continental averages
iso_continents_list <- read.csv(file.path("iso_continents.csv"),na.strings="")
iso3_continents <- iso_continents_list %>% select(c(CC,ISO3))

co2_catalogue_CC <- left_join(co2_catalogue,iso3_continents)
nox_catalogue_CC <- left_join(nox_catalogue,iso3_continents)
sox_catalogue_CC <- left_join(sox_catalogue,iso3_continents)
co_catalogue_CC <- left_join(co_catalogue,iso3_continents)
ch4_catalogue_CC <- left_join(ch4_catalogue,iso3_continents)

get_continental_weighted_avg <- function(catalogue) {
  continental_catalogue <- catalogue %>%
    group_by(CC) %>%
    summarize(across(starts_with("r"), sum))
  continental_catalogue[,2:18] <- continental_catalogue[,2:18]/continental_catalogue$rTOT
  # Keep total emission column
  continental_catalogue$emis_ty <- (catalogue %>% 
                                      group_by(CC) %>% 
                                      summarize(emis_ty = sum(emis_ty)))$emis_ty
  return(continental_catalogue)
}

co2_continental <- get_continental_weighted_avg(co2_catalogue_CC)
nox_continental <- get_continental_weighted_avg(nox_catalogue_CC)
sox_continental <- get_continental_weighted_avg(sox_catalogue_CC)
co_continental <- get_continental_weighted_avg(co_catalogue_CC)
ch4_continental <- get_continental_weighted_avg(ch4_catalogue_CC)

# Save to csv
write.csv(co2_continental,file.path("output","co2_continental_weighted_averages.csv"))
write.csv(nox_continental,file.path("output","nox_continental_weighted_averages.csv"))
write.csv(sox_continental,file.path("output","sox_continental_weighted_averages.csv"))
write.csv(co_continental,file.path("output","co_continental_weighted_averages.csv"))
write.csv(ch4_continental,file.path("output","ch4_continental_weighted_averages.csv"))

## Other regional averages
get_regional_weighted_average <- function(catalogue,region_list,region_name="region") {
  region_average <- filter(catalogue, ISO3 %in% region_list) %>%
    summarize(across(starts_with("r"), sum)) %>%
    mutate(region = region_name) %>%
    select(region,everything())
  region_average[,2:18] <- region_average[,2:18]/region_average$rTOT
  # Keep total emission column
  region_average$emis_ty <- sum(filter(catalogue, ISO3 %in% region_list)$rTOT)
  return(region_average)
}

#EU-27
EU27 <- c("AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN",
          "FRA","DEU","GRC","HUN","IRL","ITA","LVA","LTU","LUX",
          "MLT","NLD","POL","PRT","ROU","SVK","SVN","ESP","SWE")

EU27_average_co2 <- get_regional_weighted_average(co2_catalogue,EU27,"EU-27")
EU27_average_nox <- get_regional_weighted_average(nox_catalogue,EU27,"EU-27")
EU27_average_sox <- get_regional_weighted_average(sox_catalogue,EU27,"EU-27")
EU27_average_co <- get_regional_weighted_average(co_catalogue,EU27,"EU-27")
EU27_average_ch4 <- get_regional_weighted_average(ch4_catalogue,EU27,"EU-27")

EU27_averages <- bind_rows(
  EU27_average_co2 %>% mutate(species = "CO2"),
  EU27_average_nox %>% mutate(species = "NOx"),
  EU27_average_sox %>% mutate(species = "SOx"),
  EU27_average_co %>% mutate(species = "CO"),
  EU27_average_ch4 %>% mutate(species = "CH4")
)

#East Asia excluding China
eastasia_no_china <- c("JPN","MNG","KOR","PRK","TWN")

EA_average_co2 <- get_regional_weighted_average(co2_catalogue,eastasia_no_china,"East Asia excluding China")
EA_average_nox <- get_regional_weighted_average(nox_catalogue,eastasia_no_china,"East Asia excluding China")
EA_average_sox <- get_regional_weighted_average(sox_catalogue,eastasia_no_china,"East Asia excluding China")
EA_average_co <- get_regional_weighted_average(co_catalogue,eastasia_no_china,"East Asia excluding China")
EA_average_ch4 <- get_regional_weighted_average(ch4_catalogue,eastasia_no_china,"East Asia excluding China")

EA_averages <- bind_rows(
  EA_average_co2 %>% mutate(species = "CO2"),
  EA_average_nox %>% mutate(species = "NOx"),
  EA_average_sox %>% mutate(species = "SOx"),
  EA_average_co %>% mutate(species = "CO"),
  EA_average_ch4 %>% mutate(species = "CH4")
)

# Former USSR
formerUSSR <- c("ARM","AZE","BLR","EST","GEO","KAZ","KGZ","LVA",
          "LTU","MDA","RUS","TJK","TKM","UKR","UZB")

formerUSSR_average_co2 <- get_regional_weighted_average(co2_catalogue,formerUSSR,"Former USSR")
formerUSSR_average_nox <- get_regional_weighted_average(nox_catalogue,formerUSSR,"Former USSR")
formerUSSR_average_sox <- get_regional_weighted_average(sox_catalogue,formerUSSR,"Former USSR")
formerUSSR_average_co <- get_regional_weighted_average(co_catalogue,formerUSSR,"Former USSR")
formerUSSR_average_ch4 <- get_regional_weighted_average(ch4_catalogue,formerUSSR,"Former USSR")

formerUSSR_averages <- bind_rows(
  formerUSSR_average_co2 %>% mutate(species = "CO2"),
  formerUSSR_average_nox %>% mutate(species = "NOx"),
  formerUSSR_average_sox %>% mutate(species = "SOx"),
  formerUSSR_average_co %>% mutate(species = "CO"),
  formerUSSR_average_ch4 %>% mutate(species = "CH4")
)

# Middle East
middleeast <- c("BHR","CYP","EGY","IRN","IRQ","ISR","JOR","KWT","LBN",
                "OMN","PSE","QAT","SAU","SYR","TUR","ARE","YEM")

middleeast_average_co2 <- get_regional_weighted_average(co2_catalogue,middleeast,"Middle East")
middleeast_average_nox <- get_regional_weighted_average(nox_catalogue,middleeast,"Middle East")
middleeast_average_sox <- get_regional_weighted_average(sox_catalogue,middleeast,"Middle East")
middleeast_average_co <- get_regional_weighted_average(co_catalogue,middleeast,"Middle East")
middleeast_average_ch4 <- get_regional_weighted_average(ch4_catalogue,middleeast,"Middle East")

middleeast_averages <- bind_rows(
  middleeast_average_co2 %>% mutate(species = "CO2"),
  middleeast_average_nox %>% mutate(species = "NOx"),
  middleeast_average_sox %>% mutate(species = "SOx"),
  middleeast_average_co %>% mutate(species = "CO"),
  middleeast_average_ch4 %>% mutate(species = "CH4")
)

# Bind all into one dataframe
regional_averages <- bind_rows(EU27_averages,EA_averages,formerUSSR_averages,middleeast_averages)

# Reorder columns
regional_averages <- regional_averages %>%
  select(region, species, everything())

# Save as csv
write.csv(regional_averages,file.path("output","regional_weighted_averages.csv"))
