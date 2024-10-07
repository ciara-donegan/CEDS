library(ggplot2)
library(dplyr)

# Set working directory
setwd("C:/Users/done231/OneDrive - PNNL/Documents/RStudio/emissions_at_height")

# Load in values from csv
catalogue <- read.csv("coco2_ps_database_v2_0/coco2_ps_catalogue_v2.0.csv")
vertical_profiles <- read.csv("coco2_ps_database_v2_0/coco2_ps_vertical_profiles_v2.0.csv")

# Get IDs
VP_IDs <- vertical_profiles$ID_VertProf

# Function to add points in linearly (n = space between profile heights)
add_datapoints <- function(df,n) {
  added_df <- data.frame(range=as.numeric(),
                         fraction=as.numeric())
  
  for (i in 1:(nrow(df)-1)) {
    # Slope between points
    x <- c(df[i,1],df[i+1,1])
    y <- c(df[i,2],df[i+1,2])
    m <- (y[2]-y[1])/(x[2]-x[1])
    b <- y[2] - m*x[2]
    
    # Extrapolate to new points
    added_x <- seq(x[1],x[2],n)
    added_y <- m*added_x + b
    loop_df <- data.frame(range=added_x,
                          fraction=added_y)
    added_df <- bind_rows(added_df,loop_df)
  }
  # Remove repeat heights
  added_df <- unique(added_df)
  return(added_df)
}

# Extrapolate across all profiles
extrap_profiles <- data.frame(range=as.numeric(),fraction=as.numeric())
ranges <- seq(0,1500,100)
for (id in VP_IDs) {
  profile <- vertical_profiles[vertical_profiles$ID_VertProf==id,]
  fractions <- as.numeric(profile[,2:17])
  df <- data.frame(range=ranges,fraction=fractions)
  # Extrapolate data to every 25m
  extrap_profile <- add_datapoints(df,25)
  extrap_profile$ID_VertProf <- id
  extrap_profiles <- bind_rows(extrap_profiles,extrap_profile)
}

# Reshape to same format as source (wide in this sense = dataframe in wide format)
wide_profiles <- reshape(extrap_profiles,idvar="ID_VertProf",timevar="range",direction="wide")

# Plot one profile
id <- "VP_04171"
selected <- vertical_profiles[vertical_profiles$ID_VertProf==id,]
df_selected <- data.frame(range=ranges,
                          fraction=as.numeric(selected[,2:17]))
selected_extrap <- wide_profiles[wide_profiles$ID_VertProf==id,]
df_selected_extrap <- data.frame(range=seq(0,1500,25),
                                 fraction=as.numeric(selected_extrap[,2:62]))

plot_profiles <- ggplot() +
  geom_point(data=df_selected_extrap,aes(x=fraction,y=range),color='red') +
  geom_point(data=df_selected,aes(x=fraction,y=range),color='blue',size=3) +
  ggtitle(paste0("Vertical Profile: ",id)) +
  ylab("Height (m)") +
  xlab("Emission Fraction")
plot_profiles

# Export profiles to csv
write.csv(wide_profiles,"vertical_profiles_extrapolated.csv",row.names=FALSE)