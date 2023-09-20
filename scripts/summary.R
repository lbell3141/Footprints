#9/13/2023
#summarizing differences in drivers, response vars, and GS data
#===============================================================================
#loading libraries and data; defining variables
library(lubridate)
library(dplyr)
library(ggplot2)
library(zoo)
library(plantecophys)
devtools::install_github("cardiomoon/ggiraphExtra")
library(gridExtra)
library(tidyr)
library(cowplot)
library(sp)
library(raster)
library(ggspatial)

dat_file <- read.csv("C:/Users/lindseybell/OneDrive - University of Arizona/Documents/Footprints/data/AMF_US-CMW_BASE_HH_2-5.csv", na.strings = "-9999", header = TRUE, sep = ",", skip = 2)
dat_file <- read.csv("data/AMF_US-CMW_BASE_HH_2-5.csv", na.strings = "-9999", header = TRUE, sep = ",", skip = 2)

meas_h <- 14
d <- (2/3) * meas_h
dat_file$TIMESTAMP_START <- ymd_hm(as.character(dat_file$TIMESTAMP_START))

#use library plantecophys to calc VPD

dat_file$VPD = RHtoVPD(dat_file$RH_1_1_1, dat_file$TA_1_1_1, dat_file$PA)
#===============================================================================
#create data frame with desired variables and filters for daytime, precip, and u*.
#Test variable is to fit parameters of Kljun FFP code
dat_voi = dat_file %>%
  mutate(
    yyyy = year(TIMESTAMP_START),
    mm = month(TIMESTAMP_START),
    doy = yday(TIMESTAMP_START),
    day = day(TIMESTAMP_START),
    HH_UTC = hour(TIMESTAMP_START),
    MM = minute(TIMESTAMP_START),
    zm = meas_h,
    d = d, 
    u_mean = mean(WS_1_1_1, na.rm = TRUE),
    WS = WS_1_1_1,
    L = (-((USTAR^3) * (TA_1_1_1 + 273)) / (0.4 * 9.8 * (H / (1.25 * 1004)))),
    H = H,
    TA = TA_1_1_1,
    sigma_v = sqrt((u_mean*((-1.3*L + 0.1)^2))/100000),
    u_star = USTAR,
    wind_dir = WD_1_1_1,
    test = zm/L,
    #adding associated fluxes
    GPP = GPP_PI,
    NEE = NEE_PI,
    RECO = RECO_PI,
    le = LE,
    PPFD = PPFD_IN_PI_F,
    precip = P,
    RH = RH_1_1_1,
    swc = SWC_PI_1_1_A,
    VPD = VPD
  ) %>%
  filter(test >= -15.5)%>%
  filter(u_star > 0.25)%>%
  dplyr::select(yyyy, mm, doy, day, HH_UTC, MM, WS, L, u_star, wind_dir, TA, H, GPP, NEE, RECO, le, PPFD, precip, RH, swc, VPD)%>%
  filter(if_any(everything(), ~ . != "NA"))%>%
  filter(HH_UTC >= 8 & HH_UTC <= 17)%>%
  filter(lag(precip) == 0, lead(precip) == 0)%>%
  filter(precip == 0)

#subset main data into frames NW ad SE directions
#also subsetting to times of greatest flux difference (winter)
dat_voi_A <- dat_voi%>%
  filter(wind_dir >= 270 & wind_dir <= 350)%>%
  filter(doy %in% c(0:100, 330:366))
dat_voi_B <- dat_voi%>%
  filter(wind_dir >= 90 & wind_dir <= 170)%>%
  filter(doy %in% c(0:100, 330:366))

#looping over vars to find means for either direction
vars = c("WS", "VPD", "TA", "RH", "PPFD", "GPP", "RECO", "NEE")

#NW:
means_A <- numeric(length(vars))
for (i in 1:length(vars)) {
  means_A[i] <- mean(dat_voi_A[[vars[i]]], na.rm = TRUE)
}

#SE:
means_B <- numeric(length(vars))
for (i in 1:length(vars)) {
  means_B[i] <- mean(dat_voi_B[[vars[i]]], na.rm = TRUE)
}

#put into one dataframe
df_means_A <- data.frame(Variable = vars, Mean_NW = means_A)
df_means_B <- data.frame(Variable = vars, Mean_SE = means_B)

sum_df <- merge(df_means_A, df_means_B, by = "Variable")

#finding difference of means
sum_df$Difference <- sum_df$Mean_NW - sum_df$Mean_SE

#===============================================================================
#making a PDF for wind direction 
ggplot(dat_voi, aes(x = wind_dir)) +
  geom_density(alpha = 0.2, fill = "green") +
  annotate('rect', xmin=90, xmax=170, ymin=0, ymax=0.005, alpha=.2, fill='red')+
  annotate('rect', xmin=270, xmax=350, ymin=0, ymax=0.005, alpha=.2, fill='red')+
  labs(x = "Wind Direction (degrees)", y = "Density") +
  ggtitle("Probability Density Function of Wind Direction")

#===============================================================================
#Finding mean TWI value and PFT values from rasters clipped by
#by wind direction in QGIS
# _A = NW; _B = SE

files = list.files('C:/Users/lindseybell/OneDrive - University of Arizona/Documents/Footprints/data/summary_R_files', full.names = TRUE)

TWI_A = raster(files[3])
TWI_B = raster(files[5])
RAP_A = stack(files[2])
RAP_B = stack(files[4])

mean_TWI_A = mean(TWI_A[], na.rm = TRUE)
mean_TWI_B = mean(TWI_B[], na.rm = TRUE)

#average percent cover for each PFT
rap.functional.classes = c('AFG','BGR','LTR','PFG','SHR','TRE')

means_RAP_A = numeric(nlayers(RAP_A))
means_RAP_B = numeric(nlayers(RAP_B))

for (i in 1:nlayers(RAP_A)){
  functional.group <- rap.functional.classes[i]
  means_RAP_A[i] <- mean(RAP_A[[i]], na.rm = TRUE)
}
for (i in 1:nlayers(RAP_B)){
  functional.group <- rap.functional.classes[i]
  means_RAP_B[i] <- mean(RAP_B[[i]], na.rm = TRUE)
}

#adding to previous summary dataframe
df_mean_twi_A <- data.frame(Variable = "TWI", Mean_NW = mean_TWI_A)
df_mean_twi_B <- data.frame(Variable = "TWI", Mean_SE = mean_TWI_B)
sum_df_twi <- merge(df_mean_twi_A, df_mean_twi_B, by = "Variable")
sum_df_twi$Difference <- sum_df_twi$Mean_NW - sum_df_twi$Mean_SE

df_mean_rap_A <- data.frame(Variable = rap.functional.classes, Mean_NW = means_RAP_A)
df_mean_rap_B <- data.frame(Variable = rap.functional.classes, Mean_SE = means_RAP_B)
sum_df_RAP <- merge(df_mean_rap_A, df_mean_rap_B, by = "Variable")
sum_df_RAP$Difference <- sum_df_RAP$Mean_NW - sum_df_RAP$Mean_SE

sum_df = rbind(sum_df, sum_df_twi, sum_df_RAP)

#formatting summary table
#reorder variables:
var_order <- c("PPFD", "RH", "TA", "VPD", "WS", "TWI", "BGR", "LTR", "AFG", "PFG", "SHR", "TRE", "RECO", "GPP", "NEE")
sum_df$Variable <- factor(sum_df$Variable, levels = var_order)
sum_df <- sum_df %>% arrange(Variable)

sum_table <- tableGrob(sum_df)
plot(sum_table)

#===============================================================================
#reformatting graphs 
rap = stack('C:/Users/lindseybell/OneDrive - University of Arizona/Documents/Footprints/Land_Cover/FFP_RAP_VegCover_2017.tif')

rap.major.pft.map = function(rap) {
  
  # Get spatial info
  rap.crs = crs(rap)
  rap.ext = extent(rap)
  
  nrows = dim(rap)[1]
  ncols = dim(rap)[2]
  
  # Get RAP PFT layer names
  rap.pfts = names(rap)
  
  # Set up 2D map output
  output.map = rap[[1]]
  output.map[] = NA
  output.map = as.matrix(output.map)
  
  # Convert RAP raster stack to 3D array
  rap.array = as.array(rap)
  
  # Loop through rows/columns to get percent pixel coverage of each PFT vegetation cover class
  for (row in 1 : nrows){
    for (col in 1 : ncols){
      
      # Get vector of %pixel of each PFT (see index below) at specifc row+col
      # Index = 1-6; 1=AFG, 2=BGR, 3=LTR, 4=PFG, 5=SHR, 6=TRE
      pfts = rap.array[row,col,]
      
      # Get the index value of the major PFT (i.e. maximum %pixel coverage)
      major.pft = which(pfts == max(pfts, na.rm = TRUE), arr.ind = TRUE)
      
      # If there are two or more PFTs with the same %pixel coverage, randomly select one and assign the index value to output pixel
      if (length(major.pft) > 1){
        output.map[row,col] = major.pft[sample(length(major.pft),1)]
      } else {output.map[row,col] = major.pft}
    }
  }
  
  # Rasterize 2D output map and assign spatial info
  output.map = raster(output.map)
  crs(output.map) = rap.crs
  extent(output.map) = rap.ext
  
  return(output.map)
}

major.pft.map = rap.major.pft.map(rap)
# Define a custom color palette
custom_palette <- c( "AFG" = "#FFCCCC",  
                     "BGR" = "#E6E6FA",  
                     "LTR" = "#CCCCFF",
                     "TRE" = "#CCFFCC",
                     "SHR" = "#FFD700",
                     "PFG" = "#FFFF99" )

# Plot the raster with the custom color palette
plot(major.pft.map, xlab = "", ylab = "", col = custom_palette[major.pft.map[]], axes = FALSE)
title("Dominant PFT per Footprint Pixel")

# Create a custom legend
legend("topright", legend = names(custom_palette[4:6]), fill = unname(custom_palette[4:6]))

#overlay shapefile of FFP and remove right legend 