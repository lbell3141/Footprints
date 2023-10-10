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
library(rgdal)
library(maptools)
library(leaflet)

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

#finding % contribution of both WDs
GPP_total <- sum(dat_voi$GPP, na.rm = TRUE)

dat_voi_A <- dat_voi%>%
  filter(wind_dir >= 270 & wind_dir <= 350)
  GPP_total_A <- sum(dat_voi_A$GPP, na.rm = TRUE)
  GPP_contr_A <- (GPP_total_A/GPP_total)*100

dat_voi_B <- dat_voi%>%
  filter(wind_dir >= 90 & wind_dir <= 170)
  GPP_total_B <- sum(dat_voi_B$GPP, na.rm = TRUE)
  GPP_contr_B <- (GPP_total_B/GPP_total)*100

#===============================================================================
#mask twi using half meter data to compare. mask with shapefile for 0.5m res 

hm_TWI = raster('data/summary_R_files/0.5_TWI.tif')
ffp_shp = readOGR(dsn = 'data/summary_R_files/ffp_shp.gpkg')
ffp_shp = spTransform(x = ffp_shp, CRSobj = '+proj=utm +zone=12 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')
A_shp = readOGR(dsn = 'data/summary_R_files/hm_NW_clip.shp')
B_shp = readOGR(dsn = 'data/summary_R_files/hm_SE_clip.shp')

masked_TWI_A = mask(x = hm_TWI, mask = A_shp)
masked_TWI_B = mask(x = hm_TWI, mask = B_shp)

hm_A_mean <- mean(masked_TWI_A[], na.rm = TRUE)
hm_B_mean <- mean(masked_TWI_B[], na.rm = TRUE)
hm_dif <- hm_A_mean-hm_B_mean

#===============================================================================
#Finding mean TWI value and PFT values from rasters clipped by
#by wind direction in QGIS
# _A = NW; _B = SE

#masked TWI and mean found above; still need to mask the RAP data
RAP_dat = stack('Land_Cover/RAP_VegCover_2017.tif')
A_shp = spTransform(x = A_shp, CRSobj = '+proj=utm +zone=12 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')
masked_RAP_A = mask(x = RAP_dat, mask = A_shp)


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
  
  rap.crs = crs(rap)
  rap.ext = extent(rap)
  nrows = dim(rap)[1]
  ncols = dim(rap)[2]
  rap.pfts = names(rap)
  output.map = rap[[1]]
  output.map[] = NA
  output.map = as.matrix(output.map)
  
  rap.array = as.array(rap)
  
  for (row in 1 : nrows){
    for (col in 1 : ncols){
      pfts = rap.array[row,col,]
      
      major.pft = which(pfts == max(pfts, na.rm = TRUE), arr.ind = TRUE)
      
      if (length(major.pft) > 1){
        output.map[row,col] = major.pft[sample(length(major.pft),1)]
      } else {output.map[row,col] = major.pft}
    }
  }
  
  output.map = raster(output.map)
  crs(output.map) = rap.crs
  extent(output.map) = rap.ext
  
  return(output.map)
}

major.pft.map = rap.major.pft.map(rap)
custom_palette <- c( "AFG" = "#FFCCCC",  
                     "BGR" = "#E6E6FA",  
                     "LTR" = "#CCCCFF",
                     "TRE" = "#CCFFCC",
                     "SHR" = "#FFD700",
                     "PFG" = "#FFFF99" )
plot(major.pft.map, xlab = "", ylab = "", col = custom_palette[major.pft.map[]], axes = FALSE)
title("Dominant PFT per Footprint Pixel")
legend("topright", legend = names(custom_palette[4:6]), fill = unname(custom_palette[4:6]))
ffp_shp = readShapeSpatial("C:/Users/lindseybell/OneDrive - University of Arizona/Documents/Footprints/TWI/30_TWI/twi_ffp_sec.shp")
plot(ffp_shp, bg = "transparent", add = TRUE)
#===============================================================================
#fixing moving averages 
dat_voi_A <- dat_voi%>%
  filter(wind_dir >= 270 & wind_dir <= 350)
dat_voi_B <- dat_voi%>%
  filter(wind_dir >= 90 & wind_dir <= 170)

par(mfrow = c(1,1))
dat_A_arr = dat_voi_A %>% arrange(doy)
dat_B_arr = dat_voi_B %>% arrange(doy)

dat_A_arr$movavg_A = rollmean(dat_A_arr$GPP, k = 500, fill = NA)
dat_B_arr$movavg_B = rollmean(dat_B_arr$GPP, k = 500, fill = NA)


dat_A_arr <- dat_A_arr %>%
  group_by(doy) %>%
  summarize(mean_gpp = mean(GPP), 
            se_gpp = sd(GPP) / sqrt(n())) %>%
  ungroup()

dat_B_arr <- dat_B_arr %>%
  group_by(doy) %>%
  summarize(mean_gpp = mean(GPP), 
            se_gpp = sd(GPP) / sqrt(n())) %>%
  ungroup()

ggplot() +
  geom_line(data = dat_A_arr, aes(x = doy, y = movavg_A, color = "Northwestern WD")) +
  geom_line(data = dat_B_arr, aes(x = doy, y = movavg_B, color = "Southeastern WD")) +
  #geom_ribbon(data = dat_A_arr, aes(x = doy, ymin = mean_gpp - se_gpp, ymax = mean_gpp + se_gpp), fill = "blue", alpha = 0.5) +
  #geom_ribbon(data = dat_B_arr, aes(x = doy, ymin = mean_gpp - se_gpp, ymax = mean_gpp + se_gpp), fill = "red", alpha = 0.5) +
  labs(title = "Moving Avg", x = "Day of Year", y = "Mean GPP", color = "") +
  scale_color_manual(values = c("Northwestern WD" = "blue", "Southeastern WD" = "red")) +
  theme_minimal()
#------------------------------
#NEE mov avg
dat_A_arr = dat_voi_A %>% arrange(doy)
dat_B_arr = dat_voi_B %>% arrange(doy)

dat_A_arr$movavg_A_NEE = rollmean(dat_A_arr$NEE, k = 500, fill = NA)
dat_B_arr$movavg_B_NEE = rollmean(dat_B_arr$NEE, k = 500, fill = NA)

dat_A_arr$mean_NEE <- mean(dat_A_arr$NEE, na.rm = TRUE) 
sample_size <- nrow(dat_A_arr)
dat_A_arr$se_NEE <- sd(dat_A_arr$NEE, na.rm = TRUE) / sqrt(sample_size)

dat_B_arr$mean_NEE <- mean(dat_B_arr$NEE, na.rm = TRUE) 
sample_size <- nrow(dat_B_arr)
dat_B_arr$se_NEE <- sd(dat_B_arr$NEE, na.rm = TRUE) / sqrt(sample_size)

ggplot() +
  geom_line(data = dat_A_arr, aes(x = doy, y = movavg_A_NEE, color = "Northwestern WD")) +
  geom_line(data = dat_B_arr, aes(x = doy, y = movavg_B_NEE, color = "Southeastern WD")) +
  geom_ribbon(data = dat_A_arr, aes(x = doy, ymin = mean_NEE - se_NEE, ymax = mean_NEE + se_NEE), fill = "blue", alpha = 0.1) +
  geom_ribbon(data = dat_B_arr, aes(x = doy, ymin = mean_NEE - se_NEE, ymax = mean_NEE + se_NEE), fill = "red", alpha = 0.1) +
  labs(title = "Moving Avg", x = "Day of Year", y = "Mean NEE", color = "") +
  scale_color_manual(values = c("Northwestern WD" = "blue", "Southeastern WD" = "red")) +
  theme_minimal()

#===============================================================================
#Checking for WD frequency during DOY
# Create a function to calculate wind direction frequencies for a given data frame
WD_freq <- function(df) {
  df %>%
    group_by(doy) %>%
    summarise(frequency = n()) %>%
    arrange(doy)
}

# Calculate wind direction frequencies for both data frames
wind_freq_A <- WD_freq(dat_voi_A)
wind_freq_B <- WD_freq(dat_voi_B)

# Combine the frequencies into a single data frame
combined_data <- merge(wind_freq_A, wind_freq_B, by = "doy", all = TRUE)
colnames(combined_data) <- c("doy", "Frequency_A", "Frequency_B")

ggplot(combined_data, aes(x = doy)) +
  geom_line(aes(y = Frequency_A, color = "NW"), alpha = 0.7) +
  geom_line(aes(y = Frequency_B, color = "SE"), alpha = 0.7) +
  geom_smooth(aes(y = Frequency_A, color = "NW"), method = "loess", se = FALSE, span = 0.3) +
  geom_smooth(aes(y = Frequency_B, color = "SE"), method = "loess", se = FALSE, span = 0.3) +
  scale_color_manual(values = c("NW" = "blue", "SE" = "red")) +
  annotate('rect', xmin=0, xmax=100, ymin=0, ymax=200, alpha=.05, fill='red')+
  annotate('rect', xmin=145, xmax=190, ymin=0, ymax=200, alpha=.05, fill='red')+
  annotate('rect', xmin=280, xmax=300, ymin=0, ymax=200, alpha=.05, fill='red')+
  annotate('rect', xmin=315, xmax=366, ymin=0, ymax=200, alpha=.05, fill='red')+
  labs(x = "Day of Year", y = "Wind Direction Frequency", color = "Data Frame") +
  theme_minimal()


#-----------------------------------------
#changing to a moving average plot
WD_freq <- function(df) {
  df %>%
    group_by(doy) %>%
    summarise(frequency = n()) %>%
    arrange(doy)
}
wind_freq_A <- WD_freq(dat_voi_A)
wind_freq_B <- WD_freq(dat_voi_B)

# Combine the frequencies into a single frame
combined_data = merge(wind_freq_A, wind_freq_B, by = "doy", all = TRUE)
colnames(combined_data) <- c("doy", "Frequency_A", "Frequency_B")

# Calculate moving averages for both WD
combined_data$Moving_Average_A <- rollapply(combined_data$Frequency_A, width = 15, FUN = mean, fill = NA, align = "right", na.rm = TRUE)
combined_data$Moving_Average_B <- rollapply(combined_data$Frequency_B, width = 15, FUN = mean, fill = NA, align = "right", na.rm = TRUE)

ggplot(combined_data, aes(x = doy)) +
  #geom_line(aes(y = Frequency_A, color = "NW"), alpha = 0.7) +
  #geom_line(aes(y = Frequency_B, color = "SE"), alpha = 0.7) +
  geom_line(aes(y = Moving_Average_A, color = "NW Moving Avg"), alpha = 1) +
  geom_line(aes(y = Moving_Average_B, color = "SE Moving Avg"), alpha = 1) +
  scale_color_manual(values = c("NW" = "lightblue", "SE" = "lightcoral", "NW Moving Avg" = "blue", "SE Moving Avg" = "red")) +
  annotate('rect', xmin=0, xmax=100, ymin=0, ymax=200, alpha=.05, fill='red')+
  annotate('rect', xmin=145, xmax=190, ymin=0, ymax=200, alpha=.05, fill='red')+
  annotate('rect', xmin=280, xmax=300, ymin=0, ymax=200, alpha=.05, fill='red')+
  annotate('rect', xmin=315, xmax=366, ymin=0, ymax=200, alpha=.05, fill='red')+
  labs(x = "Day of Year", y = "Number of Observations", color = "Data Frame") +
  theme_minimal()

#===============================================================================
#Checking for WD frequecy during TOD
# Create a function to calculate wind direction frequencies for a given data frame
WD_freq <- function(df) {
  df %>%
    group_by(HH_UTC) %>%
    summarise(frequency = n()) %>%
    arrange(HH_UTC)
}

# Calculate wind direction frequencies for both data frames
wind_freq_A <- WD_freq(dat_voi_A)
wind_freq_B <- WD_freq(dat_voi_B)

# Combine the frequencies into a single data frame
combined_data <- merge(wind_freq_A, wind_freq_B, by = "HH_UTC", all = TRUE)
colnames(combined_data) <- c("HH_UTC", "Frequency_A", "Frequency_B")

ggplot(combined_data, aes(x = HH_UTC)) +
  geom_line(aes(y = Frequency_A, color = "NW"), alpha = 0.7) +
  geom_line(aes(y = Frequency_B, color = "SE"), alpha = 0.7) +
  geom_smooth(aes(y = Frequency_A, color = "NW"), method = "loess", se = FALSE) +
  geom_smooth(aes(y = Frequency_B, color = "SE"), method = "loess", se = FALSE) +
  scale_color_manual(values = c("NW" = "blue", "SE" = "red")) +
  labs(x = "Time of Day (hr)", y = "Number of Observations", color = "Data Frame") +
  theme_minimal()

#===============================================================================
#RC for premonsoon 
dat_voi_A_pre = dat_voi %>%
  filter(wind_dir >= 270 & wind_dir <= 350) %>%
  filter(doy >= 145 & doy <= 190)
dat_voi_B_pre = dat_voi %>%
  filter(wind_dir >= 90 & wind_dir <= 170) %>%
  filter(doy >= 145 & doy <= 190)

drv_var <- c("WS", "PPFD", "TA", "RH", "VPD")

par(mfrow = c(2, 3))
for (vars in drv_var) {
  plot(dat_voi_A[[vars]], dat_voi_A$GPP,
       xlab = vars,  # Set x-axis label to the variable name
       ylab = "GPP",
       main = "",  # Clear the default title
       pch = 19,    # Use filled circles for points
  )
  points(dat_voi_A_pre[[vars]], dat_voi_A_pre$GPP)
  points(dat_voi_B_pre[[vars]], dat_voi_B_pre$GPP)
  abline(lm(dat_voi_A_pre$GPP ~ dat_voi_A_pre[[vars]]), col = "lightblue")
  abline(lm(dat_voi_B_pre$GPP ~ dat_voi_B_pre[[vars]]), col = "red")
  
  legend("topright", legend = c("Northwestern WD", "Southeastern WD"), col = c("blue", "red"), pch = 19, cex = 0.7)
}


