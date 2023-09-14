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
    wind_sp = WS_1_1_1,
    L = (-((USTAR^3) * (TA_1_1_1 + 273)) / (0.4 * 9.8 * (H / (1.25 * 1004)))),
    H = H,
    temp_atmos = TA_1_1_1,
    sigma_v = sqrt((u_mean*((-1.3*L + 0.1)^2))/100000),
    u_star = USTAR,
    wind_dir = WD_1_1_1,
    test = zm/L,
    #adding associated fluxes
    GPP = GPP_PI,
    NEE = NEE_PI,
    RECO = RECO_PI,
    le = LE,
    ppfd = PPFD_IN_PI_F,
    precip = P,
    rel_h = RH_1_1_1,
    swc = SWC_PI_1_1_A,
    VPD = VPD
  ) %>%
  filter(test >= -15.5)%>%
  filter(u_star > 0.25)%>%
  select(yyyy, mm, doy, day, HH_UTC, MM, wind_sp, L, u_star, wind_dir, temp_atmos, H, GPP, NEE, RECO, le, ppfd, precip, rel_h, swc, VPD)%>%
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
vars = c("wind_sp", "VPD", "temp_atmos", "rel_h", "GPP", "RECO", "NEE")

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
  labs(x = "Wind Direction (degrees)", y = "Density") +
  ggtitle("Probability Density Function of Wind Direction")

#===============================================================================
#Finding mean TWI value and PFT values from rasters clipped by
#by wind direction in QGIS

files = list.files('C:/Users/lindseybell/OneDrive - University of Arizona/Documents/Footprints/data/summary_R_files', full.names = TRUE)

NW.twi = raster(files[3])
SE.twi = raster(files[13])
NW.RAP = stack(files[1])
SE.RAP = stack(files[11])








