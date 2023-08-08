#loading libraries  
library(lubridate)
library(dplyr)
library(ggplot2)
library(gridExtra)

directory <- "C:/Users/lindseybell/OneDrive - University of Arizona/Documents/Footprints/data/Fluxnet2015"
file_names <- list.files(directory, pattern = ".csv", full.names = TRUE)
data_frames = list()
for(file in file_names){
  data <- read.csv(file, header = TRUE, na.strings= "-9999", sep = ",")
  data_frames[[basename(file)]]<-data
}
for (file in file_names) {
  data <- read.csv(file, header = TRUE, na.strings = "-9999", sep = ",")
  data$TIMESTAMP_START <- ymd_hm(as.character(data$TIMESTAMP_START))
  data$Day <- day(data$TIMESTAMP_START)
  data$Month <- month(data$TIMESTAMP_START)
  data$Year <- year(data$TIMESTAMP_START)
  data$doy <- yday(data$TIMESTAMP_START)
  data_frames[[basename(file)]] <- data
}

for (df_name in names(data_frames)) {
  df <- data_frames[[df_name]]
  means <- df %>%
    group_by(doy) %>%
    summarize(across(where(is.numeric), mean, na.rm = TRUE))
  data_frames[[df_name]] <- means
}  



  selected_var <- "GPP_DT_VUT_75"
  
  plots <- lapply(data_frames, function(df) {
    ggplot(df, aes(x = doy, y = !!sym(selected_var))) +
      geom_line() +
      labs(title = basename(names(df)), x = "Day of Year", y = "Mean GPP")
  })
  grid.arrange(grobs = plots, ncol = 2)  # Adjust ncol as needed

  combined_data <- bind_rows(data_frames, .id = "Dataset")
    ggplot(combined_data, aes(x = doy, y = !!sym(selected_var), color = Dataset)) +
    geom_line() +
    labs(title = "GPP Comparison for Four Sites", x = "Day of Year", y = "DOY Averaged GPP") +
    scale_color_discrete(name = "Dataset")
#======= 


    
    selected_var <- "NEE_VUT_75"
    
    plots <- lapply(data_frames, function(df) {
      ggplot(df, aes(x = doy, y = !!sym(selected_var))) +
        geom_line() +
        labs(title = basename(names(df)), x = "Day of Year", y = "Mean NEE")
    })
    grid.arrange(grobs = plots, ncol = 2)  # Adjust ncol as needed
    
    combined_data <- bind_rows(data_frames, .id = "Dataset")
    ggplot(combined_data, aes(x = doy, y = !!sym(selected_var), color = Dataset)) +
      geom_line() +
      labs(title = "NEE Comparison for Four Sites", x = "Day of Year", y = "DOY Averaged NEE") +
      scale_color_discrete(name = "Dataset")
    