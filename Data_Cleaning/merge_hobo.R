library(dplyr)
library(lubridate)
library(patchwork)
library(ggplot2)

merge_data <- function(string) {
  #Creates a string that filters the folder for just data files for the given site
  site_id <- paste("^\\d{8}_", string, "_\\d{6}\\.csv$", sep = "")
  site_filenames <- list.files("HOBO Data Raw CSV", pattern = site_id, full.names = TRUE)
  #Uses the identifier to search the file folder, and creates a list of paths
  
  #Initializes an empty list to hold dataframes
  df_list <- list()
    #For loop iterates through the list of files
    for (i in seq_along(site_filenames)) {
      #reads in the file path for index value i
      #Comma separated
      #skip = 7 skips the first 7 lines of the .TXT file (not data)
      data <- read.csv(site_filenames[i], sep=",", header=TRUE)
      #Adds the dataframe to the given index of the list
      df_list[[i]] <- data
  }
  #Binds all of the data frames contained in the list
  combined_df <- do.call(rbind, df_list)
  #Removes first column (redundant index column)
  combined_df <- combined_df[, -1]
  #Renames remaining columns to easier names 
  combined_df <- combined_df %>% rename(datetime = colnames(combined_df)[1], temp = colnames(combined_df)[2], lux = colnames(combined_df)[3])
  #Changes the temperature column from F to C
  combined_df$temp <- ((combined_df$temp-32)*(5/9))

  #Formats datetime column correctly
  combined_df$datetime <- as.POSIXct(combined_df$datetime, format = "%m/%d/%y %H:%M")
  #Separates just date column
  combined_df$date <- as.Date(combined_df$datetime)
  #Separates just time column
  combined_df$time <- format(combined_df$datetime, format = "%H:%M:%S")
  #Reorders columns so date and time are the 2nd and 3rd column
  combined_df <- combined_df[, c(4, 5, 1, 2, 3)]
  #Removes any overlapping values (shouldn't exist for the hobo data but still)
  distinct_combined_df <- distinct(combined_df)
  #Returns the merged dataframe 
  return(distinct_combined_df)
}

mci <- merge_data("MCI")
ski <- merge_data("SKI")
spb <- merge_data("SPB")
sab <- merge_data("SAB")
wad <- merge_data("WAD")
tsc <- merge_data("TSC")

write.csv(mci, "HOBO Final/mci.csv")
write.csv(ski, "HOBO Final/ski.csv")
write.csv(spb, "HOBO Final/spb.csv")
write.csv(sab, "HOBO Final/sab.csv")
write.csv(wad, "HOBO Final/wad.csv")
write.csv(tsc, "HOBO Final/tsc.csv")
