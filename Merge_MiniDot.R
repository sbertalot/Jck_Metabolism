library(dplyr)
library(lubridate)
library(patchwork)
library(ggplot2)

merge_data <- function(string) {
  #Creates a string that filters the folder for just data files for the given site
  site_id <- paste("^\\d{6}_", string, "_\\d{6}\\.TXT$", sep = "")
  #Uses the identifier to search the file folder, and creates a list of paths
  site_filenames <- list.files("MiniDOT Raw", pattern = site_id, full.names = TRUE)
  #Initializes an empty list to hold dataframes
  df_list <- list()
  #For loop iterates through the list of files
  for (i in seq_along(site_filenames)) {
    #reads in the file path for index value i
      #Comma separated
      #skip = 7 skips the first 7 lines of the .TXT file (not data)
    data <- read.csv(site_filenames[i], sep=",", header=TRUE, skip = 7)
    #Removes the first row (units)
    data <- data[-1, ]
    #Adds the dataframe to the given index of the list
    df_list[[i]] <- data
  }
  
  #Binds all of the data frames contained in the list
  combined_df <- do.call(rbind, df_list)
  #Removes duplicate rows from the combined data frame
  distinct_combined_df <- distinct(combined_df)
  #Changes both date and time columns to type date rather than character
  distinct_combined_df$UTC_Date_._Time <- as.POSIXct(distinct_combined_df$UTC_Date_._Time, format = "%Y-%m-%d %H:%M:%S")
  distinct_combined_df$Mountain.Standard.Time <- as.POSIXct(distinct_combined_df$Mountain.Standard.Time, format = "%Y-%m-%d %H:%M:%S")
  #Rounds the time for each time to the nearest 10 minute interval
  distinct_combined_df$UTC_Date_._Time <- round_date(distinct_combined_df$UTC_Date_._Time, unit = "10 minutes")
  distinct_combined_df$Mountain.Standard.Time <- round_date(distinct_combined_df$Mountain.Standard.Time, unit = "10 minutes")
  
  return(distinct_combined_df)
}

mci <- merge_data("MCI")
ski <- merge_data("SKI")
spb <- merge_data("SPB")
sab <- merge_data("SAB")
wad <- merge_data("WAD")
tsc <- merge_data("TSC")

write.csv(mci, "MiniDOT Final/mci.csv")
write.csv(ski, "MiniDOT Final/ski.csv")
write.csv(spb, "MiniDOT Final/spb.csv")
write.csv(sab, "MiniDOT Final/sab.csv")
write.csv(wad, "MiniDOT Final/wad.csv")
write.csv(tsc, "MiniDOT Final/tsc.csv")


#plot(mci$Mountain.Standard.Time, mci$Dissolved.Oxygen.Saturation, type = "l")
#plot(ski$Mountain.Standard.Time, ski$Dissolved.Oxygen.Saturation, type = "l")
#plot(spb$Mountain.Standard.Time, spb$Dissolved.Oxygen.Saturation, type = "l")
#plot(sab$Mountain.Standard.Time, sab$Dissolved.Oxygen.Saturation, type = "l")
#plot(wad$Mountain.Standard.Time, wad$Dissolved.Oxygen.Saturation, type = "l")
#plot(tsc$Mountain.Standard.Time, tsc$Dissolved.Oxygen.Saturation, type = "l")
