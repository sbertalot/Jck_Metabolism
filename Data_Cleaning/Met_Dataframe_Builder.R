library(tidyverse)
library(LakeMetabolizer)

minidot_files <- list.files("MiniDOT Final")
hobo_files <- list.files("HOBO Final")

for (i in seq_along(hobo_files)) {
  name_list <- list("mci.csv", "sab.csv", "ski.csv", "spb.csv", "tsc.csv", "wad.csv")
  
  df1_name <- minidot_files[i]
  df2_name <- hobo_files[i]
  
  df1_path <- paste("MiniDOT Final/", df1_name, sep = "")
  df2_path <- paste("HOBO Final/", df2_name, sep = "")
  
  df1 <- read.csv(df1_path)
  df2 <- read.csv(df2_path)
  
  merged_df <- merge(df1, df2, by = "datetime")
  
  merged_df <- merged_df %>% 
    na.omit() %>% 
    select(datetime, temp.x, do, do_sat, lux) %>% 
    rename(temp = temp.x) %>% 
    mutate(datetime = as.POSIXct(merged_df$datetime, format = "%Y-%m-%d %H:%M:%S"),
           par_est = lux*0.0185)
  
  final_path <- paste("Met_Data/", name_list[i], sep = "")
  write.csv(merged_df, final_path)
}



# df1_name <- minidot_files[1]
# df2_name <- hobo_files[1]
# 
# df1_path <- paste("MiniDOT Final/", df1_name, sep = "")
# df2_path <- paste("HOBO Final/", df2_name, sep = "")
# 
# df1 <- read.csv(df1_path)
# df2 <- read.csv(df2_path)
# 
# merged_df <- merge(df1, df2, by = "datetime")
# merged_df <- na.omit(merged_df)
# 
# merged_df <- merged_df %>% 
#   select(datetime, temp.x, do, do_sat, lux) %>% 
#   rename(temp = temp.x) %>% 
#   mutate(datetime = as.POSIXct(merged_df$datetime, format = "%Y-%m-%d %H:%M:%S"))


