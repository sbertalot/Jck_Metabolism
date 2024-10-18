library(dplyr)
library(lubridate)
library(patchwork)
library(ggplot2)

wad_minidot <- read.csv("MiniDOT Final/wad.csv")
wad_hobo <- read.csv("HOBO Final/wad.csv")

merged_df <- merge(wad_minidot, wad_hobo, by.x = "Mountain.Standard.Time", by.y = "datetime")

plot(merged_df$Temperature, merged_df$temp)

merged_df <- merged_df %>% 
  select(Mountain.Standard.Time, date, time, Temperature, temp, lux, Dissolved.Oxygen, Dissolved.Oxygen.Saturation, Q) %>% 
  rename(datetime = colnames(merged_df)[1], temp_minidot = colnames(merged_df)[4], temp_hobo = colnames(merged_df)[5], do = colnames(merged_df)[7], do_sat = colnames(merged_df)[8])

ggplot(merged_df, aes(x = datetime, y = do_sat)) +
  geom_point()

ggplot(merged_df, aes(x = datetime, y = lux)) +
  geom_point()
