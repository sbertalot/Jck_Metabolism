library(dplyr)
library(ggplot2)
library(lubridate)

hobo <- read.csv("HOBO Final/mci.csv")

sd(hobo$lux)

storage <- read.csv("jck_storage.csv")
storage <- na.omit(storage)
storage$DateTime <- as.Date(storage$DateTime)

mini <- read.csv("MiniDOT Final/wad.csv")
mean(mini$Dissolved.Oxygen.Saturation)

summary <- read.csv("site_summary.csv")

custom_colors <- c("deep" = "#243E49", "shallow" = "#FFC324")
summary$site <- factor(summary$site, levels = c("SKI", "SAB", "TSC", "MCI", "WAD", "SPB"))

ggplot(summary, aes(x = site, y = temp, color = group)) +
  geom_point(size = 7, show.legend = FALSE) +  
  geom_errorbar(aes(ymin = temp - temp_SD, ymax = temp + temp_SD), width = 0.2) +
  scale_color_manual(values = custom_colors) +
  theme_minimal()

ggplot(summary, aes(x = site, y = light, color = group)) +
  geom_point(size = 7, show.legend = FALSE)  +  
  geom_errorbar(aes(ymin = light - light_SD, ymax = light + light_SD), width = 0.2) +
  scale_color_manual(values = custom_colors) +
  theme_minimal()

ggplot(storage, aes(x = DateTime, y = jck_af)) +
  geom_smooth(size = 1, show.legend = FALSE, color = "#243E49") +
  theme_minimal()

ggplot(hobo, aes(x = datetime, y = lux)) +
  geom_smooth(size = 1, show.legend = FALSE, color = "#243E49") +
  theme_minimal()

new_hobo <- mini %>% 
  mutate(SampDay = day(datetime),
         SampMonth = month(datetime, label = TRUE)) %>% 
  group_by(SampDay, SampMonth) %>% 
  summarise(MeanLight = mean(lux))

ggplot(test, aes(x = Mountain.Standard.Time, y = Dissolved.Oxygen.Saturation)) +
  geom_line(size = 1, color = "#243E49") +
  scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "1 weeks") +
  theme_minimal()


test <- mini
test$Mountain.Standard.Time <- ymd_hms(test$Mountain.Standard.Time) 


min_length <- min(length(mci_light), length(mci_temp), length(sab_light), length(sab_temp), 
                  length(ski_light), length(ski_temp), length(spb_light), length(spb_temp), 
                  length(tsc_light), length(tsc_temp), length(wad_light), length(wad_temp))

# Truncate all vectors to the minimum length
mci_light <- mci_light[1:min_length]
mci_temp <- mci_temp[1:min_length]
sab_light <- sab_light[1:min_length]
sab_temp <- sab_temp[1:min_length]
ski_light <- ski_light[1:min_length]
ski_temp <- ski_temp[1:min_length]
spb_light <- spb_light[1:min_length]
spb_temp <- spb_temp[1:min_length]
tsc_light <- tsc_light[1:min_length]
tsc_temp <- tsc_temp[1:min_length]
wad_light <- wad_light[1:min_length]
wad_temp <- wad_temp[1:min_length]



vio_plot_temp <- data.frame( 
                 ski_temp = ski_temp,
                 sab_temp = sab_temp,
                 tsc_temp = tsc_temp,
                 mci_temp = mci_temp, 
                 wad_temp = wad_temp,
                 spb_temp = spb_temp)


vio_plot_light <- data.frame(
                            ski_light = ski_light,
                            sab_light = sab_light,
                            tsc_light = tsc_light,
                            mci_light = mci_light, 
                            wad_light = wad_light,
                            spb_light = spb_light)
                            

new_vio_plot_light <- vio_plot_light %>%
  pivot_longer(cols = c(ski_light,
                        sab_light,
                        tsc_light,
                        mci_light, 
                        wad_light,
                        spb_light),
               names_to = "Category",         # New column for former column names
               values_to = "Value")
new_vio_plot_light$Category <- factor(new_vio_plot_light$Category, levels = c("ski_light", "sab_light", "tsc_light", "mci_light", "wad_light", "spb_light"))

new_vio_plot_temp <- vio_plot_temp %>%
  pivot_longer(cols = c(ski_temp, 
                        sab_temp,
                        tsc_temp,
                        mci_temp, 
                        wad_temp,
                        spb_temp),  # Select columns to pivot
               names_to = "Category",         # New column for former column names
               values_to = "Value")

new_vio_plot_temp$Category <- factor(new_vio_plot_temp$Category, levels = c("ski_temp", "sab_temp", "tsc_temp", "mci_temp", "wad_temp", "spb_temp"))

ggplot(new_vio_plot_temp, aes(x = Category, y = Value, color = Category, fill = Category)) +
  geom_violin(trim = F, alpha = 0.6) + 
  scale_color_manual(values = c("wad_temp" = "#243E49",
                                "spb_temp" = "#243E49",
                                "mci_temp" = "#FFC324",
                                "tsc_temp" = "#FFC324",
                                "ski_temp" = "#FFC324",
                                "sab_temp" = "#FFC324")) +
  scale_fill_manual(values = c("wad_temp" = "#243E49",
                               "spb_temp" = "#243E49",
                               "mci_temp" = "#FFC324",
                               "tsc_temp" = "#FFC324",
                               "ski_temp" = "#FFC324",
                               "sab_temp" = "#FFC324")) +
  stat_summary(fun = "mean",
               geom = "point", 
               size = 5,
               colour = "black") + 
  theme_minimal() +
  theme(legend.position = "none")

ggplot(new_vio_plot_light, aes(x = Category, y = Value, color = Category, fill = Category)) +
  geom_violin(trim = F, alpha = 0.6) + 
  scale_color_manual(values = c("wad_light" = "#243E49",
                                "spb_light" = "#243E49",
                                "mci_light" = "#FFC324",
                                "tsc_light" = "#FFC324",
                                "ski_light" = "#FFC324",
                                "sab_light" = "#FFC324")) +
  scale_fill_manual(values = c("wad_light" = "#243E49",
                               "spb_light" = "#243E49",
                               "mci_light" = "#FFC324",
                               "tsc_light" = "#FFC324",
                               "ski_light" = "#FFC324",
                               "sab_light" = "#FFC324")) +
  stat_summary(fun = "mean",
               geom = "point", 
               size = 5,
               colour = "black") + 
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_log10()
