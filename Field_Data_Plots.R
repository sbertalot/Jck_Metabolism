library(ggplot2)
library(dplyr)
library(lubridate)

field_data <- read.csv("Field_Data/2024_Field_Data.csv")

field_data$date <- mdy(field_data$date)


custom_colors <- c(
  "SAB" = "aquamarine",     # Assign color 'red' to site A
  "SKI" = "navy",    # Assign color 'blue' to site B
  "JLB" = "chartreuse2",   # Assign color 'green' to site C
  "WAD" = "darkgoldenrod1",  # Assign color 'purple' to site D
  "MCI" = "skyblue2",  # Assign color 'orange' to site E
  "SPB" = "darkorchid3",
  "TSC" = "black"    # Assign color 'brown' to site F# Assign color 'brown' to site F
)


ggplot(field_data, aes(x = date, y = sdd, color = site, group = site)) +
  geom_line(size = 1.5) + 
  labs(x = "Date", y = "Secchi Disk Depth (m)") +
  scale_color_manual(values = custom_colors) +
  scale_y_reverse() +
  ylim(12, 0) +
  theme_classic()

# Step 1: Define the groups
deep <- c("JLB", "WAD", "SPB")  # The 3 sites for Group 1
shallow <- c("SAB", "SKI", "MCI", "TSC")  # The 4 sites for Group 2

# Step 2: Calculate the mean for each group
data_grouped <- field_data %>%
  mutate(Group = case_when(
    site %in% deep ~ "deep",
    site %in% shallow ~ "shallow"
  )) %>%
  group_by(date, Group) %>%
  summarize(sdd = mean(sdd, na.rm = TRUE))  # Compute the mean value per group

ggplot(data_grouped, aes(x = date, y = sdd, color = Group,  group = Group)) +
  geom_smooth(size = 1.5) + 
  labs(x = "Date", y = "Secchi Disk Depth (m)") +
  scale_y_reverse() +
  ylim(12, 0) +
  theme_classic()


df_subset <- field_data %>% filter(site %in% c('WAD', 'JLB'))

ggplot(df_subset, aes(x = date, y = sdd, color = site, group = site)) +
  geom_line(size = 1.5) + 
  labs(x = "Date", y = "Secchi Disk Depth (m)") +
  scale_y_reverse() +
  ylim(12, 0) +
  theme_classic()
