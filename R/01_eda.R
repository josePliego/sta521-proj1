######################################.
# Title: EDA
# Author: Jose Pliego
# Date: 2021-09-05
######################################.

if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse", "skimr", "lubridate", "patchwork", "DataExplorer")

# 1. Data -----------------------------------------------------------------

dt_log <- read_csv("data/sonoma-data-log.csv")
dt_net <- read_csv("data/sonoma-data-net.csv")

skim(dt_log)
skim(dt_net)

# Epoch

dt_log %>%
  ggplot(aes(x = epoch)) +
  geom_histogram(binwidth = 300)

dt_net %>%
  ggplot(aes(x = epoch)) +
  geom_histogram(binwidth = 300)

# Parent

dt_log %>%
  ggplot(aes(x = parent)) +
  geom_histogram()

dt_net %>%
  ggplot(aes(x = parent)) +
  geom_histogram()

dt_log %>%
  ggplot(aes(x = voltage)) +
  geom_histogram(binwidth = 0.1)

dt_net %>%
  ggplot(aes(x = voltage)) +
  geom_histogram(binwidth = 10)

# Voltage is off, we have to divide the values in dt_net by 100

# Humidity

# Two data points have humidity of -5145 and -9375
dt_log %>%
  filter(humidity > -5) %>%
  ggplot(aes(x = humidity)) +
  geom_histogram(binwidth = 1)
# Filter to be between 0 and 100

dt_net %>%
  ggplot(aes(x = humidity)) +
  geom_histogram(binwidth = 1)
# Filter to be between 0 and 100

# Temperature

# One observation is 604
dt_log %>%
  filter(humid_temp < 600) %>%
  ggplot(aes(x = humid_temp)) +
  geom_histogram(binwidth = 1)
# Filter to greater than zero

dt_net %>%
  ggplot(aes(x = humid_temp)) +
  geom_histogram(binwidth = 1)
# Filter to less than 40

# Incident PAR (hamatop)

# One value is 22592200
dt_log %>%
  # filter(hamatop < 200000) %>%
  filter(hamatop < 200000, hamatop > 0) %>%
  ggplot(aes(x = hamatop)) +
  geom_histogram(binwidth = 1000)

dt_net %>%
  # filter(hamatop < 200000) %>%
  filter(hamatop > 0) %>%
  ggplot(aes(x = hamatop)) +
  geom_histogram(binwidth = 1000)

dt_log %>%
  mutate(
    humidity = if_else(humidity > 100, 100, humidity),
    humidity = if_else(humidity < 0, 0, humidity)
  ) %>%
  ggplot(aes(x = humidity)) +
  geom_histogram(binwidth = 1)

dt_net %>%
  ggplot(aes(x = humidity)) +
  geom_histogram(binwidth = 1)

dt_net %>%
  # filter(humidity > -5) %>%
  ggplot(aes(x = humidity, y = ..count../sum(..count..))) +
  geom_histogram()

dt_log %>%
  filter(humid_temp < 122, humid_temp > -30) %>%
  ggplot(aes(x = humid_temp, y = ..count../sum(..count..))) +
  geom_histogram()

dt_net %>%
  filter(humid_temp < 122, humid_temp > -30) %>%
  ggplot(aes(x = humid_temp, y = ..count../sum(..count..))) +
  geom_histogram()

dt_net %>%
  ggplot(aes(x = factor(nodeid), y = humid_temp)) +
  geom_violin()

dt_log %>%
  ggplot(aes(x = factor(nodeid), y = humid_temp)) +
  geom_violin()

dt_net_temp_node <- dt_net %>%
  mutate(across(nodeid, factor)) %>%
  group_by(nodeid) %>%
  summarise(
    across(
      humid_temp,
      list("mean" = mean, "median" = median, "min" = min, "max" = max)
      )
    )

dt_net_temp_node %>%
  plot_missing()

dt_net_temp_node %>%
  filter(is.na(humid_temp_mean))

dt_net %>%
  filter(nodeid == 122) %>%
  plot_missing()

# Node 122 made no readings

plt_temp_mean <- dt_net_temp_node %>%
  ggplot(aes(x = nodeid, y = humid_temp_mean)) +
  geom_point()

plt_temp_median <- dt_net_temp_node %>%
  ggplot(aes(x = nodeid, y = humid_temp_median)) +
  geom_point()

plt_temp_mean/plt_temp_median

# Nodes that seem to be having problems: 78, 122, 123, 141, 145

plt_temp_min <- dt_net_temp_node %>%
  ggplot(aes(x = nodeid, y = humid_temp_min)) +
  geom_point()

plt_temp_max <- dt_net_temp_node %>%
  ggplot(aes(x = nodeid, y = humid_temp_max)) +
  geom_point()

plt_temp_min/plt_temp_max

# Nodes that seem to be having problems: 3, 78, 122, 123, 141, 145, 198
