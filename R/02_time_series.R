library(tidyverse)
library(lubridate)
dt <- read_rds("data/dt_clean.rds")

dt %>%
  group_by(height) %>%
  summarise(median = mean(hamatop)) %>%
  ggplot(aes(x = factor(height), y = median)) +
  geom_point()

dt %>%
  group_by(day = date(datetime), height) %>%
  summarise(across(c("hamabot", "hamatop"), mean), .groups = "drop") %>%
  ggplot(aes(x = day, y = hamatop, color = height, group = height)) +
  geom_line()

dt %>%
  group_by(day = date(datetime), height) %>%
  summarise(across(c("humidity"), mean), .groups = "drop") %>%
  ggplot(aes(x = day, y = humidity, color = height, group = height)) +
  geom_line()

dt %>%
  group_by(day = date(datetime), height) %>%
  summarise(across(c("humid_temp"), mean), .groups = "drop") %>%
  ggplot(aes(x = day, y = humid_temp, color = height, group = height)) +
  geom_line()

dt %>%
  group_by(time = format(datetime, format = "%H:%M:%S"), height) %>%
  summarise(across(c("humidity"), mean), .groups = "drop") %>%
  ggplot(aes(x = time, y = humidity, color = height, group = height)) +
  geom_line()

dt %>%
  group_by(time = format(datetime, format = "%H:%M:%S"), height) %>%
  summarise(across(c("humid_temp"), median), .groups = "drop") %>%
  ggplot(aes(x = time, y = humid_temp, color = height, group = height)) +
  geom_line()

dt %>%
  ggplot(aes(x = humidity, color = height, group = height)) +
  geom_density(alpha = 0.3)

dt %>%
  ggplot(aes(x = log(hamatop + 0.01), color = height, group = height)) +
  geom_density(alpha = 0.3)

dt %>%
  group_by(datetime) %>%
  mutate(temp_mean = mean(humid_temp)) %>%
  ungroup() %>%
  ggplot(aes(y = temp_mean - humid_temp, x = factor(height))) +
  geom_boxplot()

dt %>%
  filter(hamatop > 0, hamabot > 0) %>%
  group_by(date(datetime), height) %>%
  summarise(across(starts_with("hama"), mean), .groups = "drop") %>%
  ggplot(aes(x = log(hamabot), y = log(hamatop), color = height)) +
  geom_point() +
  geom_hline(aes(yintercept = mean(log(hamatop))), lty = 2) +
  geom_vline(aes(xintercept = mean(log(hamabot))), lty = 2) +
  scale_color_gradient(
    low = "orange",
    high = "blue"
  )

dt %>%
  filter(date(datetime) == "2004-05-01") %>%
  group_by(datetime, height) %>%
  summarise(across(humid_temp, mean), .groups = "drop") %>%
  ggplot(aes(x = datetime, y = humid_temp, color = height, group = height)) +
  geom_line() +
  scale_color_gradient(
    low = "orange",
    high = "blue"
  )

