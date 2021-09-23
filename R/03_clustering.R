dt_height <- dt_clean %>%
  group_by(height) %>%
  summarise(
    across(
      c(humidity, humid_temp, reflected, incident),
      mean
      )
    )

set.seed(42)
clust <- dt_height %>%
  select(-height, -reflected, -incident) %>%
  kmeans(centers = 2, nstart = 10)

dt_height %>%
  mutate(cluster = factor(clust$cluster)) %>%
  ggplot(aes(x = height, fill = cluster, group = cluster)) +
  geom_density(alpha = 0.3)

dt_height %>%
  mutate(cluster = factor(clust$cluster)) %>%
  ggplot(aes(x = incident, fill = cluster, group = cluster)) +
  geom_density(alpha = 0.3)

dt_height %>%
  mutate(cluster = factor(clust$cluster)) %>%
  ggplot(aes(x = humid_temp, fill = cluster, group = cluster)) +
  geom_density(alpha = 0.3)

dt_height %>%
  mutate(cluster = factor(clust$cluster)) %>%
  ggplot(aes(x = humidity, fill = cluster, group = cluster)) +
  geom_density(alpha = 0.3)

dt_height %>%
  mutate(cluster = factor(clust$cluster)) %>%
  ggplot(aes(x = humid_temp, y = humidity)) +
  geom_point(aes(color = cluster))

threshold <- (max(dt_clean$height, na.rm = TRUE) - min(dt_clean$height, na.rm = TRUE))/3

dt_bin <- dt_clean %>%
  mutate(
    height_bin = case_when(
      height <= threshold ~ "low",
      height <= 2*threshold ~ "medium",
      TRUE ~ "high"
    )
  )

dt_bin %>%
  mutate(
    across(
      height_bin,
      ~factor(.x, levels = c("low", "medium", "high"), ordered = TRUE)
      )
  ) %>%
  filter(reflected > 0) %>%
  ggplot(aes(x = factor(height_bin), y = reflected)) +
  geom_boxplot()



