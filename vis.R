library(tidyverse)
library(patchwork)
library(fst)

pie <- read.csv("outputs/obis_zone_summary_land.csv")

obis_clean <- read_fst("outputs/obis_clean.fst")

zone_colors <- c("deep (benthic)" = "black", "deep (midwater)" = "#1f78b4",
                 "coastal or shallow" = "#a6cee3", "land" = "#33a02c")

# Pie chart for biodiversity observations
pa <- ggplot(pie, aes(x = "", y = pct, fill = zone)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = zone_colors) +
  theme_void() +
  theme(legend.position = "none")

# Pie chart for habitable space by volume
pb <- ggplot(pie, aes(x = "", y = space, fill = zone)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = zone_colors) +
  theme_void() +
  theme(legend.position = "none")

# Combine pie charts
pc <- pa + pb

ggsave("figures/pie.png",pc)
ggsave("figures/pie_a.png",pa)
ggsave("figures/pie_b.png",pb)



######### Observations by depth zone #########
p <- obis_clean |>
  ggplot(aes(x = minimumDepthInMeters, weight = n)) +
  geom_histogram(binwidth = 100) +
  labs(x = "min_depth", y = "Weighted Count") +
  theme_bw() +
  coord_flip() +
  scale_x_reverse() +
  labs(x = "Depth (m)", y = "Biodiversity observations") +
  geom_vline(xintercept = 200, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 1000, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 4000, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 6000, linetype = "dashed", color = "black")
ggsave("figures/depth.png",p)

# filter for deeper depths
obis_meso <- obis_clean %>%
  filter(minimumDepthInMeters > 200 & minimumDepthInMeters <= 1000)
p <- obis_meso |>
  ggplot(aes(x = minimumDepthInMeters, weight = n)) +
  geom_histogram(binwidth = 10) +
  labs(x = "min_depth", y = "Weighted Count") +
  theme_bw() +
  coord_flip() +
  scale_x_reverse() +
  labs(x = "Depth", y = "Biodiversity observations",title="mesopelagic")
ggsave("figures/depth_meso.png",p)

obis_bathy <- obis_clean %>%
  filter(minimumDepthInMeters > 1000 & minimumDepthInMeters <= 4000)
p <- obis_bathy |>
  ggplot(aes(x = minimumDepthInMeters, weight = n)) +
  geom_histogram(binwidth = 100) +
  labs(x = "min_depth", y = "Weighted Count") +
  theme_bw() +
  coord_flip() +
  scale_x_reverse() +
  labs(x = "Depth", y = "Biodiversity observations",title="bathypelagic")
ggsave("figures/depth_bathy.png",p)

obis_abyss <- obis_clean %>%
  filter(minimumDepthInMeters > 4000 & minimumDepthInMeters <= 6000)
p <- obis_abyss |>
  ggplot(aes(x = minimumDepthInMeters, weight = n)) +
  geom_histogram(binwidth = 100) +
  labs(x = "min_depth", y = "Weighted Count") +
  theme_bw() +
  coord_flip() +
  scale_x_reverse() +
  labs(x = "Depth", y = "Biodiversity observations",title="abyssopelagic")
ggsave("figures/depth_abyss.png",p)

obis_hadal <- obis_clean %>%
  filter(minimumDepthInMeters > 6100)
p <- obis_hadal |>
  ggplot(aes(x = minimumDepthInMeters, weight = n)) +
  geom_histogram(binwidth = 100) +
  labs(x = "min_depth", y = "Weighted Count") +
  theme_bw() +
  coord_flip() +
  scale_x_reverse() +
  labs(x = "Depth", y = "Biodiversity observations",title="hadal zone")
ggsave("figures/depth_hadal.png",p)


##### log10 version
obis_clean |>
  ggplot(aes(x = minimumDepthInMeters, weight = log10(n+1)))+
  geom_histogram(binwidth = 200) +
  labs(x = "min_depth", y = "Weighted Count") +
  theme_minimal() +
  coord_flip() +
  scale_x_reverse() +
  labs(x = "Depth", y = "Biodiversity observations") +
  geom_vline(xintercept = 200, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 1000, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 6000, linetype = "dashed", color = "black")

##### nodes
nodes_classification <- read_csv("nodes_obs_classified.csv") |>
  dplyr::select(node_name, n, class)

node_obs <- nodes_classification |>
  group_by(class) |>
  summarise(n = sum(n)) |>
  arrange(-n) |>
  mutate(all = sum(n)) |>
  mutate(perc = n/all*100) |>
  arrange(-perc)

node_obs$class <- factor(
  node_obs$class,
  levels = c("Not classified", "Low income","Lower middle income","Upper middle income","High income"))

ggplot(node_obs, aes(x = class, y = perc)) + geom_col() + coord_flip() +
  ylab("Biodiversity observations\nmanaged (%)") +
  xlab("") +
  ylim(c(0,100)) +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 45, hjust = 1))



