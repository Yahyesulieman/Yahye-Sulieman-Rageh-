# ==============================
# 📦 Libraries
# ==============================
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(ggrepel)

# ==============================
# 📂 Load Data
# ==============================
Ethio_1 <- st_read("eth_admin_boundaries.shp", layer = "eth_admin1")
Ethio_2 <- st_read("eth_admin_boundaries.shp", layer = "eth_admin2")

# Rename column for consistency
Ethio_1 <- Ethio_1 %>%
  rename(adm1_name = NAME_1)

# ==============================
# 🎯 Define 13 Regions
# ==============================
states_ethiopia <- c(
  "Addis Ababa", "Afar", "Amhara", "Benishangul Gumuz",
  "Dire Dawa", "Gambela", "Harari", "Oromia",
  "Sidama", "SNNP", "Somali", "South West Ethiopia", "Tigray"
)

eth_states <- Ethio_1 %>%
  filter(adm1_name %in% states_ethiopia)

# ==============================
# 🗺️ 1. BEAUTIFUL ETHIOPIA MAP
# ==============================
ggplot(data = eth_states) +
  geom_sf(aes(fill = adm1_name), color = "white", size = 0.4) +
  scale_fill_viridis_d(option = "turbo") +
  
  # Labels
  geom_text_repel(
    data = eth_states,
    aes(
      x = st_coordinates(st_centroid(geometry))[,1],
      y = st_coordinates(st_centroid(geometry))[,2],
      label = adm1_name
    ),
    size = 3,
    fontface = "bold",
    color = "black",
    max.overlaps = 20
  ) +
  
  labs(
    title = "Administrative Regions of Ethiopia",
    subtitle = "Level 1 Regions (Admin 1)",
    caption = "Source: Shapefile Data",
    fill = "Regions"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "right",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

# ==============================
# 🟢 2. SOMALI REGION (LEVEL 2)
# ==============================
som_eh <- Ethio_2 %>% 
  filter(adm2_name %in% c(
    "Afder", "Daawa", "Doolo", "Erer", "Fafan",
    "Jarar", "Korahe", "Liban", "Nogob", "Shabelle", "Siti"
  ))

ggplot(data = som_eh) +
  geom_sf(aes(fill = adm2_name), color = "white", size = 0.5) +
  scale_fill_viridis_d(option = "plasma") +
  
  geom_text_repel(
    aes(
      x = st_coordinates(st_centroid(geometry))[,1],
      y = st_coordinates(st_centroid(geometry))[,2],
      label = adm2_name
    ),
    size = 3,
    fontface = "bold",
    color = "black"
  ) +
  
  labs(
    title = "Somali Region of Ethiopia",
    subtitle = "Administrative Zones (Admin Level 2)",
    fill = "Zones"
  ) +
  
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "right"
  )

# ==============================
# 🔵 3. MINIMAL MODERN MAP STYLE
# ==============================
ggplot(data = eth_states) +
  geom_sf(fill = "#2E86AB", color = "white", size = 0.3) +
  
  geom_sf_text(
    aes(label = adm1_name),
    size = 3,
    color = "white",
    fontface = "bold"
  ) +
  
  labs(
    title = "Ethiopia Regions (Clean Modern Style)"
  ) +
  
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  )

