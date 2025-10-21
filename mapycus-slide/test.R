
# ================================
# 1️⃣  Load required packages
# ================================
library(terra)       # for raster data
library(geodata)     # for Copernicus Land Cover
library(osmdata)     # for OpenStreetMap roads
library(sf)          # for vector data
library(ggplot2)     # for plotting
library(tidyterra)   # for geom_spatraster()

# ================================
# 2️⃣  Download Copernicus Land Cover data
# ================================
# This downloads 100m resolution land cover tiles (global)
# and crops to Melbourne automatically.


# === Paths to your downloaded tiles ===
tile1 <- "C:/Users/thanh/Downloads/ESA_WorldCover_10m_2020_v100_60deg_macrotile_S90E120/ESA_WorldCover_10m_2020_v100_S36E144_Map.tif"
tile2 <- "C:/Users/thanh/Downloads/ESA_WorldCover_10m_2020_v100_60deg_macrotile_S90E120/ESA_WorldCover_10m_2020_v100_S39E144_Map.tif"

# === Merge them ===
land_vic <- merge(rast(tile1), rast(tile2))
# Crop to Melbourne
melbourne_bbox <- ext(144.5, 145.3, -38.1, -37.6)
land_melb <- crop(land_vic, melbourne_bbox)
land_melb_lowres <- aggregate(land_melb, fact = 10, fun = "modal")
# ================================
# 3️⃣  Get road network from OpenStreetMap
# ================================
set_overpass_url("https://overpass.kumi.systems/api/interpreter")
melb_highways <- opq(bbox = c(144.5, -38.1, 145.3, -37.6)) %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "trunk", "primary", "secondary")) %>%
  osmdata_sf()

roads_sf <- melb_highways$osm_lines


# 2nd approach

roads_sf_full <- st_read("australia-free-shp/gis_osm_roads_free_1.shp")
vic_roads_full <- st_crop(roads_sf, st_bbox(c(xmin = 144.5, xmax = 145.3, ymin = -38.1, ymax = -37.6)))

# ================================
# 4️⃣  Harmonize CRS
# ================================
roads_sf <- st_transform(roads_sf, crs(land_melb_lowres))
vic_roads_full <- st_transform(vic_roads_full, crs(land_melb))

# ================================
# 5️⃣  Plot using ggplot2 + tidyterra
# ================================
land_melb_factor <- as.factor(land_melb_lowres)
land_melb_full <- as.factor(land_melb)

levels(land_melb_factor) <- data.frame(
  value = c(10,20,30,40,50,60,80,90),
  class = c("Tree cover","Shrubland","Grassland","Cropland",
            "Built-up","Bare/Sparse","Water","Wetland")
)

levels(land_melb_full) <- data.frame(
  value = c(10,20,30,40,50,60,80,90),
  class = c("Tree cover","Shrubland","Grassland","Cropland",
            "Built-up","Bare/Sparse","Water","Wetland")
)




ggplot() +
  geom_spatraster(data = land_melb_factor) +
  geom_sf(
    data = roads_sf,
    aes(color = highway),
    size = 0.4,
    alpha = 0.9
  ) +
    scale_color_manual(
    values = c(
      motorway  = "#111111",
      trunk     = "#333333",
      primary   = "#555555",
      secondary = "#777777"
    ),
    name = "Road type",
    labels = c(
      motorway = "Motorway",
      trunk    = "Trunk road",
      primary  = "Primary road",
      secondary= "Secondary road"
    )
  ) +
    scale_fill_manual(
    values = c(
      "Tree cover" = "#006400",
      "Shrubland"  = "#BDB76B",
      "Grassland"  = "#FFF68F",
      "Cropland"   = "#FFD37F",
      "Built-up"   = "#FF0000",
      "Bare/Sparse"= "#D2B48C",
      "Water"      = "#0000FF",
      "Wetland"    = "#40E0D0"
    )) +
  labs(
    title = "Melbourne Land Cover and Road Network",
    subtitle = "ESA WorldCover 2020 (Tiles S36E144 + S39E144) + OSM Roads",
    caption = "Data: ESA Copernicus & OpenStreetMap",
    fill = "Land cover class"
  ) +  coord_sf(
    xlim = c(144.5, 145.3),
    ylim = c(-38.1, -37.6),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    legend.position = "right"
  )
land_poly <- as.polygons(land_melb_factor, dissolve = TRUE)
land_sf <- st_as_sf(land_poly)
land_sf <- st_simplify(land_sf, dTolerance = 50)

library(mapycusmaximus)
melbourne <- vic %>% filter(LGA_NAME == "MELBOURNE")

monash_coords <- c(145.13174059613814, -37.91381150646678)

monash_sf <- st_sf(
  name = "Monash",
  geometry = st_sfc(st_point(monash_coords)),
  crs = st_crs(melbourne)  # match CRS (GDA2020)
)


land_fish <- sf_fisheye(land_sf, center = monash_coords, zoom = 10, r_in = 0.25, r_out = 0.4)

land_fish_vec <- vect(land_fish) 
template <- land_melb
land_rast_fish <- rasterize(
  land_fish_vec,
  land_melb,           # your template raster
  field = "class"      # the attribute column to burn in
)
road_sf_fish <- sf_fisheye(roads_sf, center = monash_coords, zoom = 10, r_in = 0.25, r_out = 0.4)
ggplot() +
  geom_spatraster(data = land_rast_fish) +
    geom_sf(
    data = road_sf_fish,
    aes(color = highway),
    size = 0.4,
    alpha = 0.9
  ) +
    scale_color_manual(
    values = c(
      motorway  = "#111111",
      trunk     = "#333333",
      primary   = "#555555",
      secondary = "#777777"
    ),
    name = "Road type",
    labels = c(
      motorway = "Motorway",
      trunk    = "Trunk road",
      primary  = "Primary road",
      secondary= "Secondary road"
    )
  ) +
    scale_fill_manual(
    values = c(
      "Tree cover" = "#006400",
      "Shrubland"  = "#BDB76B",
      "Grassland"  = "#FFF68F",
      "Cropland"   = "#FFD37F",
      "Built-up"   = "#FF0000",
      "Bare/Sparse"= "#D2B48C",
      "Water"      = "#0000FF",
      "Wetland"    = "#40E0D0"
    )) +
  labs(
    title = "Melbourne Land Cover and Road Network",
    subtitle = "ESA WorldCover 2020 (Tiles S36E144 + S39E144) + OSM Roads",
    caption = "Data: ESA Copernicus & OpenStreetMap",
    fill = "Land cover class"
  ) +
  coord_sf(
    xlim = c(144.5, 145.3),
    ylim = c(-38.1, -37.6),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    legend.position = "right"
  )
