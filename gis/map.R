library(ggplot2)
library(ggpattern)
library(basemaps)
library(sf)
library(terra)

## Read files
wa <- st_read("../../../GIS/cb_2018_us_region_500k/cb_2018_us_region_500k.shp")
wa <- wa[wa$NAME == "West",]
wa <- st_transform(wa, 32610)

bc <- st_read("../../../GIS/BC Terrestrial boundaries/NRC_POLITICAL_BOUNDARIES_1M_SP/POL_BND_1M_polygon.shp")

delta <- st_read("gis/delta.gpkg")

border <- st_read("../../../GIS/Natural Earth datasets/Land boundaries/ne_10m_admin_0_boundary_lines_land.shp")
border <- border[border$adm0_a3_l == "CAN",]
border <- st_transform(border, crs = 32610)

mud <- st_read("gis/fraser_sandmud.gpkg")
mud <- st_transform(mud, crs = 32610)

catchment <- st_read("gis/survey_catchment.gpkg")
catchment <- st_transform(catchment, crs = 32610)

stations <- read.csv("gis/stations.csv")
stations <- st_as_sf(stations, coords = c("longitude", "latitude"), crs = 4326)
stations <- st_transform(stations, crs = 32610)
stations <- stations[!(stations$station_n %in% c(0, 6)), ] # Remove BP & CPSM stations

fraser <- st_read("gis/fraser_label.shp")

# Following the incredibly helpful steps here - https://medium.com/@tobias.stalder.geo/plot-rgb-satellite-imagery-in-true-color-with-ggplot2-in-r-10bdb0e4dd1f
#b_img <- raster::stack("gis/b2.tif")
#b_img <- raster::projectRaster(b_img, crs = "+init=EPSG:32610")
b_img <- terra::rast("gis/b.tif")
b_img <- terra::project(b_img, y = terra::crs(catchment)) # Reproject to the same projection as 'catchment'
b <- as.data.frame(b_img, xy = TRUE)
b <- b[(b$b_1 != 0 & !is.na(b$b_1)), 1:5]
b_subset <- b[1:10000,]

# Test with smaller subset of data
ggplot(data = b_subset, aes(x = x, y = y)) +
  geom_raster(fill = rgb(r = b_subset$b_1, 
                         g = b_subset$b_2, 
                         b = b_subset$b_3, 
                         maxColorValue = 255),
              show.legend = FALSE) + 
  scale_fill_identity() + 
  theme_minimal()


## Make map

bbox <- st_bbox(stations)
bbox[[1]] <- bbox[[1]] - 2000 # 2 km buffer to west
bbox[[3]] <- bbox[[3]] + 4500 # 4.5 km buffer to east
bbox[[2]] <- bbox[[2]] - 2000 # 2 km buffer to south
bbox[[4]] <- bbox[[4]] + 2000 # 2 km buffer to north
#bbox[c(1,2)] <- bbox[c(1,2)] - 5000 # add 5 km buffer
#bbox[c(3,4)] <- bbox[c(3,4)] + 5000

ggplot() +
  geom_raster(data = b,
              aes(x = x, y = y),
              fill = rgb(r = b$b_1,
                         g = b$b_2,
                         b = b$b_3,
                         maxColorValue = 255),
              show.legend = FALSE) +
  scale_fill_identity() +
  geom_sf(data = catchment) +
  # geom_sf(data = wa,
  #         lwd = 0,
  #         fill = "#CBCACA") +
  # geom_sf(data = delta,
  #         lwd = 0.1,
  #         color = "#C3C3C3") +
  #geom_sf(data = mud) +
  geom_sf_label(data = stations, aes(label = station_n)) +
  coord_sf(xlim = bbox[c(1,3)],
           ylim = bbox[c(2,4)],
           expand = T) +
  theme_minimal()


# Inset labels 
ins_labels <- data.frame(x = c(0.35, 0.4, 0.53, 0.12, 0.14, 0.65, 0.17, 0.18),
                         y = c(0.8, 0.55, 0.35, 0.67, 0.23, 0.2, 0.14, 0.1), 
                         label = c("Vancouver", "Richmond", "Delta", "Sturgeon\nBanks", "Roberts\nBank", "Boundary Bay", "Canada", "USA"),
                         size = c(1.2, 1.2, 1.2, 2, 2, 1.1, 1, 1),
                         color = c("A", "A", "A", "B", "B", "B", "B", "B"))

# Inset map
bbox2 <- st_bbox(delta)
bbox2[[1]] <- bbox2[[1]] - 9000 # km buffer to west
bbox2[[3]] <- bbox2[[3]] + 10000 # km buffer to east
bbox2[[2]] <- bbox2[[2]] - 2000 # km buffer to south
bbox2[[4]] <- bbox2[[4]] + 20000 # km buffer to north

i <- ggplot() +
  # geom_sf_pattern(data = mud,
  #                 pattern = "stripe",
  #                 fill = NA,
  #                 linewidth = 0.1,
  #                 pattern_fill = NA,
  #                 pattern_color = "black",
  #                 pattern_density = 0.01,
  #                 pattern_spacing = 0.05,
  #                 pattern_alpha = 0.5,
  #                 pattern_angle = 45,
  #                 pattern_res = 720) +
  geom_sf(data = mud,
          color = NA,
          fill = "#e6a800", 
          alpha = 0.3) +
  geom_sf(data = wa,
          lwd = 0,
          fill = "#575656") + #"#CBCACA") +
  geom_sf(data = bc,
          lwd = 0,
          fill = "#575656") + # "#C3C3C3") +
  geom_sf(data = border,
          color = "#adadad", #"#878585",
          size = 0.1) +
  geom_rect(aes(xmin = bbox[[1]],
            xmax = bbox[[3]],
            ymin = bbox[[2]],
            ymax = bbox[[4]]),
            color = "black",
            fill = NA,
            linewidth = 0.3) +
  ggpp::geom_text_npc(data = ins_labels, 
                      aes(npcx = x, 
                          npcy = y, 
                          label= label, 
                          size = size, 
                          color = color),
                      family = "Avenir",
                      hjust = "middle") +
  scale_size(range = c(3, 4)) +
  scale_color_manual(values = c("#f5f5f5", "black")) +
  geomtextpath::geom_textsf(data = fraser,
                            label = "Fraser River",
                            color = "white",
                            linecolour = NA) +
  coord_sf(xlim = bbox2[c(1,3)],
           ylim = bbox2[c(2,4)],
           expand = T) +
  theme_void()

  