library(ggplot2)
library(sf)
library(dplyr)
library(ggrepel)

library(rnaturalearth)
library(rnaturalearthdata)
library(maps)
library(lwgeom)
library(osmdata)
library(stringr)

world <- ne_countries(scale = "medium", returnclass = "sf")

lon_min_SK <- 5 # xmin
lon_max_SK <- 12 # xmax
lat_min_SK <- 56 # ymin
lat_max_SK <- 60 # ymax

coords_SK <- matrix(c(lon_min_SK,lon_max_SK,lat_min_SK,lat_max_SK), 
                    byrow = TRUE, nrow = 2, ncol = 2, 
                    dimnames = list(c('x','y'),
                                    c('min','max'))) 
coords_SK

lakes110 <- rnaturalearth::ne_download(scale = 10, type = "lakes",
                                       category = "physical") 

coastline10 <- rnaturalearth::ne_coastline(10)



### get sea & land as polygons
# 1. Get coastline from OpenStreetMap as an SF object using Osmdata
coast_SK <- coords_SK %>%
  opq() %>%
  add_osm_feature(key = "natural", value = "coastline") %>%
  osmdata_sf()

# Maps rendered with coast_SK$osm_lines miss many small islands and islets.
# Maps rendered with coast_SK$osm_polygons misses the main land.
# Convert coast_SK$osm_polygons to lines to capture all islands and islets.
coast2_SK <- osm_poly2line (coast_SK)

# Get overall rectangle for bbox
bb_rect_SK <- data.frame(
  lat = c(lat_min_SK, lat_max_SK),
  lon = c(lon_min_SK, lon_max_SK)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

# Split the overall rectangle for bbox via coastline (coastline10$geometry)
bb_rect_split_SK <- bb_rect_SK %>% 
  st_split(coastline10$geometry) %>%
  st_collection_extract("POLYGON")

# Extract the split rectangle parts
sea_SK  <- bb_rect_split_SK[1]
land_SK <- bb_rect_split_SK[2]

# Read in spreadsheet of locations
Map_coordinates_SK <- readxl::read_excel("Data/vasterhav_skagerrakspärren_ships.xlsx", sheet = "Sheet1", 
                                               range = "A1:E17",
                                        col_names = TRUE,
                                        col_types = c("text", "text", "numeric", 
                                                             "numeric","text")) %>%
  dplyr::rename(Label = `Label - in italics`, Type = type, Ship = ship) %>%
  dplyr::mutate(Type = case_when((Type == "ship - rhombus") ~ "Ship",
                                 TRUE ~ "Dot")) %>%
  dplyr::mutate(PointType = case_when(Type == "Ship" ~ 18,
                                      TRUE ~ 20)) %>%
  dplyr::mutate(LabelX = case_when(Ship == "Viros av Göteborg, GG 28, sept 1942" ~ "Viros av Göteborg\nGG 28\nsept 1942",
                                   Ship == "Nippon av Öckerö, jul 1943" ~ "Nippon av Öckerö\njul 1943",
                                   Ship == "Neptun av Hönö, sept 1942" ~ "Neptun av Hönö\nsept 1942",
                                   Ship == "Inez av Öckerö, GG 177, april 1940" ~ "Inez av Öckerö\nGG 177\napril 1940",
                                   Ship == "Gotland, aug 1944" ~ "Gotland\naug 1944",
                                   Ship == "Glimmaren av Gravarne, april 1944" ~ "Glimmaren av Gravarne\napril 1944",
                                   Ship == "Hermon och Vestkusten, aug 1943" ~ "Hermon och Vestkustenau\naug 1943",
                                   Ship == "Gunnaren av Fisketången, aug 1944" ~ "Gunnaren av Fisketången\naug 1944",
                                   TRUE ~ Ship)) %>%
  # Add a new column NudgeX which will be used to nudge text labels to the left or right
  dplyr::mutate(NudgeX = case_when(Label == "Cyrene" ~ -.61,  # moved left
                                   Label == "Dalarö" ~ .61,
                                   Label == "Hermon och Vestkusten" ~ .3, # moved right
                                   Label == "Marina" ~ .6,
                                   Label == "Dagny" ~ -.8, # moved left
                                   Label == "Nepton" ~ -.3,
                                   #Label == "Nippon" ~ .75,
                                   #Label == "Hugin" ~ .3,
                                   TRUE ~ 0)) %>%
  # Add a new column NudgeY which will be used to nudge text labels up or down
  dplyr::mutate(NudgeY = case_when(Label %in% c("Inez") ~ .05, # Moved up
                                   Label %in% c("Glimmaren") ~ .07,
                                   Label %in% c("Gunnaren") ~ .07,
                                   Label %in% c("Gotland") ~ -.06, # moved down
                                   Label %in% c("Nippon") ~ .06, # moved up
                                   Label %in% c("Hermon och Vestkusten") ~ -.055, # moved down
                                   Label %in% c("Hugin","Neptun") ~ -.055, # moved down
                                   Label %in% c("Viros") ~ -.09, # moved down
                                   TRUE ~ -0)) # Moved down

Points_SK <- Map_coordinates_SK %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326, agr = "constant")

Labels_SK <- dplyr::filter(Map_coordinates_SK, !Type == "Dot") %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326, agr = "constant")

scale_factor = 10

PlotcoastLabels_SK <- ggplot(data = sea_SK) +
  geom_sf(fill = "lightblue",
          color = "steelblue") +
  geom_sf(data=lakes110, fill = "lightblue") +
  geom_sf(data = Points_SK, size = 2, shape = Points_SK$PointType, fill = "darkred") +
    geom_sf_text(data = Labels_SK, 
                                aes(label = LabelX, lineheight = .85), size = 4.3,
                 fontface = "italic",
                 nudge_x = Labels_SK$NudgeX,
                 nudge_y = Labels_SK$NudgeY) +  
    coord_sf(
    xlim = c(lon_min_SK+1, lon_max_SK-1), ylim = c(lat_min_SK+1, lat_max_SK-1.5), 
    expand = TRUE) +
  theme(axis.line = element_line(),
        #axis.ticks = element_blank(),  # hides the Lat, Long tick lines
        #axis.text.x = element_blank(), # hides the Longitude lines
        #axis.text.y = element_blank(), # hides the Latitude lines
        axis.title = element_blank(),  # hides the X and Y titles
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth =.3),
        panel.background = element_rect(fill = "antiquewhite"))

print(PlotcoastLabels_SK)

# Save as PNG file
ggsave("GIFs/Skagerrakspärren_1_%03d.png",
       plot = last_plot(), 
       device = "png",
       scale = scale_factor,
       units="mm",# width=800, height=900, 
       limitsize = FALSE,
       dpi=300)

####
# options(ggrepel.max.overlaps = Inf)
# 
# PlotcoastLabelsRepel_SK <- ggplot(data = sea_SK) +
#   geom_sf(fill = "lightblue",
#           color = "steelblue") +
#   geom_sf(data=lakes110, fill = "lightblue") +
#   geom_sf(data = Points_SK, size = 2, shape = Points_SK$PointType, fill = "darkred") +
#   geom_text_repel(data = Map_coordinates_SK, aes(label = LabelX, x = Long, y = Lat),
#                   size = 4.5,
#                   fontface = "italic"#,
# #                  nudge_x = Map_coordinates_TJ$NudgeX,
# #                  nudge_y = Map_coordinates_TJ$NudgeY
#   ) +
#   coord_sf(
#     xlim = c(lon_min_SK+1, lon_max_SK-1), ylim = c(lat_min_SK+1, lat_max_SK-1.5),
#     expand = TRUE) +
#   theme(axis.line = element_line(),
#         #axis.ticks = element_blank(),  # hides the Lat, Long tick lines
#         #axis.text.x = element_blank(), # hides the Longitude lines
#         #axis.text.y = element_blank(), # hides the Latitude lines
#         axis.title = element_blank(),  # hides the X and Y titles
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_rect(colour = "black", fill=NA, linewidth =.3),
#         panel.background = element_rect(fill = "antiquewhite"))
# 
# print(PlotcoastLabelsRepel_SK)
# 
# # Save as PNG file
# ggsave("GIFs/Skagerrakspärren_2_%03d.png",
#        plot = last_plot(),
#        device = "png",
#        scale = scale_factor,
#        units="mm",# width=800, height=900,
#        limitsize = FALSE,
#        dpi=300)
# 
