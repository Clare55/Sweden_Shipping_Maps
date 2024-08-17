library(lwgeom)
library(osmdata)
library(sf)
library(dplyr)
library(ggplot2)
library(readxl)
library(ggrepel)

lon_min_TJ_F <- 11.39 # xmin
lon_max_TJ_F <- 11.9 # xmax
lat_min_TJ_F <- 57.84 # ymin
lat_max_TJ_F <- 58.14 # ymax

coords_TJ_F <- matrix(c(lon_min_TJ_F,lon_max_TJ_F,lat_min_TJ_F,lat_max_TJ_F), byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(c('x','y'),c('min','max'))) 
coords_TJ_F


### get sea & land as polygons
# 1. Get coastline from OpenStreetMap as an SF object using Osmdata
coast_TJ_F <- coords_TJ_F %>%
  opq() %>%
  add_osm_feature(key = "natural", value = "coastline") %>%
  osmdata_sf()

# Maps rendered with coast_TJ_F$osm_lines miss many small islands and islets.
# Maps rendered with coast_TJ_F$osm_polygons misses the main land.
# Convert coast_TJ_F$osm_polygons to lines to capture all islands and islets.
coast_TJ_F2 <- osm_poly2line (coast_TJ_F)

# 2. Get overall rectangle for bbox
bb_rect_TJ_F <- data.frame(
  lat = c(lat_min_TJ_F, lat_max_TJ_F),
  lon = c(lon_min_TJ_F, lon_max_TJ_F)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

# 3. Split the overall rectangle for bbox via coastline (coast2$osm_lines)
bb_rect_split_TJ_F <- bb_rect_TJ_F %>% 
  st_split(coast_TJ_F2$osm_lines) %>% 
  st_collection_extract("POLYGON")

# 4. Extract the split rectangle parts
sea_TJ_F  <- bb_rect_split_TJ_F[1]
land_TJ_F <- bb_rect_split_TJ_F[2]

### Create Plots 
# Creates a plot with the sea in light blue, outlined in steelblue. 
# Land appears as antique white because of the panel.background setting.
Tjörn_befästningar_1 <- ggplot() +
  geom_sf(
    data = sea_TJ_F,
    fill = "lightblue",
    color = "steelblue"
  ) + 
  coord_sf(xlim = c(lon_min_TJ_F, lon_max_TJ_F), 
           ylim = c(lat_min_TJ_F, lat_max_TJ_F),
           expand = FALSE) +  
  theme(axis.line = element_line(),
        axis.ticks = element_blank(),  # hides the Lat, Long tick lines
        axis.text.x = element_blank(), # hides the Longitude lines
        axis.text.y = element_blank(), # hides the Latitde lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=.3),
        panel.background = element_rect(fill = "antiquewhite"))
print(Tjörn_befästningar_1)

scale_factor = 10

## Save as PNG file
ggsave("Gifs/Tjörn_befästningar_1%03d.png",
       plot = last_plot(), 
       device = "png",
       scale = scale_factor,
       units="mm", 
       limitsize = FALSE,
       dpi=300)


### Add points
Map_coordinates_TJ_F <- read_excel("Data/Coordinates_Tjörn_befästningar.xlsx", 
                                   sheet = "Sheet1", 
                                   range = "B1:F55") %>% 
                        # Filter out any nulls
                        na.omit() # Filter out any nulls


Points_TJ_F <- dplyr::filter(Map_coordinates_TJ_F, !mark == "region (just label, no dot)") %>%
  sf::st_as_sf(coords = c("Long", "Lat"),
               crs = 4326, agr = "constant")
Labels_TJ_F <- Map_coordinates_TJ_F %>%
  sf::st_as_sf(coords = c("Long", "Lat"),
               crs = 4326, agr = "constant")


#####
options(ggrepel.max.overlaps = Inf)

# Creates a plot with the sea in light blue, outlined in steelblue. 
# Adds points from the Points_TJ_F data frame (filters the Map_coordinates_TJ_F data frame 
# Adds Labels from the Map_coordinates_TJ_F data frame
# Use ggrepel to force the labels to not overlap

Tjörn_befästningar_2 <- ggplot() +
  geom_sf(
    data = sea_TJ_F,
    fill = "lightblue",
    color = "steelblue"
  ) +
  geom_sf(data = Points_TJ_F, size = 1, shape = 21, fill = "darkred") +
  geom_text_repel(data = Map_coordinates_TJ_F, aes(label = label, x = Long, y = Lat),
                  size = 5#,
                  #nudge_x = Map_coordinates_TJ_F$NudgeX,
                  #nudge_y = Map_coordinates_TJ_F$NudgeY
  ) +
  coord_sf(xlim = c(lon_min_TJ_F, lon_max_TJ_F),
           ylim = c(lat_min_TJ_F, lat_max_TJ_F),
           expand = FALSE) +
  theme(axis.line = element_line(),
        axis.ticks = element_blank(),  # hides the Lat, Long tick lines
        axis.text.x = element_blank(), # hides the Longitude lines
        axis.text.y = element_blank(), # hides the Latitude lines
        axis.title = element_blank(),  # hides the X and Y titles
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth =.3),
        panel.background = element_rect(fill = "antiquewhite"))

print(Tjörn_befästningar_2)

# Save as PNG file
ggsave("Gifs/Tjörn_befästningar_2_%03d.png",
       plot = last_plot(),
       device = "png",
       scale = scale_factor,
       units="mm",
       limitsize = FALSE,
       dpi=300)

# More work needed
# Some issues with overlap so modifying

Map_coordinates_TJ_F_Modify <- Map_coordinates_TJ_F %>%
  # create new variable LabelX from label. Add "\n," to allow labels be wrapped in plot
  dplyr::mutate(LabelX = case_when(
    #label == "Batteri Dyrön" ~ "Batteri\nDyrön",
    #                               label == "Batteriplats Tjuvkil" ~ "Batteriplats\nTjuvkil",
    #                               label == "Batteriplats Tjuvkil södra" ~ "Batteriplats\nTjuvkil södra",
                                   TRUE ~ label)) %>%
  # Add a new column NudgeX which will be used to nudge text labels to the left or right
 dplyr::mutate(NudgeX = case_when(label %in% c("Batteri Dyrön") ~ .035,  # moved right
                                  label %in% c("Batteriplats Tjuvkil") ~ .045,  # moved right
                                  label %in% c("227 Kyrkesund") ~ .04,  # moved right
                                  label %in% c("226 Kyrkesund") ~ -.04,  # moved left
                                  label %in% c("228 Kyrkesund") ~ -.04,  # moved left
                                 # label %in% c("223 Skärhamn") ~ -.05, # moved left
                                  label %in% c("224 Skärhamn") ~ .05, # moved right
                                  label %in% c("225 Skärhamn") ~ -.059,
                                  label %in% c("Batteriplats Tjuvkil södra") ~ .059,  # moved right
                                  label == "221 Rönnäng" ~ -.02, # moved left
                                  TRUE ~ 0)) %>%
 # Add a new column NudgeY which will be used to nudge text labels up or down
 dplyr::mutate(NudgeY = case_when(label == "Batteri Koön" ~ -.005, #moved down
                                  label %in% c("226 Kyrkesund") ~ -.003,  # moved down
                                  label %in% c("228 Kyrkesund") ~ .003,  # moved up
                                  label == "400 Skåpesund" & Long == 11.7045 ~ -.005,
                                  label %in% c("223 Skärhamn") ~ -.005,
                                  label %in% c("Batteri Koön") ~ -.005,
                                  label %in% c("Batteriplats Tjuvkil") ~ 0,
                                  label %in% c("Batteriplats Tjuvkil södra") ~ 0,
                                  label == "Batteri Dyrön" ~ 0,
                                  TRUE ~ .005))

Points_TJ_F_Modify <- Map_coordinates_TJ_F_Modify %>%
  sf::st_as_sf(coords = c("Long", "Lat"), crs = 4326, agr = "constant")

Labels_TJ_F_Modify <- Map_coordinates_TJ_F_Modify %>%
  sf::st_as_sf(coords = c("Long", "Lat"), crs = 4326, agr = "constant")


Tjörn_befästningar_3 <- ggplot() +
  geom_sf(
    data = sea_TJ_F,
    fill = "lightblue",
    color = "steelblue"
  ) +
  geom_sf(data = Points_TJ_F_Modify, size = 2, shape = 21, fill = "darkred") +
  geom_sf_text(data = Labels_TJ_F_Modify, 
                  aes(label = LabelX, lineheight = .85),
                  size = 5,
                  nudge_x = Map_coordinates_TJ_F_Modify$NudgeX,
                  nudge_y = Map_coordinates_TJ_F_Modify$NudgeY) +
  coord_sf(xlim = c(lon_min_TJ_F, lon_max_TJ_F),
           ylim = c(lat_min_TJ_F, lat_max_TJ_F),
           expand = FALSE) +
  theme(axis.line = element_line(),
        axis.ticks = element_blank(),  # hides the Lat, Long tick lines
        axis.text.x = element_blank(), # hides the Longitude lines
        axis.text.y = element_blank(), # hides the Latitude lines
        axis.title = element_blank(),  # hides the X and Y titles
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth =.3),
        panel.background = element_rect(fill = "antiquewhite"))

print(Tjörn_befästningar_3)

# Save as PNG file
ggsave("Gifs/Tjörn_befästningar_3_%03d.png",
       plot = last_plot(),
       device = "png",
       scale = scale_factor,
       units="mm",
       limitsize = FALSE,
       dpi=300)


