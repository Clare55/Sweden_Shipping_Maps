library(ggplot2)
library(sf)
library(dplyr)
library(ggrepel)

library(rnaturalearth)
library(rnaturalearthdata)
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

# Import spreadsheet of locations
Map_coordinates_SK <- readxl::read_excel("Data/vasterhav_skagerrakspärren_ships-v2.xlsx", sheet = "Sheet1", 
                                               range = "A1:E20",
                                        col_names = TRUE,
                                        col_types = c("text", "text", "numeric", 
                                                             "numeric","text")) %>%
  # Rename the columns to something more standardised
  dplyr::rename(Label = `Label - in italics`, Type = type, Ship = ship) %>%
  # Update the column Type to clearly differentiate ships, towns and dots
  dplyr::mutate(Type = case_when(Type == "ship - rhombus" ~ "Ship",
                                 Type == "town" ~ "Town",
                                 TRUE ~ "Dot")) %>%
  # Add a new column "PointType" which has different Point Types depending on Type
  dplyr::mutate(PointType = case_when(Type == "Ship" ~ 18,
                                      Type == "Town" ~ 21,
                                      TRUE ~ 20)) %>%
  # Update "Ship" column to add the 2 towns because the Ship column is used below
  dplyr::mutate(Ship = case_when(Label == "Hanstholm" ~ "Hanstholm",
                                 Label == "Kristiansand" ~ "Kristiansand",
                                 TRUE ~ Ship)) %>%   
  # Add a new column "Type" which sets the font size to 6 if Town and 4.7 otherwise
  dplyr::mutate(FontSize = case_when(Type  == "Town" ~ 6,
                                     TRUE ~ 4.7)) %>%
  # Add a new column "FontFace" which is set to italic if Type is Ship or plain if not
  dplyr::mutate(FontFace = case_when((Type == "Ship") ~ "italic",
                                     TRUE ~ "plain")) %>%
  dplyr::mutate(LabelX = case_when(Ship == "Viros av Göteborg, 1942" ~ "Viros av Göteborg,\n1942",
                                   Ship == "Hugin av Hönö, 1940" ~ "Hugin av Hönö,\n1940",
                                   Ship == "Nippon av Öckerö, 1943" ~ "Nippon av Öckerö,\n1943",
                                   Ship == "Neptun av Hönö, 1942" ~ "Neptun av Hönö,\n1942",
                                   Ship == "Hermon och Vestkusten av Öckerö, 1943" ~ "Hermon och Vestkusten\nav Öckerö,\n1943",
                                   Ship == "Inez av Öckerö, 1940" ~ "Inez av Öckerö,\n1940",
                                   Ship == "Gotland av Hönö, 1944" ~ "Gotland av Hönö,\n1944",
                                   Ship == "Glimmaren av Gravarne, 1944" ~ "Glimmaren av Gravarne,\n1944",
                                   Ship == "Gunnaren av Fisketången, 1944" ~ "Gunnaren av Fisketången,\n1944",
                                   TRUE ~ Ship)) %>%
  # Add a new column NudgeX which will be used to nudge text labels to the left or right
  dplyr::mutate(NudgeX = case_when(Label == "Cyrene" ~ -.42,  # moved left
                                   Label == "Dalarö" ~ .4,
                                   Label == "Hermon och Vestkusten" ~ .05, # moved right
                                   Label == "Marina" ~ .4,
                                   Label == "Dagny" ~ -.4, # moved left
                                   Label == "Neptun" ~ -.05,
                                   TRUE ~ 0)) %>%
  # Add a new column NudgeY which will be used to nudge text labels up or down
  dplyr::mutate(NudgeY = case_when(Label %in% c("Inez") ~ .06, # Moved up
                                   Label %in% c("Glimmaren") ~ .06,
                                   Label %in% c("Gunnaren") ~ .06,
                                   Label %in% c("Gotland") ~ .06, # moved up
                                   Label %in% c("Nippon") ~ .06, # moved up
                                   Label %in% c("Hermon och Vestkusten") ~ -.08, # moved down
                                   Label %in% c("Hugin","Neptun") ~ -.06, # moved down
                                   Label %in% c("Viros") ~ -.06, # moved down
                                   Label %in% c("Hanstholm") ~ -.04, # moved down
                                   Label %in% c("Kristiansand") ~ .04, # moved up
                                   TRUE ~ 0)) # Moved down

Points_SK <- Map_coordinates_SK %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326, agr = "constant")

Labels_SK <- dplyr::filter(Map_coordinates_SK, !Type == "Dot") %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326, agr = "constant")

scale_factor = 10

PlotcoastLabels_SK <- ggplot(data = sea_SK) +
  geom_sf(fill = "lightblue",
          color = "steelblue") +
  geom_sf(data=lakes110, fill = "lightblue") +
  geom_sf(data = Points_SK, size = 2.5, shape = Points_SK$PointType, fill = "darkred") +
    geom_sf_text(data = Labels_SK, 
                                aes(label = LabelX, lineheight = .85), 
                 size = Labels_SK$FontSize,
                 fontface = Labels_SK$FontFace,
                 nudge_x = Labels_SK$NudgeX,
                 nudge_y = Labels_SK$NudgeY) +  
    coord_sf(
    xlim = c(lon_min_SK+1, lon_max_SK-1), ylim = c(lat_min_SK+.5, lat_max_SK-1.5), 
    expand = TRUE) +
  theme(axis.line = element_line(),
        axis.ticks = element_blank(),  # hides the Lat, Long tick lines
        axis.text.x = element_blank(), # hides the Longitude lines
        axis.text.y = element_blank(), # hides the Latitude lines
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

