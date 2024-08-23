# Creates Map 4. Naval War Events near Tjörn 1939-1945
# (Karta 4. Sjökrigshändelser i Tjörns närhet 1939–1945)

library(lwgeom)
library(osmdata)
library(sf)
library(dplyr)
library(ggplot2)
library(stringr)
library(readxl)

lon_min_TO <- 10.2 # xmin
lon_max_TO <- 12.2 # xmax
lat_min_TO <- 57.5 # ymin
lat_max_TO <- 58.5 # ymax

coords_TO <- matrix(c(lon_min_TO,lon_max_TO,lat_min_TO,lat_max_TO), byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(c('x','y'),c('min','max'))) 
coords_TO

# Get coastline from OpenStreetMap as an SF object using Osmdata
coast_TO <- coords_TO %>%
  opq() %>%
  add_osm_feature(key = "natural", value = "coastline") %>%
  osmdata_sf()

# get rivers and lakes
water_TO <- coords_TO %>%
  opq() %>%
  add_osm_feature(key = "natural", value = "water") %>%
  osmdata_sf()

### Create Plots 
# Creates a plot with the land outlined in steelblue and inland rivers and lakes in purple
TjornOrust_colour_1 <- ggplot() +
  geom_sf(
    data = coast_TO$osm_lines,
    color = "steelblue"
  ) + 
  geom_sf(
    data = water_TO$osm_line,
    fill = NA,
    color = "purple",
  ) +
  coord_sf(xlim = c(lon_min_TO, lon_max_TO), 
           ylim = c(lat_min_TO, lat_max_TO),
           expand = FALSE) +  
  theme(axis.line = element_line(),
        axis.ticks = element_blank(),  # hides the Lat, Long tick lines
        axis.text.x = element_blank(), # hides the Longitude lines
        axis.text.y = element_blank(), # hides the Latitude lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
print(TjornOrust_colour_1)

# Maps need to be scaled up when saved as PNG files
scale_factor = 10

## Save as PNG file
ggsave("Gifs/Tjorn_Orust_colour_1_%03d.png",
       plot = last_plot(), 
       device = "png",
       scale = scale_factor,
       units="mm", 
       limitsize = FALSE,
       dpi=300)

# Rivers adding too much clutter so removed
# Creates map based on coastline polygos
TjornOrust_colour_2 <- ggplot() +
  geom_sf(
    data = coast_TO$osm_polygons,
    fill = "lightblue",
    color = "steelblue"
  ) + 
  coord_sf(xlim = c(lon_min_TO, lon_max_TO), 
           ylim = c(lat_min_TO, lat_max_TO),
           expand = FALSE) +  
  theme(axis.line = element_line(),
        axis.ticks = element_blank(),  # hides the Lat, Long tick lines
        axis.text.x = element_blank(), # hides the Longitude lines
        axis.text.y = element_blank(), # hides the Latitde lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=.3),
        panel.background = element_rect(fill = "antiquewhite"))
print(TjornOrust_colour_2)

ggsave("Gifs/Tjorn_Orust_colour_2_%03d.png",
       plot = last_plot(), 
       device = "png",
       scale = scale_factor,
       units="mm", 
       limitsize = FALSE,
       dpi=300)

# Maps rendered with coast$osm_lines miss many small islands and islets.
# Maps rendered with coast$osm_polygons misses the main land.
# Convert coast$osm_polygons to lines to capture all islands and islets.
coast2_TO <- osm_poly2line(coast_TO)

TjornOrust_colour_3 <- ggplot() +
  geom_sf(
    data = coast2_TO$osm_lines,
    fill = "lightblue",
    color = "steelblue"
  ) + 
  coord_sf(xlim = c(lon_min_TO, lon_max_TO), 
           ylim = c(lat_min_TO, lat_max_TO),
           expand = FALSE) +  
  theme(axis.line = element_line(),
        axis.ticks = element_blank(),  # hides the Lat, Long tick lines
        axis.text.x = element_blank(), # hides the Longitude lines
        axis.text.y = element_blank(), # hides the Latitde lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=.3),
        panel.background = element_rect(fill = "antiquewhite"))
print(TjornOrust_colour_3)
## Result is very cluttered

# Convert the line to polygons
## Create a bounding box
bb_rect_TO <- data.frame(
  lat = c(lat_min_TO, lat_max_TO),
  lon = c(lon_min_TO, lon_max_TO)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

# Split the overall rectangle for bbox via coastline (coast2$osm_lines)
bb_rect_split_TO <- bb_rect_TO %>% 
  st_split(coast2_TO$osm_lines) %>% 
  st_collection_extract("POLYGON")

# Extract the split rectangle parts
land_TO <- bb_rect_split_TO[1]
sea_TO  <- bb_rect_split_TO[2]

# creates map based on polygons.
# Land is outlined in steel blue, sea is in light blue and land is in antique white
TjornOrust_colour_4 <- ggplot() +
  geom_sf(
    data = sea_TO,
    fill = "lightblue",
    color = "steelblue"
  ) + 
  coord_sf(xlim = c(lon_min_TO, lon_max_TO), 
           ylim = c(lat_min_TO, lat_max_TO),
           expand = FALSE) +  
  theme(axis.line = element_line(),
        axis.ticks = element_blank(),  # hides the Latitude and Longitude tick lines
        axis.text.x = element_blank(), # hides the Longitude lines
        axis.text.y = element_blank(), # hides the Latitude lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=.3),
        panel.background = element_rect(fill = "antiquewhite"))
print(TjornOrust_colour_4)

## Save as PNG file
ggsave("Gifs/Tjorn_Orust_colour_4_%03d.png",
       plot = last_plot(), 
       device = "png",
       scale = scale_factor,
       units="mm", 
       limitsize = FALSE,
       dpi=300)

# Import spreadsheet with latitude and longitude of towns and shipping
Map_coordinates_TO <- readxl::read_excel("Data/OrustTjorn_ships_v1.xlsx", sheet = "Sheet1", 
                                 range = "A1:D24") %>% 
  #  dplyr::mutate(LabelX = str_replace(Label, ",",",\n")) %>%
  # create new variable LabelX from Label. Replace values of "," with "\n," to allow labels be wrapped in plot
  dplyr::mutate(LabelX = case_when((Label == "MD 354 Eros av Åstol, 14/12 1944") ~ "MD 354 Eros av Åstol,\n14/12 1944",
                                   (Label == "MD 498 Elly av Dyrön, 6/11 1940") ~ "MD 498 Elly av Dyrön,\n6/11 1940",
                                   (Label =="Wiros av Göteborg, 3/9 1942") ~ "Wiros av Göteborg,\n3/9 1942",
                                   (Label == "MD 527 Silvervåg av Åstol, 14/12 1944") ~ "MD 527 Silvervåg av Åstol, 14/12 1943",
                                   (Label == "Pater Noster") ~ "Pater\nNoster",
                                   TRUE ~ Label)) %>%
  dplyr::mutate(Type = case_when(type %in% c("ship, sunk", "ship, damaged") ~ "Ship", 
                                 type == "area" ~ "Area",
                                 TRUE ~ "Land")) %>%
  dplyr::mutate(FontSize = case_when((Type == "Ship") ~ 4,
                                     (Type == "Area") ~ 5.25,
                                     TRUE ~ 5)) %>%
  dplyr::mutate(FontFace = case_when((Type == "Ship") ~ "italic",
                                     (Type == "Area") ~ "bold",
                                     TRUE ~ "plain")) %>%
  dplyr::mutate(FontColour = case_when((Type == "Ship") ~ "black",
                                       TRUE ~ "black")) %>%
  dplyr::mutate(PointType = case_when((type == "ship, damaged") ~ 20,
                                      (type == "ship, sunk") ~ 18, # 
                                      TRUE ~ 21))%>%
  dplyr::mutate(NudgeX = case_when(
    Label %in% c("Skagen") ~ -.075, # moved left
    Label %in% c("Hjvb 356 Condor, 12/10 1944") ~ -.185, # moved left
    Label %in% c("Skytteren (no), 1/4 1943") ~ -.16, # moved left
    Label %in% c("Schiff 40 (ty), 13/4 1940") ~ -.16, # moved left
    Label %in% c("MD 527 Silvervåg av Åstol, 14/12 1944") ~ -.245, # moved left
    Label %in% c("Westfalen (ty), 8/9 1944") ~ -.16,
    Label %in% c("Friedenau och Wigbert (ty), 10/4 1940") ~ -.24,
    Label %in% c("MD 466 Dunett av Rönnäng, 13/3 1941") ~ -.25, # moved left
    Label %in% c("MD 498 Elly av Dyrön, 6/11 1940") ~ -.14, # moved left
    Label %in% c("M/T Procyon, 16/5 1940") ~ .15,
    Label %in% c("HMS Ulven, 15/4 1944") ~ .15,
    Label %in% c("St Pölsan") ~ .09, # moved right
    TRUE ~ 0)) %>%
  dplyr::mutate(NudgeY = case_when(Label %in% c("Åstol","Göteborg", "Uddevalla") ~ .016, # moved above
                                   Label %in% c("Pater Noster") ~ .03, # moved below                                  
                                   Label %in% c("MD 498 Elly av Dyrön, 6/11 1940") ~ -.002, # moved below   
                                   Label %in% c("Friedenau och Wigbert (ty), 10/4 1940") ~ -.002, # moved below,
                                   Label %in% c("MD 354 Eros av Åstol, 14/12 1944") ~ -.025, # moved below                                  
                                   Label %in% c("Måseskär", "Marstrand", "Lysekil",
                                                "Skärhamn") ~ -.016, # moved below
                                   Label %in% c("Skagen", "St Pölsan") ~ 0,
                                   TRUE ~ 0)) # moved down

Labels_TO <- Map_coordinates_TO %>%
  sf::st_as_sf(coords = c("long", "lat"),
               crs = 4326, agr = "constant")

Points_TO <- dplyr::filter(Map_coordinates_TO, !type == "area") %>%
  sf::st_as_sf(coords = c("long", "lat"),
               crs = 4326, agr = "constant")

# Using ggrepel to ensure the text labels do not overlap. 
# Setting the max overlaps allowed to Inf
options(ggrepel.max.overlaps = Inf)

TjornOrust_colour_5 <- ggplot() +
  geom_sf(
    data = sea_TO,
    fill = "lightblue",
    color = "steelblue"
  ) + 
# Add points from Point_TO and use the PointType to set the shape
  geom_sf(data = Points_TO, size = 3, shape = Points_TO$PointType, fill = "darkred") +
  geom_sf_text(data = Labels_TO, aes(label = LabelX, lineheight = .85),
               size = Labels_TO$FontSize,
               fontface = Labels_TO$FontFace,
               colour = Labels_TO$FontColour,
               nudge_x = Labels_TO$NudgeX,
               nudge_y = Labels_TO$NudgeY) +
  coord_sf(xlim = c(lon_min_TO, lon_max_TO), 
           ylim = c(lat_min_TO, lat_max_TO),
           expand = FALSE) +  
  theme(axis.line = element_line(),
        axis.ticks = element_blank(),  # hides the Latitude and Longitude tick lines
        axis.text.x = element_blank(), # hides the Longitude lines
        axis.text.y = element_blank(), # hides the Latitude lines
        axis.title = element_blank(),  # hides the X and Y titles
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=.3),
        panel.background = element_rect(fill = "antiquewhite"))
print(TjornOrust_colour_5)

## Save as PNG file
ggsave("Gifs/Tjorn_Orust_colour_6_%03d.png",
       plot = last_plot(), 
       device = "png",
       scale = scale_factor,
       units="mm", 
       limitsize = FALSE,
       dpi=300)


