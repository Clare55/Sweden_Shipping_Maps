library(lwgeom)
library(osmdata)
library(sf)
library(dplyr)
library(ggplot2)
library(readxl)
library(ggrepel)

lon_min_TJ <- 11.39 # xmin
lon_max_TJ <- 11.9 # xmax
lat_min_TJ <- 57.84 # ymin
lat_max_TJ <- 58.14 # ymax

coords_TJ <- matrix(c(lon_min_TJ,lon_max_TJ,lat_min_TJ,lat_max_TJ), byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(c('x','y'),c('min','max'))) 
coords_TJ


### get sea & land as polygons
# 1. Get coastline from OpenStreetMap as an SF object using Osmdata
coast_TJ <- coords_TJ %>%
  opq() %>%
  add_osm_feature(key = "natural", value = "coastline") %>%
  osmdata_sf()

# Maps rendered with coast_TJ$osm_lines miss many small islands and islets.
# Maps rendered with coast_TJ$osm_polygons misses the main land.
# Convert coast_TJ$osm_polygons to lines to capture all islands and islets.
coast_TJ2 <- osm_poly2line (coast_TJ)

# 2. Get overall rectangle for bbox
bb_rect_TJ <- data.frame(
  lat = c(lat_min_TJ, lat_max_TJ),
  lon = c(lon_min_TJ, lon_max_TJ)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

# 3. Split the overall rectangle for bbox via coastline (coast2$osm_lines)
bb_rect_split_TJ <- bb_rect_TJ %>% 
  st_split(coast_TJ2$osm_lines) %>% 
  st_collection_extract("POLYGON")

# 4. Extract the split rectangle parts
sea_TJ  <- bb_rect_split_TJ[1]
land_TJ <- bb_rect_split_TJ[2]

### Create Plots 
# Creates a plot with the sea in light blue, outlined in steelblue. 
# Land appears as antique white because of the panel.background setting.
Tjorn_colour_1 <- ggplot() +
  geom_sf(
    data = sea_TJ,
    fill = "lightblue",
    color = "steelblue"
  ) + 
  coord_sf(xlim = c(lon_min_TJ, lon_max_TJ), 
           ylim = c(lat_min_TJ, lat_max_TJ),
           expand = FALSE) +  
  theme(axis.line = element_line(),
        axis.ticks = element_blank(),  # hides the Lat, Long tick lines
        axis.text.x = element_blank(), # hides the Longitude lines
        axis.text.y = element_blank(), # hides the Latitde lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=.3),
        panel.background = element_rect(fill = "antiquewhite"))
print(Tjorn_colour_1)

scale_factor = 10

## Save as PNG file
ggsave("Gifs/Tjorn_colour_1_%03d.png",
       plot = last_plot(), 
       device = "png",
       scale = scale_factor,
       units="mm", 
       limitsize = FALSE,
       dpi=300)


### Add points
Map_coordinates_TJ <- read_excel("Data/Coordinates_Tjörn_v2.xlsx", sheet = "Sheet1", 
                              range = "B1:F57") %>% 
                   na.omit() %>%  # Filter out any nulls
  dplyr::mutate(Type = case_when(Place %in% c("Hakefjorden", "Stigfjorden", 
                                              "Skagerrak") ~ "Sea", 
                                 type %in% c("island", "location") ~ "Location",
                                 type %in% c("spot") ~ "Spot",
                                 TRUE ~ "Town")) %>%  
  # Add a new column Type which is set to Town or Sea depending on Place
  dplyr::mutate(FontSize = case_when((Type == "Country") ~ 8,
                                     Type  == "Town" ~ 4.75,
                                     TRUE ~ 5)) %>%
  # Add a new column FontFace which is set to bold.italic if Type is Sea or plain if not
  dplyr::mutate(FontFace = case_when((Type == "Sea") ~ "bold.italic",
                                    # Type == "Location" ~ "bold",
                                     TRUE ~ "plain")) %>%
  # Add a new column FontColour which is set to steel blue if Type is Sea or black if not
  dplyr::mutate(FontColour = case_when((Type == "Sea") ~ "steelblue",
                                    TRUE ~ "black")) %>%
  # Modify Place to split Lilla Askerön into 2 lines
  dplyr::mutate(Place = case_when((Place == "Lilla Askerön") ~ "Lilla\nAskerön", 
                                   TRUE ~ Place)) %>%
  # Add a new column NudgeX which will be used to nudge text labels to the left or right
  dplyr::mutate(NudgeX = case_when((Place == "Stigfjorden") ~ -.001, # moved left
                                   (Place == "Hakefjorden") ~ .001,  # moved right
                                   (Place == "Tjörnekalv") ~ .001, 
                                   TRUE ~ 0)) %>%
  # Add a new column NudgeY which will be used to nudge text labels up or down
  dplyr::mutate(NudgeY = case_when((Place == "Koön") ~ -.001,
                                   (Place == "Älgön") ~ -.001,
                                   (Place == "Instön") ~ .001,
                                   TRUE ~ 0))

# Modify the coordinates for Klädesholmen and Flatholmen
# Was 11.544
Map_coordinates_TJ[Map_coordinates_TJ$Place=="Klädesholmen", "Long"] <- 11.526
# Was 11.518
Map_coordinates_TJ[Map_coordinates_TJ$Place=="Flatholmen", "Long"] <- 11.500


Points_TJ <- dplyr::filter(Map_coordinates_TJ, !mark == "region (just label, no dot)") %>%
  sf::st_as_sf(coords = c("Long", "Lat"),
               crs = 4326, agr = "constant")


#####
options(ggrepel.max.overlaps = Inf)

# Creates a plot with the sea in light blue, outlined in steelblue. 
# Adds points from the Points_TJ data frame (filters the Map_coordinates_TJ data frame 
# where mark equals "region (just label, no dot)"
# Adds Labels from the Map_coordinates_TJ data frame
# Use ggrepel to force the labels to not overlap

Tjorn_colour_2 <- ggplot() +
   geom_sf(
     data = sea_TJ,
     fill = "lightblue",
     color = "steelblue"
   ) +
   geom_sf(data = Points_TJ, size = 1, shape = 21, fill = "darkred") +
   geom_text_repel(data = Map_coordinates_TJ, aes(label = Place, x = Long, y = Lat),
   size = Map_coordinates_TJ$FontSize,
   fontface = Map_coordinates_TJ$FontFace,
   colour = Map_coordinates_TJ$FontColour,
   nudge_x = Map_coordinates_TJ$NudgeX,
   nudge_y = Map_coordinates_TJ$NudgeY
   ) +
   coord_sf(xlim = c(lon_min_TJ, lon_max_TJ),
            ylim = c(lat_min_TJ, lat_max_TJ),
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

 print(Tjorn_colour_2)

# Save as PNG file
 ggsave("Gifs/Tjorn_colour_2_%03d.png",
        plot = last_plot(),
        device = "png",
        scale = scale_factor,
        units="mm",
        limitsize = FALSE,
        dpi=300)
 

######
 # The labels overlap too much and some jump too far from location 
 # so try with sf_text but modifying placement for each label
######
Map_coordinates_T <- read_excel("Data/Coordinates_Tjörn_v2.xlsx", sheet = "Sheet1", 
                               range = "B1:F57") %>% 
  # Filter out any nulls
  na.omit() %>%  
  # Add a new column Type which is set to Town or Sea depending on Place
  dplyr::mutate(Type = case_when(Place %in% c("Hakefjorden", "Stigfjorden", 
                                              "Skagerrak") ~ "Sea", 
                                 mark == "region (just label, no dot)" ~ "Place",
                                 TRUE ~ "Town")) %>%  
  dplyr::mutate(FontSize = case_when((Type == "Country") ~ 8,
                                     TRUE ~ 5)) %>%
  # Add a new column FontFace which is set to bold.italic if Type is Sea or plain if not
  dplyr::mutate(FontFace = case_when((Type == "Sea") ~ "bold.italic",
                                     Type == "Place" ~ "bold",
                                     TRUE ~ "plain")) %>%
  # Add a new column FontColour which is set to steel blue if Type is Sea or black if not
    dplyr::mutate(FontColour = case_when((Type == "Sea") ~ "steelblue",
                                       Type == "Place" ~ "grey10",
                                       TRUE ~ "black")) %>%
  # Modify Place to split Lilla Askerön into 2 lines
  dplyr::mutate(Place = case_when((Place == "Lilla Askerön") ~ "Lilla\nAskerön", 
                                  TRUE ~ Place)) %>%
  # Add a new column NudgeX which will be used to nudge text labels to the left or right
  dplyr::mutate(NudgeX = case_when((Place == "Nösund") ~ .001,
                                   (Place %in% c("Stigfjorden")) ~ .015, # moved right
                                   (Place %in% c("Tjörns huvud")) ~ .035, # moved right
                                   (Place %in% c("Kållekärr")) ~ .026, # moved right
                                   (Place %in% c("Stenkyrka")) ~ .027, # moved right
                                   (Place %in% c("Kyrkesund","Stockevik")) ~ .026, # moved right
                                   (Place == "Rörtången") ~ .008,
                                   (Place == "Bleket") ~ .016,
                                   (Place %in% c("Härön","Grönskären")) ~ -.008, # moved left
                                   (Place %in% c("Flatholmen",
                                                 "Klädesholmen", "Tjörnekalv")) ~ -.016, # moved left
                                   (Place == "Sundsby") ~ .016, # moved right
                                   (Place == "Hälleviksstrand") ~ .016, # moved right
                                   (Place == "Edshultshall") ~ -.016, # moved left
                                   TRUE ~ 0)) %>%
  # Add a new column NudgeY which will be used to nudge text labels up or down
  dplyr::mutate(NudgeY = case_when((Place %in% c("Almösund", "Brattön", "Kuballe","Halsbäck", 
                                                 "Härön", "Höviksnäs", 
                                                 "Lilldal", "Mollösund", 
                                                 "Nordviksstrand", "Nösund", 
                                                 "Skåpesund", "Rönnäng",
                                                 "Hjälteby", "Harsprånget",
                                                 "Hälleviksstrand", "Edshultshall")) ~ .004, # moved above
                                   (Place %in% c("Björholmen", "Djupvik",  
                                                 "Klövedals kyrka", "Marstrand", 
                                                 "Rörtången", "Rösselvik", 
                                                 "Sibräcka", "Tjuvkil")) ~ -.004,
                                   TRUE ~ 0)) # moved down


Labels_T <- dplyr::filter(Map_coordinates_T) %>%
  sf::st_as_sf(coords = c("Long", "Lat"),
               crs = 4326, agr = "constant")

Points_T <- dplyr::filter(Map_coordinates_T, !mark == "region (just label, no dot)") %>%
  sf::st_as_sf(coords = c("Long", "Lat"),
               crs = 4326, agr = "constant")


# Creates a plot with the sea in light blue, outlined in steelblue. 
# Adds points from the Points data frame (filters the Map_coordinates data frame 
# where mark equals "region (just label, no dot)"
# Adds Labels from Labels data frame (filters the Map_coordinates data frame 
# where mark does not equal "region (just label, no dot)"

Tjorn_colour_3 <- ggplot() +
  geom_sf(
    data = sea_TJ,
    fill = "lightblue",
    color = "steelblue"
  ) + 
  geom_sf(data = Points_T, size = 1, shape = 21, fill = "darkred") +
  geom_sf_text(data = Labels_T, aes(label = Place, lineheight = .85),
               size = Labels_T$FontSize,
               fontface = Labels_T$FontFace,
               colour = Labels_T$FontColour,
               nudge_x = Labels_T$NudgeX,
               nudge_y = Labels_T$NudgeY) +
  coord_sf(xlim = c(lon_min_TJ, lon_max_TJ), 
           ylim = c(lat_min_TJ, lat_max_TJ),
           expand = FALSE) +  
  theme(axis.line = element_line(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth =.3),
        panel.background = element_rect(fill = "antiquewhite"))

print(Tjorn_colour_3)

ggsave("Gifs/Tjorn_colour_3_bold_%03d.png",
       plot = last_plot(), 
       device = "png",
       scale = scale_factor,
       units="mm",  
       limitsize = FALSE,
       dpi=300)
