# install.packages(c("ggrepel", 
#                    "ggspatial", "libwgeom", "sf", "rnaturalearth", 
#                    "rnaturalearthdata"))

# Issues installing rnaturalearthdata 
# install.packages("sp") # rnaturalearthhires has a dependency on sp so install first
# install.packages("rnaturalearthhires", 
#                  repos = "http://packages.ropensci.org", 
#                  type = "source")

###########
library(ggplot2)
library(sf)
library(dplyr)
library(ggrepel)

library(rnaturalearth)
library(rnaturalearthdata)
library(readxl)
library(lwgeom)
library(osmdata) # Used to get data from openstreetmap

# Returns a world coastline map 
world <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
coastline10 <- rnaturalearth::ne_coastline(10)

# Define the Latitude and Longitudes of the Map
lon_min_O <- 3.18 # xmin
lon_max_O <- 30.88 # xmax
lat_min_O <- 52.66 # ymin
lat_max_O <- 70.66 # ymax


# First attempt based on coastline dataset. 
# This renders only as a line because the coastline is not a polygon
Map1 <- ggplot(data = coastline10) +
  geom_sf(fill = "lightblue", color = "steelblue", linewidth = .2) +
  coord_sf(xlim = c(lon_min_O, lon_max_O), 
           ylim = c(lat_min_O, lat_max_O), 
           expand = TRUE) +
  theme_classic()
print(Map1)

scale_factor = 10

ggsave("GIFs/Map2_Ostersjon_Map1_basic_%03d.png",
       plot = last_plot(),
       device = "png",
       scale = scale_factor,
       units="mm",
       limitsize = FALSE,
       dpi=300)


# Download lakes dataset and save as lakes110
lakes110 <- rnaturalearth::ne_download(scale = 10, type = "lakes", 
                                       category = "physical") %>%
            dplyr::filter(scalerank < 6) #%>%
          #  dplyr::filter(name %in% c("Vänern","Vättern") )


# 1. Create a rectangle for the bounding box based on the Latitude and Longitude above
bb_rect_O <- data.frame(lat = c(lat_min_O, lat_max_O),
                        lon = c(lon_min_O, lon_max_O)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

# 2. Split the overall rectangle for bbox via coastline (coastline10$osm_lines)
bb_rect_O_split <- bb_rect_O %>% 
  st_split(coastline10$geometry) %>%
  st_collection_extract("POLYGON")

# 3. Extract the split rectangle parts
sea_O  <- bb_rect_O_split[1]
land_O <- bb_rect_O_split[2]

# Displays sea as light blue, land outline as steel blue 
# and land as antique white  (taken from the panel.background fill)
Map2 <- ggplot(data = sea_O) +
  geom_sf(fill = "lightblue",
          color = "steelblue") +
  geom_sf(data=lakes110, fill = "lightblue") +
  coord_sf(xlim = c(lon_min_O +1.2, lon_max_O -.8), 
           ylim = c(lat_min_O +.8, lat_max_O -.8), 
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

print(Map2)

ggsave("GIFs/Map2_Ostersjon_Map2_%03d.png",
       plot = last_plot(),
       device = "png",
       scale = scale_factor,
       units="mm",
       limitsize = FALSE,
       dpi=300)

# Read in file containing towns, seas and countries
Map_coordinates_O <- readxl::read_excel("Data/Map2_Balticmap_coordinates_v4.xlsx", 
                                             sheet = "Sheet1", range = "A1:F47",
                                             col_types = c("text", "text", "numeric", 
                                                           "numeric", "text", "text")) %>%
  # Correct some of the Town names
  # Add a new column "Label" based on Ort. 
  # Split some of the town names into 2 lines using \n
  dplyr::mutate(Label = case_when(Ort == "Stettin" ~ "Stettin / Szczecin",
                                  Ort == "Königsberg" ~ "Königsberg / Kaliningrad",
                                  Ort == "Hela" ~ "Hela /\nHel",
                                  Ort == "Danzig / Gdansk" ~ "Danzig /\nGdansk",
                                  Ort == "Stolpmünde / Utska" ~ "Stolpmünde /\nUtska",
                                  TRUE ~ Ort)) %>%
  # Add a new column "Type" which is set to Country if the Ort column and Land column are the same
  # Sea if the Ort column is "Kattegatt", "Östersjön", "Skagerrak", "Nordsjön" and otherwise set to City
  dplyr::mutate(Type = case_when((Ort == Land) ~ "Country",
                                 Ort %in% c("Kattegatt", "Östersjön", 
                                            "Skagerrak", "Nordsjön") ~ "Sea",
                                 TRUE ~ "City")) %>%
  # Add a new column "Fontsize" which is set to 7 if Type is Country, 
  # 4 if Ort is one of the capital cities and 3 if any other value
  dplyr::mutate(FontSize = case_when(
    Type == "Country" ~ 7,
    Ort %in% c("Köpenhamn","Stockholm","Oslo") ~ 4,
    TRUE ~ 3)) %>%
  # Add a new column "FontFace" which is set to "bold.italic" if Type is Sea, 
  # "bold" if Ort is one of the capital cities and "plain" if any other value
  dplyr::mutate(FontFace = case_when((Type == "Sea") ~ "bold.italic",
                                     Ort %in% c("Köpenhamn","Stockholm","Oslo") ~ "bold",
                                     TRUE ~ "plain")) %>%
  # Add a new column "FontColour" which is set to "steelblue" if Type is Sea, 
  # and "black" if any other value
  dplyr::mutate(FontColour = case_when((Type == "Sea") ~ "steelblue",
                                       TRUE ~ "black")) %>%
  # Add a new column NudgeX with values for nudging text labels left or right
  dplyr::mutate(NudgeX = case_when(
    Ort %in% c("Skagerrak") ~ .127,# moved right
    Ort %in% c("Kristiansand") ~ -1,
    Ort %in% c("Göteborg") ~ 1, # moved right
    Ort %in% c("Stettin") ~ 1, # moved right
    Ort %in% c("Königsberg") ~ 2, # moved right
    Ort %in% c("Kragerø") ~ -.2, # moved left
    Ort %in% c("Kiel","Lübeck") ~ -.6, # moved left
    Ort %in% c("Tjörn") ~ -.5, # moved left
    Ort %in% c("Åbo") ~ .2, # moved right
    Ort %in% c("Helsingfors") ~ -.3, # moved left
    Ort %in% c("Strömstad") ~ .52, # moved right
    Ort %in% c("Memel / Klaipėda") ~ .9, # moved right
    Ort %in% c("Königsberg") ~ .2, # moved right
    TRUE ~ 0)) %>%
  # Add a new column NudgeY with values for nudging text labels up or down
  dplyr::mutate(NudgeY = case_when(
    Ort %in% c("Oslo","Åbo","Helsingfors","Luleå", 
               "Kemi", "Karlstad", "Stockholm") ~ .05, # moved above
    Ort == "Skagerrak" ~ -.04, # moved below
    Ort == "Kattegatt" ~ -.1,
    Ort == "Kragerø" ~ .15,
    Ort %in% c("Strömstad") ~ .2, # moved above
    Ort %in% c("Hamburg", "Wismar") ~ -.1, # moved below
    TRUE ~ 0))

Labels_O <- Map_coordinates_O %>%
  sf::st_as_sf(coords = c("Long", "Lat"), crs = 4326, agr = "constant")

Points_O <- dplyr::filter(Map_coordinates_O, !Mark == "area") %>%
  sf::st_as_sf(coords = c("Long", "Lat"), crs = 4326, agr = "constant")

# Displays sea as light blue and points as dark red
plotcoast <- ggplot(data = sea_O) +
  geom_sf(fill = "lightblue",
          color = "steelblue") +
  geom_sf(data=lakes110, fill = "lightblue") +
  geom_sf(data = Points_O, size = 2, shape = 21, fill = "darkred") +
  geom_sf_text(data = Labels_O, aes(label = Label, lineheight = .85), 
               size = Labels_O$FontSize) +
  coord_sf(xlim = c(lon_min_O +1.2, lon_max_O -.8), 
           ylim = c(lat_min_O +.8, lat_max_O -.8), 
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

print(plotcoast)

ggsave("GIFs/Map2_Ostersjon_coast_1_%03d.png",
       plot = last_plot(), 
       device = "png",
       scale = scale_factor,
       units="mm", 
       limitsize = FALSE,
       dpi=300)

# Create Plot using ggrepel to separate the text labels
options(ggrepel.max.overlaps = Inf)


plotcoastLabels <- ggplot(data = sea_O) +
  geom_sf(fill = "lightblue",
          color = "steelblue") +
  geom_sf(data=lakes110, fill = "lightblue") +
  geom_sf(data = Points_O, size = 2, shape = 21, fill = "darkred") +
  geom_text_repel(data = Map_coordinates_O, 
                  aes(label = Label, 
                      x = Long, y = Lat,
                      lineheight = .85), 
                  size = Map_coordinates_O$FontSize, 
                  fontface = Map_coordinates_O$FontFace,
                  colour = Map_coordinates_O$FontColour,
                  nudge_x = Map_coordinates_O$NudgeX,
                  nudge_y = Map_coordinates_O$NudgeY) +
  coord_sf(xlim = c(lon_min_O +1.2, lon_max_O -.8), 
           ylim = c(lat_min_O +.8, lat_max_O -.8), 
           expand = TRUE) +
  theme(axis.line = element_line(),
        axis.ticks = element_blank(),  # hides the Lat, Long tick lines
        axis.text.x = element_blank(), # hides the Longitude lines
        axis.text.y = element_blank(), # hides the Latitde lines
        axis.title = element_blank(),  # hides the X and Y titles
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth =.3),
        panel.background = element_rect(fill = "antiquewhite"))

print(plotcoastLabels)

# Save as GIF file
ggsave("GIFs/Map2_Ostersjon_coast_withLabel_1_%03d.png",
       plot = last_plot(), 
       device = "png",
       scale = scale_factor,
       units="mm",
       limitsize = FALSE,
       dpi=300)
