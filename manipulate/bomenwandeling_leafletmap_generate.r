# ############################################## #
# Generate map for Jette bomenwandeling Isabelle #
# ############################################## #

library(dplyr)
library(rgdal)
library(leaflet)
library(readr)
library(stringr)
library(htmlwidgets)

# install.packages("devtools")
# devtools::install_github("paleolimbot/exifr")
library(exifr)

# install.packages("magick")
library(magick)

# read in GPX-track (in two parts)
gpx.track.deel1 <- readOGR('data/tracks/17-4-2017 14_24.gpx', layer = "tracks")
gpx.track.deel2 <- readOGR('data/tracks/17-4-2017 17_34.gpx', layer = "tracks")


# Read in photo metadata
bomen_imgs <- list.files('media/photos/', full.names = TRUE, pattern = '.JPG')
bomen_imgs_metadata <- read_exif(bomen_imgs)
bomen_imgs_metadata$fotonummer <- tools::file_path_sans_ext(basename(bomen_imgs))

# read in manual photo info
bomen_info <- read_csv(
  'media/20171009_bomenwandeling_jette_bomeninfo.csv', 
  col_types = cols(
    fotonummer = col_character(),
    boom_naam = col_character(),
    lokalisatie_label = col_character(),
    optionele_detailfoto = col_character(),
    lat_manueel = col_character(),
    lon_manueel = col_character()
  ))

# merge photo metadata & info
# bomen_imgs_metadata %>% select(starts_with('GPS')) %>% glimpse()
bomen_info <- left_join(
  bomen_info, 
  bomen_imgs_metadata %>%
    select(SourceFile, FileName, GPSLatitude, GPSLongitude, GPSAltitude) %>%
    mutate(fotonummer = tools::file_path_sans_ext(FileName)),
  by = 'fotonummer') %>%
  rename_all(str_to_lower)
bomen_info %>% glimpse()

# generate popup-html
bomen_info <- bomen_info %>%
  mutate(
    popup = "<div><div><h1>BOOMNAAM</h1><img width=100%, height=100% src='PHOTOPATH' /></a></div><div>LOCALISATIE</div></div>",
    popup = str_replace(popup, 'BOOMNAAM', boom_naam),
    popup = str_replace(popup, 'LOCALISATIE', lokalisatie_label),
    popup = str_replace(popup, 'PHOTOPATH', paste0('https://s3-eu-west-1.amazonaws.com/hiking-images/20170417_bomenwandeling_jette/small/', fotonummer, '.JPG')))


# convert images to smaller scale
# imgs <- image_read(bomen_imgs)
# imgs_small <- image_rotate(image_scale(imgs, "x800"), 0)
# imgs_small <- image_scale(imgs, "x800")
# imgs_small_fns <- file.path('media/photos/small/', basename(bomen_imgs))
# for (i in seq_along(imgs_small)){
#   image_write(image = imgs_small[i], path = imgs_small_fns[i])
# }



icons <- awesomeIcons(
  icon = 'fa-tree',
  iconColor = 'black',
  library = 'fa',
  markerColor = 'green'
)

map <- leaflet() %>%
  addProviderTiles("OpenMapSurfer.Roads", group = "Road map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addLayersControl(
    # baseGroups = c("Topographical", "Road map", "Satellite"),
    baseGroups = c("Road map", "Satellite"),
    overlayGroups = c("Hiking routes", "Photo markers"),
    options = layersControlOptions(collapsed = FALSE))

map <- map %>% 
  addPolylines(data = gpx.track.deel1) %>%
  addPolylines(data = gpx.track.deel2)

map <- map %>%
  addAwesomeMarkers(lat = bomen_info$gpslatitude, lng = bomen_info$gpslongitude, popup = bomen_info$popup, icon=icons)

saveWidget(map, 'bomenwandeling_jette.html')


# map %>%
#   addMarkers(lat = bomen_imgs_metadata$GPSLatitude, lng = bomen_imgs_metadata$GPSLongitude, popup = bomen_imgs_metadata$FileName)
# 
#   # Add tiles as baseGroup
#   addProviderTiles("OpenMapSurfer.Roads",
#                    group = "Wegenkaart") %>%
#   addProviderTiles("Thunderforest.Landscape",
#                    group = "Topografisch") %>%
#   addProviderTiles("Esri.WorldImagery",
#                    group = "Satelliet") %>%
#   
#   # Add layers as overlayGroup
#   addPolylines(color="red", weight = 4,
#                popup=track_label, 
#                group = "Wandelroutes")  %>%
#   
#   # addMarkers(
#   #   markers$lng,  markers$lat,
#   #   #coordinates(wp)[100,1], coordinates(wp)[100,2],
#   #   popup = as.vector(popups_html), 
#   #   group="Foto-markeringen") %>%
  