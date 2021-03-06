# ################################################# #
# Generate map for Jette bomenwandeling guided tour #
# ################################################# #

library(dplyr)
library(rgdal)
library(leaflet)
library(readr)
library(stringr)
library(htmlwidgets)
library(here)

# install.packages("devtools")
# devtools::install_github("paleolimbot/exifr")
library(exifr) # for reading EXIF photo-metadata

# only needed if you want to resize in R
# install.packages("magick")
# library(magick)

# read in GPX-track (in two parts)
gpx.track.deel1 <- readOGR(here::here('data/tracks/17-4-2017 14_24.gpx'), layer = "tracks")
gpx.track.deel2 <- readOGR(here::here('data/tracks/17-4-2017 17_34.gpx'), layer = "tracks")


# Read in photo metadata
bomen_imgs <- list.files('media/photos/', full.names = TRUE, pattern = '.JPG')
bomen_imgs_metadata <- read_exif(bomen_imgs)
bomen_imgs_metadata$fotonummer <- tools::file_path_sans_ext(basename(bomen_imgs))

# read in manual photo info
bomen_info <- read_csv(
  here::here('data/20171009_bomenwandeling_jette_bomeninfo.csv'), 
  col_types = cols(
    fotonummer = col_character(),
    boom_naam = col_character(),
    lokalisatie_label = col_character(),
    optionele_detailfoto = col_character(),
    lat_manueel = col_double(),
    lon_manueel = col_double()
  ))


# bomen_imgs_metadata %>% select(starts_with('GPS')) %>% glimpse()
bomen_info <- left_join(
  bomen_info, 
  bomen_imgs_metadata %>%
    select(SourceFile, FileName, GPSLatitude, GPSLongitude, GPSAltitude) %>%
    mutate(fotonummer = tools::file_path_sans_ext(FileName)),
  by = 'fotonummer') %>%
  rename_all(str_to_lower)

# bomen_info %>% glimpse()
# bomen_info %>%
#   select(fotonummer, boom_naam, gpslatitude, gpslongitude)

# Select the manually corrected coordinate, if present
bomen_info <- bomen_info %>%
  mutate(
    gpslatitude = ifelse(is.na(lat_manueel), gpslatitude, lat_manueel),
    gpslongitude = ifelse(is.na(lon_manueel), gpslongitude, lon_manueel))

# photos are uploaded through the AWS management console to S3, can also use https://github.com/cloudyr/aws.s3
# <img> tags are then generated by appending filename to base S3-url

# optional: convert images to smaller scale
# imgs <- image_read(bomen_imgs)
# imgs_small <- image_rotate(image_scale(imgs, "x800"), 0)
# imgs_small <- image_scale(imgs, "x800")
# imgs_small_fns <- file.path('media/photos/small/', basename(bomen_imgs))
# for (i in seq_along(imgs_small)){
#   image_write(image = imgs_small[i], path = imgs_small_fns[i])
# }

# generate popup-html
bomen_info <- bomen_info %>%
  mutate(
    popup = "<div><div><h1>BOOMNAAM</h1><img width=100%, height=100% src='PHOTOPATH' /></a></div><div>LOCALISATIE</div></div>",
    popup = str_replace(popup, 'BOOMNAAM', boom_naam),
    popup = str_replace(popup, 'LOCALISATIE', lokalisatie_label),
    popup = str_replace(popup, 'PHOTOPATH', paste0('https://s3-eu-west-1.amazonaws.com/hiking-images/20170417_bomenwandeling_jette/small/', fotonummer, '.JPG')))



# Construct interactive leaflet map
# =================================

# define custom map icons
tree_icon <- awesomeIcons(
  icon = 'fa-tree',
  iconColor = 'black',
  library = 'fa',
  markerColor = 'green'
)

flag_icon <- awesomeIcons(
  icon = 'fa-flag',
  iconColor = 'black',
  library = 'fa',
  markerColor = 'red'
)

beer_icon <- awesomeIcons(
  icon = 'fa-beer',
  iconColor = 'black',
  library = 'fa',
  markerColor = 'orange'
)

# base map layer with two tile-sets
map <- leaflet() %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Routekaart") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satelliet") %>%
  addLayersControl(
    # baseGroups = c("Topographical", "Routekaart", "Satelliet"),
    baseGroups = c("Routekaart", "Satelliet"),
    overlayGroups = c("Wandelroute", "Markeringen"),
    options = layersControlOptions(collapsed = FALSE))

# add the walk (in two parts)
map <- map %>% 
  addPolylines(data = gpx.track.deel1, group = "Wandelroute") %>%
  addPolylines(data = gpx.track.deel2, group = "Wandelroute")

# add makers for photos/trees, start/stop, break
map <- map %>%
  addAwesomeMarkers(
    lat = bomen_info$gpslatitude, 
    lng = bomen_info$gpslongitude, 
    popup = bomen_info$popup, 
    icon=tree_icon, 
    group = "Markeringen")

map <- map %>%
  addAwesomeMarkers(
    lat = 50.8811744, 
    lng = 4.328147, 
    popup = "<p><b>Start- en eindpunt</b> van de bomenwandeling in de Molenbeekvallei is op de parking van het station van Jette (achterkant).<p>",
    icon=flag_icon, 
    group = "Markeringen")

map <- map %>%
  addAwesomeMarkers(
    lat = 50.8846628, 
    lng = 4.3009835, 
    popup = "<p><b>Chalet van Laarbeek</b> ongeveer halverwege de wandeling: mogelijkheid voor drinkpauze.<p>",
    icon=beer_icon, 
    group = "Markeringen")


# Add a legend/description in the bottom right
map <- map %>% 
  addLegend(position = 'bottomright',opacity = 0.4, 
            colors = c('blue', 'red', 'orange'), 
            labels = c('Station Jette - Koning Boudewijnpark - Laarbeekbos', 'Start- en eindpunt', 'Pauze: Chalet van Laarbeek'),
            title = 'Bomenwandeling in de Molenbeekvallei  (5,5km)')

# export map to selfcontained HTML-file
saveWidget(map, here::here('output/bomenwandeling_jette.html'), selfcontained = TRUE)

rm(bomen_imgs, bomen_imgs_metadata, bomen_info,
   gpx.track.deel1, gpx.track.deel2, flag_icon, tree_icon, map, beer_icon)
