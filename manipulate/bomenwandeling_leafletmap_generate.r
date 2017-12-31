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
  'data/20171009_bomenwandeling_jette_bomeninfo.csv', 
  col_types = cols(
    fotonummer = col_character(),
    boom_naam = col_character(),
    lokalisatie_label = col_character(),
    optionele_detailfoto = col_character(),
    lat_manueel = col_double(),
    lon_manueel = col_double()
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
# bomen_info %>% glimpse()
bomen_info %>%
  select(fotonummer, boom_naam, gpslatitude, gpslongitude)

bomen_info <- bomen_info %>%
  mutate(
    gpslatitude = ifelse(is.na(lat_manueel), gpslatitude, lat_manueel),
    gpslongitude = ifelse(is.na(lon_manueel), gpslongitude, lon_manueel))

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

map <- leaflet() %>%
  addProviderTiles("OpenMapSurfer.Roads", group = "Routekaart") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satelliet") %>%
  addLayersControl(
    # baseGroups = c("Topographical", "Routekaart", "Satelliet"),
    baseGroups = c("Routekaart", "Satelliet"),
    overlayGroups = c("Wandelroute", "Markeringen"),
    options = layersControlOptions(collapsed = FALSE))

map <- map %>% 
  addPolylines(data = gpx.track.deel1, group = "Wandelroute") %>%
  addPolylines(data = gpx.track.deel2, group = "Wandelroute")

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

map <- map %>% 
  addLegend(position = 'bottomright',opacity = 0.4, 
            colors = c('blue', 'red', 'orange'), 
            labels = c('Station Jette - Koning Boudewijnpark - Laarbeekbos', 'Start- en eindpunt', 'Pauze: Chalet van Laarbeek'),
            title = 'Bomenwandeling in de Molenbeekvallei  (5,5km)')

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

rm(bomen_imgs, bomen_imgs_metadata, bomen_info,
   gpx.track.deel1, gpx.track.deel2, flag_icon, tree_icon,
   map, beer_icon)
