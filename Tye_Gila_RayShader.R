###############################################################
# 2020_Gila_Rayshader
# 20200110
# SPT
###############################################################

rm(list=ls())

library(sf)
library(leaflet)
library(lubridate)
library(viridis)
library(RColorBrewer)
library(htmlwidgets)
library(htmltools)
library(stringr)
library(mapview)
library(elevatr)
library(dplyr)
#devtools::install_github("tylermorganwall/rayshader")
library(rayshader)
library(remotes)
#remotes::install_github("vinayudyawer/KUD3D")
library(KUD3D)
library(sf)
library(tidyverse)
library(osmdata)
library(magick)
library(raster)

setwd("/Users/simontye/Documents/Research/Projects/Gila_River/2020_Gila")

###############################################################
### Functions

# Download USGS elevation data
get_usgs_elevation_data <- function(bbox, size = "400,400", file = NULL, 
                                    sr_bbox = 4326, sr_image = 4326) {
  require(httr)
  
  # TODO - validate inputs
  
  url <- parse_url("https://elevation.nationalmap.gov/arcgis/rest/services/3DEPElevation/ImageServer/exportImage")
  res <- GET(
    url, 
    query = list(
      bbox = paste(bbox$p1$long, bbox$p1$lat, bbox$p2$long, bbox$p2$lat,
                   sep = ","),
      bboxSR = sr_bbox,
      imageSR = sr_image,
      size = size,
      format = "tiff",
      pixelType = "F32",
      noDataInterpretation = "esriNoDataMatchAny",
      interpolation = "+RSP_BilinearInterpolation",
      f = "json"
    )
  )
  
  if (status_code(res) == 200) {
    body <- content(res, type = "application/json")
    # TODO - check that bbox values are correct
    # message(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE))
    
    img_res <- GET(body$href)
    img_bin <- content(img_res, "raw")
    if (is.null(file)) 
      file <- tempfile("elev_matrix", fileext = ".tif")
    writeBin(img_bin, file)
    message(paste("image saved to file:", file))
  } else {
    warning(res)
  }
  invisible(file)
}
 # Read elevation data
read_elevation_file <- function(file) {
  elev_img <- raster::raster(file)
  elev_matrix <- matrix(
    raster::extract(elev_img, raster::extent(elev_img), buffer = 1000), 
    nrow = ncol(elev_img), ncol = nrow(elev_img)
  )
  elev_matrix
}

# Downloading a map from ArcGIS API
get_arcgis_map_image <- function(bbox, map_type = "World_Imagery", file = NULL, 
                                 width = 400, height = 400, sr_bbox = 4326) {
  require(httr)
  require(glue) 
  require(jsonlite)
  
  url <- parse_url("https://utility.arcgisonline.com/arcgis/rest/services/Utilities/PrintingTools/GPServer/Export%20Web%20Map%20Task/execute")
  
  # define JSON query parameter
  web_map_param <- list(
    baseMap = list(
      baseMapLayers = list(
        list(url = jsonlite::unbox(glue("https://services.arcgisonline.com/ArcGIS/rest/services/{map_type}/MapServer",
                                        map_type = map_type)))
      )
    ),
    exportOptions = list(
      outputSize = c(width, height)
    ),
    mapOptions = list(
      extent = list(
        spatialReference = list(wkid = jsonlite::unbox(sr_bbox)),
        xmax = jsonlite::unbox(max(bbox$p1$long, bbox$p2$long)),
        xmin = jsonlite::unbox(min(bbox$p1$long, bbox$p2$long)),
        ymax = jsonlite::unbox(max(bbox$p1$lat, bbox$p2$lat)),
        ymin = jsonlite::unbox(min(bbox$p1$lat, bbox$p2$lat))
      )
    )
  )
  
  res <- GET(
    url, 
    query = list(
      f = "json",
      Format = "PNG32",
      Layout_Template = "MAP_ONLY",
      Web_Map_as_JSON = jsonlite::toJSON(web_map_param))
  )
  
  if (status_code(res) == 200) {
    body <- content(res, type = "application/json")
    message(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE))
    if (is.null(file)) 
      file <- tempfile("overlay_img", fileext = ".png")
    
    img_res <- GET(body$results[[1]]$value$url)
    img_bin <- content(img_res, "raw")
    writeBin(img_bin, file)
    message(paste("image saved to file:", file))
  } else {
    message(res)
  }
  invisible(file)
}

###############################################################

# Define image size variables from bounding box coordinates
define_image_size <- function(bbox, major_dim = 400) {
  # calculate aspect ration (width/height) from lat/long bounding box
  aspect_ratio <- abs((bbox$p1$long - bbox$p2$long) / (bbox$p1$lat - bbox$p2$lat))
  # define dimensions
  img_width <- ifelse(aspect_ratio > 1, major_dim, major_dim*aspect_ratio) %>% round()
  img_height <- ifelse(aspect_ratio < 1, major_dim, major_dim/aspect_ratio) %>% round()
  size_str <- paste(img_width, img_height, sep = ",")
  list(height = img_height, width = img_width, size = size_str)
}

###############################################################

# Build a GIF of 3D Rayshader plots
save_3d_gif <- function(hillshade, heightmap, file, duration = 5, ...) {
  require(rayshader)
  require(magick)
  require(rgl)
  require(gifski)
  require(rlang)
  
  # capture dot arguments and extract variables with length > 1 for gif frames
  dots <- rlang::list2(...)
  var_exception_list <- c("windowsize")
  dot_var_lengths <- purrr::map_int(dots, length)
  gif_var_names <- names(dots)[dot_var_lengths > 1 & 
                                 !(names(dots) %in% var_exception_list)]
  # split off dot variables to use on gif frames
  gif_dots <- dots[gif_var_names]
  static_dots <- dots[!(names(dots) %in% gif_var_names)]
  gif_var_lengths <- purrr::map_int(gif_dots, length)
  # build expressions for gif variables that include index 'i' (to use in the for loop)
  gif_expr_list <- purrr::map(names(gif_dots), ~rlang::expr(gif_dots[[!!.x]][i]))
  gif_exprs <- exprs(!!!gif_expr_list)
  names(gif_exprs) <- names(gif_dots)
  message(paste("gif variables found:", paste(names(gif_dots), collapse = ", ")))
  
  # TODO - can we recycle short vectors?
  if (length(unique(gif_var_lengths)) > 1) 
    stop("all gif input vectors must be the same length")
  n_frames <- unique(gif_var_lengths)
  
  # generate temp .png images
  temp_dir <- tempdir()
  img_frames <- file.path(temp_dir, paste0("frame-", seq_len(n_frames), ".png"))
  on.exit(unlink(img_frames))
  message(paste("Generating", n_frames, "temporary .png images..."))
  for (i in seq_len(n_frames)) {
    message(paste(" - image", i, "of", n_frames))
    rgl::clear3d()
    hillshade %>%
      plot_3d_tidy_eval(heightmap, !!!append(gif_exprs, static_dots))
    rgl::snapshot3d(img_frames[i])
  }
  
  # build gif
  message("Generating .gif...")
  magick::image_write_gif(magick::image_read(img_frames), 
                          path = file, delay = duration/n_frames)
  message("Done!")
  invisible(file)
}


plot_3d_tidy_eval <- function(hillshade, ...) {
  dots <- rlang::enquos(...)
  plot_3d_call <- rlang::expr(plot_3d(hillshade, !!!dots))
  rlang::eval_tidy(plot_3d_call)
}

###############################################################

# Create a numeric vector of transition values
transition_values <- function(from, to, steps = 10, 
                              one_way = FALSE, type = "cos") {
  if (!(type %in% c("cos", "lin")))
    stop("type must be one of: 'cos', 'lin'")
  
  range <- c(from, to)
  middle <- mean(range)
  half_width <- diff(range)/2
  
  # define scaling vector starting at 1 (between 1 to -1)
  if (type == "cos") {
    scaling <- cos(seq(0, 2*pi / ifelse(one_way, 2, 1), length.out = steps))
  } else if (type == "lin") {
    if (one_way) {
      xout <- seq(1, -1, length.out = steps)
    } else {
      xout <- c(seq(1, -1, length.out = floor(steps/2)), 
                seq(-1, 1, length.out = ceiling(steps/2)))
    }
    scaling <- approx(x = c(-1, 1), y = c(-1, 1), xout = xout)$y 
  }
  
  middle - half_width * scaling
}

###############################################################
### Rayshdaer - Test
###############################################################

# Load geotiff file
loadzip = tempfile() 
Gila = raster::raster("Gila_2016.tif")
unlink(loadzip)

# Convert geotiff to raster
Gila.raster <- raster("Gila_2016.tif")

# Resample raster file to smaller resolution
Gila.raster2 <- aggregate(Gila.raster, fact = 50)

# Convert raster to matrix for further plotting
Gila.map = raster_to_matrix(Gila.raster2)

# Create sample 2D plot
Gila.map %>%
  sphere_shade(texture = "desert") %>%
  add_shadow(ray_shade(Gila.map)) %>%
  add_shadow(ambient_shade(Gila.map)) %>%
  plot_map()
#Sys.sleep(0.2)
#render_snapshot(clear=TRUE)

###############################################################
### Rayshader - Full version
###############################################################

# Define bounding box
bbox <- list(
  p1 = list(long = -108.631, lat = 32.898), # bottom left
  p2 = list(long = -108.469, lat = 33.077) # top right
)

# Display bounding box
leaflet() %>%
  addTiles() %>% 
  addRectangles(
    lng1 = bbox$p1$long, lat1 = bbox$p1$lat,
    lng2 = bbox$p2$long, lat2 = bbox$p2$lat,
    fillColor = "transparent"
  ) %>%
  fitBounds(
    lng1 = bbox$p1$long, lat1 = bbox$p1$lat,
    lng2 = bbox$p2$long, lat2 = bbox$p2$lat,
  )
# Define image size
image_size <- define_image_size(bbox, major_dim = 400)

# Download elevation data
elev_file <- file.path("sf-elevation.tif")
get_usgs_elevation_data(bbox, size = image_size$size, file = elev_file,
                        sr_bbox = 4326, sr_image = 4326)

# Load elevation data
elev_img <- raster(elev_file)
elev_matrix <- matrix(
  raster::extract(elev_img, raster::extent(elev_img), buffer = 1000), 
  nrow = ncol(elev_img), ncol = nrow(elev_img)
)

# Define shadow layers
ambmat <- ambient_shade(elev_matrix, zscale = 30)
raymat <- ray_shade(elev_matrix, zscale = 30, lambert = TRUE)

# Overlay satellite image
overlay_file <- "terrain.png"
get_arcgis_map_image(bbox, map_type = "World_Imagery", file = overlay_file,
                     width = image_size$width, height = image_size$height, 
                     sr_bbox = 4326)
overlay_img <- png::readPNG(overlay_file)

###############################################################

# Read shapefile
#Gila.shape <- st_read("Gila_Rivers.shp")
#Gila.shape2 <-raster(Gila.shape)

# Import river channels
river.1935 = rgdal::readOGR("1935_water_final.shp")
river.1950 = rgdal::readOGR("1950_water.shp")
river.1965 = rgdal::readOGR("1965_water_final.shp")
river.1974 = rgdal::readOGR("1974_water_final.shp")
#river.1980 = rgdal::readOGR("1980_water.shp")
river.1982 = rgdal::readOGR("1982_water_final.shp")
river.1986 = rgdal::readOGR("1986_water_final.shp")
river.2006 = rgdal::readOGR("2006_water_final.shp")
river.2009 = rgdal::readOGR("2009_water_final.shp")
river.2014 = rgdal::readOGR("2014_water_final.shp")
river.2016 = rgdal::readOGR("2016_water_final.shp")

# Convert to SpatialLines
river.1935 = as(river.1935, "SpatialLines")
river.1950 = as(river.1950, "SpatialLines")
river.1965 = as(river.1965, "SpatialLines")
river.1974 = as(river.1974, "SpatialLines")
#river.1980 = as(river.1980, "SpatialLines")
river.1982 = as(river.1982, "SpatialLines")
river.1986 = as(river.1986, "SpatialLines")
river.2006 = as(river.2006, "SpatialLines")
river.2009 = as(river.2009, "SpatialLines")
river.2014 = as(river.2014, "SpatialLines")
river.2016 = as(river.2016, "SpatialLines")

# Reproject coordinate system
river.1935 <- spTransform(river.1935, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
river.1950 <- spTransform(river.1950, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
river.1965 <- spTransform(river.1965, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
river.1974 <- spTransform(river.1974, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#river.1980 <- spTransform(river.1980, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
river.1982 <- spTransform(river.1982, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
river.1986 <- spTransform(river.1986, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
river.2006 <- spTransform(river.2006, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
river.2009 <- spTransform(river.2009, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
river.2014 <- spTransform(river.2014, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
river.2016 <- spTransform(river.2016, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Create empty raster
r         = raster()
extent(r) = extent(elev_img)
dim(r)    = dim(elev_img)
res(r)    = res(elev_img)
crs(r)    = crs(elev_img)
values(r) = 0

# Resample raster file to lower resolution
#river2 <- aggregate(river, fact = 10)
#r2 <- aggregate(r, fact = 10)

# Combine 1935 channel with empty raster
r.1935 = rasterize(river.1935, r)
values(r.1935)[is.na(values(r.1935))] = 0
values(r.1935)[values(r.1935) != 0] = 1
water.1935 = matrix(raster::extract(r.1935,raster::extent(r.1935),buffer = 1000), nrow = ncol(r.1935),ncol = nrow(r.1935))
water.1935 = apply(water.1935, 2, rev)

# Combine 1950 channel with empty raster
r.1950 = rasterize(river.1950, r)
values(r.1950)[is.na(values(r.1950))] = 0
values(r.1950)[values(r.1950) != 0] = 1
water.1950 = matrix(raster::extract(r.1950,raster::extent(r.1950),buffer = 1000), nrow = ncol(r.1950),ncol = nrow(r.1950))
water.1950 = apply(water.1950, 2, rev)

# Combine 1965 channel with empty raster
r.1965 = rasterize(river.1965, r)
values(r.1965)[is.na(values(r.1965))] = 0
values(r.1965)[values(r.1965) != 0] = 1
water.1965 = matrix(raster::extract(r.1965,raster::extent(r.1965),buffer = 1000), nrow = ncol(r.1965),ncol = nrow(r.1965))
water.1965 = apply(water.1965, 2, rev)

# Combine 1974 channel with empty raster
r.1974 = rasterize(river.1974, r)
values(r.1974)[is.na(values(r.1974))] = 0
values(r.1974)[values(r.1974) != 0] = 1
water.1974 = matrix(raster::extract(r.1974,raster::extent(r.1974),buffer = 1000), nrow = ncol(r.1974),ncol = nrow(r.1974))
water.1974 = apply(water.1974, 2, rev)

# Combine 1980 channel with empty raster
# r.1980 = rasterize(river.1980, r)
# values(r.1980)[is.na(values(r.1980))] = 0
# values(r.1980)[values(r.1980) != 0] = 1
# water.1980 = matrix(raster::extract(r.1980,raster::extent(r.1980),buffer = 1000), nrow = ncol(r.1980),ncol = nrow(r.1980))
# water.1980 = apply(water.1980, 2, rev)

# Combine 1982 channel with empty raster
r.1982 = rasterize(river.1982, r)
values(r.1982)[is.na(values(r.1982))] = 0
values(r.1982)[values(r.1982) != 0] = 1
water.1982 = matrix(raster::extract(r.1982,raster::extent(r.1982),buffer = 1000), nrow = ncol(r.1982),ncol = nrow(r.1982))
water.1982 = apply(water.1982, 2, rev)

# Combine 1986 channel with empty raster
r.1986 = rasterize(river.1986, r)
values(r.1986)[is.na(values(r.1986))] = 0
values(r.1986)[values(r.1986) != 0] = 1
water.1986 = matrix(raster::extract(r.1986,raster::extent(r.1986),buffer = 1000), nrow = ncol(r.1986),ncol = nrow(r.1986))
water.1986 = apply(water.1986, 2, rev)

# Combine 2006 channel with empty raster
r.2006 = rasterize(river.2006, r)
values(r.2006)[is.na(values(r.2006))] = 0
values(r.2006)[values(r.2006) != 0] = 1
water.2006 = matrix(raster::extract(r.2006,raster::extent(r.2006),buffer = 1000), nrow = ncol(r.2006),ncol = nrow(r.2006))
water.2006 = apply(water.2006, 2, rev)

# Combine 2009 channel with empty raster
r.2009 = rasterize(river.2009, r)
values(r.2009)[is.na(values(r.2009))] = 0
values(r.2009)[values(r.2009) != 0] = 1
water.2009 = matrix(raster::extract(r.2009,raster::extent(r.2009),buffer = 1000), nrow = ncol(r.2009),ncol = nrow(r.2009))
water.2009 = apply(water.2009, 2, rev)

# Combine 2014 channel with empty raster
r.2014 = rasterize(river.2014, r)
values(r.2014)[is.na(values(r.2014))] = 0
values(r.2014)[values(r.2014) != 0] = 1
water.2014 = matrix(raster::extract(r.2014,raster::extent(r.2014),buffer = 1000), nrow = ncol(r.2014),ncol = nrow(r.2014))
water.2014 = apply(water.2014, 2, rev)

# Combine 2016 channel with empty raster
r.2016 = rasterize(river.2016, r)
values(r.2016)[is.na(values(r.2016))] = 0
values(r.2016)[values(r.2016) != 0] = 1
water.2016 = matrix(raster::extract(r.2016,raster::extent(r.2016),buffer = 1000), nrow = ncol(r.2016),ncol = nrow(r.2016))
water.2016 = apply(water.2016, 2, rev)

# Check dimensions
#dim(water) == dim(elev_matrix)

# 2D plot with terrain and river
elev_matrix %>%
  sphere_shade(texture = "desert") %>%
  add_water(water.1935, color = "blue") %>%
  add_water(water.1950, color = "red") %>%
  add_water(water.1965, color = "green") %>%
  add_water(water.1974, color = "orange") %>%
  #add_water(water.1980, color = "blueviolet") %>%
  add_water(water.1982, color = "darkgoldenrod") %>%
  add_water(water.1986, color = "darkcyan") %>%
  add_water(water.1986, color = "coral") %>%
  add_water(water.2006, color = "darkred") %>%
  add_water(water.2009, color = "darkorchid") %>%
  add_water(water.2014, color = "dodgerblue") %>%
  add_water(water.2016, color = "darkslategray") %>%
  add_shadow(raymat, max_darken = 0.5) %>%
  add_shadow(ambmat, max_darken = 0.5) %>%
  add_overlay(overlay_img, alphalayer = 0.5) %>%
  plot_3d(elev_matrix, theta =-30)

# 3D plot with terrain and river
zscale <- 10
rgl::clear3d()
elev_matrix %>% 
  sphere_shade(texture = "desert") %>% 
  add_water(water, color = "lightblue") %>%
  add_overlay(overlay_img, alphalayer = 0.5) %>%
  add_shadow(raymat, max_darken = 0.5) %>%
  add_shadow(ambmat, max_darken = 0.5) %>%
  plot_3d(elev_matrix, zscale = zscale, windowsize = c(1200, 1000),
          water = TRUE, soliddepth = -max(elev_matrix)/zscale, wateralpha = 0,
          theta = -30, phi = 30, zoom = 0.3, fov = 60)
render_snapshot()

###############################################################
### GIF


n_frames <- 5
theta <- transition_values(from = -30, to = -30, steps = n_frames, 
                           one_way = TRUE, type = "lin")
phi <- transition_values(from = 30, to = 30, steps = n_frames, 
                         one_way = FALSE, type = "cos")
zoom <- transition_values(from = 0.3, to = 0.3, steps = n_frames, 
                          one_way = FALSE, type = "cos")


zscale <- 10
elev_matrix %>% 
  sphere_shade(texture = "desert") %>% 
  add_water(water, color = "lightblue") %>%
  add_overlay(overlay_img, alphalayer = 0.5) %>%
  add_shadow(raymat, 0.4) %>%
  add_shadow(ambmat, 0.4) %>%
  save_3d_gif(elev_matrix, file = "gila.gif", duration = 6,
              zscale = zscale, windowsize = c(1200, 1000), wateralpha = 0,
              water = TRUE, soliddepth = -max(elev_matrix)/1000, 
              theta = theta, phi = phi, zoom = zoom, fov = 60)
 # montery water gif ====
#elev_matrix <- montereybay
n_frames <- 180
zscale <- 50

# frame transition variables
#waterdepthvalues <- min(elev_matrix)/2 - min(elev_matrix)/2 * cos(seq(0,2*pi,length.out = n_frames))
thetavalues <- -90 + 45 * cos(seq(0, 2*pi, length.out = n_frames))
# shadow layers
ambmat <- ambient_shade(elev_matrix, zscale = zscale)
raymat <- ray_shade(elev_matrix, zscale = zscale, lambert = TRUE)

# generate .png frame images
img_frames <- paste0("drain", seq_len(n_frames), ".png")
for (i in seq_len(frames)) {
  message(paste(" - image", i, "of", n_frames))
  elev_matrix %>%
    sphere_shade(texture = "imhof1") %>%
    add_shadow(ambmat, 0.5) %>%
    add_shadow(raymat, 0.5) %>%
    plot_3d(elev_matrix, solid = TRUE, shadow = TRUE, zscale = zscale, 
            water = TRUE, watercolor = "lightblue", wateralpha = 0.8, 
            waterlinecolor = "lightblue", waterlinealpha = 0.5,
            theta = thetavalues[i], phi = 45)
  render_snapshot(img_frames[i])
  rgl::clear3d()
}

# build gif
magick::image_write_gif(magick::image_read(img_frames), 
                        path = "montereybay.gif", 
                        delay = 6/n_frames)

# calculate input vectors for gif frames
n_frames <- 180
waterdepths <- transition_values(from = 0, to = min(montereybay), steps = n_frames) 
thetas <- transition_values(from = -45, to = -135, steps = n_frames)
# generate gif
zscale <- 50
montereybay %>% 
  sphere_shade(texture = "imhof1", zscale = zscale) %>%
  add_shadow(ambient_shade(montereybay, zscale = zscale), 0.5) %>%
  add_shadow(ray_shade(montereybay, zscale = zscale, lambert = TRUE), 0.5) %>%
  save_3d_gif(montereybay, file = "montereybay.gif", duration = 6,
              solid = TRUE, shadow = TRUE, water = TRUE, zscale = zscale,
              watercolor = "imhof3", wateralpha = 0.8, 
              waterlinecolor = "#ffffff", waterlinealpha = 0.5,
              waterdepth = waterdepths/zscale, 
              theta = thetas, phi = 45)




