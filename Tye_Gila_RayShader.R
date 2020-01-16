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

# Download USGS elevation data (bbox_1)
get_usgs_elevation_data_1 <- function(bbox_1, size = "800,800", file = NULL, 
                                    sr_bbox = 4326, sr_image = 4326) {
  require(httr)
  
  url <- parse_url("https://elevation.nationalmap.gov/arcgis/rest/services/3DEPElevation/ImageServer/exportImage")
  res <- GET(
    url, 
    query = list(
      bbox_1 = paste(bbox_1$p1$long, bbox_1$p1$lat, bbox_1$p2$long, bbox_1$p2$lat,
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
      file <- tempfile("elev_1_matrix", fileext = ".tif")
    writeBin(img_bin, file)
    message(paste("image saved to file:", file))
  } else {
    warning(res)
  }
  invisible(file)
}
 # Read elevation data (bbox_1)
read_elevation_file_1 <- function(file) {
  elev_1_img <- raster::raster(file)
  elev_1_matrix <- matrix(
    raster::extract(elev_1_img, raster::extent(elev_1_img), buffer = 1000), 
    nrow = ncol(elev_1_img), ncol = nrow(elev_1_img)
  )
  elev_1_matrix
}

# Download USGS elevation data (bbox_2)
get_usgs_elevation_data_2 <- function(bbox_2, size = "800,800", file = NULL, 
                                      sr_bbox = 4326, sr_image = 4326) {
  require(httr)
  
  url <- parse_url("https://elevation.nationalmap.gov/arcgis/rest/services/3DEPElevation/ImageServer/exportImage")
  res <- GET(
    url, 
    query = list(
      bbox_2 = paste(bbox_2$p1$long, bbox_2$p1$lat, bbox_2$p2$long, bbox_2$p2$lat,
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
      file <- tempfile("elev_2_matrix", fileext = ".tif")
    writeBin(img_bin, file)
    message(paste("image saved to file:", file))
  } else {
    warning(res)
  }
  invisible(file)
}
# Read elevation data (bbox_2)
read_elevation_file_2 <- function(file) {
  elev_2_img <- raster::raster(file)
  elev_2_matrix <- matrix(
    raster::extract(elev_2_img, raster::extent(elev_2_img), buffer = 1000), 
    nrow = ncol(elev_2_img), ncol = nrow(elev_2_img)
  )
  elev_2_matrix
}

# Download a map from ArcGIS API (bbox_1)
get_arcgis_map_image_1 <- function(bbox_1, map_type = "World_Imagery", file = NULL, 
                                 width = 800, height = 800, sr_bbox = 4326) {
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
        xmax = jsonlite::unbox(max(bbox_1$p1$long, bbox_1$p2$long)),
        xmin = jsonlite::unbox(min(bbox_1$p1$long, bbox_1$p2$long)),
        ymax = jsonlite::unbox(max(bbox_1$p1$lat, bbox_1$p2$lat)),
        ymin = jsonlite::unbox(min(bbox_1$p1$lat, bbox_1$p2$lat))
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
      file <- tempfile("overlay_1_img", fileext = ".png")
    
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

# Download a map from ArcGIS API (bbox_2)
get_arcgis_map_image_2 <- function(bbox_2, map_type = "World_Imagery", file = NULL, 
                                   width = 800, height = 800, sr_bbox = 4326) {
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
        xmax = jsonlite::unbox(max(bbox_2$p1$long, bbox_2$p2$long)),
        xmin = jsonlite::unbox(min(bbox_2$p1$long, bbox_2$p2$long)),
        ymax = jsonlite::unbox(max(bbox_2$p1$lat, bbox_2$p2$lat)),
        ymin = jsonlite::unbox(min(bbox_2$p1$lat, bbox_2$p2$lat))
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
      file <- tempfile("overlay_2_img", fileext = ".png")
    
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

# Define image size variables from bounding box coordinates (bbox_1)
define_image_1_size <- function(bbox_1, major_dim = 800) {
  # calculate aspect ration (width/height) from lat/long bounding box
  aspect_ratio <- abs((bbox_1$p1$long - bbox_1$p2$long) / (bbox_1$p1$lat - bbox_1$p2$lat))
  # define dimensions
  img_width <- ifelse(aspect_ratio > 1, major_dim, major_dim*aspect_ratio) %>% round()
  img_height <- ifelse(aspect_ratio < 1, major_dim, major_dim/aspect_ratio) %>% round()
  size_str <- paste(img_width, img_height, sep = ",")
  list(height = img_height, width = img_width, size = size_str)
}

###############################################################

# Define image size variables from bounding box coordinates (bbox_2)
define_image_2_size <- function(bbox_2, major_dim = 800) {
  # calculate aspect ration (width/height) from lat/long bounding box
  aspect_ratio <- abs((bbox_2$p1$long - bbox_2$p2$long) / (bbox_2$p1$lat - bbox_2$p2$lat))
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
  magick::image_write_gif(magick::image_read(img_frames), path = file, delay = duration/n_frames)
  message("You did the thing!")
  invisible(file)
}


plot_3d_tidy_eval <- function(hillshade, ...) {
  dots <- rlang::enquos(...)
  plot_3d_call <- rlang::expr(plot_3d(hillshade, !!!dots))
  rlang::eval_tidy(plot_3d_call)
}

###############################################################

# Create a numeric vector of transition values
transition_values <- function(from, to, steps = 10, one_way = FALSE, type = "cos") {
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
#
## Load geotiff file
#loadzip = tempfile() 
#Gila = raster::raster("Gila_2016.tif")
#unlink(loadzip)
#
## Convert geotiff to raster
#Gila.raster <- raster("Gila_2016.tif")
#
## Resample raster file to smaller resolution
#Gila.raster2 <- aggregate(Gila.raster, fact = 50)
#
## Convert raster to matrix for further plotting
#Gila.map = raster_to_matrix(Gila.raster2)
#
## Create sample 2D plot
#Gila.map %>%
#  sphere_shade(texture = "desert") %>%
#  add_shadow(ray_shade(Gila.map)) %>%
#  add_shadow(ambient_shade(Gila.map)) %>%
#  plot_map()
##Sys.sleep(0.2)
##render_snapshot(clear=TRUE)
#
###############################################################
### Rayshader - Full version
###############################################################

### Channels
## Define bounding box (Channels)
#bbox <- list(
#  p1 = list(long = -108.627, lat = 32.923), # bottom left
#  p2 = list(long = -108.514, lat = 33.071) # top right
#)

### Video: Intro
# Define bounding box (Video: Intro)
bbox_1 <- list(
  p1 = list(long = -109.434, lat = 32.157), # bottom left
  p2 = list(long = -107.604, lat = 33.957) # top right
)

#### Center of -108.519, 33.057

# Display bounding box
leaflet() %>%
  addTiles() %>% 
  addRectangles(
    lng1 = bbox_1$p1$long, lat1 = bbox_1$p1$lat,
    lng2 = bbox_1$p2$long, lat2 = bbox_1$p2$lat,
    fillColor = "transparent"
  ) %>%
  fitBounds(
    lng1 = bbox_1$p1$long, lat1 = bbox_1$p1$lat,
    lng2 = bbox_1$p2$long, lat2 = bbox_1$p2$lat,
  )

# Define image size
image_1_size <- define_image_1_size(bbox_1, major_dim = 800)

# Download elevation data
elev_1 <- file.path("bbox_1.tif")
get_usgs_elevation_data_1(bbox_1,
                        size = image_1_size$size,
                        file = elev_1,
                        sr_bbox = 4326,
                        sr_image = 4326)

# Load elevation data
elev_1_img    <- raster(elev_1)
elev_1_matrix <- matrix(
  raster::extract(elev_1_img, raster::extent(elev_1_img), buffer = 1000), 
  nrow = ncol(elev_1_img), ncol = nrow(elev_1_img)
)

# Define shadow layers
ambmat_1 <- ambient_shade(elev_1_matrix, zscale = 30)
raymat_1 <- ray_shade(elev_1_matrix, zscale = 30, lambert = TRUE)

# Overlay satellite image
overlay_1 <- "bbox_1.png"
get_arcgis_map_image(bbox_1,
                     map_type = "World_Imagery",
                     file = overlay_1,
                     width = image_1_size$width,
                     height = image_1_size$height, 
                     sr_bbox = 4326)
overlay_1_img <- png::readPNG(overlay_1)

### Video: Outro
# Define bounding box (Video)
bbox_2 <- list(
  p1 = list(long = -108.824, lat = 32.757), # bottom left
  p2 = list(long = -108.214, lat = 33.357) # top right
)

# Display bounding box
leaflet() %>%
  addTiles() %>% 
  addRectangles(
    lng1 = bbox_2$p1$long, lat1 = bbox_2$p1$lat,
    lng2 = bbox_2$p2$long, lat2 = bbox_2$p2$lat,
    fillColor = "transparent"
  ) %>%
  fitBounds(
    lng1 = bbox_2$p1$long, lat1 = bbox_2$p1$lat,
    lng2 = bbox_2$p2$long, lat2 = bbox_2$p2$lat,
  )

# Define image size
image_2_size <- define_image_2_size(bbox_2, major_dim = 800)

# Download elevation data
elev_2 <- file.path("bbox_2.tif")
get_usgs_elevation_data_2(bbox_2,
                        size = image_2_size$size,
                        file = elev_2,
                        sr_bbox = 4326, sr_image = 4326)

# Load elevation data
elev_2_img <- raster(elev_2)
elev_2_matrix <- matrix(
  raster::extract(elev_2_img, raster::extent(elev_2_img), buffer = 1000), 
  nrow = ncol(elev_2_img), ncol = nrow(elev_2_img)
)

# Define shadow layers
ambmat_2 <- ambient_shade(elev_2_matrix, zscale = 30)
raymat_2 <- ray_shade(elev_2_matrix, zscale = 30, lambert = TRUE)

# Overlay satellite image
overlay_2 <- "terrain.png"
get_arcgis_map_image(bbox_2,
                     map_type = "World_Imagery",
                     file = overlay_2,
                     width = image_2_size$width,
                     height = image_2_size$height, 
                     sr_bbox = 4326)
overlay_2_img <- png::readPNG(overlay_2)

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
extent(r) = extent(elev_2_img)
dim(r)    = dim(elev_2_img)
res(r)    = res(elev_2_img)
crs(r)    = crs(elev_2_img)
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
#elev_matrix %>%
#  sphere_shade(texture = "desert") %>%
#  add_overlay(overlay_img, alphalayer = 0.8) %>%
#  add_water(water.1935, color = "#5e4fa2") %>%
#  add_water(water.1950, color = "#3288bd") %>%
#  add_water(water.1965, color = "#66c2a5") %>%
#  add_water(water.1974, color = "#abdda4") %>%
#  #add_water(water.1980, color = "blueviolet") %>%
#  add_water(water.1982, color = "#e6f598") %>%
#  add_water(water.1986, color = "#ffffbf") %>%
#  add_water(water.1986, color = "#fee08b") %>%
#  add_water(water.2006, color = "#fdae61") %>%
#  add_water(water.2009, color = "#f46d43") %>%
#  add_water(water.2014, color = "#d53e4f") %>%
#  add_water(water.2016, color = "#9e0142") %>%
#  add_shadow(raymat, max_darken = 0.5) %>%
#  add_shadow(ambmat, max_darken = 0.5) %>%
#  plot_map()

#rgl::clear3d()

###############################################################
### Interactive map

### Intro GIF
# Settings
n_frames <- 5
theta <- transition_values(from = 108, to = 328, steps = n_frames, 
                           one_way = TRUE, type = "lin")
phi <- transition_values(from = 15, to = 50, steps = n_frames, 
                         one_way = TRUE, type = "cos")
zoom <- transition_values(from = 0.16, to = 0.04, steps = n_frames, 
                          one_way = TRUE, type = "cos")

# GIF
elev_1_matrix %>%
  sphere_shade(texture = "desert") %>%
  add_overlay(overlay_1_img, alphalayer = 0.9) %>%
  #add_water(water.1935, color = "#5e4fa2") %>%
  #add_water(water.1950, color = "#3288bd") %>%
  #add_water(water.1965, color = "#66c2a5") %>%
  #add_water(water.1974, color = "#abdda4") %>%
  #add_water(water.1982, color = "#e6f598") %>%
  #add_water(water.1986, color = "#ffffbf") %>%
  #add_water(water.1986, color = "#fee08b") %>%
  #add_water(water.2006, color = "#fdae61") %>%
  #add_water(water.2009, color = "#f46d43") %>%
  #add_water(water.2014, color = "#d53e4f") %>%
  add_water(water.2016, color = "#3288bd") %>%
  add_shadow(raymat_1, max_darken = 0.6) %>%
  add_shadow(ambmat_2, max_darken = 0.6) %>%
  save_3d_gif(elev_1_matrix,
              file = "Gila_Rayshader_1.gif",
              duration = 15,
              zscale = 30,
              windowsize = c(1000, 1000),
              water = FALSE,
              soliddepth = -max(elev_1_matrix)/1000, 
              theta = theta,
              phi = phi,
              zoom = zoom,
              fov = 100)

### Outro GIF (bbox_2)
# Settings
n_frames <- 5
theta <- transition_values(from = 328, to = 313, steps = n_frames, 
                           one_way = TRUE, type = "lin")
phi <- transition_values(from = 50, to = 65, steps = n_frames, 
                         one_way = TRUE, type = "cos")
zoom <- transition_values(from = 0.04, to = 0.14, steps = n_frames, 
                          one_way = TRUE, type = "cos")

theta <- transition_values(from = 328, to = 313, steps = n_frames, 
                           one_way = TRUE, type = "lin")
phi <- transition_values(from = 50, to = 10, steps = n_frames, 
                         one_way = TRUE, type = "cos")
zoom <- transition_values(from = 0.04, to = 0.10, steps = n_frames, 
                          one_way = TRUE, type = "cos")
# GIF
elev_2_matrix %>%
  sphere_shade(texture = "desert") %>%
  add_overlay(overlay_2_img, alphalayer = 0.9) %>%
  add_water(water.1935, color = "#5e4fa2") %>%
  add_water(water.1950, color = "#3288bd") %>%
  add_water(water.1965, color = "#66c2a5") %>%
  add_water(water.1974, color = "#abdda4") %>%
  add_water(water.1982, color = "#e6f598") %>%
  add_water(water.1986, color = "#ffffbf") %>%
  add_water(water.1986, color = "#fee08b") %>%
  add_water(water.2006, color = "#fdae61") %>%
  add_water(water.2009, color = "#f46d43") %>%
  add_water(water.2014, color = "#d53e4f") %>%
  add_water(water.2016, color = "#9e0142") %>%
  add_shadow(raymat_2, max_darken = 0.6) %>%
  add_shadow(ambmat_2, max_darken = 0.6) %>%
  save_3d_gif(elev_2_matrix,
              file = "Gila_Rayshader_2.gif",
              duration = 15,
              zscale = 30,
              windowsize = c(1000, 1000),
              water = FALSE,
              soliddepth = -max(elev_2_matrix)/1000, 
              theta = theta,
              phi = phi,
              zoom = zoom,
              fov = 100)



