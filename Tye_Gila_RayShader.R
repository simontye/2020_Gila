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

# Downloading a map from ArcGIS API
get_arcgis_map_image <- function(bbox, map_type = "World_Imagery", file = NULL, 
                                 width = 1000, height = 1000, sr_bbox = 4326) {
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
define_image_size <- function(bbox, major_dim = 1000) {
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
### Rayshdaer - Version 1
###############################################################

# Load geotiff file
loadzip = tempfile() 
Gila = raster::raster("Gila_2016.tif")
unlink(loadzip)

# Convert geotiff to raster
Gila.raster <- raster("Gila_2016.tif")

# Create sample 2D plot of raster file
#plot(Gila, col = rev(terrain.colors(50)))

# Resample raster file to smaller resolution
Gila.raster2 <- aggregate(Gila.raster, fact = 50)
####Gila.raster2 <- aggregate(Gila.raster, fact = 10) Try this?

# Convert raster to matrix for further plotting
Gila.map = raster_to_matrix(Gila.raster2)

# Create sample 3D plot of matrix file
Gila.map %>%
  sphere_shade(texture = "desert") %>%
  add_shadow(ray_shade(Gila.map)) %>%
  add_shadow(ambient_shade(Gila.map)) %>%
  plot_map()

#%>%
#  plot_3d(Gila.map, zscale = 5, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))

#Sys.sleep(0.2)
#render_snapshot(clear=TRUE)

###############################################################
### Rayshader - Version 2
###############################################################

# Define the bounding box
bbox <- list(
  p1 = list(long = -108.631, lat = 32.898), # bottom left
  p2 = list(long = -108.469, lat = 33.077) # top right
)

# Display the bounding box
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
# Define image size for map image files
image_size <- define_image_size(bbox, major_dim = 1000)

# Specify projection
prj <- "+proj=utm +zone=12 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

# Elevation zoom
#elevation_raster_zoom <- 10

# Create bounding box wtih buffer
bbox <- st_bbox(Gila)
bbox["xmin"] <- bbox$xmin - 0.2
bbox["xmax"] <- bbox$xmax + 0.2
bbox["ymin"] <- bbox$ymin - 0.2
bbox["ymax"] <- bbox$ymax + 0.2

# View bounding box with buffer
mapview(bbox)

# Find elevation raster around path  ----
#https://stackoverflow.com/questions/54165356/create-topographic-map-in-r

# Generate a dataframe of lat/long coordinates for get_elev_raster()
ex.df <- data.frame(x=seq(from = bbox$xmin , to = bbox$xmax, length.out = 1000), 
                    y=seq(from = bbox$ymin , to = bbox$ymax, length.out = 1000))

# Use elevatr package to get elevation data for each point.
elev <- get_elev_raster(ex.df, prj = prj, z = 5, clip = "bbox")

# Plot 2D topographical map
raster::contour(elev)

# Read shapefile
Gila.shape <- st_read("Gila_Rivers.shp")

# Find elevation for path from elevation raster
latlons <- map_df(Gila.shape$geometry, ~ st_coordinates(.x) %>% as_tibble() %>% rename(x = X, y = Y ) ) %>% as.data.frame

# ???
z <- raster::extract(elev, latlons %>% .[, c("x","y")])

# ???
Gila.Channels  <- latlons %>% add_column(z = z) 
Gila.Channels  <- subset(Gila.Channels, select = c(x, y, z))

Gila.Channels %>%
  select = c(x, y, z) %>%
  as_tibble() %>%
  mutate(is_max =  ifelse(z == max(z), 1, 0), # chop return
         cumsum_is_max = cumsum(is_max)) %>%
  filter(cumsum_is_max == 0 | (is_max == 1 & cumsum_is_max == 1)) %>%
  select(y, x, z)  %>%
  mutate(z = z + 30)

# Create second bounding box to overlay image of terrain
bbox2 <- list(
  p1 = list(long = bbox$xmin, lat = bbox$ymin),
  p2 =  list(long = bbox$xmax, lat = bbox$ymax)
)

# Download overlay image of terrain
get_arcgis_map_image(bbox2, map_type = "World_Imagery", file = overlay_file,
                     width = dim(elev)[2], height = dim(elev)[1], 
                     sr_bbox = 4326)

# Import overlay image of terrain
overlay_file <- "images.png"

# Use when overlay image doesn't need to be rotated
overlay_img <- png::readPNG(overlay_file)

# Use when overlay image needs to be roated
#image_write(image_rotate(image_read(overlay_file), 90) ,"rotated.png")
#overlay_img <- png::readPNG("rotated.png")

# reconfigure bathymetry data for 3D plotting (** to correct mirrored plotting in rayshader)
#Gila.map <- as.matrix(elev) %>%
  #apply(., 2, rev)

#myzscale = 20000 / (elevation_raster_zoom * elevation_raster_zoom * elevation_raster_zoom)
#shadow = ray_shade(Gila.map,zscale=myzscale,lambert=FALSE)
#ambient = ambient_shade(Gila.map,zscale=myzscale)

# plot_3d
# sphere_shade retourne RGB array of hillshaded texture mappings.
Gila.map %>%
  sphere_shade(texture = "desert") %>% 
  add_shadow(ray_shade(Gila.map)) %>%
  add_shadow(ambient_shade(Gila.map)) %>%
  plot_map(Gila.map, zscale = 5 ,fov = 0, theta = 80, phi = 45, windowsize = c(1000, 1000), zoom = 1)

#### RIVERS

# Read shapefile
Gila.shape <- st_read("Gila_Rivers.shp")

#elev = raster("./rayshader_issue/elev.tif")
river = rgdal::readOGR("Gila_Rivers.shp")

#Convert NHD to SpatialLines
river = as(river, "SpatialLines")

# Create Raster with equal extent, dim and res to elev with all values = 0 
r = raster()
extent(r) = extent(Gila.raster)
dim(r) = dim(Gila.raster)
res(r) = res(Gila.raster)
crs(r) = crs(Gila.raster)
values(r) = 0

# Resample raster file to smaller resolution
#river2 <- aggregate(river, fact = 10)

# Rasterize rivers to new raster;  set water to 1 and all else to 0
rr = rasterize(river, r)
values(rr)[is.na(values(rr))] = 0
values(rr)[values(rr) != 0] = 1

#rr
#elev

water = matrix(raster::extract(rr,raster::extent(rr),
               nrow=ncol(rr),ncol=nrow(rr)))

water =   matrix(values(rr),
                 nrow=ncol(rr),ncol=nrow(rr))

#water[nrow(water):1,] or water[,ncol(water):1]
water1 = apply(water, 2, rev)
water2 = apply(t(water1),2,rev)
water2 = apply(t(water2),2,rev)


elevmat = matrix(raster::extract(elev,raster::extent(elev),buffer=10000),
                 nrow=ncol(elev),ncol=nrow(elev))

##check dims
#dim(water) == dim(elevmat)

# Rayshade!!
Gila.map %>%
  sphere_shade(texture = "desert") %>% 
  add_water(water1, color="lightblue") %>% 
  plot_map


# Create first 3D plot
#Gila.map %>%
#  sphere_shade(texture = "desert") %>%
#  add_shadow(ray_shade(Gila.map)) %>%
#  add_shadow(ambient_shade(Gila.map)) %>%
#  plot_3d(Gila.map, zscale = 5, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))
#
#
#
##check dims
#dim(water) == dim(elevmat)
#
#
##KUD3D adds points and axes
#add_points(
#  ras = elev,
#  det = Gila.Channels,
#  zscale=myzscale,
#  cont = c(95, 5),
#  alphavec = c(0.1, 0.9),
#  drawpoints = T,
#  size = 3,
#  col.pt = "black",
#  colors = c("red","red")
#)  
#add_axes(elev,
#         zscale=myzscale,
#         axis.col = grey(0.5))
#
#render_snapshot("trailmap")


#### generate gif
## elev_matrix <- elmat
# n_frames <- 60
# zscale <- myzscale
# # frame transition variables
# thetavalues <- transition_values(from = 80, to = 35, steps = n_frames, 
#                                  one_way = TRUE, type = "lin")
# 
# phivalues <- transition_values(from = 28, to = 73, steps = n_frames, 
#                                one_way = FALSE, type = "cos")
# 
# zoomvalues <- transition_values(from = 0.6, to = 0.8, steps = n_frames, 
#                                 one_way = FALSE, type = "cos")
# # shadow layers
# ambmat <- ambient_shade(elev_matrix, zscale = zscale)
# raymat <- ray_shade(elev_matrix, zscale = zscale, lambert = TRUE)
# 
# # generate .png frame images
# img_frames <- paste0("drain", seq_len(n_frames), ".png")
# for (i in seq_len(n_frames)) {
#   message(paste(" - image", i, "of", n_frames))
#   
#   sphere_shade(heightmap = elmat, zscale=myzscale,texture = "imhof4") %>% 
#     add_shadow(shadow) %>%
#     add_shadow(ambient)  %>% 
#     #add_water(detect_water(elmat)) %>%
#     add_overlay(., overlay = overlay_img, alphacolor = NULL,alphalayer = 0.9)  %>%
#     plot_3d(., heightmap = elmat,
#             zscale=myzscale,fov=0,theta=thetavalues[i],phi=phivalues[i],windowsize=c(1000,800),zoom=zoomvalues[i],
#             water=FALSE,          baseshape = "circle" 
#     )
#   
#   
#   # render labels
#   # locaton founds by trial and error
#   render_label(heightmap = elmat,x=363,y=281, z=1000,zscale=myzscale,
#                text = "Base Camp",textsize = 2,linewidth = 5)
#   
#   
#   render_label(heightmap = elmat,x=124,y=178, z=1000,zscale=myzscale,
#                text = "Summit",textsize = 2,linewidth = 5)
#   
#   
#   #KUD3D adds points and axes
#   add_points(
#     ras = elev,
#     det = kilian_path,
#     zscale=myzscale,
#     cont = c(95, 5),
#     alphavec = c(0.1, 0.9),
#     drawpoints = T,
#     size = 3,
#     col.pt = "black",
#     colors = c("red","red")
#   )  
#   add_axes(elev,
#            zscale=myzscale,
#            axis.col = grey(0.5))
#   
#   
#   render_snapshot(img_frames[i])
#   rgl::clear3d()
# }
# 
# # build gif
# magick::image_write_gif(magick::image_read(img_frames), 
#                         path = "everest.gif", 
#                         delay = 2/n_frames)
