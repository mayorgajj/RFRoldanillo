

## Downscaling future data
## Juan Jose Mayorga 
## Jul 22

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, spatialEco, dismo, corrplot, fs, elevatr, sf, tidyverse, glue, climateR)

g <- gc(reset = TRUE)
rm(list = ls())        
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
dirs <- dir_ls('../data/tif/chelsa/chelsa_future-raw', type = 'directory')
dirs <- as.character(dirs)
srtm <- terra::rast('../data/tif/srtm/srtm_75_fill.tif')
tree <- terra::rast('../data/tif/treecover/treecover_rsm.tif')

# Spatial data
vlle <- terra::vect('../data/gpkg/valle.gpkg')
zone <- vlle[vlle$MPIO_CNMBR != 'BUENAVENTURA',]

# Base 
prec.bsln <- rast('../data/tif/tc/baseline/valle/bioc_75m.tif')

# Function to use ---------------------------------------------------------
make.down <- function(dir){
  
  # dir <- dirs[1]
  
  cat('To process: ', dir, '\n')
  nme <- basename(dir)
  fls <- dir_ls(dir, regexp = '.tif$')
  fls <- as.character(fls)

  ppt <- grep('pr', fls, value = T)
  ppt <- rast(ppt)
  
  tmx <- grep('tasmax', fls, value = T)  
  tmx <- rast(tmx)
  
  tmn <- grep('tasmin', fls, value = T)
  tmn <- rast(tmn)
  
  # To convert the precipitation
  mns <- c(31,29,31,30,31,30,31,31,30,31,30,31)
  ppt.mlt <- reduce(map(.x = 1:12, .f = function(i){pp <- (ppt[[i]] / 100) * mns[i]}), c)
  
  # To make the downscaling
  tmx <- reduce(map(.x = 1:12, .f = function(i)raster.downscale(x = srtm, y = tmx[[i]])$downscale), c)
  tmn <- reduce(map(.x = 1:12, .f = function(i)raster.downscale(x = srtm, y = tmn[[i]])$downscale), c)
  tmx <- crop(tmx, zone) %>% mask(., zone)
  tmn <- mask(tmx, zone) %>% mask(., zone)
  
  # Now the precipitation
  ppt <- reduce(map(.x = 1:12, .f = function(i)raster.downscale(x = tree, y = ppt.mlt[[i]])$downscale), c)
  ppt <- crop(ppt, zone) %>% mask(., zone)
  
  names(tmx) <- glue('tmax_{1:12}')
  names(tmn) <- glue('tmin_{1:12}')
  names(ppt) <- glue('prec_{1:12}')
  
  ppt[ppt < 0] <- 0
  
  out <- as.character(glue('../data/tif/chelsa/future/down/v1/{nme}')); dir_create(out)
  terra::writeRaster(x = tmx, filename = glue('{out}/tmax.tif'), overwrite = T)
  terra::writeRaster(x = tmn, filename = glue('{out}/tmin.tif'), overwrite = T)
  terra::writeRaster(x = ppt, filename = glue('{out}/prec.tif'), overwrite = T)
  
  # To calculate the bioclimatic rasters
  bioc <- dismo::biovars(prec = stack(ppt), tmax = stack(tmx), tmin = stack(tmn))
  bioc <- terra::rast(bioc)
  terra::writeRaster(x = bioc, filename = glue('{out}/bioc.tif'), overwrite = TRUE)  
  
  cat('Done!\n')
  
}

# To apply the function ---------------------------------------------------
map(.x = dirs[2:length(dirs)], .f = make.down)





