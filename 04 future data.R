

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(geodata, glue, rgeos, gtools, dismo, sf, tidyverse, fs, corrplot)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
ssps <- c('245', '585')
prds <- c('2021-2040', '2041-2060')
mdls <- c("ACCESS-CM2", "ACCESS-ESM1-5", "AWI-CM-1-1-MR", "BCC-CSM2-MR", "CanESM5", "CanESM5-CanOE", "CMCC-ESM2", "CNRM-CM6-1", "CNRM-CM6-1-HR", "CNRM-ESM2-1", "EC-Earth3-Veg", "EC-Earth3-Veg-LR", "FIO-ESM-2-0", "GFDL-ESM4", "GISS-E2-1-G", "GISS-E2-1-H", "HadGEM3-GC31-LL", "INM-CM4-8", "INM-CM5-0", "IPSL-CM6A-LR", "MIROC-ES2L", "MIROC6", "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", "MRI-ESM2-0", "UKESM1-0-LL")
vars <- c('prec', 'tmax', 'tmin')
vlle <- terra::vect('../data/gpkg/valle.gpkg')
zone <- vlle[vlle$MPIO_CNMBR == 'ROLDANILLO',]

cntr <- terra::centroids(vlle) %>% terra::crds() %>% as.data.frame()
cntr <- cntr[1,]

# To download -------------------------------------------------------------
down <- function(ssp, mdl, prd){
  
  ssp <- ssps[1]
  mdl <- mdls[1]
  prd <- prds[1]
  
  ppt1 <- geodata::cmip6_world(model = mdl, ssp = ssp, time = prd, var = 'prec', res = 0.5, path = 'tmpr')
  
  
}


# Function ----------------------------------------------------------------
down <- function(ssp, mdl, prd){

  ssp <- ssps[1]
  mdl <- mdls[1]
  prd <- prds[1]
  
  # To download
  cat(ssp, mdl, prd, '\n', sep = ' ')
  
  ppt1 <- geodata::cmip6_tile(var = 'prec', lon = cntr[1,1], lat = cntr[1,2], model = mdl, ssp = ssp, time = prd, path = 'tmpr') 
  ppt2 <- geodata::cmip6_tile(var = 'prec', lon = cntr[1,1], lat = cntr[1,2] + 10, model = mdl, ssp = ssp, time = prd, path = '../tmpr')
  ppt3 <- geodata::cmip6_tile(var = 'prec', lon = cntr[1,1] + 15, lat = cntr[1,2] - 5, model = mdl, ssp = ssp, time = prd, path = '../tmpr')
  
  tmx1 <- geodata::cmip6_tile(var = 'tmax', lon = cntr[1,1], lat = cntr[1,2], model = mdl, ssp = ssp, time = prd, path = '../tmpr') 
  tmx2 <- geodata::cmip6_tile(var = 'tmax', lon = cntr[1,1], lat = cntr[1,2] + 10, model = mdl, ssp = ssp, time = prd, path = '../tmpr')
  tmx3 <- geodata::cmip6_tile(var = 'tmax', lon = cntr[1,1] + 15, lat = cntr[1,2] - 5, model = mdl, ssp = ssp, time = prd, path = '../tmpr')
  
  tmn1 <- geodata::cmip6_tile(var = 'tmin', lon = cntr[1,1], lat = cntr[1,2], model = mdl, ssp = ssp, time = prd, path = '../tmpr') 
  tmn2 <- geodata::cmip6_tile(var = 'tmin', lon = cntr[1,1], lat = cntr[1,2] + 10, model = mdl, ssp = ssp, time = prd, path = '../tmpr')
  tmn3 <- geodata::cmip6_tile(var = 'tmin', lon = cntr[1,1] + 15, lat = cntr[1,2] - 5, model = mdl, ssp = ssp, time = prd, path = '../tmpr')
  
  # To extract by mask 
  ppt1 <- terra::crop(ppt1, mex0) %>% terra::mask(., mex0)
  ppt2 <- terra::crop(ppt2, mex0) %>% terra::mask(., mex0)
  ppt3 <- terra::crop(ppt3, mex0) %>% terra::mask(., mex0)
  
  tmx1 <- terra::crop(tmx1, mex0) %>% terra::mask(., mex0)
  tmx2 <- terra::crop(tmx2, mex0) %>% terra::mask(., mex0)
  tmx3 <- terra::crop(tmx3, mex0) %>% terra::mask(., mex0)
  
  tmn1 <- terra::crop(tmn1, mex0) %>% terra::mask(., mex0)
  tmn2 <- terra::crop(tmn2, mex0) %>% terra::mask(., mex0)
  tmn3 <- terra::crop(tmn3, mex0) %>% terra::mask(., mex0)
  
  # To make the mosaic
  pptn <- terra::mosaic(ppt1, ppt2, ppt3)
  tmax <- terra::mosaic(tmx1, tmx2, tmx3)
  tmin <- terra::mosaic(tmn1, tmn2, tmn3)
  
  # Write the final rasters
  dout <- glue('../rst/clima/cmp6/ssp_{ssp}/{prd}/{mdl}')
  ifelse(!file.exists(dout), dir_create(dout), print('Dir already exists!'))
  terra::writeRaster(x = pptn, filename = glue('{dout}/prec.tif'), overwrite = T)
  terra::writeRaster(x = tmax, filename = glue('{dout}/tmax.tif'), overwrite = T)
  terra::writeRaster(x = tmin, filename = glue('{dout}/tmin.tif'), overwrite = T)
  cat('Done!\n')
  
}



