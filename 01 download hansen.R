

## Download hansen
## Juan Jose Mayorga 
## Jul 19

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, spatialEco, corrplot, fs, elevatr, sf, tidyverse, glue, gfcanalysis) # climateR

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

kmz <- st_read('../data/kmz/Cultivo de caña Roldanillo.kml')

# Load data ---------------------------------------------------------------

pnts <- st_read('../data/kmz/Cultivo de caña Roldanillo.kml')
vlle <- terra::vect('../data/gpkg/valle.gpkg')
zone <- vlle[vlle$MPIO_CNMBR == 'ROLDANILLO',]

tles <- calc_gfc_tiles(aoi = st_as_sf(vlle))
frst <- gfcanalysis::download_tiles(tiles = tles, output_folder = 'tmpr')
frst <- extract_gfc(aoi = st_as_sf(vlle), data_folder = 'tmpr')
frst <- rast(frst) %>% crop(., vlle)

base.prec <- rast('../data/tif/tc/baseline/valle/prec_5km.tif') 

tree <- frst[[1]]
tree <- mask(tree, vlle)

terra::writeRaster(x = tree, filename = '../data/tif/treecover/treecover_raw.tif')

srtm <- terra::rast('../data/tif/srtm/srtm_75_fill.tif')
tree <- terra::resample(tree, srtm, method = 'bilinear')

terra::writeRaster(x = tree, filename = '../data/tif/treecover/treecover_rsm.tif')

# To make the downscaling analysis ----------------------------------------
prec <- map(.x = 1:nlyr(prec), .f = function(r){
  cat(r, '\n')
  ppt <- prec[[r]]
  # ppt.srt <- spatialEco::raster.downscale(x = srtm, y = ppt, residuals = FALSE, p = 0.95, scatter = FALSE)$downscale
  ppt.dwn <- spatialEco::raster.downscale(x = tree, y = ppt, residuals = FALSE, p = 0.95, scatter = FALSE)$downscale
  ppt.dwn <- ifel(ppt.dwn < 0, 0, ppt.dwn)
  terra::writeRaster(ppt.dwn, filename = glue('tmpr/climate/prec/prec_{names(ppt)}.tif'), overwrite = T)
  rm(ppt.dwn, ppt); gc(reset = T)
})

prec <- dir_ls('tmpr/climate/prec') %>% 
  as.character %>% 
  grep('.tif', ., value =T )

prec <- rast(prec)
dtes <- c('1990-01-01', '2020-12-31')
dtes <- seq(as.Date('1990-01-01', format = '%Y-%m-%d'), as.Date('2020-12-31', format = '%Y-%m-%d'), by = 'month')
time(prec) <- dtes

terra::writeRaster(x = prec, filename = 'tmpr/climate/prec/prec_75m_tsr.tif', overwrite = TRUE)

