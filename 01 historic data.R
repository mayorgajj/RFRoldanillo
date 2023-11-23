

## Download historic climate data 
## Juan Jose Mayorga 
## Jul 4

# Load libraries ----------------------------------------------------------
install.packages('pacman')
require(pacman)
pacman::p_load(terra, spatialEco, corrplot, fs, elevatr, sf, tidyverse, glue, climateR)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

kmz <- st_read('../data/kmz/Cultivo de caña Roldanillo.kml')
plot(kmz)
# Load data ---------------------------------------------------------------
dpto <- vect('../data/gpkg/dptos.gpkg')
mpio <- vect('../data/gpkg/mpios.gpkg')
vlle <- mpio[mpio$DPTO_CNMBR == 'VALLE DEL CAUCA',]
zone <- vlle[vlle$MPIO_CNMBR == 'ROLDANILLO',]
plot(zone)
dir_create('../data/gpkg')
writeVector(vlle, '../data/gpkg/valle.gpkg')

# Download ----------------------------------------------------------------
tmax <- climateR::getTerraClim(AOI = st_as_sf(vlle), varname = 'tmax', startDate = '1990-01-01', endDate = '2020-12-31')
tmax <- tmax[[1]]

tmin <- climateR::getTerraClim(AOI = st_as_sf(vlle), varname = 'tmin', startDate = '1990-01-01', endDate = '2020-12-31')
tmin <- tmin[[1]]

prec <- climateR::getTerraClim(AOI = st_as_sf(vlle), varname = 'ppt', startDate = '1990-01-01', endDate = '2020-12-31')
prec <- prec[[1]]

# To write the rasters ----------------------------------------------------
dout <- '../data/tif/tc/baseline/valle'
dir_create(dout)
terra::writeRaster(x = tmax, filename = glue('{dout}/tmax_5km.tif'), overwrite = T)
terra::writeRaster(x = tmin, filename = glue('{dout}/tmin_5km.tif'), overwrite = T)
terra::writeRaster(x = prec, filename = glue('{dout}/prec_5km.tif'), overwrite = T)

# Download SRTM  ----------------------------------------------------------
srtm <- get_elev_raster(locations = st_as_sf(vlle), z = 11)

# To extract by mask  -----------------------------------------------------
tmax <- terra::crop(rast(tmax), vlle) %>% terra::mask(., vlle)
tmin <- terra::crop(rast(tmin), vlle) %>% terra::mask(., vlle)
prec <- terra::crop(rast(prec), vlle) %>% terra::mask(., vlle)

srtm <- terra::crop(rast(srtm), vlle) %>% terra::mask(., vlle)
terra::writeRaster(x = srtm, filename = '../data/tif/srtm/srtm_75.tif', overwrite = TRUE)

srtm <- terra::rast('../data/tif/srtm/srtm_75_fill.tif')

# Now to make the downscaling ---------------------------------------------
nlyr(tmax) == nlyr(tmin) 
nlyr(tmin)

tasm <- map(.x = 1:nlyr(prec), .f = function(r){
  cat(r, '\n')
  # tmx <- tmax[[r]]
  # tmn <- tmin[[r]]
  ppt <- prec[[r]]
  # tmn.dwn <- spatialEco::raster.downscale(x = srtm, y = tmn, residuals = FALSE, p = 0.95, scatter = FALSE)$downscale
  # tmx.dwn <- spatialEco::raster.downscale(x = srtm, y = tmx, residuals = FALSE, p = 0.95, scatter = FALSE)$downscale
  
  ppt.dwn <- spatialEco::raster.downscale(x = srtm, y = ppt, residuals = FALSE, p = 0.95, scatter = FALSE)$downscale
  ppt.dwn <- ifel(ppt.dwn < 0, 0, ppt.dwn)
  # terra::writeRaster(tmn.dwn, filename = glue('tmpr/climate/tmin_{names(tmn)}.tif'), overwrite = T)
  # terra::writeRaster(tmx.dwn, filename = glue('tmpr/climate/tmax_{names(tmx)}.tif'), overwrite = T)
  terra::writeRaster(ppt.dwn, filename = glue('tmpr/climate/prec_{names(ppt)}.tif'), overwrite = T)
  # rm(tmn.dwn, tmx.dwn, tmx, tmn); gc(reset = T)
  rm(ppt.dwn, ppt); gc(reset = T)
})

format(object.size(tmn.dwn), units = 'Mb')

tmin.down <- map(tasm, 1)
tmax.down <- map(tasm, 2)


# Surface analysis --------------------------------------------------------
trrn <- terrain(x = srtm, v = c('slope', 'aspect', 'TPI', 'TRI', 'roughness'))
trrn <- c(srtm, trrn)
trrn.rsmp <- terra::resample(trrn, mask, method = 'bilinear')

# Precipitation analysis --------------------------------------------------
mask <- prec[[1]] * 0 + 1
smpl <- terra::as.data.frame(mask, xy = T) %>% as_tibble %>% sample_n(tbl = ., size = nrow(.) * 0.25, replace = FALSE)


trrn.vles <- (as_tibble(cbind(smpl[,1:2], terra::extract(trrn.rsmp, smpl[,c('x', 'y')]))))

prec.sum <- prec %>% 
  terra::as.data.frame(., xy = T) %>% 
  as_tibble() %>% 
  mutate(gid = 1:nrow(.)) %>% 
  gather(var, value, -gid, -x, -y) %>% 
  mutate(year = str_sub(var, 2, 5)) %>% 
  group_by(gid, x, y, year) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  group_by(gid, x, y) %>% 
  summarise(value = mean(value)) %>% 
  ungroup() %>% 
  dplyr::select(-1) %>% 
  rast(., type = 'xyz')

prec.vles <- (as_tibble(cbind(smpl, terra::extract(prec.sum, smpl[,c('x', 'y')]))))

nrow(prec.vles)
nrow(trrn.vles)

trrn.vles <- trrn.vles %>% mutate(prec = pull(prec.vles, 5))
mtrx <- trrn.vles[,c(10, 4:9)]
mtrx <- drop_na(mtrx)

corr <- cor(mtrx)
corrplot(corr)
