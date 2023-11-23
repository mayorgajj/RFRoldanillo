

## Summarise the historic climate
## Juan Jose Mayorga 
## Jul 6

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, spatialEco, corrplot, fs, elevatr, sf, tidyverse, glue, climateR)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Functions to use --------------------------------------------------------
smmr.tasm <- function(fls, var, mnt){
  # fls <- tmax; var <- 'tmax'; mnt <- 1
  cat(mnt, '\n')
  mnt <- ifelse(mnt < 10, paste0('0', mnt), as.character(mnt))
  rst <- rast(grep(paste0('.', mnt, '.tif'), fls, value = T))
  rst <- app(rst, mean)
  names(rst) <- glue('{var}_{mnt}')
  # dir <- '../data/tif/tc/baseline/valle/tsr'
  # terra::writeRaster(x = rst, filename = glue('{dir}/{var}_{yea}'))
  return(rst)
}

smmr.prec <- function(fls, var, mnt){
  # fls <- prec; var <- 'prec'; mnt <- 1
  cat(mnt, '\n')
  mnt <- ifelse(mnt < 10, paste0('0', mnt), as.character(mnt))
  rst <- rast(grep(paste0('.', mnt, '.tif'), fls, value = T))
  rst <- app(rst, mean)
  names(rst) <- glue('{var}_{mnt}')
  return(rst)
}

# List files --------------------------------------------------------------
fles <- as.character(dir_ls('tmpr/climate', regexp = '.tif$'))
tmax <- grep('tmax', fles, value = T)
tmin <- grep('tmin', fles, value = T)
prec <- grep('prec', fles, value = T)

# Summarise ---------------------------------------------------------------

# Tmax
tmax.smmr <- map(.x = 1:12, .f = function(x) smmr.tasm(fls = tmax, var = 'tmax', mnt = x))
tmax.smmr <- reduce(tmax.smmr, c)
terra::writeRaster(x = tmax.smmr, filename = '../data/tif/tc/baseline/valle/tmax_75m.tif')

rm(tmax.smmr)

# Tmin 
tmin.smmr <- map(.x = 1:12, .f = function(x) smmr.tasm(fls = tmin, var = 'tmin', mnt = x))
tmin.smmr <- reduce(tmin.smmr, c)
terra::writeRaster(x = tmin.smmr, filename = '../data/tif/tc/baseline/valle/tmin_75m.tif')

# Prec 
prec.smmr <- map(.x = 1:12, .f = function(x) smmr.prec(fls = prec, var = 'prec', mnt = x))
prec.smmr <- reduce(prec.smmr, c)
terra::writeRaster(x = prec.smmr, filename = '../data/tif/tc/baseline/valle/prec_75m.tif', overwrite = T)

# Summarise new precipitation  --------------------------------------------
prec <- rast('tmpr/climate/prec/prec_75m_tsr.tif')

prec.smmr <- map(.x = 1:12, .f = function(m){
  
  cat('To process: ', m, '\n')
  m <- ifelse(m < 10, paste0('0', m), as.character(m))
  r <- prec[[grep(paste0('-', m, '-'), time(prec))]]
  r <- app(r, mean)
  names(r) <- glue('prec_{m}')
  return(r)
  
})

prec.smmr <- reduce(prec.smmr, c)
terra::writeRaster(x = prec.smmr, filename = '../data/tif/tc/baseline/valle/prec_75m.tif')

