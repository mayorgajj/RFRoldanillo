

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, terra, rgdal, cclust, ggspatial, geosphere, ggmap, osrm, classInt, dismo, gtools, sp, rgeos, FactoMineR, pROC, randomForest, Hmisc, rgeos, fs)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
dirs <- dir_ls('../rf/cana/output/run_1/results/raw', type = 'directory') %>% 
  as.character()

# Thresholds loading
load('../rData/cana/run_1/prob_thrs.rData')
load('../rData/cana/run_1/prob_uncr.rData')

rlda <- terra::vect('../data/gpkg/valle.gpkg')
rlda <- rlda[rlda$MPIO_CNMBR == 'ROLDANILLO',]

# Matrix for the reclassify
mtx.prob <- matrix(c(0, prob.thrs, 0, prob.thrs, 1, 2), ncol = 3, byrow = T)
mtx.clst <- matrix(c(0.5, 2.5, 0, 2.5, 7.5, 1), nrow = 2, byrow = T)

# To add limitations and mixed categories ---------------------------------
calc.limitations <- function(dr){
  
  # dir <- dirs[1]
  
  cat('>>> To process: ', basename(dr), ' <<<')
  
  fls <- dir_ls(dr, regexp = '.tif')
  fls <- as.character(fls)
  
  cls <- grep('Clust', fls, value = T) %>% rast()
  prb <- grep('Prob', fls, value = T) %>% rast()
  unc <- grep('Unc', fls, value = T) %>% rast()
  
  prb.cls <- classify(prb, mtx.prob)
  cls.cls <- classify(cls, mtx.clst)
  
  # To add limitations classes ----------------------------------------------
  dffr <- prb.cls - cls.cls
  rslt <- cls
  
  rslt[dffr[] == -1] <- 2 + 3 + 1
  rslt[dffr[] == 2] <- 2 + 3 + 1
  
  # To add the mixed values -------------------------------------------------
  fnal <- rslt
  fnal[unc[] < uncr.thrs & prb[] > prob.thrs] <- 7
  
  # To write the results ----------------------------------------------------
  terra::writeRaster(x = fnal, filename = glue('{dr}/rf_fnal.tif'), overwrite = TRUE)
  cat('Done!\n')
  
}

map(dirs, calc.limitations)

# To make the ensemble model for each SSP  --------------------------------
ssps <- c('ssp370', 'ssp585')
rstr <- map(.x = 1:length(ssps), .f = function(i){
  
  cat(' >>> To process: ', ssps[i], ' <<<\n')
  drs <- grep(ssps[i], dirs, value = T)  
  fls <- map(drs, dir_ls)
  fls <- unlist(fls)
  fls <- as.character(fls)
  fls <- grep('rf_fnal', fls, value = T)
  rst <- map(fls, rast)
  rst <- map(.x = 1:length(rst), .f = function(x) terra::crop(rst[[x]], rlda) %>% terra::mask(., rlda))
  rst <- rast(fls)
  mdl <- terra::modal(rst)
  cat('Finish!\n')
  return(mdl)
  
  
})

rstr <- reduce(rstr, c)
rstr <- terra::crop(rstr, rlda) %>% terra::mask(., rlda)


names(rstr) <- ssps

terra::writeRaster(x = rstr, filename = glue('../rf/cana/output/run_1/results/raw/RF_mdl_ssps.tif'), overwrite = TRUE)



# To check the results ----------------------------------------------------
fles <- dir_ls('../rf/cana/output/run_1/results/raw', type = 'directory') %>% 
  map(., dir_ls) %>%
  unlist() %>% 
  as.character() %>% 
  grep('rf_fnal.tif$', ., value = T)

rs37 <- fles %>% 
  grep('ssp370', ., value = T) %>% 
  terra::rast()

rs58 <- fles %>% 
  grep('ssp585', ., value = T) %>% 
  terra::rast()

rs37.mdl <- terra::modal(rs37)
rs58.mdl <- terra::modal(rs58)

par(mfrow = c(1, 2))
plot(rs37.mdl, main = 'SSP 370')
plot(rs58.mdl, main = 'SSP 585')
par(mfrow = c(1, 1))

rs37.mdl.rld <- terra::crop(rs37.mdl, rlda) %>% terra::mask(., rlda)
rs58.mdl.rld <- terra::crop(rs58.mdl, rlda) %>% terra::mask(., rlda)

par(mfrow = c(1, 2))
plot(rs37.mdl.rld, main = 'SSP 370')
plot(rs58.mdl.rld, main = 'SSP 585')
par(mfrow = c(1, 1))

stck <- c(rs37.mdl.rld, rs58.mdl.rld)


# To write these results 
dout <- glue('../rf/cana/output/run_1/results/process')
rs37.mdl
terra::writeRaster(x = rs37.mdl, filename = glue('{dout}/rf_ft-mdl_370_valle.tif'), overwrite = TRUE)
terra::writeRaster(x = rs58.mdl, filename = glue('{dout}/rf_ft-mdl_585_valle.tif'), overwrite = TRUE)

terra::writeRaster(x = rs37.mdl.rld, filename = glue('{dout}/rf_ft-mdl_370_rlda.tif'), overwrite = TRUE)
terra::writeRaster(x = rs58.mdl.rld, filename = glue('{dout}/rf_ft-mdl_585_rlda.tif'), overwrite = TRUE)


