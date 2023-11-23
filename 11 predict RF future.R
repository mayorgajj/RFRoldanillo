

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, terra, rgdal, cclust, ggspatial, geosphere, ggmap, osrm, classInt, dismo, gtools, sp, rgeos, FactoMineR, pROC, randomForest, Hmisc, rgeos, fs)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

source('FunctionsRFclustering.R')

# Load data ---------------------------------------------------------------
load('../rData/cana/run_1/rflist_3.rdata')
load('../rData/cana/run_1/clustereddata.rData')

rfrs <- do.call(randomForest::combine, rflist)

# Future climate 
mdls <- dir_ls('../data/tif/chelsa/future/down/v1', type = 'directory')
mdls
fles <- map(mdls, dir_ls, regexp = 'bioc.tif')
fles <- as.character(fles)

# Presence points
pnts <- clusteredpresdata

# Function to use ---------------------------------------------------------
make.predict <- function(fle){
  
  # fle <- fles[1]
  cat('To start the analysis\n')
  
  rst <- rast(fle)
  names(rst) <- glue('bioc_{1:19}')
  rst <- stack(rst)
  vls <- data.frame(getValues(rst))
  
  rsl <- terra::predict(rfrs, vls, type = 'prob')
  rasterProbs <- rsl
  rasterRF <- rowSums(rasterProbs[,c(3:5)])
  uncertainty <- apply(rasterProbs, 1, max)
  
  rasterRFprob <- rst[[1]]
  values(rasterRFprob) <- rasterRF
  
  rasterRFuncertainty <- rst[[1]]
  values(rasterRFuncertainty) <- uncertainty
  
  rasterRF <- max.col(rasterProbs, 'first')
  rasterRFclass <- rst[[1]]
  values(rasterRFclass) <- rasterRF
  
  print("Write Raster...")
  gcm <- dirname(fle) %>% basename()
  dout <- glue('../rf/cana/output/run_1/results/raw/{gcm}')
  dir_create(dout)
  
  terra::writeRaster(x = rasterRFclass, filename = glue('{dout}/RF_3Clust_current.tif'), filetype = 'GTiff', overwrite = TRUE)
  terra::writeRaster(x = rasterRFprob, filename = glue('{dout}/RF_3Prob_current.tif'), filetype = 'GTiff', overwrite = TRUE)
  terra::writeRaster(x = rasterRFuncertainty, filename = glue('{dout}/RF_3Unc_current.tif'), filetype = 'GTiff', overwrite = TRUE)
  cat('Done!\n')
  
  
}

map(fles[2:length(fles)], make.predict)


