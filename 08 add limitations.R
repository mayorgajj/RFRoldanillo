

## Add the limitation class to the results
## Juan Jose 
## August 9th 2023

# ### Load libraries ------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, terra, rgdal, cclust, ggspatial, geosphere, ggmap, osrm, classInt, dismo, gtools, sp, rgeos, FactoMineR, pROC, randomForest, Hmisc, rgeos)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# ### Load data -----------------------------------------------------------
load('../rData/cana/run_1/clustereddata.rData')
pnts <- relocate(clusteredpresdata, cluster, .after = lat)
prob <- terra::rast('../rf/cana/output/run_1/results/raw/RF_3Prob_current.tif')
rfrs <- terra::rast('../rf/cana/output/run_1/results/raw/RF_3Clust_current.tif')
uncr <- terra::rast('../rf/cana/output/run_1/results/raw/RF_3Unc_current.tif')

## To change the names for the rasters
names(prob) <- 'prob'
names(rfrs) <- 'clust'
names(uncr) <- 'uncr'

# To extract the values from the probability  -----------------------------

# Limitations -------------------------------------------------------------
prob.vles <- terra::extract(prob, pnts[,1:2])[,2]
uncr.vles <- terra::extract(uncr, pnts[,1:2])[,2]

plot(prob.vles, type = 'l')
prob.qntl <- quantile(prob.vles, seq(0, 1, 0.01))
prob.qntl <- as.data.frame(prob.qntl) %>% rownames_to_column() %>% as_tibble()
prob.thrs <- filter(prob.qntl, rowname == '5%') %>% pull(2) %>% as.numeric()

prob.binr <- prob
prob.binr[prob.binr[] < prob.thrs] <- 0
prob.binr[prob.binr[] > prob.thrs] <- 1

save(prob.thrs, file = '../rData/cana/run_1/prob_thrs.rData')

# Mixed -------------------------------------------------------------------
uncr.qntl <- quantile(uncr.vles, seq(0, 1, 0.01))
uncr.qntl <- as.data.frame(uncr.qntl) %>% rownames_to_column() %>% as_tibble()
uncr.thrs <- filter(uncr.qntl, rowname == '5%') %>% pull(2) %>% as.numeric()

save(uncr.thrs, file = '../rData/cana/run_1/prob_uncr.rData')

# To create in the raster - Limitations -----------------------------------

mtx.prob <- matrix(c(0, prob.thrs, 0, prob.thrs, 1, 2), ncol = 3, byrow = T)
mtx.clst <- matrix(c(0.5, 2.5, 0, 2.5, 7.5, 1), nrow = 2, byrow = T)

prob.clsf <- classify(prob, mtx.prob)
clst.clsf <- classify(rfrs, mtx.clst) %>% 

dffr <- prob.clsf - clst.clsf
rslt <- rfrs

rslt[dffr[] == -1] <- 2 + 3 + 1
rslt[dffr[] == 2] <- 2 + 3 + 1

# To create in the raster - Mixed -----------------------------------------

fnal <- rslt
fnal[uncr[] < uncr.thrs & prob[] > prob.thrs] <- 7

rslt <- lyrClust
rslt[which(lyrUnc[] < thrUnc1 & lyrPrb[] > threshold)] <- max(unique(lyrClust[]), na.rm = T) + 1 
print('To write the raster')

terra::writeRaster(x = fnal, filename = '../rf/cana/output/run_1/results/raw/RF_3Unc_final.tif', overwrite = T)
