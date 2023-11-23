
## Points analysis 
## Fabio Castro - Juan Jose 
## July 19 - 2023

# ## Load libraries -------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, tidyverse, randomForest, dismo, caret, outliers, rgeos, stringr, glue, RColorBrewer,openxlsx,
               readxl)
install.packages('xlsx')
g <- gc(reset = T)
rm(list = ls())
options(scipen = 999)

# ## Functions to use -----------------------------------------------------
source('FunctionsRFclustering.R')
rf.clust <- function(occ, nforest, ntrees, nVars, nclasses){
  
  datRF_presences <- occ[,3:ncol(occ)]
  print(nrow(datRF))
  
  attach(datRF_presences)
  no.forests <- nforest
  no.trees <- ntrees
  distRF_presences <- RFdist(datRF_presences, mtry1 = nVars, no.trees, no.forests, addcl1 = T, addcl2 = F, imp = T, oob.prox1 = T)
  no.presencesclasses <- nclasses
  labelRF <- pamNew(distRF_presences$cl1, no.presencesclasses)
  print(table(labelRF))
  clusterdata <- hclust(as.dist(distRF_presences$cl1), method = 'ward.D2')
  
  Sys.sleep(100)
  return(list(labelRF, clusterdata))
  
}

# ## Study zone -----------------------------------------------------------
pnts <- st_read('../data/kmz/Cultivo de caña Roldanillo.kml')
pnts <- '../tbl/points/Aguacate Hass Roldanillo.xlsx'
pnts <- read.xlsx(pnts)
pnts <- mutate(pnts, lon = lon1 + (lon2/60) + (lon3/3600), lon = lon * -1)
pnts <- mutate(pnts, lat = lat1 + (lat2/60) + (lat3/3600))
vlle <- terra::vect('../data/gpkg/valle.gpkg')
rldn <- vlle[vlle$MPIO_CNMBR == 'ROLDANILLO',]
zone <- vlle[vlle$MPIO_CNMBR != 'BUENAVENTURA',]

plot(vlle, border = 'grey40')
plot(zone, border = 'red', add = T)
points(pnts$lon, pnts$lat, pch = 16, col = 'red')
## plot(st_geometry(pnts), add = T, pch = 16, col = 'green')

# ## Climate data ---------------------------------------------------------
fles <- dir_ls('../data/tif/tc/baseline/valle', regexp = '.tif$') %>% 
  as.character %>% 
  grep('75m', ., value = T)

prec <- fles %>% 
  grep('prec', ., value = T) %>% 
  rast() %>% 
  terra::crop(., zone) %>% 
  terra::mask(., zone)

tmax <- fles %>% 
  grep('tmax', ., value = T) %>% 
  rast() %>% 
  crop(., zone) %>% 
  mask(., zone)

tmin <- fles %>% 
  grep('tmin', ., value = T) %>% 
  rast() %>% 
  crop(., zone) %>% 
  mask(., zone)

# To create bioclimatic variables -----------------------------------------
prec.rstr <- raster::stack(prec)
tmax.rstr <- raster::stack(tmax)
tmin.rstr <- raster::stack(tmin)
bioc.rstr <- dismo::biovars(prec = prec.rstr, tmin = tmin.rstr, tmax = tmax.rstr)

terra::writeRaster(x = bioc.rstr, filename = '../data/tif/tc/baseline/valle/bioc_75m.tif', overwrite = T)
bioc <- terra::rast('../data/tif/tc/baseline/valle/bioc_75m.tif')
plot(bioc[[1]])
# To read the results -----------------------------------------------------
bioc <- terra::rast('../data/tif/tc/baseline/valle/bioc_75m.tif')
names(bioc) <- glue('bioc_{1:19}')
pnts <- st_coordinates(pnts) %>% 
  as_tibble %>% 
  setNames(c('lon', 'lat', 'rm')) %>% 
  dplyr::select(-rm)
pnts <- dplyr::select(pnts, lon, lat)
# Remove the duplicated by each cell --------------------------------------
pnts <- mutate(pnts, cell = terra::extract(bioc, pnts[,1:2], cell = T)[,'cell'])
dupv <- duplicated(pull(pnts, cell))
pnts <- pnts[!dupv,]
pnts <- dplyr::select(pnts, -cell)

# Extract the values from the presences -----------------------------------
pnts <- as_tibble(cbind(pnts, terra::extract(bioc, pnts[,c('lon', 'lat')])))
pnts <- dplyr::select(pnts, -ID)

# To remove the outliers --------------------------------------------------
norm <- scores(pnts[,3:ncol(pnts)], 'z')
norm_na <- norm
norm_na[abs(norm_na) > 3.5] <- 'NA'
normpoints <- cbind(pnts[,c('lon', 'lat')], norm_na) %>% 
  na.omit %>% 
  as_tibble()
normpoints <- normpoints[,c('lon', 'lat')]
nrow(normpoints)
nrow(pnts)
pnts <- normpoints

dir_create('../data/tbl/points/Aguacate')
write.csv(pnts, '../data/tbl/points/Aguacate/1_aguacate-raw.csv', row.names = FALSE)

# Extract the values from the presences after removed outliers ------------
pnts <- cbind(pnts, terra::extract(bioc, pnts[,c('lon', 'lat')]))
pnts <- as_tibble(pnts)
pnts <- dplyr::select(pnts, -ID)

# Clustering using Random Forest ------------------------------------------
datRF <- as.data.frame(pnts[,3:ncol(pnts)])
d <- dist(datRF, method = 'euclidean')
rfClust <- rf.clust(occ = pnts, nforest = 25, ntrees = 100, nVars = 8, nclasses = 3)
labelRF <- rfClust[[1]]
clusterdata <- rfClust[[2]]
classdata <- cbind(pb = as.factor(labelRF), pnts[,3:ncol(pnts)])
clusteredpresdata <- cbind(pnts, cluster = labelRF) %>% drop_na() %>% as_tibble()
no.clusters <- 3

# To write the results ----------------------------------------------------
dout <- glue('../rData/aguacate/run_1')
dir_create(dout)
save(datRF, file = paste0(dout, '/datRF.rData'))
save(clusterdata, file = paste0(dout, '/clusterdata.rData'))
occ <- pnts
save(occ, clusteredpresdata, no.clusters, labelRF, file = paste0(dout, '/clustereddata.rData'))
