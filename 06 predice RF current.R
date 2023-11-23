
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, terra, rgdal, cclust, ggspatial, geosphere, ggmap, osrm, classInt, dismo, gtools, sp, rgeos, FactoMineR, pROC, randomForest, Hmisc, rgeos)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

source('FunctionsRFclustering.R')

# Function to create the random forest analysis ---------------------------
rf.clust <- function(occ, nforest, ntrees, nVars, nclasses){
  
  print('To start the process\n')
  datRF_presences <- occ[,3:ncol(occ)] %>% as_tibble()
  print(nrow(datRF_presences))
  attach(datRF_presences)
  no.forest <- nforest
  no.trees <- ntrees
  distRF_presences <- RFdist(datRF_presences, mtry1 = nVars, no.trees, no.forest, addcl1 = T, addcl2 = F, imp = T, oob.prox1 = T)
  no.presencesclasses <- nclasses
  labelRF <- pamNew(distRF_presences$cl1, no.presencesclasses)
  print(table(labelRF))
  clusterdata <- hclust(as.dist(distRF_presences$cl1), method = 'ward.D2')
  cat('Done!\n')
  return(list(labelRF, clusterdata))
  
}

# Load data ---------------------------------------------------------------
bioc <- terra::rast('../data/tif/tc/baseline/valle/bioc_75m.tif')
fles <- dir_ls('../rData/cana/run_1')
vlle <- st_read('../data/gpkg/valle.gpkg')

load(fles[1])
load(fles[2])
load(fles[3])

names(bioc) <- glue('bioc_{1:19}')

clusteredpresdata

# Cluster map 
bbox <- as.numeric(c(-76.5, 4, -75.9,  4.6))
ggbx <- get_stamenmap(bbox, maptype = 'terrain', zoom = 13)

pnts <- mutate(clusteredpresdata, clust = ifelse(cluster == '1', 'A', ifelse(cluster == '2', 'B', 'C')))

gpnt <- ggmap(ggbx) + 
  geom_point(data = pnts, aes(x = lon, y = lat, col = factor(clust))) + 
  scale_color_viridis_d() +
  geom_sf(data = vlle, fill = NA, col = 'grey30', inherit.aes = FALSE) + 
  coord_sf(ylim = c(4, 4.6), xlim = c(-76.5, -75.9)) +
  labs(x = 'Lon', y = 'Lat', col = 'Clúster (caña de azúcar)') + 
  ggtitle(label = 'Ubicación espacial de los puntos de caña de azúcar', 
          subtitle = 'Clúster con Random Forest') +
  theme_minimal() + 
  theme(legend.position = 'bottom', 
        plot.title = element_text(face = 'bold', hjust = 0.5),
        plot.subtitle = element_text(face = 'bold', hjust = 0.5),
        text = element_text(family = 'serif'),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        axis.title = element_text(face = 'bold'))  +
  guides(col = guide_legend( 
    direction = 'horizontal',
    keyheight = unit(1.15, units = "mm"),
    keywidth = unit(15, units = "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.hjust = .5,
    nrow = 1,
    byrow = T,
    reverse = F,
    label.position = "bottom"
  )) +
  annotation_scale(location =  "bl", width_hint = 0.5, text_family = 'serif', text_col = 'grey60', bar_cols = c('grey60', 'grey99'), line_width = 0.2) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), 
                         style = north_arrow_fancy_orienteering(text_family = 'serif', text_col = 'grey40', line_col = 'grey60', fill = c('grey60', 'grey99'))) 

ggsave(plot = gpnt, filename = '../png/maps/canaazucar_clustRF_run1.png', units = 'in', width = 7, height = 7, dpi = 300)

# Bias raster processing --------------------------------------------------
samplesize <- round(min(summary(as.factor(clusteredpresdata$cluster))) / 2, 0)
speciescell <- terra::extract(bioc[[1]], pnts[,1:2], cells = T)
head(speciescell)
back_raster <- bioc[[1]] * 0 
back_raster[speciescell[,3]] <- NA
NumberOfClusters <- max(clusteredpresdata$cluster)
ratio <- 3/1
numberofpresences <- nrow(clusteredpresdata)
back <- randomPoints(mask = raster(back_raster), 2*numberofpresences)
back <- as.data.frame(back)

# To extract the values from the presences
back <- as_tibble(cbind(back, terra::extract(bioc, back[,c('x', 'y')])))
back <- dplyr::select(back, -ID) 

write.csv(back, '../tbl/points/back_swd_cana.csv', row.names = F)
write.csv(pnts, '../tbl/points/pnts_swd_cana.csv', row.names = F)

### Cluster analysis to psuedo-absences ---------------------------------
bckclust <- rf.clust(occ = back, nforest = 50, ntrees = 500, nVars = 8, nclasses = 2)
datRF <- as.data.frame(back[,3:ncol(back)])
attach(datRF)
no.forest <- 50
no.trees <- 500
distRF <- RFdist(datRF, mtry1 = 8, no.trees, no.forest, addcl1 = T, addcl2 = F, imp = T, oob.prox1 = T)
no.absenceclasses <- 2
labelRF <- pamNew(distRF$cl1, no.absenceclasses)
detach(datRF)
classdata <- cbind(pb = as.factor(labelRF), back[,3:ncol(back)])

presvalue_swd <- clusteredpresdata[,3:ncol(clusteredpresdata)] %>%
  cbind(pb = (clusteredpresdata$cluster + no.absenceclasses), .) %>%
  na.omit() %>%
  as.data.frame() %>%
  mutate(cluster = cluster + no.absenceclasses)

presvalue_swd <- dplyr::select(presvalue_swd, pb, bioc_1:bioc_19)
presvalue_swd <- mutate(presvalue_swd, pb = as.factor(pb))
classdata_2 <- cbind(pb = as.data.frame(classdata)$pb, classdata[,2:ncol(classdata)])

dim(classdata_2); dim(presvalue_swd)

presvalue_swd <- presvalue_swd %>% dplyr::select(-cluster)
allclasses_swd <- rbind(classdata_2, presvalue_swd[,1:ncol(classdata)])
unique(allclases_swd$pb)

write.csv(allclasses_swd, file = '../tbl/points/allclasses_cana.csv', row.names = FALSE)

# To make the random forest analysis --------------------------------------
vrs <- glue('bioc_{1:19}')
model1 <- as.formula(paste('factor(pb) ~', paste(paste(vrs, collapse = '+', sep = ' '))))
rflist <- vector('list', 50)
auc <- vector('list', 50)

for(repe in 1:50){ # 50 bosques
  
  print(repe)
  pressample <- list()
  
  for (i in 1:(NumberOfClusters+no.absenceclasses)){
    
    if(any(i==c(1:no.absenceclasses))) { 
      
      rows <- sample(rownames(allclasses_swd[allclasses_swd$pb==i,]), 
                     size = samplesize*NumberOfClusters/2/no.absenceclasses)
    } else {
      rows <- sample(rownames(allclasses_swd[allclasses_swd$pb==i,]), size=samplesize)
    }
    pressample[[i]] <- allclasses_swd[rows,] 
  }
  
  species <- na.omit(do.call(rbind, pressample)) 
  head(species)
  Samplesplit <- sample(rownames(species)) 
  
  envtrain <- species[Samplesplit[1:(0.8*nrow(species))],] 
  envtest <- species[Samplesplit[(0.8*nrow(species)):nrow(species)],] 
  
  rfmodel <- randomForest(model1, data = envtrain, ntree = 500, na.action = na.omit, nodesize = 2) 
  
  save(rfmodel, file = paste('../rf/cana/output/run_1/model/', NumberOfClusters, 'Prob_' , 'rep_' ,repe, '.rdata' ,sep=''))
  rflist[[repe]] <- rfmodel
  
  # AUC 
  predicted <- as.numeric(predict(rfmodel, envtest))
  observed <- as.vector(envtest[,'pb'])
  auc[[repe]] <- auc(observed, predicted) 
  rm(rfmodel)
  
  cat(auc[[repe]] ,'\n')
  
}

auc <- unlist(auc)
rff <- do.call(randomForest::combine, rflist)
importance <- as.data.frame(rff$importance)

save(rflist, file = paste('../rData/cana/run_1/', 'rflist_', NumberOfClusters, '.rdata', sep = ''))
save(importance, file = paste0('../rData/cana/run_1/', 'importanceRF.rData'))
save(auc, file = paste0('../rData/cana/run_1/', 'aucRF_dist.rData'))
save(rff, file = paste0('../rData/cana/run_1/', 'rff_dist.rData'))

# Predict modell
climatevalues  <- data.frame(getValues(bioc))
NumberOfClusters <- 3

rasterProbs <- predict(rff, climatevalues, type = 'prob') # proximity = T
rasterProbs_na <- na.omit(rasterProbs)
sum_rasterProbs_na <- apply(rasterProbs_na, 1, sum)

rasterRF <- rowSums(rasterProbs[,c(3:(NumberOfClusters+2))])
uncertainty <- apply(rasterProbs, 1, max)  

rasterRFprob <- bioc[[1]]
values(rasterRFprob) <- rasterRF 

rasterRFuncertainty <- bioc[[1]]
values(rasterRFuncertainty) <- uncertainty 

rasterRF <- max.col(rasterProbs, 'first')
rasterRFclass <- bioc[[1]]
values(rasterRFclass) <- rasterRF

dout <- glue('../rf/cana/output/run_1/results/raw')
dir_create(dout)

terra::writeRaster(x = rasterRFclass, filename = glue('{dout}/RF_3Clust_current.tif'), filetype = 'GTiff', overwrite = TRUE)
terra::writeRaster(x = rasterRFprob, filename = glue('{dout}/RF_3Prob_current.tif'), filetype = 'GTiff', overwrite = TRUE)
terra::writeRaster(x = rasterRFuncertainty, filename = glue('{dout}/RF_3Unc_current.tif'), filetype = 'GTiff', overwrite = TRUE)


# Done!


