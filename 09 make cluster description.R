

## Meke cluster description
## Juan Jose 
## August 9th 2023

# ### Load libraries ------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, terra, rgdal, cclust, glue, ggspatial, geosphere, ggmap, osrm, classInt, dismo, gtools, sp, rgeos, FactoMineR, pROC, randomForest, Hmisc, rgeos)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# ### Load data -----------------------------------------------------------

# Administrative data
vlle <- terra::vect('../data/gpkg/valle.gpkg')
rldn <- vlle[vlle$MPIO_CNMBR == 'ROLDANILLO',]
zone <- vlle[vlle$MPIO_CNMBR != 'BUENAVENTURA',]

# Random forest results
rstr <- terra::rast('../rf/cana/output/run_1/results/raw/RF_3Unc_final.tif')

# Climate data
bioc <- rast('../data/tif/tc/baseline/valle/bioc_75m.tif')

# ### Raster to table ---------------------------------------------------------
clst <- rstr %>% 
  terra::as.data.frame(., xy = T) %>% 
  as_tibble() %>% 
  filter(clust %in% 3:5) %>% 
  mutate(clust = factor(clust, levels = 3:5))

# ### To extract the values -----------------------------------------------

vles <- map_dfr(.x = 3:5, .f = function(i){
  
  cat('To process: ', i, '\n')
  cls <- filter(clst, clust == i)
  vls <- terra::extract(bioc, cls[,1:2])
  vls <- cbind(cls, vls)
  vls <- as_tibble(vls)
  vls <- gather(vls, var, value, -clust, -ID, -x, -y)
  vls <- mutate(vls, var = gsub('_75m', '', var))
  vls <- dplyr::select(vls, -ID)
  return(vls)
  
})

qs::qsave(x = vles, file = '../qs/bioc_run1_cana.qs')

vles <- mutate(vles, var = factor(var, levels = paste0('bioc_', 1:19)))

# ### To make the boxplot -------------------------------------------------
make.graph <- function(vrb){
  
  # vrb <- 'bioc_1'
  
  cat('To start the analysis: ', vrb, '\n')
  gbox <- ggplot(data = vles %>% filter(var == vrb), aes(x = clust, y = value)) +
    geom_boxplot(outlier.shape = NA) + 
    labs(x = '', y = 'Valor variable') +
    ggtitle(label = glue('Variable: {vrb}')) +
    scale_y_continuous(limits = quantile(vles %>% filter(var == vrb) %>% pull(value), c(0.1, 0.9))) +
    theme_bw() +
    theme(strip.text = element_text(face = 'bold'),
          plot.title = element_text(face = 'bold', hjust = 0.5, size = 16),
          axis.text.y = element_text(angle = 90, hjust = 0.5))
  
  ggsave(plot = gbox, filename = glue('../png/graphs/box_{vrb}_cana-run1.png'), units = 'in', width = 9, height = 7, dpi = 300)
  cat('Done!\n')
  
}
map(glue('bioc_{1:19}'), make.graph)

### Labels ------------------------------------------------------------------
lbls <- tibble(type = c(3, 4, 5), class = c('Templado - Húmedo', 'Cálido - Seco', 'Frío - Muy húmedo'))







