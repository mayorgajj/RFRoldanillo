
## Make bioclimatic variables and the maps
## Juan Jose Mayorga 
## Jul 7

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, spatialEco, dismo, cowplot, ggpubr, RColorBrewer, corrplot, ggspatial, fs, elevatr, sf, tidyverse, glue, climateR)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

fles <- dir_ls('../data/tif/tc/baseline/valle', regexp = '.tif$')
fles <- grep('75m', fles, value = T)
fles <- as.character(fles)

prec <- rast(grep('prec', fles, value = T))
tmin <- rast(grep('tmin', fles, value = T))
tmax <- rast(grep('tmax', fles, value = T))

vlle <- terra::vect('../data/gpkg/valle.gpkg')
zone <- vlle[vlle$MPIO_CNMBR == 'ROLDANILLO',]

plot(prec[[1]])
plot(zone, add = T, col = 'red')

# Extract by mask  --------------------------------------------------------
prec <- terra::mask(terra::crop(prec, zone), zone)
tmin <- terra::mask(terra::crop(tmin, zone), zone)
tmax <- terra::mask(terra::crop(tmax, zone), zone)

# To make the bioclimatic variables ---------------------------------------
bioc <- dismo::biovars(prec = stack(prec), tmin = stack(tmin), tmax = stack(tmax))
lbls <- tibble(var = glue('bio{1:19}'), variable = glue('Bio {1:19}'))

# Now to make the map -----------------------------------------------------
bioc.tble <- rast(bioc) %>% 
  terra::as.data.frame(., xy = T) %>% 
  as_tibble %>% 
  gather(var, value, -x, -y) %>% 
  inner_join(., lbls, by = c('var')) %>% 
  mutate(variable = factor(variable, levels = glue('Bio {1:19}')))

tmpr.1 <- c('Bio 1', 'Bio 5', 'Bio 6', 'Bio 8', 'Bio 9', 'Bio 10', 'Bio 11')
tmpr.2 <- c('Bio 2', 'Bio 3', 'Bio 4', 'Bio 7')

# Mapping temperature (°C) -----------------------------------------------
gtmp.1 <- ggplot() + 
  geom_tile(data = filter(bioc.tble, variable %in% tmpr.1), aes(x = x, y = y, fill = value)) + 
  scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'YlOrRd')) +
  geom_sf(data = st_as_sf(zone), fill = NA, col = 'grey30') +
  coord_sf() +
  facet_wrap(. ~ variable) + 
  ggtitle(label = 'Variables bioclimaticas de temperatura para Roldanillo - Valle del Cauca', 
          subtitle = 'Línea base - Unidades: °C') +
  labs(x = 'Lon', y = 'Lat', fill = 'Temperatura °C') +
  theme_minimal() + 
  theme(legend.position = 'bottom', 
        legend.key.width = unit(3, 'line'), 
        axis.text.x = element_text(size = 4), 
        axis.text.y = element_text(size = 4, angle = 90, hjust = 0.5), 
        axis.title = element_text(size = 7, face = 'bold'), 
        strip.text = element_text(face = 'bold', hjust = 0.5), 
        text = element_text(family = 'Gill Sans MT'), 
        plot.title = element_text(family = 'Gill Sans MT', hjust = 0.5, face = 'bold'), 
        plot.subtitle = element_text(family = 'Gill Sans MT', hjust = 0.5, face = 'bold')) 

gtmp.1

ggsave(plot = gtmp.1, filename = '../png/map_bio-tmpr1.png', units = 'in', width = 9, height = 7, dpi = 300)

# Mapping temperature (Adimensional)
make.map.tmp <- function(vr, nm){
  
  # vr <- 'Bio 2'
  # nm <- 'Rango de temperatura diurna'
  
  tb <- bioc.tble %>% filter(variable == vr)
  
  gtmp <- ggplot() + 
    geom_tile(data = tb, aes(x = x, y = y, fill = value)) + 
    scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'YlOrRd')) +
    geom_sf(data = st_as_sf(zone), fill = NA, col = 'grey30') +
    coord_sf() +
    ggtitle(label = glue('{nm}'), 
            subtitle = 'Línea base - Unidades: Adimensional') +
    labs(x = 'Lon', y = 'Lat', fill = 'Temperatura') +
    theme_minimal() + 
    theme(legend.position = 'bottom', 
          legend.key.width = unit(3, 'line'), 
          axis.text.x = element_text(size = 4), 
          axis.text.y = element_text(size = 4, angle = 90, hjust = 0.5), 
          axis.title = element_text(size = 7, face = 'bold'), 
          strip.text = element_text(face = 'bold', hjust = 0.5), 
          text = element_text(family = 'Gill Sans MT'), 
          plot.title = element_text(family = 'Gill Sans MT', hjust = 0.5, face = 'bold'), 
          plot.subtitle = element_text(family = 'Gill Sans MT', hjust = 0.5, face = 'bold'))
  
  return(gtmp)
  
}

gbio.2 <- make.map.tmp(vr = 'Bio 2', nm = 'Rango de Temperatura Diurna')
gbio.3 <- make.map.tmp(vr = 'Bio 3', nm = 'Isotermalidad de la temperatura')
gbio.4 <- make.map.tmp(vr = 'Bio 4', nm = 'Estacionalidad de la temperatura')

# Join adimensional maps
gbio.m <- ggarrange(gbio.2, gbio.3, gbio.4, ncol = 3, nrow = 1)
ggsave(plot = gbio.m, filename = '../png/map_bio-tmpr2_landscape.png', units = 'in', width = 15, height = 6, dpi = 300)
gbio.m <- ggarrange(gbio.2, gbio.3, gbio.4, ncol = 1, nrow = 3)
ggsave(plot = gbio.m, filename = '../png/map_bio-tmpr2_portrait.png', units = 'in', width = 6, height = 15, dpi = 300)

# Mapping precipitation (mm) ----------------------------------------------
prcp.1 <- c('Bio 12', 'Bio 13', 'Bio 14', 'Bio 16', 'Bio 17', 'Bio 18', 'Bio 19')
prcp.2 <- 'Bio 15'

filter(bioc.tble, variable %in% prcp.1)

gppt.1 <- ggplot() + 
  geom_tile(data = filter(bioc.tble, variable %in% prcp.1), aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'BrBG')) +
  geom_sf(data = st_as_sf(zone), fill = NA, col = 'grey30') +
  coord_sf() +
  facet_wrap(. ~ variable) + 
  ggtitle(label = 'Variables bioclimaticas de precipitatión para Roldanillo - Valle del Cauca', 
          subtitle = 'Línea base - Unidades: mm') +
  labs(x = 'Lon', y = 'Lat', fill = 'Precipitación (mm)') +
  theme_minimal() + 
  theme(legend.position = 'bottom', 
        legend.key.width = unit(3, 'line'), 
        axis.text.x = element_text(size = 4), 
        axis.text.y = element_text(size = 4, angle = 90, hjust = 0.5), 
        axis.title = element_text(size = 7, face = 'bold'), 
        strip.text = element_text(face = 'bold', hjust = 0.5), 
        text = element_text(family = 'Gill Sans MT'), 
        plot.title = element_text(family = 'Gill Sans MT', hjust = 0.5, face = 'bold'), 
        plot.subtitle = element_text(family = 'Gill Sans MT', hjust = 0.5, face = 'bold')) 

ggsave(plot = gppt.1, filename = '../png/map_bio-prec1.png', units = 'in', width = 9, height = 7, dpi = 300)

gppt.2 <- ggplot() + 
  geom_tile(data = filter(bioc.tble, variable %in% prcp.2), aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'BrBG')) +
  geom_sf(data = st_as_sf(zone), fill = NA, col = 'grey30') +
  coord_sf() +
  facet_wrap(. ~ variable) + 
  ggtitle(label = 'Variables bioclimaticas de precipitatión para Roldanillo - Valle del Cauca', 
          subtitle = 'Línea base - Unidades: %') +
  labs(x = 'Lon', y = 'Lat', fill = 'CV (%)') +
  theme_minimal() + 
  theme(legend.position = 'bottom', 
        legend.key.width = unit(3, 'line'), 
        axis.text.x = element_text(size = 4), 
        axis.text.y = element_text(size = 4, angle = 90, hjust = 0.5), 
        axis.title = element_text(size = 7, face = 'bold'), 
        strip.text = element_text(face = 'bold', hjust = 0.5), 
        text = element_text(family = 'Gill Sans MT'), 
        plot.title = element_text(family = 'Gill Sans MT', hjust = 0.5, face = 'bold'), 
        plot.subtitle = element_text(family = 'Gill Sans MT', hjust = 0.5, face = 'bold')) 

ggsave(plot = gppt.2, filename = '../png/map_bio-prec2.png', units = 'in', width = 9, height = 7, dpi = 300)
