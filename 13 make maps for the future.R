
## Make suitability map for the future
## Juan Jose 
## August 14th 2023

# ### Load libraries ------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, terra, colourpicker, rgdal, cclust, glue, ggspatial, geosphere, ggmap, osrm, classInt, dismo, gtools, sp, rgeos, FactoMineR, pROC, randomForest, Hmisc, rgeos)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
rfrs <- terra::rast('../rf/cana/output/run_1/results/raw/RF_mdl_ssps.tif')
rfrs <- c(
  terra::rast('../rf/cana/output/run_1/results/process/rf_ft-mdl_370_valle.tif'),
  terra::rast('../rf/cana/output/run_1/results/process/rf_ft-mdl_370_valle.tif')
)

tble <- terra::as.data.frame(rfrs, xy = T, na.rm = T)
tble <- as_tibble(tble)
tble <- gather(tble, var, value, -x, -y)
tble <- mutate(tble, var = factor(var, levels = c('ssp370', 'ssp585')))

# Spatial vector data -----------------------------------------------------
vlle <- vect('../data/gpkg/valle.gpkg')
rldn <- vlle[vlle$MPIO_CNMBR == 'ROLDANILLO',]

# Labels for each model  --------------------------------------------------
lbls <- tibble(type = c(1, 2, 3, 4, 5, 6, 7), class = c('Sin idoneidad', 'Sin idoneidad', 'Templado - Húmedo', 'Cálido - Seco', 'Frío - Muy húmedo', 'Limitaciones', 'Aptitud incierta'))
clrs <- c('#FAFDFF', '#4BA374', '#A3904B', '#85CCFF', '#B6B7B8', '#F5FF85')
names(clrs) <- c('Sin idoneidad', 'Templado - Húmedo', 'Cálido - Seco', 'Frío - Muy húmedo', 'Limitaciones', 'Aptitud incierta')


tble <- inner_join(tble, lbls, by = c('value' = 'type'))

# To make the map ---------------------------------------------------------
gmap <- ggplot() + 
  geom_tile(data = tble, aes(x = x, y = y, fill = class)) + 
  facet_wrap(.~var) +
  scale_fill_manual(values = clrs) + 
  geom_sf(data = st_as_sf(vlle), fill = NA, col = 'grey30') +
  geom_sf(data = st_as_sf(rldn), fill = NA, col = 'grey50') +
  geom_sf_text(data = st_as_sf(vlle), aes(label = MPIO_CNMBR), family = 'serif') +
  coord_sf(xlim = ext(rldn)[1:2], ylim = ext(rldn)[3:4]) +
  labs(x = 'Lon', y = 'Lat', fill = 'Zona agroclimática') +
  ggtitle(label = 'Idoneidad climática para la caña de azúcar en el municipio de Roldanillo\nbajo escenarios de cambio climático') +
  theme_minimal() + 
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        text = element_text(family = 'serif')) +
  guides(fill = guide_legend( 
    direction = 'horizontal',
    keyheight = unit(1.15, units = "mm"),
    keywidth = unit(25, units = "mm"),
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

gmap

ggsave(plot = gmap, filename = '../png/maps/suitability_future_cana-run1.png', units = 'in', width = 9, height = 7, dpi = 300)
ggsave(plot = gmap, filename = './suitability_future_cana-run1.png', units = 'in', width = 9, height = 7, dpi = 300)
