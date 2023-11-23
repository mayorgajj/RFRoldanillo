

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, raster, fs, sf, tidyverse, glue)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
stck <- terra::rast('../rf/cana/output/run_1/results/process/impact.tif')
vlle <- terra::vect('../data/gpkg/valle.gpkg')
rlda <- vlle[vlle$MPIO_CNMBR == 'ROLDANILLO',]

# Labels ------------------------------------------------------------------
lbls <- tibble(values = c(0:5), class = c('No idóneo', 'Adaptación incremental', 'Adaptación sistémica', 'Transformación', 'Oportunidades', 'Resiliencia sistémica'))
clrs <- c('#fffefe', '#84b87d', '#e4d51d', '#c95954', '#5e825f', '#F08335')
names(clrs) <- c('No idóneo', 'Adaptación incremental', 'Adaptación sistémica', 'Transformación', 'Oportunidades', 'Resiliencia sistémica')

# Raster to polygon -------------------------------------------------------
polg.ssp345 <- as.polygons(stck[[1]])
polg.ssp585 <- as.polygons(stck[[2]])

# To project
polg.ssp345 <- terra::project(polg.ssp345, 'EPSG:3116')
polg.ssp585 <- terra::project(polg.ssp585, 'EPSG:3116')

polg.ssp345$has <- terra::expanse(polg.ssp345) / 10000
polg.ssp585$has <- terra::expanse(polg.ssp585) / 10000

area.ssp345 <- tibble(values = polg.ssp345$value, has = polg.ssp345$has)
area.ssp345 <- inner_join(area.ssp345, lbls, by = 'values')

area.ssp585 <- tibble(values = polg.ssp585$value, has = polg.ssp585$has)
area.ssp585 <- inner_join(area.ssp585, lbls, by = 'values')

area.ssp345 <- mutate(area.ssp345, class = factor(class, levels = c('No idóneo', 'Adaptación incremental', 'Adaptación sistémica', 'Transformación', 'Oportunidades', 'Resiliencia sistémica')))
area.ssp585 <- mutate(area.ssp585, class = factor(class, levels = c('No idóneo', 'Adaptación incremental', 'Adaptación sistémica', 'Transformación', 'Oportunidades', 'Resiliencia sistémica')))

# To count the area -------------------------------------------------------

gbarssp370 <- ggplot(data = area.ssp345, aes(x = class, y = has, fill = class)) + 
  geom_bar(stat = 'identity') +
  geom_text(aes(label = round(has, 0)), vjust = 0) +
  scale_fill_manual(values = clrs) +
  labs(x = '', y = 'Hectáreas', fill = '') +
  ggtitle(label = 'Conteo de área para el gradiente de impacto del SSP 370') +
  theme_minimal() +
  theme(legend.position = 'bottom', 
        plot.title = element_text(face = 'bold', hjust = 0.5, size = 16),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        text = element_text(family = 'serif'),
        axis.title = element_text(face = 'bold'))

gbarssp370

ggsave(plot = gbarssp370, filename = '../png/graphs/conteo_areaSSP345.png', units = 'in', width = 9, height = 7, dpi = 300)



gbarssp585 <- ggplot(data = area.ssp345, aes(x = class, y = has, fill = class)) + 
  geom_bar(stat = 'identity') +
  geom_text(aes(label = round(has, 0)), vjust = 0) +
  scale_fill_manual(values = clrs) +
  labs(x = '', y = 'Hectáreas', fill = '') +
  ggtitle(label = 'Conteo de área para el gradiente de impacto del SSP 370') +
  theme_minimal() +
  theme(legend.position = 'bottom', 
        plot.title = element_text(face = 'bold', hjust = 0.5, size = 16),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        text = element_text(family = 'serif'),
        axis.title = element_text(face = 'bold'))

gbarssp370

ggsave(plot = gbarssp370, filename = '../png/graphs/conteo_areaSSP345.png', units = 'in', width = 9, height = 7, dpi = 300)

