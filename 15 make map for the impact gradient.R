

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
clrs <- c('#fffefe', '#84b87d', '#e4d51d', '#c95954', '#5e825f')
names(clrs) <- c('No idóneo', 'Adaptación incremental', 'Adaptación sistémica', 'Transformación', 'Oportunidades')

# To tidy the table -------------------------------------------------------
names(stck) <- c('ssp370', 'ssp585')
tble <- terra::as.data.frame(stck, xy = T) %>% 
  as_tibble() %>% 
  gather(var, value, -x, -y) %>% 
  inner_join(., lbls, by = c('value' = 'values')) %>% 
  mutate(var = factor(var, levels = c('ssp370', 'ssp585')), 
         class = factor(class, levels = c('No idóneo', 'Transformación', 'Adaptación incremental', 'Adaptación sistémica', 'Oportunidades')))

# To make the map ---------------------------------------------------------

gmap <- ggplot() + 
  geom_tile(data = tble, aes(x = x, y = y, fill = class)) + 
  facet_wrap(~var) + 
  scale_fill_manual(values = clrs) + 
  geom_sf(data = st_as_sf(rlda), fill = NA, col = 'grey50') +
  labs(x = 'Lon', y = 'Lat', fill = 'Categoría') +
  coord_sf() + 
  ggtitle(label = 'Mapa de gradiente de impacto para la caña de azúcar') +
  theme_minimal() + 
  theme(legend.position = 'bottom', 
        text = element_text(family = 'serif'),
        plot.title = element_text(face = 'bold', hjust = 0.5, size = 19),
        strip.text = element_text(face = 'bold', hjust = 0.5, size = 16),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 6), 
        axis.title = element_text(face = 'bold', hjust = 0.5)) +
  guides(fill = guide_legend( 
    title.position = 'top',
    title.hjust = 0.5,
  )) +
  annotation_scale(location =  "bl", width_hint = 0.5, text_family = 'serif', text_col = 'grey60', bar_cols = c('grey60', 'grey99'), line_width = 0.2) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), 
                         style = north_arrow_fancy_orienteering(text_family = 'serif', text_col = 'grey40', line_col = 'grey60', fill = c('grey60', 'grey99'))) 

ggsave(plot = gmap, filename = '../png/maps/canaazucar_impactGradient_run1.png', units = 'in', width = 10, height = 6, dpi = 300)



#fffefe
#84b87d
#e4d51d
#c95954
#5e825f



