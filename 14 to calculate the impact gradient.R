


# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, raster, fs, sf, tidyverse, glue)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
stck <- c(
  terra::rast('../rf/cana/output/run_1/results/raw/RF_3Unc_final.tif'),
  terra::rast('../rf/cana/output/run_1/results/process/rf_ft-mdl_370_valle.tif'),
  terra::rast('../rf/cana/output/run_1/results/process/rf_ft-mdl_585_valle.tif')
)

# bsln <- terra::rast('../rf/cana/output/run_1/results/raw/RF_3Unc_final.tif')
# ftre <- terra::rast('../rf/cana/output/run_1/results/raw/RF_mdl_ssps.tif')
# ../rf/cana/output/run_1/results/raw/RF_3Unc_final.tifstck <- c(bsln, ftre)
names(stck) <- c('current', 'future_370', 'future_585')

vlle <- terra::vect('../data/gpkg/valle.gpkg')
rlda <- vlle[vlle$MPIO_CNMBR == 'ROLDANILLO',]

# To extract by mask  -----------------------------------------------------
bsln <- stck[[1]]
ftre <- stck[[2:3]]
bsln <- terra::crop(bsln, rlda) %>% terra::mask(., rlda)
ftre <- terra::crop(ftre, rlda) %>% terra::mask(., rlda)

# Labels - impact gradient ------------------------------------------------
lbls <- tibble(value = 0:5, category = c('Unsuit', 'cope', 'adjust', 'transform', 'opportunity', 'resilience'))
allo <- suppressMessages(read_csv('../tbl/classesImpGraLimMix.csv'))
all_options <- allo

# Function to use ---------------------------------------------------------
imp.gradient <- function(crn, ftr){
  
  # crn <- bsln
  # ftr <- ftre[[1]]
  
  cat('To start the process analysis\n')
  msk <- crn * 0 
  crd_df <- crds(crn)

  x <- terra::extract(crn, crd_df, cells = T) %>% as_tibble()
  ncell <- dplyr::select(x, cell)
  x <- select_(x, names(crn))
  colnames(x) <- 'current'

  y <- as_tibble(terra::extract(ftr, crd_df, cells = T))
  y <- select_(y, names(ftr))
  colnames(y) <- 'future'

  z <- as_tibble(data.frame(x, y, ncell))
  z <- c(crn, ftr) %>% terra::as.data.frame(xy = T, cells = T, na.rm = T) %>% as_tibble() %>% dplyr::select(1, 4, 5) 
  names(z) <- c('cell', 'current', 'future')
  z <- dplyr::select(z, current, future, cell)
  
  rslt <- left_join(z, all_options, by = c('current', 'future'))
  fnal <- left_join(rslt, lbls, by = 'category')
  fnal <- pull(fnal, 5)
  trra <- msk %>% terra::as.data.frame(., xy = T, na.rm = T) %>% as_tibble() %>% mutate(value = fnal) %>% dplyr::select(x, y, value) %>% terra::rast(., type = 'xyz')
  cat('Finish!\n')
  # Sys.sleep(30)
  return(trra)
  
}

# To apply the function ---------------------------------------------------
im370 <- imp.gradient(crn = bsln, ftr = ftre[[1]])
im585 <- imp.gradient(crn = bsln, ftr = ftre[[2]])

impact <- c(im370, im585)
terra::writeRaster(x = impact, filename = '../rf/cana/output/run_1/results/process/impact.tif', overwrite = TRUE)
