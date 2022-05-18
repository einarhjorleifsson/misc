# remotes::install_github("einarhjorleifsson/gisland", dependencies = FALSE)
library(tidyverse)
library(sf)
library(rnaturalearth)
source("R/baseplot_fao-halli.R")

# get shapes -------------------------------------------------------------------
coast <-
  rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf") %>%
  st_cast("POLYGON")
countries <-
  rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
fao <-
  gisland:::gl_get_fao_area_full() %>%
  filter(F_LEVEL == "MAJOR") %>%
  select(area = F_CODE, ocean = OCEAN, name = NAME_EN) %>%
  st_cast("POLYGON")
eez <-
  gisland:::gl_get_eez() %>%
  select(cntr = territory1,
         area = iso_ter1,
         type = pol_type) %>%
  nngeo::st_remove_holes() %>%
  # takes a while
  st_make_valid() %>%
  mutate(area = as.character(area))

# get data ---------------------------------------------------------------------
#  here need to think efficiency when reading various inputs
#  ideally all the data from all countries should be in one big file
#  then we just filter the data
d_eez <-
  read_csv("data-raw/G1_EEZ.csv") %>%
  select(-1) %>%
  rename(area = EEZ) %>%
  mutate(what = "G1")
d_fao <-
  read_csv("data-raw/G1_FAOMA.csv") %>%
  select(-1) %>%
  rename(area = fao) %>%
  mutate(area = as.character(area),
         what = "G1")
d <-
  bind_rows(d_eez, d_fao)

# plot stuff -------------------------------------------------------------------
baseplot(d, eez, n, wrap = zone) +
  labs(fill = "N") +
  theme(legend.position = c(0.8,0.2))
baseplot(d, fao, n, wrap = zone) +
  labs(fill = "N")
