library(tidyverse)
library(sf)

source("R/utils.R")

axis <- st_read("data/eixo_osm+ippuc_lim_velocidade.gpkg")

amostra <- import_drivers("data/", "drivers")

amostra <- map(amostra, ~fix_sample(table = .x, var = DAY)) %>% 
  reduce(bind_rows)

# Full sample -------------------------------------------------------------

full_time <- calc_time(amostra)

full_dist <- amostra %>% 
  create_linestring() %>% 
  calc_dist()

full_trips <- calc_trips(amostra)

# Valid time --------------------------------------------------------------

amostra_com_limite <- amostra %>% 
  create_points() %>% 
  extract_speed_limits(axis) %>% 
  filter_valid_time()

valid_time <- calc_time(amostra_com_limite)

amostra_limite_linestring <- transform_linestring(amostra_com_limite)

valid_dist <- calc_dist(amostra_limite_linestring)

valid_trips <- calc_trips(amostra_com_limite)

# Exposure ----------------------------------------------------------------

exp_dist <- calc_exp_dist(amostra_limite_linestring)

exp_time <- calc_exp_time(amostra_com_limite)

# Speeding ----------------------------------------------------------------

spd_dist <- calc_spd_dist(amostra_limite_linestring)

spd_time <- calc_spd_time(amostra_com_limite)

# Resultados --------------------------------------------------------------

resultados <- tribble(
  ~status, ~tempo, ~distancia, ~viagens,
  "Total", full_time, full_dist, full_trips,
  "Valid", valid_time, valid_dist, valid_trips,
  "Exposure", exp_time, exp_dist, NA,
  "Speeding", spd_time, spd_dist, NA
)

resultados
