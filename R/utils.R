import_drivers <- function(folder, pattern) {
  pattern <- paste0("^", pattern)
  names <- list.files(folder, pattern)
  path <- paste(folder, names, sep = "/")
  drivers <- vector(length = length(names))
  drivers <- map(path, read_csv)
}

fix_cols <- function(table) {
  table %>% 
    mutate(TRIP = as.character(TRIP)) %>% 
    select(DRIVER, ID, LONG, LAT, DAY, PR, TRIP, S, SPD_KMH, VALID_TIME)
}

fix_date <- function(table, var) {
  table %>% 
    mutate(DAY = lubridate::dmy({{ var }}))
}

fix_sample <- function(table, var) {
  table %>% 
    fix_cols %>% 
    fix_date({{ var }})
}

calc_time <- function(table) {
  table %>% 
    drop_na(S) %>% 
    select(S) %>% 
    sum() / 3600
}

create_linestring <- function(table) {
  table %>%
    drop_na(LONG, LAT) %>%
    filter(LONG != 0, LAT != 0) %>%
    mutate(
      lag = PR - lag(PR),
      wkt = case_when(
        lag == 1 ~ paste0(
          "LINESTRING (", lag(LONG), " ", lag(LAT), ", ", LONG, " ", LAT, ")"
        ),
        lag > 1 ~ "0",
        lag < 1 ~ "0",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(wkt != "0") %>%
    select(-lag) %>%
    st_as_sf(wkt = "wkt") %>%
    st_set_crs(4674)
}

calc_dist <- function(linestring) {
  linestring %>% 
    st_length() %>% 
    sum()
}

create_points <- function(table) {
  table %>% 
    drop_na(LONG, LAT) %>% 
    filter(LONG != 0, LAT != 0) %>% 
    st_as_sf(coords = c("LONG", "LAT")) %>% 
    st_set_crs(4674)
}

extract_speed_limits <- function(points, axis) {
  points %>%
    st_transform(crs = 31982) %>%
    st_join(st_buffer(axis["limite_vel"], dist = 10)) %>%
    filter(!is.na(limite_vel)) %>%
    distinct(DAY, PR, .keep_all = TRUE) %>%
    st_transform(crs = 4674)
}

filter_valid_time <- function(table) {
  table %>% 
    filter(VALID_TIME == "Yes")
}

calc_trips <- function(table) {
  table %>% 
    pull(ID) %>% 
    unique() %>% 
    length() %>% 
    as.numeric()
}

transform_linestring <- function(points) {
  points %>%
    mutate(coords = st_as_text(geometry)) %>%
    st_drop_geometry() %>%
    separate(
      coords, into = c("point", "coords"), sep = "\\s", extra = "merge"
    ) %>%
    mutate(
      lag = PR - lag(PR),
      wkt = case_when(
        lag == 1 ~ paste0(
          "LINESTRING (", str_sub(lag(coords), 2, -2), ", ",
          str_sub(coords, 2, -2), ")"
        ),
        lag > 0 ~ "0",
        lag < 0 ~ "0",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(wkt != "0") %>%
    drop_na(wkt) %>%
    select(-lag, -S, -point, -coords) %>%
    st_as_sf(wkt = "wkt") %>%
    st_set_crs(4674)
}

calc_exp_dist <- function(linestring) {
  linestring %>% 
    mutate(EXP_SPD = limite_vel - SPD_KMH) %>% 
    filter(EXP_SPD < 10) %>% 
    st_length() %>% 
    sum()
}

calc_spd_dist <- function(linestring) {
  linestring %>% 
    mutate(speeding = SPD_KMH - limite_vel) %>% 
    filter(speeding > 5) %>% 
    st_length() %>% 
    sum()
}

calc_exp_time <- function(points) {
  points %>% 
    mutate(EXP_SPD = limite_vel - SPD_KMH) %>% 
    filter(EXP_SPD < 10) %>% 
    drop_na(S) %>% 
    select(S) %>% 
    sum() / 3600
}

calc_spd_time <- function(points) {
  points %>% 
    mutate(speeding = SPD_KMH - limite_vel) %>% 
    filter(speeding > 5) %>%
    drop_na(S) %>% 
    select(S) %>% 
    sum() / 3600
}
