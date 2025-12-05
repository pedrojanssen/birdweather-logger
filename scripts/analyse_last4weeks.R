# scripts/analyse_last4weeks.R
# Analyse the last 4 weeks of data in data/master_detections.csv
# and write summaries + plots to output/

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(suncalc)

# ---------------- CONFIG ----------------

path_master <- "data/master_detections.csv"

tz_local   <- "Europe/Amsterdam"  # adjust if needed
output_dir <- "output"

# Approximate coordinates of your main site (for sunrise)
# Set these to your garden / station location if you want more precision
lat_site <- 52.0   # e.g. 52.0 for NL
lon_site <- 5.0    # e.g. 5.0 for NL

# ---------------- LOAD & PREPARE DATA ----------------

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(path_master) || file.info(path_master)$size == 0) {
  stop("Master file does not exist or is empty: ", path_master)
}

df_raw <- read_csv(path_master, show_col_types = FALSE)

# Try to auto-detect species and time columns
species_candidates <- c(
  "species", "common_name", "commonName",
  "scientific_name", "scientificName",
  "species.commonName", "species.scientificName"
)

time_candidates <- c("timestamp", "created_at", "time", "datetime")

species_col <- intersect(species_candidates, names(df_raw))[1]
time_col    <- intersect(time_candidates,    names(df_raw))[1]

if (is.na(species_col)) {
  stop("Could not find a species column. Available columns: ",
       paste(names(df_raw), collapse = ", "))
}
if (is.na(time_col)) {
  stop("Could not find a timestamp column. Available columns: ",
       paste(names(df_raw), collapse = ", "))
}

cat("Using species column:", species_col, "\n")
cat("Using time column   :", time_col, "\n")

df <- df_raw %>%
  mutate(
    species  = .data[[species_col]],
    ts_utc   = ymd_hms(.data[[time_col]], quiet = TRUE),
    ts_local = with_tz(ts_utc, tz_local),
    date     = as_date(ts_local),
    hour     = hour(ts_local)
  ) %>%
  filter(!is.na(species), !is.na(date))

if (nrow(df) == 0) {
  stop("No valid rows after parsing timestamps and species.")
}

# ---------------- FILTER TO LAST 4 WEEKS ----------------

end_date   <- today(tzone = tz_local)
start_date <- end_date - weeks(4)

cat("Analysing period from", start_date, "to", end_date, " (last 4 weeks)\n")

df4 <- df %>%
  filter(date >= start_date & date <= end_date)

if (nrow(df4) == 0) {
  stop("No detections in the last 4 weeks.")
}

n_days_total <- n_distinct(df4$date)
cat("Detections in last 4 weeks:", nrow(df4), "\n")
cat("Days with detections in last 4 weeks:", n_days_total, "\n")

# ---------------- 1) Species summary: core/regular/occasional ----------------

species_summary <- df4 %>%
  group_by(species) %>%
  summarise(
    n_detections = n(),
    n_days       = n_distinct(date),
    .groups      = "drop"
  ) %>%
  mutate(
    occupancy_prop = n_days / n_days_total,
    category = case_when(
      occupancy_prop >= 0.5 ~ "core",
      occupancy_prop >= 0.1 ~ "regular",
      TRUE                  ~ "occasional"
    )
  ) %>%
  arrange(desc(n_detections))

write_csv(species_summary, file.path(output_dir, "species_summary_last4weeks.csv"))

# ---------------- 2) Daily species richness ----------------------------------

daily_richness <- df4 %>%
  group_by(date) %>%
  summarise(
    n_species    = n_distinct(species),
    n_detections = n(),
    .groups      = "drop"
  ) %>%
  arrange(date)

write_csv(daily_richness, file.path(output_dir, "daily_richness_last4weeks.csv"))

p_daily_richness <- ggplot(daily_richness, aes(x = date, y = n_species)) +
  geom_line() +
  labs(
    title = "Daily species richness (last 4 weeks)",
    x     = "date",
    y     = "number of species"
  )

ggsave(
  filename = file.path(output_dir, "daily_species_richness_last4weeks.png"),
  plot     = p_daily_richness,
  width    = 8, height = 4, dpi = 150
)

# ---------------- 3) First detection + cumulative curve ----------------------

first_detection <- df4 %>%
  group_by(species) %>%
  summarise(
    first_date = min(date),
    .groups    = "drop"
  ) %>%
  arrange(first_date)

write_csv(first_detection, file.path(output_dir, "first_detection_dates_last4weeks.csv"))

cum_curve <- first_detection %>%
  arrange(first_date) %>%
  mutate(cum_species = row_number())

write_csv(cum_curve, file.path(output_dir, "cumulative_species_curve_last4weeks.csv"))

p_cum <- ggplot(cum_curve, aes(x = first_date, y = cum_species)) +
  geom_step() +
  labs(
    title = "Cumulative species in last 4 weeks",
    x     = "date of first detection (last 4 weeks)",
    y     = "cumulative species"
  )

ggsave(
  filename = file.path(output_dir, "cumulative_species_curve_last4weeks.png"),
  plot     = p_cum,
  width    = 8, height = 4, dpi = 150
)

# ---------------- 4) Hourly activity & hourly richness -----------------------

hourly_all <- df4 %>%
  group_by(hour) %>%
  summarise(
    n_detections = n(),
    n_species    = n_distinct(species),
    .groups      = "drop"
  ) %>%
  arrange(hour)

write_csv(hourly_all, file.path(output_dir, "hourly_activity_all_species_last4weeks.csv"))

# Plot detections per hour (as before)
p_hour_all <- ggplot(hourly_all, aes(x = hour, y = n_detections)) +
  geom_col() +
  labs(
    title = "Hourly activity (detections, last 4 weeks)",
    x     = "hour of day",
    y     = "number of detections"
  )

ggsave(
  filename = file.path(output_dir, "hourly_activity_all_species_last4weeks.png"),
  plot     = p_hour_all,
  width    = 8, height = 4, dpi = 150
)

# Hourly richness curve (species per hour)
hourly_richness <- hourly_all %>%
  select(hour, n_species)

write_csv(hourly_richness, file.path(output_dir, "hourly_richness_last4weeks.csv"))

p_hour_rich <- ggplot(hourly_richness, aes(x = hour, y = n_species)) +
  geom_col() +
  labs(
    title = "Hourly species richness (last 4 weeks)",
    x     = "hour of day",
    y     = "number of species"
  )

ggsave(
  filename = file.path(output_dir, "hourly_species_richness_last4weeks.png"),
  plot     = p_hour_rich,
  width    = 8, height = 4, dpi = 150
)

# Top N species by detections
top_n <- 10

top_species <- species_summary %>%
  slice_max(order_by = n_detections, n = top_n) %>%
  pull(species)

hourly_top <- df4 %>%
  filter(species %in% top_species) %>%
  group_by(species, hour) %>%
  summarise(
    n_detections = n(),
    .groups      = "drop"
  )

write_csv(hourly_top, file.path(output_dir, "hourly_activity_top_species_last4weeks.csv"))

p_hour_top <- ggplot(hourly_top, aes(x = hour, y = n_detections)) +
  geom_col() +
  facet_wrap(~ species, scales = "free_y") +
  labs(
    title = paste0("Hourly activity for top ", top_n, " species (last 4 weeks)"),
    x     = "hour of day",
    y     = "number of detections"
  )

ggsave(
  filename = file.path(output_dir, "hourly_activity_top_species_last4weeks.png"),
  plot     = p_hour_top,
  width    = 10, height = 8, dpi = 150
)

# ---------------- 5) Dawn-to-day activity ratio ------------------------------

# Compute sunrise times per date at your site
unique_dates <- sort(unique(df4$date))

sun_times <- getSunlightTimes(
  date = unique_dates,
  lat  = lat_site,
  lon  = lon_site,
  keep = c("sunrise")
) %>%
  # getSunlightTimes returns POSIXct in UTC; convert to local tz
  mutate(
    date       = as_date(sunrise),
    sunrise_local = with_tz(sunrise, tz_local)
  ) %>%
  select(date, sunrise_local)

# Join sunrise to df4 and flag "dawn window"
df4_sun <- df4 %>%
  left_join(sun_times, by = "date") %>%
  mutate(
    dawn_start = sunrise_local - minutes(30),
    dawn_end   = sunrise_local + hours(2),
    is_dawn    = if_else(
      !is.na(sunrise_local) & ts_local >= dawn_start & ts_local <= dawn_end,
      TRUE, FALSE
    )
  )

dawn_stats <- df4_sun %>%
  group_by(species) %>%
  summarise(
    n_total = n(),
    n_dawn  = sum(is_dawn, na.rm = TRUE),
    dawn_ratio = if_else(n_total > 0, n_dawn / n_total, NA_real_),
    .groups = "drop"
  ) %>%
  arrange(desc(n_total))

write_csv(dawn_stats, file.path(output_dir, "dawn_ratio_last4weeks.csv"))

# Plot dawn ratio for species with enough detections (e.g. >= 20)
dawn_plot_df <- dawn_stats %>%
  filter(n_total >= 20, !is.na(dawn_ratio)) %>%
  arrange(desc(dawn_ratio)) %>%
  slice_head(n = 20) %>%
  mutate(species = factor(species, levels = rev(unique(species))))

if (nrow(dawn_plot_df) > 0) {
  p_dawn <- ggplot(dawn_plot_df, aes(x = species, y = dawn_ratio)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Dawn-to-day activity ratio (top species, last 4 weeks)",
      x     = "species",
      y     = "fraction of detections in dawn window"
    )

  ggsave(
    filename = file.path(output_dir, "dawn_ratio_top_species_last4weeks.png"),
    plot     = p_dawn,
    width    = 8, height = 6, dpi = 150
  )
} else {
  cat("No species with >= 20 detections in last 4 weeks for dawn plot.\n")
}

# ---------------- 6) Weekly turnover (Jaccard) -------------------------------

df4_week <- df4 %>%
  mutate(week_start = floor_date(date, unit = "week", week_start = 1))

week_species <- df4_week %>%
  group_by(week_start, species) %>%
  summarise(n = n(), .groups = "drop")

weeks_vec <- sort(unique(week_species$week_start))

if (length(weeks_vec) >= 2) {
  turnover_list <- list()

  for (i in seq_len(length(weeks_vec) - 1)) {
    w1 <- weeks_vec[i]
    w2 <- weeks_vec[i + 1]

    sp1 <- week_species %>% filter(week_start == w1) %>% pull(species) %>% unique()
    sp2 <- week_species %>% filter(week_start == w2) %>% pull(species) %>% unique()

    inter <- length(intersect(sp1, sp2))
    union <- length(union(sp1, sp2))
    jacc  <- if (union > 0) inter / union else NA_real_

    gained <- length(setdiff(sp2, sp1))
    lost   <- length(setdiff(sp1, sp2))

    turnover_list[[length(turnover_list) + 1]] <- tibble(
      week_start      = w1,
      next_week_start = w2,
      jaccard         = jacc,
      n_species_week  = length(sp1),
      n_species_next  = length(sp2),
      species_gained  = gained,
      species_lost    = lost
    )
  }

  turnover_df <- bind_rows(turnover_list)

  write_csv(turnover_df, file.path(output_dir, "weekly_turnover_last4weeks.csv"))

  p_turnover <- ggplot(turnover_df, aes(x = week_start, y = jaccard)) +
    geom_line() +
    geom_point() +
    scale_x_date(date_breaks = "1 week", date_labels = "%Y-%m-%d") +
    labs(
      title = "Weekly community similarity (Jaccard, last 4 weeks)",
      x     = "week (start date)",
      y     = "Jaccard index"
    )

  ggsave(
    filename = file.path(output_dir, "weekly_turnover_jaccard_last4weeks.png"),
    plot     = p_turnover,
    width    = 8, height = 4, dpi = 150
  )
} else {
  cat("Not enough distinct weeks in last 4 weeks to compute turnover.\n")
}

cat("Analysis finished. Outputs written to folder:", output_dir, "\n")
