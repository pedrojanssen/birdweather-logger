# scripts/analyse_last4weeks.R
# Analyse the last 4 weeks of data in data/master_detections.csv
# and write summaries + plots to output/

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

# ---------------- CONFIG ----------------

path_master <- "data/master_detections.csv"

tz_local   <- "Europe/Amsterdam"  # adjust if needed
output_dir <- "output"

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
    # timestamps in your file already have an offset (+01:00 etc.),
    # ymd_hms() respects that; we then convert to local tz:
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

# ---------------- 3) First detection (within last 4 weeks) + cumulative -------

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

# ---------------- 4) Hourly activity -----------------------------------------

hourly_all <- df4 %>%
  group_by(hour) %>%
  summarise(
    n_detections = n(),
    n_species    = n_distinct(species),
    .groups      = "drop"
  ) %>%
  arrange(hour)

write_csv(hourly_all, file.path(output_dir, "hourly_activity_all_species_last4weeks.csv"))

p_hour_all <- ggplot(hourly_all, aes(x = hour, y = n_detections)) +
  geom_col() +
  labs(
    title = "Hourly activity (all species, last 4 weeks)",
    x     = "hour of day",
    y     = "number of detections"
  )

ggsave(
  filename = file.path(output_dir, "hourly_activity_all_species_last4weeks.png"),
  plot     = p_hour_all,
  width    = 8, height = 4, dpi = 150
)

# top N species by detections in last 4 weeks
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

cat("Analysis finished. Outputs written to folder:", output_dir, "\n")
