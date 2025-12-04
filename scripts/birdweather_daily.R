# scripts/birdweather_daily.R

library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)

# ---- CONFIG / SECRETS ----

# Use your BirdWeather station token here (stored as BW_DEVICE_ID secret)
station_token <- Sys.getenv("BW_DEVICE_ID")

if (station_token == "") {
  stop("Missing BW_DEVICE_ID environment variable. Put your BirdWeather STATION TOKEN in that secret.")
}

base_dir <- "data"

# ---- TIME RANGE: previous UTC day (for naming) ----

yesterday <- Sys.Date() - 1
start_time <- paste0(yesterday, "T00:00:00Z")  # used as `since` parameter

day_dir     <- file.path(base_dir, as.character(yesterday))
daily_file  <- file.path(day_dir, "detections.csv")
master_file <- file.path(base_dir, "master_detections.csv")

dir.create(day_dir, recursive = TRUE, showWarnings = FALSE)

# ---- API CALL: BirdWeather detections endpoint ----
# According to community docs the base is:
#   https://app.birdweather.com/api/v1/stations/{token}/detections
# and `since` (ISO8601) and `limit` are supported as query params.

base_url <- sprintf(
  "https://app.birdweather.com/api/v1/stations/%s/detections",
  station_token
)

cat("Requesting detections since", start_time, "for station", station_token, "...\n")
cat("URL:", base_url, "\n")

res <- GET(
  base_url,
  query = list(
    since = start_time,
    limit = 10000   # safety cap; adjust if you ever hit this
  )
)

if (http_error(res)) {
  stop("API request failed with status: ", status_code(res), "\nBody:\n",
       content(res, as = "text", encoding = "UTF-8"))
}

json_txt  <- content(res, as = "text", encoding = "UTF-8")
json_data <- fromJSON(json_txt, flatten = TRUE)

# BirdWeather responses use `detections` at the top level for this endpoint,
# not `data`.
detections_raw <- json_data$detections

# ---- WRITE DAILY FILE ----

if (is.null(detections_raw) || length(detections_raw) == 0) {
  cat("No detections returned. Writing empty daily CSV.\n")
  daily_df <- tibble()
  write.csv(daily_df, daily_file, row.names = FALSE)
} else {
  daily_df <- as_tibble(detections_raw)
  write.csv(daily_df, daily_file, row.names = FALSE)
  cat("Saved", nrow(daily_df), "detections to", daily_file, "\n")
}

# ---- UPDATE MASTER CSV ----

# 1. Load existing master if present
if (file.exists(master_file)) {
  master_df <- read.csv(master_file, stringsAsFactors = FALSE)
  master_df <- as_tibble(master_df)
} else {
  master_df <- tibble()
}

# 2. Append new data, but avoid duplicating the same day if the job is rerun.
combined_df <- master_df

if (nrow(daily_df) > 0) {
  # If there is some kind of timestamp column, we could optionally
  # drop existing rows for 'yesterday' from master_df first.
  # For now, we rely mainly on a unique detection id to deduplicate later.

  combined_df <- bind_rows(master_df, daily_df)
}

# 3. Remove duplicates if there is a unique detection id
id_col <- intersect(names(combined_df), c("id", "detectionId", "detection_id"))[1]
if (!is.null(id_col)) {
  combined_df <- combined_df %>%
    distinct(.data[[id_col]], .keep_all = TRUE)
}

# 4. Write master file
write.csv(combined_df, master_file, row.names = FALSE)
cat("Master file updated:", master_file, " (", nrow(combined_df), " rows )\n")
