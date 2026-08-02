# scripts/birdweather_daily.R

library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)

# ---- CONFIG / SECRETS ----

# BirdWeather station token stored in BW_DEVICE_ID
station_token <- Sys.getenv("BW_DEVICE_ID")

if (station_token == "") {
  stop("Missing BW_DEVICE_ID environment variable. Put your BirdWeather STATION TOKEN in that secret.")
}

base_dir <- "data"
tz_local <- "America/Costa_Rica"

# ---- TIME RANGE: previous local calendar day ----

today_local <- as_date(with_tz(Sys.time(), tz_local))
target_date <- today_local - days(1)
from_local  <- as.POSIXct(paste(target_date, "00:00:00"), tz = tz_local)
to_local    <- as.POSIXct(paste(target_date, "23:59:59"), tz = tz_local)
from_time   <- format(with_tz(from_local, "UTC"), "%Y-%m-%dT%H:%M:%SZ")
to_time     <- format(with_tz(to_local, "UTC"), "%Y-%m-%dT%H:%M:%SZ")

day_dir     <- file.path(base_dir, as.character(target_date))
daily_file  <- file.path(day_dir, "detections.csv")
master_file <- file.path(base_dir, "master_detections.csv")

dir.create(day_dir, recursive = TRUE, showWarnings = FALSE)

# ---- API CALL: BirdWeather detections endpoint with pagination ----
# GET /api/v1/stations/{token}/detections?limit,from,to,cursor,order,...

base_url <- sprintf(
  "https://app.birdweather.com/api/v1/stations/%s/detections",
  station_token
)

cat("Requesting detections for local day", target_date, "in", tz_local, "...\n")
cat("From:", from_time, " To:", to_time, "\n")

all_pages <- list()
cursor <- NULL
page_idx <- 1L
page_limit <- 100L  # API max = 100

repeat {
  query <- list(
    limit = page_limit,
    from  = from_time,
    to    = to_time,
    order = "desc"    # default, but explicit
  )
  if (!is.null(cursor)) {
    query$cursor <- cursor
  }

  cat("Requesting page", page_idx, "with cursor =", ifelse(is.null(cursor), "NULL", cursor), "...\n")

 # at top you already have: library(httr)

res <- RETRY(
  "GET",
  base_url,
  query = query,
  times = 5,        # number of attempts
  pause_base = 2,   # base wait (seconds)
  pause_cap  = 30,  # max wait between retries
  terminate_on = c(400, 401, 403, 404) # don’t retry obvious client errors
)

if (http_error(res)) {
  status <- status_code(res)
  body_text <- content(res, as = "text", encoding = "UTF-8")
  stop("API request failed after retries. Status: ", status, "\nBody:\n", body_text)
}

  json_txt  <- content(res, as = "text", encoding = "UTF-8")
  json_data <- fromJSON(json_txt, flatten = TRUE)

  det_page <- json_data$detections

  # No more detections
  if (is.null(det_page) || NROW(det_page) == 0) {
    cat("No (more) detections returned for this page.\n")
    break
  }

  det_page <- as_tibble(det_page)
  all_pages[[length(all_pages) + 1L]] <- det_page

  cat("  Page", page_idx, "returned", nrow(det_page), "detections\n")

  # If fewer than limit, we've reached the end
  if (nrow(det_page) < page_limit) {
    break
  }

  # Prepare cursor = id of last detection in this page
  if (!"id" %in% names(det_page)) {
    cat("No 'id' field found; cannot paginate further safely.\n")
    break
  }

  cursor <- det_page$id[nrow(det_page)]
  page_idx <- page_idx + 1L
}

# Combine pages
if (length(all_pages) == 0) {
  cat("No detections found for that local day.\n")
  daily_df <- tibble()
} else {
  all_det <- bind_rows(all_pages)

  # Extra safety: keep exactly the requested Costa Rica calendar day.
  if ("timestamp" %in% names(all_det)) {
    all_det <- all_det %>%
      mutate(
        ts_utc = ymd_hms(timestamp, quiet = TRUE, tz = "UTC"),
        date_local = as_date(with_tz(ts_utc, tz_local))
      ) %>%
      filter(date_local == target_date) %>%
      select(-ts_utc, -date_local)
  }

  daily_df <- all_det
}

# ---- WRITE DAILY FILE ----

if (nrow(daily_df) == 0) {
  cat("No detections for that day after filtering. Writing empty daily CSV.\n")
  write.csv(daily_df, daily_file, row.names = FALSE)
} else {
  write.csv(daily_df, daily_file, row.names = FALSE)
  cat("Saved", nrow(daily_df), "detections to", daily_file, "\n")
}

# ---- UPDATE MASTER CSV ----

# 1. Load existing master if present AND non-empty
if (file.exists(master_file) && file.info(master_file)$size > 0) {
  master_df <- read.csv(master_file, stringsAsFactors = FALSE)
  master_df <- as_tibble(master_df)
} else {
  if (file.exists(master_file) && file.info(master_file)$size == 0) {
    cat("Master file exists but is empty; starting fresh.\n")
  } else {
    cat("No existing master file; starting new.\n")
  }
  master_df <- tibble()
}

# 2. Append new daily data
combined_df <- master_df
if (nrow(daily_df) > 0) {
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
