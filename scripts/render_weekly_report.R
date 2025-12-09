# scripts/render_weekly_report.R
# Renders scripts/weekly_report.Rmd to a dated PDF and moves it into reports/

library(rmarkdown)
library(lubridate)

tz_local    <- "Europe/Amsterdam"
today_local <- today(tz_local)

# Make sure reports/ exists
out_dir <- "reports"
if (!dir.exists(out_dir)) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
}

# Just a filename here, no directory
pdf_name <- paste0("PUC_weekly_report_", today_local, ".pdf")

# Render into the current working directory (repo root)
render(
  input       = "scripts/weekly_report.Rmd",
  output_file = pdf_name,
  envir       = new.env(parent = globalenv())
)

# Move the rendered file into reports/
src  <- pdf_name
dest <- file.path(out_dir, pdf_name)

if (file.exists(src)) {
  file.rename(src, dest)
  cat("Weekly report written to:", dest, "\n")
} else {
  stop("Expected PDF not found after render: ", src)
}
