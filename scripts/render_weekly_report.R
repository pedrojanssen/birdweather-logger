# scripts/render_weekly_report.R
# Renders scripts/weekly_report.Rmd to a dated PDF in reports/

library(rmarkdown)
library(lubridate)

tz_local <- "Europe/Amsterdam"
today_local <- today(tz_local)

out_dir  <- "reports"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

out_file <- file.path(out_dir, paste0("PUC_weekly_report_", today_local, ".pdf"))

render(
  input       = "scripts/weekly_report.Rmd",
  output_file = out_file,
  envir       = new.env(parent = globalenv())
)

cat("Weekly report written to:", out_file, "\n")
