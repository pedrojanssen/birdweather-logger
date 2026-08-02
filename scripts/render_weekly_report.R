# scripts/render_weekly_report.R
# Renders scripts/weekly_report.Rmd to a dated PDF in reports/

library(rmarkdown)
library(lubridate)

tz_local    <- "America/Costa_Rica"
today_local <- today(tz_local)

# Ensure reports/ exists
out_dir <- "reports"
if (!dir.exists(out_dir)) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
}

pdf_name <- paste0("PUC_weekly_report_", today_local, ".pdf")
dest     <- file.path(out_dir, pdf_name)

# Render directly into reports/
render(
  input       = "scripts/weekly_report.Rmd",
  output_file = pdf_name,
  output_dir  = out_dir,
  envir       = new.env(parent = globalenv())
)

# Double-check the file exists
if (!file.exists(dest)) {
  stop("Expected PDF not found after render: ", dest)
}

cat("Weekly report written to:", dest, "\n")
