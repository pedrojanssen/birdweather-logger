# BirdWeather logger and public dashboard

This repository archives one local calendar day of BirdWeather detections every
night and refreshes a near-live dashboard from aggregate data approximately every
15 minutes. Dates and hours use `America/Costa_Rica` throughout.

## Dashboard

The site files live in `docs/`. To publish them with GitHub Pages, configure the
repository's Pages source as **Deploy from a branch**, select `main`, and select
the `/docs` folder. After that one-time setting, aggregate JSON commits update the
dashboard automatically. An open browser checks for a newly published file once
per minute.

The public dataset is `docs/data/dashboard.json`. It contains:

- daily detection and species totals;
- counts and average confidence by species;
- period-over-period trends, peak hour and first-seen species;
- aggregate detection and species-diversity patterns for each hour of the day;
- a species-by-hour heatmap and aggregate review candidates;
- BirdWeather species photos with safe fallbacks for the twelve most frequently
  detected species;
- an expected-arrivals outlook for the current PUC location, ranked from the
  BirdWeather/BirdNET 48-week seasonal probability curves; and
- 7-day, 30-day and all-data views.

The dashboard separates observations into date-bounded listening sites without
using or publishing source coordinates:

- Wageningen through 11 May 2026;
- Santo Domingo, Heredia from 28 May through 31 July 2026; and
- Guápiles, Limón from 11 August 2026 onward.

The generator uses an allowlist rather than removing sensitive fields after the
fact. It never publishes detection rows, coordinates, audio links, detection or
station identifiers, or exact detection times. A recursive privacy validator runs
before every JSON write, and the daily workflow also runs the unit tests.

The historical CSV files under `data/` remain the sensitive source for the
aggregates. If repository access is public, those source files are public too;
JSON-level privacy does not hide existing repository content.

## Local checks

```bash
python3 -m unittest discover -s tests -v
python3 scripts/build_dashboard_json.py
python3 -m http.server --directory docs 8000
```

Then open `http://localhost:8000`.

## Automation

- `.github/workflows/birdweather_live_dashboard.yml` runs approximately every 15
  minutes. It downloads the current Costa Rica day to temporary runner storage,
  fetches the current station's seasonal probability curves, combines detections
  with the committed history in memory, and commits only the privacy-limited
  dashboard JSON. The temporary detection rows, station reference and full
  probability response are never published as part of this job.
- `.github/workflows/birdweather.yml` runs daily at 01:05 Costa Rica time,
  downloads the previous local day, rebuilds the public JSON, tests it, and
  commits both data and JSON.
- `.github/workflows/birdweather_weekly_report.yml` runs Mondays after the daily
  update. It installs `libuv1-dev` before the R dependencies so `fs`, `sass`,
  `bslib`, and `rmarkdown` can install successfully, then renders and stores the
  PDF report.
- All content-writing workflows share one concurrency group to prevent competing
  pushes.
