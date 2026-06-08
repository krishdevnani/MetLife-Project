# MetLife IM — Global Real Estate Dashboard

A single-file [Shiny](https://shiny.posit.co/) application (`dashboard.R`) built with **shinydashboard**. It presents country-level real estate and macro data in an interactive UI: market grid, maps, comparisons, factor-weighted scoring, and optional AI-assisted summaries (Google News + Gemini).

## Requirements

- **R** (4.x recommended)
- R packages (install once in R):

```r
install.packages(c(
  "shiny", "shinydashboard", "readxl", "dplyr", "DT", "plotly", "ggplot2",
  "shinycssloaders", "scales", "shinyWidgets", "stringr", "tidyr", "purrr",
  "httr2", "jsonlite", "xml2", "htmltools"
))
```

## Data

Place the Excel workbooks under `data/` (paths are configured at the top of `dashboard.R`):

| Variable       | Default path                    | Role |
|----------------|---------------------------------|------|
| `DATA_PATH`    | `data/country_data.xlsx`        | Main country / factor sheet (`Sheet1`) |
| `SUMMARY_PATH` | `data/MSCI_SummaryStats.xlsx`   | MSCI summary stats (`Sheet1`) |

Change `DATA_PATH` / `SUMMARY_PATH` if your filenames differ. The app assumes it is **run with working directory** set to this project folder (see below).

## Optional: Gemini API (AI features)

News briefing and related Gemini calls read the key from:

1. Environment variable `GEMINI_API_KEY`, or  
2. `GEMINI_API_KEY_INLINE` in `dashboard.R` (not recommended for shared repos).

If no key is set, AI-dependent features degrade gracefully or show an error in the UI.

## Run

From the repository root (`metlife_dashboard/`):

```bash
Rscript dashboard.R
```

The app listens at **http://127.0.0.1:4952** (`host` and `port` are set at the bottom of `dashboard.R`). With `launch.browser = FALSE`, open that URL in a browser manually.

In RStudio you can also open `dashboard.R` and run the file (again, **working directory** should be `metlife_dashboard/` so `data/` and `custom_assets/` resolve correctly).

## Project layout

```
metlife_dashboard/
├── dashboard.R          # App UI, server, scoring, and styles
├── data/                # Input spreadsheets (not necessarily tracked in git)
├── custom_assets/       # Static files (e.g. header logo PNGs) served via addResourcePath
└── README.md
```

## Features (tabs)

- **Overview** — Filters, factor weights, market grid (sortable, export/copy), charts, global factor map.
- **Country Detail** — Single-country metrics and charts; optional A/B comparison layer.
- **Market Comparison** — Scatter and related views driven by X/Y axis selectors.
- **News Intelligence** — Market selection, optional dual-scan and AI briefing (when API/network allow).

## License / data

Internal / coursework use unless otherwise specified. Do not commit secrets or large proprietary datasets; keep API keys in environment variables or local-only config.
