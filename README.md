# RADAR Report

A [Quarto](https://quarto.org) report template for BetaNYC's Research and Data Assistance Request (RADAR) service published using GitHub Pages.

## Requirements

- [R](https://cran.r-project.org) and [Quarto](https://quarto.org/docs/get-started/)
- [Docker](https://www.docker.com/products/docker-desktop/), only if the report uses the PostGIS database in `compose.yaml`

The setup chunk in `index.qmd` installs any missing R packages.

## Setup

Copy `.Renviron.example` to `.Renviron` and fill in any credentials the report uses. `.Renviron` is gitignored.

## Render

```sh
quarto render
```

This writes the site to `docs/`. Commit `docs/` along with your changes.

## Publish

In the repo's **Settings → Pages**, deploy from the `main` branch, `/docs` folder.

## Files

| Path | Purpose |
|---|---|
| `index.qmd` | The report |
| `_quarto.yml` | Site and format options |
| `cil_table.R` | `cil_table()`, for tables in the CIL style |
| `style/`, `fonts/`, `images/` | Theme, typefaces, logos |
| `radar-header.html`, `radar-footer.html` | Page header and footer. Update the source code link in the footer for each report. |

## License

All original content is Copyright, Creative Commons 4.0 Attribution-ShareAlike.
