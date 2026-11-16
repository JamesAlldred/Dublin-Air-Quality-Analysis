# Dublin Air Quality Analysis

Analysis of hourly air quality at Rathmines, Dublin (30 June 2020 to 30 September 2026), asking how much of the site's PM2.5 arrives by long range transport. NO2 is used as the local traffic comparison.

Author: James Alldred

## Approach

1. **Wind direction.** A wind rose and polar maps of PM2.5 and NO2 (hourly wind from Dublin Airport).
2. **Two back trajectory case studies.** A clean Atlantic case (21 to 22 March 2023) and a polluted European case (9 to 10 March 2024), with the PM2.5 and NO2 time series for each.
3. **PM2.5 against NO2 by wind sector.** Atlantic (wind from 180 to 270 degrees) compared with European (45 to 135 degrees).
4. **SQTBA for 2022.** A simplified quantitative transport bias analysis map of PM2.5, plus a map of how many trajectories pass through each cell.

## Repository contents

| File | Purpose |
| --- | --- |
| `dublin.R` | Wind rose, polar maps, trajectory case study figure and scatter plot |
| `SQTBA.R` | SQTBA and trajectory frequency maps for 2022 |
| `outputs/` | Saved figures |
| `Dublin.Rproj` | RStudio project |

Not included (see `.gitignore`): `data_setup.R`, `data/` and `hysplit/`. The analysis cannot be rerun from this repo alone.

## Data

* **Air quality:** hourly NO2, O3, SO2, PM10 and PM2.5 at Rathmines, downloaded as CSV files from the EPA's AirQuality.ie.
* **Meteorology:** hourly wind at Dublin Airport (GHCN station EII0000EIDW), imported with `worldmet::import_ghcn_hourly()`.
* **Cleaning:** PM10 and PM2.5 below -50 and SO2 above 1000 are set to missing. All timestamps are UTC.

`data_setup.R` merges these into one data frame, `rathAQMET`, which both scripts read from `data/rathAQMET.rds`.

## Back trajectories

* HYSPLIT with NCEP/NCAR reanalysis meteorology, 72 hours back, arrival height 500 m, one trajectory every 3 hours at the site.
* The two case studies were run on NOAA's READY web tool.
* The 2022 set for SQTBA was run locally with `run_hysplit()` (openair book, Appendix C) and saved as `hysplit/sqtba/TrajData/traj_2022.rds`. SQTBA is calculated with `openair::trajLevel()`.

## Packages

R 4.6.1 with:

```r
library(tidyverse)
library(openair)
library(openairmaps)
library(worldmet)
library(leaflet)
library(htmltools)
library(patchwork)
library(webshot2)
library(png)
library(htmlwidgets)
library(devtools)
```

## Caveats

* Wind direction is only a proxy for air mass origin, and each case study is a single event from a different year.
* Rathmines is an urban site, so local emissions contribute to both pollutants.
* SQTBA gives every point on a trajectory the PM2.5 measured at arrival. The high values off the Norwegian coast come from one event (10 to 11 December 2022) and are treated as an outlier.
* The reanalysis meteorology has a 2.5 degree resolution, which is coarse for the Irish Sea.

## References

* Stein, A.F., Draxler, R.R., Rolph, G.D., Stunder, B.J.B., Cohen, M.D. and Ngan, F. (2015). NOAA's HYSPLIT atmospheric transport and dispersion modeling system. *Bulletin of the American Meteorological Society*, 96. http://dx.doi.org/10.1175/BAMS-D-14-00110.1
* Rolph, G., Stein, A. and Stunder, B. (2017). Real-time Environmental Applications and Display sYstem: READY. *Environmental Modelling & Software*, 95. https://doi.org/10.1016/j.envsoft.2017.06.025
* The openair book: https://bookdown.org/david_carslaw/openair/
