# LN_Marker

Analysis and figure scripts for the AIM Low-to-Negative (LN) marker scenario of ScenarioMIP-CMIP7
(Fujimori et al., *Environmental Research Letters*, ERL-126126).

## Setup on a new PC

1. **Data.** Put the data folder (or a link to it) at `data/` in the repository root. The scripts
   read, among others, `data/<model><version>.csv` for the seven models, `data/climate-assessment/`
   (including `climate-assessment/marker/`) and the AR6 database files. `data/` is not tracked by git.
2. **Settings.** Copy `R/config_local_example.R` to `R/config_local.R` and edit it. This is the only
   file with PC-dependent values; it is not tracked by git.

   | Setting | Meaning | Default (`R/config.R`) |
   |---|---|---|
   | `LN_DATA_VERSION` | data release: the scripts read `data/<model><version>.csv` | `20260803` (used for the paper) |
   | `LN_FIG_DIR` | folder for the composed paper figures | `output/figure/paper` |
   | `LN_GAMS_DIR` | GAMS system folder holding `gdxdump.exe` (land-use maps only) | found from the PATH |
   | `LN_PLUM_GDX_DIR` | AIM/PLUM GDX files, PLUM `output/gdx/results` (land-use maps only) | `data/` |
   | `LN_MAP_GIJ` | `Map_GIJ.gdx`, in `AIMPLUM/data` (land-use maps only) | `data/Map_GIJ.gdx` |

   An environment variable of the same name overrides `config_local.R`, which overrides the
   default. Each script prints the values it uses (`LN_Marker settings: ...`).
3. **R packages.** tidyverse, data.table, openxlsx, scales, patchwork, ragg; for the land-use
   maps also gdxrrw and maps.

## Running

Run each script from the `R/` folder, for example on Windows:

```
cd R
& "C:\Program Files\R\R-4.4.0\bin\Rscript.exe" main.R
```

| Script | Paper figures | Supplementary figures |
|---|---|---|
| `R/main.R` | 2, 3, 4, 8 | 1, 2, 5 |
| `R/main_y.R` | 5 | 3 (land-use maps; skipped if the GDX files are not found) |
| `R/air_pollution.R` | 6 | |
| `R/main_z.R` | 7 | 4 |

`R/style_common.R` holds the shared style (theme, palettes, panel builders, saving at 17 cm
width as PDF and 600 dpi PNG). Scenario colours come from `define/define.csv`
(`define/define_z.csv` for `main_z.R`). The exploratory figures written to `output/figure/main/`
are not the paper figures.
