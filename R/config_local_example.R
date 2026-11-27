# PC-specific settings. Copy this file to R/config_local.R and edit the values.
# config_local.R is ignored by git, so each PC keeps its own copy.
# Delete a line (or leave "") to use the default in R/config.R.
# An environment variable of the same name overrides the value here.

# Data release: the scripts read data/<model><LN_DATA_VERSION>.csv
LN_DATA_VERSION <- "20260803"

# Folder for the composed paper figures ("" = output/figure/paper)
LN_FIG_DIR <- ""

# GAMS system folder holding gdxdump.exe ("" = found from the PATH)
LN_GAMS_DIR <- "C:/GAMS/33"

# AIM/PLUM GDX files for the land-use maps (main_y.R only; "" = data/)
LN_PLUM_GDX_DIR <- "X:/Individual/sfujimori/Model/IntTool/IntTool/250903ScenarioMIP/IntTool/Model/PLUM/output/gdx/results"

# Map_GIJ.gdx for the land-use maps (main_y.R only; "" = data/Map_GIJ.gdx)
LN_MAP_GIJ <- "X:/Individual/sfujimori/Model/IntTool/IntTool/250903ScenarioMIP/IntTool/Model/PLUM/AIMPLUM/data/Map_GIJ.gdx"
