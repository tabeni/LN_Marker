# Settings that depend on the PC or on the data release, in one place.
#
# Every script sources this file first (with chdir = TRUE, so the paths below
# are relative to R/). Precedence for each setting:
#   1. an environment variable of the same name, if it is set;
#   2. the value in R/config_local.R, if that file exists (not tracked by git;
#      copy R/config_local_example.R to start one);
#   3. the default below.
# The scripts read the result with Sys.getenv(), so nothing else needs editing.

v_ln_config_default <- c(
  # Data release: the scripts read data/<model><LN_DATA_VERSION>.csv.
  # The paper figures and numbers (ERL revision) use 20260803.
  LN_DATA_VERSION = "20260803",
  # Folder for the composed paper figures; "" = output/figure/paper.
  LN_FIG_DIR = "",
  # GAMS system folder (holds gdxdump.exe); "" = found from the PATH.
  # Only main_y.R needs it, for the AIM land-use maps.
  LN_GAMS_DIR = "",
  # AIM/PLUM GDX files for the land-use maps (PLUM output/gdx/results);
  # "" = data/.
  LN_PLUM_GDX_DIR = "",
  # Map_GIJ.gdx (AIMPLUM/data); "" = data/Map_GIJ.gdx.
  LN_MAP_GIJ = ""
)

v_ln_config_local <- list()
if (file.exists("config_local.R")) {
  e_ln_config <- new.env()
  sys.source("config_local.R", envir = e_ln_config)
  v_ln_config_local <- as.list(e_ln_config)
  v_unknown <- setdiff(names(v_ln_config_local), names(v_ln_config_default))
  if (length(v_unknown) > 0) {
    warning("config_local.R: unknown setting(s) ignored: ", paste(v_unknown, collapse = ", "))
  }
}

for (v_key in names(v_ln_config_default)) {
  if (Sys.getenv(v_key) != "") next
  v_value <- v_ln_config_local[[v_key]]
  if (is.null(v_value)) v_value <- v_ln_config_default[[v_key]]
  if (nzchar(v_value)) do.call(Sys.setenv, setNames(list(as.character(v_value)), v_key))
}

if (Sys.getenv("LN_GAMS_DIR") == "" && nzchar(Sys.which("gdxdump"))) {
  Sys.setenv(LN_GAMS_DIR = dirname(Sys.which("gdxdump")))
}

message(
  "LN_Marker settings: ",
  paste(names(v_ln_config_default), Sys.getenv(names(v_ln_config_default)), sep = "=", collapse = "; ")
)
rm(v_ln_config_local, v_key, v_value)
