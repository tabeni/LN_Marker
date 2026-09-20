# ScenarioMIP marker analysis
# Author: Osamu Nishiura
# Created: 2026-04-05
#
# This script imports ScenarioMIP and reference datasets, derives comparison
# indicators, and writes all tables and publication figures to the configured
# output directory. Data transformations and plot generation run in sequence.

# Path configuration -----------------------------------------------------------

v_command_args <- commandArgs(trailingOnly = FALSE)
v_file_arg <- grep("^--file=", v_command_args, value = TRUE)
v_script_dir <- if (length(v_file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", v_file_arg[1]), mustWork = TRUE))
} else {
  normalizePath(getwd(), mustWork = TRUE)
}
v_project_dir <- normalizePath(file.path(v_script_dir, ".."), mustWork = TRUE)
v_define_dir <- file.path(v_project_dir, "define")
# Default to the latest data release. Environment variables can still select
# an older release and output directory without editing this script.
v_download <- Sys.getenv("LN_MARKER_DOWNLOAD", unset = "20260915")
v_output_dir <- file.path(
  v_project_dir,
  Sys.getenv(
    "LN_MARKER_OUTPUT_DIR",
    unset = paste0("output_", v_download)
  )
)
v_data_candidates <- c(
  file.path(v_project_dir, "data"),
  file.path(v_project_dir, "..", "..", "data")
)
v_data_dir <- v_data_candidates[dir.exists(v_data_candidates)][1]
if (is.na(v_data_dir)) {
  stop("Data directory not found. Checked: ", paste(v_data_candidates, collapse = ", "))
}
v_data_dir <- normalizePath(v_data_dir, mustWork = TRUE)

v_local_library <- file.path(v_script_dir, ".r-lib")
if (dir.exists(v_local_library)) {
  .libPaths(c(v_local_library, .libPaths()))
}

# Packages ---------------------------------------------------------------------

library(tidyverse)
library(stringr)
library(openxlsx)
library(scales)

# Plot settings ----------------------------------------------------------------

theme1 <- theme(
  panel.background = element_rect(fill = "transparent", colour = "black"),
  panel.grid.major.y = element_line(color = "grey", linewidth = 0.2),
  panel.grid.major.x = element_line(color = "grey", linewidth = 0.2),
  panel.grid.minor = element_blank(),
  strip.background = element_blank(),
  legend.key = element_blank(),
  strip.text.y = element_text(size = 18),
  strip.text.x = element_text(size = 18),
  axis.title = element_text(size = 20),
  legend.title = element_text(size = 17),
  legend.text = element_text(size = 16),
  legend.direction = "horizontal",
  legend.position = "bottom",
  legend.box = "horizontal",
  legend.key.width = grid::unit(16, "pt"),
  legend.key.height = grid::unit(10, "pt"),
  legend.spacing.x = grid::unit(4, "pt"),
  legend.spacing.y = grid::unit(0, "pt"),
  legend.box.spacing = grid::unit(2, "pt"),
  legend.margin = margin(2, 0, 0, 0),
  plot.title = element_text(size = 20),
  axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 15),
  axis.text.y = element_text(size = 15),
  panel.spacing = grid::unit(4, "pt"),
  plot.margin = margin(4, 4, 4, 4)
)

# Output directories -----------------------------------------------------------

dir.create(file.path(v_output_dir, "figure", "main"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(v_output_dir, "table", "main"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(v_output_dir, "figure", "other"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(v_output_dir, "table", "other"), recursive = TRUE, showWarnings = FALSE)
v_path <- c(
  fig = file.path(v_output_dir, "figure"),
  fig_main = file.path(v_output_dir, "figure", "main"),
  fig_other = file.path(v_output_dir, "figure", "other"),
  tab = file.path(v_output_dir, "table"),
  tab_main = file.path(v_output_dir, "table", "main")
)

# Definitions ------------------------------------------------------------------

df_define <- read_csv(file.path(v_define_dir, "define.csv"), locale = locale(encoding = "shift-jis"), show_col_types = FALSE) %>%
  mutate(year1 = as.character(year1))
names(df_define$color_scenario) <- df_define$scenario1
names(df_define$color_model) <- df_define$model2
names(df_define$color_ar6) <- df_define$ar6_database

df_variable <- read_csv(file.path(v_define_dir, "variable.csv"), locale = locale(encoding = "shift-jis"), show_col_types = FALSE)

# ScenarioMIP data -------------------------------------------------------------

df_snap <- data.frame()
for (i in df_define$model4[!is.na(df_define$model4)]) {
  df_snap <- df_snap %>%
    bind_rows(read.csv(file.path(v_data_dir, paste0(i, v_download, ".csv")), header = T))
}

df_snap <- df_snap %>%
  filter(str_detect(Variable, paste(df_define$filter_variable, collapse = "|"))) %>%
  select(Model, Scenario, Region, Variable, Unit, X2020, X2025, X2030, X2035, X2040, X2045, X2050, X2055, X2060, X2065, X2070, X2075, X2080, X2085, X2090, X2095, X2100) %>%
  pivot_longer(cols = -c(Model, Scenario, Region, Variable, Unit), names_to = "Year", values_to = "Value", names_prefix = "X") %>%
  inner_join(select(df_define, model, scenario, scenario_category, SSP),
    by = c("Model" = "model", "Scenario" = "scenario")
  ) %>%
  mutate(Scenario_SSP = paste(SSP, scenario_category, sep = "_")) %>%
  inner_join(select(df_define, model1, model2, model3), by = c("Model" = "model1")) %>%
  mutate(
    Model = model2,
    Model_Initial = model3,
    Scenario = scenario_category
  ) %>%
  select(-c("model2", "model3", "scenario_category")) %>%
  mutate(Value = case_when(
    Unit == "Mt CO2/yr" ~ Value / 1000,
    Unit == "Mt CO2-equiv/yr" ~ Value / 1000,
    TRUE ~ Value
  )) %>%
  mutate(Unit = case_when(
    Unit == "Mt CO2/yr" ~ "Gt CO2/yr",
    Unit == "Mt CO2-equiv/yr" ~ "Gt CO2-equiv/yr",
    TRUE ~ Unit
  )) %>%
  filter(Region %in% c("World", df_define$region5))

df_snap <- df_snap %>%
  bind_rows(df_snap %>%
    filter(Variable == "GDP|MER") %>%
    left_join(df_snap %>%
      filter(Variable == "GDP|MER", Scenario_SSP == "SSP2_M") %>%
      select(Model, BaUVal = Value, Year, Region)) %>%
    mutate(
      Value = (Value - BaUVal) * 100 / BaUVal,
      Unit = "%",
      Variable = "GDP change from SSP2_M"
    ) %>%
    select(-BaUVal)) %>%
  filter(Model == "AIM" | Scenario_SSP == "SSP2_LN")

df_snap <- df_snap %>%
  bind_rows(df_snap %>%
    filter(Variable %in% df_variable$air_pollutant_energy[!is.na(df_variable$air_pollutant_energy)]) %>%
    left_join(df_variable %>% select(air_pollutant, air_pollutant_energy), by = c("Variable" = "air_pollutant_energy")) %>%
    left_join(
      df_snap %>%
        filter(Variable %in% df_variable$air_pollutant[!is.na(df_variable$air_pollutant)]) %>%
        select(Region, Unit, Variable, Year, Model, Scenario_SSP, tot = Value),
      by = c(
        "air_pollutant" = "Variable",
        "Model" = "Model",
        "Scenario_SSP" = "Scenario_SSP",
        "Region" = "Region",
        "Unit" = "Unit",
        "Year" = "Year"
      )
    ) %>%
    mutate(
      Value = Value * 100 / tot,
      Variable = paste0(Variable, "/total"),
      Unit = "%"
    ) %>%
    select(-c(air_pollutant, tot)))

# Derived cumulative, indexed, and change variables ----------------------------
df_snap <- df_snap %>%
  bind_rows(df_snap %>%
    filter(Model == "AIM" | Model == "GCAM" | Model == "WITCH") %>%
    mutate(
      Year = paste("X", Year, sep = ""),
      Variable = paste(Variable, "cumulative", sep = "|")
    ) %>%
    pivot_wider(names_from = Year, values_from = Value) %>%
    mutate(X2100 = (X2020 / 2 + X2025 + X2030 + X2035 + X2040 + X2045 + X2050 + X2055 + X2060 + X2065 + X2070 + X2075 + X2080 + X2085 + X2090 + X2095 + X2100 / 2) * 5) %>%
    mutate(X2090 = (X2020 / 2 + X2025 + X2030 + X2035 + X2040 + X2045 + X2050 + X2055 + X2060 + X2065 + X2070 + X2075 + X2080 + X2085 + X2090 / 2) * 5) %>%
    mutate(X2080 = (X2020 / 2 + X2025 + X2030 + X2035 + X2040 + X2045 + X2050 + X2055 + X2060 + X2065 + X2070 + X2075 + X2080 / 2) * 5) %>%
    mutate(X2070 = (X2020 / 2 + X2025 + X2030 + X2035 + X2040 + X2045 + X2050 + X2055 + X2060 + X2065 + X2070 / 2) * 5) %>%
    mutate(X2060 = (X2020 / 2 + X2025 + X2030 + X2035 + X2040 + X2045 + X2050 + X2055 + X2060 / 2) * 5) %>%
    mutate(X2050 = (X2020 / 2 + X2025 + X2030 + X2035 + X2040 + X2045 + X2050 / 2) * 5) %>%
    mutate(X2040 = (X2020 / 2 + X2025 + X2030 + X2035 + X2040 / 2) * 5) %>%
    mutate(X2030 = (X2020 / 2 + X2025 + X2030 / 2) * 5) %>%
    mutate(X2020 = (X2020)) %>%
    pivot_longer(cols = -c(Model, Model_Initial, Region, Variable, Unit, Scenario, SSP, Scenario_SSP), names_to = "Year", values_to = "Value") %>%
    filter(Year == "X2020" | Year == "X2030" | Year == "X2040" | Year == "X2050" | Year == "X2060" | Year == "X2070" | Year == "X2080" | Year == "X2090" | Year == "X2100") %>%
    mutate(Year = case_when(
      Year == "X2020" ~ "2020",
      Year == "X2030" ~ "2030",
      Year == "X2040" ~ "2040",
      Year == "X2050" ~ "2050",
      Year == "X2060" ~ "2060",
      Year == "X2070" ~ "2070",
      Year == "X2080" ~ "2080",
      Year == "X2090" ~ "2090",
      Year == "X2100" ~ "2100"
    )) %>%
    mutate(Unit = str_replace_all(Unit, pattern = "/yr", replacement = ""))) %>%
  bind_rows(df_snap %>%
    filter(Model == "MESSAGEix-GLOBIOM" | Model == "REMIND-MAgPIE") %>%
    mutate(
      Year = paste("X", Year, sep = ""),
      Variable = paste(Variable, "cumulative", sep = "|")
    ) %>%
    pivot_wider(names_from = Year, values_from = Value) %>%
    mutate(X2100 = (X2020 / 2 + X2025 + X2030 + X2035 + X2040 + X2045 + X2050 + X2055 + X2060 / 2) * 5 + (X2060 / 2 + X2070 + X2080 + X2090 + X2100 / 2) * 10) %>%
    mutate(X2090 = (X2020 / 2 + X2025 + X2030 + X2035 + X2040 + X2045 + X2050 + X2055 + X2060 / 2) * 5 + (X2060 / 2 + X2070 + X2080 + X2090 / 2) * 10) %>%
    mutate(X2080 = (X2020 / 2 + X2025 + X2030 + X2035 + X2040 + X2045 + X2050 + X2055 + X2060 / 2) * 5 + (X2060 / 2 + X2070 + X2080 / 2) * 10) %>%
    mutate(X2070 = (X2020 / 2 + X2025 + X2030 + X2035 + X2040 + X2045 + X2050 + X2055 + X2060 / 2) * 5 + (X2060 / 2 + X2070 / 2) * 10) %>%
    mutate(X2060 = (X2020 / 2 + X2025 + X2030 + X2035 + X2040 + X2045 + X2050 + X2055 + X2060 / 2) * 5) %>%
    mutate(X2050 = (X2020 / 2 + X2025 + X2030 + X2035 + X2040 + X2045 + X2050 / 2) * 5) %>%
    mutate(X2040 = (X2020 / 2 + X2025 + X2030 + X2035 + X2040 / 2) * 5) %>%
    mutate(X2030 = (X2020 / 2 + X2025 + X2030 / 2) * 5) %>%
    mutate(X2020 = (X2020)) %>%
    pivot_longer(cols = -c(Model, Model_Initial, Region, Variable, Unit, Scenario, SSP, Scenario_SSP), names_to = "Year", values_to = "Value") %>%
    filter(Year == "X2020" | Year == "X2030" | Year == "X2040" | Year == "X2050" | Year == "X2060" | Year == "X2070" | Year == "X2080" | Year == "X2090" | Year == "X2100") %>%
    mutate(Year = case_when(
      Year == "X2020" ~ "2020",
      Year == "X2030" ~ "2030",
      Year == "X2040" ~ "2040",
      Year == "X2050" ~ "2050",
      Year == "X2060" ~ "2060",
      Year == "X2070" ~ "2070",
      Year == "X2080" ~ "2080",
      Year == "X2090" ~ "2090",
      Year == "X2100" ~ "2100"
    )) %>%
    mutate(Unit = str_replace_all(Unit, pattern = "/yr", replacement = ""))) %>%
  bind_rows(df_snap %>%
    filter(Model == "IMAGE" | Model == "COFFEE") %>%
    mutate(
      Year = paste("X", Year, sep = ""),
      Variable = paste(Variable, "cumulative", sep = "|")
    ) %>%
    pivot_wider(names_from = Year, values_from = Value) %>%
    mutate(X2100 = (X2020 / 2 + X2025 + X2030 + X2035 + X2040 + X2045 + X2050 / 2) * 5 + (X2050 / 2 + X2060 + X2070 + X2080 + X2090 + X2100 / 2) * 10) %>%
    mutate(X2090 = (X2020 / 2 + X2025 + X2030 + X2035 + X2040 + X2045 + X2050 / 2) * 5 + (X2050 / 2 + X2060 + X2070 + X2080 + X2090 / 2) * 10) %>%
    mutate(X2080 = (X2020 / 2 + X2025 + X2030 + X2035 + X2040 + X2045 + X2050 / 2) * 5 + (X2050 / 2 + X2060 + X2070 + X2080 / 2) * 10) %>%
    mutate(X2070 = (X2020 / 2 + X2025 + X2030 + X2035 + X2040 + X2045 + X2050 / 2) * 5 + (X2050 / 2 + X2060 + X2070 / 2) * 10) %>%
    mutate(X2060 = (X2020 / 2 + X2025 + X2030 + X2035 + X2040 + X2045 + X2050 / 2) * 5 + (X2050 / 2 + X2060 / 2) * 10) %>%
    mutate(X2050 = (X2020 / 2 + X2025 + X2030 + X2035 + X2040 + X2045 + X2050 / 2) * 5) %>%
    mutate(X2040 = (X2020 / 2 + X2025 + X2030 + X2035 + X2040 / 2) * 5) %>%
    mutate(X2030 = (X2020 / 2 + X2025 + X2030 / 2) * 5) %>%
    mutate(X2020 = (X2020)) %>%
    pivot_longer(cols = -c(Model, Model_Initial, Region, Variable, Unit, Scenario, SSP, Scenario_SSP), names_to = "Year", values_to = "Value") %>%
    filter(Year == "X2020" | Year == "X2030" | Year == "X2040" | Year == "X2050" | Year == "X2060" | Year == "X2070" | Year == "X2080" | Year == "X2090" | Year == "X2100") %>%
    mutate(Year = case_when(
      Year == "X2020" ~ "2020",
      Year == "X2030" ~ "2030",
      Year == "X2040" ~ "2040",
      Year == "X2050" ~ "2050",
      Year == "X2060" ~ "2060",
      Year == "X2070" ~ "2070",
      Year == "X2080" ~ "2080",
      Year == "X2090" ~ "2090",
      Year == "X2100" ~ "2100"
    )) %>%
    mutate(Unit = str_replace_all(Unit, pattern = "/yr", replacement = ""))) %>%
  bind_rows(df_snap %>%
    mutate(
      Year = paste("X", Year, sep = ""),
      Variable = paste(Variable, "[2020=1]", sep = "")
    ) %>%
    pivot_wider(names_from = Year, values_from = Value) %>%
    mutate(X2100 = (X2100 / X2020)) %>%
    mutate(X2090 = (X2090 / X2020)) %>%
    mutate(X2080 = (X2080 / X2020)) %>%
    mutate(X2070 = (X2070 / X2020)) %>%
    mutate(X2060 = (X2060 / X2020)) %>%
    mutate(X2050 = (X2050 / X2020)) %>%
    mutate(X2040 = (X2040 / X2020)) %>%
    mutate(X2030 = (X2030 / X2020)) %>%
    mutate(X2020 = (X2020 / X2020)) %>%
    filter(!is.na(X2020) | X2020 == 0) %>%
    pivot_longer(cols = -c(Model, Model_Initial, Region, Variable, Unit, Scenario, SSP, Scenario_SSP), names_to = "Year", values_to = "Value") %>%
    filter(Year == "X2020" | Year == "X2030" | Year == "X2040" | Year == "X2050" | Year == "X2060" | Year == "X2070" | Year == "X2080" | Year == "X2090" | Year == "X2100") %>%
    mutate(Year = case_when(
      Year == "X2020" ~ "2020",
      Year == "X2030" ~ "2030",
      Year == "X2040" ~ "2040",
      Year == "X2050" ~ "2050",
      Year == "X2060" ~ "2060",
      Year == "X2070" ~ "2070",
      Year == "X2080" ~ "2080",
      Year == "X2090" ~ "2090",
      Year == "X2100" ~ "2100"
    ))) %>%
  bind_rows(df_snap %>%
    mutate(
      Year = paste("X", Year, sep = ""),
      Variable = paste(Variable, "[change from 2020]", sep = "")
    ) %>%
    pivot_wider(names_from = Year, values_from = Value) %>%
    mutate(X2100 = (X2100 - X2020)) %>%
    mutate(X2090 = (X2090 - X2020)) %>%
    mutate(X2080 = (X2080 - X2020)) %>%
    mutate(X2070 = (X2070 - X2020)) %>%
    mutate(X2060 = (X2060 - X2020)) %>%
    mutate(X2050 = (X2050 - X2020)) %>%
    mutate(X2040 = (X2040 - X2020)) %>%
    mutate(X2030 = (X2030 - X2020)) %>%
    mutate(X2020 = (X2020 - X2020)) %>%
    filter(!is.na(X2020) | X2020 == 0) %>%
    pivot_longer(cols = -c(Model, Model_Initial, Region, Variable, Unit, Scenario, SSP, Scenario_SSP), names_to = "Year", values_to = "Value") %>%
    filter(Year == "X2020" | Year == "X2030" | Year == "X2040" | Year == "X2050" | Year == "X2060" | Year == "X2070" | Year == "X2080" | Year == "X2090" | Year == "X2100") %>%
    mutate(Year = case_when(
      Year == "X2020" ~ "2020",
      Year == "X2030" ~ "2030",
      Year == "X2040" ~ "2040",
      Year == "X2050" ~ "2050",
      Year == "X2060" ~ "2060",
      Year == "X2070" ~ "2070",
      Year == "X2080" ~ "2080",
      Year == "X2090" ~ "2090",
      Year == "X2100" ~ "2100"
    ))) %>%
  na.omit()

# Optional climate assessment data ---------------------------------------------
for (i in df_define$model1[!is.na(df_define$model1)]) {
  v_climate_file <- file.path(
    v_data_dir, "climate-assessment", i,
    paste0("assessed-warming-timeseries-quantiles_", i, ".csv")
  )
  if (!file.exists(v_climate_file)) {
    warning("Skipping missing climate assessment file: ", v_climate_file)
    next
  }
  df_snap <- df_snap %>%
    bind_rows(read.csv(v_climate_file, header = T) %>%
      mutate(Variable = paste(variable, " ", round(quantile * 100, 1), "th", sep = "")) %>%
      select(Model = model, Scenario = scenario, Region = region, Variable, Unit = unit, X2020, X2025, X2030, X2035, X2040, X2045, X2050, X2055, X2060, X2065, X2070, X2075, X2080, X2085, X2090, X2095, X2100) %>%
      pivot_longer(cols = -c(Model, Scenario, Region, Variable, Unit), names_to = "Year", values_to = "Value", names_prefix = "X") %>%
      inner_join(select(df_define, model, scenario, scenario_category, SSP),
        by = c("Model" = "model", "Scenario" = "scenario")
      ) %>%
      mutate(Scenario_SSP = paste(SSP, scenario_category, sep = "_")) %>%
      inner_join(select(df_define, model1, model2, model3), by = c("Model" = "model1")) %>%
      mutate(
        Model = model2,
        Model_Initial = model3,
        Scenario = scenario_category
      ) %>%
      select(-c("model2", "model3", "scenario_category")) %>%
      filter(Model == "AIM" | Scenario_SSP == "SSP2_LN"))
}

# Add the crop category not reported separately in the source data.
v_crop_components <- c(
  "Agricultural Production|Crops",
  "Agricultural Production|Crops|Cereals",
  "Agricultural Production|Crops|Oil Crops",
  "Agricultural Production|Crops|Sugar Crops"
)
df_agricultural_other <- df_snap %>%
  filter(Variable %in% v_crop_components) %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  mutate(
    Value = .data[["Agricultural Production|Crops"]] -
      .data[["Agricultural Production|Crops|Cereals"]] -
      .data[["Agricultural Production|Crops|Oil Crops"]] -
      .data[["Agricultural Production|Crops|Sugar Crops"]],
    Variable = "Agricultural Production|Crops|Other Crops"
  ) %>%
  select(Model, Model_Initial, Region, Variable, Unit, Scenario, SSP, Scenario_SSP, Year, Value)
df_snap <- bind_rows(df_snap, df_agricultural_other)

# Reference datasets -----------------------------------------------------------
df_AR6 <- read.csv(file.path(v_data_dir, "AR6_Scenario_Database.csv"), header = T) %>%
  filter(str_detect(Variable, paste(df_define$filter_variable, collapse = "|"))) %>%
  pivot_longer(cols = !c(Model, Scenario, Region, Variable, Unit), names_to = "Year", values_to = "Value", names_prefix = "X") %>%
  filter(!(Value %in% NA)) %>%
  left_join(read.xlsx(file.path(v_data_dir, "AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx"), sheet = "meta_Ch3vetted_withclimate") %>%
    select("Model", "Scenario", "Category")) %>%
  filter(!(Category %in% NA)) %>%
  filter(Year %in% df_define$year1) %>%
  mutate(Value = case_when(
    Unit == "Mt CO2/yr" ~ Value / 1000,
    Unit == "Mt CO2-equiv/yr" ~ Value / 1000,
    TRUE ~ Value
  )) %>%
  mutate(Unit = case_when(
    Unit == "Mt CO2/yr" ~ "Gt CO2/yr",
    Unit == "Mt CO2-equiv/yr" ~ "Gt CO2-equiv/yr",
    TRUE ~ Unit
  )) %>%
  mutate(Unit = case_when(
    Unit == "US$2010/t CO2" ~ "USD_2010/t CO2",
    TRUE ~ Unit
  ))

df_AR6 <- df_AR6 %>%
  bind_rows(df_AR6 %>%
    filter(Variable == "Carbon Sequestration|Land Use" |
      Variable == "Carbon Sequestration|CCS|Biomass" |
      Variable == "Carbon Sequestration|Direct Air Capture" |
      Variable == "Carbon Sequestration|Enhanced Weathering") %>%
    group_by(Model, Region, Unit, Year, Scenario, Category) %>%
    summarize(Value = sum(abs(Value))) %>%
    ungroup() %>%
    mutate(Variable = "Carbon Removal")) %>%
  bind_rows(df_AR6 %>%
    filter(Variable == "Carbon Sequestration|Land Use") %>%
    group_by(Model, Region, Unit, Year, Scenario, Category) %>%
    summarize(Value = sum(abs(Value))) %>%
    ungroup() %>%
    mutate(Variable = "Carbon Removal|Conventional")) %>%
  bind_rows(df_AR6 %>%
    filter(Variable == "Carbon Sequestration|CCS|Biomass" |
      Variable == "Carbon Sequestration|Direct Air Capture" |
      Variable == "Carbon Sequestration|Enhanced Weathering") %>%
    group_by(Model, Region, Unit, Year, Scenario, Category) %>%
    summarize(Value = sum(abs(Value))) %>%
    ungroup() %>%
    mutate(Variable = "Carbon Removal|Novel")) %>%
  bind_rows(df_AR6 %>%
    filter(Variable == "Carbon Sequestration|CCS" |
      Variable == "Carbon Sequestration|Direct Air Capture") %>%
    group_by(Model, Region, Unit, Year, Scenario, Category) %>%
    summarize(Value = sum(abs(Value))) %>%
    ungroup() %>%
    mutate(Variable = "Carbon Capture|Geological Storage")) %>%
  replace(is.na(.), 0)

df_AR6 <- df_AR6 %>%
  bind_rows(df_AR6 %>%
    filter(Variable %in% df_variable$air_pollutant_energy[!is.na(df_variable$air_pollutant_energy)]) %>%
    left_join(df_variable %>% select(air_pollutant, air_pollutant_energy), by = c("Variable" = "air_pollutant_energy")) %>%
    left_join(
      df_AR6 %>%
        filter(Variable %in% df_variable$air_pollutant[!is.na(df_variable$air_pollutant)]) %>%
        select(Region, Unit, Variable, Year, Model, Scenario, Category, tot = Value),
      by = c(
        "air_pollutant" = "Variable",
        "Model" = "Model",
        "Scenario" = "Scenario",
        "Category" = "Category",
        "Region" = "Region",
        "Unit" = "Unit",
        "Year" = "Year"
      )
    ) %>%
    mutate(
      Value = Value * 100 / tot,
      Variable = paste0(Variable, "/total"),
      Unit = "%"
    ) %>%
    select(-c(air_pollutant, tot)))

df_AR6 <- df_AR6 %>%
  bind_rows(df_AR6 %>%
    mutate(
      Year = paste("X", Year, sep = ""),
      Variable = paste(Variable, "[change from 2020]", sep = "")
    ) %>%
    pivot_wider(names_from = Year, values_from = Value) %>%
    mutate(X2100 = (X2100 - X2020)) %>%
    mutate(X2090 = (X2090 - X2020)) %>%
    mutate(X2080 = (X2080 - X2020)) %>%
    mutate(X2070 = (X2070 - X2020)) %>%
    mutate(X2060 = (X2060 - X2020)) %>%
    mutate(X2050 = (X2050 - X2020)) %>%
    mutate(X2040 = (X2040 - X2020)) %>%
    mutate(X2030 = (X2030 - X2020)) %>%
    mutate(X2020 = (X2020 - X2020)) %>%
    filter(!is.na(X2020) | X2020 == 0) %>%
    pivot_longer(cols = -c(Model, Region, Variable, Unit, Scenario, Category), names_to = "Year", values_to = "Value") %>%
    filter(Year == "X2020" | Year == "X2030" | Year == "X2040" | Year == "X2050" | Year == "X2060" | Year == "X2070" | Year == "X2080" | Year == "X2090" | Year == "X2100") %>%
    mutate(Year = case_when(
      Year == "X2020" ~ "2020",
      Year == "X2030" ~ "2030",
      Year == "X2040" ~ "2040",
      Year == "X2050" ~ "2050",
      Year == "X2060" ~ "2060",
      Year == "X2070" ~ "2070",
      Year == "X2080" ~ "2080",
      Year == "X2090" ~ "2090",
      Year == "X2100" ~ "2100"
    )))

df_AR6 <- df_AR6 %>% bind_rows(df_AR6 %>%
  mutate(
    Year = paste("X", Year, sep = ""),
    Variable = paste(Variable, "cumulative", sep = "|")
  ) %>%
  pivot_wider(names_from = Year, values_from = Value) %>%
  mutate(X2100 = (X2020 / 2 + X2030 + X2040 + X2050 + X2060 + X2070 + X2080 + X2090 + X2100 / 2) * 10) %>%
  mutate(X2090 = (X2020 / 2 + X2030 + X2040 + X2050 + X2060 + X2070 + X2080 + X2090 / 2) * 10) %>%
  mutate(X2080 = (X2020 / 2 + X2030 + X2040 + X2050 + X2060 + X2070 + X2080 / 2) * 10) %>%
  mutate(X2070 = (X2020 / 2 + X2030 + X2040 + X2050 + X2060 + X2070 / 2) * 10) %>%
  mutate(X2060 = (X2020 / 2 + X2030 + X2040 + X2050 + X2060 / 2) * 10) %>%
  mutate(X2050 = (X2020 / 2 + X2030 + X2040 + X2050 / 2) * 10) %>%
  mutate(X2040 = (X2020 / 2 + X2030 + X2040 / 2) * 10) %>%
  mutate(X2030 = (X2020 / 2 + X2030 / 2) * 10) %>%
  mutate(X2020 = (X2020)) %>%
  pivot_longer(cols = -c(Model, Region, Variable, Unit, Scenario, Category), names_to = "Year", values_to = "Value") %>%
  filter(Year == "X2020" | Year == "X2030" | Year == "X2040" | Year == "X2050" | Year == "X2060" | Year == "X2070" | Year == "X2080" | Year == "X2090" | Year == "X2100") %>%
  mutate(Year = case_when(
    Year == "X2020" ~ "2020",
    Year == "X2030" ~ "2030",
    Year == "X2040" ~ "2040",
    Year == "X2050" ~ "2050",
    Year == "X2060" ~ "2060",
    Year == "X2070" ~ "2070",
    Year == "X2080" ~ "2080",
    Year == "X2090" ~ "2090",
    Year == "X2100" ~ "2100"
  )) %>%
  mutate(Unit = str_replace_all(Unit, pattern = "/yr", replacement = "")))

df_AR6_EmiRem <- df_AR6 %>%
  filter(Variable == "Emissions|CO2|cumulative" | Variable == "Carbon Removal|Novel|cumulative" | Variable == "Carbon Removal|Conventional|cumulative") %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  filter(Year == "2100", Category == "C1" | Category == "C2" | Category == "C3" | Category == "C4")

df_AR6 <- df_AR6 %>%
  bind_rows(df_AR6 %>%
    filter(Variable == "AR6 climate diagnostics|Surface Temperature (GSAT)|CICERO-SCM|50.0th Percentile" |
      Variable == "AR6 climate diagnostics|Surface Temperature (GSAT)|FaIRv1.6.2|50.0th Percentile" |
      Variable == "AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile") %>%
    mutate(Variable = "Surface Temperature (GSAT) 50th"))

df_AR6 <- df_AR6 %>%
  group_by(Category, Year, Unit, Region, Variable) %>%
  summarise(
    max = max(Value),
    min = min(Value),
    up5 = quantile(Value,
      probs = 0.95,
      na.rm = T
    ),
    up25 = quantile(Value,
      probs = 0.75,
      na.rm = T
    ),
    med = median(Value,
      na.rm = T
    ),
    lo25 = quantile(Value,
      probs = 0.25,
      na.rm = T
    ),
    lo5 = quantile(Value,
      probs = 0.05,
      na.rm = T
    )
  ) %>%
  ungroup()

v_geo_file <- file.path(v_data_dir, "gidden_et_al_geologic_carbon_storage.xlsx")
if (file.exists(v_geo_file)) {
  df_geo <- read.xlsx(v_geo_file, "country") %>%
    select(c("NODE", "Region5", "Technical_Potential", "Planetary_Limit")) %>%
    group_by(Region5) %>%
    summarize(
      Technical_Potential = 1000 * sum(Technical_Potential),
      Planetary_Limit = 1000 * sum(Planetary_Limit)
    ) %>%
    ungroup() %>%
    drop_na() %>%
    mutate(Region = case_when(
      Region5 == "R5ASIA" ~ "Asia (R5)",
      Region5 == "R5LAM" ~ "Latin America (R5)",
      Region5 == "R5MAF" ~ "Middle East & Africa (R5)",
      Region5 == "R5OECD90+EU" ~ "OECD & EU (R5)",
      Region5 == "R5REF" ~ "Reforming Economies (R5)"
    )) %>%
    select(c("Region", "Technical_Potential", "Planetary_Limit"))

  df_geo <- bind_rows(df_geo, df_geo %>%
    summarize(
      Technical_Potential = sum(Technical_Potential),
      Planetary_Limit = sum(Planetary_Limit)
    ) %>%
    mutate(Region = "World"))
} else {
  warning("Skipping missing geologic carbon storage file: ", v_geo_file)
  df_geo <- tibble(
    Region = character(),
    Technical_Potential = numeric(),
    Planetary_Limit = numeric()
  )
}
df_CCS <- read.csv(file.path(v_data_dir, "CCS_ref.csv"), header = T) %>%
  mutate(Year = as.character(Year))

df_land <- read.csv(file.path(v_data_dir, "Inputs_LandUse_E_All_Area_Groups_NOFLAG.csv"), header = T) %>%
  select(c("Area", "Item", "Unit", "Y1990", "Y2000", "Y2010", "Y2020")) %>%
  filter(Area == "World", Unit == "1000 ha") %>%
  pivot_longer(cols = -c(Area, Item, Unit), names_to = "Year", values_to = "Value", names_prefix = "Y") %>%
  replace_na(replace = list(Value = 0))

df_land <- df_land %>%
  filter(Item == "Permanent meadows and pastures") %>%
  group_by(Area, Unit, Year) %>%
  summarize(Value = sum(Value) / 1000) %>%
  ungroup() %>%
  mutate(Unit = "million ha", Variable = "Land Cover|Pasture[change from 2020]") %>%
  bind_rows(df_land %>%
    filter(Item == "Other land") %>%
    group_by(Area, Unit, Year) %>%
    summarize(Value = sum(Value) / 1000) %>%
    ungroup() %>%
    mutate(Unit = "million ha", Variable = "Land Cover|Other Natural[change from 2020]")) %>%
  bind_rows(df_land %>%
    filter(Item == "Cropland") %>%
    group_by(Area, Unit, Year) %>%
    summarize(Value = sum(Value) / 1000) %>%
    ungroup() %>%
    mutate(Unit = "million ha", Variable = "Land Cover|Cropland|Non-Energy Crops[change from 2020]")) %>%
  bind_rows(df_land %>%
    filter(Item == "Planted Forest") %>%
    group_by(Area, Unit, Year) %>%
    summarize(Value = sum(Value) / 1000) %>%
    ungroup() %>%
    mutate(Unit = "million ha", Variable = "Land Cover|Forest|Planted[change from 2020]")) %>%
  bind_rows(df_land %>%
    filter(Item == "Forest land" | Item == "Planted Forest") %>%
    pivot_wider(names_from = Item, values_from = Value) %>%
    mutate(
      Unit = "million ha",
      Value = (`Forest land` - `Planted Forest`) / 1000,
      Variable = "Land Cover|Forest|Natural[change from 2020]"
    ) %>%
    select(-c("Forest land", "Planted Forest"))) %>%
  filter(Value != 0)

df_land <- df_land %>%
  left_join(df_land %>%
    filter(Year == "2020") %>%
    pivot_wider(names_from = Year, values_from = Value), by = c("Area", "Unit", "Variable")) %>%
  mutate(
    Value = Value - `2020`,
    Model = "FAOstat",
    Variable = factor(Variable, levels = c(
      "Land Cover|Other Natural[change from 2020]",
      "Land Cover|Forest|Natural[change from 2020]",
      "Land Cover|Forest|Planted|Plantation[change from 2020]",
      "Land Cover|Pasture[change from 2020]",
      "Land Cover|Cropland|Non-Energy Crops[change from 2020]",
      "Land Cover|Cropland|Energy Crops[change from 2020]"
    )),
    Year = factor(Year, levels = c("1970", "1980", "1990", "2000", "2010", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100"))
  )

df_dummy <- data.frame(Year = factor(df_define$year2[!is.na(df_define$year2)], df_define$year2[!is.na(df_define$year2)])) %>%
  mutate(Value = 0)

# Plot functions ---------------------------------------------------------------

# Line plots -------------------------------------------------------------------
f_fig_line1 <- function(v_name, v_var) {
  df_fig1 <- filter(
    df_snap, Variable %in% v_var,
    Model == "AIM", Region == "World",
    Scenario_SSP %in% df_define$marker_scenario[!is.na(df_define$marker_scenario)]
  ) %>%
    mutate(
      Variable = factor(Variable, levels = v_var),
      Scenario_SSP = factor(Scenario_SSP, levels = df_define$marker_scenario[!is.na(df_define$marker_scenario)]),
      Scenario = factor(Scenario, levels = df_define$scenario1[!is.na(df_define$scenario1)])
    )
  df_fig2 <- filter(
    df_snap, Variable %in% v_var, Year %in% df_define$year1,
    Scenario == "LN", Region == "World"
  ) %>%
    mutate(Variable = factor(Variable, levels = v_var))
  df_fig3 <- filter(
    df_snap, Variable %in% v_var, Year %in% df_define$year1,
    Scenario == "LN", Region == "World"
  ) %>%
    summarise(max = max(Value), min = min(Value), .by = c(Scenario, Year, Region, Variable, Unit)) %>%
    mutate(Variable = factor(Variable, levels = v_var))
  df_fig4 <- filter(df_AR6, Variable %in% v_var, Region == "World", Year == "2100") %>%
    mutate(Variable = factor(Variable, levels = v_var))
  p <- ggplot() +
    geom_point(data = df_dummy, aes(x = Year, y = Value), alpha = 0) +
    geom_line(data = df_fig1, aes(x = Year, y = Value, group = Scenario_SSP, color = Scenario), linewidth = 0.8, alpha = 0.9) +
    geom_line(data = df_fig2, aes(x = Year, y = Value, group = interaction(Scenario_SSP, Model), color = Scenario), linewidth = 0.2, alpha = 0.6, linetype = "solid") +
    geom_ribbon(data = df_fig3, aes(x = Year, ymin = min, ymax = max, group = Scenario, ), alpha = 0.1, fill = df_define$color_scenario["LN"]) +
    geom_linerange(data = df_fig4, aes(x = Category, ymin = lo5, ymax = up5, group = Category), color = "grey", alpha = 0.4, linewidth = 5, show.legend = FALSE) +
    geom_linerange(data = df_fig4, aes(x = Category, ymin = lo25, ymax = up25, group = Category), color = "grey", alpha = 0.5, linewidth = 5, show.legend = FALSE) +
    facet_wrap(Variable ~ Unit, scales = "free", nrow = 2) +
    scale_x_discrete(breaks = c(df_define$year1[!is.na(df_define$year1)], df_define$ar6_database[!is.na(df_define$ar6_database)])) +
    scale_colour_manual(values = c(df_define$color_scenario)) +
    ylab("") +
    xlab("Year") +
    theme1 +
    guides(colour = guide_legend(nrow = 1, byrow = TRUE))
  png(paste(v_path["fig_main"], "/", v_name, "_line.png", sep = ""), width = length(v_var) / 2 * 1800 + 450, height = 3200, res = 300)
  print(p)
  dev.off()
}
f_fig_line2 <- function(v_name, v_var) {
  df_fig1 <- filter(
    df_snap, Variable %in% v_var,
    Model == "AIM", Region == "World",
    Scenario_SSP %in% df_define$marker_scenario[!is.na(df_define$marker_scenario)]
  ) %>%
    mutate(
      Variable = factor(Variable, levels = v_var),
      Scenario_SSP = factor(Scenario_SSP, levels = df_define$marker_scenario[!is.na(df_define$marker_scenario)]),
      Scenario = factor(Scenario, levels = df_define$scenario1[!is.na(df_define$scenario1)])
    )
  df_fig2 <- filter(
    df_snap, Variable %in% v_var, Year %in% df_define$year1,
    Scenario == "LN", Region == "World"
  ) %>%
    mutate(Variable = factor(Variable, levels = v_var))
  df_fig3 <- filter(
    df_snap, Variable %in% v_var, Year %in% df_define$year1,
    Scenario == "LN", Region == "World"
  ) %>%
    summarise(max = max(Value), min = min(Value), .by = c(Scenario, Year, Region, Variable, Unit)) %>%
    mutate(Variable = factor(Variable, levels = v_var))
  df_fig4 <- filter(df_AR6, Variable %in% v_var, Region == "World", Year == "2100") %>%
    mutate(Variable = factor(Variable, levels = v_var))
  p <- ggplot() +
    geom_point(data = df_dummy, aes(x = Year, y = Value), alpha = 0) +
    geom_line(data = df_fig1, aes(x = Year, y = Value, group = Scenario_SSP, color = Scenario), linewidth = 0.8, alpha = 0.9) +
    geom_line(data = df_fig2, aes(x = Year, y = Value, group = interaction(Scenario_SSP, Model), color = Scenario), linewidth = 0.2, alpha = 0.6, linetype = "solid") +
    geom_ribbon(data = df_fig3, aes(x = Year, ymin = min, ymax = max, group = Scenario, ), alpha = 0.1, fill = df_define$color_scenario["LN"]) +
    geom_linerange(data = df_fig4, aes(x = Category, ymin = lo5, ymax = up5, group = Category), color = "grey", alpha = 0.4, linewidth = 5, show.legend = FALSE) +
    geom_linerange(data = df_fig4, aes(x = Category, ymin = lo25, ymax = up25, group = Category), color = "grey", alpha = 0.5, linewidth = 5, show.legend = FALSE) +
    facet_wrap(Variable ~ Unit, scales = "free", nrow = 1) +
    scale_x_discrete(breaks = c(df_define$year1[!is.na(df_define$year1)], df_define$ar6_database[!is.na(df_define$ar6_database)])) +
    scale_colour_manual(values = c(df_define$color_scenario)) +
    ylab("") +
    xlab("Year") +
    theme1 +
    guides(colour = guide_legend(nrow = 1, byrow = TRUE))
  png(paste(v_path["fig_main"], "/", v_name, "_line.png", sep = ""), width = length(v_var) * 1800 + 450, height = 1800, res = 300)
  print(p)
  dev.off()
}
f_fig_line3 <- function(v_name, v_var) {
  df_fig1 <- filter(
    df_snap, Variable %in% v_var,
    Model == "AIM", Region %in% df_define$region5,
    Scenario_SSP %in% df_define$marker_scenario[!is.na(df_define$marker_scenario)]
  ) %>%
    mutate(
      Variable = factor(Variable, levels = v_var),
      Scenario_SSP = factor(Scenario_SSP, levels = df_define$marker_scenario[!is.na(df_define$marker_scenario)]),
      Scenario = factor(Scenario, levels = df_define$scenario1[!is.na(df_define$scenario1)])
    )
  df_fig3 <- filter(
    df_snap, Variable %in% v_var, Year %in% df_define$year1,
    Scenario == "LN", Region %in% df_define$region5
  ) %>%
    summarise(max = max(Value), min = min(Value), .by = c(Scenario, Year, Region, Variable, Unit)) %>%
    mutate(Variable = factor(Variable, levels = v_var))
  p <- ggplot() +
    geom_ribbon(
      data = df_fig3,
      aes(x = Year, ymin = min, ymax = max, group = Scenario),
      alpha = 0.1, fill = df_define$color_scenario["LN"],
      show.legend = FALSE
    ) +
    geom_line(data = df_fig1, aes(x = Year, y = Value, group = Scenario_SSP, color = Scenario), linewidth = 0.8, alpha = 0.9) +
    facet_wrap(~Region, scales = "fixed", nrow = 1) +
    coord_cartesian(ylim = c(2000, NA)) +
    scale_x_discrete(breaks = c(df_define$year1[!is.na(df_define$year1)], df_define$ar6_database[!is.na(df_define$ar6_database)])) +
    scale_colour_manual(values = c(df_define$color_scenario)) +
    ylab("") +
    xlab("Year") +
    labs(title = v_var) +
    theme1 +
    theme(plot.title = element_text(hjust = 0.5)) +
    guides(colour = guide_legend(nrow = 1, byrow = TRUE))
  png(paste(v_path["fig_main"], "/", v_name, "_line_region5.png", sep = ""), width = 6000, height = 2400, res = 300)
  print(p)
  dev.off()
}
f_fig_line4 <- function(v_name, v_var) {
  df_fig1 <- filter(
    df_snap, Variable %in% v_var,
    Model == "AIM", Region == "World",
    Scenario_SSP %in% df_define$marker_scenario[!is.na(df_define$marker_scenario)]
  ) %>%
    mutate(
      Variable = factor(Variable, levels = v_var),
      Scenario_SSP = factor(Scenario_SSP, levels = df_define$marker_scenario[!is.na(df_define$marker_scenario)]),
      Scenario = factor(Scenario, levels = df_define$scenario1[!is.na(df_define$scenario1)])
    )
  df_fig2 <- filter(
    df_snap, Variable %in% v_var, Year %in% df_define$year1,
    Scenario == "LN", Region == "World"
  ) %>%
    mutate(Variable = factor(Variable, levels = v_var))
  df_fig3 <- filter(
    df_snap, Variable %in% v_var, Year %in% df_define$year1,
    Scenario == "LN", Region == "World"
  ) %>%
    summarise(max = max(Value), min = min(Value), .by = c(Scenario, Year, Region, Variable, Unit)) %>%
    mutate(Variable = factor(Variable, levels = v_var))
  df_fig4 <- filter(df_AR6, Variable %in% v_var, Region == "World", Year == "2100") %>%
    mutate(Variable = factor(Variable, levels = v_var))
  p <- ggplot() +
    geom_point(data = df_dummy, aes(x = Year, y = Value), alpha = 0) +
    geom_line(data = df_fig1, aes(x = Year, y = Value, group = Scenario_SSP, color = Scenario), linewidth = 0.8, alpha = 0.9) +
    geom_line(data = df_fig2, aes(x = Year, y = Value, group = interaction(Scenario_SSP, Model), color = Scenario), linewidth = 0.2, alpha = 0.6, linetype = "solid") +
    geom_ribbon(data = df_fig3, aes(x = Year, ymin = min, ymax = max, group = Scenario, ), alpha = 0.1, fill = df_define$color_scenario["LN"]) +
    facet_wrap(Variable ~ Unit, scales = "free", nrow = 1) +
    scale_x_discrete(breaks = c(df_define$year1[!is.na(df_define$year1)], df_define$ar6_database[!is.na(df_define$ar6_database)])) +
    scale_colour_manual(values = c(df_define$color_scenario)) +
    ylab("") +
    xlab("Year") +
    theme1 +
    guides(colour = guide_legend(nrow = 1, byrow = TRUE))
  png(paste(v_path["fig_main"], "/", v_name, "_line.png", sep = ""), width = length(v_var) * 1800 + 450, height = 1800, res = 300)
  print(p)
  dev.off()
}
f_fig_line5 <- function(v_name, v_var) {
  df_fig1 <- filter(
    df_snap, Variable %in% v_var,
    Model == "AIM", Region == "World",
    Scenario_SSP %in% df_define$marker_scenario[!is.na(df_define$marker_scenario)]
  ) %>%
    mutate(
      Variable = factor(Variable, levels = v_var),
      Scenario_SSP = factor(Scenario_SSP, levels = df_define$marker_scenario[!is.na(df_define$marker_scenario)])
    )
  df_fig2 <- filter(
    df_snap, Variable %in% v_var, Year %in% df_define$year1,
    Scenario == "LN", Region == "World"
  ) %>%
    mutate(Variable = factor(Variable, levels = v_var))
  df_fig3 <- filter(
    df_snap, Variable %in% v_var, Year %in% df_define$year1,
    Scenario == "LN", Region == "World"
  ) %>%
    summarise(max = max(Value), min = min(Value), .by = c(Scenario, Year, Region, Variable, Unit)) %>%
    mutate(Variable = factor(Variable, levels = v_var))
  df_fig4 <- filter(df_AR6, Variable %in% v_var, Region == "World", Year == "2100") %>%
    mutate(Variable = factor(Variable, levels = v_var))
  p <- ggplot() +
    geom_line(data = df_fig1, aes(x = Year, y = Value, group = Scenario_SSP, color = Scenario), linewidth = 0.8, alpha = 0.9) +
    geom_line(data = df_fig2, aes(x = Year, y = Value, group = interaction(Scenario_SSP, Model), color = Scenario), linewidth = 0.2, alpha = 0.6, linetype = "solid") +
    geom_ribbon(data = df_fig3, aes(x = Year, ymin = min, ymax = max, group = Scenario, ), alpha = 0.1, fill = df_define$color_scenario["LN"]) +
    facet_wrap(Variable ~ Unit, scales = "free", nrow = 3) +
    scale_x_discrete(breaks = c(df_define$year1[!is.na(df_define$year1)], df_define$ar6_database[!is.na(df_define$ar6_database)])) +
    scale_colour_manual(values = c(df_define$color_scenario)) +
    ylab("") +
    xlab("Year") +
    theme1 +
    guides(colour = guide_legend(nrow = 1, byrow = TRUE))
  png(paste(v_path["fig_main"], "/", v_name, "_line.png", sep = ""), width = length(v_var) / 3 * 2400 + 800, height = 5200, res = 300)
  print(p)
  dev.off()
}
# Area plots -------------------------------------------------------------------
f_fill_labels <- function(v_name, v_area) {
  if (v_name == "Agricultural_Production") {
    return(str_remove(v_area, "^.*\\|"))
  }
  if (v_name == "Land_Cover") {
    v_labels <- v_area %>%
      str_remove("\\[change from 2020\\]$") %>%
      str_remove("^Land Cover\\|") %>%
      str_remove("^Cropland\\|") %>%
      str_remove("^Forest\\|")
    v_labels[v_labels %in% c("Planted", "Primary", "Secondary")] <-
      paste(v_labels[v_labels %in% c("Planted", "Primary", "Secondary")], "Forest")
    v_labels <- str_replace(v_labels, "^Built-Up Area$", "Built-up Area")
    v_labels <- str_replace(v_labels, "^Other Natural$", "Other Natural Land")
    return(v_labels)
  }
  waiver()
}

f_fig_area <- function(v_name, v_area, v_line) {
  v_area_source <- v_area
  if (v_name == "Land_Cover") {
    v_forest_components <- c(
      "Land Cover|Forest|Planted[change from 2020]",
      "Land Cover|Forest|Primary[change from 2020]",
      "Land Cover|Forest|Secondary[change from 2020]"
    )
    v_forest_total <- "Land Cover|Forest[change from 2020]"
    # Preserve the original order, replacing the three mutually exclusive
    # forest components with one summed Forest category.
    v_area <- unique(if_else(v_area %in% v_forest_components,
      v_forest_total, v_area
    ))
  }
  v_fill_labels <- f_fill_labels(v_name, v_area)
  v_fill_title <- if (v_name == "Agricultural_Production") "Product" else if (v_name == "Land_Cover") "Land type" else "Category"
  df_fig1 <- df_snap %>%
    filter(Variable %in% v_area_source, Scenario_SSP %in% df_define$marker_scenario[!is.na(df_define$marker_scenario)], Region == "World", Model == "AIM") %>%
    mutate(Variable = if (v_name == "Land_Cover") {
      if_else(Variable %in% v_forest_components, v_forest_total, Variable)
    } else {
      Variable
    }) %>%
    group_by(across(-Value)) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      Variable = factor(Variable, levels = v_area),
      Scenario_SSP = factor(Scenario_SSP, levels = df_define$marker_scenario[!is.na(df_define$marker_scenario)]),
      Scenario = factor(Scenario, levels = df_define$scenario1[!is.na(df_define$scenario1)])
    )
  df_fig2 <- df_snap %>%
    filter(Variable %in% v_line, Scenario_SSP %in% df_define$marker_scenario[!is.na(df_define$marker_scenario)], Region == "World", Model == "AIM") %>%
    mutate(
      Variable = factor(Variable, levels = v_line),
      Scenario_SSP = factor(Scenario_SSP, levels = df_define$marker_scenario[!is.na(df_define$marker_scenario)])
    )
  p <- ggplot() +
    geom_area(data = df_fig1, aes(x = Year, y = Value, group = Variable, fill = Variable), linewidth = 1, alpha = 0.7) +
    geom_line(data = df_fig2, aes(x = Year, y = Value, group = Variable, linetype = Variable), alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "longdash", color = "black") +
    scale_fill_manual(values = df_define$color_palette, labels = v_fill_labels) +
    facet_wrap(Scenario_SSP ~ ., scales = "fixed", nrow = 1) +
    scale_x_discrete(breaks = c(df_define$year1[!is.na(df_define$year1)])) +
    ylab("") +
    xlab("") +
    labs(fill = v_fill_title, linetype = "") +
    theme1 +
    guides(
      fill = guide_legend(nrow = 2, byrow = TRUE),
      linetype = guide_legend(nrow = 1, byrow = TRUE)
    )
  scale_x_discrete(breaks = df_define$year1[!is.na(df_define$year1)])
  png(paste(v_path["fig_main"], "/", v_name, "area_.png", sep = ""), width = length(df_define$marker_scenario[!is.na(df_define$marker_scenario)]) * 800, height = 2400 + length(v_area) * 20, res = 300)
  print(p)
  dev.off()
}
# Bar plots --------------------------------------------------------------------
f_fig_bar <- function(v_name, v_area, v_point) {
  v_fill_labels <- f_fill_labels(v_name, v_area)
  v_fill_title <- if (v_name == "Agricultural_Production") "Product" else if (v_name == "Land_Cover") "Land type" else "Category"
  df_fig1 <- df_snap %>%
    filter(Year == "2050" | Year == "2100", Variable %in% v_area, Scenario_SSP %in% df_define$marker_scenario[!is.na(df_define$marker_scenario)], Region == "World", Model == "AIM") %>%
    mutate(
      Variable = factor(Variable, levels = v_area),
      Scenario_SSP_Model = factor(paste0(Model, "_", Scenario_SSP), levels = df_define$scenario_model[!is.na(df_define$scenario_model)]),
      Scenario = factor(Scenario, levels = df_define$scenario1[!is.na(df_define$scenario1)])
    )
  df_fig2 <- df_snap %>%
    filter(Year == "2050" | Year == "2100", Variable %in% v_area, Region == "World", Scenario == "LN", Model != "AIM") %>%
    mutate(
      Variable = factor(Variable, levels = v_area),
      Scenario_SSP_Model = factor(paste0(Model, "_", Scenario_SSP), levels = df_define$scenario_model[!is.na(df_define$scenario_model)])
    )
  df_fig3 <- df_snap %>%
    filter(Year == "2050" | Year == "2100", Variable %in% v_point, Region == "World", Scenario == "LN", Model != "AIM") %>%
    mutate(
      Variable = factor(Variable, levels = v_point),
      Scenario_SSP_Model = factor(paste0(Model, "_", Scenario_SSP), levels = df_define$scenario_model[!is.na(df_define$scenario_model)])
    ) %>%
    bind_rows(df_snap %>%
      filter(Year == "2050" | Year == "2100", Variable %in% v_point, Scenario_SSP %in% df_define$marker_scenario[!is.na(df_define$marker_scenario)], Region == "World", Model == "AIM") %>%
      mutate(
        Variable = factor(Variable, levels = v_point),
        Scenario_SSP_Model = factor(paste0(Model, "_", Scenario_SSP), levels = df_define$scenario_model[!is.na(df_define$scenario_model)])
      ))
  p <- ggplot() +
    geom_bar(data = df_fig1, aes(x = Scenario_SSP_Model, y = Value, group = Variable, fill = Variable), stat = "identity", alpha = 0.7) +
    geom_bar(data = df_fig2, aes(x = Scenario_SSP_Model, y = Value, group = Variable, fill = Variable), stat = "identity", alpha = 0.7) +
    geom_point(data = df_fig3, aes(x = Scenario_SSP_Model, y = Value, group = Variable, shape = Variable), alpha = 0.8, size = 4, stroke = 1.2) +
    geom_hline(yintercept = 0, linetype = "longdash", color = "black") +
    scale_fill_manual(values = df_define$color_palette, labels = v_fill_labels) +
    scale_shape_manual(values = c(4, 2, 3, 5)) +
    facet_wrap(Year ~ ., scales = "fixed", nrow = 1) +
    ylab("") +
    xlab("") +
    labs(fill = v_fill_title, linetype = "") +
    theme1 +
    guides(
      fill = guide_legend(nrow = 2, byrow = TRUE),
      shape = guide_legend(nrow = 1, byrow = TRUE)
    )
  png(paste(v_path["fig_main"], "/", v_name, "_bar.png", sep = ""), width = 5000, height = 3000, res = 300)
  print(p)
  dev.off()
}

# Global food availability by source -------------------------------------------
f_fig_food_source_area <- function() {
  v_food_total <- "Food Availability [per capita]"
  v_food_crops <- "Food Availability|Crops [per capita]"
  v_food_livestock <- "Food Availability|Livestock [per capita]"
  v_scenario_levels <- df_define$marker_scenario[!is.na(df_define$marker_scenario)]

  df_food_source <- df_snap %>%
    filter(
      Model == "AIM",
      Region == "World",
      Scenario_SSP %in% v_scenario_levels,
      Year %in% as.character(seq(2020, 2100, 5)),
      Variable %in% c(v_food_total, v_food_crops, v_food_livestock)
    ) %>%
    select(Model, Scenario_SSP, Year, Variable, Value) %>%
    pivot_wider(names_from = Variable, values_from = Value) %>%
    transmute(
      Scenario_SSP = factor(Scenario_SSP, levels = v_scenario_levels),
      Year = factor(Year, levels = as.character(seq(2020, 2100, 5))),
      Total = .data[[v_food_total]],
      Crops = .data[[v_food_crops]],
      Livestock = .data[[v_food_livestock]]
    )

  v_food_gap <- max(abs(df_food_source$Total - df_food_source$Crops - df_food_source$Livestock),
    na.rm = TRUE
  )
  if (v_food_gap > 1e-3) {
    warning("Food availability components do not sum to total; maximum gap = ", v_food_gap)
  }

  v_observed_scenarios <- v_scenario_levels[
    v_scenario_levels %in% as.character(unique(df_food_source$Scenario_SSP))
  ]
  df_food_source <- df_food_source %>%
    select(-Total) %>%
    pivot_longer(c(Crops, Livestock),
      names_to = "Source", values_to = "Value"
    ) %>%
    mutate(
      Source = factor(Source, levels = c("Crops", "Livestock")),
      Scenario_SSP = factor(Scenario_SSP, levels = v_observed_scenarios)
    )

  p <- ggplot(
    df_food_source,
    aes(x = Year, y = Value, fill = Source, group = Source)
  ) +
    geom_area(alpha = 0.85, colour = "white", linewidth = 0.15) +
    facet_wrap(~Scenario_SSP, nrow = 1, scales = "fixed") +
    scale_fill_manual(values = c(
      "Crops" = "#66A61E",
      "Livestock" = "#A6761D"
    )) +
    scale_x_discrete(breaks = as.character(seq(2020, 2100, 20))) +
    ylab("Food availability (kcal/capita/day)") +
    xlab("Year") +
    labs(fill = "Source") +
    theme1 +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 13),
      panel.spacing.x = grid::unit(16, "pt"),
      panel.spacing.y = grid::unit(8, "pt")
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE))

  png(file.path(v_path["fig_main"], "Food_Availability_source_area_world.png"),
    width = 7200, height = 2400, res = 300
  )
  print(p)
  dev.off()
}

# SSP2-LN land-cover comparison at 2050 and 2100. Forest subcategories are
# replaced by the common parent category because their reporting differs by model.
f_fig_land_cover_ln_comparison <- function() {
  v_land_reported <- c(
    "Land Cover|Built-Up Area",
    "Land Cover|Cropland|Cereals",
    "Land Cover|Cropland|Energy Crops",
    "Land Cover|Cropland|Oil Crops",
    "Land Cover|Cropland|Other Crops",
    "Land Cover|Cropland|Sugar Crops",
    "Land Cover|Forest",
    "Land Cover|Other Natural",
    "Land Cover|Pasture"
  )
  v_land_absolute <- append(
    v_land_reported,
    "Land Cover|Other/Unspecified Land",
    after = 7
  )
  v_land_labels <- c(
    "Built-up Area", "Cereals", "Energy Crops", "Oil Crops",
    "Other Crops", "Sugar Crops", "Forest", "Other / unspecified land",
    "Other Natural Land", "Pasture"
  )
  v_land_colors <- setNames(c(
    "#66BD63", "#5B9BD5", "#E83E3E", "#FF8C2A",
    "#B8DE69", "#B66D44", "#228833", "#8DB8D3",
    "#FDBF73", "#8DD3C7"
  ), v_land_labels)
  v_variable_labels <- setNames(v_land_labels, v_land_absolute)
  v_model_levels <- c(
    "AIM", "COFFEE", "GCAM", "IMAGE",
    "MESSAGEix-GLOBIOM", "REMIND-MAgPIE", "WITCH"
  )
  v_year_levels <- c("2050", "2100")

  df_land_source <- df_snap %>%
    filter(
      Scenario_SSP == "SSP2_LN",
      Region == "World",
      Model %in% v_model_levels,
      Year %in% v_year_levels,
      Variable %in% c("Land Cover", v_land_reported)
    ) %>%
    group_by(Model, Year, Variable, Unit) %>%
    summarise(Value = mean(Value), .groups = "drop")

  df_land_residual <- df_land_source %>%
    filter(Variable == "Land Cover") %>%
    select(Model, Year, Unit, Total = Value) %>%
    left_join(
      df_land_source %>%
        filter(Variable != "Land Cover") %>%
        group_by(Model, Year, Unit) %>%
        summarise(Reported = sum(Value, na.rm = TRUE), .groups = "drop"),
      by = c("Model", "Year", "Unit")
    ) %>%
    transmute(Model, Year, Unit,
      Variable = "Land Cover|Other/Unspecified Land",
      Value = pmax(Total - coalesce(Reported, 0), 0)
    )

  df_land <- df_land_source %>%
    filter(Variable != "Land Cover") %>%
    bind_rows(df_land_residual) %>%
    mutate(
      Model = factor(Model, levels = v_model_levels),
      Year = factor(Year, levels = v_year_levels),
      Land_type = factor(
        unname(v_variable_labels[Variable]),
        levels = v_land_labels
      )
    )

  p_absolute <- ggplot(
    df_land,
    aes(x = Model, y = Value, fill = Land_type, group = Land_type)
  ) +
    geom_col(
      width = 0.75, alpha = 0.85,
      colour = "white", linewidth = 0.15
    ) +
    facet_wrap(~Year, nrow = 1, scales = "fixed") +
    scale_fill_manual(values = v_land_colors, drop = FALSE) +
    scale_y_continuous(
      labels = label_number(big.mark = ","),
      expand = expansion(mult = c(0, 0.04))
    ) +
    xlab("") +
    ylab("Land cover (million ha)") +
    labs(fill = "Land type") +
    theme1 +
    theme(
      axis.text.x = element_text(
        angle = 45, hjust = 1,
        vjust = 1, size = 13
      ),
      panel.spacing.x = grid::unit(18, "pt")
    ) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))

  png(file.path(v_path["fig_main"], "Land_Cover_LN_absolute_bar_by_year.png"),
    width = 5000, height = 2800, res = 300
  )
  print(p_absolute)
  dev.off()

  df_coverage <- expand_grid(
    Model = v_model_levels,
    Land_type = v_land_labels
  )
  df_coverage <- df_coverage %>%
    left_join(
      df_land %>%
        distinct(Model, Land_type) %>%
        mutate(Reported = TRUE),
      by = c("Model", "Land_type")
    ) %>%
    mutate(Reported = replace_na(Reported, FALSE))
  write_csv(
    df_coverage,
    file.path(v_path["tab_main"], "Land_Cover_LN_category_coverage.csv")
  )
}

# AIM spatial land-use maps for 2050 and 2100 ----------------------------------
f_fig_aim_land_grid <- function(v_scenario_ids = NULL) {
  v_gams_dir <- "C:/GAMS/win64/26.1"
  df_aim_scenarios <- tribble(
    ~Scenario_ID, ~Scenario_title, ~GDX_file,
    "SSP1_M", "SSP1 - Medium Emissions",
    "analysis_SSP1_BaU_NoCC_scenarioMIP_global2.gdx",
    "SSP1_VL", "SSP1 - Very Low Emissions",
    "analysis_SSP1_600C_2020NDC-high-Sus_NoCC_scenarioMIP_global2.gdx",
    "SSP2_HL", "SSP2 - High-Low Emissions",
    "analysis_SSP2_1100C_2020NDC-medhigh_NoCC_scenarioMIP_global2.gdx",
    "SSP2_1100C_HIGH", "SSP2 1100C - 2020NDC-high (AIM internal)",
    "analysis_SSP2_1100C_2020NDC-high_NoCC_scenarioMIP_global2.gdx",
    "SSP2_M", "SSP2 - Medium Emissions",
    "analysis_SSP2_1500C_2050CP-low_NoCC_scenarioMIP_global2.gdx",
    "SSP2_ML", "SSP2 - Medium-Low Emissions",
    "analysis_SSP2_900C_2030CP-vlow_NoCC_scenarioMIP_global2.gdx",
    "SSP2_L", "SSP2 - Low Emissions",
    "analysis_SSP2_800C_2030CP-vlow_NoCC_scenarioMIP_global2.gdx",
    # AIM's published SSP2-LN name is "SSP2 - Low Overshoot_a".
    "SSP2_LN", "SSP2 - LN",
    "analysis_SSP2_700C_2030CP-vlow_NoCC_scenarioMIP_global1.gdx",
    "SSP2_VL", "SSP2 - Very Low Emissions",
    "analysis_SSP2_1000C_2020NDC-high_NoCC_scenarioMIP_global2.gdx",
    "SSP3_H", "SSP3 - High Emissions",
    "analysis_SSP3_BaU_NoCC_scenarioMIP_global2.gdx",
    "SSP5_H", "SSP5 - High Emissions",
    "analysis_SSP5_BaU_NoCC_scenarioMIP_global2.gdx"
  ) %>%
    mutate(GDX_path = file.path(v_data_dir, GDX_file))

  # Default output is restricted to the nine AIM scenarios actually present
  # in the ScenarioMIP submission CSV. The two 1100C runs remain available by
  # passing their Scenario_ID explicitly.
  v_scenariomip_ids <- c(
    "SSP1_M", "SSP1_VL", "SSP2_M", "SSP2_ML", "SSP2_L",
    "SSP2_LN", "SSP2_VL", "SSP3_H", "SSP5_H"
  )
  if (is.null(v_scenario_ids)) {
    df_aim_scenarios <- df_aim_scenarios %>%
      filter(Scenario_ID %in% v_scenariomip_ids)
  } else {
    v_unknown <- setdiff(v_scenario_ids, df_aim_scenarios$Scenario_ID)
    if (length(v_unknown) > 0) {
      stop("Unknown AIM scenario ID(s): ", paste(v_unknown, collapse = ", "))
    }
    df_aim_scenarios <- df_aim_scenarios %>%
      filter(Scenario_ID %in% v_scenario_ids)
  }

  v_missing <- df_aim_scenarios$GDX_file[!file.exists(df_aim_scenarios$GDX_path)]
  if (length(v_missing) > 0) {
    warning("Skipping missing AIM GDX file(s): ", paste(v_missing, collapse = ", "))
    df_aim_scenarios <- df_aim_scenarios %>%
      filter(file.exists(GDX_path))
  }
  if (nrow(df_aim_scenarios) == 0) {
    return(invisible(character()))
  }

  v_map_gdx <- file.path(v_data_dir, "Map_GIJ.gdx")
  if (!file.exists(v_map_gdx)) {
    warning("Skipping AIM grid maps: Map_GIJ.gdx is missing.")
    return(invisible(character()))
  }

  v_output_grid_dir <- file.path(v_path["fig_main"], "AIM_land_grid")
  dir.create(v_output_grid_dir, recursive = TRUE, showWarnings = FALSE)
  v_years <- c(2050, 2100)

  v_crop_codes <- c(
    "PDRIR", "WHTIR", "GROIR", "OSDIR", "C_BIR", "OTH_AIR",
    "PDRRF", "WHTRF", "GRORF", "OSDRF", "C_BRF", "OTH_ARF"
  )
  df_land_code <- bind_rows(
    tibble(Land_type = "Cropland", Land_code = v_crop_codes),
    tibble(Land_type = "Energy crops", Land_code = "BIO"),
    # FRS is total forest; adding PLNFRS would double-count planted forest.
    tibble(Land_type = "Forest", Land_code = "FRS")
  )
  v_land_levels <- c("Cropland", "Energy crops", "Forest")

  # Prefer the same gdxrrw route as analysis.R. A gdxdump fallback keeps the
  # code runnable with newer R versions that cannot load the old gdxrrw binary.
  v_has_gdxrrw <- suppressWarnings(
    tryCatch(requireNamespace("gdxrrw", quietly = TRUE),
      error = function(e) FALSE
    )
  )

  f_gdxdump_csv <- function(v_gdx, v_symbol) {
    v_gdxdump <- file.path(v_gams_dir, "gdxdump.exe")
    if (!file.exists(v_gdxdump) || !requireNamespace("data.table", quietly = TRUE)) {
      stop("Neither a working gdxrrw nor the GAMS gdxdump/data.table fallback is available.")
    }
    v_csv <- tempfile(pattern = paste0(v_symbol, "_"), fileext = ".csv")
    on.exit(unlink(v_csv), add = TRUE)
    v_status <- system2(
      v_gdxdump,
      args = c(
        shQuote(v_gdx), paste0("Symb=", v_symbol), "Format=csv",
        paste0("Output=", shQuote(v_csv))
      )
    )
    if (v_status != 0 || !file.exists(v_csv)) {
      stop("gdxdump failed for symbol ", v_symbol, " in ", v_gdx)
    }
    data.table::fread(v_csv, data.table = FALSE)
  }

  if (v_has_gdxrrw) {
    Sys.setenv(PATH = paste(Sys.getenv("PATH"), v_gams_dir,
      sep = .Platform$path.sep
    ))
    try(gdxrrw::igdx(v_gams_dir), silent = TRUE)
    df_grid_map <- gdxrrw::rgdx.set(v_map_gdx, "MAP_GIJ")
  } else {
    df_grid_map <- f_gdxdump_csv(v_map_gdx, "MAP_GIJ")
  }

  df_grid_map <- as_tibble(df_grid_map[, seq_len(3)])
  names(df_grid_map) <- c("G", "I", "J")
  df_grid_map <- df_grid_map %>%
    transmute(
      G = as.character(G),
      I = as.numeric(as.character(I)),
      J = as.numeric(as.character(J))
    )

  if (requireNamespace("maps", quietly = TRUE)) {
    df_world <- ggplot2::map_data("world") %>%
      mutate(
        map_x = long * 2 + 360,
        map_y = lat * (-2) + 180
      )
  } else {
    df_world <- NULL
  }

  v_output_files <- character(nrow(df_aim_scenarios))
  l_grid_plot <- vector("list", nrow(df_aim_scenarios))
  for (i in seq_len(nrow(df_aim_scenarios))) {
    if (v_has_gdxrrw) {
      df_grid_raw <- gdxrrw::rgdx.param(df_aim_scenarios$GDX_path[i], "VYL")
    } else {
      df_grid_raw <- f_gdxdump_csv(df_aim_scenarios$GDX_path[i], "VYL")
    }

    # Normalize the dimension names returned by either GDX reader.
    df_grid_raw <- as_tibble(df_grid_raw[, seq_len(5)])
    names(df_grid_raw) <- c("Region", "Year", "Land_code", "G", "Value")
    df_grid_raw <- df_grid_raw %>%
      transmute(
        Year = as.numeric(as.character(Year)),
        Land_code = as.character(Land_code),
        G = as.character(G),
        Value = as.numeric(Value)
      ) %>%
      filter(Year %in% v_years)

    df_land_grids <- df_grid_raw %>%
      distinct(G) %>%
      inner_join(df_grid_map, by = "G")
    df_grid_plot <- df_grid_raw %>%
      inner_join(df_land_code, by = "Land_code") %>%
      group_by(Year, G, Land_type) %>%
      summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
      right_join(
        crossing(df_land_grids, Year = v_years, Land_type = v_land_levels),
        by = c("G", "Year", "Land_type")
      ) %>%
      mutate(
        Value = replace_na(Value, 0),
        Year = factor(Year, levels = v_years),
        Land_type = factor(Land_type, levels = v_land_levels),
        Scenario_ID = df_aim_scenarios$Scenario_ID[i]
      )
    l_grid_plot[[i]] <- df_grid_plot

    p_grid <- ggplot(df_grid_plot, aes(x = J, y = I, fill = Value)) +
      geom_tile() +
      facet_grid(Land_type ~ Year, drop = FALSE) +
      scale_fill_viridis_c(
        option = "C", direction = -1, limits = c(0, 1),
        oob = scales::squish, breaks = c(0, 0.25, 0.5, 0.75, 1),
        name = "Grid-cell share"
      ) +
      coord_fixed(xlim = c(0, 720), ylim = c(360, 0), expand = FALSE) +
      labs(title = df_aim_scenarios$Scenario_ID[i]) +
      theme_minimal(base_size = 13) +
      theme(
        panel.background = element_rect(fill = "grey92", colour = NA),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 15, face = "bold"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.width = grid::unit(42, "pt"),
        panel.spacing = grid::unit(5, "pt"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.margin = margin(4, 4, 4, 4)
      )

    if (!is.null(df_world)) {
      p_grid <- p_grid +
        geom_polygon(
          data = df_world,
          aes(x = map_x, y = map_y, group = group),
          inherit.aes = FALSE, fill = NA,
          colour = "grey25", linewidth = 0.15
        )
    }

    v_output_files[i] <- file.path(
      v_output_grid_dir,
      paste0(
        "AIM_", df_aim_scenarios$Scenario_ID[i],
        "_land_use_grid_2050_2100.png"
      )
    )
    ggsave(v_output_files[i],
      plot = p_grid,
      width = 12, height = 9, dpi = 300, bg = "white"
    )
  }

  # Marker scenarios are ordered to match the other ScenarioMIP figures.
  v_marker_ids <- c(
    "SSP3_H", "SSP2_M", "SSP2_ML", "SSP2_L", "SSP1_VL", "SSP2_LN"
  )
  df_grid_all <- bind_rows(l_grid_plot)
  v_missing_marker <- setdiff(v_marker_ids, unique(df_grid_all$Scenario_ID))
  if (length(v_missing_marker) > 0) {
    warning(
      "Missing marker scenario(s) in horizontal grid figure: ",
      paste(v_missing_marker, collapse = ", ")
    )
  }
  v_available_marker <- v_marker_ids[v_marker_ids %in% df_grid_all$Scenario_ID]
  if (length(v_available_marker) > 0) {
    df_grid_all <- df_grid_all %>%
      filter(Scenario_ID %in% v_available_marker) %>%
      mutate(Scenario_ID = factor(Scenario_ID, levels = v_available_marker))

    v_combined_files <- file.path(
      v_output_grid_dir,
      paste0("AIM_marker_scenarios_land_use_grid_", v_years, ".png")
    )
    for (j in seq_along(v_years)) {
      p_combined <- ggplot(
        filter(df_grid_all, as.numeric(as.character(Year)) == v_years[j]),
        aes(x = J, y = I, fill = Value)
      ) +
        geom_tile() +
        facet_grid(Land_type ~ Scenario_ID, drop = FALSE) +
        scale_fill_viridis_c(
          option = "C", direction = -1, limits = c(0, 1),
          oob = scales::squish, breaks = c(0, 0.25, 0.5, 0.75, 1),
          name = "Grid-cell share"
        ) +
        coord_fixed(xlim = c(0, 720), ylim = c(360, 0), expand = FALSE) +
        labs(title = as.character(v_years[j])) +
        theme_minimal(base_size = 11) +
        theme(
          panel.background = element_rect(fill = "grey92", colour = NA),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          strip.text = element_text(size = 12, face = "bold"),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.key.width = grid::unit(42, "pt"),
          panel.spacing = grid::unit(3, "pt"),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          plot.margin = margin(4, 4, 4, 4)
        )

      if (!is.null(df_world)) {
        p_combined <- p_combined +
          geom_polygon(
            data = df_world,
            aes(x = map_x, y = map_y, group = group),
            inherit.aes = FALSE, fill = NA,
            colour = "grey25", linewidth = 0.12
          )
      }
      ggsave(v_combined_files[j],
        plot = p_combined,
        width = 18, height = 7.5, dpi = 300, bg = "white"
      )
    }

    # Put the two year-specific figures into one panel with a shared legend.
    v_panel_row_levels <- unlist(lapply(
      v_years,
      function(v_year) paste(v_year, v_land_levels, sep = " | ")
    ))
    df_grid_panel <- df_grid_all %>%
      mutate(Panel_row = factor(
        paste(as.character(Year), as.character(Land_type), sep = " | "),
        levels = v_panel_row_levels
      ))
    p_panel <- ggplot(df_grid_panel, aes(x = J, y = I, fill = Value)) +
      geom_tile() +
      facet_grid(Panel_row ~ Scenario_ID, drop = FALSE) +
      scale_fill_viridis_c(
        option = "C", direction = -1, limits = c(0, 1),
        oob = scales::squish, breaks = c(0, 0.25, 0.5, 0.75, 1),
        name = "Grid-cell share"
      ) +
      coord_fixed(xlim = c(0, 720), ylim = c(360, 0), expand = FALSE) +
      theme_minimal(base_size = 11) +
      theme(
        panel.background = element_rect(fill = "grey92", colour = NA),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.width = grid::unit(42, "pt"),
        panel.spacing = grid::unit(3, "pt"),
        plot.margin = margin(4, 4, 4, 4)
      )
    if (!is.null(df_world)) {
      p_panel <- p_panel +
        geom_polygon(
          data = df_world,
          aes(x = map_x, y = map_y, group = group),
          inherit.aes = FALSE, fill = NA,
          colour = "grey25", linewidth = 0.12
        )
    }
    v_panel_file <- file.path(
      v_output_grid_dir,
      "AIM_marker_scenarios_land_use_grid_2050_2100_panel.png"
    )
    ggsave(v_panel_file,
      plot = p_panel,
      width = 18, height = 13, dpi = 300, bg = "white"
    )
    v_output_files <- c(v_output_files, v_combined_files, v_panel_file)
  }
  invisible(v_output_files)
}

# Generate outputs -------------------------------------------------------------

v_agricultural_production <- append(
  df_variable$agricultural_production[!is.na(df_variable$agricultural_production)],
  "Agricultural Production|Crops|Other Crops",
  after = 4
)

f_fig_line1("GHG_Emissions", df_variable$GHG[!is.na(df_variable$GHG)])
f_fig_line1("Air_Pollutant", df_variable$air_pollutant[!is.na(df_variable$air_pollutant)])
f_fig_line1("CDR_CCS", df_variable$CDR_CCS[!is.na(df_variable$CDR_CCS)])
f_fig_line1("Air_Pollutant_Ratio", df_variable$air_pollutant_energy_ratio[!is.na(df_variable$air_pollutant_energy_ratio)])
f_fig_line5("SDG", df_variable$sdg[!is.na(df_variable$sdg)])
f_fig_line2("Primary_Energy", "Primary Energy")
f_fig_line2("Final_Energy", "Final Energy")
f_fig_line2("Agricultural_Production", "Agricultural Production")
f_fig_line3("Food_Availability", "Food Availability [per capita]")
f_fig_line4("Economic_indicator", df_variable$economic_impact[!is.na(df_variable$economic_impact)])


f_fig_area("CO2", df_variable$CO2_sector[!is.na(df_variable$CO2_sector)], "Emissions|CO2")
f_fig_area("CDR", df_variable$CDR[!is.na(df_variable$CDR)], "Carbon Removal")
f_fig_area("Final_Energy_Sector", df_variable$final_energy_sector[!is.na(df_variable$final_energy_sector)], "Final Energy")
f_fig_area("Final_Energy_Source", df_variable$final_energy_source[!is.na(df_variable$final_energy_source)], "Final Energy")
f_fig_area("Primary_Energy", df_variable$primary_energy[!is.na(df_variable$primary_energy)], "Primary Energy")
f_fig_area("Land_Cover", df_variable$land_cover[!is.na(df_variable$land_cover)], NA)
f_fig_area("Agricultural_Production", v_agricultural_production, NA)


f_fig_bar("CO2", df_variable$CO2_sector[!is.na(df_variable$CO2_sector)], "Emissions|CO2")
f_fig_bar("CDR", df_variable$CDR[!is.na(df_variable$CDR)], "Carbon Removal")
f_fig_bar("Final_Energy_Sector", df_variable$final_energy_sector[!is.na(df_variable$final_energy_sector)], "Final Energy")
f_fig_bar("Final_Energy_Source", df_variable$final_energy_source[!is.na(df_variable$final_energy_source)], "Final Energy")
f_fig_bar("Primary_Energy", df_variable$primary_energy[!is.na(df_variable$primary_energy)], "Primary Energy")
f_fig_bar("Land_Cover", df_variable$land_cover[!is.na(df_variable$land_cover)], NA)
f_fig_bar("Agricultural_Production", v_agricultural_production, "Agricultural Production")
f_fig_food_source_area()
f_fig_land_cover_ln_comparison()
f_fig_aim_land_grid()
