# Common style and panel builders for the paper figures (ERL revision).
#
# Sourced by main.R, main_y.R, main_z.R and air_pollution.R so that every
# paper figure uses one scenario palette, one style for the LN multi-model
# range, one theme, and one way of tagging and saving composed figures.
# Nothing here changes which data are plotted; it only controls how.

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})

# Output -----------------------------------------------------------------------

# Composed paper figures go here (override with LN_FIG_DIR).
f_paper_dir <- function(v_default) {
  v_dir <- Sys.getenv("LN_FIG_DIR", unset = v_default)
  dir.create(v_dir, recursive = TRUE, showWarnings = FALSE)
  v_dir
}

# Width of one portrait A4 text column, in cm.
v_paper_width <- 17

# Vector PDF (fonts embedded by cairo) plus a 600 dpi PNG.
f_save_paper <- function(p, v_name, v_height, v_dir, v_width = v_paper_width) {
  ggsave(file.path(v_dir, paste0(v_name, ".pdf")), p,
    width = v_width, height = v_height, units = "cm",
    device = cairo_pdf, bg = "white"
  )
  ggsave(file.path(v_dir, paste0(v_name, ".png")), p,
    width = v_width, height = v_height, units = "cm",
    dpi = 600, device = ragg::agg_png, bg = "white"
  )
  message("Saved ", file.path(v_dir, v_name), ".pdf/.png")
  invisible(p)
}

# Panel tags (a), (b), ... for a patchwork.
f_tags <- function() {
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")
}

# Palettes ---------------------------------------------------------------------

# Named scenario colours from define.csv (scenario1 -> color_scenario).
# Colours are always looked up by name, never by position.
f_pal_scenario <- function(df_define) {
  v_ok <- !is.na(df_define$scenario1) & df_define$scenario1 != ""
  setNames(unname(df_define$color_scenario[v_ok]), df_define$scenario1[v_ok])
}

f_pal_model <- function(df_define) {
  v_ok <- !is.na(df_define$model2) & df_define$model2 != ""
  setNames(unname(df_define$color_model[v_ok]), df_define$model2[v_ok])
}

# The LN multi-model range is always a translucent band in the LN colour.
v_ln_range_alpha <- 0.18
v_ln_range_label <- "LN, range across models"
# Other models' LN runs are thin lines in the LN colour.
v_ln_other_linewidth <- 0.2
v_ln_other_alpha <- 0.5
# AIM scenario lines.
v_aim_linewidth <- 0.5
# Zero line.
f_zero_line <- function() {
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.35)
}

# Theme ------------------------------------------------------------------------

# Sizes are in pt at the final printed size (17 cm wide figures).
theme_paper <- function(v_base = 7) {
  theme_bw(base_size = v_base, base_family = "sans") +
    theme(
      panel.border = element_rect(fill = NA, colour = "grey35", linewidth = 0.3),
      panel.grid.major = element_line(colour = "grey90", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(2, "mm"),
      strip.background = element_blank(),
      strip.text = element_text(size = v_base, margin = margin(1, 1, 2, 1)),
      axis.text = element_text(size = v_base - 1, colour = "grey15"),
      axis.title = element_text(size = v_base),
      axis.ticks = element_line(colour = "grey35", linewidth = 0.25),
      axis.ticks.length = unit(0.8, "mm"),
      plot.title = element_text(size = v_base, hjust = 0.5, margin = margin(0, 0, 2, 0)),
      legend.title = element_text(size = v_base),
      legend.text = element_text(size = v_base - 0.5),
      legend.key = element_blank(),
      legend.key.size = unit(3, "mm"),
      legend.background = element_blank(),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.spacing = unit(2, "mm"),
      plot.tag = element_text(size = v_base + 1, face = "bold"),
      plot.margin = margin(2, 3, 2, 2)
    )
}

# x labels turned by 90 degrees (used where AR6 categories sit next to years)
theme_x90 <- function() {
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
}

# Chemical formulas ------------------------------------------------------------

# Displayed text writes CO2, CH4, N2O, SO2, NO2 and NH3 with real subscripts.
# Unicode subscript digits are not used: Arial has no glyphs for them and the
# cairo PDF shows empty boxes, so the subscripts are set with plotmath.
# Only displayed labels are converted; data variable names are left as they are.
v_chem_re <- "CO2|CH4|N2O|SO2|NO2|NH3"

# one line of text -> plotmath source, e.g. "Gt CO2/yr" -> "Gt CO"[2]*"/yr"
f_chem_line <- function(s) {
  q <- function(x) encodeString(x, quote = '"')
  m <- gregexpr(v_chem_re, s)[[1]]
  if (m[1] == -1) return(q(s))
  v_len <- attr(m, "match.length")
  v_parts <- character()
  v_pos <- 1
  for (k in seq_along(m)) {
    if (m[k] > v_pos) v_parts <- c(v_parts, q(substr(s, v_pos, m[k] - 1)))
    v_tok <- substr(s, m[k], m[k] + v_len[k] - 1)
    i <- regexpr("[0-9]", v_tok)
    v_piece <- paste0(q(substr(v_tok, 1, i - 1)), "[", substr(v_tok, i, i), "]")
    if (i < nchar(v_tok)) v_piece <- paste0(v_piece, "*", q(substr(v_tok, i + 1, nchar(v_tok))))
    v_parts <- c(v_parts, v_piece)
    v_pos <- m[k] + v_len[k]
  }
  if (v_pos <= nchar(s)) v_parts <- c(v_parts, q(substr(s, v_pos, nchar(s))))
  paste(v_parts, collapse = "*")
}

# text (possibly two lines) -> plotmath source; usable with label_parsed
f_chem_txt <- function(x) {
  vapply(as.character(x), function(s) {
    v_l <- vapply(strsplit(s, "\n", fixed = TRUE)[[1]], f_chem_line, "")
    if (length(v_l) == 1) v_l else paste0("atop(", v_l[1], ",", v_l[2], ")")
  }, "", USE.NAMES = FALSE)
}

# label for titles, axes, legend titles and legend keys: an expression when
# the text holds a formula, otherwise the text unchanged
f_chem_lab <- function(x) {
  if (is.null(x) || all(is.na(x)) || !any(grepl(v_chem_re, x))) return(x)
  parse(text = f_chem_txt(x))
}

# facet labeller with subscripts; one_line puts a two-line label on one line
f_chem_labeller <- function(one_line = FALSE) {
  as_labeller(function(x) f_chem_txt(if (one_line) gsub("\n", " ", x, fixed = TRUE) else x),
              default = label_parsed)
}

# Panel builders ---------------------------------------------------------------

# Year breaks used on the discrete year axes.
v_year_breaks <- as.character(seq(2020, 2100, 20))

# One variable over time: AIM scenario lines, other models' LN runs as thin
# lines, the LN multi-model range as a band, and optionally the AR6 2100
# ranges (p5-p95 and p25-p75 by category) to the right of the time axis.
# The data selection is the one of f_fig_line1/2/4/5 in main.R.
f_panel_line <- function(df_snap, v_var, v_title, pal, v_scen, v_marker, v_year1,
                         df_AR6 = NULL, df_dummy = NULL, zero_line = TRUE,
                         v_ylab = NULL, v_ar6_cat = NULL, v_drop_model = NULL) {
  df1 <- df_snap %>%
    filter(Variable == v_var, Model == "AIM", Region == "World", Scenario_SSP %in% v_marker) %>%
    mutate(Scenario = factor(Scenario, levels = v_scen))
  df2 <- df_snap %>%
    filter(Variable == v_var, Year %in% v_year1, Scenario == "LN", Region == "World") %>%
    filter(!(Model %in% v_drop_model))
  df3 <- df2 %>%
    summarise(max = max(Value), min = min(Value), .by = c(Scenario, Year, Region, Variable, Unit)) %>%
    mutate(Range = v_ln_range_label)
  if (is.null(v_ylab)) v_ylab <- unique(c(df1$Unit, df2$Unit))[1]
  v_breaks <- v_year_breaks
  v_limits <- NULL
  p <- ggplot()
  if (!is.null(df_dummy)) {
    p <- p + geom_point(data = df_dummy, aes(x = Year, y = Value), alpha = 0)
  }
  p <- p +
    geom_ribbon(
      data = df3, aes(x = Year, ymin = min, ymax = max, group = Scenario, fill = Range),
      alpha = v_ln_range_alpha
    )
  if (!is.null(df_AR6)) {
    df4 <- df_AR6 %>% filter(Variable == v_var, Region == "World", Year == "2100")
    if (nrow(df4) > 0) {
      p <- p +
        geom_linerange(
          data = df4, aes(x = Category, ymin = lo5, ymax = up5, group = Category),
          colour = "grey", alpha = 0.4, linewidth = 1.6, show.legend = FALSE
        ) +
        geom_linerange(
          data = df4, aes(x = Category, ymin = lo25, ymax = up25, group = Category),
          colour = "grey", alpha = 0.5, linewidth = 1.6, show.legend = FALSE
        )
      v_breaks <- c(v_breaks, v_ar6_cat)
      # an empty slot separates the time axis from the AR6 categories
      v_years <- if (!is.null(df_dummy)) levels(df_dummy$Year) else sort(unique(as.character(df1$Year)))
      v_limits <- c(v_years, " ", v_ar6_cat[v_ar6_cat %in% df4$Category])
    }
  }
  if (zero_line) p <- p + f_zero_line()
  p +
    geom_line(
      data = df2, aes(x = Year, y = Value, group = interaction(Scenario_SSP, Model), colour = Scenario),
      linewidth = v_ln_other_linewidth, alpha = v_ln_other_alpha, show.legend = FALSE
    ) +
    geom_line(
      data = df1, aes(x = Year, y = Value, group = Scenario_SSP, colour = Scenario),
      linewidth = v_aim_linewidth,
      # draw a key for every scenario so that the legends of all panels match
      # and are merged, but only in the colour legend
      show.legend = c(colour = TRUE, fill = FALSE)
    ) +
    scale_colour_manual(values = pal, limits = v_scen, name = "Scenario (AIM)") +
    scale_fill_manual(values = setNames(unname(pal["LN"]), v_ln_range_label), name = NULL) +
    scale_x_discrete(breaks = v_breaks, limits = v_limits) +
    labs(title = f_chem_lab(v_title), x = NULL, y = f_chem_lab(v_ylab)) +
    guides(
      colour = guide_legend(order = 1, override.aes = list(linewidth = 0.8)),
      fill = guide_legend(order = 2)
    ) +
    theme_paper() +
    (if (length(v_breaks) > length(v_year_breaks)) theme_x90() else theme())
}

# Stacked areas by category for each AIM marker scenario, with a total line.
# The data selection is the one of f_fig_area in main.R.
f_panel_area <- function(df_snap, v_area, v_line, v_line_label, v_marker, pal_fill,
                         v_fill_labels, v_fill_title, v_ylab) {
  df1 <- df_snap %>%
    filter(Variable %in% v_area, Scenario_SSP %in% v_marker, Region == "World", Model == "AIM") %>%
    mutate(
      Variable = factor(Variable, levels = v_area),
      Scenario_SSP = factor(Scenario_SSP, levels = v_marker)
    )
  df2 <- df_snap %>%
    filter(Variable %in% v_line, Scenario_SSP %in% v_marker, Region == "World", Model == "AIM") %>%
    mutate(
      Variable = v_line_label,
      Scenario_SSP = factor(Scenario_SSP, levels = v_marker)
    )
  ggplot() +
    geom_area(data = df1, aes(x = Year, y = Value, group = Variable, fill = Variable), alpha = 0.85) +
    f_zero_line() +
    geom_line(data = df2, aes(x = Year, y = Value, group = Variable, linetype = Variable), linewidth = 0.5) +
    scale_fill_manual(
      values = pal_fill, name = f_chem_lab(v_fill_title),
      labels = function(b) f_chem_lab(setNames(v_fill_labels, v_area)[b])
    ) +
    scale_linetype_manual(values = "solid", name = NULL, labels = function(b) f_chem_lab(b)) +
    facet_wrap(~Scenario_SSP, nrow = 1, scales = "fixed") +
    scale_x_discrete(breaks = v_year_breaks) +
    labs(x = NULL, y = f_chem_lab(v_ylab)) +
    guides(fill = guide_legend(order = 1, ncol = 2), linetype = guide_legend(order = 2)) +
    theme_paper() +
    theme_x90() +
    theme(
      legend.position = "bottom", legend.justification = "left",
      legend.box = "vertical", legend.box.just = "left"
    )
}

# Stacked bars by category for 2050 and 2100: AIM marker scenarios and other
# models' LN runs, with the reported total as a cross.
# The data selection is the one of f_fig_bar in main_y.R.
f_panel_bar <- function(df_snap, v_area, v_point, v_marker, v_scen_model, pal_fill,
                        v_fill_labels, v_fill_title, v_ylab, v_years = c("2050", "2100"),
                        v_drop_model = NULL) {
  f_x <- function(df) mutate(df, x = factor(paste0(Model, "_", Scenario_SSP), levels = v_scen_model))
  df1 <- df_snap %>%
    filter(Year %in% v_years, Variable %in% v_area, Scenario_SSP %in% v_marker, Region == "World", Model == "AIM") %>%
    bind_rows(df_snap %>%
      filter(Year %in% v_years, Variable %in% v_area, Region == "World", Scenario == "LN", Model != "AIM")) %>%
    filter(!(Model %in% v_drop_model)) %>%
    mutate(Variable = factor(Variable, levels = v_area)) %>%
    f_x()
  df3 <- df_snap %>%
    filter(Year %in% v_years, Variable %in% v_point, Region == "World", Scenario == "LN", Model != "AIM") %>%
    bind_rows(df_snap %>%
      filter(Year %in% v_years, Variable %in% v_point, Scenario_SSP %in% v_marker, Region == "World", Model == "AIM")) %>%
    filter(!(Model %in% v_drop_model)) %>%
    mutate(Total = "Total (reported)") %>%
    f_x()
  ggplot() +
    geom_col(data = df1, aes(x = x, y = Value, fill = Variable), width = 0.8, alpha = 0.9) +
    f_zero_line() +
    geom_point(data = df3, aes(x = x, y = Value, shape = Total), size = 1.2, stroke = 0.5) +
    scale_fill_manual(
      values = pal_fill, name = f_chem_lab(v_fill_title),
      labels = function(b) f_chem_lab(setNames(v_fill_labels, v_area)[b])
    ) +
    scale_shape_manual(values = 4, name = NULL) +
    facet_wrap(~Year, nrow = 1, scales = "fixed") +
    labs(x = NULL, y = f_chem_lab(v_ylab)) +
    guides(fill = guide_legend(order = 1, ncol = 1), shape = guide_legend(order = 2)) +
    theme_paper() +
    theme_x90() +
    theme(panel.grid.major.x = element_blank())
}

# Scenarios that actually appear among the AIM marker runs, in define.csv order.
f_scen_present <- function(df_snap, v_vars, v_marker, v_scen_all) {
  v_have <- df_snap %>%
    filter(Variable %in% v_vars, Model == "AIM", Region == "World", Scenario_SSP %in% v_marker) %>%
    pull(Scenario) %>%
    unique()
  v_scen_all[v_scen_all %in% c(v_have, "LN")]
}
