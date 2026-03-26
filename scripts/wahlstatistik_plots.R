# ============================================================
# Repräsentative Wahlstatistik – Zweitstimmen Visualisierungen
# BTW 2017 / 2021 / 2025 nach Altersgruppe und Geschlecht
# ============================================================

library(tidyverse)

# ============================================================
# 1. Daten laden und bereinigen
# ============================================================

raw <- read.csv2(
  "data/btw_rws_zwst-1953.csv",
  skip        = 14,
  header      = TRUE,
  fileEncoding = "UTF-8",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

colnames(raw) <- c("Jahr", "Geschlecht", "Altersgruppe",
                   "SPD", "CDU", "GRÜNE", "FDP", "AfD", "CSU",
                   "Linke", "Sonstige")

# Dezimalkomma → Punkt, dann numeric
party_cols <- c("SPD", "CDU", "GRÜNE", "FDP", "AfD", "CSU", "Linke", "Sonstige")
raw[, party_cols] <- lapply(raw[, party_cols], function(x) as.numeric(gsub(",", ".", x)))
raw$Jahr <- as.integer(raw$Jahr)

# CDU/CSU kombiniert
raw$CDCSU <- raw$CDU + raw$CSU

# Geschlecht vereinheitlichen
raw$Geschlecht_label <- dplyr::case_when(
  raw$Geschlecht == "Summe"               ~ "Gesamt",
  raw$Geschlecht %in% c("m", "m|d|o")    ~ "Männlich",
  raw$Geschlecht == "w"                   ~ "Weiblich",
  TRUE                                    ~ raw$Geschlecht
)

# ============================================================
# 2. Filtern: Jahre 2017 / 2021 / 2025
#    Altersgruppen wie in den Referenz-Abbildungen (7 Punkte)
#    Reihenfolge: 18-24, 25-34, 35-44, 45-59, 60-69, >=60, >=70
#    (>=60 als Aggregat zwischen den Sub-Gruppen 60-69 / >=70)
# ============================================================

age_order  <- c("18-24", "25-34", "35-44", "45-59", "60-69", ">=60", ">=70")
age_labels <- c("18-24", "25-34", "35-44", "45-59", "60-69", "60+",  "70+")

df <- raw %>%
  filter(
    Jahr             %in% c(2017, 2021, 2025),
    Altersgruppe     %in% age_order,
    Geschlecht_label %in% c("Gesamt", "Männlich", "Weiblich")
  ) %>%
  mutate(
    Altersgruppe     = factor(Altersgruppe, levels = age_order, labels = age_labels),
    Geschlecht_label = factor(Geschlecht_label, levels = c("Gesamt", "Männlich", "Weiblich"))
  )

# ============================================================
# 3. Hilfsfunktionen
# ============================================================

# Farben für Jahresvergleichs-Plots
COL_OLD  <- "#7BAFD4"   # älteres Jahr – helles Blau
COL_NEW  <- "#1A2F55"   # neueres Jahr – dunkles Navy

# ----------------------------------------------------------------
# 3a. Jahresvergleichs-Plot  (wie Abbildungen 2 + 3)
#     Facets: Gesamt / Männlich / Weiblich
#     Zwei Linien pro Facet (year1 vs year2)
#     Annotierter Zuwachs über der year2-Linie
# ----------------------------------------------------------------
plot_year_comparison <- function(data, party_col, party_name, year1, year2) {

  df_plot <- data %>%
    filter(Jahr %in% c(year1, year2)) %>%
    select(Jahr, Geschlecht_label, Altersgruppe, value = all_of(party_col)) %>%
    filter(!is.na(value)) %>%
    mutate(Jahr = factor(Jahr, levels = c(year1, year2)))

  if (nrow(df_plot) == 0) return(NULL)

  # Zuwachs-Annotation
  y_max <- max(df_plot$value, na.rm = TRUE)

  df_diff <- df_plot %>%
    pivot_wider(names_from = Jahr, values_from = value,
                names_prefix = "y") %>%
    rename(v1 = paste0("y", year1), v2 = paste0("y", year2)) %>%
    mutate(
      diff  = v2 - v1,
      label = ifelse(diff >= 0,
                     paste0("+", round(diff, 1)),
                     as.character(round(diff, 1))),
      y_pos = v2 + y_max * 0.08
    )

  ggplot(df_plot, aes(x = Altersgruppe, y = value,
                       color = Jahr, group = Jahr)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.2) +
    geom_text(data = df_diff,
              aes(x = Altersgruppe, y = y_pos, label = label),
              inherit.aes = FALSE, size = 2.8, color = "grey30") +
    facet_wrap(~ Geschlecht_label, ncol = 1, strip.position = "top") +
    scale_color_manual(
      values = setNames(c(COL_OLD, COL_NEW), c(year1, year2)),
      name   = "Wahljahr"
    ) +
    scale_y_continuous(
      labels = ~ paste0(.x, " %"),
      limits = c(0, NA),
      expand = expansion(mult = c(0.02, 0.15))
    ) +
    labs(
      title    = paste0(party_name,
                        " \u2013 Zweitstimmenanteil nach Altersgruppen (",
                        year1, " - ", year2, ")"),
      subtitle = paste0("(Zuwachs ", year1, "\u2013", year2,
                        " in Prozentpunkten; Daten: Repr\u00e4sentative Wahlstatistik)"),
      x = "Altersgruppe",
      y = paste0(party_name, " Zweitstimmenanteil")
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title        = element_text(face = "bold", size = 13),
      plot.subtitle     = element_text(size = 9, color = "grey40"),
      strip.background  = element_rect(fill = "grey90", color = NA),
      strip.text        = element_text(face = "bold", size = 11),
      panel.grid.minor  = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position   = "right",
      axis.text.x       = element_text(size = 9)
    )
}

# ----------------------------------------------------------------
# 3b. Geschlechter-Aufschlüsselung für ein Jahr  (wie Abbildung 5)
#     Alle Geschlechter auf einem Panel
# ----------------------------------------------------------------
plot_gender_breakdown <- function(data, party_col, party_name, year = 2025) {

  df_plot <- data %>%
    filter(Jahr == year) %>%
    select(Geschlecht_label, Altersgruppe, value = all_of(party_col)) %>%
    filter(!is.na(value))

  if (nrow(df_plot) == 0) return(NULL)

  ggplot(df_plot, aes(x = Altersgruppe, y = value,
                       color = Geschlecht_label, group = Geschlecht_label)) +
    geom_line(linewidth = 1.0) +
    geom_point(size = 2.8) +
    scale_color_manual(
      values = c("Gesamt"   = "#7B3F6E",
                 "Männlich" = "#3C1A38",
                 "Weiblich" = "#C0A0BD"),
      name   = "Geschlecht"
    ) +
    scale_y_continuous(
      labels = ~ paste0(.x, " %"),
      limits = c(0, NA),
      expand = expansion(mult = c(0.02, 0.08))
    ) +
    labs(
      title    = paste0(party_name,
                        " \u2013 Zweitstimmenanteil nach Altersgruppen,",
                        " Bundestagswahl ", year),
      subtitle = "(Daten: Repr\u00e4sentative Wahlstatistik)",
      x = "Altersgruppe",
      y = paste0("Zweitstimmenanteil ", party_name,
                 " \u2013 Bundestagswahl ", year)
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 9, color = "grey40"),
      panel.border  = element_rect(color = "grey70", fill = NA),
      panel.grid.minor  = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position   = "right"
    )
}

# ============================================================
# 4. Plots erstellen und speichern
# ============================================================

dir.create("output", showWarnings = FALSE)

party_list <- list(
  list(col = "AfD",   name = "AfD"),
  list(col = "SPD",   name = "SPD"),
  list(col = "CDU",   name = "CDU"),
  list(col = "CDCSU", name = "CDU/CSU"),
  list(col = "CSU",   name = "CSU"),
  list(col = "GRÜNE", name = "GRÜNE"),
  list(col = "FDP",   name = "FDP"),
  list(col = "Linke", name = "Die Linke")
)

for (p in party_list) {
  safe_name <- gsub("/", "", p$name)
  cat("Erstelle Plots für:", p$name, "\n")

  # 2021 vs 2025
  plt <- plot_year_comparison(df, p$col, p$name, 2021, 2025)
  if (!is.null(plt)) {
    ggsave(file.path("output", paste0(safe_name, "_2021_2025.png")),
           plot = plt, width = 7, height = 9, dpi = 150, bg = "white")
  }

  # 2017 vs 2025
  plt17 <- plot_year_comparison(df, p$col, p$name, 2017, 2025)
  if (!is.null(plt17)) {
    ggsave(file.path("output", paste0(safe_name, "_2017_2025.png")),
           plot = plt17, width = 7, height = 9, dpi = 150, bg = "white")
  }

  # 2025: alle Geschlechter
  plt_g <- plot_gender_breakdown(df, p$col, p$name)
  if (!is.null(plt_g)) {
    ggsave(file.path("output", paste0(safe_name, "_2025_Geschlecht.png")),
           plot = plt_g, width = 8, height = 5, dpi = 150, bg = "white")
  }
}

cat("\nFertig! Alle Plots gespeichert in output/\n")
