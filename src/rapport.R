# ─────────────────────────────────────────────────────────────────────────────
# Rapport : graphiques ggplot2 et exports Excel
# ─────────────────────────────────────────────────────────────────────────────

library(ggplot2)
library(tidyr)
library(dplyr)

OUTPUT_DIR <- "output"

# Palette
BLEU   <- "#1a5276"
ROUGE  <- "#c0392b"
VERT   <- "#1e8449"
ORANGE <- "#d35400"
GRIS   <- "#7f8c8d"
FOND   <- "#f8f9fa"

theme_actuariel <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.background  = element_rect(fill = FOND, color = NA),
      panel.background = element_rect(fill = FOND, color = NA),
      plot.title       = element_text(face = "bold", size = 12),
      plot.subtitle    = element_text(color = GRIS, size = 10),
      legend.background = element_rect(fill = FOND, color = NA)
    )
}

sauver <- function(p, nom, w = 12, h = 5) {
  path <- file.path(OUTPUT_DIR, nom)
  ggsave(path, plot = p, width = w, height = h, dpi = 150, bg = FOND)
  cat(sprintf("  -> %s\n", path))
}

# ── 1. Tables de mortalité ────────────────────────────────────────────────────

graph_mortalite <- function(th, tf) {
  ages <- 20:90
  df <- rbind(
    data.frame(age = ages, qx = th$qx[ages + 1], genre = "Hommes TH0002"),
    data.frame(age = ages, qx = tf$qx[ages + 1], genre = "Femmes TF0002")
  )
  df$qx_mille <- df$qx * 1000

  p1 <- ggplot(df, aes(age, qx_mille, color = genre)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = c("Hommes TH0002" = BLEU, "Femmes TF0002" = ROUGE)) +
    labs(title = "Taux de mortalite q(x) pour mille",
         x = "Age", y = "q(x) ‰", color = NULL) +
    theme_actuariel()

  p2 <- ggplot(df, aes(age, qx, color = genre)) +
    geom_line(linewidth = 1) +
    scale_y_log10() +
    scale_color_manual(values = c("Hommes TH0002" = BLEU, "Femmes TF0002" = ROUGE)) +
    labs(title = "q(x) - echelle logarithmique",
         x = "Age", y = "log q(x)", color = NULL) +
    theme_actuariel()

  library(patchwork)
  suppressWarnings(
    p <- (p1 | p2) + plot_annotation(
      title = "Tables de mortalite francaises TH/TF 00-02",
      theme = theme(plot.title = element_text(face = "bold", size = 14))
    )
  )
  sauver(p, "01_tables_mortalite.png")
}

# ── 2. Barèmes ────────────────────────────────────────────────────────────────

graph_bareme <- function(barem_H, barem_F) {
  ages   <- as.integer(rownames(barem_H))
  durees <- gsub("D", "", colnames(barem_H))

  barem_long <- function(df, lbl) {
    df$age <- as.integer(rownames(df))
    tidyr::pivot_longer(df, -age, names_to = "duree", values_to = "taux") |>
      mutate(genre = lbl, duree = gsub("D","", duree))
  }

  df <- rbind(barem_long(barem_H, "Hommes"), barem_long(barem_F, "Femmes")) |>
    filter(!is.na(taux))

  p <- ggplot(df, aes(age, taux, color = duree, group = duree)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    facet_wrap(~genre) +
    labs(title = "Baremes de tarification - Assurance deces temporaire",
         subtitle = "Prime pour mille du capital assure (non-fumeurs)",
         x = "Age de souscription", y = "Prime / Capital (‰)", color = "Duree") +
    theme_actuariel()

  sauver(p, "02_baremes_tarification.png")
}

# ── 3. Décomposition prime ────────────────────────────────────────────────────

graph_decomposition <- function(res) {
  df <- data.frame(
    composante = factor(
      c("Prime nette", "Frais acq.", "Frais gestion",
        "Frais capital", "Frais fixes", "Marge"),
      levels = c("Prime nette", "Frais acq.", "Frais gestion",
                 "Frais capital", "Frais fixes", "Marge")
    ),
    valeur = c(res$prime_nette_annuelle, res$dont_frais_acquisition,
               res$dont_frais_gestion,  res$dont_frais_capital,
               res$dont_frais_fixes,    res$marge_technique),
    couleur = c(BLEU, ORANGE, ORANGE, ORANGE, GRIS, VERT)
  )

  p <- ggplot(df, aes(composante, valeur, fill = composante)) +
    geom_col(width = 0.65, show.legend = FALSE) +
    geom_text(aes(label = sprintf("%.1f EUR", valeur)),
              vjust = -0.4, size = 3.5) +
    scale_fill_manual(values = setNames(df$couleur, df$composante)) +
    labs(
      title    = "Decomposition de la prime commerciale",
      subtitle = sprintf("H, %d ans, %d ans, %s EUR",
                         res$age, res$duree,
                         format(res$capital, big.mark = " ")),
      x = NULL, y = "Prime annuelle (EUR)"
    ) +
    theme_actuariel()

  sauver(p, "03_decomposition_prime.png", w = 9)
}

# ── 4. Profil provisions ──────────────────────────────────────────────────────

graph_provisions <- function(df_prov, age, n) {
  p <- ggplot(df_prov, aes(t, provision)) +
    geom_area(fill = BLEU, alpha = 0.15) +
    geom_line(color = BLEU, linewidth = 1.8) +
    geom_point(color = BLEU, size = 2.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = GRIS) +
    labs(
      title    = "Profil des provisions mathematiques prospectives",
      subtitle = sprintf("H, %d ans, duree %d ans, capital 100 000 EUR", age, n),
      x = "Anciennete (annees)", y = "Provision (EUR)"
    ) +
    theme_actuariel()

  sauver(p, "04_profil_provisions.png", w = 10)
}

# ── 5. Portefeuille ───────────────────────────────────────────────────────────

graph_portefeuille <- function(df) {
  library(patchwork)

  p1 <- ggplot(df, aes(age_souscription)) +
    geom_histogram(bins = 20, fill = BLEU, color = "white", alpha = 0.85) +
    labs(title = "Ages de souscription", x = "Age", y = "Effectif") +
    theme_actuariel()

  p2 <- ggplot(df, aes(capital_assure / 1000)) +
    geom_histogram(bins = 30, fill = VERT, color = "white", alpha = 0.85) +
    labs(title = "Capitaux assures (kEUR)", x = "Capital (kEUR)", y = "Effectif") +
    theme_actuariel()

  p3 <- df |>
    count(duree) |>
    ggplot(aes(factor(duree), n)) +
    geom_col(fill = ORANGE, color = "white", alpha = 0.85) +
    labs(title = "Durees", x = "Duree (ans)", y = "Effectif") +
    theme_actuariel()

  p4 <- df |>
    mutate(statut = ifelse(fumeur, "Fumeur", "Non-fumeur")) |>
    count(genre, statut) |>
    ggplot(aes(genre, n, fill = statut)) +
    geom_col(position = "dodge", color = "white", alpha = 0.85) +
    scale_fill_manual(values = c("Non-fumeur" = BLEU, "Fumeur" = ROUGE)) +
    labs(title = "Genre x Statut fumeur", x = NULL, y = "Effectif", fill = NULL) +
    theme_actuariel()

  suppressWarnings(
    p <- (p1 | p2) / (p3 | p4) + plot_annotation(
      title = "Analyse du portefeuille de contrats",
      theme = theme(plot.title = element_text(face = "bold", size = 14))
    )
  )
  sauver(p, "05_analyse_portefeuille.png", w = 13, h = 9)
}

# ── 6. Sensibilité ────────────────────────────────────────────────────────────

graph_sensibilite <- function(df_taux, df_mort) {
  library(patchwork)

  p1 <- ggplot(df_taux, aes(taux * 100, prime_commerciale,
                             color = factor(duree), group = factor(duree))) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    scale_color_brewer(palette = "Set1") +
    labs(title = "Sensibilite au taux technique",
         subtitle = "H, 40 ans, 100 000 EUR",
         x = "Taux technique (%)", y = "Prime commerciale (EUR)", color = "Duree") +
    theme_actuariel()

  p2 <- ggplot(df_mort, aes(marge_mort * 100, prime_commerciale,
                              color = factor(duree), group = factor(duree))) +
    geom_line(linewidth = 1) +
    geom_point(shape = 15, size = 2.5) +
    scale_color_brewer(palette = "Set1") +
    labs(title = "Sensibilite a la marge mortalite",
         subtitle = "H, 40 ans, 100 000 EUR, taux=2%",
         x = "Majoration mortalite (%)", y = "Prime commerciale (EUR)", color = "Duree") +
    theme_actuariel()

  p <- (p1 | p2)
  sauver(p, "06_sensibilite.png")
}

# ── Export Excel ──────────────────────────────────────────────────────────────

export_excel <- function(barem_H, barem_F, barem_H_fum,
                          df_tarif, df_prov) {
  library(openxlsx)

  barem_H$age   <- rownames(barem_H)
  barem_F$age   <- rownames(barem_F)
  barem_H_fum$age <- rownames(barem_H_fum)

  wb <- createWorkbook()

  ajouter_sheet <- function(df, nom) {
    addWorksheet(wb, nom)
    writeDataTable(wb, nom, df, tableStyle = "TableStyleMedium9")
    setColWidths(wb, nom, cols = seq_len(ncol(df)), widths = "auto")
  }

  ajouter_sheet(barem_H,      "Bareme_H_NonFumeur")
  ajouter_sheet(barem_F,      "Bareme_F_NonFumeur")
  ajouter_sheet(barem_H_fum,  "Bareme_H_Fumeur")
  ajouter_sheet(head(df_tarif, 500), "Tarification")
  ajouter_sheet(head(df_prov,  500), "Provisions")

  path <- file.path(OUTPUT_DIR, "rapport_tarification.xlsx")
  saveWorkbook(wb, path, overwrite = TRUE)
  cat(sprintf("  -> %s\n", path))
}
