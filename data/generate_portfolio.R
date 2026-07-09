# ─────────────────────────────────────────────────────────────────────────────
# Génération d'un portefeuille simulé de contrats décès temporaires
# ─────────────────────────────────────────────────────────────────────────────

generate_portfolio <- function(n = 5000L, seed = 42L,
                                output_dir = "data") {
  set.seed(seed)

  ages   <- sample(20:60, n, replace = TRUE)
  durees <- sample(c(5, 10, 15, 20, 25, 30), n, replace = TRUE,
                   prob = c(0.10, 0.30, 0.25, 0.20, 0.10, 0.05))
  durees <- pmin(durees, 70L - ages)
  durees <- pmax(durees, 1L)

  genres <- sample(c("H", "F"), n, replace = TRUE, prob = c(0.58, 0.42))

  capitaux <- rlnorm(n, meanlog = log(150000), sdlog = 0.7)
  capitaux <- round(capitaux / 1000) * 1000
  capitaux <- pmax(pmin(capitaux, 1500000), 10000)

  fumeurs <- ifelse(
    genres == "H",
    runif(n) < 0.25,
    runif(n) < 0.18
  )

  annees_sous <- sample(2018:2025, n, replace = TRUE,
                        prob = c(0.05, 0.08, 0.10, 0.15, 0.20, 0.18, 0.14, 0.10))

  df <- data.frame(
    id_contrat       = sprintf("DC%05d", seq_len(n)),
    age_souscription = ages,
    genre            = genres,
    duree            = durees,
    capital_assure   = as.integer(capitaux),
    fumeur           = fumeurs,
    annee_souscription = annees_sous,
    age_actuel       = ages + (2026L - annees_sous),
    anciennete       = 2026L - annees_sous,
    stringsAsFactors = FALSE
  )

  # Conserver uniquement les contrats actifs
  df <- df[df$anciennete <= df$duree, ]
  df$duree_restante <- df$duree - df$anciennete
  df <- df[df$duree_restante > 0, ]
  rownames(df) <- NULL

  path <- file.path(output_dir, "portefeuille.csv")
  write.csv(df, path, row.names = FALSE)

  cat(sprintf("Portefeuille généré : %d contrats actifs -> %s\n", nrow(df), path))
  cat(sprintf("  Age moyen         : %.1f ans\n",  mean(df$age_souscription)))
  cat(sprintf("  Capital moyen     : %.0f EUR\n",  mean(df$capital_assure)))
  cat(sprintf("  Durée moyenne     : %.1f ans\n",  mean(df$duree)))
  cat(sprintf("  Part fumeurs      : %.1f %%\n",   mean(df$fumeur) * 100))

  invisible(df)
}
