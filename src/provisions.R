# ─────────────────────────────────────────────────────────────────────────────
# Provisions mathématiques du portefeuille
# ─────────────────────────────────────────────────────────────────────────────

provisions_portefeuille <- function(portefeuille, hyp = HYP_BASE) {
  resultats <- lapply(seq_len(nrow(portefeuille)), function(i) {
    row      <- portefeuille[i, ]
    age_sous <- as.integer(row$age_souscription)
    n        <- as.integer(row$duree)
    t        <- as.integer(row$anciennete)
    C        <- as.numeric(row$capital_assure)
    genre    <- row$genre
    fumeur   <- as.logical(row$fumeur)

    res            <- prime_commerciale(age_sous, n, C, genre, fumeur, hyp)
    P_nette_unit   <- res$prime_nette_annuelle / C
    prov_unit      <- provision_prospective(age_sous, n, t, P_nette_unit,
                                            genre, hyp$taux_technique)

    list(
      id_contrat          = row$id_contrat,
      age_souscription    = age_sous,
      age_actuel          = age_sous + t,
      duree               = n,
      anciennete          = t,
      duree_restante      = n - t,
      capital             = C,
      genre               = genre,
      fumeur              = fumeur,
      prime_nette         = res$prime_nette_annuelle,
      prime_commerciale   = res$prime_commerciale,
      provision           = round(prov_unit * C, 2)
    )
  })

  cols <- names(resultats[[1]])
  df   <- as.data.frame(
    lapply(cols, function(nm) vapply(resultats, function(r) as.character(r[[nm]]),
                                     character(1L))),
    stringsAsFactors = FALSE
  )
  names(df) <- cols

  num_cols <- c("age_souscription", "age_actuel", "duree", "anciennete",
                "duree_restante", "capital", "prime_nette",
                "prime_commerciale", "provision")
  for (col in num_cols) df[[col]] <- as.numeric(df[[col]])
  df
}

bilan_provisions <- function(df_prov) {
  list(
    nb_contrats              = nrow(df_prov),
    capital_total            = sum(df_prov$capital),
    primes_annuelles_totales = sum(df_prov$prime_commerciale),
    provisions_totales       = sum(df_prov$provision),
    provision_moyenne        = mean(df_prov$provision),
    provision_max            = max(df_prov$provision),
    provision_par_euro_cap   = sum(df_prov$provision) / sum(df_prov$capital)
  )
}
