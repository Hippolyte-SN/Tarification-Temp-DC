# ─────────────────────────────────────────────────────────────────────────────
# Tarification complète — assurance décès temporaire
#
# Structure des chargements :
#   alpha = frais acquisition (% prime commerciale)
#   beta  = frais gestion continu (% prime commerciale)
#   gamma = frais sur capital (% annuel du capital)
#   delta = frais fixes annuels (EUR par contrat)
#
# Prime commerciale :
#   P_c = (P_nette + gamma*C + delta) / (1 - alpha - beta)
# ─────────────────────────────────────────────────────────────────────────────

# ── Hypothèses tarifaires ─────────────────────────────────────────────────────

nouvelles_hypotheses <- function(
  taux_technique   = 0.02,
  taux_actif       = 0.04,
  alpha            = 0.08,
  beta             = 0.06,
  gamma            = 0.0004,
  delta            = 15.0,
  surcoef_fum_H    = 1.50,
  surcoef_fum_F    = 1.40,
  marge_mortalite  = 0.10,
  label            = "Base"
) {
  list(
    taux_technique  = taux_technique,
    taux_actif      = taux_actif,
    alpha           = alpha,
    beta            = beta,
    gamma           = gamma,
    delta           = delta,
    surcoef_fum_H   = surcoef_fum_H,
    surcoef_fum_F   = surcoef_fum_F,
    marge_mortalite = marge_mortalite,
    label           = label
  )
}

HYP_BASE    <- nouvelles_hypotheses()
HYP_SS2     <- nouvelles_hypotheses(taux_technique = 0.00, marge_mortalite = 0.15, label = "Solvabilite II")
HYP_STRESS  <- nouvelles_hypotheses(taux_technique = 0.01, taux_actif = 0.02,
                                     marge_mortalite = 0.25, label = "Stress")

# ── Tarification unitaire ─────────────────────────────────────────────────────

prime_commerciale <- function(age, n, C, genre = "H", fumeur = FALSE,
                               hyp = HYP_BASE) {
  taux <- hyp$taux_technique

  # Âge actuariel (majorations fumeur + prudence réglementaire)
  age_act <- age
  if (fumeur) age_act <- age_act + if (genre == "H") 3L else 2L
  age_act <- age_act + floor(hyp$marge_mortalite * 10 + 0.5)
  age_act <- min(age_act, 75L)

  pnu  <- prime_nette_unique_terme(age_act, n, genre, taux)      # pour 1 EUR
  pna  <- prime_nette_annuelle_terme(age_act, n, genre, taux) * C # en EUR
  ann  <- annuite_vie_terme(age_act, n, genre, taux)

  denom <- 1 - hyp$alpha - hyp$beta
  if (denom <= 0) stop("alpha + beta >= 1 : chargements incoherents")

  pc <- (pna + hyp$gamma * C + hyp$delta) / denom

  frais_acq  <- hyp$alpha * pc
  frais_gest <- hyp$beta  * pc
  frais_cap  <- hyp$gamma * C
  frais_fix  <- hyp$delta
  marge      <- pc - pna - frais_acq - frais_gest - frais_cap - frais_fix

  list(
    age                      = age,
    age_actuariel            = age_act,
    genre                    = genre,
    fumeur                   = fumeur,
    duree                    = n,
    capital                  = C,
    pnu_unitaire             = round(pnu, 6),
    prime_nette_annuelle     = round(pna, 2),
    prime_commerciale        = round(pc, 2),
    dont_frais_acquisition   = round(frais_acq, 2),
    dont_frais_gestion       = round(frais_gest, 2),
    dont_frais_capital       = round(frais_cap, 2),
    dont_frais_fixes         = round(frais_fix, 2),
    marge_technique          = round(marge, 2),
    taux_chargement          = round((pc - pna) / pna * 100, 2),
    annuite_actuarielle      = round(ann, 4),
    hypotheses               = hyp$label
  )
}

# ── Tarification d'un portefeuille ────────────────────────────────────────────

tarifer_portefeuille <- function(portefeuille, hyp = HYP_BASE) {
  resultats <- lapply(seq_len(nrow(portefeuille)), function(i) {
    row <- portefeuille[i, ]
    res <- prime_commerciale(
      age    = as.integer(row$age_souscription),
      n      = as.integer(row$duree),
      C      = as.numeric(row$capital_assure),
      genre  = row$genre,
      fumeur = as.logical(row$fumeur),
      hyp    = hyp
    )
    res$id_contrat <- row$id_contrat
    res
  })

  # Convertir liste de listes en data.frame
  cols <- names(resultats[[1]])
  df   <- as.data.frame(
    lapply(cols, function(nm) vapply(resultats, function(r) as.character(r[[nm]]), character(1L))),
    stringsAsFactors = FALSE
  )
  names(df) <- cols

  # Typage numérique
  num_cols <- c("prime_nette_annuelle", "prime_commerciale", "pnu_unitaire",
                "dont_frais_acquisition", "dont_frais_gestion",
                "dont_frais_capital", "dont_frais_fixes",
                "marge_technique", "taux_chargement", "annuite_actuarielle",
                "capital", "age", "age_actuariel", "duree")
  for (col in num_cols) df[[col]] <- as.numeric(df[[col]])
  df
}

# ── Barème (grille âge × durée) ───────────────────────────────────────────────

bareme <- function(ages   = c(20, 25, 30, 35, 40, 45, 50, 55, 60),
                   durees = c(5, 10, 15, 20, 25, 30),
                   capital = 100000,
                   genre   = "H",
                   fumeur  = FALSE,
                   hyp     = HYP_BASE) {

  mat <- matrix(NA_real_, nrow = length(ages), ncol = length(durees),
                dimnames = list(ages, paste0("D", durees)))

  for (i in seq_along(ages)) {
    for (j in seq_along(durees)) {
      age <- ages[i]; n <- durees[j]
      if (age + n > 75L) next
      res           <- prime_commerciale(age, n, capital, genre, fumeur, hyp)
      mat[i, j]     <- round(res$prime_commerciale / capital * 1000, 3)
    }
  }
  as.data.frame(mat)
}
