# ─────────────────────────────────────────────────────────────────────────────
# Chargement et accès aux tables de mortalité
# Majoration fumeurs : +50% qx (H), +40% qx (F)
# ─────────────────────────────────────────────────────────────────────────────

.TABLE_CACHE <- new.env(parent = emptyenv())

charger_table <- function(genre = "H", data_dir = "data") {
  label <- if (genre == "H") "TH" else "TF"
  if (!exists(label, envir = .TABLE_CACHE)) {
    path <- file.path(data_dir, sprintf("table_%s0002.csv", label))
    df   <- read.csv(path)
    rownames(df) <- df$age
    assign(label, df, envir = .TABLE_CACHE)
  }
  get(label, envir = .TABLE_CACHE)
}

get_qx <- function(age, genre = "H", fumeur = FALSE) {
  table <- charger_table(genre)
  age   <- min(age, max(table$age))
  base  <- table[as.character(age), "qx"]
  if (fumeur) {
    coeff <- if (genre == "H") 1.50 else 1.40
    base  <- min(1.0, base * coeff)
  }
  base
}

get_lx <- function(age, genre = "H") {
  table <- charger_table(genre)
  age   <- min(age, max(table$age))
  table[as.character(age), "lx"]
}

get_ex <- function(age, genre = "H") {
  table <- charger_table(genre)
  age   <- min(age, max(table$age))
  table[as.character(age), "ex"]
}

# _t p_x : probabilité de survie de x à x+t
tpx <- function(age, t, genre = "H", fumeur = FALSE) {
  if (t <= 0) return(1.0)
  prod <- 1.0
  for (k in 0:(t - 1L)) {
    q    <- get_qx(age + k, genre, fumeur)
    prod <- prod * (1.0 - q)
    if (prod < 1e-12) break
  }
  prod
}
