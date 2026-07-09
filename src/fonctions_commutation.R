# ─────────────────────────────────────────────────────────────────────────────
# Fonctions de commutation actuarielles
#
#   D_x = v^x * l_x
#   C_x = v^(x+1) * d_x
#   M_x = sum_{t=x}^{omega} C_t   (cumul inverse)
#   N_x = sum_{t=x}^{omega} D_t   (cumul inverse)
#   R_x = sum_{t=x}^{omega} M_t   (cumul inverse)
# ─────────────────────────────────────────────────────────────────────────────

.COMM_CACHE <- new.env(parent = emptyenv())

calculer_commutation <- function(genre = "H", taux = 0.03) {
  key <- sprintf("%s_%.6f", genre, taux)
  if (exists(key, envir = .COMM_CACHE)) return(get(key, envir = .COMM_CACHE))

  table <- charger_table(genre)
  ages  <- table$age
  lx    <- table$lx
  dx    <- table$dx
  v     <- 1.0 / (1.0 + taux)

  Dx <- v^ages * lx
  Cx <- v^(ages + 1) * dx

  # Cumul de la droite vers la gauche : rev(cumsum(rev(x)))
  rev_cumsum <- function(x) rev(cumsum(rev(x)))

  Mx <- rev_cumsum(Cx)
  Nx <- rev_cumsum(Dx)
  Rx <- rev_cumsum(Mx)

  result <- list(ages = ages, Dx = Dx, Cx = Cx, Mx = Mx, Nx = Nx, Rx = Rx)
  assign(key, result, envir = .COMM_CACHE)
  result
}

# Indice (1-based) correspondant à un âge
.idx <- function(comm, age) which(comm$ages == age)

# ── Assurance décès temporaire ────────────────────────────────────────────────

prime_nette_unique_terme <- function(age, n, genre = "H", taux = 0.03) {
  comm <- calculer_commutation(genre, taux)
  ix   <- .idx(comm, age)
  ixn  <- .idx(comm, age + n)
  (comm$Mx[ix] - comm$Mx[ixn]) / comm$Dx[ix]
}

annuite_vie_terme <- function(age, n, genre = "H", taux = 0.03) {
  # ä_{x:n|} = (N_x - N_{x+n}) / D_x  (début de période)
  comm <- calculer_commutation(genre, taux)
  ix   <- .idx(comm, age)
  ixn  <- .idx(comm, age + n)
  (comm$Nx[ix] - comm$Nx[ixn]) / comm$Dx[ix]
}

prime_nette_unique_survie <- function(age, n, genre = "H", taux = 0.03) {
  # D_{x+n} / D_x
  comm <- calculer_commutation(genre, taux)
  ix   <- .idx(comm, age)
  ixn  <- .idx(comm, age + n)
  comm$Dx[ixn] / comm$Dx[ix]
}

prime_nette_annuelle_terme <- function(age, n, genre = "H", taux = 0.03) {
  pnu <- prime_nette_unique_terme(age, n, genre, taux)
  ann <- annuite_vie_terme(age, n, genre, taux)
  if (ann < 1e-10) return(pnu)
  pnu / ann
}

# ── Provision prospective ─────────────────────────────────────────────────────

provision_prospective <- function(age, n, t, P_nette, genre = "H", taux = 0.03) {
  # _tV = A^1_{x+t:n-t|} - P * ä_{x+t:n-t|}
  if (t >= n) return(0.0)
  age_t  <- age + t
  n_t    <- n - t
  eng    <- prime_nette_unique_terme(age_t, n_t, genre, taux)
  annuit <- annuite_vie_terme(age_t, n_t, genre, taux)
  eng - P_nette * annuit
}

provision_retrospective <- function(age, n, t, P_nette, genre = "H", taux = 0.03) {
  # _tV_retro = (P * ä_{x:t|} - A^1_{x:t|}) * D_x / D_{x+t}
  if (t == 0) return(0.0)
  comm   <- calculer_commutation(genre, taux)
  ix     <- .idx(comm, age)
  ixt    <- .idx(comm, age + t)
  ann_xt <- annuite_vie_terme(age, t, genre, taux)
  pnu_xt <- prime_nette_unique_terme(age, t, genre, taux)
  ratio  <- comm$Dx[ix] / comm$Dx[ixt]
  (P_nette * ann_xt - pnu_xt) * ratio
}

profil_provisions <- function(age, n, P_nette_unit, C = 100000,
                               genre = "H", taux = 0.03) {
  ts <- 0:n
  provs <- vapply(ts, function(t)
    provision_prospective(age, n, t, P_nette_unit, genre, taux) * C,
    numeric(1L))
  data.frame(t = ts, age = age + ts, provision = round(provs, 2))
}
