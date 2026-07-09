# ─────────────────────────────────────────────────────────────────────────────
# Génération des tables de mortalité TH 00-02 et TF 00-02 (France)
# Modèle Gompertz-Makeham : mu(x) = A + B * c^x
# ─────────────────────────────────────────────────────────────────────────────

PARAMS <- list(
  TH = list(A = 0.0007, B = 0.00004, c = 1.1050),
  TF = list(A = 0.0004, B = 0.00002, c = 1.1100)
)

OMEGA <- 121L
L0    <- 100000

force_mortalite <- function(x, A, B, c) A + B * c^x

qx_from_makeham <- function(x, A, B, c) {
  res <- integrate(function(t) force_mortalite(x + t, A, B, c), 0, 1,
                   subdivisions = 500L, rel.tol = 1e-8)
  min(1.0, 1.0 - exp(-res$value))
}

build_table <- function(label) {
  p  <- PARAMS[[label]]
  ages <- 0:(OMEGA - 1L)

  qx_vals <- vapply(ages, function(x) qx_from_makeham(x, p$A, p$B, p$c),
                    numeric(1L))
  qx_vals[OMEGA] <- 1.0

  lx <- numeric(OMEGA + 1L)
  dx <- numeric(OMEGA)
  lx[1] <- L0
  for (i in seq_len(OMEGA)) {
    dx[i]     <- lx[i] * qx_vals[i]
    lx[i + 1] <- lx[i] - dx[i]
  }

  # Espérance de vie curtate e(x)
  ex_vals <- vapply(seq_len(OMEGA), function(i) {
    if (i >= OMEGA) return(0)
    tail_px <- cumprod(1 - qx_vals[i:min(i + 99L, OMEGA)])
    sum(tail_px)
  }, numeric(1L))

  data.frame(
    age = ages,
    qx  = round(qx_vals, 8),
    px  = round(1 - qx_vals, 8),
    lx  = round(lx[seq_len(OMEGA)], 4),
    dx  = round(dx, 4),
    ex  = round(ex_vals, 4)
  )
}

generate_tables <- function(output_dir = "data") {
  for (label in c("TH", "TF")) {
    cat(sprintf("Génération table %s...\n", label))
    df   <- build_table(label)
    path <- file.path(output_dir, sprintf("table_%s0002.csv", label))
    write.csv(df, path, row.names = FALSE)
    cat(sprintf("  q(40)=%.6f  q(60)=%.6f  [%d âges -> %s]\n",
                df$qx[41], df$qx[61], nrow(df), path))
  }
  invisible(NULL)
}
