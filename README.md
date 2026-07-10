# Tarification Assurance Décès Temporaire

Implémentation complète en R de la tarification, du provisionnement et de l'analyse d'un portefeuille d'assurances décès temporaires (ADT). Ce projet produit des barèmes de primes, des provisions mathématiques, six graphiques ggplot2 et un fichier Excel exportable.

---

## Prérequis

| Élément | Version minimale |
|---------|-----------------|
| R | ≥ 4.1 |
| ggplot2 | ≥ 3.4 |
| dplyr | ≥ 1.1 |
| tidyr | ≥ 1.3 |
| patchwork | ≥ 1.1 |
| openxlsx | ≥ 4.2 |

---

## Installation des packages

Lancer une fois :

```r
source("installer_packages.R")
```

Ou manuellement :

```r
install.packages(c("ggplot2", "dplyr", "tidyr", "patchwork", "openxlsx"))
```

---

## Lancement

**Depuis RStudio** — ouvrez `main.Rmd` avec le Project File `Tarification temp DC.Rproj` et exécuter le fichier :

---

## Structure du projet

```
/
├── Tarification temp DC.Rproj  # Project File
├── installer_packages.R        # Installation des dépendances
├── main.Rmd                    # Point d'entrée principal à exécuter
├── main.html                   # Sortie html après le Knit
│
├── src/
│   ├── tables_mortalite.R      # Chargement et accès aux tables TH/TF
│   ├── fonctions_commutation.R # Dx, Cx, Mx, Nx — primes nettes, annuités
│   ├── tarification.R          # Prime commerciale, barèmes, scénarios
│   ├── provisions.R            # Provisions prospectives, bilan portefeuille
│   └── rapport.R               # Graphiques ggplot2 et export Excel
│
├── data/
│   ├── generate_tables.R       # Génération des tables TH/TF 00-02 (Makeham)
│   ├── generate_portfolio.R    # Simulation du portefeuille de contrats
│   ├── table_TH0002.csv        # Table de mortalité Hommes (générée)
│   ├── table_TF0002.csv        # Table de mortalité Femmes (générée)
│   └── portefeuille.csv        # Portefeuille simulé (généré)
│
└── output/                     # Sorties générées (PNG + Excel)
    ├── 01_tables_mortalite.png
    ├── 02_baremes_tarification.png
    ├── 03_decomposition_prime.png
    ├── 04_profil_provisions.png
    ├── 05_analyse_portefeuille.png
    ├── 06_sensibilite.png
    ├── rapport_tarification.xlsx
    └── tarification_portefeuille.csv
```

---

## Modules

### `src/tables_mortalite.R`

Charge les tables TH 00-02 et TF 00-02 avec cache en mémoire (`new.env`).

```r
charger_table(genre = "H")          # data.frame avec colonnes age, qx, lx, dx, ex
get_qx(age = 40, genre = "H", fumeur = FALSE)
get_ex(age = 40, genre = "H")
tpx(age = 40, t = 5, genre = "H")  # probabilité de survie t années
```

### `src/fonctions_commutation.R`

Calcule les fonctions de commutation (Dx, Cx, Mx, Nx) avec cache par couple `(genre, taux)`.

```r
prime_nette_unique_terme(age = 40, n = 20, genre = "H", taux = 0.02)
annuite_vie_terme(age = 40, n = 20, genre = "H", taux = 0.02)
prime_nette_annuelle_terme(age = 40, n = 20, genre = "H", taux = 0.02)
provision_prospective(age = 40, n = 20, t = 5, P_nette = 0.007516,
                      genre = "H", taux = 0.02)
profil_provisions(age = 40, n = 20, P_nette = 0.007516,
                  C = 150000, genre = "H", taux = 0.02)
```

### `src/tarification.R`

Hypothèses tarifaires, prime commerciale et barèmes.

```r
# Hypothèses prédéfinies
HYP_BASE    # i=2%, marge mortalité 10%
HYP_SS2     # i=0%, marge mortalité 15% (Solvabilité II)
HYP_STRESS  # i=1%, marge mortalité 25%

# Créer des hypothèses personnalisées
h <- nouvelles_hypotheses(taux_technique = 0.03, marge_mortalite = 0.10)

# Tarification unitaire
res <- prime_commerciale(age = 40, n = 20, C = 150000,
                         genre = "H", fumeur = FALSE, hyp = HYP_BASE)
res$prime_commerciale       # 1 398,22 €
res$prime_nette_annuelle    # 1 127,47 €
res$taux_chargement         # 24,01 %

# Barème (taux pour mille du capital)
barem <- bareme(genre = "H", fumeur = FALSE, hyp = HYP_BASE)

# Tarification d'un portefeuille entier
df_tarif <- tarifer_portefeuille(df_port, hyp = HYP_BASE)
```

**Structure des chargements :**

| Paramètre | Valeur | Description |
|-----------|--------|-------------|
| `alpha` | 8 % | Frais d'acquisition (% prime commerciale) |
| `beta` | 6 % | Frais de gestion (% prime commerciale) |
| `gamma` | 0,04 % | Frais sur capital (% du capital / an) |
| `delta` | 15 € | Frais fixes annuels par contrat |

Formule : `Pc = (Pnette + γ·C + δ) / (1 − α − β)`

### `src/provisions.R`

```r
df_prov <- provisions_portefeuille(df_port, hyp = HYP_BASE)
bilan   <- bilan_provisions(df_prov)
bilan$provisions_totales       # total des provisions du portefeuille
bilan$provision_par_euro_cap   # ratio provision / capital
```

### `src/rapport.R`

Génère six graphiques PNG dans `output/` et un fichier Excel.

```r
graph_mortalite(th, tf)              # 01 — courbes qx et log(qx)
graph_bareme(barem_H, barem_F)       # 02 — grilles de primes
graph_decomposition(ex)              # 03 — décomposition prime commerciale
graph_provisions(df_prov_ex, 40, 20) # 04 — profil temporel des provisions
graph_portefeuille(df_port)          # 05 — analyse du portefeuille
graph_sensibilite(sensi_taux, sensi_mort) # 06 — sensibilités taux et mortalité
export_excel(barem_H, barem_F, barem_H_fum, df_tarif, df_prov)
```

---

## Modèle de mortalité

Les tables sont construites par intégration numérique de la loi de **Gompertz-Makeham** :

```
μ(x) = A + B · cˣ
qₓ   = 1 − exp(−∫₀¹ μ(x+t) dt)
```

| Paramètre | Hommes (TH) | Femmes (TF) |
|-----------|-------------|-------------|
| A | 0,0007 | 0,0004 |
| B | 0,00004 | 0,00002 |
| c | 1,1050 | 1,1100 |
| Radix l₀ | 100 000 | 100 000 |
| Âge limite ω | 121 | 121 |

---

## Paramètres modifiables

Pour changer les hypothèses tarifaires, modifier `src/tarification.R` ou créer de nouvelles hypothèses avec `nouvelles_hypotheses()`. Les paramètres clés :

```r
h <- nouvelles_hypotheses(
  taux_technique  = 0.02,   # taux d'actualisation prudentiel
  marge_mortalite = 0.10,   # majoration réglementaire sur qx (+1 an / 10%)
  alpha           = 0.08,   # frais acquisition
  beta            = 0.06,   # frais gestion
  gamma           = 0.0004, # frais capital
  delta           = 15.0    # frais fixes (€)
)
```

Pour modifier la taille ou la composition du portefeuille simulé :

```r
df_port <- generate_portfolio(n = 5000L, seed = 42L)
```

---

## Sorties produites

| Fichier | Contenu |
|---------|---------|
| `output/01_tables_mortalite.png` | Courbes qx (linéaire et log) TH/TF |
| `output/02_baremes_tarification.png` | Grille de primes ‰ par âge et durée |
| `output/03_decomposition_prime.png` | Décomposition prime nette / frais / marge |
| `output/04_profil_provisions.png` | Évolution de la provision sur la durée du contrat |
| `output/05_analyse_portefeuille.png` | Distributions âge, capital, durée, genre/fumeur |
| `output/06_sensibilite.png` | Impact du taux technique et de la marge mortalité |
| `output/rapport_tarification.xlsx` | Barèmes H/F, tarification et provisions (500 premiers contrats) |
| `output/tarification_portefeuille.csv` | Tarification complète du portefeuille |
