# Installation des packages requis
pkgs <- c("ggplot2", "dplyr", "tidyr", "patchwork", "openxlsx")
manquants <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(manquants) > 0) {
  install.packages(manquants, repos = "https://cloud.r-project.org")
}
cat("Tous les packages sont installes.\n")
