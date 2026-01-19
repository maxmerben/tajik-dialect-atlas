packages <- c("tidyverse", "RColorBrewer",
              "DT", "leaflet", "leaflet.minicharts",
              "lingtypology", "knitr")
to_install <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install) > 0) {
  cat("📦 Installing required packages...\n")
  install.packages(to_install, dependencies = TRUE)
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(RColorBrewer)
  library(DT)
  library(leaflet)
  library(leaflet.minicharts)
  library(lingtypology)
  library(knitr)
})

cat("✅ All packages loaded successfully\n\n")

source("scripts/generate_qmd_files_for_main_features.R")
source("scripts/generate_qmd_file_for_feature_18.R")
source("scripts/generate_qmd_file_for_index.R")

if (dir.exists("docs")) {
  unlink("docs", recursive = TRUE)
}
dir.create("docs")

# Render the document
system("quarto render --output-dir docs", intern = FALSE)

if (dir.exists("docs") && length(list.files("docs", pattern = "\\.html$")) > 3) {
  cat("Feature pages rendered successfully!\n")
} else {
  cat("Feature pages rendering failed!\n")
}


