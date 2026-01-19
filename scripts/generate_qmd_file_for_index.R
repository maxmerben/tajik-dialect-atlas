# Set working directory
setwd("D:/Учёба/Фарси/tajik-dialect-atlas")

# Read data files
if (file.exists('data/features.csv')) {
  features <- read_delim("data/features.csv",
                         delim = ",", show_col_types = FALSE)
} else {
  stop("Features database file not found: data/features.csv")
}

if (file.exists('data/features_dialects.csv')) {
  features_dialects <- read_delim("data/features_dialects.csv",
                         delim = ",", show_col_types = FALSE)
} else {
  stop("Features database file not found: data/features_dialects.csv")
}
features_dialects$feature_eng <- paste0(
  "**", features_dialects$feature_eng, "**")

features <- bind_rows(features, features_dialects)

#features <- drop_na(features, "value_eng")
features$feature_orig <- as.factor(features$feature_orig)
features$feature_rus <- as.factor(features$feature_rus)
features$feature_eng <- as.factor(features$feature_eng)

# Get unique features
unique_features <- features |> 
  group_by(feature_id, feature_orig, feature_eng) |>
  summarise(
    unique_settlements = n_distinct(settlement_id),
    datapoints = n(),
    unique_values = n_distinct(value_eng),
    .groups = 'drop'
  ) |>
  arrange(feature_id)

unique_features %>%
  group_by(feature_id, feature_eng) %>%
  summarise(html = str_c(
    feature_id, ". <a href='/feature_", feature_id, ".html'>",
    feature_eng, "</a>"),
    .groups = "drop") -> unique_features_html
unique_features$html <- unique_features_html$html

yaml_header <- paste0(
  "---\n",
  "title: \"Digital Atlas of Tajik Dialects\"\n",
  #"author: \"", compiled, "\"\n",
  "date: ", Sys.Date(), "\n",
  "output:\n",
  "  html_document:\n",
  "    toc: true\n",
  "    toc_float: true\n",
  "    theme: cosmo\n",
  "    highlight: tango\n",
  "    css: styles.css\n",
  "---\n\n"
)

map <- str_c('
## Maps

', paste(unique_features$html, collapse="\n"), '

---

## How to cite

> Elizaveta Korobova, Timofey Lugovskoy, Maksim Melenchenko. **Digital Atlas of Tajik Dialects**. 2025.

## Acknowledgements

')

cat("Cleaning up existing QMD file for the index page ...\n")
to_remove <- list.files(".", pattern = "index.qmd$")
if (length(to_remove) > 0) {
  file.remove(to_remove)
}

cat("Generating QMD file for the index page...\n")

qmd_content <- paste0(yaml_header, map)
filename <- "index.qmd"
writeLines(qmd_content, filename)

cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("Successfully generated QMD file for the index page\n")
