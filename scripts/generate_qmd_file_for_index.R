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
  "title: \"Rastorgueva’s Atlas of Tajik Dialects\"\n",
  "date: ", Sys.Date(), "\n",
  "bibliography: data/bib.bib\n",
  "link-citations: TRUE\n",
  "output:\n",
  "  html_document:\n",
  "    toc: true\n",
  "    toc_float: true\n",
  "    theme: cosmo\n",
  "    highlight: tango\n",
  "    css: styles.css\n",
  "---\n\n"
)

description <- "This is a digitized version 
of the **dialectal atlas of the Tajik language** 
from the book *An experience of the comparative study 
of Tajik dialects* (Rus. *Опыт сравнительного изучения 
таджикских говоров*) by Soviet linguist Vera Rastorgueva 
[@rastorgueva1964] (<a href='/about.html'>read more</a>). 
It contains 17 maps that show the distribution of a variety 
of linguistic features in around 100 settlements in modern-day 
Tajikistan and Uzbekistan, as well as a general map of dialect groups.\n"

map <- str_c('
## Maps

', paste(unique_features$html, collapse="\n"), '

The original maps can be found <a href="/original.html">here</a>.

---

## How to cite

> Elizaveta Korobova, Timofey Lugovskoy, Maksim Melenchenko. **Rastorgueva’s Atlas of Tajik Dialects**. 2026.

## Acknowledgements

**TBD**

## References

')

cat("Cleaning up existing QMD file for the index page ...\n")
to_remove <- list.files(".", pattern = "index.qmd$")
if (length(to_remove) > 0) {
  file.remove(to_remove)
}

cat("Generating QMD file for the index page...\n")

qmd_content <- paste0(yaml_header, description, map)
filename <- "index.qmd"
writeLines(qmd_content, filename)

cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("Successfully generated QMD file for the index page\n")
