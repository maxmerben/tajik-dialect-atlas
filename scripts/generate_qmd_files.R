cat("Starting QMD generation...\n")

# Set working directory
setwd("D:/Учёба/Фарси/tajik dialect atlas/github")

# Read data files
cat("Reading data files...\n")
if (file.exists('data/features.csv')) {
  features <- read_delim("data/features.csv",
                         delim = ",", show_col_types = FALSE)
  cat("✓ Features database loaded:", nrow(features), "rows\n")
} else {
  stop("Features database file not found: data/features.csv")
}

#features <- drop_na(features, "value")
features$feature_rus <- as.factor(features$feature_rus)

if (file.exists('data/coordinates.csv')) {
  coordinates <- read_delim("data/coordinates.csv",
                          delim = ",", show_col_types = FALSE)
  cat("✓ Villages database loaded:", nrow(coordinates), "rows\n")
} else {
  stop("Villages database not found: data/coordinates.csv")
}

features <- left_join(features, coordinates)

# Get unique features
unique_features <- features |> 
  group_by(feature_id, feature_rus) |>
  summarise(
    unique_settlements = n_distinct(settlement_id),
    datapoints = n(),
    unique_values = n_distinct(value),
    .groups = 'drop'
  ) |>
  arrange(feature_id)

cat("Found", nrow(unique_features), "unique features to process\n")

# Function to create QMD file
create_qmd_file <- function(feature_row) {
  print(feature_row)
  feature_id <- feature_row$feature_id
  feature_title <- feature_row$feature_rus
  #feature_description <- feature_row$feature_description
  #compiled <- feature_row$compiled
  
  # Get data for this feature
  feature_data <- features |> filter(feature_id == !!feature_id)
  feature_values <- sort(unique(feature_data$value))

  # Create YAML header with proper CSS styling
  yaml_header <- paste0(
    "---\n",
    "title: \"", feature_title, "\"\n",
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
  
  # Add feature description if available
  #description_section <- ""
  #if (!is.na(feature_description) && feature_description != "" && feature_description != "NA") {
  #  description_section <- paste0(feature_description, "\n\n")
  #}
  
  # Map
  
  map <- str_c('
```{r setup, echo=FALSE, include=FALSE, warning=FALSE}
knitr::opts_chunk$set(
  echo = FALSE, message = FALSE, warning = FALSE,
  fig.height = 5, fig.width = 6)

library(tidyverse)
library(RColorBrewer)
library(leaflet)
library(leaflet.minicharts)
library(lingtypology)
library(DT)

setwd("D:/Учёба/Фарси/tajik dialect atlas/github")
```

::: {{.panel-tabset}}

```{r echo=FALSE, include=FALSE}
features <- read_delim("data/features.csv",
                       delim = ",", show_col_types = FALSE)
features <- drop_na(features, "value")
features$feature_rus <- as.factor(features$feature_rus)

coordinates <- read_delim("data/coordinates.csv",
                          delim = ",", show_col_types = FALSE)
features <- left_join(features, coordinates)

palette_set <- "Set1"

feature_data <- features |> filter(feature_id == ', feature_id, ')
feature_values <- sort(unique(feature_data$value))

lang_palette <- colorFactor(
  palette = palette_set, domain = feature_values)
```

### Map

```{r}
minichart_data <- feature_data %>%
  count(settlement_id, value) %>%
  pivot_wider(
    names_from = value,
    values_from = n,
    values_fill = 0
  ) %>%
  arrange(settlement_id)

feature_coordinates <- coordinates %>%
  filter(settlement_id %in% minichart_data$settlement_id)

feature_values_by_settlement <- feature_data %>%
  group_by(settlement_id) %>%
  summarise(
    values = paste(unique(value),
                   collapse = \'", "\'),
    .groups = "drop")
feature_coordinates$values <- str_c(
  \'"\', feature_values_by_settlement$values, \'"\')

feature_coordinates$html_popup <- str_c(
  "<b>", feature_coordinates$settlement_name_rastorgueva, "</b><br>",
  "modern name: ", feature_coordinates$settlement_name_official, "<br>",
  "modern country: ", feature_coordinates$country_modern, "<br>",
  "coordinates: ", feature_coordinates$lat, ", ",
  feature_coordinates$lon, "<br><br>",
  "values: ", feature_coordinates$values
)
  
map <- leaflet(feature_data) %>%
  addTiles() %>%
  leaflet.minicharts::addMinicharts(
    feature_coordinates$lon,
    feature_coordinates$lat,
    type = "pie",
    chartdata = minichart_data[, -1],
    #colorPalette = lang_palette(feature_values),
    #popup = leaflet.minicharts::popupArgs(
    #  html = feature_coordinates$html_popup),
    width = 16,
    opacity = 1,
    legendPosition="bottomleft") %>%
  addCircleMarkers(
    feature_coordinates$lon, feature_coordinates$lat,
    group = "villages",
    label = feature_coordinates$settlement_name_rastorgueva,
    labelOptions = labelOptions(textsize = "12px"),
    popup = feature_coordinates$html_popup,
    fillColor = "white",
    fillOpacity = 0,
    opacity = 0,
    radius = 8) -> map
map
```

### Data

```{r}
DT::datatable(feature_coordinates %>%
                select(
                  settlement_name_rastorgueva,
                  settlement_name_official,
                  lat, lon,
                  values), 
              class = "cell-border stripe",
              rownames = FALSE,
              filter = "top",
              options = list(pageLength = 10, 
                           autoWidth = TRUE,
                           info = FALSE))
```
')
  
  # Combine all sections
  qmd_content <- paste0(yaml_header, map)
  
  # Create filename
  filename <- str_c("feature_", feature_id, ".qmd")
  
  # Write QMD file
  writeLines(qmd_content, filename)
  
  return(filename)
}

# Remove existing QMD files (except core pages)
cat("Cleaning up existing QMD files...\n")
existing_qmd_files <- list.files(".", pattern = "\\.qmd$")
core_files <- c("index.qmd", "about.qmd", "features.qmd")
to_remove <- existing_qmd_files[!(existing_qmd_files %in% core_files)]
if (length(to_remove) > 0) {
  file.remove(to_remove)
  cat("Removed", length(to_remove), "existing QMD files\n")
}

# Generate QMD files for all features
cat("Generating QMD files for all features...\n")
generated_files <- c()

for (i in 1:nrow(unique_features)) {
  feature_row <- unique_features[i, ]
  cat(sprintf("[%d/%d] Processing feature %d: %s\n", 
              i, nrow(unique_features), feature_row$feature_id, feature_row$feature_title))
  
  filename <- create_qmd_file(feature_row)
  generated_files <- c(generated_files, filename)
}

unique_features %>%
  group_by(feature_id, feature_rus) %>%
  summarise(html = str_c(
      feature_id, ". <a href='/feature_", feature_id, ".html'>",
      feature_rus, "</a>"),
    .groups = "drop") -> unique_features_html
unique_features$html <- unique_features_html$html

writeLines(paste(unique_features$html, collapse="\n"), "features_list.txt")

cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("Successfully generated", length(generated_files), "QMD files\n")
