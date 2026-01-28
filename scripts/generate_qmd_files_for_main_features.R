cat("Starting QMD generation...\n")

# Set working directory
setwd("D:/Учёба/Фарси/tajik-dialect-atlas")

# Read data files
cat("Reading data files...\n")
if (file.exists('data/features.csv')) {
  features <- read_delim("data/features.csv",
                         delim = ",", show_col_types = FALSE)
  cat("✓ Features database loaded:", nrow(features), "rows\n")
} else {
  stop("Features database file not found: data/features.csv")
}

#features <- drop_na(features, "value_eng")
features$feature_orig <- as.factor(features$feature_orig)
features$feature_rus <- as.factor(features$feature_rus)
features$feature_eng <- as.factor(features$feature_eng)

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
  group_by(feature_id, feature_orig, feature_eng) |>
  summarise(
    unique_settlements = n_distinct(settlement_id),
    datapoints = n(),
    unique_values = n_distinct(value_eng),
    .groups = 'drop'
  ) |>
  arrange(feature_id)

cat("Found", nrow(unique_features), "unique features to process\n")

# Function to create QMD file
create_qmd_file <- function(feature_row) {
  feature_id <- feature_row$feature_id
  feature_title <- feature_row$feature_eng
  
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

setwd("D:/Учёба/Фарси/tajik-dialect-atlas")
```

::: {{.panel-tabset}}

```{r echo=FALSE, include=FALSE}
features <- read_delim("data/features.csv",
                       delim = ",", show_col_types = FALSE)
features <- drop_na(features, "value_eng")

dialects <- read_delim("data/features_dialects.csv",
                       delim = ",", show_col_types = FALSE)
names(dialects)[names(dialects) == "value_orig"] <- "dialect_orig"
names(dialects)[names(dialects) == "value_rus"] <- "dialect_rus"
names(dialects)[names(dialects) == "value_eng"] <- "dialect_eng"

dialects <- dialects[c("settlement_id",
           "dialect_orig", "dialect_rus", "dialect_eng",
           "group_orig", "group_rus", "group_eng")]

coordinates <- read_delim("data/coordinates.csv",
                          delim = ",", show_col_types = FALSE)
coordinates <- left_join(coordinates, dialects)
features <- left_join(features, coordinates)

feature_data <- features |> filter(feature_id == ', feature_id, ')
```

```{r echo=FALSE, include=FALSE}

# SORTING AND FACTORING

sort_smart <- function(tbl, col) {
  col <- dplyr::enquo(col)

  tbl %>%
    mutate(
      .sort_key = case_when(
        is.na(!!col) ~ 2L,
        !!col == "no data" ~ 2L,
        TRUE ~ 1L
      )
    ) %>%
    arrange(.sort_key, !!col) %>%
    select(-.sort_key)
}

feature_data <- feature_data |> sort_smart(value_eng)

feature_values <- unique(feature_data$value_eng)

feature_data$feature_orig <- factor(feature_data$feature_orig)
feature_data$feature_rus <- factor(feature_data$feature_rus)
feature_data$feature_eng <- factor(feature_data$feature_eng)

feature_data$value_orig <- factor(feature_data$value_orig)
feature_data$value_rus <- factor(feature_data$value_rus)
feature_data$value_eng <- factor(feature_data$value_eng)
```

```{r echo=FALSE, include=FALSE}
palette_set <- "Set1"

value_palette <- colorFactor(
  palette = palette_set,
  domain = feature_values
)
value_colors <- value_palette(feature_values)
value_colors[feature_values %in% c("no data", "нет данных")] <- "lightgray"
```

### Map

```{r}
minichart_data <- feature_data %>%
  count(number, value_eng) %>%
  sort_smart(value_eng) %>%
  pivot_wider(
    names_from = value_eng,
    values_from = n,
    values_fill = 0
  ) %>%
  arrange(number)

coordinates.current <- coordinates %>%
  filter(number %in% minichart_data$number)

x <- feature_data %>%
  group_by(number) %>%
  summarise(
    values = paste(unique(value_eng),
                   collapse = \', \'),
    .groups = "drop")
coordinates.current$values <- x$values
rm(x)

coordinates.current$html_popup <- str_c(
  "<b>", coordinates.current$settlement_name_official_full, "</b><br>",
  "ID: ", coordinates.current$settlement_id, "<br>",
  "name in the original source: ", coordinates.current$settlement_name_rastorgueva, "<br>",
  "modern country: ", coordinates.current$country_modern, "<br>",
  "coordinates: ", coordinates.current$lat, ", ",
  coordinates.current$lon, "<br><br>",
  "dialect: ", coordinates.current$dialect_eng, "<br>",
  "dialect group: ", coordinates.current$group_eng, "<br><br>",
  "values: ", coordinates.current$values
)
  
map <- leaflet(feature_data) %>%
  addTiles() %>%
  leaflet.minicharts::addMinicharts(
    coordinates.current$lon,
    coordinates.current$lat,
    type = "pie",
    chartdata = minichart_data[, -1],
    colorPalette = value_colors,
    width = 16,
    opacity = 1,
    legendPosition="bottomleft") %>%
  addCircleMarkers(
    coordinates.current$lon, coordinates.current$lat,
    group = "villages",
    label = coordinates.current$settlement_name_official_full,
    labelOptions = labelOptions(textsize = "12px"),
    popup = coordinates.current$html_popup,
    fillColor = "white",
    fillOpacity = 0,
    opacity = 0,
    radius = 8) %>%
  addLabelOnlyMarkers(
    coordinates.current$lon, coordinates.current$lat,
    group = "numbers",
    label = coordinates.current$settlement_id,
    labelOptions = labelOptions(
      noHide = TRUE, textOnly = TRUE,
      direction = "left", offset = c(-10, 1.7),
      style = list("font-size" = "14px"))
  ) %>%
  addLabelOnlyMarkers(
    coordinates.current$lon, coordinates.current$lat,
    group = "names",
    label = coordinates.current$settlement_name_official,
    labelOptions = labelOptions(
      noHide = TRUE, textOnly = TRUE,
      direction = "right", offset = c(10, 1.7),
      style = list("font-size" = "14px"))
  ) %>%
  addLayersControl(
    overlayGroups = c("numbers", "names"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup("numbers") %>%
  hideGroup("names")
map
```

### Data

```{r}
names(coordinates.current)[names(coordinates.current) == "settlement_name_rastorgueva"] <- "name (orig.)"
names(coordinates.current)[names(coordinates.current) == "settlement_name_official_full"] <- "name (modern)"
DT::datatable(coordinates.current %>%
                arrange(number) %>%
                select(
                  `name (orig.)`,
                  `name (modern)`,
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
core_files <- c("index.qmd", "about.qmd", "features.qmd", "feature_18.qmd")
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
  if (feature_row$feature_eng != "Dialect groups") {
    cat(sprintf("[%d/%d] Processing feature %d: %s\n", 
                i, nrow(unique_features),
                feature_row$feature_id,
                feature_row$feature_title))
    
    filename <- create_qmd_file(feature_row)
    generated_files <- c(generated_files, filename)
  }
}

unique_features %>%
  group_by(feature_id, feature_eng) %>%
  summarise(html = str_c(
      feature_id, ". <a href='/feature_", feature_id, ".html'>",
      feature_eng, "</a>"),
    .groups = "drop") -> unique_features_html
unique_features$html <- unique_features_html$html

writeLines(paste(unique_features$html, collapse="\n"),
           "features_list.txt")

cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("Successfully generated", length(generated_files), "QMD files\n")
