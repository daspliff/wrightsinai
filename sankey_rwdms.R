library(networkD3)
library(tidyverse)
library(stringi)
library(ggsankey)
library(ggplot2)
library(ggalluvial)
library(plotly)
library(htmlwidgets)
library(RColorBrewer)

# Load your data with appropriate column names
data <- read.csv(
  "/Users/RM/OneDrive - The Mount Sinai Hospital/work/rwdms_assay/data/file_paths_20230905.txt", # nolint: line_length_linter.
  header = FALSE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UCS-2LE",
  col.names = c(
    "root_dir",
    "abs_path",
    "file_name",
    "file_ext",
    "file_size",
    "file_date",
    "file_owner"
  )
) %>%
  mutate(
    root_dir = str_replace_all(root_dir, "_", " "),
    # root_dir = stringi::stri_trans_totitle(root_dir),
    file_ext = str_squish(file_ext),
    file_size = file_size / (1024^3),
    abs_path = str_replace(abs_path, "D:\\\\", ""),
    file_owner = str_squish(str_replace(file_owner, ".*\\\\", "")),
    file_date = format(as.POSIXct(file_date, format = "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d") # noLint: line_length_linter
  ) %>%
  filter(root_dir != "tree") %>% # Remove the root directory
  filter(file_owner != "O:S-1-5-21-1503492212-1939026780-3606245752-1012" &
           file_owner != "")

# Categorize file extensions (uniform treatment & streamlined code)
data <- data %>%
  mutate(file_ext = tolower(str_trim(file_ext))) %>%
  mutate(category = case_when(
    file_ext == ".csv" & file_size > 0.5 ~ "Databases & Data Files",
    file_ext == ".csv" & file_size <= 0.5 ~ "Documents & Spreadsheets",
    file_ext %in% c(".txt", ".json", ".xlsx", ".docx", ".xls", ".doc", ".pptx", ".pdf") ~ "Documents & Spreadsheets",
    file_ext %in% c(".csv", ".txt", ".json", ".xlsx", ".docx", ".xls", ".doc", ".pptx", ".pdf") ~ "Documents & Spreadsheets",
    file_ext %in% c(".zip", ".gz", ".ZIP", ".GZ") ~ "Archive Files",
    file_ext %in% c(".tif", ".shp", ".shx", ".dbf", ".kml", ".kmz", ".geojson", ".gdbindexes", ".gdbtable", ".tiff", ".grib",
                    ".gdbtablx", ".gpkg", ".qmd", ".xyz", ".qgz", ".fits", ".las", ".los", ".img", ".ige", ".rde") ~ "Geospatial & GIS",
    file_ext %in% c(".xml", ".xsl", ".dtd", ".xsd", ".rng", ".xslt", ".html", ".css", ".js", ".scss", ".yaml", ".rst", ".md", ".toml") ~ "Markup & Web",
    file_ext %in% c(".jpg", ".jpeg", ".png", ".gif", ".eps", ".ico", ".xbm", ".ppm", ".svg", ".svgz", ".ani", ".cur", ".eps") ~ "Images",
    file_ext %in% c(".r", ".rhistory", ".rdata", ".rmd", ".py", ".pyc", ".pyw", ".pyi", ".pyd", ".c", ".cpp", ".h", ".ini", ".sh", ".ps1", 
                    ".csh", ".fish", ".tm", ".tcl", ".f90", ".f", ".bat", ".com") ~ "Programming & Scripts",
    file_ext %in% c(".gdb", ".xml", ".db", ".sql", ".sqlite", ".mat", ".rds", ".nc", ".ncf", ".dat", ".pickle", ".npy", ".npz", ".sas7bdat", ".pkl", ".fsl", ".rrd") ~ "Databases & Data Files",
    TRUE ~ "Others" # Catch-all for other file formats
  )) %>%
  group_by(category) %>%
  mutate(cat_size = sum(file_size, na.rm = TRUE)/1024) %>%
  mutate(category = ifelse(
    cat_size < 1,
    paste0(category, " (", round(cat_size * 1000, 1), " GB)"),
    paste0(category, " (", round(cat_size, 1), " TB)")
  )) %>%
  ungroup() %>%
  group_by(root_dir) %>%
  mutate(dir_size = sum(file_size, na.rm = TRUE)/1024) %>%
  mutate(root_dir = ifelse(
    dir_size < 1,
    paste0(root_dir, " (", round(dir_size * 1000, 1), " GB)"),
    paste0(root_dir, " (", round(dir_size, 1), " TB)")
  )) %>%
  ungroup()
  

# Prepare nodes & links (avoid duplication in nodes)
nodes <- data.frame(name = unique(c(data$root_dir, data$category)))
nodes$id <- seq(0, nrow(nodes) - 1)
links <- data %>%
  group_by(root_dir, category) %>%
  summarise(value = sum(file_size, na.rm = TRUE)) %>%
  left_join(nodes, by = c("root_dir" = "name")) %>%
  rename(source = id) %>%
  left_join(nodes, by = c("category" = "name")) %>%
  rename(target = id) %>%
  filter(!is.na(source) & !is.na(target))

################## Owner nodes #############
owner_nodes <- data.frame(name = unique(data$file_owner))
nodes <- rbind(select(nodes, "name"), owner_nodes)
nodes$id <- seq(0, nrow(nodes) - 1)

owner_links <- data %>%
  group_by(category, file_owner) %>%
  summarise(value = sum(file_size, na.rm = TRUE)) %>%
  left_join(nodes, by = c("category" = "name")) %>%
  rename(source = id) %>%
  left_join(nodes, by = c("file_owner" = "name")) %>%
  rename(target = id) %>%
  filter(!is.na(source) & !is.na(target))

# Combine old and new links
links <- rbind(links, owner_links)

############################################

# Choose a Vibrant Color Palette
# Define a vibrant color scale
color_scale <- RColorBrewer::brewer.pal(8, "Set3")

# Calculate the number of unique node names
num_unique_names <- length(unique(nodes$name))

# Repeat the color scale to match the number of unique node names
color_scale <- rep(color_scale, length.out = num_unique_names)

# Map the node names to colors
color_mapping <- setNames(color_scale, unique(nodes$name))


# Plot the Sankey diagram (enhanced settings)
sankeyPlot <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  NodeGroup = "name",
  colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10)"),
  fontSize = 17,
  nodeWidth = 35,
  nodePadding = 25,
  margin = list(left = 15, right = 15)
)

# Save or render the plot
saveNetwork(sankeyPlot, "/Users/RM/OneDrive - The Mount Sinai Hospital/work/rwdms_assay/sankey.html")
sankeyPlot




### Using Plotly ####
# Using previous nodes & links 
# Convert to a format compatible with plotly's sankey diagram
sankey_data <- list(
  type = "sankey",
  domain = list(
    x = c(0,1),
    y = c(0,1)
  ),
  orientation = "h",
  valueformat = ".0f",
  valuesuffix = "TWh",
  node = list(
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    ),
    label = nodes$name
  ),
  link = list(
    source = links$source,
    target = links$target,
    value = links$value
  )
)

# Given your nodes and links preparation

# Create a sankey diagram layout
layout <- list(
  title = "Sankey Diagram using Plotly",
  font = list(size = 10),
  updatemenu = list(type = 'buttons', 
                    showactive = FALSE, 
                    buttons = list(list(label = 'Toggle', method = 'relayout', args = list('paper_bgcolor', 'white')))
  )
)

# Plot Sankey diagram
fig <- plot_ly(
  type = "sankey",
  domain = list(
    x = c(0, 1),
    y = c(0, 1)
  ),
  orientation = "h",
  valueformat = ".0f",
  valuesuffix = "TWh",
  node = list(
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    ),
    label = nodes$name
  ),
  link = list(
    source = links$source,
    target = links$target,
    value = links$value
  )
) %>%
  layout(layout)

# Display the plot
fig
saveWidget(fig, "/Users/RM/OneDrive - The Mount Sinai Hospital/work/rwdms_assay/sankey_plotly.html") # nolint: line_length_linter.
