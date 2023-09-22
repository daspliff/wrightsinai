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
    root_dir = stringi::stri_trans_toupper(root_dir),
    file_ext = str_squish(file_ext),
    file_size = file_size / (1024^3),
    abs_path = str_replace(abs_path, "D:\\\\", ""),
    file_owner = str_squish(str_replace(file_owner, ".*\\\\", "")),
    file_date = format(as.POSIXct(file_date, format = "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d") # noLint: line_length_linter
  ) %>%
  filter(root_dir != "TREE") %>% # Remove the root directory
  filter(file_owner != "O:S-1-5-21-1503492212-1939026780-3606245752-1012" &
           file_owner != "")

# Categorize file extensions (uniform treatment & streamlined code)
data <- data %>%
  mutate(file_ext = tolower(str_trim(file_ext))) %>%
  mutate(category = case_when(
    file_ext == ".csv" & file_size > 0.5 ~ "Databases & Data Files",
    file_ext == ".csv" & file_size <= 0.5 ~ "Documents & Spreadsheets",
    file_ext %in% c(".txt", ".json", ".xlsx", ".docx", ".xls", 
                    ".doc", ".pptx", ".pdf") ~ "Documents & Spreadsheets",
    file_ext %in% c(".zip", ".gz") ~ "Archive Files",
    file_ext %in% c(".tif", ".shp", ".shx", ".dbf", 
                    ".kml", ".kmz", ".geojson", ".gdbindexes", 
                    ".gdbtable", ".tiff", ".grib", ".gdbtablx", 
                    ".gpkg", ".qmd", ".xyz", ".qgz", ".fits", 
                    ".las", ".los", ".img", ".ige", ".rde") ~ "Geospatial & GIS",
    file_ext %in% c(".xml", ".xsl", ".dtd", ".xsd",
                    ".rng", ".xslt", ".html", ".css", 
                    ".js", ".scss", ".yaml", ".rst", 
                    ".md", ".toml") ~ "Markup & Web",
    file_ext %in% c(".jpg", ".jpeg", ".png", ".gif", 
                    ".eps", ".ico", ".xbm", ".ppm", 
                    ".svg", ".svgz", ".ani", ".cur", ".eps") ~ "Images",
    file_ext %in% c(".r", ".rhistory", ".rdata", ".rmd", 
                    ".py", ".pyc", ".pyw", ".pyi", 
                    ".pyd", ".c", ".cpp", ".h", ".ini", 
                    ".sh", ".ps1", ".csh", ".fish", 
                    ".tm", ".tcl", ".f90", ".f", 
                    ".bat", ".com") ~ "Programming & Scripts",
    file_ext %in% c(".gdb", ".xml", ".db", ".sql", 
                    ".sqlite", ".mat", ".rds", ".nc", 
                    ".ncf", ".dat", ".pickle", ".npy", 
                    ".npz", ".sas7bdat", ".pkl", ".fsl", ".rrd") ~ "Databases & Data Files",
    TRUE ~ "Others" # Catch-all for other file formats
  )) %>%
  group_by(category) %>%
  mutate(cat_size = sum(file_size, na.rm = TRUE)/1024) %>%
  mutate(category = ifelse(
    cat_size < 0.001, # Files less than 1 GB
    paste0(category, " (", round(cat_size * 1000000, 1), " MB)"),
    ifelse(
      cat_size < 1, # Files between 1 GB and 1 TB
      paste0(category, " (", round(cat_size * 1000, 1), " GB)"),
      paste0(category, " (", round(cat_size, 1), " TB)") # Files greater than 1 TB
    )
  )) %>%
  ungroup() %>%
  group_by(root_dir) %>%
  mutate(dir_size = sum(file_size, na.rm = TRUE)/1024) %>%
  mutate(root_dir = ifelse(
    dir_size < 0.001, # Files less than 1 GB
    paste0(root_dir, " (", round(dir_size * 1000000, 1), " MB)"),
    ifelse(
      dir_size < 1, # Files between 1 GB and 1 TB
      paste0(root_dir, " (", round(dir_size * 1000, 1), " GB)"),
      paste0(root_dir, " (", round(dir_size, 1), " TB)") # Files greater than 1 TB
    )
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

################## Add Owner nodes #############
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

# Define a color scale
set.seed(as.numeric(Sys.time()))
color_scale <- c('#e6194B', '#3cb44b', '#ffe119', '#4363d8', '#f58231', 
                 '#911eb4', '#42d4f4', '#f032e6', '#bfef45', '#fabed4', 
                 '#469990', '#dcbeff', '#9A6324', '#fffac8', '#800000', 
                 '#aaffc3', '#808000', '#ffd8b1', '#000075', '#a9a9a9',
                 '#d62728', '#bcbd22', '#1f77b4'
)

# Calculate the number of unique node names
num_unique_names <- length(unique(nodes$name))

# Shuffle and repeat the color scale to match the number of unique node names
color_scale <- sample(rep(color_scale, ceiling(num_unique_names / length(color_scale))))
color_scale <- color_scale[1:num_unique_names]

# Map the node names to colors
color_mapping <- setNames(color_scale, unique(nodes$name))

# Apply the color mapping to the nodes data frame
nodes$color <- color_mapping[nodes$name]

# Plot the Sankey diagram (enhanced settings)
sankeyPlot <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  NodeGroup = "color",
  fontSize = 14,
  fontFamily = "Helvetica",
  nodeWidth = 35,
  nodePadding = 25,
  # sinksRight = TRUE,
  margin = list(left = 15, right = 15)
)

# # Add plot title NOT WORKING CORRECTLY!!!!
# sankeyPlot <- htmlwidgets::prependContent(sankeyPlot,
#                                           htmltools::tags$h4("Analysis of RWDMS Filesystem",
#                                                              style="text-align:center; color:#a9a9a9;"))

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

# Create a sankey diagram layout
layout <- list(
  title = "Sankey Diagram using Plotly",
  font = list(size = 10),
  updatemenu = list(type = 'buttons', 
                    showactive = FALSE, 
                    buttons = list(list(label = 'Toggle', 
                                        method = 'relayout', 
                                        args = list('paper_bgcolor', 'white')))
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
