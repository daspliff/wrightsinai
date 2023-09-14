library(networkD3)
library(tidyverse)
library(stringi)
library(ggsankey)
library(ggplot2)
library(ggalluvial)

# Load your data with appropriate column names
data <- read.csv(
  "/Users/RM/Library/CloudStorage/OneDrive-TheMountSinaiHospital/work/rwdms_assay/file_paths_20230830.txt",
  header = FALSE, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UCS-2LE",
  col.names = c("root_dir", "abs_path", "file_name", "file_ext", "file_size") 
) %>%  filter(root_dir != "tree")

# Categorize file extensions (uniform treatment & streamlined code)
data <- data %>%
  mutate(file_ext = tolower(str_trim(file_ext))) %>%
  mutate(category = case_when(
    file_ext %in% c(".csv", ".txt", ".json") ~ "Text-based Files", # Common text-based file formats
    file_ext %in% c(".nc", ".ncf", ".hdf", ".mat", ".rds", ".rdata", ".pickle", 
                    ".npy", ".npz", ".fits", ".sas7bdat") ~ "Data Files", # Data storage file formats
    file_ext %in% c(".tif", ".tiff", ".shp", ".shx", ".dbf", ".kml", ".kmz", 
                    ".grib", ".las", ".geojson", ".gdbindexes", ".gdbtable", 
                    ".gdbtablx") ~ "Geospatial and GIS Files", # Geospatial file formats
    file_ext %in% c(".sql", ".sqlite", ".db", ".gdb", ".xml") ~ "Database Files", # Database related file formats
    file_ext %in% c(".zip", ".gz") ~ "Archive Files", # Archive file formats
    file_ext %in% c(".pdf", ".docx", ".pptx", ".xls", ".xlsx", ".doc") ~ "Document Files", # Document file formats
    file_ext %in% c(".html", ".css", ".js", ".r", ".rmd", ".md", ".rst", ".yaml", ".toml") ~ "Script and Markup Files", # Script and markup file formats
    file_ext %in% c(".py", ".pyc", ".php", ".cpp", ".c", ".h", ".f90", ".f") ~ "Programming Files", # Programming related file formats
    file_ext %in% c(".exe", ".dll", ".bat", ".sh", ".ps1", ".ini") ~ "Executable and System Files", # Executable and system file formats
    file_ext %in% c(".jpg", ".jpeg", ".png", ".gif", ".svg", ".eps") ~ "Image Files", # Image file formats
    TRUE ~ "Others" # Catch-all for other file formats
  ))

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

# Plot the Sankey diagram (enhanced settings)
sankeyPlot <- sankeyNetwork(
  Links = links, 
  Nodes = nodes, 
  Source = "source", 
  Target = "target", 
  Value = "value", 
  NodeID = "name", 
  fontSize = 16,
  nodeWidth = 20, 
  margin = list(left = 100, right = 50)
)

sankeyPlot <- htmlwidgets::onRender(
  sankeyPlot,
  '
  function(el, x) {
  var width = d3.select(el).select("svg").attr("width");
  d3.selectAll(".node text").attr("text-anchor", "begin").attr("x", 30);
  }
  '
)

# Save or render the plot
saveNetwork(sankeyPlot, "sankey.html")
sankeyPlot


#################

# Create a summarized data frame with total file size for each root_dir and category pair
data_sum <- data %>%
  group_by(root_dir, category) %>%
  summarise(file_size_sum = sum(file_size, na.rm = TRUE), .groups = 'drop')

# Bin the file sizes into categories
data_sum <- data_sum %>%
  mutate(file_size_bin = cut(file_size_sum, 
                             breaks = c(-Inf, 1000, 10000, 100000, Inf), 
                             labels = c("Very Small", "Small", "Medium", "Large")))

# Prepare the data for the alluvial plot
data_long <- data_sum %>%
  pivot_longer(cols = c(root_dir, file_size_bin, category), 
               names_to = "variable", 
               values_to = "value")

# Create the Sankey plot
ggplot(data = data_long, 
       aes(axis1 = variable, axis2 = value, y = file_size_sum)) +
  geom_alluvium(aes(fill = value)) +
  geom_stratum(aes(fill = value)) + 
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Root Directory", "File Size", "Category"), expand = c(0.15, 0.05)) +
  theme_minimal() +
  labs(title = "Sankey Diagram Representing File Categories and Sizes")

#####################
# Your existing data manipulation steps (with a correction in the labels assignment)
data2 <- data %>% 
  select(root_dir, file_size, category) %>%
  mutate(
    root_dir = toupper(str_trim(str_replace_all(root_dir, "_", " "))),
    file_size = round(file_size / (1024)^3, 4),
    size_cat = cut(
      file_size,
      breaks = c(-Inf, 0.01, 0.1, 0.5, 1, 10, 50, 100, Inf),
      labels = c(
        "< 10 MB",
        "10 - 99 MB",
        "100 - 499 MB",
        "500 - 999 MB",
        "1 - 9 GB",
        "10 - 49 GB",
        "50 - 99 GB",
        "> 100 GB"
      ),
      right = FALSE
    )
  )

# Create dlong from data2 (before summarizing to get dagg)
dlong <- data2 %>%
  unite("node", c("category", "size_cat"), sep = " - ") %>%
  group_by(root_dir, node) %>%
  summarise(total_size = sum(file_size, na.rm = TRUE))

# Now create the plot with emphasis on the total size
pl <- ggplot(dlong, aes(axis1 = root_dir, axis2 = node, y = total_size)) +
  geom_alluvium(aes(fill = node)) +
  geom_stratum(aes(fill = node)) + 
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("root_dir", "node"), expand = c(0.15, 0.05)) +
  theme_minimal() +
  labs(title = "Analysis of RWDMS Filesystem")

# Display the plot
pl


########################
# Prepare your data

# Prepare your data
data2 <- data %>%
  select(root_dir, file_size, category) %>%
  mutate(
    root_dir = toupper(str_trim(str_replace_all(root_dir, "_", " "))),
    file_size = round(file_size / (1024)^3, 4),
    size_cat = cut(
      file_size,
      breaks = c(-Inf, 0.01, 0.1, 0.5, 1, 10, 50, 100, Inf),
      labels = c(
        "< 10 MB",
        "10 - 99 MB",
        "100 - 499 MB",
        "500 - 999 MB",
        "1 - 9 GB",
        "10 - 49 GB",
        "50 - 99 GB",
        "> 100 GB"
      ),
      right = FALSE
    )
  )

# Create a longer version of data2 to represent each transition
dlong <- data2 %>%
  group_by(root_dir, size_cat, category) %>%
  summarise(total_size = sum(file_size, na.rm = TRUE), .groups = 'drop') %>%
  pivot_longer(cols = c(root_dir, size_cat, category), names_to = "variable", values_to = "value")

# Create the Sankey plot with three nodes
pl <- ggplot(data = dlong,
             aes(axis1 = variable, axis2 = value, y = total_size)) +
  geom_alluvium(aes(fill = value)) +
  geom_stratum(aes(fill = value)) + 
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("root_dir", "size_cat", "category"), expand = c(0.15, 0.05)) +
  theme_minimal() +
  labs(title = "Analysis of Filesystem")

# Display the plot
print(pl)
