# Load necessary library
library(quarto)

# Get the command line arguments
args <- commandArgs(trailingOnly = TRUE)

# Check if the correct number of arguments are provided
if (length(args) != 2) {
  stop("You must supply both the input and output file names.")
}

# Assign arguments to variables
input_file <- args[1]
output_file <- args[2]

# Check if the input file exists
if (!file.exists(input_file)) {
  stop("The input file does not exist.")
}

# Check if the output file already exists
if (file.exists(output_file)) {
  stop("The output file already exists.")
}

# Render the Quarto file using the input file name and output file name
quarto_render(input_file, output_file = output_file, output_format = "html")
