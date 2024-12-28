#!/usr/bin/env Rscript

################################################################################
### Packages/Libs
################################################################################
# Load or install required packages
install_or_load_packages <- function(packages) {
  for(package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package, repos = "http://cran.r-project.org")
      library(package, character.only = TRUE)
    } else {
      library(package, character.only = TRUE)
    }
  }
}

# List of packages you need
packages_needed <- c("optparse", "tidyverse", "jsonlite", "reshape2")

# Install or load the packages
install_or_load_packages(packages_needed)

################################################################################
### command line setup
################################################################################
option_list <- list(
  make_option(c("-j", "--json"), type="character", default=NULL, 
              help="path to the input file", metavar="character"),
  make_option(c("-v", "--verbose"), action="store_true", default=FALSE, 
              help="Print extra output [default %default]")
)

# Parse the arguments
opt_parser <- OptionParser(option_list=option_list)
opt <- parse_args(opt_parser)

# Check if required arguments are provided
if (is.null(opt$json)){
  print_help(opt_parser)
  stop("Input json file must be supplied (-j or --json)", call.=FALSE)
}


################################################################################
### functions
################################################################################
getData <- function(file) {
  #get base name for file
  fileName <- basename(file)
  #get file prefix for later
  filePrefix <- tools::file_path_sans_ext(fileName)
  #load json file
  json_data <- fromJSON(file)
  #extract confidence scores 
  confidence_scores <- json_data$pae
  #create properly formatted long data frame for graphing
  melted_confidence <- melt(confidence_scores)
  #set the column names in the new long data frame
  colnames(melted_confidence) <- c("Residue1", "Residue2", "Confidence")
  #Return a list of return values
  return(list(fileName = fileName,
              filePrefix = filePrefix,
              melted_confidence = melted_confidence))
}

plotHeatMap <- function(data, output_dir = ".") {
  pHM <- data$melted_confidence %>% ggplot(aes(x = Residue1, y = Residue2, fill = Confidence)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue",
                         mid = "white",
                         high = "red",
                         midpoint = 15) +
    labs(title = NULL, x = NULL, y = NULL)+
    theme_bw() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_reverse(expand = c(0, 0)) 
  
  pHM_name <- paste0(data$filePrefix, ".png")
  
  
  ggsave(filename = pHM_name, plot = pHM, width = 4, height = 3, dpi = 300)
  
}

makeConfidenceHeatMap <- function(file) {
  data <- getData(file)
  plotHeatMap(data)
}



################################################################################
### work
################################################################################

makeConfidenceHeatMap(opt$json)

cat(paste("Results have been saved for", opt$json, "\n"))

