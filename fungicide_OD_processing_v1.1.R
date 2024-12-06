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
packages_needed <- c("optparse", "tidyverse", "readxl")

# Install or load the packages
install_or_load_packages(packages_needed)

################################################################################
### command line setup
################################################################################
option_list <- list(
  make_option(c("-f", "--file"), type="character", default=NULL, 
              help="path to the input file", metavar="character"),
    make_option(c("-v", "--verbose"), action="store_true", default=FALSE, 
              help="Print extra output [default %default]")
)

# Parse the arguments
opt_parser <- OptionParser(option_list=option_list)
opt <- parse_args(opt_parser)

# Check if required arguments are provided
if (is.null(opt$file)){
  print_help(opt_parser)
  stop("Input file must be supplied (-f or --file)", call.=FALSE)
}


################################################################################
### functions
################################################################################
get_data_long <- function(file) {
  d445 <- read_excel(file, range = "A45:M53") %>%
    pivot_longer(cols = c(2:13), names_to = "Isolate", values_to = "OD")
  colnames(d445)[colnames(d445) == '<>'] <- "Treatment"
  c445 <- d445 %>% 
    filter(Treatment == "Control") %>%
    group_by(Isolate) %>%
    summarize(Control = mean(OD))
  d445c <- d445 %>% left_join(c445, by="Isolate")
  d445c <- d445c %>%
    mutate(Percent_Relative_Growth = (OD/Control)*100)
  
  d445cp <- d445c %>% 
    ggplot(aes(x = Treatment, y = Percent_Relative_Growth)) +
    geom_bar(stat = "identity", position = "dodge") +
    #add labels
    labs(title = "Percent of Control by Treatment and Individual (445nm)",
         x = "Fungicide",
         y = "Percent of Control (%)") +
    # Facet by treatment
    facet_wrap(~ Isolate, ncol = 4, scales = "free_x") +  # Adjust 'ncol' as needed
    # Theme adjustments
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  d615 <- read_excel(file, range = "A68:M76") %>%
    pivot_longer(cols = c(2:13), names_to = "Isolate", values_to = "OD")
  colnames(d615)[colnames(d615) == '<>'] <- "Treatment"
  c615 <- d615 %>% 
    filter(Treatment == "Control") %>%
    group_by(Isolate) %>%
    summarize(Control = mean(OD))
  d615c <- d615 %>% left_join(c615, by="Isolate")
  d615c <- d615c %>%
    mutate(Percent_Relative_Growth = (OD/Control)*100)
  write.csv(d615c, file = "Results_615.csv")
  
  d615cp <- d615c %>% 
    ggplot(aes(x = Treatment, y = Percent_Relative_Growth)) +
    geom_bar(stat = "identity", position = "dodge") +
    #add labels
    labs(title = "Percent of Control by Treatment and Individual (615nm)",
         x = "Fungicide",
         y = "Percent of Control (%)") +
    # Facet by treatment
    facet_wrap(~ Isolate, ncol = 4, scales = "free_x") +  # Adjust 'ncol' as needed
    # Theme adjustments
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  write.csv(d445c, file = "Results_445.csv")
  write.csv(d615c, file = "Results_615.csv")
  ggsave(filename = "Results_445.png", plot = d445cp, width = 6, height = 6, dpi = 300)
  ggsave(filename = "Results_615.png", plot = d615cp, width = 6, height = 6, dpi = 300)
}
################################################################################
### work
################################################################################

get_data_long(opt$file)

cat(paste("Results have been saved to has", opt$out, "\n"))

