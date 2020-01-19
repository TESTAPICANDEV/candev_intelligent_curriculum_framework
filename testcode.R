library(tidyverse)

# Read in data
metadata <- read_csv(file.path(getwd(), "GcCampus metadata.csv"), skip = 1)
catalogue <- read_csv(file.path(getwd(), "catalogue.csv"))
translations <- read_csv(file.path(getwd(), "CST20160704.csv"))
