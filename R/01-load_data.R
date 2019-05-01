# Read and prepare data

library(tidyverse)
library(jahMisc)

# Read data
data <- readxl::read_xlsx("analysis/data/raw_data/WiSTEM Data10.xlsx")

# Data cleanup -----------------------------------------------------------------

# Rename columns to make it easier for writing scripts.
data <- data %>%
  rename(author = "Author") %>%
  rename(title = "Title") %>%
  rename(pubtitle = "Publication Title") %>%
  rename(publisher = "Publisher") %>%
  rename(date = "Date")

# Rename ASEE conferences -- they're the same, but labeled differently
data <- data %>%
  mutate_all(funs(str_replace(., "Proceedings of the ASEE Annual Conference & Exposition", "ASEE Annual Conference and Exposition, Conference Proceedings")))
