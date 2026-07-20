##############################################################
## 01_data_prep.R
## Reads all species files, builds wide/long/seqdef formats
##############################################################

library(dplyr)
library(tidyr)
library(markovchain)
library(TraMineR)
library(seqHMM)

## ---- File map ----
file_map <- c(
  bat        = "rspb20141370supp2_bat.csv",
  chickadee  = "rspb20141370supp2_Chickadee.csv",
  english    = "rspb20141370supp2_English.csv",
  finch      = "rspb20141370supp2_Finch.csv",
  hyrax      = "rspb20141370supp2_Hyrax.csv",
  orangutan  = "rspb20141370supp2_Orangutan.csv",
  orca       = "rspb20141370supp2_Orca.csv",
  pilot      = "rspb20141370supp2_Pilot.csv"
)

## ---- Long-format helper ----
to_long <- function(df) {
  n_positions <- ncol(df)
  colnames(df)[1:n_positions] <- paste0("pos_", 1:n_positions)
  df %>%
    mutate(sequence_id = row_number()) %>%
    pivot_longer(cols = -sequence_id, names_to = "timepoint", values_to = "sound") %>%
    mutate(timepoint = as.integer(gsub("\\D", "", timepoint))) %>%
    filter(!is.na(sound)) %>%
    arrange(sequence_id, timepoint)
}

## ---- Read all species (pilot needs header = FALSE) ----
wide_data <- list()
long_data <- list()
seq_data  <- list()

for (sp in names(file_map)) {
  if (sp == "pilot") {
    df <- read.csv(file_map[sp], sep = ";", header = FALSE)
  } else {
    df <- read.csv(file_map[sp], sep = ";")
  }
  colnames(df) <- paste0("pos_", 1:ncol(df))
  wide_data[[sp]] <- df
  long_data[[sp]] <- to_long(df)
  seq_data[[sp]]  <- seqdef(df, informat = "STS", right = "DEL")
}

cat("Data prep complete. Species loaded:", paste(names(wide_data), collapse = ", "), "\n")

