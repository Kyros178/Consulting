# ── LIBRARIES ─────────────────────────────────────────
library(dplyr)
library(tidyr)
library(markovchain)

# ── LOAD DATA ─────────────────────────────────────────
setwd("~/Desktop/Statistical_Consulting")

bat   <- read.csv("rspb20141370supp2_bat.csv",       sep=";")
chick <- read.csv("rspb20141370supp2_Chickadee.csv", sep=";")
eng   <- read.csv("rspb20141370supp2_English.csv",   sep=";")
finch <- read.csv("rspb20141370supp2_Finch.csv",     sep=";")
hyrax <- read.csv("rspb20141370supp2_Hyrax.csv",     sep=";")
orang <- read.csv("rspb20141370supp2_Orangutan.csv", sep=";")
orca  <- read.csv("rspb20141370supp2_Orca.csv",      sep=";")
pilot <- read.csv("rspb20141370supp2_Pilot.csv",     sep=";")

cat("CSV files loaded \n")

# ── DATA TO LONG FORMAT ───────────────────────────────
to_long <- function(df) {
  n <- ncol(df)
  colnames(df)[1:n] <- paste0("pos_", 1:n)
  df %>%
    mutate(sequence_id = row_number()) %>%
    pivot_longer(
      cols      = -sequence_id,
      names_to  = "timepoint",
      values_to = "sound"
    ) %>%
    mutate(timepoint = as.integer(
      gsub("\\D", "", timepoint))) %>%
    filter(!is.na(sound)) %>%
    arrange(sequence_id, timepoint)
}

bat_long   <- to_long(bat)
chick_long <- to_long(chick)
eng_long   <- to_long(eng)
finch_long <- to_long(finch)
hyrax_long <- to_long(hyrax)
orang_long <- to_long(orang)
orca_long  <- to_long(orca)
pilot_long <- to_long(pilot)

cat("Long format done \n")

# ── EXTRACT SEQUENCES ─────────────────────────────────
get_seqs <- function(data_long) {
  data_long %>%
    arrange(sequence_id, timepoint) %>%
    group_by(sequence_id) %>%
    summarise(seq = list(sound), .groups = "drop") %>%
    pull(seq)
}

sequences_bat   <- get_seqs(bat_long)
sequences_chick <- get_seqs(chick_long)
sequences_eng   <- get_seqs(eng_long)
sequences_finch <- get_seqs(finch_long)
sequences_hyrax <- get_seqs(hyrax_long)
sequences_orang <- get_seqs(orang_long)
sequences_orca  <- get_seqs(orca_long)
sequences_pilot <- get_seqs(pilot_long)

cat("Sequences extracted \n")

# ── FIT FIRST ORDER MC ────────────────────────────────
bat_mc1   <- markovchainFit(sequences_bat,   method="mle")
chick_mc1 <- markovchainFit(sequences_chick, method="mle")
eng_mc1   <- markovchainFit(sequences_eng,   method="mle")
finch_mc1 <- markovchainFit(sequences_finch, method="mle")
hyrax_mc1 <- markovchainFit(sequences_hyrax, method="mle")
orang_mc1 <- markovchainFit(sequences_orang, method="mle")
orca_mc1  <- markovchainFit(sequences_orca,  method="mle")
pilot_mc1 <- markovchainFit(sequences_pilot, method="mle")

cat("MC1 fitted \n")

# ── LIKELIHOOD FUNCTIONS ──────────────────────────────

# MC0 initial distribution
get_mc0_probs <- function(data_long) {
  tbl <- prop.table(table(data_long$sound))
  setNames(as.numeric(tbl), as.character(names(tbl)))
}

# MC1 initial distribution (first element)
get_init_dist_mc1 <- function(data_long) {
  data_long %>%
    filter(timepoint == 1) %>%
    count(sound) %>%
    mutate(prob = n / sum(n)) %>%
    select(sound, prob)
}

# MC2 initial distribution (first TWO elements — bigram)
get_init_dist_mc2 <- function(data_long) {
  data_long %>%
    arrange(sequence_id, timepoint) %>%
    group_by(sequence_id) %>%
    slice(1:2) %>%
    mutate(pos = row_number()) %>%
    ungroup() %>%
    pivot_wider(
      id_cols     = sequence_id,
      names_from  = pos,
      values_from = sound,
      names_prefix = "s"
    ) %>%
    filter(!is.na(s1) & !is.na(s2)) %>%
    count(s1, s2) %>%
    mutate(prob = n / sum(n)) %>%
    mutate(pair = paste(s1, s2, sep="->")) %>%
    select(pair, prob)
}

# ── LOG LIKELIHOOD FUNCTIONS ──────────────────────────

get_loglik_mc0 <- function(sequences, mc0_probs) {
  log_lik <- 0
  for (seq in sequences) {
    for (s in seq) {
      p <- ifelse(as.character(s) %in% names(mc0_probs),
                  mc0_probs[as.character(s)], 1e-10)
      log_lik <- log_lik + log(p)
    }
  }
  return(log_lik)
}

get_loglik_mc1 <- function(sequences, mc1, init_dist) {
  init_vec <- setNames(init_dist$prob,
                       as.character(init_dist$sound))
  tm       <- as(mc1$estimate, "matrix")
  log_lik  <- 0
  for (seq in sequences) {
    if (length(seq) < 1) next
    first  <- as.character(seq[1])
    p_init <- ifelse(first %in% names(init_vec),
                     init_vec[first], 1e-10)
    log_lik <- log_lik + log(p_init)
    if (length(seq) > 1) {
      for (i in 1:(length(seq)-1)) {
        from <- as.character(seq[i])
        to   <- as.character(seq[i+1])
        if (from %in% rownames(tm) &&
            to   %in% colnames(tm)) {
          p <- tm[from, to]
          p <- ifelse(p == 0, 1e-10, p)
          log_lik <- log_lik + log(p)
        }
      }
    }
  }
  return(log_lik)
}

get_loglik_mc2 <- function(data_long, init2_dist) {
  # Build second order transition matrix
  t2 <- data_long %>%
    arrange(sequence_id, timepoint) %>%
    group_by(sequence_id) %>%
    mutate(
      s_t2 = lag(sound, 1),
      s_t1 = sound,
      s_t0 = lead(sound, 1)
    ) %>%
    ungroup() %>%
    filter(!is.na(s_t2), !is.na(s_t0)) %>%
    mutate(from_pair = paste(s_t2, s_t1, sep="->"))
  
  mc2_mat <- prop.table(
    table(from=t2$from_pair, to=t2$s_t0),
    margin=1
  )
  
  # Initial bigram lookup
  init2_vec <- setNames(init2_dist$prob,
                        init2_dist$pair)
  
  log_lik <- 0
  
  # Process each sequence
  seqs <- data_long %>%
    arrange(sequence_id, timepoint) %>%
    group_by(sequence_id) %>%
    summarise(seq = list(sound), .groups="drop") %>%
    pull(seq)
  
  for (seq in seqs) {
    n <- length(seq)
    if (n == 0) next
    
    if (n == 1) {
      # Only one element — use MC1 initial
      first <- as.character(seq[1])
      log_lik <- log_lik + log(1e-10)
      
    } else {
      # Use bigram initial probability
      pair <- paste(seq[1], seq[2], sep="->")
      p_init2 <- ifelse(pair %in% names(init2_vec),
                        init2_vec[pair], 1e-10)
      log_lik <- log_lik + log(p_init2)
      
      # Transitions from position 3 onwards
      if (n >= 3) {
        for (i in 2:(n-1)) {
          fp <- paste(seq[i-1], seq[i], sep="->")
          ts <- as.character(seq[i+1])
          if (fp %in% rownames(mc2_mat) &&
              ts %in% colnames(mc2_mat)) {
            p <- mc2_mat[fp, ts]
            p <- ifelse(p == 0, 1e-10, p)
            log_lik <- log_lik + log(p)
          }
        }
      }
    }
  }
  return(log_lik)
}

cat("Functions defined \n")

# ── RUN ALL SPECIES ───────────────────────────────────
species_data <- list(
  Bat       = list(long=bat_long,
                   seqs=sequences_bat,   mc1=bat_mc1),
  Chickadee = list(long=chick_long,
                   seqs=sequences_chick, mc1=chick_mc1),
  Finch     = list(long=finch_long,
                   seqs=sequences_finch, mc1=finch_mc1),
  Hyrax     = list(long=hyrax_long,
                   seqs=sequences_hyrax, mc1=hyrax_mc1),
  Orangutan = list(long=orang_long,
                   seqs=sequences_orang, mc1=orang_mc1),
  Orca      = list(long=orca_long,
                   seqs=sequences_orca,  mc1=orca_mc1),
  Pilot     = list(long=pilot_long,
                   seqs=sequences_pilot, mc1=pilot_mc1),
  English   = list(long=eng_long,
                   seqs=sequences_eng,   mc1=eng_mc1)
)

results_all <- data.frame(
  Species  = character(),
  N_states = integer(),
  N_seqs   = integer(),
  LogLik_0 = numeric(),
  LogLik_1 = numeric(),
  LogLik_2 = numeric(),
  stringsAsFactors = FALSE
)

for (sp in names(species_data)) {
  d <- species_data[[sp]]
  
  cat("\n===", sp, "===\n")
  
  # MC0
  mc0   <- get_mc0_probs(d$long)
  ll0   <- get_loglik_mc0(d$seqs, mc0)
  
  # MC1
  init1 <- get_init_dist_mc1(d$long)
  ll1   <- get_loglik_mc1(d$seqs, d$mc1, init1)
  
  # MC2 with bigram initial state
  init2 <- get_init_dist_mc2(d$long)
  ll2   <- get_loglik_mc2(d$long, init2)
  
  cat("Initial state MC1 (first element):\n")
  print(init1)
  cat("\nInitial state MC2 (first two elements):\n")
  print(init2)
  cat("\nLogLik MC0:", round(ll0, 3), "\n")
  cat("LogLik MC1:", round(ll1, 3), "\n")
  cat("LogLik MC2:", round(ll2, 3), "\n")
  
  results_all <- rbind(results_all, data.frame(
    Species  = sp,
    N_states = length(unique(d$long$sound)),
    N_seqs   = length(d$seqs),
    LogLik_0 = round(ll0, 3),
    LogLik_1 = round(ll1, 3),
    LogLik_2 = round(ll2, 3)
  ))
}

# ── SUMMARY TABLE ─────────────────────────────────────
cat("\n\n=== FINAL SUMMARY — ALL SPECIES ALL ORDERS ===\n")
print(results_all)

# ── SAVE TO FILE ──────────────────────────────────────
write.csv(
  results_all,
  "log_likelihood_results.csv",
  row.names = FALSE
)
cat("\nResults saved to log_likelihood_results.csv \n")

