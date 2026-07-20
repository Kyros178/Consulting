##############################################################
## 03_markov_chains.R
## Fits MC0 (marginal), MC1 (1st order), MC2 (2nd order)
## for all species
##############################################################

source("01_data_prep.R")

## ---- MC0: marginal/stationary distribution ----
get_mc0 <- function(long_df) {
  prop.table(table(long_df$sound))
}

## ---- MC1: first-order Markov chain ----
get_mc1 <- function(long_df) {
  sequences <- long_df %>%
    arrange(sequence_id, timepoint) %>%
    group_by(sequence_id) %>%
    summarise(seq = list(sound), .groups = "drop") %>%
    pull(seq)
  markovchainFit(data = sequences, method = "mle")
}

## ---- MC2: second-order Markov chain ----
get_mc2 <- function(long_df) {
  transitions_2 <- long_df %>%
    arrange(sequence_id, timepoint) %>%
    group_by(sequence_id) %>%
    mutate(
      s_t2 = lag(sound, 1),
      s_t1 = sound,
      s_t0 = lead(sound, 1)
    ) %>%
    ungroup() %>%
    filter(!is.na(s_t2), !is.na(s_t0)) %>%
    mutate(from_pair = paste(s_t2, s_t1, sep = "->"))
  
  count_mat <- table(from = transitions_2$from_pair, to = transitions_2$s_t0)
  prop.table(count_mat, margin = 1)
}

## ---- Run for a single species first: hyrax ----
hyrax_mc0 <- get_mc0(long_data$hyrax)
hyrax_mc0

hyrax_mc1 <- get_mc1(long_data$hyrax)
hyrax_mc1$estimate

hyrax_mc2 <- get_mc2(long_data$hyrax)
round(hyrax_mc2, 3)
## ---- Log-likelihood functions (so MC0/MC1/MC2 are comparable to HMM) ----

loglik_mc0 <- function(long_df, probs) {
  p <- probs[as.character(long_df$sound)]
  sum(log(p))
}

loglik_mc1 <- function(long_df, mc1_fit) {
  tm <- mc1_fit$estimate@transitionMatrix
  seqs <- long_df %>%
    arrange(sequence_id, timepoint) %>%
    group_by(sequence_id) %>%
    summarise(seq = list(sound), .groups = "drop") %>%
    pull(seq)
  ll <- 0
  for (s in seqs) {
    if (length(s) > 1) {
      for (i in 2:length(s)) {
        p <- tm[as.character(s[i-1]), as.character(s[i])]
        if (p > 0) ll <- ll + log(p)
      }
    }
  }
  ll
}

loglik_mc2 <- function(long_df, mc2_mat) {
  trans <- long_df %>%
    arrange(sequence_id, timepoint) %>%
    group_by(sequence_id) %>%
    mutate(s_t2 = lag(sound, 1), s_t1 = sound, s_t0 = lead(sound, 1)) %>%
    ungroup() %>%
    filter(!is.na(s_t2), !is.na(s_t0)) %>%
    mutate(from_pair = paste(s_t2, s_t1, sep = "->"))
  ll <- 0
  for (i in seq_len(nrow(trans))) {
    fp <- trans$from_pair[i]
    to <- as.character(trans$s_t0[i])
    if (fp %in% rownames(mc2_mat) && to %in% colnames(mc2_mat)) {
      p <- mc2_mat[fp, to]
      if (p > 0) ll <- ll + log(p)
    }
  }
  ll
}

## ---- Run MC0/MC1/MC2 + log-likelihoods for all species ----
mc_results <- list()

for (sp in names(long_data)) {
  ld <- long_data[[sp]]
  mc0 <- get_mc0(ld)
  mc1 <- get_mc1(ld)
  mc2 <- get_mc2(ld)
  
  ll0 <- loglik_mc0(ld, mc0)
  ll1 <- loglik_mc1(ld, mc1)
  ll2 <- loglik_mc2(ld, mc2)
  
  mc_results[[sp]] <- list(mc0 = mc0, mc1 = mc1, mc2 = mc2,
                           ll0 = ll0, ll1 = ll1, ll2 = ll2)
  cat("Finished:", sp, "\n")
}

## ---- Build summary table ----
mc_summary_rows <- list()

for (sp in names(mc_results)) {
  r <- mc_results[[sp]]
  n_states <- length(r$mc0)
  
  mc_summary_rows[[paste(sp, "mc0")]] <- data.frame(
    species = sp, model = "MC0",
    n_params = n_states - 1,
    logLik = r$ll0
  )
  mc_summary_rows[[paste(sp, "mc1")]] <- data.frame(
    species = sp, model = "MC1",
    n_params = n_states * (n_states - 1),
    logLik = r$ll1
  )
  mc_summary_rows[[paste(sp, "mc2")]] <- data.frame(
    species = sp, model = "MC2",
    n_params = n_states^2 * (n_states - 1),
    logLik = r$ll2
  )
}

mc_summary <- do.call(rbind, mc_summary_rows)
mc_summary$BIC <- -2 * mc_summary$logLik + mc_summary$n_params * log(sapply(long_data[mc_summary$species], nrow))
rownames(mc_summary) <- NULL
print(mc_summary)

write.csv(mc_summary, "mc_summary_results.csv", row.names = FALSE)

