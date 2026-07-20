##############################################################
## 05_semi_markov.R
## Semi-Markov / renewal process: collapse runs, fit embedded MC1
## + zero-truncated Poisson for sojourn (run) lengths
##############################################################

source("01_data_prep.R")

## ---- Collapse each sequence into runs (embedded chain + durations) ----
semi_markov_prep <- function(long_df) {
  long_df %>%
    arrange(sequence_id, timepoint) %>%
    group_by(sequence_id) %>%
    summarise(run = list(rle(sound)), .groups = "drop") %>%
    mutate(
      embedded  = lapply(run, function(r) r$values),
      durations = lapply(run, function(r) r$lengths)
    )
}

## ---- Zero-truncated Poisson log-likelihood (for optimizing lambda) ----
ztp_loglik <- function(lambda, x) {
  sum(x * log(lambda) - lambda - log(1 - exp(-lambda)) - lgamma(x + 1))
}

## ---- Test on hyrax first ----
hyrax_sm <- semi_markov_prep(long_data$hyrax)

## embedded chain: MC1 fit (diagonal ~0 by construction, no immediate repeats)
hyrax_mc_embedded <- markovchainFit(data = hyrax_sm$embedded, method = "mle")
hyrax_mc_embedded$estimate

## sojourn/run length distribution
hyrax_durations <- unlist(hyrax_sm$durations)
hist(hyrax_durations, breaks = 20, main = "Hyrax: sojourn/run lengths")

hyrax_opt <- optimize(ztp_loglik, interval = c(0.01, 20), x = hyrax_durations, maximum = TRUE)
hyrax_lambda_hat <- hyrax_opt$maximum
hyrax_lambda_hat
## ---- Run semi-Markov analysis for all species ----
sm_results <- list()

for (sp in names(long_data)) {
  sm <- semi_markov_prep(long_data[[sp]])
  mc_emb <- markovchainFit(data = sm$embedded, method = "mle")
  durations <- unlist(sm$durations)
  opt <- optimize(ztp_loglik, interval = c(0.01, 20), x = durations, maximum = TRUE)
  
  sm_results[[sp]] <- list(
    mc_embedded = mc_emb,
    durations = durations,
    lambda_hat = opt$maximum
  )
  cat("Finished:", sp, "- lambda_hat =", round(opt$maximum, 3), "\n")
}

## ---- Summary table: lambda_hat per species ----
sm_summary <- data.frame(
  species = names(sm_results),
  lambda_hat = sapply(sm_results, function(x) x$lambda_hat),
  mean_run_length = sapply(sm_results, function(x) mean(x$durations)),
  n_runs = sapply(sm_results, function(x) length(x$durations))
)
rownames(sm_summary) <- NULL
print(sm_summary)

write.csv(sm_summary, "semi_markov_summary.csv", row.names = FALSE)

