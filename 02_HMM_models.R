##############################################################
## 02_HMM_models.R
## Fits HMMs (2,3,4 states) for all species
##############################################################

source("01_data_prep.R")

## ---- HMM fitting function ----
fit_hmm <- function(seq_obj, n_states, n_restarts = 10) {
  init_mod <- build_hmm(observations = seq_obj, n_states = n_states)
  fit_model(init_mod, control_em = list(restart = list(times = n_restarts)))
}

## ---- Fit HMMs for all species ----
hmm_results <- list()

for (sp in names(seq_data)) {
  hmm_results[[sp]] <- list(
    hmm2 = fit_hmm(seq_data[[sp]], 2),
    hmm3 = fit_hmm(seq_data[[sp]], 3),
    hmm4 = fit_hmm(seq_data[[sp]], 4)
  )
  cat("Finished:", sp, "\n")
}

## ---- Build summary table ----
summary_rows <- list()

for (sp in names(hmm_results)) {
  for (state_label in c("hmm2", "hmm3", "hmm4")) {
    mod <- hmm_results[[sp]][[state_label]]$model
    summary_rows[[paste(sp, state_label)]] <- data.frame(
      species  = sp,
      n_states = as.integer(gsub("hmm", "", state_label)),
      logLik   = as.numeric(logLik(mod)),
      BIC      = BIC(mod)
    )
  }
}

hmm_summary <- do.call(rbind, summary_rows)
rownames(hmm_summary) <- NULL
print(hmm_summary)

write.csv(hmm_summary, "hmm_summary_results.csv", row.names = FALSE)
