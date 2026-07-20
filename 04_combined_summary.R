##############################################################
## 04_combined_summary.R
## Combines HMM and Markov Chain results into one table
##############################################################

hmm_summary <- read.csv("hmm_summary_results.csv")
mc_summary  <- read.csv("mc_summary_results.csv")

hmm_summary$model <- paste0("HMM", hmm_summary$n_states)
hmm_summary_clean <- hmm_summary[, c("species", "model", "logLik", "BIC")]

mc_summary_clean <- mc_summary[, c("species", "model", "logLik", "BIC")]

combined <- rbind(hmm_summary_clean, mc_summary_clean)
combined <- combined[order(combined$species, combined$BIC), ]
rownames(combined) <- NULL

print(combined)

## ---- Best model per species (lowest BIC) ----
best_per_species <- do.call(rbind, lapply(split(combined, combined$species), function(d) d[1, ]))
rownames(best_per_species) <- NULL
cat("\n--- Best model per species (lowest BIC) ---\n")
print(best_per_species)

write.csv(combined, "combined_model_comparison.csv", row.names = FALSE)


