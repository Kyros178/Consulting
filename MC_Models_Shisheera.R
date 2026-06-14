library(dplyr)
library(tidyr)
library(markovchain)
for (sp in names(species_data)) {
  d <- species_data[[sp]]
  mc0 <- setNames(as.numeric(prop.table(
    table(d$long$sound))),
    as.character(names(prop.table(
      table(d$long$sound)))))
  ll0 <- get_loglik_mc0(d$seqs, mc0)
  init_dist <- get_init_dist(d$long)
  ll1 <- get_loglik_mc1(d$seqs, d$mc1, init_dist)
  ll2 <- get_loglik_mc2(d$long)
  cat("\n===", sp, "===\n")
  cat("LogLik MC0:", round(ll0,3), "\n")
  cat("LogLik MC1:", round(ll1,3), "\n")
  cat("LogLik MC2:", round(ll2,3), "\n")
  results_all <- rbind(results_all, data.frame(
    Species  = sp,
    N_states = length(unique(d$long$sound)),
    N_seqs   = length(d$seqs),
    LogLik_0 = round(ll0,3),
    LogLik_1 = round(ll1,3),
    LogLik_2 = round(ll2,3)
  ))
}

cat("\n\n=== SUMMARY — ALL MODELS ===\n")
print(results_all)
