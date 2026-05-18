# Point de départ
df0 <- panel_1995 %>% filter(iso3 %in% all_countries)
cat("Départ — NA totaux:\n")
print(colSums(is.na(df0[vars_to_impute])))

# Après niveau 1
df1 <- df0 %>%
  group_by(group, year) %>%
  mutate(across(all_of(vars_to_impute),
                ~ ifelse(!is.finite(.), mean(.[is.finite(.)], na.rm = TRUE), .))) %>%
  ungroup()
cat("\nAprès niveau 1 (groupe × année) — NA restants:\n")
print(colSums(is.na(df1[vars_to_impute])))
cat("Remplis au niveau 1:\n")
print(colSums(is.na(df0[vars_to_impute])) - colSums(is.na(df1[vars_to_impute])))

# Après niveau 2
df2 <- df1 %>%
  group_by(group) %>%
  mutate(across(all_of(vars_to_impute),
                ~ ifelse(!is.finite(.), mean(.[is.finite(.)], na.rm = TRUE), .))) %>%
  ungroup()
cat("\nAprès niveau 2 (groupe) — NA restants:\n")
print(colSums(is.na(df2[vars_to_impute])))
cat("Remplis au niveau 2:\n")
print(colSums(is.na(df2[vars_to_impute])) - colSums(is.na(df2[vars_to_impute])))


# Résumé global
cat("\n=== RÉSUMÉ ===\n")
bilan <- data.frame(
  Depart    = colSums(is.na(df0[vars_to_impute])),
  Niveau1   = colSums(is.na(df0[vars_to_impute])) - colSums(is.na(df1[vars_to_impute])),
  Niveau2   = colSums(is.na(df1[vars_to_impute])) - colSums(is.na(df2[vars_to_impute])),
  Final     = colSums(is.na(df1[vars_to_impute]))
)
print(bilan)

# Une fois satisfait, panel_imputed = df4
panel_imputed <- df4