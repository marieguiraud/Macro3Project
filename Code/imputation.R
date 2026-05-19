#PART ON IMPUTATION
#imputation of NA on mean value of the group. 


panel_1995 <- panel_1995 %>%
  mutate(group = case_when(
    iso3 %in% industrial_countries ~ "industrial",
    iso3 %in% africa               ~ "africa",
    iso3 %in% developing_countries ~ "developing_excl_africa",
    TRUE ~ NA_character_
  ))
colSums(is.na(panel_1995))
vars_to_impute <- c("CAGDP","GOVBGDP","CombinedGOVBGDP","NFAGDP","RELY", "RELY2", "RELDEPY","RELDEPO",
                    "YGRAVG","YGRSD","TOTSD","CombinedTOTSD","IndirectTOTS",
                    "OPEN","FDEEP","ka_open","LREER","BRREER", "NSGDP")


# Fonction d'imputation en cascade
impute_cascade <- function(x) {
  if (all(is.na(x))) return(x)
  # 1. Moyenne simple
  if (!is.na(mean(x, na.rm = TRUE))) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
  }
  x
}

panel_imputed <- panel_1995 %>%
  filter(iso3 %in% all_countries) %>%
  
  # Niveau 1 : moyenne groupe × année
  group_by(group, year) %>%
  mutate(across(all_of(vars_to_impute),
                ~ ifelse(!is.finite(.), mean(.[is.finite(.)], na.rm = TRUE), .))) %>%
  ungroup() %>%
  
  # Niveau 2 : moyenne groupe (toutes années)
  group_by(group) %>%
  mutate(across(all_of(vars_to_impute),
                ~ ifelse(!is.finite(.), mean(.[is.finite(.)], na.rm = TRUE), .))) %>%
  ungroup()


#Table 2 with imputation
all_countries_reg2_imp <- lm( CAGDP ~ -1 + CombinedGOVBGDP + NFAGDP + RELY + RELY2 +  RELDEPY + RELDEPO + YGRAVG + YGRSD + CombinedTOTSD + BRREER + OPEN + FDEEP + ka_open + NSGDP, data = panel_imputed[panel_imputed$iso3 %in% all_countries, ])
summary(all_countries_reg2_imp)


developing_reg2_imp <- lm(CAGDP ~ -1 + CombinedGOVBGDP + NFAGDP + RELY + RELY2 + RELDEPY + RELDEPO + 
                            YGRAVG + YGRSD + TOTSD + BRREER + OPEN + FDEEP + 
                            ka_open + NSGDP, 
                          data = panel_imputed[panel_imputed$iso3 %in% developing_countries, ])
summary(developing_reg2_imp)


excluding_africa_reg2_imp <- lm(CAGDP ~ -1 + CombinedGOVBGDP + NFAGDP + RELY + RELY2 + RELDEPY + RELDEPO + 
                                  YGRAVG + YGRSD + CombinedTOTSD + BRREER + OPEN + FDEEP + 
                                  ka_open + NSGDP, 
                                data = panel_imputed[panel_imputed$iso3 %in% excluding_africa, ])
summary(excluding_africa_reg2_imp)

industrial_imp <- lm(CAGDP ~ -1 + CombinedGOVBGDP + NFAGDP + RELY + RELY2 + RELDEPY + RELDEPO + 
                       YGRAVG + YGRSD + CombinedTOTSD + BRREER + OPEN + FDEEP + 
                       ka_open + NSGDP, 
                     data = panel_imputed[panel_imputed$iso3 %in% industrial_countries, ])

summary(industrial_imp)

#reproduction table 4 under imputation : 
# Table 4 - col 1 : Full sample 

t4_col1_all_countries_imp  <- plm(CAGDP ~ CombinedGOVBGDP + NFAGDP + RELY + RELY2 + RELDEPY + RELDEPO + FDEEP + ka_open,
                                  data = panel_imputed[panel_imputed$iso3 %in% all_countries, ],
                                  index = c("iso3", "year"),
                                  model = "within", effect = "twoways")

summary(t4_col1_all_countries_imp) 

# Table 4 - col 2 : Full sample excl. Africa
t4_col2_excluding_africa_imp <- plm(CAGDP ~ CombinedGOVBGDP + NFAGDP + RELY + RELY2 + RELDEPY + RELDEPO + FDEEP + ka_open,
                                    data = panel_imputed[panel_imputed$iso3 %in% excluding_africa, ],
                                    index = c("iso3", "year"),
                                    model = "within", effect = "twoways")
summary(t4_col2_excluding_africa_imp)

# Table 4 - col 3 : Industrial
t4_col3_industrial_countries_imp <- plm(CAGDP ~ CombinedGOVBGDP + NFAGDP + RELY + RELY2 + RELDEPY + RELDEPO + FDEEP + ka_open,
                                        data = panel_imputed[panel_imputed$iso3 %in% industrial_countries, ],
                                        index = c("iso3", "year"),
                                        model = "within", effect = "twoways")
summary(t4_col3_industrial_countries_imp) #toujours un problème sur les industrials ont a que 9 observations

# Table 4 - col 4 : Developing
t4_col4_developing_countries_imp <- plm(CAGDP ~ CombinedGOVBGDP + NFAGDP + RELY + RELY2 + RELDEPY + RELDEPO + FDEEP + ka_open,
                                        data = panel_imputed[panel_imputed$iso3 %in% developing_countries, ],
                                        index = c("iso3", "year"),
                                        model = "within", effect = "twoways")
summary(t4_col4_developing_countries_imp)

# Table 4 - col 5 : Developing excl. Africa
t_4_col5_dev_excluding_africa_imp <- plm(CAGDP ~ CombinedGOVBGDP + NFAGDP + RELY + RELY2 + RELDEPY + RELDEPO + FDEEP + ka_open,
                                         data = panel_imputed[panel_imputed$iso3 %in% dev_excluding_africa, ],
                                         index = c("iso3", "year"),
                                         model = "within", effect = "twoways")
summary(t_4_col5_dev_excluding_africa_imp)

stargazer(t4_col1_all_countries, t4_col2_excluding_africa, t4_col3_industrial_countries, t4_col4_developing_countries, t_4_col5_dev_excluding_africa, type = "text",
          column.labels = c("Full","Full excl. Africa","Industrial","Developing","Dev. excl. Africa"),
          title = "Table 4 — Fixed Effects with time effects")

