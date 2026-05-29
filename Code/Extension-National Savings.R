library(lmtest)

#table 3 : regression of variables on the national savings
get_robust_se <- function(model) {
  sqrt(diag(vcovHC(model, type = "HC1")))
}

formula_t3_ns <- COMBINEDNSGDP ~ CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 +
  RELDEPY + RELDEPO + FDEEP + CombinedTOTSD + YGRAVG + OPEN +
  ka_open + oil_exporter + factor(year)

t3_col1_ns <- lm(formula_t3_ns, data = panel_actualisation[panel_actualisation$iso3 %in% all_countries, ])
t3_col2_ns <- lm(formula_t3_ns, data = panel_actualisation[panel_actualisation$iso3 %in% excluding_africa, ])
t3_col3_ns <- lm(formula_t3_ns, data = panel_actualisation[panel_actualisation$iso3 %in% industrial_countries, ])
t3_col4_ns <- lm(formula_t3_ns, data = panel_actualisation[panel_actualisation$iso3 %in% developing_countries, ])
t3_col5_ns <- lm(formula_t3_ns, data = panel_actualisation[panel_actualisation$iso3 %in% dev_excluding_africa, ])

stargazer(t3_col1_ns, t3_col2_ns, t3_col3_ns, t3_col4_ns, t3_col5_ns,
          type = "text",
          se = list(get_robust_se(t3_col1_ns), get_robust_se(t3_col2_ns),
                    get_robust_se(t3_col3_ns), get_robust_se(t3_col4_ns),
                    get_robust_se(t3_col5_ns)),
          omit        = "factor",
          omit.labels = "Time dummies",
          omit.stat   = c("f", "ser"),
          column.labels = c("Full", "Full excl. Africa", "Industrial", "Developing", "Dev. excl. Africa"),
          title = "Table 3 — OLS with time effects",
          dep.var.labels = "National Savings to GDP ratio",
          covariate.labels = c(
            "Govt. budget balance", "NFA to GDP ratio",
            "Relative income", "Relative income squared",
            "Rel. dependency (young)", "Rel. dependency (old)",
            "Financial deepening", "ToT volatility",
            "Avg. GDP growth", "Openness ratio",
            "Capital controls", "Oil exporter dummy"
          ))

#table 4 regression on national savings
t4_col1_all_countries_act_ns <- plm(COMBINEDNSGDP ~ CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 + RELDEPY + RELDEPO + FDEEP + ka_open,
                                 data = panel_actualisation[panel_actualisation$iso3 %in% all_countries, ],
                                 index = c("iso3", "year"),
                                 model = "within", effect = "twoways")
summary(t4_col1_all_countries_act_ns)

t4_col2_excluding_africa_act_ns <- plm(COMBINEDNSGDP ~ CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 + RELDEPY + RELDEPO + FDEEP + ka_open,
                                    data = panel_actualisation[panel_actualisation$iso3 %in% excluding_africa, ],
                                    index = c("iso3", "year"),
                                    model = "within", effect = "twoways")
summary(t4_col2_excluding_africa_act_ns)

t4_col3_industrial_countries_act_ns <- plm(COMBINEDNSGDP ~ CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 + RELDEPY + RELDEPO + FDEEP + ka_open,
                                        data = panel_actualisation[panel_actualisation$iso3 %in% industrial_countries, ],
                                        index = c("iso3", "year"),
                                        model = "within", effect = "twoways")
summary(t4_col3_industrial_countries_act_ns)

t4_col4_developing_countries_act_ns <- plm(COMBINEDNSGDP ~ CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 + RELDEPY + RELDEPO + FDEEP + ka_open,
                                        data = panel_actualisation[panel_actualisation$iso3 %in% developing_countries, ],
                                        index = c("iso3", "year"),
                                        model = "within", effect = "twoways")
summary(t4_col4_developing_countries_act_ns)

t_4_col5_dev_excluding_africa_act_ns <- plm(COMBINEDNSGDP ~ CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 + RELDEPY + RELDEPO + FDEEP + ka_open,
                                         data = panel_actualisation[panel_actualisation$iso3 %in% dev_excluding_africa, ],
                                         index = c("iso3", "year"),
                                         model = "within", effect = "twoways")
summary(t_4_col5_dev_excluding_africa_act_ns)

#calcul of the rcardian's offset and test of the heterogeneity on the financial deepening



panel_actualisation <- panel_actualisation %>%
  mutate(S_private = COMBINEDNSGDP - CombinedGOVBGDP)

formula_ricardian <- S_private ~ CombinedGOVBGDP * FDEEP +
  NFAGDP + PennRELY + PennRELY2 +
  RELDEPY + RELDEPO + CombinedTOTSD + YGRAVG + OPEN +
  ka_open + oil_exporter + factor(year)

r_col1_act <- lm(formula_ricardian, data = panel_actualisation[panel_actualisation$iso3 %in% all_countries, ])
r_col2_act <- lm(formula_ricardian, data = panel_actualisation[panel_actualisation$iso3 %in% excluding_africa, ])
r_col3_act <- lm(formula_ricardian, data = panel_actualisation[panel_actualisation$iso3 %in% industrial_countries, ])
r_col4_act <- lm(formula_ricardian, data = panel_actualisation[panel_actualisation$iso3 %in% developing_countries, ])
r_col5_act <- lm(formula_ricardian, data = panel_actualisation[panel_actualisation$iso3 %in% dev_excluding_africa, ])

stargazer(r_col1_act, r_col2_act, r_col3_act, r_col4_act, r_col5_act,
          type = "text",
          se = list(get_robust_se(r_col1_act), get_robust_se(r_col2_act),
                    get_robust_se(r_col3_act), get_robust_se(r_col4_act),
                    get_robust_se(r_col5_act)),
          omit        = "factor",
          omit.labels = "Time dummies",
          omit.stat   = c("f", "ser"),
          keep        = c("CombinedGOVBGDP", "FDEEP", "CombinedGOVBGDP:FDEEP"),
          column.labels = c("Full", "Full excl. Africa", "Industrial",
                            "Developing", "Dev. excl. Africa"),
          title = "Extension — Offset ricardien hétérogène (Actualisé)",
          dep.var.labels = "Private Saving to GDP ratio",
          covariate.labels = c("Govt. budget balance (GOV)",
                               "Financial deepening (FDEEP)",
                               "GOV × FDEEP"))

fdeep_q <- quantile(
  panel_actualisation[panel_actualisation$iso3 %in% developing_countries, "FDEEP"],
  probs = c(0.25, 0.50, 0.75), na.rm = TRUE
)

b_gov      <- coef(r_col4_act)["CombinedGOVBGDP"]
b_interact <- coef(r_col4_act)["CombinedGOVBGDP:FDEEP"]

cat("\nOffset ricardien — Developing countries (Actualisé) :\n")
for (q in names(fdeep_q)) {
  offset <- b_gov + b_interact * fdeep_q[q]
  cat(sprintf("  FDEEP au %s (%.2f) : offset = %.3f\n", q, fdeep_q[q], offset))
}

