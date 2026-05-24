library(pwt10)
library(tidyverse)
library(stargazer)
library(plm)
library(xtable)
library(zoo)
library(lmtest)
library(sandwich)

#prelimiaries
# Data
panel <- read.csv("Data/panel_3.csv")
panel_actualisation <- panel %>% 
  mutate(PennRELY2=PennRELY^2)

# Country groups
industrial_countries <- c(
  "AUS","AUT","CAN","DNK","FIN","FRA","GRC","ISL",
  "IRL","ITA","JPN","NLD","NZL","NOR","PRT","ESP",
  "CHE","USA"
)

developing_countries <- c(
  "DZA","ARG","BHR","BGD","BEN","BOL","BWA","BRA","BFA",
  "BDI","CMR","TCD","CHL","COL","COG","CRI","CIV","DMA",
  "ECU","EGY","SLV","GAB","GMB","GHA","GTM","HTI","HND",
  "IND","IDN","IRN","ISR","JAM","JOR","KEN","KOR","MDG",
  "MWI","MYS","MLI","MRT","MUS","MEX","MAR","NPL","NER",
  "NGA","PAK","PAN","PNG","PRY","PER","PHL","RWA","SEN",
  "SYC","SLE","SGP","ZAF","LKA","SWZ","SYR","THA","TGO",
  "TTO","TUN","TUR","UGA","URY","VEN","ZMB","ZWE"
)

africa <- c(
  "DZA","BEN","BFA","BDI","CMR","TCD","COG","CIV",
  "EGY","GAB","GMB","GHA","KEN","MDG","MWI","MLI",
  "MRT","MAR","NER","NGA","RWA","SEN","SLE","ZAF",
  "SWZ","TGO","TUN","UGA","ZMB","ZWE"
)

oil_exporting_countries <- c(
  "DZA", "COG", "GNQ", "IRN", "IRQ", "KWT",
  "LBY", "NGA", "SAU", "VEN"
)

panel_actualisation <- panel_actualisation %>%
  mutate(oil_exporter=ifelse(iso3 %in% oil_exporting_countries, 1, 0))
panel_p_act <- pdata.frame(panel_actualisation, index = c("iso3", "year"))
View(panel_p_act)

all_countries <- c(industrial_countries, developing_countries)
excluding_africa <- setdiff(all_countries, africa)
dev_excluding_africa <- setdiff(developing_countries, africa)

vars <- c(
  "CAGDP","GOVBGDP","RELY", "RELY2", "RELDEPY","RELDEPO",
  "YGRAVG","YGRSD","TOTSD","LREER",
  "OPEN","FDEEP","NSGDP","ka_open","NFAGDP"
)
vars2 <- c(
  "CAGDP","CombinedGOVBGDP","PennRELY", "PennRELY2", "RELDEPY","RELDEPO",
  "YGRAVG","YGRSD","CombinedTOTSD","BRREER",
  "OPEN","FDEEP","COMBINEDNSGDP","ka_open","NFAGDP", "oil_exporter"
)

# Variance decomposition function
variance_decomp <- function(data, varname) {
  x <- data[[varname]]
  overall <- var(x, na.rm = TRUE)
  country_mean <- ave(x, data$iso3,
                      FUN = function(z) mean(z, na.rm = TRUE))
  within <- var(x - country_mean, na.rm = TRUE)
  between <- overall - within
  tibble(
    Variable = varname,
    Between_pct = 100 * between / overall,
    Within_pct  = 100 * within / overall
  )
}

# Apply to group
run_group <- function(df, name) {
  map_dfr(vars2, ~variance_decomp(df, .x)) %>%
    mutate(Group = name)
}

# Compute results
industrial_table_act <- run_group(
  filter(panel_p_act, iso3 %in% industrial_countries),
  "Industrial"
)

developing_table_act <- run_group(
  filter(panel_p_act, iso3 %in% developing_countries),
  "Developing"
)

# Combine results
all_results_act <- bind_rows(industrial_table_act, developing_table_act)

# Reshape to one comparative table
final_table_act <- all_results_act %>%
  select(Variable, Group, Between_pct, Within_pct) %>%
  pivot_wider(
    names_from = Group,
    values_from = c(Between_pct, Within_pct)
  ) %>%
  select(
    Variable,
    Between_pct_Industrial,
    Within_pct_Industrial,
    Between_pct_Developing,
    Within_pct_Developing
  )
print(final_table_act)

#table 2 actualisation
all_countries_reg2_act <- lm(CAGDP ~ -1 + CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 + RELDEPY + RELDEPO + YGRAVG + YGRSD + CombinedTOTSD + BRREER + OPEN + FDEEP + ka_open + oil_exporter, data = panel_actualisation[panel_actualisation$iso3 %in% all_countries, ])
summary(all_countries_reg2_act)

developing_reg2_act <- lm(CAGDP ~ -1 + CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 + RELDEPY + RELDEPO + 
                        YGRAVG + YGRSD + TOTSD + BRREER + OPEN + FDEEP + 
                        ka_open + oil_exporter, 
                      data = panel_actualisation[panel_actualisation$iso3 %in% developing_countries, ])
summary(developing_reg2_act)

excluding_africa_reg2_act <- lm(CAGDP ~ -1 + CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 + RELDEPY + RELDEPO + 
                              YGRAVG + YGRSD + CombinedTOTSD + BRREER + OPEN + FDEEP + 
                              ka_open + oil_exporter, 
                            data = panel_actualisation[panel_actualisation$iso3 %in% excluding_africa, ])
summary(excluding_africa_reg2_act)

#industrial countries
industrial_act <- lm(CAGDP ~ -1 + CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 + RELDEPY + RELDEPO + 
                   YGRAVG + YGRSD + CombinedTOTSD + BRREER + OPEN + FDEEP + 
                   ka_open + oil_exporter, 
                 data = panel_actualisation[panel_actualisation$iso3 %in% industrial_countries, ])
summary(industrial_act)

#developping countries without africa
dev_excluding_af_act <- lm(CAGDP ~ -1 + CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 + RELDEPY + RELDEPO + 
                       YGRAVG + YGRSD + CombinedTOTSD + BRREER + OPEN + FDEEP + 
                       ka_open + oil_exporter, 
                     data = panel_actualisation[panel_actualisation$iso3 %in% dev_excluding_africa, ])
summary(dev_excluding_af_act)


#table 3
get_robust_se <- function(model) {
  sqrt(diag(vcovHC(model, type = "HC1")))
}

formula_t3_act <- CAGDP ~ CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 +
  RELDEPY + RELDEPO + FDEEP + CombinedTOTSD + YGRAVG + OPEN +
  ka_open + oil_exporter + factor(year)

t3_col1_act <- lm(formula_t3_act, data = panel_actualisation[panel_actualisation$iso3 %in% all_countries, ])
t3_col2_act <- lm(formula_t3_act, data = panel_actualisation[panel_actualisation$iso3 %in% excluding_africa, ])
t3_col3_act <- lm(formula_t3_act, data = panel_actualisation[panel_actualisation$iso3 %in% industrial_countries, ])
t3_col4_act <- lm(formula_t3_act, data = panel_actualisation[panel_actualisation$iso3 %in% developing_countries, ])
t3_col5_act <- lm(formula_t3_act, data = panel_actualisation[panel_actualisation$iso3 %in% dev_excluding_africa, ])

stargazer(t3_col1_act, t3_col2_act, t3_col3_act, t3_col4_act, t3_col5_act,
          type = "text",
          se = list(get_robust_se(t3_col1_act), get_robust_se(t3_col2_act),
                    get_robust_se(t3_col3_act), get_robust_se(t3_col4_act),
                    get_robust_se(t3_col5_act)),
          omit        = "factor",
          omit.labels = "Time dummies",
          omit.stat   = c("f", "ser"),
          column.labels = c("Full", "Full excl. Africa", "Industrial", "Developing", "Dev. excl. Africa"),
          title = "Table 3 — OLS with time effects",
          dep.var.labels = "Current Account to GDP ratio",
          covariate.labels = c(
            "Govt. budget balance", "NFA to GDP ratio",
            "Relative income", "Relative income squared",
            "Rel. dependency (young)", "Rel. dependency (old)",
            "Financial deepening", "ToT volatility",
            "Avg. GDP growth", "Openness ratio",
            "Capital controls", "Oil exporter dummy"
          ))

#table 4
t4_col1_all_countries_act <- plm(CAGDP ~ CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 + RELDEPY + RELDEPO + FDEEP + ka_open,
                             data = panel_actualisation[panel_actualisation$iso3 %in% all_countries, ],
                             index = c("iso3", "year"),
                             model = "within", effect = "twoways")
summary(t4_col1_all_countries_act)

t4_col2_excluding_africa_act <- plm(CAGDP ~ CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 + RELDEPY + RELDEPO + FDEEP + ka_open,
                                data = panel_actualisation[panel_actualisation$iso3 %in% excluding_africa, ],
                                index = c("iso3", "year"),
                                model = "within", effect = "twoways")
summary(t4_col2_excluding_africa_act)

t4_col3_industrial_countries_act <- plm(CAGDP ~ CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 + RELDEPY + RELDEPO + FDEEP + ka_open,
                                    data = panel_actualisation[panel_actualisation$iso3 %in% industrial_countries, ],
                                    index = c("iso3", "year"),
                                    model = "within", effect = "twoways")
summary(t4_col3_industrial_countries_act)

t4_col4_developing_countries_act <- plm(CAGDP ~ CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 + RELDEPY + RELDEPO + FDEEP + ka_open,
                                    data = panel_actualisation[panel_actualisation$iso3 %in% developing_countries, ],
                                    index = c("iso3", "year"),
                                    model = "within", effect = "twoways")
summary(t4_col4_developing_countries_act)

t_4_col5_dev_excluding_africa_act <- plm(CAGDP ~ CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 + RELDEPY + RELDEPO + FDEEP + ka_open,
                                     data = panel_actualisation[panel_actualisation$iso3 %in% dev_excluding_africa, ],
                                     index = c("iso3", "year"),
                                     model = "within", effect = "twoways")
summary(t_4_col5_dev_excluding_africa_act)


#Table 5
panel_annual <- read.csv("Data/AnnualPanel.csv")   # adjust path as needed

panel_annual_act <- panel_annual %>%
  filter(year >= 1971) %>%
  mutate(
    oil_exporter     = ifelse(iso3 %in% oil_exporting_countries, 1, 0),
    PennRELY2        = PennRELY^2) %>% 
  group_by(iso3) %>%
  mutate(
    CAGDP_lag       = lag(CAGDP, 1),
    BRREER_diff_lag = lag(BRREER - lag(BRREER, 1), 1)
  ) %>%
  ungroup()

# Formula for Table 5
formula_t5_act <- CAGDP ~ IMFGOVBGDP + NFAGDP + PennRELY + PennRELY2 +
  RELDEPY + RELDEPO + FDEEP + IndirectTOTS + gdpgr + OPEN +
  ka_open + oil_exporter +
  CAGDP_lag + BRREER_diff_lag +
  factor(year)

# Col 1: Full sample
t5_col1_act <- lm(formula_t5_act,
              data = panel_annual_act[panel_annual_act$iso3 %in% all_countries, ])
summary(t5_col1_act)
# Col 2: Full sample excl. Africa
t5_col2_act <- lm(formula_t5_act,
              data = panel_annual_act[panel_annual_act$iso3 %in% excluding_africa, ])
summary(t5_col2_act)
# Col 3: Industrial
t5_col3_act <- lm(formula_t5_act,
              data = panel_annual_act[panel_annual_act$iso3 %in% industrial_countries, ])
summary(t5_col3_act)
# Col 4: Developing
t5_col4_act <- lm(formula_t5_act,
              data = panel_annual_act[panel_annual_act$iso3 %in% developing_countries, ])
summary(t5_col4_act)
# Col 5: Developing excl. Africa
t5_col5_act <- lm(formula_t5_act,
              data = panel_annual_act[panel_annual_act$iso3 %in% dev_excluding_africa, ])
summary(t5_col5_act)

# Output with robust SE
stargazer(t5_col1_act, t5_col2_act, t5_col3_act, t5_col4_act, t5_col5_act,
          type = "text",
          se = list(get_robust_se(t5_col1_act), get_robust_se(t5_col2_act),
                    get_robust_se(t5_col3_act), get_robust_se(t5_col4_act),
                    get_robust_se(t5_col5_act)),
          omit        = "factor",
          omit.labels = "Year dummies",
          omit.stat   = c("f", "ser"),
          column.labels = c("Full", "Full excl. Africa", "Industrial",
                            "Developing", "Dev. excl. Africa"),
          title = "Table 5 — OLS annual data between 1971 and today with time effects",
          dep.var.labels = "Current Account to GDP ratio",
          covariate.labels = c(
            "Govt. budget balance", "NFA to GDP ratio",
            "Relative income", "Relative income squared",
            "Rel. dependency (young)", "Rel. dependency (old)",
            "Financial deepening", "ToT volatility",
            "Avg. GDP growth", "Openness ratio",
            "Capital controls", "Oil exporter dummy",
            "Lagged CA/GDP", "Lagged Δ log REER"
          ))



