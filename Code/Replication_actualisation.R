library(pwt10)
library(tidyverse)
library(stargazer)
library(plm)
library(xtable)
library(zoo)
library(lmtest)
library(sandwich)
library(ggplot2)
library(patchwork)

#prelimiaries
# Data
panel <- read.csv("Data/5yearPanel.csv")
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
  arrange(year) %>% 
  mutate(
    CAGDP_lag       = dplyr::lag(CAGDP, 1),
    BRREER_diff_lag = dplyr::lag(BRREER - dplyr::lag(BRREER, 1), 1)
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

#Extension on the gross flows

#table 3
get_robust_se <- function(model) {
  sqrt(diag(vcovHC(model, type = "HC1")))
}

formula_t3_gr <- GrossFlows ~ CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 +
  RELDEPY + RELDEPO + FDEEP + CombinedTOTSD + YGRAVG + OPEN +
  ka_open + oil_exporter + factor(year)

t3_col1_gr <- lm(formula_t3_gr, data = panel_actualisation[panel_actualisation$iso3 %in% all_countries, ])
t3_col2_gr <- lm(formula_t3_gr, data = panel_actualisation[panel_actualisation$iso3 %in% excluding_africa, ])
t3_col3_gr <- lm(formula_t3_gr, data = panel_actualisation[panel_actualisation$iso3 %in% industrial_countries, ])
t3_col4_gr <- lm(formula_t3_gr, data = panel_actualisation[panel_actualisation$iso3 %in% developing_countries, ])
t3_col5_gr <- lm(formula_t3_gr, data = panel_actualisation[panel_actualisation$iso3 %in% dev_excluding_africa, ])

stargazer(t3_col1_gr, t3_col2_gr, t3_col3_gr, t3_col4_gr, t3_col5_gr,
          se = list(get_robust_se(t3_col1_gr), get_robust_se(t3_col2_gr),
                    get_robust_se(t3_col3_gr), get_robust_se(t3_col4_gr),
                    get_robust_se(t3_col5_gr)),
          omit        = "factor",
          omit.labels = "Time dummies",
          omit.stat   = c("f", "ser"),
          column.labels = c("Full", "Full excl. Africa", "Industrial", "Developing", "Dev. excl. Africa"),
          title = "Table 3 — OLS with time effects",
          dep.var.labels = "Gross Flows",
          covariate.labels = c(
            "Govt. budget balance", "NFA to GDP ratio",
            "Relative income", "Relative income squared",
            "Rel. dependency (young)", "Rel. dependency (old)",
            "Financial deepening", "ToT volatility",
            "Avg. GDP growth", "Openness ratio",
            "Capital controls", "Oil exporter dummy"
          ))

#table 4 is non significative except for industrials countries. 
t4_col1_all_countries_gr <- plm(GrossFlows ~ CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 + RELDEPY + RELDEPO + FDEEP + ka_open,
                                 data = panel_actualisation[panel_actualisation$iso3 %in% all_countries, ],
                                 index = c("iso3", "year"),
                                 model = "within", effect = "twoways")
summary(t4_col1_all_countries_gr)

t4_col2_excluding_africa_gr <- plm(GrossFlows ~ CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 + RELDEPY + RELDEPO + FDEEP + ka_open,
                                    data = panel_actualisation[panel_actualisation$iso3 %in% excluding_africa, ],
                                    index = c("iso3", "year"),
                                    model = "within", effect = "twoways")
summary(t4_col2_excluding_africa_gr)

t4_col3_industrial_countries_gr <- plm(GrossFlows ~ CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 + RELDEPY + RELDEPO + FDEEP + ka_open,
                                        data = panel_actualisation[panel_actualisation$iso3 %in% industrial_countries, ],
                                        index = c("iso3", "year"),
                                        model = "within", effect = "twoways")
summary(t4_col3_industrial_countries_gr)

t4_col4_developing_countries_gr <- plm(GrossFlows ~ CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 + RELDEPY + RELDEPO + FDEEP + ka_open,
                                        data = panel_actualisation[panel_actualisation$iso3 %in% developing_countries, ],
                                        index = c("iso3", "year"),
                                        model = "within", effect = "twoways")
summary(t4_col4_developing_countries_gr)

t_4_col5_dev_excluding_africa_gr <- plm(GrossFlows ~ CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 + RELDEPY + RELDEPO + FDEEP + ka_open,
                                         data = panel_actualisation[panel_actualisation$iso3 %in% dev_excluding_africa, ],
                                         index = c("iso3", "year"),
                                         model = "within", effect = "twoways")
summary(t_4_col5_dev_excluding_africa_gr)

#table 3 to understand the decomposition of the gross flows between inflows and outflows

#table 3
get_robust_se <- function(model) {
  sqrt(diag(vcovHC(model, type = "HC1")))
}

formula_t3_in <- InflowsGDP ~ CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 +
  RELDEPY + RELDEPO + FDEEP + CombinedTOTSD + YGRAVG + OPEN +
  ka_open + oil_exporter + factor(year)

t3_col1_in <- lm(formula_t3_in, data = panel_actualisation[panel_actualisation$iso3 %in% all_countries, ])
t3_col2_in <- lm(formula_t3_in, data = panel_actualisation[panel_actualisation$iso3 %in% excluding_africa, ])
t3_col3_in <- lm(formula_t3_in, data = panel_actualisation[panel_actualisation$iso3 %in% industrial_countries, ])
t3_col4_in <- lm(formula_t3_in, data = panel_actualisation[panel_actualisation$iso3 %in% developing_countries, ])
t3_col5_in <- lm(formula_t3_in, data = panel_actualisation[panel_actualisation$iso3 %in% dev_excluding_africa, ])

stargazer(t3_col1_in, t3_col2_in, t3_col3_in, t3_col4_in, t3_col5_in,
          se = list(get_robust_se(t3_col1_in), get_robust_se(t3_col2_in),
                    get_robust_se(t3_col3_in), get_robust_se(t3_col4_in),
                    get_robust_se(t3_col5_in)),
          omit        = "factor",
          omit.labels = "Time dummies",
          omit.stat   = c("f", "ser"),
          column.labels = c("Full", "Full excl. Africa", "Industrial", "Developing", "Dev. excl. Africa"),
          title = "Table 3 — OLS with time effects",
          dep.var.labels = "Inflows",
          covariate.labels = c(
            "Govt. budget balance", "NFA to GDP ratio",
            "Relative income", "Relative income squared",
            "Rel. dependency (young)", "Rel. dependency (old)",
            "Financial deepening", "ToT volatility",
            "Avg. GDP growth", "Openness ratio",
            "Capital controls", "Oil exporter dummy"
          ))

formula_t3_out <- OutflowsGDP ~ CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 +
  RELDEPY + RELDEPO + FDEEP + CombinedTOTSD + YGRAVG + OPEN +
  ka_open + oil_exporter + factor(year)

t3_col1_out <- lm(formula_t3_out, data = panel_actualisation[panel_actualisation$iso3 %in% all_countries, ])
t3_col2_out <- lm(formula_t3_out, data = panel_actualisation[panel_actualisation$iso3 %in% excluding_africa, ])
t3_col3_out <- lm(formula_t3_out, data = panel_actualisation[panel_actualisation$iso3 %in% industrial_countries, ])
t3_col4_out <- lm(formula_t3_out, data = panel_actualisation[panel_actualisation$iso3 %in% developing_countries, ])
t3_col5_out <- lm(formula_t3_out, data = panel_actualisation[panel_actualisation$iso3 %in% dev_excluding_africa, ])

stargazer(t3_col1_out, t3_col2_out, t3_col3_out, t3_col4_out, t3_col5_out,
          se = list(get_robust_se(t3_col1_out), get_robust_se(t3_col2_out),
                    get_robust_se(t3_col3_out), get_robust_se(t3_col4_out),
                    get_robust_se(t3_col5_out)),
          omit        = "factor",
          omit.labels = "Time dummies",
          omit.stat   = c("f", "ser"),
          column.labels = c("Full", "Full excl. Africa", "Industrial", "Developing", "Dev. excl. Africa"),
          title = "Table 3 — OLS with time effects",
          dep.var.labels = "Outflows",
          covariate.labels = c(
            "Govt. budget balance", "NFA to GDP ratio",
            "Relative income", "Relative income squared",
            "Rel. dependency (young)", "Rel. dependency (old)",
            "Financial deepening", "ToT volatility",
            "Avg. GDP growth", "Openness ratio",
            "Capital controls", "Oil exporter dummy"
          ))

#EXTENSION CRISIS INFLUENCE

panel_annual_crisis <- panel_annual %>%
  filter(year >= 1971) %>%
  mutate(
    oil_exporter = ifelse(iso3 %in% oil_exporting_countries, 1, 0),
    PennRELY2    = PennRELY^2
  ) %>%
  group_by(iso3) %>%
  arrange(year) %>% 
  mutate(
    CAGDP_lag       = dplyr::lag(CAGDP, 1),
    BRREER_diff_lag = dplyr::lag(BRREER - dplyr::lag(BRREER, 1), 1)
  ) %>%
  ungroup() %>%
  mutate(
    crisis_asian = ifelse(iso3 %in% c("THA","KOR","IDN","MYS","PHL") &
                            year %in% c(1997, 1998), 1, 0),
    crisis_arg   = ifelse(iso3 == "ARG" & year %in% c(2001, 2002), 1, 0),
    crisis_gfc   = ifelse(year %in% c(2008, 2009), 1, 0),
    crisis_any   = ifelse(crisis_asian == 1 | crisis_arg == 1 |
                            crisis_gfc == 1, 1, 0),
    post_asian = ifelse(iso3 %in% c("THA","KOR","IDN","MYS","PHL") &
                          year %in% c(1999, 2000), 1, 0),
    post_gfc_dummy = ifelse(year >= 2008, 1, 0)
  )

#regressions: 
m_crisis_all <- lm(
  CAGDP ~
    IMFGOVBGDP * crisis_any + NFAGDP * crisis_any + PennRELY + PennRELY2 +
    RELDEPY + RELDEPO + FDEEP + IndirectTOTS + gdpgr + OPEN + ka_open +
    oil_exporter + CAGDP_lag + BRREER_diff_lag +
    factor(year),
  data = panel_annual_crisis[
    panel_annual_crisis$iso3 %in% all_countries, ]
)
m_crisis_all_excl_af <-lm(
  CAGDP ~
    IMFGOVBGDP * crisis_any + NFAGDP * crisis_any + PennRELY + PennRELY2 +
    RELDEPY + RELDEPO + FDEEP + IndirectTOTS + gdpgr + OPEN + ka_open +
    oil_exporter + CAGDP_lag + BRREER_diff_lag +
    factor(year),
  data = panel_annual_crisis[
    panel_annual_crisis$iso3 %in% excluding_africa, ]
)
m_crisis_industrial<- lm(
  CAGDP ~
    IMFGOVBGDP * crisis_any + NFAGDP * crisis_any + PennRELY + PennRELY2 +
    RELDEPY + RELDEPO + FDEEP + IndirectTOTS + gdpgr + OPEN + ka_open +
    oil_exporter + CAGDP_lag + BRREER_diff_lag +
    factor(year),
  data = panel_annual_crisis[
    panel_annual_crisis$iso3 %in% industrial_countries, ]
)
m_crisis_dev <- lm(
  CAGDP ~
    IMFGOVBGDP * crisis_any + NFAGDP * crisis_any + PennRELY + PennRELY2 +
    RELDEPY + RELDEPO + FDEEP + IndirectTOTS + gdpgr + OPEN + ka_open +
    oil_exporter + CAGDP_lag + BRREER_diff_lag +
    factor(year),
  data = panel_annual_crisis[
    panel_annual_crisis$iso3 %in% developing_countries, ]
)
m_crisis_dev_excl <- lm(
  CAGDP ~
    IMFGOVBGDP * crisis_any + NFAGDP * crisis_any + PennRELY + PennRELY2 +
    RELDEPY + RELDEPO + FDEEP + IndirectTOTS + gdpgr + OPEN + ka_open + CAGDP_lag +BRREER_diff_lag, 
  data = panel_annual_crisis[
    panel_annual_crisis$iso3 %in% dev_excluding_africa, ]
)

stargazer(
  m_crisis_all, 
  m_crisis_all_excl_af, 
  type = "text")

stargazer(
  m_crisis_dev,
  m_crisis_industrial, 
  type = "text"
)



#event study to grephically understand the moves in CA around crisis. 

# ── EVENT STUDY ──────────────────────────────────────────────

#1. asian crisis -1997
es_asian <- panel_annual_crisis %>%
  filter(iso3 %in% c("THA","KOR","IDN","MYS","PHL")) %>%
  mutate(event_time = year - 1997) %>%
  filter(event_time >= -5 & event_time <= 5) %>%
  group_by(event_time) %>%
  summarise(mean_CA = mean(CAGDP, na.rm = TRUE))

# 2. argentine crisis
es_arg <- panel_annual_crisis %>%
  filter(iso3 == "ARG") %>%
  mutate(event_time = year - 2001) %>%
  filter(event_time >= -5 & event_time <= 5) %>%
  group_by(event_time) %>%
  summarise(mean_CA = mean(CAGDP, na.rm = TRUE))

p_asian <- ggplot(es_asian, aes(x = event_time, y = mean_CA)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  labs(title = "The Asian crisis (1997)",
       x = "years around the crisis",
       y = "average CA/GDP") +
  theme_bw(base_size = 10)

p_arg <- ggplot(es_arg, aes(x = event_time, y = mean_CA)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  labs(title = "The Argentine crisis (2001)",
       x = "years around the crisis",
       y = "average CA/GDP") +
  theme_bw(base_size = 10)

# To print both table
p_asian + p_arg +
  plot_annotation(title = "Event study, adjustement of CA for asian and argentine's crises")

# Graphics to understand the necessity to add a crisis dummy 
# GFC — all countries
es_gfc_all <- panel_annual_crisis %>%
  filter(iso3 %in% all_countries) %>%
  mutate(event_time = year - 2008) %>%
  filter(event_time >= -5 & event_time <= 5) %>%
  group_by(event_time) %>%
  summarise(mean_CA = mean(CAGDP, na.rm = TRUE))

# GFC — industrial countries
es_gfc_ind <- panel_annual_crisis %>%
  filter(iso3 %in% industrial_countries) %>%
  mutate(event_time = year - 2008) %>%
  filter(event_time >= -5 & event_time <= 5) %>%
  group_by(event_time) %>%
  summarise(mean_CA = mean(CAGDP, na.rm = TRUE))

# GFC — developing countries  
es_gfc_dev <- panel_annual_crisis %>%
  filter(iso3 %in% developing_countries) %>%
  mutate(event_time = year - 2008) %>%
  filter(event_time >= -5 & event_time <= 5) %>%
  group_by(event_time) %>%
  summarise(mean_CA = mean(CAGDP, na.rm = TRUE))

es_gfc_all$group <- "All countries"
es_gfc_ind$group <- "Industrial"
es_gfc_dev$group <- "Developing"

es_gfc_combined <- bind_rows(es_gfc_all, es_gfc_ind, es_gfc_dev)

ggplot(es_gfc_combined, aes(x = event_time, y = mean_CA,
                            color = group, shape = group)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  labs(title = "GFC (2008) — adjustment of the CA / GDP by group of countries",
       x = "years around the crisis",
       y = "average CA/GDP",
       color = NULL, shape = NULL) +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom")


####Logit on crisis !!

library(fixest)



# Logit poolé avec FE temporels
panel_annual_crisis = panel_annual_crisis %>% 
mutate(abouttoasiancrisis = ifelse(iso3 %in% c("THA","KOR","IDN","MYS","PHL") &
                        year %in% c(1994, 1995, 1996), 1, 0), )

AsianModel <- feglm(
  abouttoasiancrisis ~ InflowsGDP + OutflowsGDP + NFAGDP + 
    PennRELY + gov_balance + NSGDP + FDEEP | year,  # FE temporels après le |
  data = panel_annual_crisis,
  family = binomial(link = "logit")
)

etable(AsianModel, tex = TRUE)

ArgModel <- feglm(
  crisis_arg ~ GrossFlows + CAGDP + NFAGDP + 
    PennRELY + gov_balance + WEONSGDP | year,  # FE temporels après le |
  data = panel_annual_crisis,
  family = binomial(link = "logit")
)

etable(ArgModel, tex = FALSE)


