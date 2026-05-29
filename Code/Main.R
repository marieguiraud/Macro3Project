library(pwt10)
library(knitr)
library(tidyverse)
library(stargazer)
library(plm)
library(xtable)
library(zoo)
library(lmtest)
library(sandwich)

#preliminaries 
# Data
panel <- read.csv("Data/5yearPanel.csv")
panel_1995 <- panel%>%
  filter(year <= 1995) %>% #all data between 1971 and 1995
  mutate(PennRELY2=PennRELY^2, RELY2 = RELY^2) #PennRELY is an improved version of RELY (cf data description) 
panel_1995$CombinedGOVBGDP[panel_1995$iso3 == "GNQ"] <- NA #removing GNQ from CombinedGOVBDGP since it's an outlier (cf description)

#Robust standard error definition 
get_robust_se <- function(model) {
  sqrt(diag(vcovHC(model, type = "HC1")))
}

# Defining country groups
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

all_countries <- c(industrial_countries, developing_countries)
excluding_africa <- setdiff(all_countries, africa)
dev_excluding_africa <- setdiff(developing_countries, africa)

#adding the oil exporting dummy in the data
panel_1995 <- panel_1995 %>%
  mutate(oil_exporter=ifelse(iso3 %in% oil_exporting_countries, 1, 0))
panel_p <- pdata.frame(panel_1995, index = c("iso3", "year"))

#initial variables 
vars <- c(
  "CAGDP","GOVBGDP","RELY", "RELY2", "RELDEPY","RELDEPO",
  "YGRAVG","YGRSD","TOTSD","LREER",
  "OPEN","FDEEP","NSGDP","ka_open","NFAGDP"
)
#improved variables (the one that we use in the regression since they allow to have more observations)
vars2 <- c(
  "CAGDP","CombinedGOVBGDP","PennRELY", "PennRELY2", "RELDEPY","RELDEPO",
  "YGRAVG","YGRSD","CombinedTOTSD","BRREER",
  "OPEN","FDEEP","COMBINEDNSGDP","ka_open","NFAGDP", "oil_exporter"
)

#replication of table 1 from 1971 to 1995
# Clean variance decomposition function
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
industrial_table <- run_group(
  filter(panel_p, iso3 %in% industrial_countries),
  "Industrial"
)

developing_table <- run_group(
  filter(panel_p, iso3 %in% developing_countries),
  "Developing"
)

# Combine results
all_results <- bind_rows(industrial_table, developing_table)

# Reshape to one comparative table
final_table <- all_results %>%
  select(Variable, Group, Between_pct, Within_pct) %>%
  pivot_wider(
    names_from = Group,
    values_from = c(Between_pct, Within_pct)
  ) %>%
  
  # clean ordering
  select(
    Variable,
    Between_pct_Industrial,
    Within_pct_Industrial,
    Between_pct_Developing,
    Within_pct_Developing
  )

print(final_table)

#create table 1 on latex 
table1_latex <- final_table %>%
  select(
    Variable,
    Between_pct_Industrial,
    Within_pct_Industrial,
    Between_pct_Developing,
    Within_pct_Developing
  )

colnames(table1_latex) <- c(
  "Variable",
  "Across (Industrial)",
  "Over time (Industrial)",
  "Across (Developing)",
  "Over time (Developing)"
)

tab1 <- xtable(
  table1_latex,
  digits = 2,
  caption = "Decomposition of variance into cross-section and time-series components (in percent)",
  label = "tab:table1"
)

print(
  tab1,
  type = "latex",
  include.rownames = FALSE,
  file = "Tables/table1.tex",
  booktabs = TRUE
)

#Table 2 (cross country regression)
#regression for all countries with the initial data
all_countries_reg1 <- lm( CAGDP ~ -1 + GOVBGDP + NFAGDP + RELY + RELY2 + RELDEPY + RELDEPO + YGRAVG + YGRSD + TOTSD + LREER + OPEN + FDEEP + ka_open + NSGDP, data = panel_p)
summary(all_countries_reg1)

#all countries with new variables 
all_countries_reg2 <- lm( CAGDP ~ -1 + CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 +  RELDEPY + RELDEPO + YGRAVG + YGRSD + CombinedTOTSD + BRREER + OPEN + FDEEP + ka_open, data = panel_1995[panel_1995$iso3 %in% all_countries, ])
summary(all_countries_reg2)

#NB: for industrial we didn't manage to get results with the first variables (too few observations) so there is only one regression with improved data 
industrial <- lm(CAGDP ~ -1 + CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 + RELDEPY + RELDEPO + 
                   YGRAVG + YGRSD + CombinedTOTSD + BRREER + OPEN + FDEEP + 
                   ka_open + oil_exporter, 
                 data = panel_1995[panel_1995$iso3 %in% industrial_countries, ])
summary(industrial)

#dev countries with initial data 
developing_reg1 <- lm(CAGDP ~ -1 + GOVBGDP + NFAGDP + RELY + RELY2 + RELDEPY + RELDEPO + 
                        YGRAVG + YGRSD + TOTSD + LREER + OPEN + FDEEP + 
                        ka_open + NSGDP, 
                      data = panel_1995[panel_1995$iso3 %in% developing_countries, ])
summary(developing_reg1)

#dev countries with the new variables 
developing_reg2 <- lm(CAGDP ~ -1 + CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 + RELDEPY + RELDEPO + 
                        YGRAVG + YGRSD + CombinedTOTSD + BRREER + OPEN + FDEEP + 
                        ka_open + oil_exporter, 
                      data = panel_1995[panel_1995$iso3 %in% developing_countries, ])
summary(developing_reg2)


excluding_africa_reg1 <- lm(CAGDP ~ -1 + GOVBGDP + NFAGDP + RELY + RELY2 + RELDEPY + RELDEPO + 
                              YGRAVG + YGRSD + TOTSD + LREER + OPEN + FDEEP + 
                              ka_open + NSGDP, 
                            data = panel_1995[panel_1995$iso3 %in% excluding_africa, ])
summary(excluding_africa_reg1)

excluding_africa_reg2 <- lm(CAGDP ~ -1 + CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 + RELDEPY + RELDEPO + 
                              YGRAVG + YGRSD + CombinedTOTSD + BRREER + OPEN + FDEEP + 
                              ka_open + oil_exporter, 
                            data = panel_1995[panel_1995$iso3 %in% excluding_africa, ])
summary(excluding_africa_reg2)


dev_excluding_africa_reg2 <- lm( CAGDP ~ -1 + CombinedGOVBGDP + NFAGDP + PennRELY +   PennRELY2 + RELDEPY + RELDEPO +  YGRAVG + YGRSD + CombinedTOTSD +  BRREER + OPEN + FDEEP +   ka_open + oil_exporter,
  data = panel_1995[panel_1995$iso3 %in% dev_excluding_africa, ])

summary(dev_excluding_africa_reg2)

#result of table 2 separated in 2 tables (easier to compute on Latex otherwise it was too long )
stargazer(
  all_countries_reg2,
  excluding_africa_reg2,
  industrial,
  type = "text",
  se = list(
    get_robust_se(all_countries_reg2),
    get_robust_se(excluding_africa_reg2),
    get_robust_se(industrial)
  ),
  omit.stat = c("f", "ser"),
  column.labels = c(
    "Full sample",
    "Full excl. Africa",
    "Industrial"
  ),
  title = "Table 2A — Cross-section regressions (current account to GDP ratio)",
  dep.var.labels = "Current account to GDP ratio",
  covariate.labels = c(
    "Govt. budget balance",
    "NFA to GDP ratio",
    "Relative income",
    "Relative income squared",
    "Rel. dependency (young)",
    "Rel. dependency (old)",
    "GDP growth",
    "GDP growth volatility",
    "Terms of trade volatility",
    "Real exchange rate",
    "Trade openness",
    "Financial deepening",
    "Capital account openness",
    "Oil exporter dummy"
  )
)

stargazer(
  developing_reg2,
  dev_excluding_africa_reg2,
  
  type = "text",
  
  se = list(
    get_robust_se(developing_reg2),
    get_robust_se(dev_excluding_africa_reg2)
  ),
  
  omit.stat = c("f", "ser"),
  
  column.labels = c(
    "Developing",
    "Dev. excl. Africa"
  ),
  
  title = "Table 2B — Cross-section regressions (developing countries)",
  
  dep.var.labels = "Current account to GDP ratio",
  
  covariate.labels = c(
    "Govt. budget balance",
    "NFA to GDP ratio",
    "Relative income",
    "Relative income squared",
    "Rel. dependency (young)",
    "Rel. dependency (old)",
    "GDP growth",
    "GDP growth volatility",
    "Terms of trade volatility",
    "Real exchange rate",
    "Trade openness",
    "Financial deepening",
    "Capital account openness",
    "Oil exporter dummy"
  )
)
#table 3 panel analysis 
# Helper function for robust standard errors (HC1, as typical in this literature)
get_robust_se <- function(model) {
  sqrt(diag(vcovHC(model, type = "HC1")))
}

# Formula for Table 3
# Note: factor(year) adds time dummies
formula_t3 <- CAGDP ~ CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 +
  RELDEPY + RELDEPO + FDEEP + CombinedTOTSD + YGRAVG + OPEN +
  ka_open + oil_exporter + factor(year)

# Col 1: Full sample
t3_col1 <- lm(formula_t3, data = panel_1995[panel_1995$iso3 %in% all_countries, ])

# Col 2: Full sample excl. Africa
t3_col2 <- lm(formula_t3, data = panel_1995[panel_1995$iso3 %in% excluding_africa, ])

# Col 3: Industrial countries
t3_col3 <- lm(formula_t3, data = panel_1995[panel_1995$iso3 %in% industrial_countries, ])

# Col 4: Developing countries
t3_col4 <- lm(formula_t3, data = panel_1995[panel_1995$iso3 %in% developing_countries, ])

# Col 5: Developing excl. Africa
t3_col5 <- lm(formula_t3, data = panel_1995[panel_1995$iso3 %in% dev_excluding_africa, ])

# Output with robust SE, hiding year dummies from display
stargazer(t3_col1, t3_col2, t3_col3, t3_col4, t3_col5,
          type = "text",
          se = list(get_robust_se(t3_col1), get_robust_se(t3_col2),
                    get_robust_se(t3_col3), get_robust_se(t3_col4),
                    get_robust_se(t3_col5)),
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
#table 4 : FE regression 

# Table 4 - col 1 : Full sample 

t4_col1_all_countries  <- plm(CAGDP ~ CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 + RELDEPY + RELDEPO + FDEEP + ka_open,
            data = panel_1995[panel_1995$iso3 %in% all_countries, ],
            index = c("iso3", "year"),
            model = "within", effect = "twoways")

summary(t4_col1_all_countries)

# Table 4 - col 2 : Full sample excl. Africa
t4_col2_excluding_africa <- plm(CAGDP ~ CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 + RELDEPY + RELDEPO + FDEEP + ka_open,
            data = panel_1995[panel_1995$iso3 %in% excluding_africa, ],
            index = c("iso3", "year"),
            model = "within", effect = "twoways")
summary(t4_col2_excluding_africa)

# Table 4 - col 3 : Industrial
t4_col3_industrial_countries <- plm(CAGDP ~ CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 + RELDEPY + RELDEPO + FDEEP + ka_open,
            data = panel_1995[panel_1995$iso3 %in% industrial_countries, ],
            index = c("iso3", "year"),
            model = "within", effect = "twoways")
summary(t4_col3_industrial_countries) 

# Table 4 - col 4 : Developing
t4_col4_developing_countries <- plm(CAGDP ~ CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 + RELDEPY + RELDEPO + FDEEP + ka_open,
            data = panel_1995[panel_1995$iso3 %in% developing_countries, ],
            index = c("iso3", "year"),
            model = "within", effect = "twoways")
summary(t4_col4_developing_countries)

# Table 4 - col 5 : Developing excl. Africa
t_4_col5_dev_excluding_africa <- plm(CAGDP ~ CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 + RELDEPY + RELDEPO + FDEEP + ka_open,
            data = panel_1995[panel_1995$iso3 %in% dev_excluding_africa, ],
            index = c("iso3", "year"),
            model = "within", effect = "twoways")
summary(t_4_col5_dev_excluding_africa)

stargazer(
  t4_col1_all_countries,
  t4_col2_excluding_africa,
  t4_col3_industrial_countries,
  
  type = "text",
  
  se = list(
    get_robust_se(t4_col1_all_countries),
    get_robust_se(t4_col2_excluding_africa),
    get_robust_se(t4_col3_industrial_countries)
  ),
  
  omit.stat = c("f", "ser"),
  
  column.labels = c(
    "Full sample",
    "Full excl. Africa",
    "Industrial"
  ),
  
  title = "Table 4A — Fixed effects panel regressions",
  
  dep.var.labels = "Current account to GDP ratio",
  
  covariate.labels = c(
    "Govt. budget balance",
    "NFA to GDP ratio",
    "Relative income",
    "Relative income squared",
    "Rel. dependency (young)",
    "Rel. dependency (old)",
    "Financial deepening",
    "Capital account openness"
  )
)

stargazer(
  t4_col4_developing_countries,
  t_4_col5_dev_excluding_africa,
  
  type = "text",
  
  se = list(
    get_robust_se(t4_col4_developing_countries),
    get_robust_se(t_4_col5_dev_excluding_africa)
  ),
  
  omit.stat = c("f", "ser"),
  
  column.labels = c(
    "Developing",
    "Dev. excl. Africa"
  ),
  
  title = "Table 4B — Fixed effects panel regressions (developing countries)",
  
  dep.var.labels = "Current account to GDP ratio",
  
  covariate.labels = c(
    "Govt. budget balance",
    "NFA to GDP ratio",
    "Relative income",
    "Relative income squared",
    "Rel. dependency (young)",
    "Rel. dependency (old)",
    "Financial deepening",
    "Capital account openness"
  )
)

#Table 5
#for this regression we are using a different data set (annual instead of 5-years average)
panel_annual <- read.csv("Data/AnnualPanel.csv")   # adjust path as needed


panel_annual_1995 <- panel_annual %>%
  filter(year <= 1995) %>%
  filter(year >= 1971) %>%
  mutate(
    oil_exporter     = ifelse(iso3 %in% oil_exporting_countries, 1, 0),
    PennRELY2        = PennRELY^2) %>% 
     group_by(iso3) %>%
    arrange(year) %>% 
     mutate(
       CAGDP_lag       = dplyr::lag(CAGDP, 1),#adding lag of CA and BRREER
       BRREER_diff_lag = dplyr::lag(BRREER - dplyr::lag(BRREER, 1), 1)
     ) %>%
     ungroup()

# Formula for Table 5
formula_t5 <- CAGDP ~ IMFGOVBGDP + NFAGDP + PennRELY + PennRELY2 +
  RELDEPY + RELDEPO + FDEEP + IndirectTOTS + gdpgr + OPEN +
  ka_open + oil_exporter +
  CAGDP_lag + BRREER_diff_lag +
  factor(year)

# Col 1: Full sample
t5_col1 <- lm(formula_t5,
              data = panel_annual_1995[panel_annual_1995$iso3 %in% all_countries, ])
summary(t5_col1)
# Col 2: Full sample excl. Africa
t5_col2 <- lm(formula_t5,
              data = panel_annual_1995[panel_annual_1995$iso3 %in% excluding_africa, ])
summary(t5_col2)
# Col 3: Industrial
t5_col3 <- lm(formula_t5,
              data = panel_annual_1995[panel_annual_1995$iso3 %in% industrial_countries, ])
summary(t5_col3)
# Col 4: Developing
t5_col4 <- lm(formula_t5,
              data = panel_annual_1995[panel_annual_1995$iso3 %in% developing_countries, ])
summary(t5_col4)
# Col 5: Developing excl. Africa
t5_col5 <- lm(formula_t5,
              data = panel_annual_1995[panel_annual_1995$iso3 %in% dev_excluding_africa, ])
summary(t5_col5)

# Output with robust SE
stargazer(t5_col1, t5_col2, t5_col3, t5_col4, t5_col5,
          type = "text",
          se = list(get_robust_se(t5_col1), get_robust_se(t5_col2),
                    get_robust_se(t5_col3), get_robust_se(t5_col4),
                    get_robust_se(t5_col5)),
          omit        = "factor",
          omit.labels = "Year dummies",
          omit.stat   = c("f", "ser"),
          column.labels = c("Full", "Full excl. Africa", "Industrial",
                            "Developing", "Dev. excl. Africa"),
          title = "Table 5 — OLS annual data between 1971 and 1995 with time effects",
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

#EXTENSIONS 

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
panel_actualisation = panel_actualisation %>% 
  group_by(iso3) %>% 
  arrange(year) %>% 
  mutate(lagKcontrols = dplyr::lag(ka_open))

formula_t3_in <- InflowsGDP ~ CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 +
  RELDEPY + RELDEPO + FDEEP + CombinedTOTSD + YGRAVG + OPEN +
  ka_open + lagKcontrols + oil_exporter + factor(year)

t3_col1_in <- lm(formula_t3_in, data = panel_actualisation[panel_actualisation$iso3 %in% all_countries, ])
t3_col2_in <- lm(formula_t3_in, data = panel_actualisation[panel_actualisation$iso3 %in% excluding_africa, ])
t3_col3_in <- lm(formula_t3_in, data = panel_actualisation[panel_actualisation$iso3 %in% industrial_countries, ])
t3_col4_in <- lm(formula_t3_in, data = panel_actualisation[panel_actualisation$iso3 %in% developing_countries, ])
t3_col5_in <- lm(formula_t3_in, data = panel_actualisation[panel_actualisation$iso3 %in% dev_excluding_africa, ])

stargazer(t3_col1_in, t3_col2_in, t3_col3_in, t3_col4_in, t3_col5_in,
          se = list(get_robust_se(t3_col1_in), get_robust_se(t3_col2_in),
                    get_robust_se(t3_col3_in), get_robust_se(t3_col4_in),
                    get_robust_se(t3_col5_in)),
          type = "text",
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
            "Capital controls", "Capital controls in previous period", "Oil exporter dummy"
          )
)

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
          type = "text",
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

# EVENT STUDY

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
    PennRELY + gov_balance + NSGDP + ka_open| year,  # FE temporels après le |
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

#EXTENSION ON THE DECOMPOSITION OF THE NATIONAL SAVING
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


# FIGURES 1-4 — Reproduction of Chinn and Prasad graphics. 

install.packages("gridExtra")
library(gridExtra)
library(ggplot2)
library(dplyr)
library(tidyr)

panel1<-read.csv("Data/5yearPanel.csv")
panel_1995 <- panel1%>%
  filter(year <= 1995) %>% 
  mutate(PennRELY2=PennRELY^2, RELY2 = RELY^2)

# Figure 1 : CA vs NFA initial (cross-section)

# NFA initial = mean on 5 years
initial_nfa <- panel_1995 %>%
  filter(iso3 %in% all_countries) %>%
  arrange(iso3, year) %>%
  group_by(iso3) %>%
  summarise(initial_NFA = mean(NFAGDP, na.rm = TRUE))

mean_ca <- panel_1995 %>%
  filter(iso3 %in% all_countries) %>%
  group_by(iso3) %>%
  summarise(mean_CAGDP = mean(CAGDP / 100, na.rm = TRUE))

fig1_data <- initial_nfa %>%
  left_join(mean_ca, by = "iso3") %>%
  drop_na() %>%
  mutate(
    group = ifelse(iso3 %in% industrial_countries, "Industrial", "Developing"),
    label = ifelse(iso3 %in% industrial_countries, toupper(iso3), tolower(iso3))
  )

plot_fig1 <- function(grp) {
  sub <- fig1_data %>% filter(group == grp)
  ggplot(sub, aes(x = initial_NFA, y = mean_CAGDP, label = label)) +
    geom_point(size = 1.2) +
    geom_text(size = 2.2, vjust = -0.4) +
    geom_hline(yintercept = 0, linewidth = 0.4) +
    geom_vline(xintercept = 0, linewidth = 0.4) +
    labs(title = grp,
         x = "Initial NFA/GDP Ratio",
         y = "Current Account to GDP Ratio") +
    theme_bw(base_size = 10)
}

fig1_ind <- plot_fig1("Industrial")
fig1_dev <- plot_fig1("Developing")

gridExtra::grid.arrange(fig1_ind, fig1_dev, ncol = 1,
                        top = "Fig. 1 — Current accounts and net foreign assets, cross section")

# Figure 2 : CA vs relative income (cross-section)

mean_rely <- panel_1995 %>%
  filter(iso3 %in% all_countries) %>%
  group_by(iso3) %>%
  summarise(mean_RELY = mean(PennRELY, na.rm = TRUE))

fig2_data <- mean_rely %>%
  left_join(mean_ca, by = "iso3") %>%
  drop_na() %>%
  mutate(
    group = ifelse(iso3 %in% industrial_countries, "Industrial", "Developing"),
    label = ifelse(iso3 %in% industrial_countries, toupper(iso3), tolower(iso3))
  )

fig2 <- ggplot(fig2_data, aes(x = mean_RELY, y = mean_CAGDP, label = label)) +
  geom_point(size = 1.2) +
  geom_text(size = 2.2, vjust = -0.4) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  labs(title = "Fig. 2 — Current accounts and relative income, cross section",
       x = "Relative Income",
       y = "Current Account to GDP Ratio") +
  theme_bw(base_size = 10)

print(fig2)

# ── Figure 3 : Actual vs Fitted CA pour les pays en développement ─────────────

# Régression avec et sans dummies temporelles
formula_with    <- CAGDP ~ CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 +
  RELDEPY + RELDEPO + FDEEP + CombinedTOTSD + YGRAVG + OPEN +
  ka_open + oil_exporter + factor(year)
formula_without <- CAGDP ~ CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 +
  RELDEPY + RELDEPO + FDEEP + CombinedTOTSD + YGRAVG + OPEN +
  ka_open + oil_exporter

dev_data <- panel_1995 %>%
  filter(iso3 %in% developing_countries) %>%
  mutate(oil_exporter = ifelse(iso3 %in% oil_exporting_countries, 1, 0))

m_with    <- lm(formula_with,    data = dev_data)
m_without <- lm(formula_without, data = dev_data)

dev_data_fitted <- dev_data %>%
  filter(complete.cases(select(., all_of(c("CAGDP","CombinedGOVBGDP","NFAGDP",
                                           "PennRELY","PennRELY2","RELDEPY","RELDEPO","FDEEP","CombinedTOTSD",
                                           "YGRAVG","OPEN","ka_open","oil_exporter"))))) %>%
  mutate(
    fitted_with    = fitted(m_with),
    fitted_without = fitted(m_without)
  )

asia   <- c("BGD","IND","IDN","KOR","MYS","NPL","PAK","PHL","LKA","THA","SGP")
lat_am <- c("ARG","BOL","BRA","CHL","COL","CRI","ECU","GTM","HTI","HND",
            "JAM","MEX","PAN","PRY","PER","TTO","URY","VEN")

blocks <- list(
  "All Developing" = developing_countries,
  "Asia"           = asia,
  "Africa"         = africa,
  "Latin America"  = lat_am
)

# Agréger par période (les years dans panel_1995 sont déjà des débuts de période 5 ans)
period_labels <- c("1971"="1971-75","1976"="1976-80","1981"="1981-85",
                   "1986"="1986-90","1991"="1991-95")

fig3_plots <- lapply(names(blocks), function(blk) {
  sub <- dev_data_fitted %>%
    filter(iso3 %in% blocks[[blk]]) %>%
    mutate(period = factor(as.character(year), levels = names(period_labels),
                           labels = period_labels)) %>%
    group_by(period) %>%
    summarise(
      Actual              = mean(CAGDP,          na.rm = TRUE),
      `With time effects` = mean(fitted_with,    na.rm = TRUE),
      `No time effects`   = mean(fitted_without, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(-period, names_to = "Series", values_to = "value")
  
  ggplot(sub, aes(x = period, y = value, group = Series,
                  shape = Series, linetype = Series)) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0, linewidth = 0.3, linetype = "dashed") +
    scale_shape_manual(values = c("Actual" = 1, "With time effects" = 15,
                                  "No time effects" = 2)) +
    scale_linetype_manual(values = c("Actual" = "solid", "With time effects" = "dashed",
                                     "No time effects" = "dotted")) +
    labs(title = blk, x = "", y = "CA/GDP Ratio", shape = NULL, linetype = NULL) +
    theme_bw(base_size = 9) +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 30, hjust = 1))
})

gridExtra::grid.arrange(grobs = fig3_plots, ncol = 2,
                        top = "Fig. 3 — Actual, fitted current accounts: developing country blocks")

# ── Figure 4 : Partial scatterplots ──────────────────────────────────────────


partial_resid <- function(data, y, x, controls) {
  f_y <- as.formula(paste(y, "~", paste(controls, collapse = " + ")))
  f_x <- as.formula(paste(x, "~", paste(controls, collapse = " + ")))
  sub <- data %>% select(all_of(c(y, x, controls))) %>% drop_na()
  ry  <- residuals(lm(f_y, data = sub))
  rx  <- residuals(lm(f_x, data = sub))
  data.frame(rx = rx, ry = ry)
}

controls <- c("NFAGDP","PennRELY","PennRELY2","RELDEPY","RELDEPO",
              "FDEEP","CombinedTOTSD","YGRAVG","OPEN","ka_open","oil_exporter")

fig4_configs <- list(
  list(grp = industrial_countries,  x = "CombinedGOVBGDP", title = "Industrial — Govt Balance"),
  list(grp = developing_countries,  x = "CombinedGOVBGDP", title = "Developing — Govt Balance"),
  list(grp = industrial_countries,  x = "NFAGDP",          title = "Industrial — Initial NFA/GDP"),
  list(grp = developing_countries,  x = "NFAGDP",          title = "Developing — Initial NFA/GDP")
)

fig4_plots <- lapply(fig4_configs, function(cfg) {
  sub  <- panel_1995 %>%
    filter(iso3 %in% cfg$grp) %>%
    mutate(oil_exporter = ifelse(iso3 %in% oil_exporting_countries, 1, 0))
  ctrl <- controls[controls != cfg$x]
  pr   <- partial_resid(sub, "CAGDP", cfg$x, ctrl)
  
  ggplot(pr, aes(x = rx, y = ry)) +
    geom_point(size = 1, alpha = 0.6) +
    geom_hline(yintercept = 0, linewidth = 0.4) +
    geom_vline(xintercept = 0, linewidth = 0.4) +
    labs(title = cfg$title,
         x = cfg$x,
         y = "CA/GDP Ratio") +
    theme_bw(base_size = 9)
})

gridExtra::grid.arrange(grobs = fig4_plots, ncol = 2,
                        top = "Fig. 4 — Partial scatterplots")

#figure with actual data

panel1<-read.csv("Data/panel_3.csv")
panel_act <- panel1%>%
  mutate(PennRELY2=PennRELY^2, RELY2 = RELY^2)

# Figure 1 : CA vs NFA initial (cross-section) 

initial_nfa_act <- panel_act %>%
  filter(iso3 %in% all_countries) %>%
  arrange(iso3, year) %>%
  group_by(iso3) %>%
  summarise(initial_NFA = mean(NFAGDP, na.rm = TRUE))

mean_ca_act <- panel_act %>%
  filter(iso3 %in% all_countries) %>%
  group_by(iso3) %>%
  summarise(mean_CAGDP = mean(CAGDP / 100, na.rm = TRUE))

fig1_data_act <- initial_nfa_act %>%
  left_join(mean_ca_act, by = "iso3") %>%
  drop_na() %>%
  mutate(
    group = ifelse(iso3 %in% industrial_countries, "Industrial", "Developing"),
    label = ifelse(iso3 %in% industrial_countries, toupper(iso3), tolower(iso3))
  )

plot_fig1_act <- function(grp) {
  sub <- fig1_data_act %>% filter(group == grp)
  ggplot(sub, aes(x = initial_NFA, y = mean_CAGDP, label = label)) +
    geom_point(size = 1.2) +
    geom_text(size = 2.2, vjust = -0.4) +
    geom_hline(yintercept = 0, linewidth = 0.4) +
    geom_vline(xintercept = 0, linewidth = 0.4) +
    labs(title = grp,
         x = "Initial NFA/GDP Ratio",
         y = "Current Account to GDP Ratio") +
    theme_bw(base_size = 10)
}

fig1_ind_act <- plot_fig1_act("Industrial")
fig1_dev_act <- plot_fig1_act("Developing")

gridExtra::grid.arrange(fig1_ind_act, fig1_dev_act, ncol = 1,
                        top = "Fig. 1 — Current accounts and net foreign assets, cross section with actual data")

# Figure 2 : CA vs revenu relatif (cross-section) 

mean_rely_act <- panel_act %>%
  filter(iso3 %in% all_countries) %>%
  group_by(iso3) %>%
  summarise(mean_RELY = mean(PennRELY, na.rm = TRUE))

fig2_data_act <- mean_rely_act %>%
  left_join(mean_ca_act, by = "iso3") %>%
  drop_na() %>%
  mutate(
    group = ifelse(iso3 %in% industrial_countries, "Industrial", "Developing"),
    label = ifelse(iso3 %in% industrial_countries, toupper(iso3), tolower(iso3))
  )

fig2_act <- ggplot(fig2_data_act, aes(x = mean_RELY, y = mean_CAGDP, label = label)) +
  geom_point(size = 1.2) +
  geom_text(size = 2.2, vjust = -0.4) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  labs(title = "Fig. 2 — Current accounts and relative income, cross section with actual data",
       x = "Relative Income",
       y = "Current Account to GDP Ratio") +
  theme_bw(base_size = 10)

print(fig2_act)

# Figure 3 : Actual vs Fitted CA for developing countries

# Régression with and without time dummy
formula_with_act    <- CAGDP ~ CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 +
  RELDEPY + RELDEPO + FDEEP + CombinedTOTSD + YGRAVG + OPEN +
  ka_open + oil_exporter + factor(year)
formula_without_act <- CAGDP ~ CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 +
  RELDEPY + RELDEPO + FDEEP + CombinedTOTSD + YGRAVG + OPEN +
  ka_open + oil_exporter

dev_data_act <- panel_act %>%
  filter(iso3 %in% developing_countries) %>%
  mutate(oil_exporter = ifelse(iso3 %in% oil_exporting_countries, 1, 0))

m_with_act    <- lm(formula_with_act,    data = dev_data_act)
m_without_act <- lm(formula_without_act, data = dev_data_act)

dev_data_fitted_act <- dev_data_act %>%
  filter(complete.cases(select(., all_of(c("CAGDP","CombinedGOVBGDP","NFAGDP",
                                           "PennRELY","PennRELY2","RELDEPY","RELDEPO","FDEEP","CombinedTOTSD",
                                           "YGRAVG","OPEN","ka_open","oil_exporter"))))) %>%
  mutate(
    fitted_with    = fitted(m_with_act),
    fitted_without = fitted(m_without_act)
  )

asia   <- c("BGD","IND","IDN","KOR","MYS","NPL","PAK","PHL","LKA","THA","SGP")
lat_am <- c("ARG","BOL","BRA","CHL","COL","CRI","ECU","GTM","HTI","HND",
            "JAM","MEX","PAN","PRY","PER","TTO","URY","VEN")

blocks <- list(
  "All Developing" = developing_countries,
  "Asia"           = asia,
  "Africa"         = africa,
  "Latin America"  = lat_am
)

# Agréger par période (les years dans panel_1995 sont déjà des débuts de période 5 ans)
period_labels_act <- c("1971"="1971-75","1976"="1976-80","1981"="1981-85",
                       "1986"="1986-90","1991"="1991-95", "1996"="1996-2000", "2001"="2001-05",
                       "2006"="2006-10", "2011"="2011-15", "2016"="2016-20", "2021"="2021-today")

fig3_plots_act <- lapply(names(blocks), function(blk) {
  sub <- dev_data_fitted_act %>%
    filter(iso3 %in% blocks[[blk]]) %>%
    mutate(period = factor(as.character(year), levels = names(period_labels),
                           labels = period_labels)) %>%
    group_by(period) %>%
    summarise(
      Actual              = mean(CAGDP,          na.rm = TRUE),
      `With time effects` = mean(fitted_with,    na.rm = TRUE),
      `No time effects`   = mean(fitted_without, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(-period, names_to = "Series", values_to = "value")
  
  ggplot(sub, aes(x = period, y = value, group = Series,
                  shape = Series, linetype = Series)) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0, linewidth = 0.3, linetype = "dashed") +
    scale_shape_manual(values = c("Actual" = 1, "With time effects" = 15,
                                  "No time effects" = 2)) +
    scale_linetype_manual(values = c("Actual" = "solid", "With time effects" = "dashed",
                                     "No time effects" = "dotted")) +
    labs(title = blk, x = "", y = "CA/GDP Ratio", shape = NULL, linetype = NULL) +
    theme_bw(base_size = 9) +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 30, hjust = 1))
})

gridExtra::grid.arrange(grobs = fig3_plots_act, ncol = 2,
                        top = "Fig. 3 — Actual, fitted current accounts: developing country blocks, with actual data")

# Figure 4 : Partial scatterplots 


partial_resid_act <- function(data, y, x, controls) {
  f_y <- as.formula(paste(y, "~", paste(controls, collapse = " + ")))
  f_x <- as.formula(paste(x, "~", paste(controls, collapse = " + ")))
  sub <- data %>% select(all_of(c(y, x, controls))) %>% drop_na()
  ry  <- residuals(lm(f_y, data = sub))
  rx  <- residuals(lm(f_x, data = sub))
  data.frame(rx = rx, ry = ry)
}

controls <- c("NFAGDP","PennRELY","PennRELY2","RELDEPY","RELDEPO",
              "FDEEP","CombinedTOTSD","YGRAVG","OPEN","ka_open","oil_exporter")

fig4_configs_act <- list(
  list(grp = industrial_countries,  x = "CombinedGOVBGDP", title = "Industrial — Govt Balance"),
  list(grp = developing_countries,  x = "CombinedGOVBGDP", title = "Developing — Govt Balance"),
  list(grp = industrial_countries,  x = "NFAGDP",          title = "Industrial — Initial NFA/GDP"),
  list(grp = developing_countries,  x = "NFAGDP",          title = "Developing — Initial NFA/GDP")
)

fig4_plots_act <- lapply(fig4_configs_act, function(cfg) {
  sub  <- panel_act %>%
    filter(iso3 %in% cfg$grp) %>%
    mutate(oil_exporter = ifelse(iso3 %in% oil_exporting_countries, 1, 0))
  ctrl <- controls[controls != cfg$x]
  pr   <- partial_resid(sub, "CAGDP", cfg$x, ctrl)
  
  ggplot(pr, aes(x = rx, y = ry)) +
    geom_point(size = 1, alpha = 0.6) +
    geom_hline(yintercept = 0, linewidth = 0.4) +
    geom_vline(xintercept = 0, linewidth = 0.4) +
    labs(title = cfg$title,
         x = cfg$x,
         y = "CA/GDP Ratio") +
    theme_bw(base_size = 9)
})

gridExtra::grid.arrange(grobs = fig4_plots_act, ncol = 2,
                        top = "Fig. 4 — Partial scatterplots with actual data")

