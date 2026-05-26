library(pwt10)
library(knitr)
library(tidyverse)
library(stargazer)
library(plm)
library(xtable)
library(zoo)
library(lmtest)
library(sandwich)


# Data
panel <- read.csv("Data/5yearPanel.csv")
panel_1995 <- panel%>%
  filter(year <= 1995) %>% 
  mutate(PennRELY2=PennRELY^2, RELY2 = RELY^2)
panel_1995$CombinedGOVBGDP[panel_1995$iso3 == "GNQ"] <- NA

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

panel_1995 <- panel_1995 %>%
  mutate(oil_exporter=ifelse(iso3 %in% oil_exporting_countries, 1, 0))
panel_p <- pdata.frame(panel_1995, index = c("iso3", "year"))
summary(panel_1995)

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

#Table 2 

all_countries_reg1 <- lm( CAGDP ~ -1 + GOVBGDP + NFAGDP + RELY + RELY2 + RELDEPY + RELDEPO + YGRAVG + YGRSD + TOTSD + LREER + OPEN + FDEEP + ka_open + NSGDP, data = panel_p)
summary(all_countries_reg1)

#all countries with new new variables 
all_countries_reg2 <- lm( CAGDP ~ -1 + CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 +  RELDEPY + RELDEPO + YGRAVG + YGRSD + CombinedTOTSD + BRREER + OPEN + FDEEP + ka_open, data = panel_1995[panel_1995$iso3 %in% all_countries, ])
summary(all_countries_reg2)


developing_reg1 <- lm(CAGDP ~ -1 + GOVBGDP + NFAGDP + RELY + RELY2 + RELDEPY + RELDEPO + 
                        YGRAVG + YGRSD + CombinedTOTSD + LREER + OPEN + FDEEP + 
                        ka_open + NSGDP, 
                      data = panel_1995[panel_1995$iso3 %in% developing_countries, ])
summary(developing_reg1)

developing_reg2 <- lm(CAGDP ~ -1 + CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 + RELDEPY + RELDEPO + 
                        YGRAVG + YGRSD + TOTSD + BRREER + OPEN + FDEEP + 
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

 
industrial <- lm(CAGDP ~ -1 + CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 + RELDEPY + RELDEPO + 
                   YGRAVG + YGRSD + CombinedTOTSD + BRREER + OPEN + FDEEP + 
                   ka_open + oil_exporter, 
                 data = panel_1995[panel_1995$iso3 %in% industrial_countries, ])

summary(industrial)

dev_excluding_africa_reg2 <- lm(
  CAGDP ~ -1 + CombinedGOVBGDP + NFAGDP + PennRELY +
    PennRELY2 + RELDEPY + RELDEPO +
    YGRAVG + YGRSD + CombinedTOTSD +
    BRREER + OPEN + FDEEP +
    ka_open + oil_exporter,
  data = panel_1995[panel_1995$iso3 %in% dev_excluding_africa, ]
)

summary(dev_excluding_africa_reg2)

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
#table 3
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
#table 4 

# Table 4 - col 1 : Full sample 

t4_col1_all_countries  <- plm(CAGDP ~ CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 + RELDEPY + RELDEPO + FDEEP + ka_open,
            data = panel_1995[panel_1995$iso3 %in% all_countries, ],
            index = c("iso3", "year"),
            model = "within", effect = "twoways")

summary(t4_col1_all_countries) #il n'y a que 56 observations et les pays n'ont que 1 ou 2 observations ...

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
summary(t4_col3_industrial_countries) #toujours un problème sur les industrials ont a que 9 observations

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

#check how many NA we have for each variable 
df <- panel_1995[panel_1995$iso3 %in% developing_countries, ]
na_report <- sapply(df[, vars2], function(x) sum(is.na(x)))
na_report
class(df)

#Table 5
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
       CAGDP_lag       = dplyr::lag(CAGDP, 1),
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


