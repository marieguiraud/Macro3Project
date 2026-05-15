library(tidyverse)
library(plm)
library(xtable)

# Data
panel <- read.csv("Data/panel_3.csv")
panel_p <- pdata.frame(panel, index = c("iso3", "year"))

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

# Variables
vars <- c(
  "CAGDP","GOVBGDP","RELY","RELDEPY","RELDEPO",
  "YGRAVG","YGRSD","TOTSD","LREER",
  "OPEN","FDEEP","NSGDP","ka_open","NFAGDP"
)
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
  map_dfr(vars, ~variance_decomp(df, .x)) %>%
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

# Combine if needed
all_results <- bind_rows(industrial_table, developing_table)
print(all_results)

# Export to LaTeX
xtable(
  industrial_table,
  caption = "Variance decomposition - Industrial countries"
)

xtable(
  developing_table,
  caption = "Variance decomposition - Developing countries"
)

# Combine results correctly

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

# Display final table

print(final_table)

# Export to LaTeX (single table)
library(xtable)

xtable(
  final_table,
  caption = "Variance decomposition: Industrial vs Developing countries",
  label = "tab:variance_decomposition_comparison"
)