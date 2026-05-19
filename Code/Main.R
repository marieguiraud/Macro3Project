library(tidyverse)
library(stargazer)
library(plm)
library(xtable)
library(zoo)

#prelimiaries
# Data
panel <- read.csv("Data/panel_3.csv")
panel_1995 <- panel%>%
  filter(year <= 1995) %>% 
  mutate(PennRELY2=PennRELY^2)
panel_p <- pdata.frame(panel_1995, index = c("iso3", "year"))
View(panel_p)
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
  "OPEN","FDEEP","NSGDP","ka_open","NFAGDP"
)
#PANEL PAPER
panel_paper <- panel_1995 %>%
  filter(iso3 %in% all_countries) %>%
  select(iso3, year, CAGDP, CombinedGOVBGDP, PennRELY, , PennRELY2, RELDEPY, RELDEPO, 
         YGRAVG, YGRSD, CombinedTOTSD, BRREER, OPEN, FDEEP, NSGDP, ka_open, NFAGDP)
colSums(is.na(panel_paper))

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

all_countries_reg1 <- lm( CAGDP ~ -1 + GOVBGDP + NFAGDP + RELY + RELY2 + RELDEPY + RELDEPO + YGRAVG + YGRSD + TOTSD + LREER + OPEN + FDEEP + ka_open + NSGDP, data = panel_p)
summary(all_countries_reg1)

#all countries with ne new variables 
all_countries_reg2 <- lm( CAGDP ~ -1 + CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 +  RELDEPY + RELDEPO + YGRAVG + YGRSD + CombinedTOTSD + BRREER + OPEN + FDEEP + ka_open + NSGDP, data = panel_1995[panel_1995$iso3 %in% all_countries, ])
summary(all_countries_reg2)

developing_reg1 <- lm(CAGDP ~ -1 + GOVBGDP + NFAGDP + RELY + RELY2 + RELDEPY + RELDEPO + 
                        YGRAVG + YGRSD + CombinedTOTSD + LREER + OPEN + FDEEP + 
                        ka_open + NSGDP, 
                      data = panel_1995[panel_1995$iso3 %in% developing_countries, ])
summary(developing_reg1)

developing_reg2 <- lm(CAGDP ~ -1 + CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 + RELDEPY + RELDEPO + 
                        YGRAVG + YGRSD + TOTSD + BRREER + OPEN + FDEEP + 
                        ka_open + NSGDP, 
                      data = panel_1995[panel_1995$iso3 %in% developing_countries, ])
summary(developing_reg2)


excluding_africa_reg1 <- lm(CAGDP ~ -1 + GOVBGDP + NFAGDP + RELY + RELY2 + RELDEPY + RELDEPO + 
                              YGRAVG + YGRSD + TOTSD + LREER + OPEN + FDEEP + 
                              ka_open + NSGDP, 
                            data = panel_1995[panel_1995$iso3 %in% excluding_africa, ])
summary(excluding_africa_reg1)

excluding_africa_reg2 <- lm(CAGDP ~ -1 + CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 + RELDEPY + RELDEPO + 
                              YGRAVG + YGRSD + CombinedTOTSD + BRREER + OPEN + FDEEP + 
                              ka_open + NSGDP, 
                            data = panel_1995[panel_1995$iso3 %in% excluding_africa, ])
summary(excluding_africa_reg2)

#table 2 (run when missing value problem is solved) 
#indutrial countries 
industrial <- lm(CAGDP ~ -1 + CombinedGOVBGDP + NFAGDP + PennRELY + PennRELY2 + RELDEPY + RELDEPO + 
                   YGRAVG + YGRSD + CombinedTOTSD + BRREER + OPEN + FDEEP + 
                   ka_open + NSGDP, 
                 data = panel_1995[panel_1995$iso3 %in% industrial_countries, ])

summary(industrial)




#check how many NA we have for each variable 
df <- panel_1995[panel_1995$iso3 %in% developing_countries, ]
na_report <- sapply(df[, vars2], function(x) sum(is.na(x)))
na_report
class(df)

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

stargazer(t4_col1_all_countries, t4_col2_excluding_africa, t4_col3_industrial_countries, t4_col4_developing_countries, t_4_col5_dev_excluding_africa, type = "text",
          column.labels = c("Full","Full excl. Africa","Industrial","Developing","Dev. excl. Africa"),
          title = "Table 4 — Fixed Effects with time effects")



