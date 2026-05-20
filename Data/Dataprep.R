library(WDI)
library(tidyverse)
library(dplyr)
library(readxl)
library(dplyr)
library(imf.data)
library(countrycode)

# ── 1. Pull all WDI-available Chinn-Prasad variables ────────────────────────

wdi_raw <- WDI(
  indicator = c(
    "BN.CAB.XOKA.GD.ZS",   # CAGDP
    "NY.GDP.PCAP.PP.CD",   # RELY (raw, needs transformation)
    "SP.POP.DPND.YG",      # RELDEPY (raw, needs normalization)
    "SP.POP.DPND.OL",      # RELDEPO (raw, needs normalization)
    "NY.GDP.MKTP.KD.ZG",   # YGRAVG + YGRSD (same series, both derived)
    "TT.PRI.MRCH.XD.WD",    #TOTSD
    "PX.REX.REER",         # LREER (needs log)
    "NE.TRD.GNFS.ZS",      # OPEN
    "FM.LBL.BMNY.GD.ZS",   # FDEEP
    "NY.GNS.ICTR.ZS"    # NSGDP
    ),
  country = "all",
  start   = 1960,
  end     = 2023,
  extra   = TRUE
) |>
  as_tibble() |>
  filter(region != "Aggregates") |>
  select(
    iso3  = iso3c,
    year,
    CAGDP   = BN.CAB.XOKA.GD.ZS,
    gdppc   = NY.GDP.PCAP.PP.CD,    # intermediate — transformed below
    dep_y   = SP.POP.DPND.YG,       # intermediate — normalized below
    dep_o   = SP.POP.DPND.OL,       # intermediate — normalized below
    gdpgr   = NY.GDP.MKTP.KD.ZG,    # intermediate — averaged/SD below
    LREER   = PX.REX.REER,
    OPEN    = NE.TRD.GNFS.ZS,
    FDEEP   = FM.LBL.BMNY.GD.ZS,
    NSGDP   = NY.GNS.ICTR.ZS,
    totindex = TT.PRI.MRCH.XD.WD
  )


# ── 2. Construct transformed variables ──────────────────────────────────────

# US GDP per capita (reference for RELY)
us_gdppc <- wdi_raw |>
  filter(iso3 == "USA") |>
  select(year, us_gdppc = gdppc)

wdi_clean <- wdi_raw |>
  left_join(us_gdppc, by = "year") |>
  mutate(
    # RELY: PPP income relative to US, clipped to [0, 1]
    RELY  = pmin(gdppc / us_gdppc, 1),
    # LREER: log of REER index
    LREER = log(LREER)
  ) |>
  # RELDEPY / RELDEPO: normalize by cross-country mean in each year
  group_by(year) |>
  mutate(
    RELDEPY = dep_y / mean(dep_y, na.rm = TRUE),
    RELDEPO = dep_o / mean(dep_o, na.rm = TRUE)
  ) |>
  ungroup() |>
  select(iso3, year, CAGDP, RELY, RELDEPY, RELDEPO,
         gdpgr, LREER, OPEN, FDEEP, NSGDP,totindex)


# ── 3. Collapse to 5-year non-overlapping periods (as in the paper) ─────────
yearpanel = wdi_clean

panel <- wdi_clean |>
  filter(year >= 1971) |>
  mutate(period = (year - 1971) %/% 5) |>
  group_by(iso3, period) |>
  summarise(
    year = min(year)-((min(year)-1971)%%5),          # label each period by its first year
    CAGDP   = mean(CAGDP,   na.rm = TRUE),
    RELY    = mean(RELY,    na.rm = TRUE),
    RELDEPY = mean(RELDEPY, na.rm = TRUE),
    RELDEPO = mean(RELDEPO, na.rm = TRUE),
    YGRAVG  = mean(gdpgr,   na.rm = TRUE),   # average GDP growth
    YGRSD   = sd(gdpgr,     na.rm = TRUE),   # SD of GDP growth
    LREER   = mean(LREER,   na.rm = TRUE),
    OPEN    = mean(OPEN,    na.rm = TRUE),
    FDEEP   = mean(FDEEP,   na.rm = TRUE),
    NSGDP   = mean(NSGDP,   na.rm = TRUE),
    TOTSD = sd(totindex, na.rm = TRUE),
    .groups = "drop"
  ) |>
  select(-period)

#Introduction of the data set for KC2 and KC3 gathered in ka_open 
ka_open<- read.csv("Data/ka_open.csv")


#the goal is to transform the entire name of countries to iso3c.
ka_open$iso3 <- countrycode(
  ka_open$country_name,
  origin = "country.name",
  destination = "iso3c"
)
#some values have not succeed in to be transform because strange name like S? Tom for Sao Tom and Principe
unique(ka_open$country_name[is.na(ka_open$iso3)])
#we delete this countries: one is an island of Netherlands and the other Sao Tome and Principe

ka_open <- subset(
  ka_open,
  !country_name %in% c(
    "Netherlands Antilles",
    "S? Tom and Principe"
  )
)

#I delete the column country_name and I put iso3 in first column
ka_open <- ka_open %>%
  select(iso3, everything(), -country_name)

yearpanel = yearpanel %>% 
  left_join(ka_open, by = c("iso3", "year"))

#I clean for data from 71 to today and i compute a mean for period of 5 years
ka_open_clean <- ka_open |>
  filter(year >= 1971) |>
  mutate(period = (year - 1971) %/% 5) |>
  group_by(iso3, period) |>
  summarise(
    year = min(year)-((min(year)-1971)%%5),          # label each period by its first year
    ka_open = mean(ka_open,   na.rm = TRUE),
    .groups = "drop"
  ) |>
  select(-period)

# I merge ka_open_clean in panel 
panel_2<- panel %>%
  left_join(ka_open_clean, by = c("iso3", "year"))

#introduction of the NFAGDP variable
NFAGDP<-read_xlsx("Data/NFAGDP.xlsx")

#I keep only variable Country, year and Net IIP excl gold

NFAGDP_clean <- NFAGDP %>%
  select("Country", "Year", "net IIP excl gold / GDP domestic currency")

#I transform Country from the letter name to iso3
NFAGDP_clean$iso3 <- countrycode(
  NFAGDP_clean$Country,
  origin = "country.name",
  destination = "iso3c"
)

#I see observations with a problem in their iso3
unique(NFAGDP_clean$Country[is.na(NFAGDP_clean$iso3)])

#I delete these information of the panel
NFAGDP_clean_2 <- subset(
  NFAGDP_clean,
  !Country %in% c(
    "Netherlands Antilles",
    "Kosovo",
    "ECCU",
    "Euro Area",
    "Micronesia"
  ))

#I delete the column Country and I pu the column iso3 in first and i rename the column Year in year
NFAGDP_clean_2 <- NFAGDP_clean_2 %>%
  select(iso3, everything(), -Country) %>% 
  rename(year = Year) %>%
  rename(NFAGDP = `net IIP excl gold / GDP domestic currency` )

yearpanel = yearpanel %>% 
  left_join(NFAGDP_clean_2, by = c("iso3", "year"))

###GOvBalancetoGDP part ! 
GovBalance = read_xls("Data/IMF_Balance.xls")
GovBalance$iso3 <- countrycode(
  GovBalance$`General government net lending/borrowing (Percent of GDP)`,
  origin = "country.name",
  destination = "iso3c"
)

GovBalance = GovBalance %>% filter(!is.na(iso3)) %>%   rename(country = `General government net lending/borrowing (Percent of GDP)`)

GovBalance = GovBalance %>% select(-`Estimates start after`, -country) |>  # drop junk columns
  pivot_longer(
    cols      = -iso3,
    names_to  = "year",
    values_to = "gov_balance"
  ) |>
  mutate(
    gov_balance = na_if(gov_balance, "no data"),
    gov_balance = as.numeric(gov_balance),
    year        = as.integer(year)
  ) |>
  filter(!is.na(gov_balance))

### 5 Year Averages

yearpanel = yearpanel %>% 
  left_join(GovBalance, by = c("iso3", "year"))

GovBalance <- GovBalance |>
  filter(year >= 1971) |>
  mutate(period = (year - 1971) %/% 5) |>
  group_by(iso3, period) |>
  summarise(
    year = min(year)-((min(year)-1971)%%5),          # label each period by its first year
    GOVBGDP = mean(gov_balance,   na.rm = TRUE),
    .groups = "drop"
  ) |>
  select(-period)




#I create the mean for each period of 5 years 
NFAGDP_clean_2 <- NFAGDP_clean_2 |>
  filter(year >= 1971) |>
  mutate(period = (year - 1971) %/% 5) |>
  group_by(iso3, period) |>
  summarise(
    year = min(year)-((min(year)-1971)%%5),          # label each period by its first year
    NFAGDP = mean(NFAGDP,   na.rm = TRUE),
    .groups = "drop"
  ) |>
  select(-period)

#je merge la base de donnée NFAGDP_clean_2 avec panel_2
panel_3<- panel_2 %>%
  left_join(NFAGDP_clean_2, by = c("iso3", "year"))
###Merge with govbgdp

panel4 <- panel_3 %>% 
  left_join(GovBalance, by = c("iso3","year"))


####Alternative source for LREER



####Introducing Bruegel

bruegeldf = read_xlsx("Data/Bruegel_REER.xlsx")
bruegelcorr = read_csv("Data/Bruegel_REER_Correspondance.csv")

bruegeldf = bruegeldf %>% 
  rename(year = `Updated: 23 October 2025`) %>% 
  pivot_longer(
    cols      = -year,
    names_to  = "Country",
    values_to = "BRREER"
  ) %>% 
  mutate(`Country code` = stringr::str_extract(Country, "[A-Z]{2}$")) %>% 
  left_join(bruegelcorr, by = "Country code")

  bruegeldf$iso3 <- countrycode(
    bruegeldf$`Country name`,
    origin = "country.name",
    destination = "iso3c"
  )  

bruegeldf = bruegeldf %>% 
  select(c(year, iso3,BRREER)) %>% 
  mutate(BRREER = log(BRREER)) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year > 1970)

yearpanel = yearpanel %>% 
  left_join(bruegeldf, by = c("iso3", "year"))

bruegeldf = bruegeldf %>% 
  mutate(period = (year - 1971) %/% 5) %>% 
  group_by(iso3, period) |>
  summarise(
    year = min(year)-((min(year)-1971)%%5),          # label each period by its first year
    BRREER = mean(BRREER,   na.rm = TRUE),
    .groups = "drop"
  ) |>
  select(-period)

panel4 = panel4 %>% left_join(bruegeldf, by = c("iso3","year"))


####Public Finances in Modern History database use

  
public_exp = read_xls("Data/IMF_Expenditure.xls")
public_rev = read_xls("Data/IMF_Revenue.xls")

public_exp = public_exp %>% 
  rename(Country = `Government expenditure, percent of GDP (% of GDP)`) %>% 
  pivot_longer(
    cols      = -Country,
    names_to  = "year",
    values_to = "EXPGDP"
  ) %>% 
  filter(year > 1970 ) %>% 
  filter(EXPGDP != "no data") %>% 
  mutate(EXPGDP = as.numeric(EXPGDP))

public_rev = public_rev %>% 
  rename(Country = `Government revenue, percent of GDP (% of GDP)`) %>% 
  pivot_longer(
    cols      = -Country,
    names_to  = "year",
    values_to = "REVGDP"
  ) %>% 
  filter(year > 1970 ) %>% 
  filter(REVGDP != "no data") %>% 
  mutate(REVGDP = as.numeric(REVGDP)) 

public_rev = public_rev %>%
  left_join(public_exp, by = c("Country","year")) %>% 
  mutate(IMFGOVBGDP = REVGDP-EXPGDP) %>% 
  mutate(year = as.numeric(year))


public_rev$iso3 <- countrycode(
  public_rev$Country,
  origin = "country.name",
  destination = "iso3c"
)  
  
yearpanel = yearpanel %>% 
  left_join(public_rev, by = c("iso3", "year"))

public_rev = public_rev %>%
  mutate(period = (year - 1971) %/% 5) %>% 
  group_by(Country, period) |>
  summarise(
    year = min(year)-((min(year)-1971)%%5),          # label each period by its first year
    IMFGOVBGDP = mean(IMFGOVBGDP,   na.rm = TRUE),
    .groups = "drop"
  ) |>
  select(-period) 

public_rev$iso3 <- countrycode(
  public_rev$Country,
  origin = "country.name",
  destination = "iso3c"
)  

public_rev = public_rev %>% 
  select(-Country)

panel4 = panel4 %>% 
  left_join(public_rev, by = c("iso3","year"))

panel4 <- panel4 |>
  group_by(iso3) |>
  mutate(
    na_var1 = sum(is.na(IMFGOVBGDP)),
    na_var2 = sum(is.na(GOVBGDP)),
    CombinedGOVBGDP = ifelse(na_var1 <= na_var2, IMFGOVBGDP, GOVBGDP)
  ) |>
  select(-na_var1, -na_var2) |>
  ungroup()

#### TERMS OF TRADE INDIRECT

totsdf <- WDI(
  indicator = c(
    exp_current  = "NE.EXP.GNFS.CD",   # Exports current USD
    exp_constant = "NE.EXP.GNFS.KD",   # Exports constant USD
    imp_current  = "NE.IMP.GNFS.CD",   # Imports current USD
    imp_constant = "NE.IMP.GNFS.KD"    # Imports constant USD
  ),
  country = "all",
  start   = 1971,
  end     = 2026
) |>
  mutate(
    export_deflator = exp_current / exp_constant,
    import_deflator = imp_current / imp_constant,
    terms_of_trade  = (export_deflator / import_deflator) * 100
  ) |>
  select(iso3c, year, terms_of_trade) |>
  filter(!is.na(terms_of_trade)) %>% 
  rename(IndirectTOTS = terms_of_trade) %>% 
  rename(iso3 = iso3c)

yearpanel = yearpanel %>% 
  left_join(totsdf, by = c("iso3", "year"))


totsdf = totsdf %>% 
  mutate(period = (year - 1971) %/% 5) %>% 
  group_by(iso3, period) |>
  summarise(
    year = min(year)-((min(year)-1971)%%5),          # label each period by its first year
    IndirectTOTS = sd(IndirectTOTS,   na.rm = TRUE),
    .groups = "drop"
  ) |>
  select(-period) 

panel4 = panel4 %>% 
  left_join(totsdf, by = c("iso3","year")) 
  

panel4 <- panel4 |>
  group_by(iso3) |>
  mutate(
    na_var1 = sum(is.na(TOTSD)),
    na_var2 = sum(is.na(IndirectTOTS)),
    CombinedTOTSD = ifelse(na_var1 < na_var2, TOTSD, IndirectTOTS)
  ) |>
  select(-na_var1, -na_var2) |>
  ungroup()



###Penn World Tables source for relative income

library(pwt10)
library(dplyr)

data("pwt10.01")

# GDP per capita = rgdpe (PPP-adjusted) / pop
df_pwt <- pwt10.01 |>
  mutate(gdppc_ppp = rgdpe / pop) |>
  select(country, isocode, year, gdppc_ppp)

# Get US values to compute relative income
us_gdppc <- df_pwt |>
  filter(isocode == "USA") |>
  select(year, us_gdppc = gdppc_ppp)

# Compute relative income (0 to 1], US = 1
df_rel_income <- df_pwt |>
  left_join(us_gdppc, by = "year") |>
  mutate(PennRELY = gdppc_ppp / us_gdppc) |>
  filter(year >= 1971) |>
  select(isocode, year, PennRELY) |>
  rename(iso3 = isocode) %>% 
  mutate(PennRELY = ifelse(PennRELY > 1, 1, PennRELY))
  
yearpanel = yearpanel %>% 
  left_join(df_rel_income, by = c("iso3", "year"))

df_rel_income = df_rel_income %>%  
  mutate(period = (year - 1971) %/% 5) %>% 
  group_by(iso3, period) |>
  summarise(
    year = min(year)-((min(year)-1971)%%5),          # label each period by its first year
    PennRELY = mean(PennRELY,   na.rm = TRUE),
    .groups = "drop"
  ) |>
  select(-period) 

panel4 = panel4 %>% 
  left_join(df_rel_income, by = c("iso3","year"))


####WEO Savings

weosavings = read.csv("Data/Savings_WEO.csv")

weosavings$iso3 <- countrycode(
  weosavings$COUNTRY,
  origin = "country.name",
  destination = "iso3c"
) 

weosavings = weosavings %>% 
  select(c("iso3","TIME_PERIOD","OBS_VALUE")) %>% 
  rename(year = TIME_PERIOD) %>% 
  rename(WEONSGDP = OBS_VALUE) 

yearpanel = yearpanel %>% 
  left_join(weosavings, by = c("iso3", "year"))

weosavings = weosavings %>% 
  mutate(period = (year - 1971) %/% 5) %>% 
  group_by(iso3, period) |>
  summarise(
    year    = min(year)-((min(year)-1971)%%5),          # label each period by its first year
    WEONSGDP = sd(WEONSGDP,   na.rm = TRUE),
    .groups = "drop"
  ) |>
  select(-period) 

panel4 = panel4 %>% 
  left_join(weosavings, by = c("iso3","year"))
  
  
###Keep the best savings series

panel4 <- panel4 |>
  group_by(iso3) |>
  mutate(
    na_var1 = sum(is.na(NSGDP[year <= 1995 & year >= 1971])),
    na_var2 = sum(is.na(WEONSGDP[year <= 1995 & year >= 1971])),
    COMBINEDNSGDP = ifelse(na_var1 < na_var2, NSGDP, WEONSGDP)
  ) |>
  select(-na_var1, -na_var2) |>
  ungroup()

###Compile everything
colMeans(is.na(panel4))
write.csv(panel4,"Data/5yearPanel.csv")

write.csv(yearpanel,"Data/AnnualPanel.csv")


