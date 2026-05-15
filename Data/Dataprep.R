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

panel <- wdi_clean |>
  filter(year >= 1971) |>
  mutate(period = (year - 1971) %/% 5) |>
  group_by(iso3, period) |>
  summarise(
    year    = min(year),          # label each period by its first year
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

#I clean for data from 71 to today and i compute a mean for period of 5 years
ka_open_clean <- ka_open |>
  filter(year >= 1971) |>
  mutate(period = (year - 1971) %/% 5) |>
  group_by(iso3, period) |>
  summarise(
    year    = min(year),          # label each period by its first year
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




#I create the mean for each period of 5 years 
NFAGDP_clean_2 <- NFAGDP_clean_2 |>
  filter(year >= 1971) |>
  mutate(period = (year - 1971) %/% 5) |>
  group_by(iso3, period) |>
  summarise(
    year    = min(year),          # label each period by its first year
    NFAGDP = mean(NFAGDP,   na.rm = TRUE),
    .groups = "drop"
  ) |>
  select(-period)

#je merge la base de donnée NFAGDP_clean_2 avec panel_2
panel_3<- panel_2 %>%
  left_join(NFAGDP_clean_2, by = c("iso3", "year"))


