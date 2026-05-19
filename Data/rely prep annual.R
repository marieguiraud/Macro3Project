# Installer si nécessaire
library(pwt10)
library(tidyverse)
library(stargazer)
library(plm)
library(xtable)
library(zoo)
library(lmtest)
library(sandwich)
# Extraire gdppc depuis PWT
pwt_data <- as.data.frame(pwt10.0) %>%
  select(isocode, year, rgdpna, pop) %>%
  filter(!is.na(rgdpna), !is.na(pop)) %>%
  mutate(gdppc = rgdpna / pop) %>%
  rename(iso3 = isocode)

# Construire RELY = gdppc pays / gdppc USA
pwt_data <- pwt_data %>%
  group_by(year) %>%
  mutate(
    RELY  = gdppc / gdppc[iso3 == "USA"],
    RELY2 = RELY^2
  ) %>%
  ungroup() %>%
  select(iso3, year, RELY, RELY2)

# Retirer l'ancienne RELY et merger la nouvelle
annual <- annual %>%
  select(-RELY, -RELY2) %>%
  left_join(pwt_data, by = c("iso3", "year"))

# Vérifier la couverture
cat("NAs dans RELY après merge:", sum(is.na(annual$RELY[annual$iso3 %in% all_countries])), "\n")