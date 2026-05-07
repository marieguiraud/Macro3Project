library(WDI)
library(tidyverse)

# ── 1. Pull all WDI-available Chinn-Prasad variables ────────────────────────

wdi_raw <- WDI(
  indicator = c(
    "BN.CAB.XOKA.GD.ZS",   # CAGDP
    "NY.GDP.PCAP.PP.CD",   # RELY (raw, needs transformation)
    "SP.POP.DPND.YG",      # RELDEPY (raw, needs normalization)
    "SP.POP.DPND.OL",      # RELDEPO (raw, needs normalization)
    "NY.GDP.MKTP.KD.ZG",   # YGRAVG + YGRSD (same series, both derived)
    "PX.REX.REER",         # LREER (needs log)
    "NE.TRD.GNFS.ZS",      # OPEN
    "FM.LBL.BMNY.GD.ZS",   # FDEEP
    "NY.GNS.ICTR.ZS",       # NSGDP
    "TT.PRI.MRCH.XD.WD"
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
         gdpgr, LREER, OPEN, FDEEP, NSGDP)


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
    .groups = "drop"
  ) |>
  select(-period)