# ============================================================
# FIGURES 1-4 — Reproduction des graphiques de Chinn-Prasad
# ============================================================
install.packages("gridExtra")
library(gridExtra)
library(ggplot2)
library(dplyr)
library(tidyr)

panel1<-read.csv("Data/panel_3.csv")
panel_1995 <- panel1%>%
  filter(year <= 1995) %>% 
  mutate(PennRELY2=PennRELY^2, RELY2 = RELY^2)

# ── Figure 1 : CA vs NFA initial (cross-section) ─────────────────────────────

# NFA initial = moyenne des 5 premières années par pays
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

# ── Figure 2 : CA vs revenu relatif (cross-section) ──────────────────────────

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

# Résidus après partialling out les autres régresseurs
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

# ── Figure 1 : CA vs NFA initial (cross-section) ─────────────────────────────

# NFA initial = moyenne des 5 premières années par pays
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

# ── Figure 2 : CA vs revenu relatif (cross-section) ──────────────────────────

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

# ── Figure 3 : Actual vs Fitted CA pour les pays en développement ─────────────

# Régression avec et sans dummies temporelles
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

# ── Figure 4 : Partial scatterplots ──────────────────────────────────────────

# Résidus après partialling out les autres régresseurs
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
