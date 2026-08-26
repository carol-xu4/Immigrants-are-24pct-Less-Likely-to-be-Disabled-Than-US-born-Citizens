## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, matrixStats)

setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Immigrants are 24pct Less Likely to be Disabled Than US-Born Citizens")

# read in cleaned data ------------------------------------------------------
cps = fread("data/output/cpsdata_disability.csv")

colors = c(
  "Native-born citizens"= "#3043B4",
  "Legal immigrants"    = "#7C756D",
  "Illegal immigrants"  = "#C97703")

# total immigrant population in ASEC
CPS_immig_counts = cps %>%
  group_by(year, immig_status) %>%
  summarise(
    n = n(),
    population = sum(asecwt, na.rm = TRUE)) %>%
  ungroup()

write_csv(CPS_immig_counts, "results/CPS_immig_counts_year.csv")

# disabled populations (diffany universe: civilians age 15+)
disabled_pop = cps %>%
  filter(diffany %in% c(1, 2)) %>%
  mutate(disabled = diffany == 2) %>%
  group_by(year, immig_status) %>%
  summarise(
    disabled = sum(asecwt[disabled], na.rm = TRUE),
    population = sum(asecwt, na.rm = TRUE),
    pct = disabled / population * 100, 
    .groups = "drop")

print(disabled_pop, n = Inf)

ggplot(disabled_pop, aes(x = as.numeric(year), y = pct, color = immig_status)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 3) +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = c(0.02, 0), limits = c(0, 15)) +
  labs(
    title = "Disability Rate by Immigration Status (2010-2025)",
    subtitle = "CPS ASEC; Civilians ages 15+; \n Any difficulty: hearing, vision, remembering, physical, disability limiting mobility, personal care limitation",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: CPS ASEC via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 20),
    legend.key.width = unit(1.5, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 25, color = "gray40"),
    axis.text.y = element_text(size = 25, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/CPS_disabled_pop.png", width = 15, height = 10)

# Fig. 3: Share of US-born citizens and immigrants receiving disability income, 2024
disability_benefit_pop_2025 = cps %>%
  filter(diffany %in% c(1, 2)) %>%
  filter(year == 2025) %>%
  mutate(
    ss_disability          = (whyss1 == 2 | whyss2 == 2) & incss > 0 & incss < 99999,
    ssi_disability         = (whyssi1 %in% c(1, 2) | whyssi2 %in% c(1, 2)) & incssi > 0 & incssi < 99999,
    other_disability       = incdisab > 0 & incdisab < 9999999,
    any_disability_benefit = ss_disability | ssi_disability | other_disability
  ) %>%
  group_by(immig_status) %>%
  summarise(
    n = n(),
    population = sum(asecwt, na.rm = TRUE),
    pct_ss    = sum(asecwt[ss_disability], na.rm = TRUE)          / population * 100,
    pct_ssi   = sum(asecwt[ssi_disability], na.rm = TRUE)         / population * 100,
    pct_other = sum(asecwt[other_disability], na.rm = TRUE)       / population * 100,
    pct_any   = sum(asecwt[any_disability_benefit], na.rm = TRUE) / population * 100,
    .groups = "drop")

disability_benefit_pop_2025_allimm = cps %>%             # creating "all immigrnats" row
  filter(diffany %in% c(1, 2)) %>%
  filter(year == 2025) %>%
  filter(immig_status %in% c("Legal immigrants", "Illegal immigrants")) %>%
  mutate(
    ss_disability          = (whyss1 == 2 | whyss2 == 2) & incss > 0 & incss < 99999,
    ssi_disability         = (whyssi1 %in% c(1, 2) | whyssi2 %in% c(1, 2)) & incssi > 0 & incssi < 99999,
    other_disability       = incdisab > 0 & incdisab < 9999999,
    any_disability_benefit = ss_disability | ssi_disability | other_disability
  ) %>%
  summarise(
    immig_status = "All immigrants",
    n = n(),
    population = sum(asecwt, na.rm = TRUE),
    pct_ss    = sum(asecwt[ss_disability], na.rm = TRUE)          / population * 100,
    pct_ssi   = sum(asecwt[ssi_disability], na.rm = TRUE)         / population * 100,
    pct_other = sum(asecwt[other_disability], na.rm = TRUE)       / population * 100,
    pct_any   = sum(asecwt[any_disability_benefit], na.rm = TRUE) / population * 100)

disability_benefit_pop_2025 = bind_rows(disability_benefit_pop_2025, disability_benefit_pop_2025_allimm) %>%
  mutate(immig_status = factor(immig_status, levels = c(
    "Native-born citizens", "Legal immigrants", "Illegal immigrants", "All immigrants")))

print(disability_benefit_pop_2025)

write_csv(disability_benefit_pop_2025, "results/fig.3.csv")

# Fig. 4: Immigrants are less likely to receive government disability income
full_disability_income_2025 = cps %>%
  filter(diffany %in% c(1, 2)) %>%
  filter(incdisab >= 0, incdisab < 999999) %>%
  filter(year == 2025) %>%
  mutate(
    disabled = diffany == 2,
    ss_disability     = (whyss1 == 2 | whyss2 == 2) & incss > 0,
    ssi_disability    = (whyssi1 %in% c(1, 2) | whyssi2 %in% c(1, 2)) & incssi > 0,
    other_disability  = incdisab > 0,
    any_disability_income = ss_disability | ssi_disability | other_disability
  ) %>%
  filter(disabled) %>%
  group_by(immig_status) %>%
  summarise(
    n = n(),
    population = sum(asecwt, na.rm = TRUE),
    pct_ss    = sum(asecwt[ss_disability], na.rm = TRUE) / population * 100,
    pct_ssi   = sum(asecwt[ssi_disability], na.rm = TRUE) / population * 100,
    pct_other = sum(asecwt[other_disability], na.rm = TRUE) / population * 100,
    pct_any   = sum(asecwt[any_disability_income], na.rm = TRUE) / population * 100,
    .groups = "drop")

full_disability_income_2025_allimm = cps %>%
  filter(diffany %in% c(1, 2)) %>%
  filter(incdisab >= 0, incdisab < 999999) %>%
  filter(year == 2025) %>%
  filter(immig_status %in% c("Legal immigrants", "Illegal immigrants")) %>%
  mutate(
    disabled = diffany == 2,
    ss_disability     = (whyss1 == 2 | whyss2 == 2) & incss > 0,
    ssi_disability    = (whyssi1 %in% c(1, 2) | whyssi2 %in% c(1, 2)) & incssi > 0,
    other_disability  = incdisab > 0,
    any_disability_income = ss_disability | ssi_disability | other_disability
  ) %>%
  filter(disabled) %>%
  summarise(
    immig_status = "All immigrants",
    n = n(),
    population = sum(asecwt, na.rm = TRUE),
    pct_ss    = sum(asecwt[ss_disability], na.rm = TRUE) / population * 100,
    pct_ssi   = sum(asecwt[ssi_disability], na.rm = TRUE) / population * 100,
    pct_other = sum(asecwt[other_disability], na.rm = TRUE) / population * 100,
    pct_any   = sum(asecwt[any_disability_income], na.rm = TRUE) / population * 100)

full_disability_income_2025 = bind_rows(full_disability_income_2025, full_disability_income_2025_allimm) %>%
  mutate(immig_status = factor(immig_status, levels = c(
    "Native-born citizens", "Legal immigrants", "Illegal immigrants", "All immigrants")))

print(full_disability_income_2025)

write_csv(full_disability_income_2025, "results/fig.4.csv")

# Fig. 5: Per capita disability benefit consumption, 2024
cps_disability_percapita_2025 = cps %>%
  filter(year == 2025) %>%
  filter(diffany %in% c(1, 2)) %>%
  mutate(
    disabled        = diffany == 2,
    ss_disability   = (whyss1 == 2 | whyss2 == 2) & incss > 0 & incss < 99999,
    ssi_disability  = (whyssi1 %in% c(1, 2) | whyssi2 %in% c(1, 2)) & incssi > 0 & incssi < 99999,
    incss_clean     = ifelse(ss_disability,  incss,  0),
    incssi_clean    = ifelse(ssi_disability, incssi, 0),
    incdisab_clean  = ifelse(incdisab > 0 & incdisab < 9999999, incdisab, 0)) %>%
  group_by(immig_status) %>%
  summarise(
    n = n(),
    population = sum(asecwt, na.rm = TRUE),
    disab_population = sum(asecwt[disabled], na.rm = TRUE),
    percapita_ss     = sum(asecwt * incss_clean, na.rm = TRUE)    / population,
    percapita_ssi    = sum(asecwt * incssi_clean, na.rm = TRUE)   / population,
    percapita_other  = sum(asecwt * incdisab_clean, na.rm = TRUE) / population,
    percapita_any    = percapita_ss + percapita_ssi + percapita_other,
    .groups = "drop")

cps_disability_percapita_2025_allimm = cps %>%
  filter(year == 2025) %>%
  filter(diffany %in% c(1, 2)) %>%
  filter(immig_status %in% c("Legal immigrants", "Illegal immigrants")) %>%
  mutate(
    disabled        = diffany == 2,
    ss_disability   = (whyss1 == 2 | whyss2 == 2) & incss > 0 & incss < 99999,
    ssi_disability  = (whyssi1 %in% c(1, 2) | whyssi2 %in% c(1, 2)) & incssi > 0 & incssi < 99999,
    incss_clean     = ifelse(ss_disability,  incss,  0),
    incssi_clean    = ifelse(ssi_disability, incssi, 0),
    incdisab_clean  = ifelse(incdisab > 0 & incdisab < 9999999, incdisab, 0)) %>%
  summarise(
    immig_status = "All immigrants",
    n = n(),
    population = sum(asecwt, na.rm = TRUE),
    disab_population = sum(asecwt[disabled], na.rm = TRUE),
    percapita_ss     = sum(asecwt * incss_clean, na.rm = TRUE)    / population,
    percapita_ssi    = sum(asecwt * incssi_clean, na.rm = TRUE)   / population,
    percapita_other  = sum(asecwt * incdisab_clean, na.rm = TRUE) / population,
    percapita_any    = percapita_ss + percapita_ssi + percapita_other)

cps_disability_percapita_2025 = bind_rows(cps_disability_percapita_2025, cps_disability_percapita_2025_allimm)

print(cps_disability_percapita_2025, width = Inf)

write_csv(cps_disability_percapita_2025, "results/fig.5.csv")
