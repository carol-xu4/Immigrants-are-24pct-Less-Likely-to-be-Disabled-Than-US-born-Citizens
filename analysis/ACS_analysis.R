## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr)

setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Immigrants are 24pct Less Likely to be Disabled Than US-Born Citizens")

# read in cleaned data ------------------------------------------------------
acs = fread("data/output/acs_disability.csv") %>% select(-good, -slegal, -good1, -hlegal)

colors = c(
  "Native-born citizens"= "#3043B4",
  "Legal immigrants"    = "#7C756D",
  "Illegal immigrants"  = "#C97703")

colors_4 = c(
  "Native-born citizens" = "#3043B4",
  "Legal immigrants"     = "#7C756D",
  "Illegal immigrants"   = "#C97703",
  "All immigrants"       = "#0D0E51")

linetypes_4 = c(
  "Native-born citizens" = "solid",
  "Legal immigrants"     = "solid",
  "Illegal immigrants"   = "solid",
  "All immigrants"       = "dotted")

# Total immigrant population in ACS
immig_counts = acs %>%
  group_by(year, immig_status) %>%
  summarise(
    n = n(),
    population = sum(perwt, na.rm = TRUE)) %>%
  ungroup()

print(immig_counts, n = Inf)

write_csv(immig_counts, "results/ACS_immig_counts_year.csv")

# Table 1: Disability rates among US-born citizens and immigrants, 2024
disab_all_2024 = acs %>%
  filter(year == 2024) %>%
  mutate(
    diffany = case_when(
      diffhear == 2 | diffeye == 2 | diffrem == 2 | 
        diffphys == 2 | diffcare == 2 | diffmob == 2 ~ 2,
      diffhear == 1 | diffeye == 1 | diffrem == 1 | 
        diffphys == 1 | diffcare == 1 | diffmob == 1 ~ 1,
      TRUE ~ NA_real_))

disab_rates_all_2024 = disab_all_2024 %>%
  mutate(disabled = diffany == 2) %>%
  group_by(immig_status) %>%
  summarise(
    disabled = sum(perwt[disabled], na.rm = TRUE),
    population = sum(perwt, na.rm = TRUE),
    pct = disabled / population * 100, 
    .groups = "drop")

print(disab_rates_all_2024)

disab_rates_all_2024_allimm = disab_all_2024 %>%
  filter(immig_status %in% c("Legal immigrants", "Illegal immigrants")) %>%        # creating "all immigrants" row
  mutate(disabled = diffany == 2) %>%
  summarise(
    immig_status = "All immigrants",
    disabled = sum(perwt[disabled], na.rm = TRUE),
    population = sum(perwt, na.rm = TRUE),
    pct = disabled / population * 100)

disab_rates_all_2024_allimm = disab_all_2024 %>%
  filter(immig_status %in% c("Legal immigrants", "Illegal immigrants")) %>%
  mutate(disabled = diffany == 2) %>%
  summarise(
    immig_status = "All immigrants",
    disabled = sum(perwt[disabled], na.rm = TRUE),
    population = sum(perwt, na.rm = TRUE),
    pct = disabled / population * 100)

disab_rates_all_2024 = bind_rows(disab_rates_all_2024, disab_rates_all_2024_allimm) %>%
  mutate(immig_status = factor(immig_status, levels = c(
    "Native-born citizens", "Legal immigrants", "Illegal immigrants", "All immigrants")))

print(disab_rates_all_2024)

write_csv(disab_rates_all_2024, "results/table1.csv")

# Fig. 1: Disability rates by immigration status over time, 2014-2024
disab_all = acs %>%
  mutate(
    diffany = case_when(
  diffhear == 2 | diffeye == 2 | diffrem == 2 | 
    diffphys == 2 | diffcare == 2 | diffmob == 2 ~ 2,
  diffhear == 1 | diffeye == 1 | diffrem == 1 | 
    diffphys == 1 | diffcare == 1 | diffmob == 1 ~ 1,
  TRUE ~ NA_real_))

disabled_rates_all = disab_all %>%
  mutate(disabled = diffany == 2) %>%
  group_by(year, immig_status) %>%
  summarise(
    disabled = sum(perwt[disabled], na.rm = TRUE),
    population = sum(perwt, na.rm = TRUE),
    pct = disabled / population * 100, 
    .groups = "drop")

print(disabled_rates_all, n = Inf)

disabled_rates_all_allimm = disab_all %>%
  filter(immig_status %in% c("Legal immigrants", "Illegal immigrants")) %>%       # creating "all immigrants" row
  mutate(disabled = diffany == 2) %>%
  group_by(year) %>%
  summarise(
    immig_status = "All immigrants",
    disabled = sum(perwt[disabled], na.rm = TRUE),
    population = sum(perwt, na.rm = TRUE),
    pct = disabled / population * 100,
    .groups = "drop")

disabled_rates_all = bind_rows(disabled_rates_all, disabled_rates_all_allimm) %>%
  mutate(immig_status = factor(immig_status, levels = c(
    "Native-born citizens", "Legal immigrants", "Illegal immigrants", "All immigrants")))

print(disabled_rates_all, n = Inf)
  
ggplot(disabled_rates_all, aes(x = as.numeric(year), y = pct, color = immig_status, linetype = immig_status)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 2) +
  scale_color_manual(values = colors_4) +
  scale_linetype_manual(values = linetypes_4) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = c(0.02, 0), limits = c(0, 15)) +
  labs(
    title = "Disability Rate by Immigration Status (2010-2024)",
    subtitle = "ACS;  \n Any difficulty: cognitive, ambulatory, independent living, self-care, vision, hearing",
    x = NULL,
    y = NULL,
    color = NULL,
    linetype = NULL,
    caption = "Source: ACS via IPUMS") +
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

ggsave("results/fig.1_acs_disabled_rates_all.png", width = 15, height = 10)

# Fig. 2 (RAW): Immigrants are les likely to be disabled at every age group
disabled_all_by_age_2024_65p = disab_all_2024 %>%
  mutate(disabled = diffany == 2,
         age_pooled = pmin(age, 65)) %>%            # pool age 65+
  group_by(age_pooled, immig_status) %>%
  summarise(
    disabled = sum(perwt[disabled], na.rm = TRUE),
    population = sum(perwt, na.rm = TRUE),
    pct = disabled / population * 100,
    .groups = "drop")

disabled_all_by_age_2024_65p_allimm = disab_all_2024 %>%
  filter(immig_status %in% c("Legal immigrants", "Illegal immigrants")) %>%
  mutate(disabled = diffany == 2,
         age_pooled = pmin(age, 65)) %>%
  group_by(age_pooled) %>%
  summarise(
    immig_status = "All immigrants",
    disabled = sum(perwt[disabled], na.rm = TRUE),
    population = sum(perwt, na.rm = TRUE),
    pct = disabled / population * 100,
    .groups = "drop")

disabled_all_by_age_2024_65p = bind_rows(disabled_all_by_age_2024_65p, disabled_all_by_age_2024_65p_allimm) %>%
  mutate(immig_status = factor(immig_status, levels = c(
    "Native-born citizens", "Legal immigrants", "Illegal immigrants", "All immigrants")))

print(disabled_all_by_age_2024_65p, n = Inf)

ggplot(disabled_all_by_age_2024_65p, aes(x = age_pooled, y = pct, color = immig_status, linetype = immig_status)) +
  geom_line(linewidth = 1.8) +
  scale_color_manual(values = colors_4) +
  scale_linetype_manual(values = linetypes_4) +
  scale_x_continuous(breaks = seq(15, 100, by = 10), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = c(0.02, 0)) +
  labs(
    title = "Non-GQ Disability Rate by Age and Immigration Status (2024)",
    subtitle = "ACS; age 15+; \n Any difficulty: cognitive, ambulatory, independent living, self-care, vision, hearing",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: ACS via IPUMS") +
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

ggsave("results/acs_disabled_all_by_age_2024_RAW.png", widt = 15, height = 10)

# Fig. 2 (LOESS)
ggplot(disabled_all_by_age_2024_65p, aes(x = age_pooled, y = pct, color = immig_status, linetype = immig_status)) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.8, span = 0.3) +
  scale_color_manual(values = colors_4) +
  scale_linetype_manual(values = linetypes_4) +
  scale_x_continuous(breaks = seq(0, 65, by = 10), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = c(0.02, 0), limits = c(0, 35)) +
  labs(
    title = "Disability Rate by Age and Immigration Status, all persons (2024)",
    subtitle = "ACS; Age 65+ pooled \n Any difficulty: cognitive, ambulatory, independent living, self-care, vision, hearing",
    x = NULL,
    y = NULL,
    color = NULL,
    linetype = NULL,
    caption = "Source: ACS via IPUMS") +
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

ggsave("results/fig.2_acs_disabled_all_by_age_2024_smooth.png", width = 15, height = 10)

# veterans
# vets by immig status (vetstat universe is persons age 17+)
vets2024 = disab_all %>%
  filter(vetstat == 2, year == 2024) %>%
  group_by(year, immig_status) %>%
  summarise(n = n(),
            population = sum(perwt, na.rm = TRUE),
            .groups = "drop")

print(vets2024)

# % of veterans who are disabled, US-born vs. legal immigrants
disabled_vets_pop = disab_all %>%
  filter(vetstat == 2) %>%
  filter(diffany %in% c(1, 2)) %>%
  mutate(disabled = diffany == 2) %>%
  group_by(year, immig_status) %>%
  summarise(
    disabled = sum(perwt[disabled], na.rm = TRUE),
    population = sum(perwt, na.rm = TRUE),
    pct = disabled / population * 100,
    .groups = "drop")

print(disabled_vets_pop, n = Inf)

# no veterans, disabled by age, 2024
disab_all_2024_novets = disab_all_2024 %>%
  filter(age >= 18) %>%
  filter(vetstat != 2)

disabled_novets_by_age_2024_65p = disab_all_2024_novets %>%
  mutate(disabled = diffany == 2,
         age_pooled = pmin(age, 65)) %>%
  group_by(age_pooled, immig_status) %>%
  summarise(
    disabled = sum(perwt[disabled], na.rm = TRUE),
    population = sum(perwt, na.rm = TRUE),
    pct = disabled / population * 100,
    .groups = "drop")

disabled_novets_by_age_2024_65p_allimm = disab_all_2024_novets %>%
  filter(immig_status %in% c("Legal immigrants", "Illegal immigrants")) %>%
  mutate(disabled = diffany == 2,
         age_pooled = pmin(age, 65)) %>%
  group_by(age_pooled) %>%
  summarise(
    immig_status = "All immigrants",
    disabled = sum(perwt[disabled], na.rm = TRUE),
    population = sum(perwt, na.rm = TRUE),
    pct = disabled / population * 100,
    .groups = "drop")

disabled_novets_by_age_2024_65p = bind_rows(disabled_novets_by_age_2024_65p, disabled_novets_by_age_2024_65p_allimm) %>%
  mutate(immig_status = factor(immig_status, levels = c(
    "Native-born citizens", "Legal immigrants", "Illegal immigrants", "All immigrants")))

print(disabled_novets_by_age_2024_65p, n = Inf)

ggplot(disabled_novets_by_age_2024_65p, aes(x = age_pooled, y = pct, color = immig_status, linetype = immig_status)) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.8, span = 0.3) +
  scale_color_manual(values = colors_4) +
  scale_linetype_manual(values = linetypes_4) +
  scale_x_continuous(breaks = seq(20, 65, by = 10), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = c(0.02, 0), limits = c(0, 35)) +
  labs(
    title = "Disability Rate by Age and Immigration Status, Non-Veterans (2024)",
    subtitle = "ACS; age 18+, excluding veterans; Age 65+ pooled \n Any difficulty: cognitive, ambulatory, independent living, self-care, vision, hearing",
    x = NULL,
    y = NULL,
    color = NULL,
    linetype = NULL,
    caption = "Source: ACS via IPUMS") +
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

ggsave("results/acs_disabled_novets_by_age_2024_65plus_loess.png", width = 15, height = 10)
