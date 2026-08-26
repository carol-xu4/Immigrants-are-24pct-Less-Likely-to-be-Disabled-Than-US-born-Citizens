if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr)

setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Immigrant Health Coverage 2010-2024")

acs3 = fread("data/output/acsdata3.csv") %>% select(-good, -slegal, -good1, -hlegal)

colors = c(
  "Native-born citizens"= "#3043B4",
  "Legal immigrants"    = "#7C756D",
  "Illegal immigrants"  = "#C97703")

# Table 1: Disability rates among US-born citizens and immigrants, 2024

# Fig. 1: Disability rates by immigration status, 2014-2024

# Fig. 2 (RAW): Immigrants are les likely to be disabled at every age group

# Fig. 2 (LOESS)