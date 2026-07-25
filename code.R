# Goal -----
# The goal of this script to was to inspect the accompanying dataset for Cobb-Lewis et al. bioRxiv 2014
# as part of the FORCE11 PREreview Club 
# Dataset are imported directly from Zenodo:https://doi.org/10.5281/zenodo.14262739


# Load libraries ----
library(tidyverse)


# Compliance data ----
## Import data from Zenodo ----
compliance_df <- read_csv(file = "https://zenodo.org/records/14262739/files/compliance%20data.csv")


## Inspect data ----
glimpse(compliance_df)
## README specify that there are 1008 rows. However, this is only 943 rows in this csv file

## materials_reuse_denom, protocol_shared, and protocol_denom are repeated
## test if data in these columns are the same
all.equal(compliance_df$materials_reuse_denom...17, compliance_df$materials_reuse_denom...20)
all.equal(compliance_df$protocol_shared...18, compliance_df$protocol_shared...21)
all.equal(compliance_df$protocol_denom...19, compliance_df$protocol_denom...22)
## so we can safely remove the last three columns from further analysis


## Reproduce analysis ----
## according to READM, this dataset was used to "calculate change in compliance with the ASAP publication policy"
## presumably, Table 5 in the preprint
## the following analysis is based on the formula in the Calculations.xlsx

compliance_summary <- compliance_df[ , -c(20:22)] %>% 
  rename_with(~ str_remove(., "\\.{3}\\d+$")) %>%
  ## contrain dataset based on functions in the Calculation.xlsx
  filter(str_detect(status, "4_"),
         life_cycle == "D",
         date_ds1 %in% c("2021", "2022", "2023", "2024")) %>%
  ## convert columns with stats about different inputs/outputs into numeric
  mutate(across(ends_with("_shared"), as.numeric)) %>%
  mutate(across(ends_with("_denom"), as.numeric)) %>%
  ## recode variables of interest can calculate sums for each publication
  mutate(version_recoded = if_else(version == 1, "ver1", "ver2more"),
         total_shared = rowSums(pick(ends_with("_shared")), na.rm = TRUE),
         total_denom = rowSums(pick(ends_with("_denom")), na.rm = TRUE)) %>%
  select(date_ds1, version_recoded, total_shared, total_denom, ends_with("shared"), ends_with("denom")) 
## note, there are only 118 rows left in compliance_summary after the filtering


## calculate for each year and verion numbers (=1 or >1)
compliance_summary %>%
  group_by(date_ds1, version_recoded) %>%
  summarise(sum_shared = sum(total_shared),
            sum_denom = sum(total_denom)) %>%
  ungroup() %>%
  mutate(percent = sum_shared / sum_denom * 100) %>%
  select(-starts_with("sum")) %>%
  pivot_wider(id_cols = "date_ds1", names_from = "version_recoded", values_from = "percent")


## calculate all years
compliance_summary %>%
  group_by(version_recoded) %>%
  summarise(sum_shared = sum(total_shared),
            sum_denom = sum(total_denom)) %>%
  ungroup() %>%
  mutate(percent = sum_shared / sum_denom * 100)

## Results from this analyses match what's shown in Table 5 of the preprint and Calculations.xlsx



# Figure 4.csv (APC vs Impact Factor) ----
## this is actually figure 1 in the preprint ;D

## Import and inspect data ----
fig4_df <- read_csv(file = "https://zenodo.org/records/14262739/files/Figure%204.csv",
                    name_repair = "universal")

glimpse(fig4_df)
## 69 rows, matching README file 


## Graph data (aka, reproducing Figure 1) ----
ggplot(data = fig4_df,
       mapping = aes(x = Impact.Factor, y = APC.Cost)) +
  geom_point(shape = 21, size = 2,
             color = "darkslateblue", fill = "deepskyblue", alpha = 0.5) +
  theme_bw() +
  labs(title = "Association of journal impact factor and APC costs",
       subtitle = "(using data from Cobb-Lewis et al. Zenodo [Dataset]. 2024)",
       x = "Impact Factor",
       y = "APC Cost")

ggsave(filename = "figure-apc-vs-impact-factor.png", 
       width = 6, height = 4, unit = "in", dpi = 300)

## Hmm... this looks quite different from Fig 1 in the manuscript


ggplot(data = fig4_df %>% mutate(row_no = row_number()),
       mapping = aes(x = row_no, y = APC.Cost)) +
  geom_point(shape = 21, size = 2,
             color = "brown", fill = "salmon", alpha = 0.5) +
  scale_x_continuous(expand = 0.01) +
  scale_y_continuous(limits = c(0, 15000), expand = 0) +
  theme_bw() +
  labs(title = "Association of journal impact factor and APC costs | TEST",
       subtitle = "(using data from Cobb-Lewis et al. Zenodo [Dataset]. 2024)",
       x = "Row Number",
       y = "APC Cost")

ggsave(filename = "figure-apc-vs-row-number.png", 
       width = 6, height = 4, unit = "in", dpi = 300)

## So.... it looks like Figure 1 in the preprint is actually plotting APC vs row_number()



# Table 7.csv (APC payment) ----
## should be Table 4 of the preprint...

## Import and inspect data ----
table7_df <- read_csv(file = "https://zenodo.org/records/14262739/files/Table%207.csv",
                      name_repair = "universal")

glimpse(table7_df)
## 141 rows...


## Reproduce calculations -----
table7_df %>%
  group_by(APC.Date) %>%
  summarise(no_publications = n(),
            APC_mean = mean(APC.Cost),
            APC_sd = sd(APC.Cost),
            APC_Total = sum(APC.Cost))
## Matching what's reported in Table 4 of the preprint

