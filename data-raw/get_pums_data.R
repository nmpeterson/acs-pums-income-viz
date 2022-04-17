library(tidyverse)
library(tidycensus)

# STATES <- c('ID', 'MT')
STATES <- c('AK', 'AL', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'FL', 'GA',
            'HI', 'ID', 'IL', 'IN', 'IA', 'KS', 'KY', 'LA', 'ME', 'MD',
            'MA', 'MI', 'MN', 'MS', 'MO', 'MT', 'NE', 'NV', 'NH', 'NJ',
            'NM', 'NY', 'NC', 'ND', 'OH', 'OK', 'OR', 'PA', 'RI', 'SC',
            'SD', 'TN', 'TX', 'UT', 'VT', 'VA', 'WA', 'WV', 'WI', 'WY')

# Specify the PUMS variables of interest. Data dictionary:
# https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2016-2020.pdf
PUMS_VARS <- c(
  "ST", # State abbreviation
  "WAGP", # Wage or salary income, past 12 months
  "ADJINC", # Income adjustment factor (to convert to 2020$)
  "OCCP", # Occupation recode for 2018 and later based on 2018 OCC codes
  "AGEP", # Age
  "SEX", # Sex
  "RAC1P", # Race
  "HISP" # Hispanic origin
)

PUMS_FILTER <-list(
  WKHP = 35:99, # Worked at least 35 hours per week in the last 12 months
  WKW = 1:2 # Worked at least 48 weeks in the last 12 months
)

pums_raw <- tibble()
for (STATE in STATES) {
  state_pums <- get_pums(PUMS_VARS, STATE, variables_filter = PUMS_FILTER,
                         recode = TRUE)
  pums_raw <- bind_rows(pums_raw, state_pums)
  rm(state_pums)
}

pums <- pums_raw %>%
  mutate(
    Income = WAGP * as.numeric(ADJINC),
    Age = case_when(
      AGEP < 30 ~ "29 or younger",
      AGEP < 40 ~ "30 to 39",
      AGEP < 50 ~ "40 to 49",
      AGEP < 60 ~ "50 to 59",
      TRUE ~ "60 or older"
    ),
    `Race/Ethnicity` = case_when(
      HISP != "01" ~ "Hispanic/Latino (any race)",
      RAC1P == "1" ~ "White (non-Hispanic)",
      RAC1P == "2" ~ "Black (non-Hispanic)",
      RAC1P == "6" ~ "Asian (non-Hispanic)",
      TRUE ~ "Other/multiple races (non-Hispanic)"
    ),
    Sex = as.character(SEX_label),
    Occupation = as.character(OCCP_label),
    State = as.character(ST_label)
  ) %>%
  select(Income, Sex, Age, `Race/Ethnicity`, Occupation, State, PWGTP) %>%
  filter(Income > 0)

save(pums, file = "data/pums.rds")


# # View all occupations
# select(pums, Occupation) %>% distinct() %>% arrange(Occupation) %>% View()

# # Sample plot
# pums %>%
#   ggplot() +
#     geom_density(aes(x = Income, weight = PWGTP), fill = "darkblue", col = NA) +
#     facet_wrap(~Occ_Grp, ncol = 1, scales = "free_y", strip.position = "left") +
#     scale_x_continuous(limits = c(0, 200000), breaks = seq(0, 200000, 25000)) +
#     see::theme_modern() +
#     theme(axis.line.y=element_blank(), axis.text.y = element_blank())
#
# occ_filter = "All Occupations"
# state_filter = "Illinois/IL"
# sex_filter = "Male"
# age_filter = "30 to 39"
# race_filter = "White (non-Hispanic)"
#
# df <- pums %>%
#   filter(State == state_filter,
#          Sex == sex_filter,
#          Age == age_filter,
#          `Race/Ethnicity` == race_filter)
#
# ggplot() +
#   geom_density(aes(x = Income, weight = PWGTP), data = pums, alpha = 0.15, fill = "black", col = NA) +
#   geom_density(aes(x = Income, weight = PWGTP), data = df, alpha = 0.75, fill = "darkblue", col = NA) +
#   scale_x_continuous(limits = c(0, 250000), labels = scales::label_dollar()) +
#   theme_minimal() +
#   theme(axis.text.y = element_blank(),
#         panel.grid.major.y = element_blank(),
#         panel.grid.minor.y = element_blank()) +
#   labs(title = "Annual Earnings of Full-Time Workers",
#        subtitle = paste(state_filter, occ_filter, sex_filter, age_filter, race_filter, sep = " | "),
#        caption = paste(
#          "Only includes workers who reported working at least 35 hours per week",
#          "and at least 48 weeks per year (including paid time off).\n",
#          "Income includes wages, salary, commissions, bonuses and tips.",
#          "Other income sources (including from self-employment) are excluded.\n",
#          "The distribution is an estimate based on", comma(nrow(df)),
#          "surveyed individuals, representing", comma(sum(df$PWGTP)), "workers.")) +
#   xlab("Annual earnings (2020 dollars)") +
#   ylab("Proportion of full-time workers")
