options(scipen = 999)

if(!require("pacman")) install.packages("pacman")

#Load packages
pacman::p_load(
  feather,
  rio,
  here,
  tidyverse
)

#Load files needed
migration <- import(here("data", "processed", "WPP", "migration.csv"))
fertility <- import(here("data", "processed", "WPP", "fertility.csv"))
mortality <- import(here("data", "processed", "WPP", "deaths.csv"))
population_all <- import(here("data", "processed", "WPP", "age_both.csv"))
population_female <- import(here("data", "processed", "WPP", "age_female.csv"))
contact_matricies <- import(here("data", "raw", "contact_matricies", "contact_all.rdata"))
routine_vaccination_data <- import("data/raw/WHO/coverage-data_updated.xlsx")
sia_vaccination <- import("data/processed/vaccination/sia_vimc.rds")
full_disease_df <- import("data/processed/WHO/reported_cases_data.csv")
vaccination_schedule <- import("data/processed/WHO/vaccine-schedule-data.xlsx")
vaccination_translated <- import("data/processed/vaccination/Vaccine_Abbreviations_and_Diseases.csv")
vaccination_pre1980 <- import("data/processed/vaccination/vaccine_coverage_backextrapolation_rules.xlsx")

routine_vaccination_data <- routine_vaccination_data %>%
  left_join(vaccination_translated, by = c("ANTIGEN" = "Abbreviation"))

disease_parameters <- import("data/processed/model_parameters/disease_parameters_table.xlsx")
vaccine_parameters <- import("data/processed/vaccination/vaccine_protection.xlsx")

#Save data
write_rds(migration %>%
            select(area, iso3, year, migration_rate_1000), "output/for_package/migration.rds")
write_rds(fertility %>%
            select(area, iso3, year, x15:x49), "output/for_package/fertility.rds")
write_rds(mortality %>%
            select(area, iso3, year, x0:x100), "output/for_package/mortality.rds")
write_rds(population_all %>%
            select(area, iso3, year, x0:x100), "output/for_package/population_all.rds")
write_rds(population_female %>%
            select(area, iso3, year, x0:x100), "output/for_package/population_female.rds")
write_rds(contact_matricies, "output/for_package/contact_matricies.rds")
write_rds(routine_vaccination_data, "output/for_package/routine_vaccination_data.rds")
write_rds(sia_vaccination, "output/for_package/sia_vaccination.rds")
write_rds(full_disease_df, "output/for_package/full_disease_df.rds")
write_rds(vaccination_schedule, "output/for_package/vaccination_schedule.rds")
write_rds(vaccination_pre1980, "output/for_package/vaccination_pre1980.rds")

write_rds(disease_parameters, "output/for_package/disease_parameters.rds")
write_rds(vaccine_parameters, "output/for_package/vaccine_parameters.rds")




