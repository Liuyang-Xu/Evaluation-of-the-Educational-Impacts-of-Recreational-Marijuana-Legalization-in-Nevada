# ********************************************************************************
# ==== 1. Library ====
# ********************************************************************************
library(tidyverse)
library(plyr)


# ********************************************************************************
# ==== 2. File Type Convert ====
# ********************************************************************************
# **** Do not run this part 2
# these .txt files had already been turned to csv files

# names of the file needs converting 
Geo_Data <- c("Geo_Data_2011-12_membership",
              "Geo_Data_2012-13_membership",
              "Geo_Data_2013-14_membership",
              "Geo_Data_2014-15_membership")

# change .txt to .csv and save new files
for(i in 1: length(Geo_Data)){
  path <- paste0('../../Data/TXT_FILE/', Geo_Data[i], '.txt')
  result <- read.delim(path, sep = "\t")  
  write.csv(result, paste0('../../Data/CSV_FILE/Geo_Data_2011-2022', Geo_Data[i], '.csv'))
}

# clean environment
rm(list = ls())


# ********************************************************************************
# ==== 3. Load Files ====
# ********************************************************************************
# files were all pre-downloaded

# https://www.gerkelab.com/blog/2018/09/import-directory-csv-purrr-readr/
# read all files
file.list <- list.files(path = "../../Data/CSV_FILE/Geo_Data_2011-2022", pattern = "*.csv") %>% 
  paste0("../../Data/CSV_FILE/Geo_Data_2011-2022/", .)
Geo_Data.list <- lapply(file.list, read.csv)

Nevada_Accountability_Portal <- read_csv("../../Data/CSV_FILE/Nevada_Aaccountability_Portal_Data_2011-2022/Report.csv", 
                                         skip = 2)


# ********************************************************************************
# ==== 4. Data Cleaning ====
# ********************************************************************************
# Clean the raw dataset from year 2011 - 2022

# rename cols
# different years datasets have different col names
# unify them
new_col_name.list <- c(state_abr = "MSTATE",
                       state_abr = "STABR",
                       state_abr = "ST",
                       year = "SURVYEAR",
                       year = "SCHOOL_YEAR")
Geo_Data.list <- lapply(Geo_Data.list, 
                   function(x) {dplyr::rename(x, any_of(new_col_name.list))})

# combine datassets across all years
Geo_Data <- do.call(plyr::rbind.fill, Geo_Data.list)

Geo_Data <- Geo_Data %>%
  filter(state_abr %in% c("NV", "CA")) %>% # NV for our analysis, CA for the other group
  mutate(year = case_when(year == "2011" ~ 2011,
                          year == "2012" ~ 2012,
                          year == "2013" ~ 2013,
                          year == "2014-2015" ~ 2014,
                          year == "2014-2015" ~ 2014,
                          year == "2015-2016" ~ 2015,
                          year == "2016-2017" ~ 2016,
                          year == "2017-2018" ~ 2017,
                          year == "2018-2019" ~ 2018,
                          year == "2019-2020" ~ 2019,
                          year == "2020-2021" ~ 2020,
                          year == "2021-2022" ~ 2021,
                          TRUE ~ NA))

# **************************************
# ==== 4.1 2016 - 2021 ====
# **************************************
# race_ratio 
Geo_data.race_ratio.2016_2021 <- Geo_Data %>% 
  filter(year %in% c(2016:2021),
         RACE_ETHNICITY %in% c("American Indian or Alaska Native", "Asian", 
                               "Black or African American", "Hispanic/Latino",
                               "Native Hawaiian or Other Pacific Islander",
                               "Two or more races", "White")) %>% 
  filter_at(vars(STUDENT_COUNT), all_vars(!is.na(.))) %>%
  group_by(state_abr, year, RACE_ETHNICITY, LEAID) %>%
  summarize(n = sum(STUDENT_COUNT)) %>% ungroup() %>% 
  dplyr::select(year, state_abr, RACE_ETHNICITY, LEAID, n) %>%
  group_by(state_abr, year, LEAID) %>%
  mutate(RACE_RATIO = n/sum(n)) %>% ungroup() %>% 
  dplyr::select(-n)%>%
  spread(key = RACE_ETHNICITY, value = RACE_RATIO)

# gender_ratio
Geo_data.gender_ratio.2016_2021 <- Geo_Data %>% 
  filter(year %in% c(2016:2021),
         SEX %in% c("Female", "Male")) %>% 
  filter_at(vars(STUDENT_COUNT), all_vars(!is.na(.))) %>%
  group_by(state_abr, year, SEX, LEAID) %>%
  summarize(n = sum(STUDENT_COUNT)) %>% ungroup() %>% 
  dplyr::select(state_abr, year, SEX, LEAID, n) %>%
  group_by(state_abr, LEAID, year) %>%
  mutate(GENDER_RATIO = n/sum(n)) %>% ungroup() %>% 
  dplyr::select(-n) %>%
  spread(key = SEX , value = GENDER_RATIO)

# total_enrollment
Geo_data.total_enrollment.2016_2021 <- Geo_Data %>% 
  filter_at(vars(STUDENT_COUNT), all_vars(!is.na(.))) %>%  
  group_by(state_abr, year, LEAID)%>%
  summarise(TOTAL_ENROLLMENT = sum(STUDENT_COUNT))

# merge them all 
Geo_data.2016_2021 <- Geo_data.race_ratio.2016_2021 %>% 
  left_join(Geo_data.gender_ratio.2016_2021, by = c("year", "LEAID", "state_abr")) %>% 
  left_join(Geo_data.total_enrollment.2016_2021, by = c("year", "LEAID", "state_abr"))
                                                        

# **************************************
# ==== 4.2 2011 - 2015 ====
# **************************************
# Notes:
# AMALM ----- Total students, all grades - American Indian/Alaska Native - male
# AMALF ----- Total students, all grades - American Indian/Alaska Native - female

# ASALM ----- Total students, all grades - Asian - male
# ASALF ----- Total students, all grades - Asian - female

# HIALM ----- Total students, all grades - Hispanic - male
# HIALF ----- Total students, all grades - Hispanic - female

# BLALM ----- Total students, all grades - Black, non-Hispanic - male
# BLALF ----- Total students, all grades - Black, non-Hispanic - female

# WHALM ----- Total students, all grades - White, non-Hispanic - male
# WHALF ----- Total students, all grades - White, non-Hispanic - female

# HPALM ----- Total students, all grades - Hawaiian Native/Pacific Islander - male
# HPALF ----- Total students, all grades - Hawaiian Native/Pacific Islander - female

# TRALM ----- Total students, all grades - Two or more races - male
# TRALF ----- Total students, all grades - Two or more races - female

# gender_ratio
Geo_data.gender_ratio.2011_2015 <- Geo_Data %>%
  filter(year %in% c(2011:2015)) %>% 
  dplyr::select(year, LEAID, state_abr,
                AMALM, AMALF, ASALM, ASALF, HIALM,HIALF, BLALM,BLALF,
                WHALM,WHALF, HPALM,HPALF, TRALM,TRALF, TOTETH) %>% 
  mutate(MALE_STUDENT = AMALM + ASALM + HIALM + BLALM + WHALM +HPALM + TRALM,
         FEMALE_STUDENT = AMALF + ASALF + HIALF + BLALF + WHALF +HPALF + TRALF) %>%
  group_by(state_abr, year, LEAID) %>%
  summarize(Male = {MALE_STUDENT / (MALE_STUDENT + FEMALE_STUDENT)},
                   Female = {FEMALE_STUDENT / (MALE_STUDENT + FEMALE_STUDENT)})

# race_ratio
Geo_data.race_ratio.2011_2015 <- Geo_Data %>%
  filter(year %in% c(2011:2015)) %>% 
  dplyr::select(year, LEAID, state_abr,
                AMALM, AMALF, ASALM, ASALF, HIALM,HIALF, BLALM,BLALF,
                WHALM,WHALF, HPALM,HPALF, TRALM,TRALF, TOTETH) %>%
  mutate(AMAL = AMALM + AMALF,
         ASAL = ASALM + ASALF,
         HIAL = HIALM + HIALF,
         BLAL = BLALM + BLALF,
         WHAL = WHALM + WHALF,
         HPAL = HPALM + HPALF,
         TRAL = TRALM + TRALF) %>%
  group_by(state_abr, year, LEAID) %>%
  summarize(AMAL_RATIO = {AMAL / (AMAL + ASAL + HIAL + BLAL + WHAL + HPAL + TRAL)},
            ASAL_RATIO = {ASAL / (AMAL + ASAL + HIAL + BLAL + WHAL + HPAL + TRAL)},
            HIAL_RATIO = {HIAL / (AMAL + ASAL + HIAL + BLAL + WHAL + HPAL + TRAL)},
            BLAL_RATIO = {BLAL / (AMAL + ASAL + HIAL + BLAL + WHAL + HPAL + TRAL)},
            WHAL_RATIO = {WHAL / (AMAL + ASAL + HIAL + BLAL + WHAL + HPAL + TRAL)},
            HPAL_RATIO = {HPAL / (AMAL + ASAL + HIAL + BLAL + WHAL + HPAL + TRAL)},
            TRAL_RATIO = {TRAL / (AMAL + ASAL + HIAL + BLAL + WHAL + HPAL + TRAL)}) %>%
  dplyr::rename("American Indian or Alaska Native" = "AMAL_RATIO" ,
                "Asian" = "ASAL_RATIO",
                "Hispanic/Latino" = "HIAL_RATIO",
                "Black or African American" = "BLAL_RATIO" ,
                "White" = "WHAL_RATIO" ,
                "Native Hawaiian or Other Pacific Islander" = "HPAL_RATIO" ,
                "Two or more races" = "TRAL_RATIO")

# total_enrollment
Geo_data.total_enrollment.2011_2015 <- Geo_Data %>%
  filter(year %in% c(2011:2015)) %>% 
  dplyr::select(year, LEAID, state_abr, TOTETH) %>%
  group_by(state_abr, year, LEAID) %>%
  summarize(TOTAL_ENROLLMENT = sum(TOTETH))

# merge them all 
Geo_data.2011_2015 <- Geo_data.race_ratio.2011_2015 %>% 
  left_join(Geo_data.gender_ratio.2011_2015, by = c("year", "LEAID", "state_abr")) %>%
  left_join(Geo_data.total_enrollment.2011_2015, by = c("year", "LEAID", "state_abr"))


# **************************************
# ==== 4.3 2011 - 2021 ====
# **************************************
Geo_data.2011_2021 <- plyr::rbind.fill(Geo_data.2011_2015, 
                                       Geo_data.2016_2021, 
                                       fill = NULL)
View(Geo_data_2011_2022)


# **************************************
# ==== 4.4 Nevada group ====
# **************************************
# Further cleaning on Nevada group data from 2011 - 2021 
Geo_data.2011_2021.NV <- Geo_data.2011_2021 %>%
  filter(state_abr == "NV",
         LEAID %in% c(3200001, 3200030, 3200060, 3200090, 3200120, 3200150, 
                      3200180, 3200210, 3200240, 3200270, 3200300, 3200330,
                      3200360, 3200390, 3200420, 3200450, 3200480, 3200510,
                      3200021, 3200022, 3200023)) %>% 
  mutate(FIPS = case_when(LEAID == 3200030 ~ 32001,
                          LEAID == 3200060 ~ 32003, 
                          LEAID == 3200090 ~ 32005, 
                          LEAID == 3200120 ~ 32007, 
                          LEAID == 3200150 ~ 32009, 
                          LEAID == 3200180 ~ 32011, 
                          LEAID == 3200210 ~ 32013, 
                          LEAID == 3200240 ~ 32015, 
                          LEAID == 3200270 ~ 32017, 
                          LEAID == 3200300 ~ 32019, 
                          LEAID == 3200330 ~ 32021, 
                          LEAID == 3200360 ~ 32023, 
                          LEAID == 3200420 ~ 32027, 
                          LEAID == 3200450 ~ 32029, 
                          LEAID == 3200480 ~ 32031, 
                          LEAID == 3200510 ~ 32033, 
                          LEAID == 3200390 ~ 32510)) %>% drop_na(FIPS)
               
# **************************************
# ==== 4.5 Nevada Accountability Portal ====
# **************************************
Nevada_Accountability_Portal <- Nevada_Accountability_Portal %>% 
  mutate(`District/Authority Name` = ifelse(`District/Authority Name` == "State", 
                                            paste0(sub(" -.*", "", Nevada_Accountability_Portal$Name)),
                                            `District/Authority Name`),
         FIPS = case_when(`District/Authority Name` == "Clark" ~ "32003",
                          `District/Authority Name` == "Washoe" ~ "32031",
                          `District/Authority Name` == "Elko" ~ "32007",
                          `District/Authority Name` == "Carson City" ~ "32510",
                          `District/Authority Name` == "Humboldt" ~ "32013",
                          `District/Authority Name` == "Mineral" ~ "32021",
                          `District/Authority Name` == "Nye" ~ "32023",
                          `District/Authority Name` == "Douglas" ~ "32005",
                          `District/Authority Name` == "Lander" ~ "32015",
                          `District/Authority Name` == "White Pine" ~ "32033",
                          `District/Authority Name` == "Lincoln" ~ "32017",
                          `District/Authority Name` == "Churchill" ~ "32001",
                          `District/Authority Name` == "Lyon" ~ "32019",
                          `District/Authority Name` == "Eureka" ~ "32011",
                          `District/Authority Name` == "Esmeralda" ~ "32009",
                          `District/Authority Name` == "Storey" ~ "32029",
                          `District/Authority Name` == "Pershing" ~ "32027")) %>%
  dplyr::select(FIPS, `FRL Eligible #`, `FRL Eligible %`, `Accountability Year`) %>%
  dplyr::rename("year" = "Accountability Year") %>%
  mutate(year = case_when(year == "2011-2012" ~ 2011,
                          year == "2012-2013" ~ 2012,
                          year == "2013-2014" ~ 2013,
                          year == "2014-2015" ~ 2014,
                          year == "2015-2016" ~ 2015,
                          year == "2016-2017" ~ 2016,
                          year == "2017-2018" ~ 2017,
                          year == "2018-2019" ~ 2018,
                          year == "2019-2020" ~ 2019,
                          year == "2020-2021" ~ 2020,
                          year == "2021-2022" ~ 2021),
         FIPS = as.numeric(FIPS)) %>% drop_na(FIPS)

Nevada_Accountability_Portal <- Nevada_Accountability_Portal %>%
  mutate(`FRL Eligible #` = {ifelse(`FRL Eligible #` %in% c("-", '|','N/A'), 0, `FRL Eligible #`) %>% as.numeric()},
         `FRL Eligible %` = {ifelse(`FRL Eligible %`%in% c("-", '|','N/A'), 0, `FRL Eligible %`) %>% as.numeric()},
         Total_Students = {`FRL Eligible #` / (0.01 * `FRL Eligible %`)})%>%
  dplyr::select(year,FIPS,`FRL Eligible #`,Total_Students)%>%
  group_by(year,FIPS) %>%
  summarize(FRL_eligible_number = sum(as.numeric(`FRL Eligible #`), na.rm=TRUE),
                   Total_student = sum(as.numeric(Total_Students), na.rm=TRUE))%>%
  mutate(FRL_eligible_percent = FRL_eligible_number / Total_student)
# NA "State" "State Public Charter School Authority" "University"

# ********************************************************************************
# ==== 5. Geo_data_2011_2022_NV & Nevada_Accountability_Portal ====
# ********************************************************************************
Geo_Data.NV <- Nevada_Accountability_Portal %>%
  left_join(Geo_data.2011_2021.NV, by = c("FIPS", "year"))

Geo_Data.NV <- write.csv(Geo_Data.NV, "geo_data_2011_2022_NV.csv")

