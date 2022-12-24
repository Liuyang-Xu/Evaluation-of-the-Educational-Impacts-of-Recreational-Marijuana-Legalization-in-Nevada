################################################################################
# library
library(did)
library(tidyverse)
library(patchwork)
library(ggplot2)
library(corrplot)
library(MASS)
library(tableone)
################################################################################

################################################################################
# load the data
graduation <- read_csv("marijuanna_data_for_analysis.csv")
geo <- read_csv("geo_data_2011_2022_NV.csv")

graduation <- graduation %>% 
  dplyr::select(County, `School District`, fips, `Graduating Class`, `Graduation Rate`) %>%
  mutate(`Graduation Rate` = ifelse(`Graduation Rate` == ">95", 95, `Graduation Rate`),
         `Graduation Rate` = as.numeric(`Graduation Rate`),
         `Recreational Status` = as.factor(ifelse(County == "Churchill County" |
                                                    County == "Clark County" |
                                                    County == "Elko County" |
                                                    County == "Humboldt County" |
                                                    County == "Lyon County" |
                                                    County == "Nye County" |
                                                    County == "Washoe County" |
                                                    County == "White Pine County", 1, 0))) %>% na.omit()
################################################################################

################################################################################
# visualize the outcome

# ggplot(data = graduation) +
#   geom_point(aes(x = `Graduating Class`, y = `Graduation Rate`, color = `Recreational Status`))

p0 <- ggplot(data = graduation %>% filter(`Recreational Status` == 0 )) +
  geom_point(aes(x = `Graduating Class`, y = `Graduation Rate`), color = "red") +
  xlab("Recreational Status 0")
p1 <- ggplot(data = graduation %>% filter(`Recreational Status` == 1 )) +
  geom_point(aes(x = `Graduating Class`, y = `Graduation Rate`), color = "blue") +
  xlab("Recreational Status 1")
p0 + p1 & scale_y_continuous(limits = c(40, 100))

graduation_by_group <- graduation %>% 
  mutate(year = as.numeric(substr(`Graduating Class`, 1, 4)))
p0 <- ggplot(data = graduation_by_group %>% filter(`Recreational Status` == 0 )) +
  geom_point(aes(x = year, y = `Graduation Rate`), color = "red") +
  xlab("Recreational Status 0")
p1 <- ggplot(data = graduation_by_group %>% filter(`Recreational Status` == 1 )) +
  geom_point(aes(x = year, y = `Graduation Rate`), color = "blue") +
  xlab("Recreational Status 1")
p0 + p1 & scale_x_continuous(breaks = seq(2014, 2020, 1)) & scale_y_continuous(limits = c(55, 100))

graduation_by_group <- graduation %>% 
  group_by(`Graduating Class`, `Recreational Status`) %>% 
  summarise(`Ave Graduation Rate` = mean(`Graduation Rate`)) %>% ungroup() %>% 
  mutate(year = as.numeric(substr(`Graduating Class`, 1, 4)))
ggplot(data = graduation_by_group, aes(x = year, y = `Ave Graduation Rate`, group = `Recreational Status`)) +
  geom_line(aes(color = `Recreational Status`), lwd = 1.5) + scale_x_continuous(breaks = seq(2014, 2020, 1)) +
  geom_vline(aes(xintercept = 2017), lty = 20) + 
  geom_text(aes(x = 2017, label="\nYear of Legalization", y = 78), colour = "black", hjust = 0)

# ggplot(data = graduation, aes(x = `Graduating Class`, y = `Graduation Rate`, group = `School District`)) +
#   geom_point(aes(color = `School District`))
# 
# ggplot(data = graduation, aes(x = `Graduating Class`, y = `Graduation Rate`, group = `School District`)) +
#   geom_line(aes(color = `Recreational Status`))

covariates_names <- colnames(dta_joined)[8:20]
covariates_names
ggplot(data = dta_joined, aes(x = year, y = FRL_eligible_number, group = fips)) +
  geom_point(aes(color = `School District`))
################################################################################

################################################################################
# model
# plain
geo <- geo %>% 
  mutate(fips = FIPS) %>% 
  dplyr::select(-FIPS, -state_name, -LEAID)

geo_check <- geo %>% 
  dplyr::select(-year, -fips) %>% 
  na.omit()

geo_check_collinearity <- cor(geo_check %>% dplyr::select(-Total_student, -FRL_eligible_percent))
pairs(geo_check)
corrplot(geo_check_collinearity, type = "upper")
corrplot.mixed(geo_check_collinearity, upper = 'ellipse', lower ='number')

dta <- graduation %>% 
  mutate(first.treat = as.numeric(ifelse(`Recreational Status` == 1, "2017", 0)),
         year = as.numeric(substr(`Graduating Class`, 1, 4)))

dta_joined <- dta %>% 
  left_join(geo, by = c("year", "fips")) %>% 
  dplyr::select(-`Graduating Class`) %>% 
  replace(is.na(.), 0)

table1 <- CreateTableOne(vars = c("Graduation Rate", "FRL_eligible_number",
                                  "American Indian or Alaska Native", "Asian", 
                                  "Hispanic/Latino", 
                                  "Black or African American", "White", 
                                  "Native Hawaiian or Other Pacific Islander", 
                                  "Two or more races", "Male", "Female", 
                                  "TOTAL_ENROLLMENT"), strata = "Recreational Status", data = dta_joined)
summary(table1, digits = 3)

# estimate group-time average treatment effects
out <- att_gt(yname = "Graduation Rate",
              gname = "first.treat",
              idname = "fips",
              tname = "year",
              xformla = ~ FRL_eligible_number + Total_student + FRL_eligible_percent + 
                `American Indian or Alaska Native` + Asian + `Hispanic/Latino` + 
                `Black or African American` + White + `Native Hawaiian or Other Pacific Islander` + 
                `Two or more races` + Male + Female + TOTAL_ENROLLMENT,
              control_group = "notyettreated",
              data = dta_joined,
              est_method = "reg")

out <- att_gt(yname = "Graduation Rate",
              gname = "first.treat",
              idname = "fips",
              tname = "year",
              xformla = ~ 1,
              data = dta_joined,
              est_method = "reg")

out <- att_gt(yname = "Graduation Rate",
              gname = "first.treat",
              idname = "fips",
              tname = "year",
              xformla = ~ FRL_eligible_number,
              allow_unbalanced_panel = TRUE,
              control_group = "notyettreated",
              data = dta_joined,
              est_method = "reg")

out <- att_gt(yname = "Graduation Rate",
              gname = "first.treat",
              idname = "fips",
              tname = "year",
              xformla = ~ FRL_eligible_number + Female,
              allow_unbalanced_panel = TRUE,
              control_group = "notyettreated",
              data = dta_joined,
              est_method = "reg")

out <- att_gt(yname = "Graduation Rate",
              gname = "first.treat",
              idname = "fips",
              tname = "year",
              xformla = ~ FRL_eligible_number + Female + `Two or more races`,
              allow_unbalanced_panel = TRUE,
              control_group = "notyettreated",
              data = dta_joined,
              est_method = "reg")

summary(out)
# plot the group-time average treatment effects
ggdid(out)

# make an event study plot
es <- aggte(out, type = "dynamic")
summary(es)
ggdid(es)

# compute this overall average treatment effect parameter
group_effects <- aggte(out, type = "group")
summary(group_effects)
################################################################################

################################################################################
# model**
## please reload the data in the beginning first

geo <- geo %>% 
  mutate(fips = FIPS) %>% 
  dplyr::select(-FIPS, -state_name, -LEAID)

dta <- graduation %>% 
  mutate(first.treat = as.numeric(ifelse(`Recreational Status` == 1, "2017", 0)),
         year = as.numeric(substr(`Graduating Class`, 1, 4)))

## 1. canonical format
dta_joined <- dta %>% 
  left_join(geo, by = c("year", "fips")) %>% 
  dplyr::select(-`Graduating Class`) %>% 
  replace(is.na(.), 0) %>% 
  mutate(time.new = ifelse(year >= 2017, 1, 0))

dta_joined <- dta_joined %>%
  filter(year == 2016 | year == 2017)

# dta_joined <- dta_joined %>% 
#   spread(year, `Graduation Rate`)

result.mean <- dta_joined %>% 
  group_by(`Recreational Status`, time.new) %>% 
  summarise(mean = mean(`Graduation Rate`))

estimate <- (result.mean$mean[4] - result.mean$mean[3]) - 
  (result.mean$mean[2] - result.mean$mean[1])
estimate

didreg <- glm(`Graduation Rate` ~ `Recreational Status`*time.new, data = dta_joined)
summary(didreg)

didreg_wc <- glm(`Graduation Rate` ~ `Recreational Status`*time.new +
                 FRL_eligible_number +
                 `American Indian or Alaska Native` + Asian + `Hispanic/Latino` + 
                 `Black or African American` + White + `Native Hawaiian or Other Pacific Islander` + 
                 `Two or more races` + Female + TOTAL_ENROLLMENT, 
               data = dta_joined)
summary(didreg_wc)

didreg_wc.step <- stepAIC(didreg_wc, 
                          scope=list(lower=as.formula(`Graduation Rate` ~ `Recreational Status`*time.new), 
                                     upper = didreg_wc ))
summary(didreg_wc.step)

## 2. collapse time
dta_joined <- dta %>% 
  left_join(geo, by = c("year", "fips")) %>% 
  dplyr::select(-`Graduating Class`) %>% 
  replace(is.na(.), 0) %>% 
  mutate(time.new = ifelse(year >= 2017, 1, 0))

result.mean <- dta_joined %>% 
  group_by(`Recreational Status`, time.new) %>% 
  summarise(mean = mean(`Graduation Rate`))

estimate <- (result.mean$mean[4] - result.mean$mean[3]) - 
  (result.mean$mean[2] - result.mean$mean[1])
estimate

didreg <- glm(`Graduation Rate` ~ `Recreational Status`*time.new, data = dta_joined)
summary(didreg)

didreg_wc <- glm(`Graduation Rate` ~ `Recreational Status`*time.new +
                   FRL_eligible_number + 
                   `American Indian or Alaska Native` + Asian + `Hispanic/Latino` + 
                   `Black or African American` + White + `Native Hawaiian or Other Pacific Islander` + 
                   `Two or more races` + Female + TOTAL_ENROLLMENT, 
                 data = dta_joined)
summary(didreg_wc)

didreg_wc.step <- stepAIC(didreg_wc, 
                          scope=list(lower=as.formula(`Graduation Rate` ~ `Recreational Status`*time.new), 
                                     upper = didreg_wc ))
summary(didreg_wc.step)
################################################################################