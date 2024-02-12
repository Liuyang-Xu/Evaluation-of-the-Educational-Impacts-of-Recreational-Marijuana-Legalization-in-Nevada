#############################################
#Load package and expand memory limit
#############################################
library(data.table)
library(dplyr)
library(tidyverse)
library(readr)
library(plyr)
library(reshape)
library(tidyr)
library(str2str)
library(usmap)
memory.limit(size = 80000)

#############################################
#set working directory
#############################################
setwd("C:/Users/stigg/OneDrive/Desktop/2022 Fall/Seminar/Membership_Data")
#"your directory goes here"
setwd("")


#############################################
#read text file                           ---------Do not run this code, those txt files had already been transforemed to csv file.
#############################################
Geo_Data <- c("Geo_Data_2011-12_membership","Geo_Data_2012-13_membership",
              "Geo_Data_2013-14_membership","Geo_Data_2014-15_membership")

for(i in 1: length(Geo_Data)){
  path <- paste0('C:/Users/stigg/OneDrive/Desktop/2022 Fall/Seminar/Membership_Data/',
                 Geo_Data[i],'.txt')
  result <- read.delim(path, sep="\t")  
  fwrite( result, paste0(Geo_Data[i], '.csv'))
}

#############################################
#read csv 
#############################################
#memory.limit(size = 80000)
#df <- list.files(path = "/Users/stigg/OneDrive/Desktop/2022 Fall/Seminar/Membership_Data/", pattern = "*.csv")%>%
 # map_df(~read.csv(.))
memory.limit(size=80000)
df_1 <- list.files(path = "/Users/stigg/OneDrive/Desktop/2022 Fall/Seminar/Membership_Data/", 
                   pattern = "*.csv")
df_1 <- as.list(lapply(df_1, read.csv))




#############################################
#Clean the raw dataset from year 2011 - 2022
#############################################
#df_1[[1]]
df_1_1 <<- as.data.frame(df_1[[1]])%>%
  filter(MSTATE == "NV" | MSTATE == "CA" )%>%
dplyr::rename("state_name" = "MSTATE",
              "year" = "SURVYEAR")

#df_2[[2]]
df_1_2 <- as.data.frame(df_1[[2]])%>%
  filter(MSTATE == "NV" | MSTATE == "CA" )%>%
  dplyr::rename("state_name" = "MSTATE",
                "year" = "SURVYEAR")

#df_3[[3]]
df_1_3 <- as.data.frame(df_1[[3]])%>%
  filter(MSTATE == "NV" | MSTATE == "CA" )%>%
  dplyr::rename("state_name" = "MSTATE",
                "year" = "SURVYEAR")

#df_4[[4]]
df_1_4 <- as.data.frame(df_1[[4]])%>%
  filter(STABR == "NV" | STABR == "CA" )%>%
  dplyr::rename("state_name" = "STABR",
                "year" = "SURVYEAR")%>%
  mutate(year = ifelse(year == "2014-2015", 2014,0))

#df_5[[5]]
df_1_5 <- as.data.frame(df_1[[5]])%>%
  filter(STABR == "NV" | STABR == "CA" )%>%
  dplyr::rename("state_name" = "STABR",
                "year" = "SURVYEAR")%>%
  mutate(year = ifelse(year == "2015-2016", 2015,0))

#df_6[[6]]
df_1_6 <- as.data.frame(df_1[[6]])%>%
  filter(ST == "NV" | ST == "CA" )%>%
  dplyr::rename("state_name" = "ST",
                "year" = "SCHOOL_YEAR")%>%
  mutate(year = ifelse(year == "2016-2017", 2016,0))

#df_7[[7]]
df_1_7<- as.data.frame(df_1[[7]])%>%
  filter(ST == "NV" | ST == "CA" )%>%
  dplyr::rename("state_name" = "ST",
                "year" = "SCHOOL_YEAR")%>%
  mutate(year = ifelse(year == "2017-2018", 2017,0))

#df_5[[8]]
df_1_8 <- as.data.frame(df_1[[8]])%>%
  filter(ST == "NV" | ST == "CA" )%>%
  dplyr::rename("state_name" = "ST",
                "year" = "SCHOOL_YEAR")%>%
  mutate(year = ifelse(year == "2018-2019", 2018,0))

#df_5[[9]]
df_1_9 <- as.data.frame(df_1[[9]])%>%
  filter(ST == "NV" | ST == "CA" )%>%
  dplyr::rename("state_name" = "ST",
                "year" = "SCHOOL_YEAR")%>%
  mutate(year = ifelse(year == "2019-2020", 2019,0))

#df_10[[10]]
df_1_10 <- as.data.frame(df_1[[10]])%>%
  filter(ST == "NV" | ST == "CA" )%>%
  dplyr::rename("state_name" = "ST",
                "year" = "SCHOOL_YEAR")%>%
  mutate(year = ifelse(year == "2020-2021", 2020,0))

#df_11[[11]]
df_1_11 <- as.data.frame(df_1[[11]])%>%
  filter(ST == "NV" | ST == "CA" )%>%
  dplyr::rename("state_name" = "ST",
                "year" = "SCHOOL_YEAR")%>%
  mutate(year = ifelse(year == "2021-2022", 2022,0))

df_1_11_test <- df_1_11 %>% filter(state_name == "NV")




#################################################################
#Cleaning data that year from 2016 - 2022
#################################################################

Geo_data_2016_2022 <- rbind.fill(df_1_6,df_1_7,df_1_8,df_1_9,df_1_10,df_1_11)

###########
#race_ratio 
###########
Geo_data_2016_2022_race_ratio <- Geo_data_2016_2022 %>% 
                                                 dplyr::filter_at(vars(STUDENT_COUNT), all_vars(!is.na(.)))%>%                      #DROP??? mutate(STUDENT_COUNT = ifelse(is.na(STUDENT_COUNT), 0, STUDENT_COUNT))
                                                 dplyr::group_by(state_name,year,RACE_ETHNICITY,LEAID)%>%
                                                 dplyr::summarize(n = sum(STUDENT_COUNT))%>%
                                                 dplyr::select(year,state_name,RACE_ETHNICITY ,LEAID,n)%>%
                                                 dplyr::filter(RACE_ETHNICITY == "American Indian or Alaska Native"|
                                                                 RACE_ETHNICITY == "Asian"|
                                                                 RACE_ETHNICITY == "Black or African American"|
                                                                 RACE_ETHNICITY == "Hispanic/Latino"|
                                                                 RACE_ETHNICITY == "Native Hawaiian or Other Pacific Islander"|
                                                                 RACE_ETHNICITY == "Two or more races"|
                                                                 RACE_ETHNICITY == "White")%>%
                                                 dplyr::group_by(state_name,LEAID,year)%>%
                                                 dplyr::mutate(RACE_RATIO = n/sum(n))%>%
                                                 select(-n)%>%
                                                 spread(., key = RACE_ETHNICITY , value = RACE_RATIO)

View(Geo_data_2016_2022_race_ratio)

###########
#gender_ratio 
###########
Geo_data_2016_2022_gender_ratio <- Geo_data_2016_2022 %>% filter(SEX == "Female"| SEX == "Male")%>%dplyr::filter_at(vars(STUDENT_COUNT), all_vars(!is.na(.)))%>%                      #DROP??? mutate(STUDENT_COUNT = ifelse(is.na(STUDENT_COUNT), 0, STUDENT_COUNT))
                                                         dplyr::group_by(state_name,year,SEX,LEAID)%>%
                                                         dplyr::summarize(n = sum(STUDENT_COUNT))%>%
                                                         dplyr::select(state_name,SEX ,LEAID,n)%>%
                                                         dplyr::group_by(state_name,LEAID,year)%>%
                                                         dplyr::mutate(GENDER_RATIO = n/sum(n))%>%
                                                         dplyr::select(-n)%>%
                                                         spread(., key = SEX , value = GENDER_RATIO)


View(Geo_data_2016_2022_gender_ratio)
###########
#total_enrollment 
###########
Geo_data_2016_2022_total_enrollment <- Geo_data_2016_2022 %>% 
                                              dplyr::filter_at(vars(STUDENT_COUNT), all_vars(!is.na(.)))%>%  
                                              dplyr::group_by(state_name,year,LEAID)%>%
                                              dplyr::summarise(TOTAL_ENROLLMENT = sum(STUDENT_COUNT))

                

View(Geo_data_2016_2022_total_enrollment)

###########
#merge them all 
###########
Geo_data_2016_2022 <- Geo_data_2016_2022_race_ratio %>% dplyr::left_join(.,Geo_data_2016_2022_gender_ratio,
                                                                    by = c("year","LEAID","state_name"))%>%
                                                        dplyr::left_join(.,Geo_data_2016_2022_total_enrollment,
                                                                    by = c("year","LEAID","state_name"))
                                                        
                                                          
View(Geo_data_2016_2022)



                              
                                                                                 
                                                
#Question:
          #1,should we drop all NA or change NA to 0 in the dataset 
          #2,there is no poverty that I found in the dataset from 2016-2022

                                




#################################################################
#Cleaning year that from 2011 - 2015
#################################################################

Geo_data_2011_2015 <- rbind.fill(df_1_1,df_1_2,df_1_3,df_1_4,df_1_5)
#AMALM ----- Total students, all grades - American Indian/Alaska Native - male
#AMALF -----Total students, all grades - American Indian/Alaska Native - female

#ASALM ----- Total students, all grades - Asian - male
#ASALF ----- Total students, all grades - Asian - female

#HIALM ----- Total students, all grades - Hispanic - male
#HIALF ----- Total students, all grades - Hispanic - female

#BLALM ----- Total students, all grades - Black, non-Hispanic - male
#BLALF ----- 	Total students, all grades - Black, non-Hispanic - female

#WHALM ----- Total students, all grades - White, non-Hispanic - male
#WHALF ----- Total students, all grades - White, non-Hispanic - female

#HPALM ----- Total students, all grades - Hawaiian Native/Pacific Islander - male
#HPALF ----- Total students, all grades - Hawaiian Native/Pacific Islander - female

#TRALM ----- Total students, all grades - Two or more races - male
#TRALF ----- Total students, all grades - Two or more races - female
Geo_data_2011_2015 <-  Geo_data_2011_2015%>% select(year, LEAID, state_name,
                                                    AMALM, AMALF,
                                                    ASALM, ASALF,
                                                    HIALM,HIALF,
                                                    BLALM,BLALF,
                                                    WHALM,WHALF,
                                                    HPALM,HPALF,
                                                    TRALM,TRALF,
                                                    TOTETH)
                                              
###########
#gender_ratio 
###########
Geo_data_2011_2015_gender_ratio <- Geo_data_2011_2015 %>% dplyr::mutate(MALE_STUDENT  = AMALM + ASALM + HIALM + BLALM + WHALM +HPALM + TRALM,
                                                                 FEMALE_STUDENT = AMALF + ASALF + HIALF + BLALF + WHALF +HPALF + TRALF)%>%
                                                          dplyr::group_by(state_name,year, LEAID) %>%
                                                          dplyr::summarize(Male = MALE_STUDENT/(MALE_STUDENT+FEMALE_STUDENT),
                                                                    Female = (FEMALE_STUDENT/(MALE_STUDENT+FEMALE_STUDENT)))

View(Geo_data_2011_2015_gender_ratio)

###########
#race_ratio 
###########
Geo_data_2011_2015_race_ratio <- Geo_data_2011_2015 %>% dplyr::mutate(AMAL  = AMALM + AMALF,
                                                                      ASAL =  ASALM +  ASALF,
                                                                      HIAL =  HIALM + HIALF,
                                                                      BLAL =  BLALM + BLALF,
                                                                      WHAL =  WHALM + WHALF,
                                                                      HPAL =  HPALM + HPALF,
                                                                      TRAL =  TRALM + TRALF)%>%
  dplyr::group_by(state_name,year, LEAID)%>%
  dplyr::summarize(AMAL_RATIO = AMAL/(AMAL+ASAL+HIAL+BLAL+WHAL+HPAL+TRAL),
                   ASAL_RATIO = ASAL/(AMAL+ASAL+HIAL+BLAL+WHAL+HPAL+TRAL),
                   HIAL_RATIO = HIAL/(AMAL+ASAL+HIAL+BLAL+WHAL+HPAL+TRAL),
                   BLAL_RATIO = BLAL/(AMAL+ASAL+HIAL+BLAL+WHAL+HPAL+TRAL),
                   WHAL_RATIO = WHAL/(AMAL+ASAL+HIAL+BLAL+WHAL+HPAL+TRAL),
                   HPAL_RATIO = HPAL/(AMAL+ASAL+HIAL+BLAL+WHAL+HPAL+TRAL),
                   TRAL_RATIO = TRAL/(AMAL+ASAL+HIAL+BLAL+WHAL+HPAL+TRAL)
  )%>%
  dplyr::rename("American Indian or Alaska Native" = "AMAL_RATIO" ,
                "Asian" = "ASAL_RATIO",
                "Hispanic/Latino" = "HIAL_RATIO",
                "Black or African American" = "BLAL_RATIO" ,
                "White" = "WHAL_RATIO" ,
                "Native Hawaiian or Other Pacific Islander" = "HPAL_RATIO" ,
                "Two or more races" = "TRAL_RATIO" 
  )
                                                        
View(Geo_data_2011_2015_race_ratio)

###########
#total_enrollment
###########
Geo_data_2011_2015_total_enrollment <- Geo_data_2011_2015 %>% dplyr::select(year,LEAID,TOTETH,state_name)%>%
                                                              dplyr::group_by(state_name,year,LEAID)%>%
                                                              dplyr::summarize(TOTAL_ENROLLMENT = sum(TOTETH))


###########
#merge them all 
###########
Geo_data_2011_2015 <- Geo_data_2011_2015_race_ratio %>% dplyr::left_join(.,Geo_data_2011_2015_gender_ratio,
                                                                  by = c("year","LEAID","state_name"))%>%
                                                        dplyr::left_join(.,Geo_data_2011_2015_total_enrollment,
                                                        by = c("year","LEAID","state_name"))
  



#######################################################
#bind Geo_data_2011_2015 with Geo_data_2016_2022
#######################################################
Geo_data_2011_2022 <- rbind.fill(Geo_data_2011_2015,Geo_data_2016_2022, fill = NULL)
View(Geo_data_2011_2022)


#######################
# Further cleaning on Nevada group data from 2011 - 2022
#######################
Geo_data_2011_2022_NV <- Geo_data_2011_2022 %>%filter(state_name == "NV",
                                                            LEAID == 3200001  | 
                                                            LEAID == 3200030  |
                                                            LEAID == 3200060  |
                                                            LEAID == 3200090  |
                                                            LEAID == 3200120 |
                                                            LEAID == 3200150  |
                                                            LEAID == 3200180  |
                                                            LEAID == 3200210  |
                                                            LEAID == 3200240 |
                                                            LEAID == 3200270  |
                                                            LEAID == 3200300  |
                                                            LEAID == 3200330  |
                                                            LEAID == 3200360  |
                                                            LEAID == 3200390  |
                                                            LEAID == 3200420  |
                                                            LEAID == 3200450 |
                                                            LEAID == 3200480  |
                                                            LEAID == 3200510 |
                                                            LEAID == 3200021  |
                                                            LEAID == 3200022  |
                                                            LEAID == 3200023 )%>%
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
                            LEAID == 3200390 ~ 32510))%>%drop_na(FIPS)
               


#######################
#Nevada Accountability Portal
#######################
Nevada_Accountability_Portal <- read_csv("Report.csv",skip=2)
Nevada_Accountability_Portal <- Nevada_Accountability_Portal %>% dplyr::mutate(
  `District/Authority Name` = ifelse(`District/Authority Name` == "State", paste0(sub(" -.*","",Nevada_Accountability_Portal$Name)),`District/Authority Name`))%>%
  dplyr::mutate(
  
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
                   `District/Authority Name` == "Pershing" ~ "32027"))%>%
  dplyr::select(FIPS,`FRL Eligible #`,`FRL Eligible %`,`Accountability Year`, )%>%
  dplyr::rename("year" = "Accountability Year")%>%
  dplyr::mutate(year = case_when(year == "2011-2012" ~ 2011,
                                 year == "2012-2013" ~ 2012,
                                 year == "2013-2014" ~ 2013,
                                 year == "2014-2015" ~ 2014,
                                 year == "2015-2016" ~ 2015,
                                 year == "2016-2017" ~ 2016,
                                 year == "2017-2018" ~ 2017,
                                 year == "2018-2019" ~ 2018,
                                 year == "2019-2020" ~ 2019,
                                 year == "2020-2021" ~ 2020,
                                 year == "2021-2022" ~ 2021
                                 ))%>%
  drop_na(FIPS)%>%
  mutate(FIPS = as.double(FIPS))

Nevada_Accountability_Portal <- Nevada_Accountability_Portal %>% 
  dplyr::mutate(`FRL Eligible #` = as.numeric(ifelse(`FRL Eligible #` == "-" |`FRL Eligible #` == '|' | `FRL Eligible #` == 'N/A' ,
                                          0, `FRL Eligible #`)),
                `FRL Eligible %` = as.numeric(ifelse(`FRL Eligible %` == "-" |`FRL Eligible %` == '|' | `FRL Eligible %` == 'N/A' ,
                                          0, `FRL Eligible %`)),
                Total_Students = as.numeric(`FRL Eligible #`)/(0.01*(as.numeric(`FRL Eligible %`))))%>%
  dplyr::select(year,FIPS,`FRL Eligible #`,Total_Students)%>%
  dplyr::group_by(year,FIPS) %>%
  dplyr::summarize(FRL_eligible_number = sum(as.numeric(`FRL Eligible #`),na.rm=TRUE),
                   Total_student = sum(as.numeric(Total_Students),na.rm=TRUE))%>%
  dplyr::mutate(FRL_eligible_percent = FRL_eligible_number/Total_student)
#NA "State" "State Public Charter School Authority" "University"

#####################
#merge Geo_data_2011_2022_NV with Nevada_Accountability_Portal
#####################
Nevada_Accountability_Portal <- Nevada_Accountability_Portal %>% left_join(.,Geo_data_2011_2022_NV, by = c("FIPS","year"))

View(Nevada_Accountability_Portal)
Geo_data_2011_2022_NV <- fwrite( Nevada_Accountability_Portal, "geo_data_2011_2022_NV.csv")









