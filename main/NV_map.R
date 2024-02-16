# ********************************************************************************
# ==== 1. Library ====
# ********************************************************************************
library(usmap)
library(rvest)
library(tidyverse)
library(ggplot2)


# ********************************************************************************
# ==== 2. US Marijuana Status ====
# ********************************************************************************
# download website data
url.marijuana_legality <- "https://disa.com/maps/marijuana-legality-by-state"
response.marijuana_legality <- read_html(url.marijuana_legality)

# table of legality information
table.marijuana_legality <- html_elements(response.marijuana_legality, 
                                          xpath = '//div[@class = "main-content-sections"]//tr') %>% 
  html_text2(preserve_nbsp = T)
table.marijuana_legality <- table.marijuana_legality[-1] %>% 
  strsplit(., "\t") %>% unlist() %>% 
  matrix(ncol = 5, byrow = T, 
         dimnames = list(c(), c("state", "Legal Status", "Medicinal", "Decriminalized", "State Laws"))) %>% 
  as_tibble() %>% 
  mutate(State = unique(countypop$abbr)) %>%  # they are the same order so replace directly
  select(-`State Laws`)
  
# legality score for plot
table.marijuana_legality <- table.marijuana_legality %>% 
  mutate(legality = ifelse(Medicinal == "Yes" & Decriminalized == "Yes", 4, 0),
         legality = ifelse(Medicinal == "Yes" & Decriminalized == "No", 3, legality),
         legality = ifelse(Medicinal == "No" & Decriminalized == "Yes", 2, legality),
         legality = ifelse(Medicinal == "CBD Oil Only", 1, legality),
         legality = ifelse(`Legal Status` == "Fully Legal", 5, legality))

# plot map with legality status
# plot_usmap(regions = "states", 
#            data = table.marijuana_legality, 
#            # labels = T,
#            values = "legality") +
#   scale_fill_continuous(low = "white", high = "grey", 
#                         name = "Legality", 
#                         labels = c("Fully Illegal", 
#                                    "CBD with THC Only", 
#                                    "Decriminalized", 
#                                    "Medical", 
#                                    "Medical and Decriminalized", 
#                                    "Legalized")) +
#   labs(title = "U.S. States",
#        subtitle = "Legality Status for U.S. States") + 
#   theme(legend.position = "right")

plot_usmap(regions = "states", 
           data = table.marijuana_legality, 
           # labels = T,
           values = "legality") +
  scale_fill_continuous(color = c("red", "blue", "yellow", "black", "green", "purple"), 
                        name = "Legality", 
                        labels = c("Fully Illegal", 
                                   "CBD with THC Only", 
                                   "Decriminalized", 
                                   "Medical", 
                                   "Medical and Decriminalized", 
                                   "Legalized")) +
  labs(title = "U.S. States",
       subtitle = "Legality Status for U.S. States") + 
  theme(legend.position = "right")


# ********************************************************************************
# ==== 3. NV Marijuana Status ====
# ********************************************************************************
# use data from the package to create the recreational status data
recreactional_status <- countypop %>% 
  filter(abbr == "NV") %>% 
  select(-pop_2015) %>% 
  mutate(`recreational status` = as.factor(ifelse(county %in% c("Churchill County", "Clark County", 
                                                                "Elko County", "Humboldt County", 
                                                                "Lyon County", "Nye County", "Washoe County", 
                                                                "White Pine County"), 1, 0)))

# plot map with recreational status
plot_usmap(regions = "counties", 
           include = c("NV"),
           data = recreactional_status, 
           # labels = T,
           values = "recreational status") +
  scale_fill_grey(start = 1, end = 0.5, name = "Recreational Status") +
  labs(title = "Nevada counties",
       subtitle = "Recreational Status for Nevada counties") + 
  theme(legend.position = "right")

