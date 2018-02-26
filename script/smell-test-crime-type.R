
# load packages -----------------------------------------------------------

library(tidyverse)
library(readxl)
# read in data ------------------------------------------------------------

# data from - http://www.gov.scot/Publications/2015/12/5123/downloads

direct_receptions <- read_excel("data/raw/00491417.xlsx", sheet = "Table 11d", skip = 2) %>% 
  rename(crime_type = X__1)

# removing NAs
direct_receptions <- 
  direct_receptions %>% 
  filter(!is.na(crime_type))

# removing summary rows
count(direct_receptions, crime_type) %>% View


# selecting only crime groups
crime_group <- 
direct_receptions %>% 
  filter(crime_type == "Non-sexual crimes of violence" |
         crime_type == "Sexual crimes" |
         crime_type == "Crimes of dishonesty" |
         crime_type == "Fire-raising,vandalism etc" |
         crime_type == "Other crimes" |
         crime_type == "Miscellaneous offences" |
         crime_type == "Motor vehicle offences") %>% 
  gather("year", "n_direct_receptions", 2:11)


# visualizing -------------------------------------------------------------

# plot by crime type
crime_group %>% 
  ggplot(aes(x = year, y = n_direct_receptions, colour = crime_type, group = crime_type)) +
  geom_line() +
  facet_wrap(~ crime_type)


# selecting only crimes
crime_type <- 
direct_receptions %>% 
  filter(crime_type != "Total crimes & offences",
         crime_type != "Crimes",
         crime_type != "Offences",
         crime_type != "Non-sexual crimes of violence",
         crime_type != "Sexual crimes",
         crime_type != "Crimes of dishonesty",
         crime_type != "Fire-raising,vandalism etc",
         crime_type != "Other crimes",
         crime_type != "Miscellaneous offences",
         crime_type != "Motor vehicle offences") %>% 
  gather("year", "n_direct_receptions", 2:11)

# visualizing -------------------------------------------------------------

# plot by crime type
crime_type %>% 
  ggplot(aes(x = year, y = n_direct_receptions, colour = crime_type, group = crime_type)) +
  geom_line() +
  facet_wrap(~ crime_type)

# hmm, something weird is goin gon there with the other crime category

direct_receptions %>% 
  mutate(type_id = row_number()) %>% 
  mutate(crime_type_2 = crime_type) %>% 
  unite(crime_type_2, crime_type_2, type_id) %>% 
  mutate(crime_type = if_else(crime_type == "Other" | crime_type == "Other 1", crime_type_2, crime_type)) %>% 
  filter(crime_type != "Total crimes & offences",
         crime_type != "Crimes",
         crime_type != "Offences",
         crime_type != "Non-sexual crimes of violence",
         crime_type != "Sexual crimes",
         crime_type != "Crimes of dishonesty",
         crime_type != "Fire-raising,vandalism etc",
         crime_type != "Other crimes",
         crime_type != "Miscellaneous offences",
         crime_type != "Motor vehicle offences") %>% 
  gather("year", "n_direct_receptions", 2:11) %>% 
  ggplot(aes(x = year, y = n_direct_receptions, colour = crime_type, group = crime_type)) +
  geom_line() +
  facet_wrap(~ crime_type)

