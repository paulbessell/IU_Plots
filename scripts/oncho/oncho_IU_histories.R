library(tidyverse)
library(readxl)
library(stringr)
library(sf)
library(readr)
setwd("..../IU_Plots") # Update

filter_countries <- c("Seychelles", "Mauritius", "Eswatini", "Madagascar", "Gambia",
                      "Botswana", "Namibia", "Tanzania (Zanzibar)",
                      "Sao Tome and Principe", "Cabo Verde", "South Africa",
                      "Lesotho", "Comoros", "Algeria", "Mauritania", "Egypt",
                      "Djibouti", "Somalia", "Yemen", "Zimbabwe", "Eritrea")

oncho <- read_csv("Data/Oncho/History/Oncho_IU_MDA_202308.csv")  %>%
  mutate(pID = row_number()) # Add a unique id in case it is useful

ius<- read_sf("Data/IUs/ESPEN_IU_2021.shp")
ius_valid <- st_make_valid(ius)

list_new_ius <- read_csv("Data/Oncho/History/List_New_IU.csv")

oncho_parent <- oncho %>%
  left_join(list_new_ius, by = c("IU_ID" = "ESPEN_IU_ID")) # Joining up Jorge's index file

oncho_parent_slice <- oncho_parent %>%
  filter(!is.na(Parent1_IU_ID)) %>%
  group_by(IU_ID, Parent1_IU_ID) %>%
  slice_min(Year) %>% # For those that have a parent we select the first year in which the IU has a record
  filter(Year > 2013)

yearDiff <- oncho_parent_slice$Year - 2013 # Create a vector of the number of years to be filled in

yearSeq <- unlist(sapply(oncho_parent_slice$Year, function(x) 2013:(x-1))) #  the years that go into the inserted years

newDF <- oncho_parent_slice[rep(1:nrow(oncho_parent_slice), yearDiff),] # the dummy data inserted to back faill
newDF$Year <- yearSeq

newDF_Join <- newDF %>% # Create the new columns in the data frame
  left_join(oncho, by = c("Year" = "Year", "Parent1_IU_ID" = "IU_ID"), suffix = c("", "_P")) %>%
  group_by(Parent1_IU_ID, Year) %>%
  mutate(Sisters = n()) # The number of other children from that parent


oncho_bind <- oncho %>% # Put the data back into the main data frame
  bind_rows(newDF_Join) %>%
  arrange(IU_ID, Year) %>%
  mutate("SHP_Present" = ifelse(IU_ID %in% ius$IU_ID, "Present", "Absent")) %>% # Is the IU recorded in the spatial data
  mutate(Included_Ctry = ifelse(ADMIN0 %in% filter_countries, "Exclude", "Include")) # Is the countyr one thta we are including in the oncho list

write_excel_csv(oncho_bind, file = "Oncho/History/oncho_bind_parents.csv", na = "")


oncho_bind_min <- oncho_bind %>% # Checking how many records go back to 2012
  group_by(IU_ID) %>%
  slice_min(Year)

View(oncho_bind_min %>% filter(Year != 2013))

oncho_bind_max <- oncho_bind %>% # Find how many records go to 2022
  group_by(IU_ID) %>%
  slice_max(Year)
View(oncho_bind_min %>% filter(Year != 2022))


