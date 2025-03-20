
library(tidyverse)
library(readxl)
library(stringr)
library(sf)
library(readr)

setwd("..../IU_Plots") # Update

schisto <- read_csv("Data/Schisto/History/schistoComb_IU.csv")  %>%
  mutate(pID = row_number()) # Add a unique id in case it is useful

filters <- read_excel("Data/Schisto/History/ParentChildFilter.xlsx")
schisto <- schisto %>%
  left_join(filters, by = c("IU_ID", "Year"), suffix = c("", "_Drop")) %>%
  filter(is.na(ADMIN0_Drop)) %>%
  dplyr::select(-ADMIN0_Drop)


ius<- read_sf("Data/IUs/ESPEN_IU_2021.shp") %>%
  mutate(IU_NAME_IC = iconv(IUs_NAME, to='ASCII//TRANSLIT'))
ius_valid <- st_make_valid(ius)

ius_included <- ius

list_new_ius <- read_csv("Data/Oncho/History/List_New_IU.csv")

name_changes <- read_excel("Data/Schisto/History/NameChanges.xlsx")


schisto_parent <- schisto %>%
  left_join(list_new_ius, by = c("IU_ID" = "ESPEN_IU_ID")) # Joining up Jorge's index file

schisto_parent_slice <- schisto_parent %>%
  filter(!is.na(Parent1_IU_ID)) %>%
  group_by(IU_ID, Parent1_IU_ID) %>%
  slice_min(Year) %>% # For those that have a parent we select the first year in which the IU has a record
  filter(Year > 2013)

yearDiff <- schisto_parent_slice$Year - 2013 # Create a vector of the number of years to be filled in

yearSeq <- unlist(sapply(schisto_parent_slice$Year, function(x) 2013:(x-1))) #  the years that go into the inserted years

newDF <- schisto_parent_slice[rep(1:nrow(schisto_parent_slice), yearDiff),] # the dummy data inserted to back faill
newDF$Year <- yearSeq

schisto_status <- schisto %>%
  group_by(IU_ID, Endemicity) %>%
  tally() %>%
  group_by(IU_ID) %>%
  slice_max(n) %>%
  rename("Child_Endemicity" = "Endemicity") %>%
  dplyr::select(-n)

newDF_Join <- newDF %>% # Create the new columns in the data frame
  #left_join(schisto_status, by = "IU_ID") %>%
  left_join(schisto, by = c("Year" = "Year", "Parent1_IU_ID" = "IU_ID", "TargetPop" = "TargetPop"), suffix = c("", "_P")) %>%
  group_by(Parent1_IU_ID, Year, TargetPop) %>%
  mutate(Sisters = n()) # The number of other children from that parent

parents_removed <- read_excel("Data/Schisto/History/Parents_No_Longer_Exist.xlsx") # These are parents that no longer exist after they have kids


schisto_bind <- schisto %>% # Put the data back into the main data frame
  bind_rows(newDF_Join) %>%
  arrange(IU_ID, Year) %>%
  mutate("SHP_Present" = ifelse(IU_ID %in% ius$IU_ID, "Present", "Absent")) %>% # Is the IU recorded in the spatial data
  filter(!IU_ID %in% parents_removed$IU_ID) %>%
  filter(ADMIN0 != "Central African Republic" | SHP_Present == "Present") %>%
  filter(ADMIN0 != "Ethiopia" | SHP_Present == "Present") %>%
  filter(ADMIN0 != "Kenya" | SHP_Present == "Present") # Bulk remove a bunch of rows from CAR and Ethiopia



schisto_bind_mapping <- schisto_bind %>%
  mutate(IUs_ADM_MAPPING = IUs_ADM,
         IUs_NAME_MAPPING = IUs_NAME,
         IU_ID_MAPPING = IU_ID,
         IU_CODE_MAPPING = IU_CODE)


schisto_bind_mapping_Red <- schisto_bind_mapping %>%
  filter(SHP_Present == "Absent") %>% 
  mutate(IUs_NAME_MAPPING = str_to_title(IUs_NAME_MAPPING))


schisto_bind_mapping_Red_Adj <- schisto_bind_mapping_Red %>%
  left_join(ius %>% st_drop_geometry() %>% dplyr::select(ADMIN0, IUs_ADM, IU_ID, IUs_NAME, IU_CODE, IU_NAME_IC),
            by = c("ADMIN0" = "ADMIN0", "IUs_ADM_MAPPING" = "IUs_ADM", "IUs_NAME_MAPPING" = "IU_NAME_IC"), suffix = c("", "_SF"))

schisto_bind_mapping_Red_Adj_Fix <- schisto_bind_mapping_Red_Adj %>%
  mutate(IU_ID_MAPPING = ifelse(is.na(IU_ID_SF), IU_ID_MAPPING, IU_ID_SF),
         IU_CODE_MAPPING = ifelse(is.na(IU_CODE_SF), IU_CODE_MAPPING, IU_CODE_SF)) %>%
  left_join(name_changes %>%
              dplyr::select(IU_ID, NEW_IU_NAME, NEW_IU_ID, NEW_IU_CODE), 
            by = c("IU_ID_MAPPING" = "IU_ID"), suffix = c("", "_NEW")) %>%
  mutate(IUs_NAME_MAPPING = ifelse(is.na(NEW_IU_NAME), IUs_NAME_MAPPING, NEW_IU_NAME),
         IU_ID_MAPPING = ifelse(is.na(NEW_IU_ID), IU_ID_MAPPING, NEW_IU_ID),
         IU_CODE_MAPPING = ifelse(is.na(NEW_IU_CODE), IU_CODE_MAPPING, NEW_IU_CODE)) %>%
  dplyr::select(names(schisto_bind_mapping_Red))

final_df <- schisto_bind_mapping %>%
  filter(SHP_Present == "Present") %>%
  bind_rows(schisto_bind_mapping_Red_Adj_Fix)

final_df_included <- final_df %>%
  mutate(SHP_Present = ifelse(IU_ID_MAPPING %in% ius$IU_ID, "Present", "Absent"))


final_df_included_n <- final_df_included %>%
  group_by(IU_ID_MAPPING) %>%
  tally(name = "Count")

final_df_included <- final_df_included %>% left_join(final_df_included_n, by = "IU_ID_MAPPING") %>%
  arrange(IU_ID_MAPPING, Year)


final_df_included_v2 <- final_df_included %>%
  filter(Count > 10) %>%
  filter(IU_ID == IU_ID_MAPPING) %>%
  bind_rows(final_df_included %>%
              filter(Count <= 10)) %>%
  rename("FirstCount" = "Count")



final_df_included_v2_n <- final_df_included_v2 %>%
  group_by(IU_ID_MAPPING) %>%
  tally(name = "Count")


final_df_included_v2 <- final_df_included_v2 %>% left_join(final_df_included_v2_n, by = "IU_ID_MAPPING") %>%
  arrange(IU_ID_MAPPING, Year)

write_excel_csv(final_df_included_v2, file = "Schisto/History/SchistoCleaned_1.csv", na = "")

