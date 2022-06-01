# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# DESCRIPTION ----
# ______________________________________________________________________________

# Last updated 31 May, 2021 by Trevor Incerti

# LA County uses Socrata API. Database and query details can be found here:
# https://data.lacounty.gov/Parcel-/Assessor-Parcel-Data-2020/42ne-gwcj 
# https://dev.socrata.com/foundry/data.lacounty.gov/42ne-gwcj

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# LIBRARIES ----
# ______________________________________________________________________________

# Load libraries
library("tidyverse")
library("janitor")
library("RSocrata")
library("postmastr")

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# IMPORT PARCEL SHAPEFILE FROM LA COUNTY API ----
# ______________________________________________________________________________

# Read in residential parcels from LA county API: takes about 30 minutes
# parcels <- read.socrata(
#   "https://data.lacounty.gov/resource/42ne-gwcj.json?$where=usecodedescchar2 not in('Single Family Residence')",
#   app_token = "INSERT APP TOKEN HERE",
#   email     = "INSERT EMAIL HERE",
#   password  = "INSERT PASSWORD HERE"
# )

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# CLEAN DATA ----
# ______________________________________________________________________________

parcels_clean <- parcels %>% 
  filter( # Remove non-residential buildings and those without addresses
    usecodedescchar1 == "Residential",
    !is.na(propertylocation)
    ) %>%
  select( # Select needed variables only
    ain, propertylocation, situscity, situszip5, situszip, units, bedrooms, 
    usecodedescchar2, usecodedescchar3, yearbuilt, roll_totalvalue,
    center_lat, center_lon
    ) %>%
  rename( # Rename variables
    id = ain, 
    address = propertylocation, 
    city = situscity, 
    zip5 = situszip5,
    zip9 = situszip, 
    unit_type1 = usecodedescchar2, 
    unit_type2 = usecodedescchar3, 
    value = roll_totalvalue,
    lat = center_lat,
    long = center_lon
    ) %>%
  mutate_at( # Change data types (initally all character)
    c("units", "bedrooms", "value"), as.numeric
  )

parcels_clean <- parcels_clean %>%
  mutate(
    units = na_if(units, 0),
    bedrooms = na_if(bedrooms, 0),
    value = na_if(value, 0),
    yearbuilt = na_if(yearbuilt, 0)
    )

#### Address cleaning ####
parcels_clean <- parcels_clean %>%
  mutate(
    house_number = sub(" .*", "", address),
    number_street = str_split_fixed(address, city, 2),
    number_street = sub(",.*", "", number_street),
    city = sub(" CA.*", "", city),
    state = "CA" # All addresses are in CA
  )

# Split street from house number 
street <- as_tibble(with(parcels_clean, 
                       str_split_fixed(number_street, house_number, n = 2))) %>%
  slice_head(n = nrow(parcels_clean)) %>%
  select(V2) %>%
  rename(street = V2)

parcels_clean <- cbind(parcels_clean, street)

# If yearbuilt is equal to zero, change to NA

# Finalize data frame
parcels_clean <- parcels_clean %>% 
  select(-number_street) %>%
  relocate(house_number, street, city, state, zip5, zip9, .before = units)

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# FILTER DATA TO APARTMENTS/RENTERS ONLY ----
# ______________________________________________________________________________

# Filter on apartments only
apartments <- parcels_clean %>% filter(str_detect(unit_type1, "Apartment"))

# Save as CSV (for use in ArcGIS)
write_csv(apartments, path = "data/cleaned/apartments.csv")


