# Load libraries
library(tidyverse)
library(rvest)
library(polite)
library(httr)
library(jsonlite)
library(glue)
library(purrr)
library(readxl)
library(plotly)
library(censusapi)
library(fuzzyjoin)

# Import AZ cities-----
cities_df <- read_csv("enrollment_prediction_app/web/az_cities.csv") %>% 
  # Remove cities with 0 population
  filter(pop2022 > 0) %>% 
  # Add a column with city names appropriately formatted for scraping 
  #(replace spaces and change to lowercase)
  mutate(city_formatted = tolower(str_replace_all(name, " ", "-")))

# Define function to scrape city population data
scrape_city_data <- function(city_name){
  url <- glue("https://worldpopulationreview.com/us-cities/{city_name}-az-population")
  city_data <- read_html(url) %>% 
    html_element(css = "#byPopulation") %>% 
    html_table() %>% 
    mutate(city = city_name)
  
  return(city_data)
}

# If function returns an error, output NULL
scrape_city_data_errors <- possibly(scrape_city_data, otherwise = NULL)

# Create a list of dataframes scraped from the site
cities_data_list <- lapply(cities_df$city_formatted, scrape_city_data_errors)

# Index list for only non-NULL elements
cities_data_list <- cities_data_list[-which(sapply(cities_data_list, is.null))]

# Change all columns in all dataframes to character to prepare for binding
# all_char <- function(x) { x <- mutate(x, across(.fns = as.character()))}
cities_data_list <- lapply(cities_data_list, function(x){
  x <- mutate(x, across(.fns = as.character))
  })

# Bind dataframes together
cities_pop_df <- map_dfr(cities_data_list, as_tibble) %>% 
  left_join(
    select(cities_df, rank, name, density, aland_sqmi, city_formatted), 
    by = c("city" = "city_formatted")
    ) %>% 
  mutate(
    # Remove percent sign from Annual Growth Rate
    `Annual Growth Rate` = str_remove_all(`Annual Growth Rate`, "%"), 
    # Remove commas in Population and Growth Columns
    across(
      .cols = c(Population, Growth), 
      .fns = str_remove_all, 
      ","
      ), 
    across(
      .cols = c(Year, Population, Growth, `Annual Growth Rate`), 
      .fns = as.numeric
      ), 
    # Clean the city names to match across data sources, replace all non-letter characters
    clean_city = str_replace_all(tolower(city), "[^\\w]+", "_")
    )

# investigate missing values before coercing numbers
pop_na <- sum(is.na(cities_pop_df$Population))
growth_na <- sum(is.na(cities_pop_df$Growth))
rate_na <- sum(is.na(cities_pop_df$`Annual Growth Rate`))

pop_na - sum(is.na(cities_pop_df$Population))
growth_na - sum(is.na(cities_pop_df$Growth))
rate_na - sum(is.na(cities_pop_df$`Annual Growth Rate`))

# Write csv of city population data
# write_csv(cities_pop_df, "cities_population.csv") # Backup copy
write_csv(cities_pop_df, "enrollment_prediction_app/web/cities_population.csv")

# Import and clean ADE enrollment data -----
# Download data from https://www.azed.gov/accountability-research/data


# List all files in the enrollment folder
enrollment_files <- str_subset(
  list.files(
    "enrollment_prediction_app/web/enrollment_data", 
    recursive = TRUE, 
    full.names = TRUE
  ), 
  # remove paths to open Excel files
  pattern = "\\$", 
  negate = T
  )

# Define a function to import and clean ADE data
import_ade_f1 <- function(file_path){
  # Extract year from file name
  year <- as.numeric(str_extract(file_path, "\\d{4}"))
  
  # Import and clean the files
  enrollment_f1 <- read_excel(
      file_path, 
      sheet = "SchoolbyGrade", 
      skip = 1) %>% 
    distinct() %>% 
    # Coerce all columns to strings
    mutate(across(.fns = as.character))
  
  # Clean column names
  names(enrollment_f1) <- tolower(
    str_replace_all(
      names(enrollment_f1), 
      " ", "_"
      )
    )
  
  # Rename the district and school entity id columns
  names(enrollment_f1)[str_detect(names(enrollment_f1), "dist.*id$")] <- "lea_entity_id"
  names(enrollment_f1)[str_detect(names(enrollment_f1), "school.*id$")] <- "sch_entity_id"
  names(enrollment_f1)[str_detect(names(enrollment_f1), "^district.*")] <- "lea_name"
  
  # Define a vector of grade columns (some files include PreSchool, some don't)
  columns <- names(enrollment_f1)[names(enrollment_f1) %in% c("ps", "kg", as.character(1:12), "total")]
  
  # Redefine enrollment_f1
  enrollment_f1 <- enrollment_f1 %>% 
    # Convert to long format
    pivot_longer(
      cols = columns, 
      names_to = "grade", 
      values_to = "n", 
      values_drop_na = TRUE) %>% 
    mutate(
      # Remove all the asterisks from redaction
      n = str_remove_all(n, "\\*"), 
      # Convert n to numeric
      n = as.numeric(n)
    ) %>% 
    # Filter out school level rows
    filter(
      (!is.na(lea_entity_id) & lea_entity_id != "_"), # one file uses "_" for missing values
      (is.na(sch_entity_id)|sch_entity_id == "_")
    ) %>% 
    # Calculate total students by district
    group_by(lea_entity_id, lea_name, grade) %>% 
    summarise(students = sum(n, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(fiscal_yr = year)

  return(enrollment_f1)
  }

ade_f1_list <- lapply(
  grep("format_1", enrollment_files, value = T), 
  import_ade_f1
)

ade_f1 <- map_dfr(ade_f1_list, as_tibble)

# Define a function to import format 2 data from ADE
import_ade_f2 <- function(file_path){

  year <- as.numeric(str_extract(file_path, "\\d{4}"))
  
  enrollment_f2 <- read_excel(
    file_path, 
    sheet = "LEA by Grade"
    ) %>% 
    distinct() %>% 
    # Coerce all columns to strings
    mutate(across(.fns = as.character))
  
  # Clean column names
    names(enrollment_f2) <- tolower(
      str_replace_all(
        names(enrollment_f2), 
        " ", "_"
      )
    )

  # Redefine enrollment_f2
  enrollment_f2 <- enrollment_f2 %>% 
    select(-fiscal_year) %>% 
    # Convert to long format
    pivot_longer(
      cols = ps:total, 
      names_to = "grade", 
      values_to = "students", 
      values_drop_na = TRUE) %>% 
    # Remove rows with missing LEA ID
    filter(!is.na(lea_entity_id)) %>% 
    mutate(
      # Remove all the asterisks from redaction
      students = str_remove_all(students, "\\*"), 
      # Convert students to numeric
      students = as.numeric(students)
    ) %>% 
    mutate(fiscal_yr = year)
}

ade_f2_list <- lapply(
  grep("format_2", enrollment_files, value = T), 
  import_ade_f2
)

# Bind the enrollment files from each year
ade_f2 <- map_dfr(ade_f2_list, as_tibble)

# Create a consolidated lookup table for LEA names
# Data shows some entities that changed their name slightly over the years, 
# but the most frequent cause of duplicates is for students tuitioned out.
lea_lookup <- bind_rows(ade_f1, ade_f2) %>% 
  distinct() %>% 
  # Slice the max number of students to get the correct lea name, 
  group_by(lea_entity_id, lea_name, fiscal_yr) %>% 
  summarise(students = sum(students, na.rm = TRUE)) %>% 
  group_by(lea_entity_id, fiscal_yr) %>% 
  slice_max(order_by = students, n = 1, with_ties = FALSE) %>%
  # Slice the max year to get the latest version of that name.
  group_by(lea_entity_id) %>% 
  slice_max(order_by = fiscal_yr, n = 1, with_ties = FALSE) %>% 
  ungroup() %>% 
  select(lea_entity_id, lea_name) %>% 
  distinct() %>% 
  # Create shorter versions of LEA names to make visuals and app inputs easier to use
  mutate(
    lea_name = str_squish(lea_name),
    lea_abbr = str_to_title(lea_name), 
    lea_abbr = str_replace_all(lea_abbr, "Union High School District", "UHSD"), 
    lea_abbr = str_replace_all(lea_abbr, "High School", "HS"), 
    lea_abbr = str_replace_all(lea_abbr, "Elementary.*District", "El."), 
    lea_abbr = str_replace_all(lea_abbr, "Unified District", "Unified"), 
    lea_abbr = str_replace_all(lea_abbr, "School", "Sch."), 
    lea_abbr = str_replace_all(lea_abbr, "District", "Dist."), 
    lea_abbr = str_replace_all(lea_abbr, "Academy", "Acad."),
    lea_abbr = str_replace_all(lea_abbr, "Preparatory", "Prep"),
    lea_abbr = str_replace_all(lea_abbr, "Arizona", "AZ")
  )

# Bind the format 1 and 2 dataframes
enrollment_all <- bind_rows(
  ade_f1, ade_f2
) %>% 
  # Slice the highest value for each entity ID each year
  # That will exclude tuitioned out students since they should not be considered in a school's enrollment.
  group_by(lea_entity_id, grade, fiscal_yr) %>% 
  slice_max(order_by = students, n = 1, with_ties = FALSE) %>% 
  ungroup() %>% 
  select(-lea_name) %>% 
  left_join(lea_lookup, by = "lea_entity_id")


# Quick exploration of data -----
ggplotly(
enrollment_all %>% 
  filter(
    lea_entity_id %in% c(
      4289 #AFUHSD
      ,4281 #LESD
      ,4235 #Mesa
      ,4269 # Buckeye El
      ,4284 # Buckeye U
      ,4288 # Tolleson U
      ,4239 # Gilbert
      ), 
    grade == "total"
    ) %>% 
ggplot(aes(x = fiscal_yr, y = students, color = lea_abbr)) +
  geom_line() +
  geom_point()) 

# Scrape City latitude and longitude data
city_geocodes <- read_html("https://www.mapsofworld.com/usa/states/arizona/lat-long.html") %>% 
  html_element(css = ".geo_facts_td_algn") %>% 
  html_table() %>% 
  mutate(
    # Clean city names for matching across data sources by replacing non-letter characters
    clean_city = str_remove(Location, " city| town"),
    clean_city = str_replace_all(
      str_squish(tolower(clean_city)), 
      "[^\\w]+",
      "_"
    ), 
  )


# Scrape districts by city
lea_cities <- read_html("https://www.greatschools.org/schools/districts/Arizona/AZ/") %>% 
  html_element(css = ".districts-cities-list") %>% 
  html_table()

# Scrape School District Addresses
lea_cities_formatted <- lea_cities %>% 
  # Add a column with URLs
  mutate(
    url_string = paste(
      "https://www.greatschools.org/arizona", 
      City, 
      `District name`, 
      "",
      sep = "/"
      ),
    url_string = str_replace_all(url_string, " ", "-"), 
    url_string = tolower(url_string), 
    # Add an empty column to fill with address information
    info = ""
    )

# Loop through urls to scrape address information and add it to lea_cities_formatted object
for #(d in 1:5) {
  (d in 1:length(lea_cities_formatted$url_string)) {
  lea_info <- read_html(lea_cities_formatted$url_string[d]) %>% 
    # html_element(css = ".container-gs-v2 , .content") %>% 
    html_element(css = ".school-info") %>% 
    html_text() %>% 
    str_squish() 
  
  lea_cities_formatted$info[d] <- lea_info
}

# Define an object to clean addresses and extract entity ID for lookup
lea_addresses_clean <- lea_cities_formatted %>% 
  rename("lea_name" = "District name") %>% 
  mutate(
    lea_name_clean = str_squish(lea_name),
    lea_entity_id = str_extract(str_squish(lea_name), "\\(\\d*\\)"), 
    lea_entity_id = str_remove_all(lea_entity_id, "[\\(\\)]"), 
    lea_address = str_remove(info, fixed(lea_name_clean)), 
    lea_address = str_squish(lea_address),
    lea_phone = str_extract(lea_address, " \\(\\d{3}\\) \\d{3}-\\d{4}"), 
    lea_address = str_squish(str_remove(lea_address, "\\(.*$")), 
    lea_street = str_remove_all(lea_address, glue(", {fixed(str_squish(City), ignore_case = T)}, AZ *\\d+")), 
    lea_zip = str_extract(lea_address, "\\d{5} *$"), 
    lea_state = "AZ", 
    # Clean city names for matching across sources by replacin non-letter characters
    clean_city = str_replace_all(tolower(str_squish(City)), "[^\\w]+", "_")
    ) 



# Review matches and missing values
# View(count(city_data_final, clean_city, Latitude, Longitude))




# Write a csv file for geocoding
write.csv(
  lea_addresses_clean[ , c("lea_street", "City", "lea_state", "lea_zip")], 
  "lea_addresses_for_census.csv", 
  col.names = FALSE)

# Census Bureau
system2(
  command = "curl", 
  args = c(
    "form addressFile=@lea_addresses_for_census.csv", 
    "form benchmark=2020 https://geocoding.geo.census.gov/geocoder/locations/addressbatch", 
    "output geocoderesult.csv"
  )
)

# Import School District Geocode data
lea_geocodes <- read_csv(
  "geocoderesult.csv", 
  skip = 1, 
  col_names = c(
    "record", 
    "original_addr", 
    "match", 
    "exact", 
    "matched_addr", 
    "lon_lat", 
    "id", 
    "rl"
    ) 
  ) %>% 
  # Remove the comma after state code
  mutate(original_addr = str_replace(original_addr, "AZ,", "AZ"))




# Attribution. If I use it, I need to include the following attribution:
# "This product uses the Census Bureau Data API but is not endorsed or certified by the Census Bureau."
saipe_vars <- listCensusMetadata(
  name = "timeseries/poverty/saipe/schdist", 
  type = "variables")
View(saipe_vars)

sahie_national <- getCensus(
  key = keyring::key_get("CENSUS_KEY", "ssmith2@aguafria.org"),
  name = "timeseries/poverty/saipe/schdist",
  vars = c("SD_NAME", "YEAR", "GEOID", "STATE", "LEAID", "GEOCAT"), 
  region = "state:*", 
  regionin = "state:03")
View(sahie_national)


# Check city names for missing entries
# unique(lea_addresses_clean$clean_city[!lea_addresses_clean$clean_city %in% cities_pop_df$clean_city])

# Create a formatted dataframe of city data to use in app
city_data_final <- cities_pop_df %>% 
  left_join(city_geocodes, by = "clean_city") %>% 
  select(name, clean_city, Latitude, Longitude, Year, Population, `Annual Growth Rate`) %>% 
  distinct()

write_csv(city_data_final, "enrollment_prediction_app/web/city_data_app.csv")

# Create a formatted dataframe of enrollment data to use in app
enrollment_data_final <- enrollment_all %>% 
  left_join(select(lea_addresses_clean, -lea_name), by = "lea_entity_id") %>% 
  left_join(lea_geocodes, present = TRUE, by = c("lea_address" = "original_addr")) %>% 
  select(lea_entity_id, lea_name, lea_abbr, City, grade, students, fiscal_yr, lea_address, clean_city, lon_lat) %>% 
  left_join(
    distinct(
      transmute(city_data_final, clean_city, city_lat = Latitude, city_lon = Longitude)
      ), 
    by = "clean_city"
    ) %>% 
  # split latitude and longitude into columns
  separate(lon_lat, into = c("lon", "lat"), sep = ",", convert = TRUE) %>% 
  arrange(lea_name)

write_csv(enrollment_data_final, "enrollment_prediction_app/web/enroll_data_app.csv")
# Inspect missing data in enrollment_data_final object
# View(count(filter(enrollment_data_final, is.na(lon_lat), !is.na(lea_address)), lea_abbr, lea_entity_id, lea_address))
# length((unique(enrollment_data_final$lea_entity_id[is.na(enrollment_data_final$lon_lat)])))
# sum((lea_geocodes$match == "Match"))
# check <- (count(distinct(enrollment_data_final, lea_address, present), lea_address, present))
# sum(check$present, na.rm = T)
# check$lea_address[!check$lea_address %in% lea_geocodes$original_addr]

