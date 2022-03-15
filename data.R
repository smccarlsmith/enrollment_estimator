# Load libraries
library(rvest)
library(polite)
library(httr)
library(jsonlite)
library(glue)
library(purrr)
library(readxl)

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
      )
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
enrollment_files <- list.files(
    "enrollment_prediction_app/web/enrollment_data", 
    recursive = TRUE, 
    full.names = TRUE
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
  names(enrollment_f1)[str_detect(names(enrollment_f1), "dist.*id$")] <- "dist_entity_id"
  names(enrollment_f1)[str_detect(names(enrollment_f1), "school.*id$")] <- "sch_entity_id"
  
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
      !is.na(dist_entity_id), 
      is.na(sch_entity_id)
    ) %>% 
    # Calculate total students by district
    group_by(dist_entity_id, grade) %>% 
    summarise(students = sum(n, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(fiscal_yr = year)

  return(enrollment_f1)
  }

ade_f1_list <- lapply(
  grep("format_1/ade", enrollment_files, value = T), 
  import_ade_f1
)

ade_f1 <- map_dfr(ade_f1_list, as_tibble)

# Define a function to import format 2 data from ADE
import_ade_f2 <- function(file_path){}

# Quick exploration of data -----
ade_f1 %>% 
  filter(
    dist_entity_id %in% c(
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
ggplot(aes(x = fiscal_yr, y = students, color = dist_entity_id)) +
  geom_line()

