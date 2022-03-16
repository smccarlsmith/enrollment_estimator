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
lea_lookup <- bind_rows(
  distinct(ade_f1, lea_name, lea_entity_id), 
  distinct(ade_f2, lea_name, lea_entity_id)
) %>% 
  distinct()

# Check names for consistency across years in lea lookup
dup_index <- lea_lookup$lea_entity_id[duplicated(lea_lookup$lea_entity_id)]
# View(lea_lookup[lea_lookup$lea_entity_id %in% dup_index, ]) %>% 
#   arrange(lea_entity_id)

# Bind the format 1 and 2 dataframes
enrollment_all <- bind_rows(
  ade_f1, ade_f2
)

View(enrollment_all[enrollment_all$lea_entity_id %in% dup_index, ]) %>% 
  arrange(lea_entity_id)

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
ggplot(aes(x = fiscal_yr, y = students, color = lea_entity_id)) +
  geom_line() +
  geom_point()) 

