################################################################################
# CYO Project
# Emanuela Pannini
# GitHub profile: https://github.com/EmanuelaPannini
################################################################################
# Install all the requires packages
# The repos argument was added because of some errors emerged in the rmd knit phase
if(!require(dplyr)) install.packages('dplyr', repos = 'http://cran.us.r-project.org')
if(!require(lubridate)) install.packages('lubridate', repos = 'http://cran.us.r-project.org')
if(!require(ggplot2)) install.packages('ggplot2', repos = 'http://cran.us.r-project.org')
if(!require(corrplot)) install.packages('corrplot', repos = 'http://cran.us.r-project.org')
if(!require(zoo)) install.packages('zoo', repos = 'http://cran.us.r-project.org')
if(!require(stringr)) install.packages('stringr', repos = 'http://cran.us.r-project.org')
if(!require(tidyr)) install.packages('tidyr', repos = 'http://cran.us.r-project.org')
if(!require(forecast)) install.packages('forecast', repos = 'http://cran.us.r-project.org')
if(!require(tseries)) install.packages('tseries', repos = 'http://cran.us.r-project.org')
if(!require(plotly)) install.packages('plotly', repos = 'http://cran.us.r-project.org')
if(!require(caret)) install.packages('caret', repos = 'http://cran.us.r-project.org')
if(!require(doParallel)) install.packages('doParallel', repos = 'http://cran.us.r-project.org')
if(!require(randomForest)) install.packages('randomForest', repos = 'http://cran.us.r-project.org')
if(!require(xgboost)) install.packages('xgboost', repos = 'http://cran.us.r-project.org')
if(!require(kableExtra)) install.packages('kableExtra', repos = 'http://cran.us.r-project.org')

library(dplyr)
library(lubridate)
library(ggplot2)
library(corrplot)
library(zoo)
library(stringr)
library(tidyr)
library(forecast)
library(tseries)
library(plotly)
library(caret)
library(doParallel)
library(randomForest)
library(xgboost)
library(kableExtra)

set.seed(2024, sample.kind='Rounding')

# Upload the two csv files into R datasets
# The following code uses relative paths, if the files already exists in the
# current directory, or download them from the GitHub repository
load_dataset <- function(filename){

  # Base url to the GitHub repository
  base_url <- 'https://raw.githubusercontent.com/EmanuelaPannini/edx-cyo-project/refs/heads/main/'

  # Concatenate the base url to the current filename
  url <- paste0(base_url, filename)

  # Find the source to retrieve the data
  source <- ifelse(file.exists(filename), filename, url)

  # Return the source
  return(source)
}

# Upload the dataset for energy and weather data
energy_data <- read.csv(load_dataset('energy_dataset.csv'))
weather_data <- read.csv(load_dataset('weather_features.csv'))

# Quick inspection of the data inside this two datasets
head_energy_data <- head(energy_data)
head_weather_data <- head(weather_data)

# As we will be working with time-dependent features, it is important to avoid
# time conversion problems and key mismatches. To do so we set 'CET' as the
# TZ (timezone) environment variable. This change will affect only the current
# session and will not interfere with your default general settings
Sys.setenv(TZ = 'CET')

# The column names use dot ('.') as a separator and this may lead to issues when
# using some R functions, so we replace those with an underscore ('_'),
# adopting snake_case
colnames(energy_data) <- str_replace_all(colnames(energy_data), '\\.', '_')
colnames(weather_data) <- str_replace_all(colnames(weather_data), '\\.', '_')

################################################################################
# Definition of the personal theme to reproduce similar styles between plots
personal_theme <- function(current_plot_type){
  if(current_plot_type == 'histogram')
  {
    theme(
      plot.title = element_text(size = 14, face = 'bold.italic'),
      legend.title = element_text(size = 8, vjust = 1.2),
      legend.key.size = unit(0.4, 'cm'),
      legend.text = element_text(size = 8, face = 'italic'),
      axis.text = element_text(size = 8, face = 'italic'),
      axis.ticks = element_line(colour = 'black'),
      axis.title.x = element_text(size = 9, face = 'italic'),
      axis.title.y = element_text(size = 9, face = 'italic'),
      axis.line = element_line(colour = 'black')
    )
  }
  else
  {
    if(current_plot_type == 'bar')
    {
      theme(
        plot.title = element_text(size = 14, face = 'bold.italic'),
        legend.title = element_text(size = 8, vjust = 1.2),
        legend.position = 'none',
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 8, face = 'italic'),
        axis.ticks.y = element_line(colour = 'black'),
        axis.title.x = element_text(size = 9, face = 'italic'),
        axis.title.y = element_text(size = 9, face = 'italic'),
        axis.line = element_line(colour = 'black')
      )
    }
    else
    {
      if(current_plot_type == 'bar_legend')
      {
        theme(
          plot.title = element_text(size = 14, face = 'bold.italic'),
          legend.title = element_text(size = 8, hjust = 0.5),
          legend.text = element_text(size = 6, face = 'italic'),
          legend.position = 'bottom',
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 8, face = 'italic'),
          axis.ticks.y = element_line(colour = 'black'),
          axis.title.x = element_text(size = 9, face = 'italic'),
          axis.title.y = element_text(size = 9, face = 'italic'),
          axis.line = element_line(colour = 'black')
        )
      }
      else
      {
        theme(
          plot.title = element_text(size = 14, face = 'bold.italic'),
          legend.title = element_text(size = 8, vjust = 1.2),
          legend.key.size = unit(0.6, 'cm'),
          legend.text = element_text(size = 8, face = 'italic'),
          axis.text = element_text(size = 8, face = 'italic'),
          axis.ticks = element_line(colour = 'black'),
          axis.title = element_text(size = 9, face = 'italic'),
          axis.line = element_line(colour = 'black'),
          panel.background = element_blank()
        )
      }
    }
  }
}

# Definition of the personal palettes for the legends
cities_palette <- c('#fb9a99', '#cab2d6', '#6a3d9a', '#1f78b4', '#a6cee3')
years_palette <- c('#ca0020', '#f4a582', '#92c5de', '#0571b0')

################################################################################
# Observations on energy data

# Retrieve the number of rows and columns
nrow_energy_data <- nrow(energy_data)
ncol_energy_data <- ncol(energy_data)

# Inspect the different columns and classes of the energy dataset
energy_classes <- sapply(energy_data, class)
energy_schema <- data.frame(column_name = names(energy_classes), class = energy_classes)
# This last line is necessary for visualisation purposes
rownames(energy_schema) <- NULL

# Correction of the time column
# Since time is initially stored as a character column and not a datetime, we 
# convert it. In this case we are using the UTC timezone to avoid dealing with 
# the daylight saving time present in the CET/CEST timezones and the days of 
# change of hour in March and October
energy_data <- energy_data %>% mutate(time = ymd_hms(time, tz = 'UTC'))

# Definition of a custom summary
# This was performed as I was not satisfied on how the information were shown in
# summary(energy_data)
personal_summary <- function(current_dataset){

  # Export the general dataset information like names and classes
  general_information <- data.frame(
    names = names(current_dataset),
    class = sapply(current_dataset,
                   function(column){
                     paste(class(column),
                           collapse = ', ')
                   }))

  # Export the statistics, divided coherently for each type of classes
  general_summary <- sapply(current_dataset, function(column){
    if(is.numeric(column))
    {
      c(
        min = round(min(column, na.rm = TRUE), 2),
        max = round(max(column, na.rm = TRUE), 2),
        mean = round(mean(column, na.rm = TRUE), 2),
        median = round(median(column, na.rm = TRUE), 2),
        sd = round(sd(column, na.rm = TRUE), 2),
        unique_values = NA,
        num_NAs = sum(is.na(column))
      )
    }
    else if(is.POSIXt(column))
    {
      c(
        min = as.character(min(column, na.rm = TRUE)),
        max = as.character(max(column, na.rm = TRUE)),
        mean = NA,
        median = NA,
        sd = NA,
        unique_values = NA,
        num_NAs = sum(is.na(column))
      )
    }
    else if(is.character(column))
    {
      # The values are transformed to characters to provide better
      # visualisations without altering the standard behaviour
      c(
        min = as.character(min(nchar(column), na.rm = TRUE)),
        max = as.character(max(nchar(column), na.rm = TRUE)),
        mean = as.character(round(mean(nchar(column), na.rm = TRUE), 2)),
        median = as.character(median(nchar(column), na.rm = TRUE)),
        sd = as.character(sd(mean(nchar(column), na.rm = TRUE), 2)),
        unique_values = round(length(unique(column)), 2),
        num_NAs = sum(is.na(column))
      )
    }
    else
    {
      c(
        min = NA,
        max = NA,
        mean = NA,
        median = NA,
        sd = NA,
        unique_values = NA,
        num_NAs = sum(is.na(column))
      )
    }
  })

  # Return the custom summary as a dataset
  # The perc_NAs column is added to show the percentage of NAs values
  # This column is formatted as string to avoid changing the default settings
  # related to exponential visualisation of decimals
  columns_summary <- as.data.frame(t(general_summary), stringsAsFactors = FALSE) %>%
    mutate(perc_NAs = format(round(as.numeric(num_NAs)*100/nrow(current_dataset), 2),
                             nsmall = 2,
                             scientific = FALSE))

  final_summary <- cbind(general_information, columns_summary)
  rownames(final_summary) <- NULL

  return(final_summary)
}

# Personal energy summary
energy_summary <- personal_summary(energy_data)

# Inspect if there are duplicated rows
energy_duplicated <- sum(duplicated(energy_data))

# Filter-out columns with all NAs, zeros and forecast that are not in the scope
energy_data <- energy_data %>% select(-c(generation_fossil_coal_derived_gas,
                                         generation_fossil_oil_shale,
                                         generation_fossil_peat,
                                         generation_geothermal,
                                         generation_hydro_pumped_storage_aggregated,
                                         generation_marine,
                                         generation_wind_offshore,
                                         forecast_solar_day_ahead,
                                         forecast_wind_offshore_eday_ahead,
                                         forecast_wind_onshore_day_ahead,
                                         total_load_forecast))

# Check if the sum of generation is typically above or below with respect to the total_load
generation_load_plot <- energy_data %>%
                          mutate(sum_generation = rowSums(select(.,
                                    generation_biomass:generation_wind_onshore),
                                    na.rm = TRUE),
                                 delta = sum_generation - total_load_actual) %>%
                          group_by(delta) %>%
                          summarise(num = n()) %>%
                          ggplot(aes(delta)) +
                          geom_histogram(binwidth = 700,
                                         colour = 'black',
                                         fill = '#92c5de') +
                          scale_y_sqrt() +
                          labs(
                            x = 'delta',
                            y = 'number of observations'
                          ) +
                          personal_theme('histogram')

# Interpolate all NAs
energy_data <- energy_data %>%
                mutate(across(generation_biomass:total_load_actual, na.approx))

# Verify that there are no NAs in the dataset
check_energy_NA <- sum(is.na(energy_data))

# Correlation analysis
energy_data_correlation <- energy_data %>%
                            select(- time) %>%
                            rename_with(~ gsub('generation_', 'gen_', .)) %>%
                            rename_with(~ gsub('_consumption', '_cons', .)) %>%
                            cor(use = 'pairwise.complete.obs')

# Plot the results to achieve a better insight
energy_data_corr <- corrplot(energy_data_correlation,
                               method = 'color',
                               outline = T,
                               order = 'FPC',
                               tl.col = 'black',
                               tl.cex = 0.7,
                               tl.srt = 60,
                               col = COL2('RdYlBu', n = 100))

# Save the plot in order to incorporate it in the pdf file
energy_data_corr_plot <- recordPlot()

# Remove price_day_ahead column
energy_data <- energy_data %>% select(-c(price_day_ahead))

################################################################################
# Observation on weather data

# Find the number of rows and columns
nrow_weather_data <- nrow(weather_data)
ncol_weather_data <- ncol(weather_data)

# Like we did with the energy dataset, inspect the different columns and classes 
# of the weather dataset 
weather_classes <- sapply(weather_data, class)
weather_schema <- data.frame(column_name = names(weather_classes), class = weather_classes)
rownames(weather_schema) <- NULL

# Correction of the dt_iso column (analogous to the time column in the energy dataset)
weather_data <- weather_data %>% mutate(dt_iso = ymd_hms(dt_iso, tz = 'UTC'))

# Inspect the values in the city_name column
distinct_cities <- weather_data %>% distinct(city_name)

# Correction of the city_name column, by removing leading/trailing blank spaces
weather_data <- weather_data %>% mutate(city_name = str_trim(city_name))

# Weather custom summary
weather_summary <- personal_summary(weather_data)

# Inspect if there are duplicated rows
weather_duplicated <- sum(duplicated(weather_data))

# Remove duplicated rows
weather_data <- distinct(weather_data)

# weather_id, weather_main, weather_description and weather_icon analysis

# Extrapolate from the weather data some relevant information like the
# ones related to day/night and the general icon code
weather_string_information <- weather_data %>%
                                mutate(simplified_icon = str_replace(weather_icon,
                                                                     '[nd]', ''),
                                       icon_day_night = str_replace_all(
                                         weather_icon, '\\d', ''),
                                       hour = hour(with_tz(dt_iso, tzone = 'CET')),
                                       day_night = case_when(
                                         (hour <= 5) ~ 'n',
                                         (hour >= 10 & hour <= 16) ~ 'd',
                                         .default = '')) %>%
                                select(weather_id,
                                       weather_main,
                                       weather_description,
                                       weather_icon,
                                       simplified_icon,
                                       icon_day_night,
                                       day_night) %>%
                                distinct()

# Verify that the hours (day, night) are effectively the ones stated above, when
# present
test_day_night_hours <- weather_string_information %>%
                          select (day_night, icon_day_night) %>%
                          distinct() %>%
                          arrange(day_night, icon_day_night)

# Show that the weather id does not always match with the simplified icon,
# even if for the majority of cases it does
match_weather_id_icon <- weather_string_information %>%
                          select (weather_id, simplified_icon) %>%
                          group_by(weather_id) %>%
                          summarize(num_icon = n_distinct(simplified_icon)) %>%
                          filter(num_icon > 1) %>% pull(weather_id)

# Show the combination of features that have more than one simplified_icon for
# the same weather_id
mismatch_weather_id_icon <- weather_string_information %>%
                              filter(weather_id %in% match_weather_id_icon) %>%
                              select(weather_id, weather_main, weather_description, simplified_icon) %>%
                              distinct() %>%
                              arrange(weather_id, weather_main, weather_description, simplified_icon)

# As they are always the same, try to understand if it is related to one of the
# other non-numerical data
check_weather_mismatch <- weather_data %>%
                            filter(weather_id %in% match_weather_id_icon) %>%
                            group_by(weather_icon) %>%
                            summarize(min_date = min(dt_iso),
                                      max_date = max(dt_iso),
                                      num_city = n_distinct(city_name)) %>%
                            arrange(weather_icon)

# Check which kind of information add the weather_description feature
# Check if for each weather_id there is just one weather_description
weather_id_description <- weather_string_information %>%
                            group_by(weather_id) %>%
                            summarize(num_description = n_distinct(weather_description)) %>%
                            filter(num_description > 1) %>%
                            select(weather_id) %>% pull()

# Check if for each weather_description there is just one weather_id
weather_description_id <- weather_string_information %>%
                            group_by(weather_description) %>%
                            summarize(num_id = n_distinct(weather_id),
                                      max_ids = max(num_id)) %>%
                            distinct(max_ids) %>%
                            pull()

# Filter only on weather_id that has more than one description and analyse the
# frequency inside the dataframe
weather_id_desc_filt <- weather_data %>%
                          filter(weather_id %in% weather_id_description) %>%
                          group_by(weather_id, weather_description) %>%
                          mutate(weather_description = reorder(weather_description, weather_id),
                                 weather_description = as.factor(weather_description),
                                 weather_id = as.factor(weather_id)) %>%
                          ggplot(aes(weather_description, fill = weather_id)) +
                          geom_bar() +
                          scale_y_sqrt() +
                          scale_fill_manual(values = cities_palette) +
                          labs(
                            x = 'weather description',
                            y = 'number of observations',
                            fill = 'weather id'
                          ) +
                          personal_theme('histogram') +
                          theme(
                            axis.text.x = element_text(angle = 55, hjust = 1)
                          )

# Check the general behaviour between different weather_description and weather_id
weather_id_desc <- weather_data %>%
                    group_by(weather_id, weather_description) %>%
                    mutate(weather_description = reorder(weather_description, weather_id),
                           weather_description = as.factor(weather_description),
                           weather_id = as.factor(weather_id)) %>%
                    ggplot(aes(weather_description, fill = weather_id)) +
                    geom_bar(colour = 'black') +
                    scale_y_sqrt() +
                    labs(
                      x = 'weather description',
                      y = 'number of observations',
                    ) +
                    personal_theme('bar')

# Check the connection between weather_main and weather_description
weather_main_description <- weather_string_information %>%
                              select (weather_main, weather_description) %>%
                              distinct() %>%
                              arrange(weather_main, weather_description)

# Check which kind of information add the weather_description feature
weather_id_main <- weather_data %>%
                    group_by(weather_id, weather_main) %>%
                    mutate( weather_main = as.factor(weather_main),
                            weather_id = as.factor(weather_id)) %>%
                    ggplot(aes(weather_id, fill = weather_main)) +
                    geom_bar(colour = 'black') +
                    scale_y_sqrt() +
                    labs(
                      x = 'weather id',
                      y = 'number of observations',
                      fill = 'weather main'
                    ) +
                    personal_theme('bar_legend')

# Remove these features from the weather dataset
weather_data <- weather_data %>% select(-c( weather_main,
                                            weather_description,
                                            weather_icon)) %>% distinct()

# Check if we still have duplicates after having removed the weather_id
weather_data_doubles <- weather_data %>%
                          select(- weather_id) %>%
                          distinct() %>%
                          group_by(city_name) %>%
                          summarize(num_rows = n())

# Check correlation between different features
# Even though it is not really coherent to verify this aspect for character 
# columns, we decided to add city_name and weather_id for completeness
weather_data_correlation <- weather_data %>% select(- dt_iso) %>%
                              mutate(city_name = as.numeric(as.factor(city_name))) %>%
                              cor()

# Plot the results to achieve a better insight
weather_data_corr <- corrplot(weather_data_correlation,
                             method = 'color',
                             outline = T,
                             order = 'hclust',
                             tl.col = 'black',
                             tl.cex = 0.7,
                             tl.srt = 60,
                             col = COL2('RdYlBu', n = 100))

# Save the plot in order to incorporate it in the pdf file
weather_data_corr_plot <- recordPlot()

# Temperature (K)
# Plot the curve related to temperature values
temperature_plot <- weather_data %>%
                      ggplot(aes(temp, fill = city_name)) +
                      geom_histogram(binwidth = 1,
                                     colour = 'black') +
                      scale_fill_manual(values = cities_palette) +
                      labs(
                        x = 'temperature',
                        y = 'number of observations',
                        fill = 'city name'
                      ) +
                      personal_theme('histogram')

# Plot the curve related to minimum temperature values
min_temperature_plot <- weather_data %>%
                          ggplot(aes(temp_min, fill = city_name)) +
                          geom_histogram(binwidth = 1,
                                         colour = 'black') +
                          scale_fill_manual(values = cities_palette) +
                          labs(
                            x = 'minimum temperature',
                            y = 'number of observations',
                            fill = 'city name'
                          ) +
                          personal_theme('histogram')

# Plot the curve related to maximum temperature values
max_temperature_plot <- weather_data %>%
                          ggplot(aes(temp_max, fill = city_name)) +
                          geom_histogram(binwidth = 1,
                                         colour = 'black') +
                          scale_fill_manual(values = cities_palette) +
                          labs(
                            x = 'maximum temperature',
                            y = 'number of observations',
                            fill = 'city name'
                          ) +
                          personal_theme('histogram')

# Coherence of temp against temp_min and temp_max
temp_greater_max <- weather_data %>%
                      filter(temp < temp_min) %>%
                      summarize(n = n()) %>%
                      pull()

temp_less_min <- weather_data %>%
                  filter(temp > temp_max) %>%
                  summarize(n = n()) %>%
                  pull()

# Check if all the values of temperature are within an acceptable range
min_acceptable_temp <- 241.15
max_acceptable_temp <- 320.75

check_temperature_range <- weather_data %>%
                            filter(temp < min_acceptable_temp | temp > max_acceptable_temp) %>%
                            summarize(num_obs = n()) %>% pull(num_obs)

# Pressure (hPa)
# Check if all the values of pressure are within an acceptable range
min_acceptable_pressure <- 950
max_acceptable_pressure <- 1050.6

check_pressure_range <- weather_data %>%
                          filter(pressure < min_acceptable_pressure |
                                   pressure > max_acceptable_pressure) %>%
                          group_by(city_name) %>%
                          summarize(num_obs = n())

# Plot with only acceptable values
pressure_plot <- weather_data %>%
                  filter(pressure < max_acceptable_pressure &
                           pressure > min_acceptable_pressure) %>%
                  ggplot(aes(pressure, fill = city_name)) +
                  geom_histogram(binwidth = 2,
                                 colour = 'black') +
                  scale_fill_manual(values = cities_palette) +
                  labs(
                    x = 'pressure',
                    y = 'number of observations',
                    fill = 'city name'
                  ) +
                  personal_theme('histogram')

# Decide to replace the values outside the range with NA and then interpolate
weather_data <- weather_data %>%
                mutate(pressure = ifelse(
                                          pressure < min_acceptable_pressure | 
                                          pressure > max_acceptable_pressure,
                                          NA,
                                          pressure
                                        ))

# Number of NAs in the pressure feature after the replacement
num_na_pressure <- sum(is.na(weather_data$pressure))

# Create an lm fit for pressure, this time without using the na.approx 
# method from the zoo package
lm_pressure <- lm(pressure ~ as.numeric(dt_iso), data = weather_data, na.action = na.exclude)

# Update weather_data with the correct values for pressure, when necessary
weather_data <- weather_data %>%
                  mutate(pressure = ifelse(is.na(pressure),
                                      predict(lm_pressure,
                                              newdata  = weather_data),
                                              pressure))

# Check again if the values are all in the correct range
check_pressure_range_2 <- weather_data %>%
                          filter(pressure < min_acceptable_pressure |
                                   pressure > max_acceptable_pressure |
                                   is.na(pressure)) %>%
                          summarize(num_obs = n()) %>%
                          pull()

# Humidity (%)
# General plot
# As they are between 0 and 100, no further activity is needed
humidity_plot <- weather_data %>%
                  ggplot(aes(humidity, fill = city_name)) +
                  geom_histogram(binwidth = 1,
                  colour = 'black') +
                  scale_y_sqrt() +
                  scale_fill_manual(values = cities_palette) +
                  labs(
                    x = 'humidity',
                    y = 'number of observations',
                    fill = 'city name'
                  ) +
                  personal_theme('histogram')

# Wind 
# Wind speed (m/s)
# Define the min value to classify a tornado accordingly with Fujita scale
fujita_f0 <- 18
fujita_f1 <- 33
fujita_f2 <- 50

# Find the city with values outside the range
city_outside_wind_speed <- weather_data %>%
                            filter(wind_speed > fujita_f0) %>%
                            distinct(city_name) %>%
                            pull()

# Find the range where these values are distributed
dt_iso_outside_wind_speed <- weather_data %>%
                              filter(wind_speed > fujita_f0) %>%
                              summarize(min = min(dt_iso),
                                        max = max(dt_iso))

# Find the point of maximum wind speed
# This variable is used only to beautify the plot below
max_wind_speed <- weather_data %>%
                  filter(wind_speed == max(wind_speed)) %>%
                  select(dt_iso, wind_speed)

# Plot the values to understand the distribution
wind_speed_outsider <- weather_data %>%
                        filter(city_name %in% city_outside_wind_speed &
                                 dt_iso >= dt_iso_outside_wind_speed$min - days(100) &
                                 dt_iso <= dt_iso_outside_wind_speed$max + days(100)) %>%
                        ggplot(aes(dt_iso, wind_speed)) +
                        geom_point(size = 0.8, colour = '#2166ac') +
                        geom_line(aes(y = fujita_f0), colour = '#b2182b') +
                        geom_line(aes(y = fujita_f1), colour = '#b2182b') +
                        geom_line(aes(y = fujita_f2), colour = '#b2182b') +
                        geom_ribbon(aes(ymin = fujita_f0, ymax = fujita_f1),
                                    fill = '#fddbc7',
                                    alpha = 0.5) +
                        geom_ribbon(aes(ymin = fujita_f1, ymax = fujita_f2),
                                    fill = '#ef8a62',
                                    alpha = 0.5) +
                        annotate('point',
                                 x = max_wind_speed$dt_iso,
                                 y = max_wind_speed$wind_speed,
                                 colour = '#b2182b',
                                 size = 5,
                                 stroke = 1.2,
                                 shape = 22) +
                        annotate('text',
                                 x = dt_iso_outside_wind_speed$max,
                                 y = fujita_f0 + 9,
                                 label = 'Fujita level F0',
                                 fontface  = 'italic',
                                 hjust = 1,
                                 size = 3,
                                 colour = '#b2182b') +
                        annotate('text',
                                 x = dt_iso_outside_wind_speed$max,
                                 y = fujita_f1 + 9,
                                 label = 'Fujita level F1',
                                 fontface  = 'italic',
                                 hjust = 1,
                                 size = 3,
                                 colour = '#b2182b') +
                        scale_y_continuous(breaks = seq(0, 120, by = 30)) +
                        labs(
                          x = 'years',
                          y = 'wind speed'
                        ) +
                        personal_theme('histogram')

# Define the acceptable range
min_acceptable_wind_speed <- 0
max_acceptable_wind_speed <- fujita_f1

# Plot the wind_speed
wind_speed_plot <- weather_data %>%
                    filter(wind_speed > min_acceptable_wind_speed &
                             wind_speed < max_acceptable_wind_speed) %>%
                    ggplot(aes(wind_speed, fill = city_name)) +
                    geom_histogram(binwidth = 2,
                                   colour = 'black') +
                    scale_y_log10() +
                    scale_fill_manual(values = cities_palette) +
                    labs(
                      x = 'wind speed',
                      y = 'number of observations',
                      fill = 'city name'
                    ) +
                    personal_theme('histogram')

# Replace values outside the acceptable range with NAs
weather_data <- weather_data %>%
                  mutate(wind_speed = ifelse(
                                        wind_speed < min_acceptable_wind_speed |
                                        wind_speed > max_acceptable_wind_speed,
                                        NA,
                                       wind_speed
                                      ))

# Define a subset of the dataframe with only the relevant observations
weather_data_Valencia <- weather_data %>%
                          filter(city_name == 'Valencia') %>%
                          select(dt_iso, wind_speed)

# Define a linear model to interpolate the NAs
# Again, a different approach may be better, however we decided to keep it simple
lm_wind_speed <- lm(wind_speed ~ as.numeric(dt_iso),
                    data = weather_data_Valencia,
                    na.action = na.exclude)

# Interpolate the values with the predictions, when needed
weather_data <- weather_data %>%
                  mutate(wind_speed = ifelse(is.na(wind_speed),
                                           predict(lm_wind_speed,
                                                   newdata  = weather_data_Valencia),
                                           wind_speed))

# Check the wind speed range after the interpolation
check_wind_speed_range_2 <- weather_data %>%
                              filter(wind_speed < min_acceptable_wind_speed |
                                       wind_speed > max_acceptable_wind_speed |
                                       is.na(wind_speed)) %>%
                              summarize(num_obs = n()) %>%
                              pull()

# Wind direction (degrees)
wind_deg_plot <- weather_data %>%
                    ggplot(aes(wind_deg, fill = city_name)) +
                    geom_histogram(binwidth = 5,
                                   colour = 'black') +
                    scale_x_continuous(breaks = seq(0, 360, by = 90)) +
                    scale_y_log10() +
                    scale_fill_manual(values = cities_palette) +
                    labs(
                      x = 'wind direction',
                      y = 'number of observations',
                      fill = 'city name'
                    ) +
                    personal_theme('histogram')

# Rain (mm)
# Rain_1h
# Check if all the values of rain_1h are within an acceptable range
min_acceptable_rain_1h <- 0
max_acceptable_rain_1h <- 50

check_rain_1h_range <- weather_data %>%
                        filter(rain_1h < min_acceptable_rain_1h |
                                rain_1h > max_acceptable_rain_1h) %>%
                        summarize(num_obs = n()) %>% pull()

# Plot the rain_1h
rain_1h_plot <- weather_data %>%
                  ggplot(aes(rain_1h, fill = city_name)) +
                  geom_histogram(binwidth = 0.5,
                                 colour = 'black') +
                  scale_y_sqrt() +
                  scale_fill_manual(values = cities_palette) +
                  labs(
                    x = 'rain in 1 hour',
                    y = 'number of observations',
                    fill = 'city name'
                  ) +
                  personal_theme('histogram')

# Check if all the values of rain_3h are within an acceptable range, 
# accordingly with the ones above
min_acceptable_rain_3h <- 3 * min_acceptable_rain_1h
max_acceptable_rain_3h <- 3 * max_acceptable_rain_1h

# Check if all the values of rain_3h are within an acceptable range
check_rain_3h_range <- weather_data %>%
                        filter(rain_3h < min_acceptable_rain_3h |
                                 rain_3h > max_acceptable_rain_3h) %>%
                        summarize(num_obs = n()) %>% pull()

# Plot the values for rain_3h
rain_3h_plot <- weather_data %>%
                  ggplot(aes(rain_3h, fill = city_name)) +
                  geom_histogram(binwidth = 0.25,
                                 colour = 'black') +
                  scale_y_log10() +
                  scale_fill_manual(values = cities_palette) +
                  labs(
                    x = 'rain in 3 hours',
                    y = 'number of observations',
                    fill = 'city name'
                  ) +
                  personal_theme('histogram')

# Coherence rain_1h and rain_3h
rain_1h_great_3h <- weather_data %>%
                      filter(rain_1h > rain_3h) %>%
                      mutate(delta = rain_1h - rain_3h) %>%
                      group_by(delta) %>%
                      summarize(num_observations = n()) %>%
                      arrange(desc(delta))

# Snow (mm)
# Check if all the values of snow_3h are within an acceptable range
min_acceptable_snow_3h <- 0
max_acceptable_snow_3h <- 50

check_snow_3h_range <- weather_data %>%
                        filter(snow_3h < min_acceptable_snow_3h |
                                 snow_3h > max_acceptable_snow_3h) %>%
                        summarize(num_obs = n()) %>% pull()

# General plot
snow_plot <- weather_data %>%
              ggplot(aes(snow_3h, fill = city_name)) +
              geom_histogram(binwidth = 0.5,
                             colour = 'black') +
              scale_y_log10() +
              scale_fill_manual(values = cities_palette) +
              labs(
                x = 'snow 3 hours',
                y = 'number of observations',
                fill = 'city name'
              ) +
              personal_theme('histogram')

# Clouds (%)
# General plot
clouds_plot <- weather_data %>%
                ggplot(aes(clouds_all, fill = city_name)) +
                geom_histogram(binwidth = 1,
                               colour = 'black') +
                scale_y_sqrt() +
                scale_fill_manual(values = cities_palette) +
                labs(
                  x = 'snow rs',
                  y = 'number of observations',
                  fill = 'city name'
                ) +
                personal_theme('histogram')

# Update weather_data to keep only the selected features
weather_data <- weather_data %>%
                  select(- c(temp_min, temp_max, rain_3h, weather_id)) %>%
                  distinct()

# Verify that now all the cites have the expected number of information
check_doubles <- weather_data %>%
                  group_by(city_name) %>%
                  summarize(num_obs = n())

# Verify the general coverage of these cities if compared to Spain population by year
city_population <- data.frame(years = c(2015, 2016, 2017, 2018),
                               Madrid = c(6221000, 6312000, 6404000, 6497000),
                               Barcelona = c(5275000, 5348000, 5421000, 5494000),
                               Bilbao = c(352000, 352000, 352000, 352000),
                               Seville = c(703000, 704000, 706000, 707000),
                               Valencia = c(813000, 818000, 824000, 830000),
                               Spain = c(46431342, 46473315, 46584170, 46792043)) %>%
                      mutate(city_perc = round((Madrid + Barcelona + Bilbao +
                                                  Seville + Valencia)/Spain, 2),
                             perc_Madrid = round(Madrid/ (Madrid + Barcelona + Bilbao + Seville + Valencia), 2),
                             perc_Barcelona = round(Barcelona/ (Madrid + Barcelona + Bilbao + Seville + Valencia), 2),
                             perc_Seville =  round(Seville/ (Madrid + Barcelona + Bilbao + Seville + Valencia), 2),
                             perc_Valencia =  round(Valencia/ (Madrid + Barcelona + Bilbao + Seville + Valencia), 2),
                             # Perform this subtraction in order to be sure that it sums to 1 and
                             # limit the rounding error. Bilbao was chosen as it is the smallest one
                             perc_Bilbao =  1 - perc_Madrid - perc_Barcelona - perc_Seville - perc_Valencia)

# Pivot in order to scale the values accordingly with the population
city_percentages <- city_population %>%
                      select(years, perc_Madrid, perc_Barcelona, perc_Bilbao,
                              perc_Seville, perc_Valencia) %>%
                      pivot_longer(!years,
                                   names_to = 'city',
                                   values_to = 'percentual',
                                   names_prefix = 'perc_')

# Add the information related to the year in weather_data so that we can match
# the city_percentages values and scale them
weather_data <- weather_data %>%
                  mutate(spain_time = as.POSIXct(dt_iso, tz = 'CET'),
                          year = year(spain_time))

# Final form of the weather data before joining the energy information
weather_data <- inner_join(weather_data,
                           city_percentages,
                           by = c('city_name' = 'city',
                                  'year' = 'years')) %>%
                # We decided to not introduce new columns name in order to
                # keep next step simpler
                mutate(across(temp:clouds_all, ~ . * percentual)) %>%
                group_by(dt_iso, spain_time, year) %>%
                summarize(across(temp:clouds_all, sum))

################################################################################
# Aggregated dataset that joins the energy information to the weather ones
energy_weather_dataset <- inner_join(energy_data,
                                     weather_data,
                                     by = c('time' = 'dt_iso'))

################################################################################
# Seasonality analysis - focus on timeseries

# As we will need additional features, we define some simple functions that
# can help to clarify the next steps
# Function to determine if a date is on the weekend (Saturday/Sunday)
is_weekend <- function(date) {
  return(wday(date) %in% c(1, 7))
}

# Function to determine if it is the Friday before Easter, as in Spain is holiday
is_easter_friday <- function(date) {
  return(date %in% c('2015-04-03', '2016-03-25', '2017-04-14', '2018-03-30'))
}

# Function to determine if a date is a national holiday in Spain not included in
# the cases above
is_holiday <- function(date) {
  return(format(date, '%m-%d') %in% c('01-01', '01-06', '05-01', '08-15', '10-12', '11-01', '12-06', '12-08', '12-25'))
}

# Function to determine whether a day is a business day, accordingly with the
# previous conditions
is_business_day <- function(date) {
  return(is_weekend(date) | is_easter_friday(date) | is_holiday(date))
}

# Derive the additional features from the spain_time column
energy_weather_dataset <- energy_weather_dataset %>%
                            mutate(
                              date = as_date(spain_time),
                              month = month(date),
                              day = day(date),
                              hour = hour(spain_time),
                              day_of_year = yday(date),
                              week_of_year = week(date),
                              day_of_week = wday(date),
                              business_day = if_else(is_business_day(date), 0, 1)
                            )

# Reorder the columns, just for visual adjustments
energy_weather_dataset <- energy_weather_dataset %>%
                            select(spain_time,
                                   year,
                                   month,
                                   day,
                                   hour,
                                   day_of_year,
                                   week_of_year,
                                   day_of_week,
                                   business_day,
                                   generation_biomass,
                                   generation_fossil_brown_coal_lignite,
                                   generation_fossil_gas,
                                   generation_fossil_hard_coal,
                                   generation_fossil_oil,
                                   generation_hydro_pumped_storage_consumption,
                                   generation_hydro_run_of_river_and_poundage,
                                   generation_hydro_water_reservoir,
                                   generation_nuclear,
                                   generation_other,
                                   generation_other_renewable,
                                   generation_solar,
                                   generation_waste,
                                   generation_wind_onshore,
                                   total_load_actual,
                                   temp,
                                   pressure,
                                   humidity,
                                   wind_speed,
                                   wind_deg,
                                   rain_1h,
                                   snow_3h,
                                   clouds_all,
                                   price_actual)

# Simple plot to show how the price (mean) is affected by the day of the year
daily_prices <- energy_weather_dataset %>%
                  group_by(day_of_year, year) %>%
                  summarize(avg_price = mean(price_actual)) %>%
                  ggplot(aes(day_of_year, avg_price, colour = as.factor(year))) +
                  geom_point() +
                  scale_x_continuous(breaks = seq(0, 366, by = 30)) +
                  scale_y_continuous(breaks = seq(0, 100, by = 20)) +
                  scale_colour_manual(values = years_palette) +
                  labs(
                    x = 'day of the year',
                    y = 'mean price actual',
                    colour = 'year'
                  ) +
                  personal_theme('point')

# Simple plot to show how the price (mean) is affected by the week of the year
weekly_prices <- energy_weather_dataset %>%
                  group_by(week_of_year, year) %>%
                  summarize(avg_price = mean(price_actual)) %>%
                  ggplot(aes(week_of_year, avg_price, colour = as.factor(year))) +
                  geom_line(linewidth = 1.1) +
                  scale_x_continuous(breaks = seq(1, 53, by = 2)) +
                  scale_y_continuous(breaks = seq(0, 90, by = 10)) +
                  scale_colour_manual(values = years_palette) +
                  labs(
                    x = 'week of the year',
                    y = 'mean price actual',
                    colour = 'year'
                  ) +
                  personal_theme('line')

# Simple plot to show how the price (mean) is affected by the month of the year
monthly_prices <- energy_weather_dataset %>%
                    group_by(month, year) %>%
                    summarize(avg_price = mean(price_actual)) %>%
                    ggplot(aes(month, avg_price, colour = as.factor(year))) +
                    geom_line(linewidth = 1.1) +
                    scale_x_continuous(breaks = seq(1, 12, by = 1)) +
                    scale_y_continuous(breaks = seq(0, 90, by = 10)) +
                    scale_colour_manual(values = years_palette) +
                    labs(
                      x = 'month',
                      y = 'mean price actual',
                      colour = 'year'
                    ) +
                    personal_theme('line')

# Simple plot to confirm that the business_day affects the final price
business_day_prices <- energy_weather_dataset %>%
                        ggplot(aes(as.factor(year),
                                   price_actual,
                                   fill = as.factor(business_day))) +
                        geom_boxplot() +
                        scale_y_continuous(breaks = seq(10, 120, by = 10)) +
                        scale_fill_manual(values = cities_palette) +
                        labs(
                          x = 'year',
                          y = 'price actual',
                          fill = 'business day'
                        ) +
                        personal_theme('boxplot')

# Seasonality analysis at daily level
# Find the first date in the energy_weather_dataset to be able to dynamically
# valorise the start parameter in the timeseries
timeseries_start <- energy_weather_dataset %>%
                      filter(spain_time == min(spain_time)) %>%
                      select(year, day_of_year) %>%
                      as.numeric()

# Create the timeseries at daily level
daily_timeseries <- ts(energy_weather_dataset$price_actual,
                       frequency = 24,
                       start = timeseries_start)

# STL daily decomposition
daily_decomposition <- stl(daily_timeseries, s.window = 'periodic')

# Simple plot of the decomposition
daily_plot <- autoplot(daily_decomposition) +
                ggtitle('STL Daily decomposition') +
                scale_x_continuous(
                  breaks = seq(timeseries_start[1],
                               ceiling(max(time(daily_timeseries))),
                               by = 365.25),
                  labels = seq(timeseries_start[1],
                               max(energy_weather_dataset$year)+1,
                               by = 1)) +
                labs(
                  x = 'years'
                ) +
                theme_minimal()

# Interactive plotly to let zoom and analyse the relevant information
daily_plotly <- ggplotly(daily_plot)

# Seasonality analysis at weekly level
# Create the timeseries at weekly level
weekly_timeseries <- ts(energy_weather_dataset$price_actual,
                       frequency = 24*7,
                       start = timeseries_start)

# STL weekly decomposition
weekly_decomposition <- stl(weekly_timeseries, s.window = 'periodic')

# Simple plot of the decomposition
weekly_plot <- autoplot(weekly_decomposition) +
                ggtitle('STL Weekly decomposition') +
                scale_x_continuous(
                  breaks = seq(timeseries_start[1],
                               ceiling(max(time(weekly_timeseries))),
                               by = 52.25),
                  labels = seq(timeseries_start[1],
                               max(energy_weather_dataset$year)+1,
                               by = 1)) +
                labs(
                  x = 'years'
                ) +
                theme_minimal()

# Interactive plotly to let zoom and analyse the relevant information
weekly_plotly <- ggplotly(weekly_plot)

# Seasonality analysis at monthly level
# Create the timeseries at monthly level
monthly_timeseries <- ts(energy_weather_dataset$price_actual,
                        frequency = 24*30.4,
                        start = timeseries_start)

# STL monthly decomposition
monthly_decomposition <- stl(monthly_timeseries, s.window = 'periodic')

# Simple plot of the decomposition
monthly_plot <- autoplot(monthly_decomposition) +
                  ggtitle('STL Monthly decomposition') +
                  scale_x_continuous(
                    breaks = seq(timeseries_start[1],
                                 ceiling(max(time(monthly_timeseries))),
                                 by = 12.25),
                    labels = seq(timeseries_start[1],
                                 max(energy_weather_dataset$year)+1,
                                 by = 1)) +
                  labs(
                    x = 'years'
                  ) +
                  theme_minimal()

# Interactive plotly to let zoom and analyse the relevant information
monthly_plotly <- ggplotly(monthly_plot)

# Seasonality analysis at yearly level
# Create the timeseries at yearly level
yearly_timeseries <- ts(energy_weather_dataset$price_actual,
                         frequency = 24*365.25,
                         start = timeseries_start)

# STL yearly decomposition
yearly_decomposition <- stl(yearly_timeseries, s.window = 'periodic')

# Simple plot of the decomposition
yearly_plot <- autoplot(yearly_decomposition) +
                ggtitle('STL Yearly decomposition') +
                theme_minimal()

# Interactive plotly to let zoom and analyse the relevant information
yearly_plotly <- ggplotly(yearly_plot)

# Verify if the timeseries for price_actual is auto-correlated with plot = FALSE 
# in order to be able to use ggplot, to improve readability
autocorrelation <- acf(daily_timeseries, plot = FALSE, ci = 0.95)

# Plot the results of auto-correlation, also including the confidence interval
autocorrelation_plot <- data.frame(lag = autocorrelation$lag,
                                   hour_lag = ceiling(autocorrelation$lag*24),
                                   acf = autocorrelation$acf) %>%
                          ggplot(aes(hour_lag, acf)) +
                          geom_bar(stat = 'identity', fill = '#a6cee3') +
                          geom_hline(yintercept = qnorm(0.975)/sqrt(length(daily_timeseries)),
                                     linetype = 'dashed',
                                     colour = '#b2182b',
                                     linewidth = 1) +
                          geom_hline(yintercept = -qnorm(0.975)/sqrt(length(daily_timeseries)),
                                     linetype = 'dashed',
                                     colour = '#b2182b',
                                     linewidth = 1) +
                          labs(
                            x = 'lag hours',
                            y = 'autocorrelation'
                          ) +
                          personal_theme('')

# Verify if the timeseries for price_actual is partially auto-correlated
partial_autocorrelation <- pacf(daily_timeseries, plot = FALSE, ci = 0.95)

# Plot the results of partial auto-correlation
partial_autocorrelation_plot <- data.frame(lag = partial_autocorrelation$lag,
                                            hour_lag = ceiling(partial_autocorrelation$lag*24),
                                            pacf = partial_autocorrelation$acf) %>%
                                  ggplot(aes(hour_lag, pacf)) +
                                  geom_bar(stat = 'identity', fill = '#a6cee3') +
                                  geom_hline(yintercept = qnorm(0.975)/sqrt(length(daily_timeseries)),
                                             linetype = 'dashed',
                                             colour = '#b2182b',
                                             linewidth = 1) +
                                  geom_hline(yintercept = -qnorm(0.975)/sqrt(length(daily_timeseries)),
                                             linetype = 'dashed',
                                             colour = '#b2182b',
                                             linewidth = 1) +
                                  scale_y_continuous(breaks = seq(-1, 1, by = 0.2)) +
                                  labs(
                                    x = 'lag hours',
                                    y = 'partial autocorrelation'
                                  ) +
                                  personal_theme('')

################################################################################
# Final dataset

# Remove the spain_time feature, as we have all the derived ones
energy_weather_dataset <- energy_weather_dataset %>%
                            select(- spain_time)

# Split into training/validation and test set
# Find the total number of rows and define the size of the test set
final_number_of_rows <- nrow(energy_weather_dataset)
test_size <- 24 * 14

# Split in training/validation and test set without overlapping
training_validation_set <- energy_weather_dataset %>%
                            head(final_number_of_rows - test_size)

test_set <- energy_weather_dataset %>%
              tail(test_size)

################################################################################
# Manual window functions

# Fixed proportion of training, validation and test set
# This idea can help comparing the results of the evaluation functions, but
# it is not easily manageable when increasing the size of the dataset.
# Please also note that all variables defined below should be fixed after a real
# phase of hyperparameter tuning.

# Define the function to perform the expanding window with fixed proportions
expanding_window_prop <- function(dataset,
                                  prop_training_set,
                                  prop_validation_set,
                                  prop_test_set,
                                  min_num_observations,
                                  step){

  # Variable definitions:
  # dataset = dataset that will be splitted into 2 or 3 sets, as required
  # prop_training_set = proportion of data in the training set (range 0-1)
  # prop_validation_set = proportion of data in the validation set (range 0-1)
  # prop_test_set = proportion of data in the test set (range 0-1)
  # min_num_observations = minimum number of observations in the training
  # step = step between two consecutive splits

  # verify if the percentages sum up to 1
  if(prop_training_set + prop_validation_set + prop_test_set != 1){
    stop('The percentage of train, validation and test sets do not sum up to 1')
  }

  # Define a list to store all the different splits
  splits <- list()

  # Even if not required, it was decided to add the additional rows at the
  # beginning of the dataset to be sure to reach the last row
  offset <- (nrow(dataset)-(min_num_observations))%%step

  # Find the last indexes accordingly with the start and the step parameter
  final_indexes <- seq(offset+min_num_observations, nrow(dataset), by = step)

  # Evaluate the sum of the pecentages between train and validation
  prop_train_validation <- prop_training_set + prop_validation_set

  # Find the last indexes for train and validation
  # It is not evaluated for test because it is straightforward from the
  # initial split
  train_end <- round(final_indexes * prop_training_set, 0)
  validation_end <- round(final_indexes * prop_train_validation, 0)

  # Perform the loop to split at all the iterations
  for(current_split in 1:length(final_indexes)){

    # Find the current stop indexes
    train_stop <- train_end[current_split]
    validation_stop <- validation_end[current_split]
    test_stop <- final_indexes[current_split]

    # Create the training and test set
    train_set <- dataset[1:train_stop, ]
    validation_set <- dataset[(train_stop + 1):validation_stop, ]

    # Create the test set, if necessary
    if(validation_stop + 1 < test_stop){
      test_set <- dataset[(validation_stop + 1):test_stop, ]
    }else{
      test_set <- data.frame()
    }

    # Update the splits list with the current information
    splits[[current_split]] <- list(split = current_split,
                                    training_set = train_set,
                                    validation_set = validation_set,
                                    test_set = test_set)
  }

  # Return the list of dataset splits
  return(splits)

}

# If you are interested, you can perform the expanding window with fixed
# proportions on the current dataset. The values of the parameters are set by
# default
# split_window_proportion <- expanding_window_prop(energy_weather_dataset,
#                                                    0.8, 0.1, 0.1, 744, 168)

# Fixed number of rows in validation and test set
# This idea can easily lead to over-fitting, as we may use quite the entire
# dataset to predict only few rows. However, it cannot be ignored that it is
# theoretically consistent and can easily mimic a real world scenario where
# you have a subset of data and you predict from there.

# Define the function to perform the expanding window with fixed number of rows
expanding_window_fixed <- function(dataset,
                                   num_training_set,
                                   num_validation_set,
                                   num_test_set,
                                   step){

  # Variable definitions:
  # dataset = dataset that will be splitted into 2 or 3 sets, as required
  # num_training_set = minimum number of rows in the training set (first iteration)
  # num_validation_set = number of rows in the validation set
  # num_test_set = number of rows in the test set
  # step = step between two consecutive splits

  # Evaluate the number of observation required for the first run
  num_observations <- num_training_set + num_validation_set + num_test_set

  # Verify that the number of rows does not exceed the size of the dataset
  if(num_observations > nrow(dataset)){
    stop('Required more observation than available')
  }

  # Define a list to store all the different splits
  splits <- list()

  # As before, even if not required, it was decided to add the additional rows
  # at the beginning of the dataset to be sure to reach the last row
  offset <- (nrow(dataset)-(num_observations))%%step

  # Find the last indexes accordingly with the start and the step parameter
  final_indexes <- seq(offset+num_observations, nrow(dataset), by = step)

  # Define the total number of rows in the validation and test sets
  validation_test <- num_validation_set + num_test_set

  # Find the first index for each set in the split.
  # In this case the training set is not evaluated because it will be determined
  # by the difference with the remaining two
  test_first <- final_indexes - num_test_set + 1
  validation_first <- final_indexes - validation_test + 1

  # Perform the loop to split at all the iterations
  for(current_split in 1:length(final_indexes)){

    # Find the current start indexes
    test_start <- test_first[current_split]
    validation_start <- validation_first[current_split]

    # Create the test set, if necessary
    if(num_test_set != 0){
      test_set <- dataset[test_start:(test_start + num_test_set - 1), ]
    }else{
      test_set <- data.frame()
    }

    # Create the validation and training set
    validation_set <- dataset[validation_start:(validation_start + num_validation_set - 1), ]
    train_set <- dataset[1:(validation_start - 1), ]

    # Update the splits list with the current information
    splits[[current_split]] <- list(split = current_split,
                                    training_set = train_set,
                                    validation_set = validation_set,
                                    test_set = test_set)
  }

  # Return the list of dataset splits
  return(splits)

}

# Again, if you are interested, you can perform the expanding window with fixed
# number of rows on the current dataset. The values of the parameters are set by
# default
# split_window_fixed <- expanding_window_fixed(energy_weather_dataset,
#                                               744, 24, 0, 24)

###############################################################################
# Models

# Simplest idea: predict using the linear model
linear_model <- function(dataset, target, initial_window, horizon,
                          window_type, in_parallel){

  # Save start time for performance evaluation
  time_start <- Sys.time()

  # Use parallel computing, if desired
  clusters <- makePSOCKcluster(in_parallel)
  registerDoParallel(clusters)

  # Define the current train control
  current_train_control <- trainControl(method = 'timeslice',
                                        initialWindow = initial_window,
                                        horizon = horizon,
                                        skip = horizon - 1,
                                        fixedWindow = window_type)

  # Fit and evaluate the linear model
  # For technical reasons, the formula has been defined to avoid problems when
  # specifying column names
  formula <- as.formula(paste(target, '~ .'))
  lm_model <- train(formula,
                    method = 'lm',
                    data = dataset,
                    metric = 'RMSE',
                    trControl = current_train_control)

  # Stop parallel computing, if active
  stopCluster(clusters)

  # Save end time and evaluate the minutes required
  time_end <- Sys.time()
  total_time <- round(as.numeric(difftime(time_end, time_start, units = 'mins')), 2)

  # Save and return the model and the time required
  model_summary <- list(lm_model, total_time)
  return(model_summary)
}

# Define the functional variables required by the linear model
current_target <- 'price_actual'
train_size <- 24 * 31
validation_size <- 24

# Define the number of clusters you want to have in parallel
# Please note that this parameter is highly dependent on the specifications of
# your computer, so we strongly recommend to set this to a reasonable number
current_parallel <- 6

# Train the linear model with the expanding window approach
lm_expanding <- linear_model(training_validation_set, current_target, train_size,
                              validation_size, FALSE, current_parallel)

# Save the metrics and the training time
lm_expanding_metrics <- as.data.frame((lm_expanding[[1]]$results)) %>% select(RMSE, MAE)
lm_expanding_time <- lm_expanding[[2]]

sprintf('The linear model with an expanding window produced an RMSE of %.5f in %.2f minutes',
        lm_expanding_metrics$RMSE, lm_expanding_time)

# Train the linear model with the sliding window approach
lm_fixed <- linear_model(training_validation_set, current_target, train_size,
                          validation_size, TRUE, current_parallel)

# Save the metrics and the training time
lm_fixed_metrics <- as.data.frame((lm_fixed[[1]]$results)) %>% select(RMSE, MAE)
lm_fixed_time <- lm_fixed[[2]]

sprintf('The linear model with a sliding window produced an RMSE of %.5f in %.2f minutes',
        lm_fixed_metrics$RMSE, lm_fixed_time)

# Second approach: predict using the XG Boost model
xgboost_model <- function(dataset, target, initial_window, horizon,
                          window_type, in_parallel){

  # Save start time for performance evaluation
  time_start <- Sys.time()

  # Use parallel computing, if desired
  clusters <- makePSOCKcluster(in_parallel)
  registerDoParallel(clusters)

  # Define the current train control
  current_train_control <- trainControl(method = 'timeslice',
                                        initialWindow = initial_window,
                                        horizon = horizon,
                                        skip = horizon - 1,
                                        fixedWindow = window_type)

  # Fit and evaluate the XG Boost model
  # For technical reasons, the formula has been defined to avoid problems when
  # specifying column names
  formula <- as.formula(paste(target, '~ .'))
  xg_boost_model <- train(formula,
                          method = 'xgbTree',
                          data = dataset,
                          metric = 'RMSE',
                          trControl = current_train_control)

  # Stop parallel computing, if active
  stopCluster(clusters)

  # Save end time and evaluate the minutes required
  time_end <- Sys.time()
  total_time <- round(as.numeric(difftime(time_end, time_start, units = 'mins')), 2)

  # Save and return the model and the time required
  model_summary <- list(xg_boost_model, total_time)
  return(model_summary)
}

# Train the XG Boost model with the sliding window approach
xgboost_fixed <- xgboost_model(training_validation_set, current_target, train_size,
                               validation_size, TRUE, current_parallel)

# Save the metrics, the parameters of the best model and the training time
xgboost_fixed_metrics <- as.data.frame((xgboost_fixed[[1]]$results)) %>%
                          filter(RMSE == min(RMSE)) %>%
                          select(RMSE, MAE)

xgboost_best_tune <- xgboost_fixed[[1]]$bestTune %>%
                      pivot_longer(cols = everything(),
                                   names_to = 'hyperparameter',
                                   values_to = 'value')

xgboost_fixed_time <- xgboost_fixed[[2]]

sprintf('The XG Boost model with a sliding window produced an RMSE of %.5f in %.2f minutes',
        xgboost_fixed_metrics$RMSE, xgboost_fixed_time)

# Due to performance reasons, we will not run the training of the XG boost
# model with the expanding window, but you can run it if you like.
# We would also like to point out that in our runs it lasted longer (even
# using a smaller dataset) and produced under performing results
# Train the XG Boost model with the expanding window approach
#xgboost_expanding <- xgboost_model(training_validation_set, current_target, train_size,
#                                     validation_size, FALSE, current_parallel)
#xgboost_expanding_metrics <- as.data.frame((xgboost_expanding[[1]]$results)) %>%
#                                filter(RMSE == min(RMSE)) %>%
#                                select(RMSE, MAE)
#xgboost_expanding_time <- xgboost_expanding[[2]]

# Final idea: predict using the random forest
random_forest_model <- function(dataset, target, initial_window, horizon,
                                  window_type, in_parallel){

  # Save start time for performance evaluation
  time_start <- Sys.time()

  # Use parallel computing, if desired
  clusters <- makePSOCKcluster(in_parallel)
  registerDoParallel(clusters)

  # Define the current train control
  current_train_control <- trainControl(method = 'timeslice',
                                        initialWindow = initial_window,
                                        horizon = horizon,
                                        skip = horizon - 1,
                                        fixedWindow = window_type)

  # Fit and evaluate the random forest model
  # For technical reasons, the formula has been defined to avoid problems when
  # specifying column names
  formula <- as.formula(paste(target, '~ .'))
  rf_model <- train(formula,
                    method = 'rf',
                    data = dataset,
                    metric = 'RMSE',
                    trControl = current_train_control)

  # Stop parallel computing, if active
  stopCluster(clusters)

  # Save end time and evaluate the minutes required
  time_end <- Sys.time()
  total_time <- round(as.numeric(difftime(time_end, time_start, units = 'mins')), 2)

  # Save and return the model and the time required
  model_summary <- list(rf_model, total_time)
  return(model_summary)
}

# Train the random forest model with the sliding window approach
rf_fixed <- random_forest_model(training_validation_set, current_target, train_size,
                                  validation_size, TRUE, current_parallel)

# Save the metrics, the parameters of the best model and the training time
rf_fixed_metrics <- as.data.frame((rf_fixed[[1]]$results)) %>%
                                    filter(RMSE == min(RMSE)) %>%
                                    select(RMSE, MAE)

rf_best_tune <- rf_fixed[[1]]$bestTune %>%
                  pivot_longer(cols = everything(),
                               names_to = 'hyperparameter',
                               values_to = 'value')

rf_fixed_time <- rf_fixed[[2]]

sprintf('The random forest model with a sliding window produced an RMSE of %.5f in %.2f minutes',
        rf_fixed_metrics$RMSE, rf_fixed_time)

# For the same reasons as above, we will not be training the random forest model
# with the expanding window approach, however you can do so if you wish.
# Train the random forest model with the expanding window approach
#rf_expanding <- random_forest_model(training_validation_set, current_target, train_size,
#                                     validation_size, FALSE, current_parallel)
#rf_expanding_metrics <- as.data.frame((rf_expanding[[1]]$results)) %>%
#                                filter(RMSE == min(RMSE)) %>%
#                                select(RMSE, MAE)
#rf_expanding_time <- rf_expanding[[2]]

################################################################################
# Results: final test
# Compare the results of the random forest model against the independent test set
rf_test_prediction <- predict(rf_fixed[[1]], newdata = test_set)

# First idea: simple evaluation of the metrics
final_RMSE <- RMSE(rf_test_prediction, test_set$price_actual)
final_MAE <- MAE(rf_test_prediction, test_set$price_actual)

sprintf('The random forest model with a sliding window produced an RMSE of %.5f against indipendent test set',
        final_RMSE)

# Second idea: define a function to evaluate metrics on the test set in a way
# that mimics the training phase
test_set_performance_evaluation <- function(test_set,
                                            target,
                                            model_prediction,
                                            validation_size){

  # Isolate the target feature from the test set
  test_y <- test_set %>% select(all_of(target))

  # Merge predictions and test set to easily evaluate the metrics
  final_outcomes <- as.data.frame(cbind(test_y, model_prediction))

  # Define the dataframes to store the metrics
  final_metrics <- data.frame()

  # Initialise i for the while loop
  i = 1
  while(i <= nrow(final_outcomes) - validation_size){

    # Define the current slice
    # The function dplyr::slice was not used as it was not straightforward
    # at this stage of the evaluations
    current_slice <- final_outcomes %>%
                      head(i + validation_size - 1) %>%
                      tail(validation_size)

    # Evaluate the metrics on the current slice
    current_RMSE <- RMSE(current_slice[[1]], current_slice$model_prediction)
    current_MAE <- MAE(current_slice[[1]], current_slice$model_prediction)

    # Save the results in the final dataset
    final_metrics <- rbind(final_metrics,
                           data.frame(slice = i,
                                      RMSE = current_RMSE,
                                      MAE = current_MAE))
    # Update the loop parameter for the next iteration
    i <- i + 1
  }

  # Return the results
  return(final_metrics)

}

# Evaluate the results with the second approach, storing a result for each iteration
final_metrics_new <- test_set_performance_evaluation(test_set, current_target, rf_test_prediction, validation_size)

# Create a summary with some statistic for each metric
summary_final_metrics_new <- final_metrics_new %>%
                              summarise(across(RMSE:MAE,
                                               list(min = min,
                                                    max = max,
                                                    mean = mean,
                                                    median = median,
                                                    sd = sd))) %>%
                              pivot_longer(cols = everything(),
                                            names_to = c('metric', 'statistic'),
                                            names_sep = '_',
                                            values_to = 'value') %>%
                              arrange(desc(metric))

# Plot the RMSE value through the slices

plot_RMSE_new <- final_metrics_new %>%
                  ggplot(aes(slice, RMSE)) +
                  geom_line(linewidth = 1.1, colour = '#2166ac') +
                  scale_x_continuous(breaks = seq(0, 300, by = 50)) +
                  labs(
                    x = 'slice',
                    y = 'RMSE value',
                  ) +
                  personal_theme('line')

# Plot the deltas between the prediction and the test_set
final_comparison <- test_set %>%
                      select(year, month, day, hour, price_actual) %>%
                      cbind(rf_test_prediction) %>%
                      pivot_longer(cols = c(price_actual, rf_test_prediction),
                                   names_to = 'source',
                                   values_to = 'price') %>%
                      mutate(source = ifelse(source == 'rf_test_prediction', 'prediction', 'actual'),
                             time = as.POSIXct(paste(year, month, day, hour,  sep = '-'),
                                               format = '%Y-%m-%d-%H')) %>%
                      ggplot(aes(time, price, colour = source)) +
                      geom_line(linewidth = 0.8) +
                      scale_x_datetime(date_labels = '%m-%d',
                                        date_breaks = '2 day') +
                      scale_color_manual(values = c('#2166ac', '#b2182b')) +
                      labs(
                        x = 'date',
                        y = 'price',
                        color = 'source'
                      ) +
                      personal_theme('')