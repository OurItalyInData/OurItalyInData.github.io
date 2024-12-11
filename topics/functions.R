library(tidyverse)
library(ggplot2)
library(plotly)
library(scales)
library(roxygen2)
library(lubridate)
library(rsdmx)
library(owidR)
library(reticulate)
library(gt)
library(janitor)
library(shiny)
library(readr)  # For reading and writing CSV files
library(R.utils)
library(eurostat)


#' Retrieve and Process Eurostat Homicide Data
#'
#' This function retrieves homicide data from Eurostat, processes it, and optionally filters the data for a specific country (e.g., Italy). 
#' It groups the data by specified columns and returns both the raw and grouped data.
#'
#' @param id Character string. The dataset identifier from Eurostat (e.g., "crim_homicide").
#' @param filter_it Logical. If `TRUE`, filters the data for Italy only. If `FALSE`, no filtering is applied.
#' @param time_format Character string. The time format to be used when retrieving the data (e.g., "YM" for year-month format).
#' @param cache Logical. If `TRUE`, uses cached data if available.
#' @param update_cache Logical. If `TRUE`, updates the cache with the latest data.
#' @param aggregate_columns Character vector. The columns by which to group the data (e.g., `c("geo", "sex")`).
#'
#' @return A list containing two data frames:
#' \itemize{
#'   \item `df`: The raw (optionally filtered) homicide data, with columns for `geo`, `year`, `unit`, and other variables.
#'   \item `df_group`: The aggregated homicide data, with the sum of homicides grouped by the specified columns.
#' }
#'
#' @import dplyr
#' @import eurostat
#' @export
get_eurostat_data <- function(id, filter_it, time_format, cache, update_cache, aggregate_columns) {
  
  # Retrieve the data from Eurostat
  df <- get_eurostat(id = id, time_format = time_format, cache = cache, update_cache = update_cache)
  
  # Label and rename the columns
  df <- df %>%
    label_eurostat() %>%
    rename(year = TIME_PERIOD) %>%
    arrange(geo, year)  # Sorting by country and year
  
  # Optionally filter the data for Italy
  if (filter_it == TRUE) {
    df <- df %>% filter(geo == "Italy")
    
    # Group the data by the specified columns and summarize
    df_group <- df %>%
      group_by(across(all_of(aggregate_columns))) %>%
      summarise(values_grouped = sum(values), .groups = "drop") %>%
      filter(unit == "Number") %>%
      arrange(geo, year, sex)
  } else if(filter_it == FALSE) {
    
    # Group the data by the specified columns and summarize
    df_group <- df %>%
      group_by(across(all_of(aggregate_columns))) %>%
      summarise(values_grouped = sum(values), .groups = "drop") %>%
      filter(unit == "Number") %>%
      arrange(geo, year, sex)
  }
  
  # Return both datasets as a list
  return(list(df = df, df_group = df_group))
}

#' Create Time Series Plot
#'
#' This function creates a time series plot of the given data. It uses `ggplot2` to create the plot and `plotly` for interactive features.
#'
#' @param t Data frame containing the time series data (e.g., columns for `x`, `y`, and `colour`).
#' @param x Numeric vector for the x-axis. Typically represents time (e.g., date or year).
#' @param x_text Character vector for the x-axis labels.
#' @param y Numeric vector for the y-axis. Typically represents values (e.g., homicide rates, counts).
#' @param y_text Character vector for the y-axis labels.
#' @param colour Factor variable for coloring the lines/points. Can represent categories (e.g., countries, genders).
#' @param palette Character string specifying the color palette. Defaults to a Brewer palette like "Set1".
#' @param title Character string for the plot title.
#' @param subtitle Character string for the plot subtitle.
#' @param xlabs Character string for the x-axis label.
#' @param ylabs Character string for the y-axis label.
#' @param legend_title Character string for the legend title.
#' @param is_percentage Logical. If `TRUE`, scales the y-axis values to percentages.
#' 
#' @return A `plotly` object, an interactive time series plot.
#'
#' @import ggplot2
#' @import plotly
#' @import scales
#' @importFrom dplyr mutate
#' @examples
#' \dontrun{
#' plot_time_series(
#'   t = data.frame(time = as.Date('2021-01-01') + 0:9, value = rnorm(10)),
#'   x = as.Date('2021-01-01') + 0:9, 
#'   y = rnorm(10), 
#'   colour = as.factor(rep(1, 10)),
#'   palette = "Set1", 
#'   title = "Time Series Plot", 
#'   subtitle = "Example Plot", 
#'   xlabs = "Time", 
#'   ylabs = "Value", 
#'   legend_title = "Series",
#'   is_percentage = FALSE
#' )
#' }
#' @export
plot_time_series <- function(
    t, x, x_text, y, y_text, colour, palette, title, subtitle, xlabs, ylabs, legend_title, is_percentage = FALSE
) {
  
  if (is.null(x) || length(x) == 0 || all(x == 0)) {
    message("x is empty, null, or zero. No plot will be generated.")
    return(print("Non ci sono dati disponibili."))
  }
  
  if (is_percentage) {
    y <- y * 100
    y_text <- paste0(y_text, " (%)")
  }
  
  line_plot <- ggplot(t) +
    geom_line(aes(x = x, y = y, colour = colour)) +
    geom_point(aes(
      x = x,
      y = y,
      colour = colour,
      text = str_c(str_c(x_text, ": "), x, str_c("<br>", y_text, ": "), scales::comma(y), if (is_percentage) "%" else "")
    )) +
    theme_classic() +
    scale_color_brewer(palette = palette) +
    labs(
      title = title,
      subtitle = subtitle,
      x = xlabs,
      y = ylabs,
      color = legend_title
    ) +
    scale_x_date(breaks = pretty_breaks(n = 10)) +
    scale_y_continuous(
      breaks = pretty_breaks(n = 10),
      labels = number_format(big.mark = ",", suffix = if (is_percentage) "%" else "")
    )
  
  line_plotly <- ggplotly(line_plot, tooltip = c("text")) %>% 
    layout(hovermode = "x unified")
  
  return(line_plotly)
}


#' Read and Save Data from SDMX
#'
#' This function reads data from the SDMX service provided by ISTAT and saves it as an RDS file for future use. 
#' If the RDS file already exists, it loads the data from the file.
#'
#' @param id The ID of the data flow.
#' @param dsd The data structure definition.
#' @param path The path where the RDS file will be saved.
#' @param version The version of the data (currently unused in the function).
#'
#' @return A data frame containing the data read from the SDMX service or the RDS file.
#'
#' @import readr
#' @import stringr
#' @import SDMX
#' @export
#'
#' @examples
#' \dontrun{
#'   df <- read_db("dataflow_id", "dsd_id", "path/to/save/", "version")
#' }
read_db <- function(id, dsd, path, version) {
  
  rds_path <- str_c(path, id, ".rds")
  
  if (file.exists(rds_path)) {
    # If the RDS file exists, read data from the RDS file
    df <- readRDS(rds_path)
  } else {
    # If the RDS file does not exist, read data using readSDMX and save to RDS
    df <- readSDMX(
      agencyId = "IT1",
      resource = "dataflow", 
      flowRef = id,
      dsd = dsd
    ) %>% 
      as.data.frame(labels = TRUE) %>% 
      mutate(year = ymd(str_c(obsTime, "-01-01"))) %>%
      janitor::clean_names() %>%
      rename(value = obs_value) %>%
      select(year, matches("_it"), value) %>% 
      rename_with(~ str_remove(.x, "_label_it$"))
    
    # Save data to RDS file
    saveRDS(df, rds_path)
  }
  
  return(df)
}
