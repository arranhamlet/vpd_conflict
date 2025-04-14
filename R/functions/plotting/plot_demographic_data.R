#' Plot Demographic Trends Over Time
#'
#' This function generates a suite of demographic visualizations using population,
#' fertility, mortality, and migration data. It outputs two panels of plots:
#' one showing population trends and age distribution, and the other showing 
#' crude death, birth, and migration rates over time.
#'
#' @param demographic_data A list containing demographic data objects. Must include:
#' \itemize{
#'   \item \code{input_data$year_start}: Integer start year.
#'   \item \code{population_data}: Matrix or data frame of population counts (time x age).
#'   \item \code{crude_death}: Data frame with mortality values and dimensions \code{dim1}, \code{dim3}.
#'   \item \code{crude_birth}: Data frame with birth rate values and \code{time}.
#'   \item \code{migration_in_number}: Data frame with migration counts and \code{dim1}.
#' }
#'
#' @return A `patchwork` grid of ggplot objects showing:
#' \enumerate{
#'   \item Total population over time
#'   \item Final year population age distribution
#'   \item Annual mortality
#'   \item Annual fertility
#'   \item Annual migration
#' }
#'
#' @import data.table
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom scales comma
#' @import patchwork
#'
#' @examples
#' # plot_demographic_data(my_demographic_object)
#'
#' @export
plot_demographic_data <- function(
    demographic_data  
){
  
  # Extract base year
  year_start <- demographic_data$input_data$year_start
  
  # Reshape population data to long format
  population_by_age <- reshape2::melt(demographic_data$population_data) %>%
    setnames(c("time", "age", "value")) %>%
    arrange(time, age)
  
  # Total population over time
  population_total <- population_by_age %>%
    group_by(time) %>%
    summarise(value = sum(value), .groups = "drop")
  
  # Weighted average mortality (crude death rate)
  demographic_data$crude_death$population <- population_by_age$value
  overall_mortality <- demographic_data$crude_death %>%
    group_by(dim1, dim3) %>%
    mutate(
      population_weight = population / sum(population),
      weighted_value = value * population_weight
    ) %>%
    summarise(value = sum(weighted_value), .groups = "drop")
  
  # Total migration by year
  migration_aggregate <- demographic_data$migration_in_number %>%
    group_by(dim1) %>%
    summarise(value = sum(value))
  
  # Plot: Population over time
  population_over_time <- ggplot(
    data = population_total,
    mapping = aes(
      x = time + year_start,
      y = value * 1000
    )
  ) +
    geom_line() +
    labs(x = "", y = "Population") +
    scale_y_continuous(label = scales::comma) +
    theme_bw()
  
  # Plot: Age pyramid in the final year
  age_pyramid_final <- ggplot(
    data = population_by_age %>%
      subset(time == max(time)),
    mapping = aes(
      x = age,
      y = value / sum(value)
    )
  ) +
    geom_bar(stat = "identity") +
    labs(x = "Age", y = "Proportion of\nthe population") +
    theme_bw()
  
  # Plot: Mortality over time
  mortality_plot <- ggplot(
    data = overall_mortality,
    mapping = aes(
      x = dim1 + year_start - 1,
      y = value
    )
  ) +
    geom_line() +
    labs(x = "", y = "Annual\nmortality") +
    scale_y_continuous(label = scales::comma) +
    theme_bw()
  
  # Plot: Fertility over time
  fertility_plot <- ggplot(
    data = demographic_data$crude_birth,
    mapping = aes(
      x = time + year_start,
      y = value
    )
  ) +
    geom_line() +
    labs(x = "", y = "Annual\nfertility") +
    scale_y_continuous(label = scales::comma) +
    theme_bw()
  
  # Plot: Migration over time
  migration_plot <- ggplot(
    data = migration_aggregate,
    mapping = aes(
      x = dim1 + year_start - 1,
      y = value
    )
  ) +
    geom_line() +
    labs(x = "", y = "Annual\nmigration") +
    scale_y_continuous(label = scales::comma) +
    theme_bw() +
    geom_hline(yintercept = 0, linetype = "dashed")
  
  # Assemble and return plots
  population_plots <- population_over_time + age_pyramid_final
  inflow_outflow_plots <- mortality_plot + fertility_plot + migration_plot
  
  population_plots / inflow_outflow_plots + 
    plot_annotation(title = "Demographics")
  
}
