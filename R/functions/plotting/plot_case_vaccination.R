#' Plot Case Counts and Vaccination Coverage
#'
#' This function creates two plots side-by-side for visual comparison:
#' a bar chart showing disease case counts and a line chart showing vaccination coverage over time.
#' The plots are faceted by country or region.
#'
#' @param case_data A data frame containing reported disease case counts.
#'   Must include the columns: `year`, `cases`, `disease_description`, and `name`.
#'
#' @param vaccination_data A data frame containing vaccination data.
#'   Must include the columns: `iso3`, `year`, `name`, `fully_vaccinated_persons`, and `total_population`.
#'
#' @return A combined ggplot object (using `patchwork`) with case bar plots and vaccination coverage line plots.
#'   The case plot is faceted by `name`, and the vaccination plot is faceted by `iso3`.
#'
#' @import dplyr
#' @import ggplot2
#' @import patchwork
#' @importFrom scales comma
#'
#' @examples
#' # Example usage:
#' plot_case_vaccination(case_df, vaccination_df)
#'
#' @export
plot_case_vaccination <- function(
    case_data,
    vaccination_data
){
  
  # Aggregate vaccination data by country, year, and name
  vaccination_aggregated <- vaccination_data %>%
    group_by(iso3, year, name) %>%
    summarise(
      fully_vaccinated = sum(fully_vaccinated_persons),
      total = sum(total_population),
      .groups = "drop"
    ) %>%
    mutate(
      coverage = fully_vaccinated / total
    )
  
  # Plot case counts
  cases <- ggplot(
    data = case_data %>%
      filter(!is.na(cases)),
    mapping = aes(
      x = year,
      y = cases
    )
  ) +
    geom_bar(stat = "identity") +
    theme_bw() +
    labs(
      x = "",
      y = "Cases",
      fill = ""
    ) +
    theme(
      legend.position = "top"
    ) +
    scale_y_continuous(label = scales::comma)
  
  # Plot vaccination coverage
  vaccination <- ggplot(
    data = vaccination_aggregated,
    mapping = aes(
      x = year,
      y = coverage * 100
    )
  ) +
    geom_line() + 
    theme_bw() +
    labs(
      x = "",
      y = "Vaccination\ncoverage (%)",
      color = ""
    ) +
    theme(
      legend.position = "none"
    ) +
    coord_cartesian(ylim = c(0, 100))
  
  # Combine and return the plots
  cases / vaccination + 
    plot_layout(guides = 'collect') +
    plot_annotation(title = "Cases and vaccinations")
}
