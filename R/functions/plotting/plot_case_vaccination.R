
plot_case_vaccination <- function(
  case_data,
  vaccination_data
  ){
  
  #Plot
  vaccination_aggregated <- vaccination_data %>%
    group_by(iso3, year, name) %>%
    summarise(
      fully_vaccinated = sum(fully_vaccinated_persons),
      total = sum(total_population)
    ) %>%
    mutate(
      coverage = fully_vaccinated/total
    )
  
  cases <- ggplot(
    data = case_data,
    mapping = aes(
      x = year,
      y = cases,
      fill = disease_description
    )
  ) +
    geom_bar(stat = "identity", position = position_dodge()) +
    theme_bw() +
    labs(
      x = "",
      y = "Cases",
      fill = ""
    ) +
    facet_wrap(~name) +
    theme(
      legend.position = "top"
    ) +
    scale_y_continuous(label = scales::comma)
  
  vaccination <- ggplot(
    data = vaccination_aggregated,
    mapping = aes(
      x = year,
      y = coverage * 100,
      color = name
    )
  ) +
    geom_line() + 
    theme_bw() +
    labs(
      x = "",
      y = "Vaccination coverage (%)",
      color = ""
    ) +
    facet_wrap(~iso3) +
    theme(
      legend.position = "none"
    ) +
    coord_cartesian(ylim = c(0, 100))
  
  cases/vaccination + plot_layout(guides = 'collect')
  
}