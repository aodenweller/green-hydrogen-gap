plotRequiredSubsidies <- function(scenario.select,
                                  carbonprice.select,
                                  title,
                                  years.plot = seq(2024, 2045),
                                  ymax.annual = NA,
                                  secaxis.factor = NA) {
  
  # Color for cost gap and annual subsidies
  gap.color <- "#C10505"

  # Color for cumulative subsidies depends on carbon price
  cum.color <- ifelse(carbonprice.select == TRUE, "#7C4569", "#0e6684")

  # Required annual subsidies
  # Grouped by projects built until 2030 and afterwards
  data.plot.subsidies.annual <- data.subsidies.annual %>%
    filter(scenario == scenario.select,
           carbonprice == carbonprice.select) %>% 
    filter(year.cap <= 2030) %>% 
    filter(year %in% years.plot) %>% 
    mutate(project.group = case_when(year.cap <= 2026 ~ "Projects 2024-2026",
                                     year.cap <= 2028 ~ "Projects 2027-2028",
                                     year.cap <= 2030 ~ "Projects 2029-2030")) %>% 
    group_by(year, project.group) %>% 
    summarise(subsidies.annual = sum(subsidies.annual)) %>%
    order.levels(project.group = c("Projects 2029-2030", "Projects 2027-2028", "Projects 2024-2026"))
  
  
  # Required cumulative subsidies
  calcCumulative <- function(until) {
  data.plot.subsidies.cumulative <- data.subsidies.annual %>% 
    filter(scenario == scenario.select,
           carbonprice == carbonprice.select) %>% 
    filter(year.cap <= until) %>% 
    group_by(year) %>% 
    summarise(subsidies.annual = sum(subsidies.annual)) %>%
    complete(year = years.plot, fill = list(subsidies.annual = 0)) %>% 
    arrange(year) %>% 
    mutate(subsidies.cumulative = cumsum(subsidies.annual)) %>% 
    mutate(until = until) %>% 
    select(year, until, subsidies.annual, subsidies.cumulative)
  }
  
  # Required cumulative subsidies
  data.plot.subsidies.cumulative <- map_dfr(c(2026, 2028, 2030), ~calcCumulative(.x))
  
  # If not provided calculate scaling factor between annual and cumulative
  if (is.na(secaxis.factor)) {
    # Determine scaling factor for secondary axis and scale data
    secaxis.factor <- data.plot.subsidies.cumulative %>%
      summarise(secaxis.factor = max(subsidies.cumulative) / max(subsidies.annual)) %>% 
      pull()
  }
  
  # Scale data (for secondary axis on the right)
  data.plot.subsidies.cumulative <- data.plot.subsidies.cumulative %>% 
    mutate(subsidies.cumulative.adj = 1/secaxis.factor * subsidies.cumulative)
  
  # Create plot for required subsidies
  p.subsidies <- ggplot() +
    # Annual required subsidies
    geom_col(data = data.plot.subsidies.annual,
             mapping = aes(x = year, y = subsidies.annual, alpha = project.group),
             fill = gap.color) +
    scale_alpha_manual(name = "Annual subsidies (left axis)",
                       values = c("Projects 2024-2026" = 0.3,
                                  "Projects 2027-2028" = 0.2,
                                  "Projects 2029-2030" = 0.1),
                       guide = guide_legend(order = 1)) +
    # Cumulative required subsidies
    geom_line(data = data.plot.subsidies.cumulative,
              mapping = aes(x = year, y = subsidies.cumulative.adj, linetype = as.character(until)),
              color = cum.color,
              linewidth = 0.5) +
    scale_linetype_manual(
      name = "Cumulative subsidies (right axis)",
      breaks = c("2030", "2028", "2026"),
      values = c("2026" = "dotted",
                 "2028" = "dashed",
                 "2030" = "solid"),
      labels = c("2026" = "Projects until 2026",
                 "2028" = "Projects until 2028",
                 "2030" = "Projects until 2030"),
      guide = guide_legend(order = 2)
    ) +
    scale_x_continuous(
      name = "Year",
      breaks = c(2024, 2030, 2035, 2040, 2045)
    ) +
    scale_y_continuous(
      name = "Annual subsidies (Billion $/yr)",
      limits = c(0, ymax.annual),
      sec.axis = sec_axis(
        transform=~.*secaxis.factor,
        name = "Cumulative subsidies (Billion $)")
    ) +
    coord_cartesian(xlim = c(years.plot[1]-0.5, years.plot[length(years.plot)]+0.5)) +
    ggtitle(title) +
    theme(axis.line.y.right = element_line(color = cum.color),
          axis.ticks.y.right = element_line(color = cum.color),
          axis.text.y.right = element_text(color = cum.color),
          axis.title.y.right = element_text(color = cum.color))
  
  # Get maximum value of annual subsidies
  subsidies.annual.max <- data.plot.subsidies.annual %>%
    group_by(year) %>% 
    summarise(sum = sum(subsidies.annual)) %>% 
    pull(sum) %>% 
    max()
  
  # Get maximum value of cumulative subsidies
  subsidies.cumulative.2030 <- data.plot.subsidies.cumulative %>% 
    pull(subsidies.cumulative) %>% 
    max()
  
  return(list(p.subsidies, secaxis.factor, subsidies.annual.max, subsidies.cumulative.2030))
  
}