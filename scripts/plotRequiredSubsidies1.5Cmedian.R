plotRequiredSubsidies1.5Cmedian <- function(scenario.select,
                                            carbonprice.select,
                                            title,
                                            ymax.annual = NA,
                                            secaxis.factor = NA) {
  
  # Years to plot
  years.plot <- seq(2024, 2050)
  
  # Color for cost gap and annual subsidies
  gap.color <- "#C10505"

  # Color for cumulative subsidies depends on carbon price
  cum.color <- ifelse(carbonprice.select == TRUE, "#7C4569", "#0e6684")

  # Required annual subsidies
  # Grouped by projects built until 2030 and afterwards
  data.plot.subsidies.annual <- data.subsidies.annual %>%
    filter(scenario == scenario.select,
           carbonprice == carbonprice.select) %>% 
    filter(year.cap <= 2050) %>% 
    filter(year %in% years.plot) %>% 
    mutate(project.group = case_when(year.cap <= 2030 ~ "Projects until 2030",
                                     year.cap <= 2050 ~ "1.5C scenario median")) %>% 
    group_by(year, project.group) %>% 
    summarise(subsidies.annual = sum(subsidies.annual)) %>%
    order.levels(project.group = c("1.5C scenario median", "Projects until 2030"))
  
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
  data.plot.subsidies.cumulative <- map_dfr(c(2030, 2050), ~calcCumulative(.x))
  
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
             mapping = aes(x = year, y = subsidies.annual, fill = project.group),
             alpha = 0.3) +
    scale_fill_manual(name = "Annual subsidies (left axis)",
                      values = c("Projects until 2030" = gap.color,
                                 "1.5C scenario median" = "black"),
                      guide = guide_legend(order = 1)) +
    # Cumulative required subsidies
    geom_line(data = data.plot.subsidies.cumulative,
              mapping = aes(x = year, y = subsidies.cumulative.adj, linetype = as.character(until)),
              color = cum.color,
              linewidth = 0.5) +
    scale_linetype_manual(
      name = "Cumulative subsidies (right axis)",
      breaks = c("2050", "2030"),
      values = c("2030" = "dotted",
                 "2050" = "solid"),
      labels = c("2030" = "Projects until 2030",
                 "2050" = "1.5C scenario median"),
      guide = guide_legend(order = 2)
    ) +
    scale_x_continuous(
      name = "Year",
      breaks = c(2024, 2030, 2035, 2040, 2045, 2050)
    ) +
    scale_y_continuous(
      name = "Annual subsidies (Billion $/yr)",
      limits = c(0, ymax.annual),
      sec.axis = sec_axis(
        transform=~.*secaxis.factor,
        name = "Cumulative subsidies (Billion $)")
    ) +
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