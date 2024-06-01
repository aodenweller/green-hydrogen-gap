plotSubsidiesWithScenarios <- function(data.subsidies.annual,
                                       scen,
                                       withCarbonPrice,
                                       color.cumulative,
                                       ymax.subsidies = NA,
                                       secaxis.factor = NA) {
  
  # Plotting period
  period.plot <- seq(2024, 2045)
  
  # Select scenario
  data.plot.subsidies.annual <- data.subsidies.annual %>% 
    filter(scenario == scen)
  
  # Scenario suffix for title
  if (scen == "Default") {
    scen.suffix = "(default)"
  } else if (scen == "Progressive") {
    scen.suffix = "(progressive)"
  } else if (scen == "Conservative") {
    scen.suffix = "(conservative)"
  }
  
  # Function to calculate cumulative subsidies until given year
  calcCumulative <- function(data.plot.subsidies.annual, until){
    data.subsidies.cumulative <- data.plot.subsidies.annual %>%
      filter(projects.until <= until) %>%
      mutate(value = ifelse(value < 0, 0, value)) %>% 
      # Fill with zero if no subsidies any longer
      group_by(scenario, region, unit, variable, projects.until) %>% 
      complete(period = period.plot,
               fill = list(value = 0)) %>% 
      # Calculate cumulative
      group_by(scenario, variable, region, unit, period) %>% 
      summarise(sum = sum(value)) %>% 
      mutate(value = cumsum(sum),
             variable = paste("Subsidies|Cumulative|Projects until", until),
             unit = "bn USD") %>% 
      select(scenario, variable, region, unit, period, value)
    
    return(data.subsidies.cumulative)
  }
  
  # Cumulative subsidies for which years
  period.cumulative <- c(2030, 2045)
  
  # Gap and subsidy color
  gap.color <- "#C10505"
  
  # With or without carbon price
  if (withCarbonPrice == TRUE){
    
    # Annual subsidies with carbon price
    data.plot.subsidies.annual <- data.plot.subsidies.annual %>% 
      filter(variable == "Subsidies|Annual")
    
    # Calculate cumulative subsidies
    data.subsidies.cumulative <- map_dfr(period.cumulative, ~{
      data.plot.subsidies.annual %>%
        filter(variable == "Subsidies|Annual") %>%
        calcCumulative(until = .x)
    })
    
    # Suffix to plot title
    title.suffix <- "with carbon price"
    
    # Color for cumulative
    color.cum <- color.cumulative["With CO2"]
    
  } else if (withCarbonPrice == FALSE){
    
    # Subsidies per year with carbon price
    data.plot.subsidies.annual <- data.plot.subsidies.annual %>% 
      filter(variable == "Subsidies|Annual|w/o CO2")
    
    # Calculate cumulative subsidies
    data.subsidies.cumulative <- map_dfr(period.cumulative, ~{
      data.plot.subsidies.annual %>%
        filter(variable == "Subsidies|Annual|w/o CO2") %>%
        calcCumulative(until = .x)
    })
    
    # Suffix to plot title
    title.suffix <- "without carbon price"
    
    # Color for cumulative
    color.cum <- color.cumulative["Without CO2"]
    
  }
  
  # Required annual subsidies
  # Grouped by projects built until 2030 and afterwards
  data.plot.subsidies.annual <- data.plot.subsidies.annual %>%
    filter(period %in% period.plot) %>% 
    mutate(project.group = case_when(projects.until <= 2030 ~ "Until 2030",
                                     projects.until <= 2045 ~ "Until 2045")) %>% 
    group_by(scenario, variable, region, unit, period, project.group) %>% 
    summarise(value = sum(value)) %>% 
    order.levels(project.group = c("Until 2045", "Until 2030"))
  
  # Required total cumulative subsidies
  data.plot.subsidies.cumulative <- data.subsidies.cumulative %>% 
    filter(period %in% period.plot)
  
  # If not provided calculate scaling factor between annual and cumulative
  if (is.na(secaxis.factor)) {
    # Determine scaling factor for secondary axis and scale data
    subsidies.cumulative.max <- data.plot.subsidies.cumulative %>% 
      pull(value) %>% 
      max()
    
    subsidies.annual.max <- data.plot.subsidies.annual %>%
      group_by(period) %>%
      summarise(sum = sum(value)) %>%
      pull(sum) %>%
      max()
    
    secaxis.factor <- subsidies.cumulative.max / subsidies.annual.max
  }
  
  # Scale data (for secondary axis on the right)
  data.plot.subsidies.cumulative <- data.plot.subsidies.cumulative %>% 
    mutate(value.adjusted = 1/secaxis.factor * value)
  
  # Create plot for required subsidies
  p.subsidies <- ggplot() +
    # Annual required subsidies
    geom_col(data = data.plot.subsidies.annual,
             mapping = aes(x = period, y = value, fill = project.group),
             alpha = 0.3) +
    scale_fill_manual(name = "Annual subsidies (left axis)",
                      values = c("Until 2030" = gap.color,
                                 "Until 2045" = "grey20"),
                      labels = c("Until 2030" = "Projects until 2030",
                                 "Until 2045" = "1.5C scenarios 2031-2045"),
                      guide = guide_legend(order = 1)) +
    # Cumulative required subsidies
    geom_line(data = data.plot.subsidies.cumulative,
              mapping = aes(x = period, y = value.adjusted, linetype = variable),
              color = color.cum,
              linewidth = 0.5) +
    scale_linetype_manual(
      name = "Cumulative subsidies (right axis)",
      breaks = c(
        "Subsidies|Cumulative|Projects until 2045",
        "Subsidies|Cumulative|Projects until 2030"
      ),
      values = c(
        "Subsidies|Cumulative|Projects until 2030" = "dotted",
        "Subsidies|Cumulative|Projects until 2045" = "solid"
      ),
      labels = c(
        "Subsidies|Cumulative|Projects until 2030" = "Projects until 2030",
        "Subsidies|Cumulative|Projects until 2045" = "1.5C scenarios 2031-2045"),
      guide = guide_legend(order = 2)
    ) +
    xlab("Year") +
    scale_y_continuous(
      name = "Annual subsidies (Billion $/yr)",
      limits = c(0, ymax.subsidies),
      sec.axis = sec_axis(
        transform=~.*secaxis.factor,
        name = "Cumulative subsidies (Billion $)")
    ) +
    ggtitle(paste("Required subsidies", title.suffix, scen.suffix)) +
    theme(plot.margin = unit(c(3,3,3,3), units = "pt"),
          axis.line.y.right = element_line(color = color.cum),
          axis.ticks.y.right = element_line(color = color.cum),
          axis.text.y.right = element_text(color = color.cum),
          axis.title.y.right = element_text(color = color.cum))
  
  # Get cumulative level at last time step
  subsidies.cumulative.2030 <- data.plot.subsidies.cumulative %>%
    filter(period == period.plot[length(period.plot)],
           variable == "Subsidies|Cumulative|Projects until 2045") %>%
    pull(value)
  
  return(list(p.subsidies, subsidies.cumulative.2030, secaxis.factor))
}