plotGapSubsidies <- function(data.costs,
                             data.subsidies.annual,
                             scen,
                             withCarbonPrice,
                             color.cumulative,
                             ymax.subsidies = NA,
                             secaxis.factor = NA) {
  
  # Find intersection between two lines (L1, L2), where L1: (x1,y1)-(x2,y2) and
  # L2: (x3,y3)-(x4,y4) and x1 = x3 and x2 = x4 (in years)
  findIntersection <- function(period,lcoh,lcoe) {
    x1 <- period[1]
    x2 <- period[2]
    y1 <- lcoh[1]
    y2 <- lcoh[2]
    y3 <- lcoe[1]
    y4 <- lcoe[2]
    x <- (x1*y2 - y1*x2 - x1*y4 + y3*x2)/(y3 - y4 - y1 + y2)
    y <- ((x1*y2 - y1*x2)*(y3-y4) - (y1-y2)*(x1*y4-y3*x2))/((x1-x2)*(y3-y4-y1+y2))
    return(c(x,y))
  }
  
  # Wrapper function for findIntersection
  findGapIntersection <- function(data.plot.gap) {
    # Find years between which intersection must be
    data.intersection <- bind_rows(
      data.plot.gap %>%
        filter(LCOH > `Gas|Total`) %>%
        slice(n()),
      data.plot.gap %>%
        filter(LCOH < `Gas|Total`) %>%
        slice(1)
    )
    # Intersection only exists if more than one row
    if (nrow(data.intersection) > 1) {
      # Determine intersection
      intersection <-
        findIntersection(data.intersection$period,
                         data.intersection$LCOH,
                         data.intersection$`Gas|Total`)
      # Add intersection point to data and return
      data.plot.gap <- data.plot.gap %>% 
        filter(period < intersection[1]) %>% 
        add_row(region = "World",
                unit = "$/MWh", 
                period = intersection[1], 
                LCOH = intersection[2],
                `Gas|Total` = intersection[2])
    }
    return(data.plot.gap)
  }
  
  # Plotting period
  period.plot <- seq(2024, 2045)
  
  # Time steps at which stacked bars are shown
  period.bars <- c(2024, 2030, 2035, 2040, 2045)
  
  # Select scenario
  data.plot.subsidies.annual <- data.subsidies.annual %>% 
    filter(scenario == scen)
  
  # Scenario suffix for title
  if (scen == "Default") {
    scen.suffix = ""
  } else if (scen == "Progressive") {
    scen.suffix = "(progressive scenario)"
  } else if (scen == "Conservative") {
    scen.suffix = "(conservative scenario)"
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
  period.cumulative <- c(2026, 2028, 2030)

  # Select scenario
  data.costs <- data.costs %>% 
    filter(scenario == scen)
  
  # LCOH variables
  lcoh.labels <- c(
    "LCOH|Electricity" = "Electricity",
    "LCOH|VOM" = "Transport and storage",
    "LCOH|FOM" = "Fixed O&M",
    "LCOH|CAPEX|Stack" = "Investment: Stack",
    "LCOH|CAPEX|Other" = "Investment: Balance of plant"
  )
  
  lcoh.colors <- c(
    "LCOH|Electricity" = "#FDC366",
    "LCOH|VOM" = "#F5835A",
    "LCOH|FOM" = "#AFA0CB",
    "LCOH|CAPEX|Stack" = "#8F9F7C",
    "LCOH|CAPEX|Other" = "#4B5C67"
  )
  
  # LCOE gas variables
  gas.labels <- c(
    "Cost|Gas|CO2" = "CO2 price",
    "Price|Gas" = "Gas price"
  )
  # LCOE gas colours
  gas.colors <- c(
    "Cost|Gas|CO2" = "grey30",
    "Price|Gas" = "#B79F7E"
  )
  
  # LCOH and LCOE gas totals
  tot.labels <- c(
    "LCOH" = "Green hydrogen",
    # Define gas total below
    "Gas|Total" = "Natural gas"
  )
  # LCOH and LCOE gas colours
  tot.color <- c(
    "LCOH" = "#76AF63",
    "Gas|Total" = "#6A5541"
  )
  
  # Gap and subsidy color
  gap.color <- "#C10505"
  gap.alpha <- 0.1
  
  # With or without carbon price
  if (withCarbonPrice == TRUE){
    
    # LCOE gas data
    data.plot.gas <- data.costs %>%
      filter(variable %in% names(gas.labels)) %>% 
      order.levels(variable = names(gas.labels)) %>% 
      filter(period %in% period.bars)

    # Total gas costs including CO2 price
    data.costs <- data.costs %>% 
      calc_addVariable(
        "`Gas|Total`" = "`Cost|Gas`",
        unit = "$/MWh")
    
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
    
    # LCOE gas data
    data.plot.gas <- data.costs %>%
      filter(variable == "Price|Gas") %>% 
      filter(period %in% period.bars)
    
    # Calculate 
    data.costs <- data.costs %>% 
      calc_addVariable(
        "`Gas|Total`" = "`Price|Gas`",
        unit = "$/MWh")
    
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
  
  # LCOH variables for bar plots
  data.plot.lcoh <- data.costs %>% 
    filter(variable %in% names(lcoh.labels)) %>% 
    order.levels(variable = names(lcoh.labels)) %>% 
    filter(period %in% period.bars)
  
  data.plot.lcoh.tot <- data.plot.lcoh %>% 
    group_by(period) %>% 
    summarise(value = sum(value))
  
  # Total gas
  data.plot.gas.tot <- data.plot.gas %>% 
    group_by(period) %>% 
    summarise(value = sum(value))
  
  # LCOH and LCOE total data
  data.plot.tot <- data.costs %>% 
    filter(variable %in% names(tot.labels),
           period %in% period.plot) %>% 
    order.levels(variable = names(tot.labels))
  
  # Gap between LCOH and gas
  data.plot.gap <- data.plot.tot %>% 
    pivot_wider(names_from = variable, values_from = value) %>% 
    findGapIntersection()
  
  # Gap arrows
  data.plot.gap.arrows <- data.plot.gap %>%
    group_by(scenario, region, unit) %>% 
    complete(period = seq(2024,2045,0.5)) %>% 
    mutate(LCOH = zoo::na.approx(LCOH, na.rm = FALSE),
           `Gas|Total` = zoo::na.approx(`Gas|Total`, na.rm = FALSE)) %>% 
    filter(period %in% c(2027, 2032.5, 2037.5, 2042.5),
           LCOH > `Gas|Total` + 15) %>% 
    mutate(LCOH = LCOH - 3,
           `Gas|Total` = `Gas|Total` + 3)
  
  # Plot cost gap
  p.costgap <- ggplot() +
    # Competitiveness gap as background
    geom_ribbon(data = data.plot.gap,
                mapping = aes(x = period, 
                              ymin = `Gas|Total`,
                              ymax = LCOH),
                fill = gap.color,
                alpha = gap.alpha) +
    # Competitiveness gap arrows
    geom_segment(data = data.plot.gap.arrows,
                 mapping = aes(x = period,
                               xend = period,
                               y = LCOH,
                               yend = `Gas|Total`),
                 arrow = arrow(length = unit(1.5, "mm"), ends = "both"),
                 linewidth = 0.25,
                 color = gap.color) + 
    # LCOH
    geom_col(data = data.plot.lcoh,
             mapping = aes(x = period, y = value, fill = variable),
             position = position_stacknudge(x = -0.4),
             width = 0.6) +
    scale_fill_manual(name = "Levelised cost of hydrogen",
                      values = lcoh.colors,
                      labels = lcoh.labels) +
    geom_col(data = data.plot.lcoh.tot,
             mapping = aes(x = period, y = value),
             color = tot.color["LCOH"],
             fill = NA,
             width = 0.6,
             position = position_nudge(x = -0.4),
             linewidth = 0.25) +
    # LCOE gas
    new_scale_fill() +
    geom_col(data = data.plot.gas,
             mapping = aes(x = period, y = value, fill = variable),
             position = position_stacknudge(x = 0.4),
             width = 0.6) +
    scale_fill_manual(name = "Natural gas",
                      values = gas.colors,
                      labels = gas.labels) +
    geom_col(data = data.plot.gas.tot,
             mapping = aes(x = period, y = value),
             color = tot.color["Gas|Total"],
             fill = NA,
             width = 0.6,
             position = position_nudge(x = 0.4),
             linewidth = 0.25) +
    # Line for totals
    geom_line(data = data.plot.tot,
              mapping = aes(x = period, y = value, color = variable),
              linewidth = 0.5) +
    scale_color_manual(name = "Total cost",
                       values = tot.color,
                       labels = tot.labels,
                       guide = guide_legend(order = 1)) +
    xlab("Year") +
    ylab("Specific cost ($/MWh)") +
    ggtitle(paste("Cost gap", title.suffix, scen.suffix)) +
    theme(plot.margin = unit(c(3,3,3,3), units = "pt"))
  
  # Required annual subsidies
  # Grouped by projects built until 2030 and afterwards
  data.plot.subsidies.annual <- data.plot.subsidies.annual %>%
    filter(projects.until <= 2030) %>% 
    filter(period %in% period.plot) %>% 
    mutate(project.group = case_when(projects.until <= 2026 ~ "Until 2026",
                                     projects.until <= 2028 ~ "Until 2028",
                                     projects.until <= 2030 ~ "Until 2030")) %>% 
    group_by(scenario, variable, region, unit, period, project.group) %>% 
    summarise(value = sum(value)) %>% 
    order.levels(project.group = c("Until 2030", "Until 2028", "Until 2026"))
  
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
             mapping = aes(x = period, y = value, alpha = project.group),
             fill = gap.color) +
    scale_alpha_manual(name = "Annual subsidies (left axis)",
                       values = c("Until 2026" = 0.3,
                                  "Until 2028" = 0.2,
                                  "Until 2030" = 0.1),
                       labels = c("Until 2026" = "Projects 2024-2026",
                                  "Until 2028" = "Projects 2027-2028",
                                  "Until 2030" = "Projects 2029-2030"),
                       guide = guide_legend(order = 1)) +
    # Cumulative required subsidies
    geom_line(data = data.plot.subsidies.cumulative,
              mapping = aes(x = period, y = value.adjusted, linetype = variable),
              color = color.cum,
              linewidth = 0.5) +
    scale_linetype_manual(
      name = "Cumulative subsidies (right axis)",
      breaks = c(
        "Subsidies|Cumulative|Projects until 2030",
        "Subsidies|Cumulative|Projects until 2028",
        "Subsidies|Cumulative|Projects until 2026"
      ),
      values = c(
        "Subsidies|Cumulative|Projects until 2026" = "dotted",
        "Subsidies|Cumulative|Projects until 2028" = "dashed",
        "Subsidies|Cumulative|Projects until 2030" = "solid"
      ),
      labels = c(
        "Subsidies|Cumulative|Projects until 2026" = "Projects until 2026",
        "Subsidies|Cumulative|Projects until 2028" = "Projects until 2028",
        "Subsidies|Cumulative|Projects until 2030" = "Projects until 2030"),
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
    ggtitle(paste("Required subsidies to realise all projects by 2030\n", title.suffix, scen.suffix)) +
    theme(plot.margin = unit(c(3,3,3,3), units = "pt"),
          axis.line.y.right = element_line(color = color.cum),
          axis.ticks.y.right = element_line(color = color.cum),
          axis.text.y.right = element_text(color = color.cum),
          axis.title.y.right = element_text(color = color.cum))
  
  # Get cumulative level at last time step
  subsidies.cumulative.2030 <- data.plot.subsidies.cumulative %>%
    filter(period == period.plot[length(period.plot)],
           variable == "Subsidies|Cumulative|Projects until 2030") %>%
    pull(value)
  
  return(list(p.costgap, p.subsidies, subsidies.cumulative.2030, secaxis.factor)) 
}