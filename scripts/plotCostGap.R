plotCostGap <- function(scenario.select,
                        carbonprice.select,
                        greenproduct.select,
                        fossilcompetitor.select,
                        greenproduct.label,
                        fossilcompetitor.label,
                        title) {
  
  # LCOX
  lcox.colors <- c(
    "Electricity" = "#FDC366",
    "Green hydrogen" = "#76AF63",
    "VOM" = "#F5835A",
    "FOM" = "#AFA0CB",
    "CAPEX|Stack" = "#8F9F7C",
    "CAPEX|Other" = "#4B5C67",
    "CAPEX" = "#4B5C67"
  )
  
  lcox.labels <- c(
    "Electricity" = "Electricity",
    "Green hydrogen" = "Green hydrogen",
    "VOM" = "Transport and storage",
    "FOM" = "Fixed O&M",
    "CAPEX|Stack" = "Investment: Stack",
    "CAPEX|Other" = "Investment: Balance of plant",
    "CAPEX" = "Investment: Synthesis"
  )
  
  # Fossil competitor
  fossil.colors <- c(
    "CO2" = "grey30",
    "VOM" = "#F5835A",
    "Price" = "#B79F7E"
  )
  
  fossil.labels <- c(
    "CO2" = expression("CO"[2]*" price"),
    "VOM" = "Transport and storage",
    "Price" = "Price"
  )
  
  # Total colors
  tot.colors <- c(
    "greenproduct" = "#76AF63",
    "fossilcompetitor" = "#6A5541"
  )
  
  # Gap and subsidy color
  gap.color <- "#C10505"
  gap.alpha <- 0.1
  
  year.cols <- c(2024, 2030, 2035, 2040, 2045)
  
  # LCOX components
  data.plot.lcox <- data.lcox %>% 
    filter(period <= 2045) %>% 
    full_join(data.enduse.transportcost) %>% 
    filter(scenario == scenario.select,
           greenproduct == greenproduct.select,
           fossilcompetitor == fossilcompetitor.select) %>% 
    group_by(variable, region, unit, period, greenproduct, component) %>% 
    summarise(value = unique(value)) %>% 
    order.levels(component = names(lcox.labels))
  
  # Total
  data.plot.lcox.tot <- data.plot.lcox %>% 
    filter(is.na(component))
  
  # Components only
  data.plot.lcox <- data.plot.lcox %>% 
    filter(!is.na(component),
           period %in% year.cols)
  
  # Fossil cost components
  data.plot.fossilcosts <- data.fossilcosts %>% 
    filter(period <= 2045) %>% 
    full_join(data.enduse.transportcost) %>% 
    filter(scenario == scenario.select,
           greenproduct == greenproduct.select,
           fossilcompetitor == fossilcompetitor.select,
           carbonprice == carbonprice.select) %>% 
    group_by(variable, region, unit, period, fossilcompetitor, component) %>% 
    summarise(value = unique(value)) %>% 
    order.levels(component = names(fossil.labels))
  
  # Total
  data.plot.fossilcosts.tot <- data.plot.fossilcosts %>% 
    filter(is.na(component))
  
  # Components only
  data.plot.fossilcosts <- data.plot.fossilcosts %>% 
    filter(!is.na(component),
           period %in% year.cols)
  
  # Helper function
  # Find intersection between two lines (L1, L2), where L1: (x1,y1)-(x2,y2) and
  # L2: (x3,y3)-(x4,y4) and x1 = x3 and x2 = x4 (in years)
  findIntersection <- function(period, L1, L2) {
    x1 <- period[1]
    x2 <- period[2]
    y1 <- L1[1]
    y2 <- L1[2]
    y3 <- L2[1]
    y4 <- L2[2]
    x <- (x1*y2 - y1*x2 - x1*y4 + y3*x2)/(y3 - y4 - y1 + y2)
    y <- ((x1*y2 - y1*x2)*(y3-y4) - (y1-y2)*(x1*y4-y3*x2))/((x1-x2)*(y3-y4-y1+y2))
    return(c(x,y))
  }
  
  # Wrapper function for findIntersection
  findGapIntersection <- function(data.plot.gap) {
    # Find years between which intersection is located
    data.intersection <- bind_rows(
      data.plot.gap %>%
        filter(greenproduct > fossilcompetitor) %>%
        slice(n()),
      data.plot.gap %>%
        filter(greenproduct < fossilcompetitor) %>%
        slice(1)
    )
    # Intersection only exists if more than one row
    if (nrow(data.intersection) > 1) {
      # Determine intersection
      intersection <-
        findIntersection(data.intersection$period,
                         data.intersection$greenproduct,
                         data.intersection$fossilcompetitor)
      # Add intersection point to data and return
      data.plot.gap <- data.plot.gap %>% 
        filter(period < intersection[1]) %>% 
        add_row(region = "World",
                period = intersection[1], 
                greenproduct = intersection[2],
                fossilcompetitor = intersection[2])
    }
    return(data.plot.gap)
  }
  
  # Cost gap
  data.plot.gap <- data.plot.lcox.tot %>%
    ungroup() %>% 
    select(region, period, value) %>% 
    mutate(variable = "greenproduct") %>% 
    bind_rows(data.plot.fossilcosts.tot %>%
                ungroup() %>% 
                select(region, period, value) %>% 
                mutate(variable = "fossilcompetitor")) %>% 
    pivot_wider(names_from = variable, values_from = value) %>% 
    findGapIntersection()
  
  # Cost gap arrows
  data.plot.gap.arrows <- data.plot.gap %>%
    group_by(region) %>%
    complete(period = seq(2024, 2045, 0.5)) %>% 
    mutate(greenproduct = zoo::na.approx(greenproduct, na.rm = FALSE),
           fossilcompetitor = zoo::na.approx(fossilcompetitor, na.rm = FALSE)) %>% 
    filter(period %in% c(2027, 2032.5, 2037.5, 2042.5),
           greenproduct > fossilcompetitor + 15) %>% 
    mutate(greenproduct = greenproduct - 3,
           fossilcompetitor = fossilcompetitor + 3)

  # Plot
  p.costgap <- ggplot() +
    # Cost gap
    geom_ribbon(data = data.plot.gap,
                mapping = aes(x = period, ymin = fossilcompetitor, ymax = greenproduct),
                fill = gap.color,
                alpha = gap.alpha) +
    # Cost gap arrows
    geom_segment(data = data.plot.gap.arrows,
                 mapping = aes(x = period,
                               xend = period,
                               y = greenproduct,
                               yend = fossilcompetitor),
                 arrow = arrow(length = unit(1.5, "mm"), ends = "both"),
                 linewidth = 0.25,
                 color = gap.color) + 
    # Stacked bars for LCOX
    geom_col(data = data.plot.lcox,
             mapping = aes(x = period, y = value, fill = component),
             position = position_stacknudge(x = -0.4),
             width = 0.6) +
    scale_fill_manual(
      name = greenproduct.label,
      values = lcox.colors,
      labels = lcox.labels,
      guide = guide_legend(order = 2)) +
    geom_col(data = data.plot.lcox.tot %>% filter(period %in% year.cols),
             mapping = aes(x = period, y = value),
             position = position_nudge(x = -0.4),
             width = 0.6,
             linewidth = 0.25,
             fill = NA,
             color = tot.colors["greenproduct"]) +
    new_scale_fill() +
    # Stacked bars for fossils
    geom_col(data = data.plot.fossilcosts,
             mapping = aes(x = period, y = value, fill = component),
             position = position_stacknudge(x = 0.4),
             width = 0.6) +
    scale_fill_manual(
      name = fossilcompetitor.label,
      values = fossil.colors,
      labels = fossil.labels,
      guide = guide_legend(order = 3)) +
    geom_col(data = data.plot.fossilcosts.tot %>% filter(period %in% year.cols),
             mapping = aes(x = period, y = value),
             position = position_nudge(x = 0.4),
             width = 0.6,
             linewidth = 0.25,
             fill = NA,
             color = tot.colors["fossilcompetitor"]) +
    # Totals
    geom_line(data = data.plot.lcox.tot,
              mapping = aes(x = period, y = value, color = "greenproduct")) +
    geom_line(data = data.plot.fossilcosts.tot,
              mapping = aes(x = period, y = value, color = "fossilcompetitor")) +
    scale_color_manual(
      name = "Total cost",
      values = tot.colors,
      breaks = c("greenproduct", "fossilcompetitor"),
      labels = c("greenproduct" = greenproduct.label,
                 "fossilcompetitor" = fossilcompetitor.label),
      guide = guide_legend(order = 1)) +
    scale_x_continuous(name = "Year",
                       breaks = c(2024, seq(2030, 2045, 5))) +
    coord_cartesian(xlim = c(2023.5, 2045.5)) +
    ylab(expression("Specific cost ($/MWh"[LHV]*")")) +
    ggtitle(title)
  
  return(p.costgap)
}