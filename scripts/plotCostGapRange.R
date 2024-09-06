plotCostGapRange <- function(carbonprice.select,
                             greenproduct.select,
                             fossilcompetitor.select,
                             greenproduct.label,
                             fossilcompetitor.label,
                             remove.xaxis.labels = TRUE,
                             title.noco2 = FALSE,
                             adjust.lcox = 1) {
  
  # LCOX
  lcox.colors <- c(
    "Electricity" = "#FDC366",
    "Hydrogen" = "#76AF63",
    "Carbon" = "#E8E88A",
    "VOM" = "#F5835A",
    "FOM" = "#AFA0CB",
    "CAPEX|Stack" = "#8F9F7C",
    "CAPEX|Other" = "#4B5C67",
    "CAPEX" = "#4B5C67"
  )
  
  lcox.labels <- c(
    "Electricity" = "Electricity",
    "Hydrogen" = "Green hydrogen",
    "Carbon" = "Renewable carbon",
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
  
  year.cols <- c(2024, 2030, 2035, 2040, 2045, 2050)
  
  # LCOX range (conservative - progressive)
  data.plot.lcox.range <- data.lcox %>% 
    full_join(data.enduse.transportcost) %>% 
    filter(scenario %in% c("Progressive", "Conservative"),
           greenproduct == greenproduct.select,
           fossilcompetitor == fossilcompetitor.select,
           is.na(component)) %>% 
    group_by(scenario, region, period) %>% 
    mutate(value = 1/adjust.lcox * value) %>% 
    pivot_wider(names_from = scenario, values_from = value)
  
  # LCOX components (default)
  data.plot.lcox <- data.lcox %>% 
    full_join(data.enduse.transportcost) %>% 
    filter(scenario == "Default",
           greenproduct == greenproduct.select,
           fossilcompetitor == fossilcompetitor.select) %>% 
    group_by(variable, region, unit, period, greenproduct, component) %>% 
    summarise(value = unique(value)) %>% 
    mutate(value = 1/adjust.lcox * value) %>% 
    order.levels(component = names(lcox.labels))
  
  # Total (default)
  data.plot.lcox.tot <- data.plot.lcox %>% 
    filter(is.na(component))
  
  # Components only (default)
  data.plot.lcox <- data.plot.lcox %>% 
    filter(!is.na(component),
           period %in% year.cols)
  
  # Fossil cost range (conservative - progressive)
  data.plot.fossilcosts.range <- data.fossilcosts %>% 
    full_join(data.enduse.transportcost) %>% 
    filter(scenario %in% c("Progressive", "Conservative"),
           greenproduct == greenproduct.select,
           fossilcompetitor == fossilcompetitor.select,
           carbonprice == carbonprice.select,
           is.na(component)) %>% 
    group_by(scenario, region, period) %>% 
    summarise(value = unique(value)) %>% 
    pivot_wider(names_from = scenario, values_from = value)
  
  # Fossil cost components (default)
  data.plot.fossilcosts <- data.fossilcosts %>% 
    full_join(data.enduse.transportcost) %>% 
    filter(scenario == "Default",
           greenproduct == greenproduct.select,
           fossilcompetitor == fossilcompetitor.select,
           carbonprice == carbonprice.select) %>% 
    group_by(variable, region, unit, period, fossilcompetitor, component) %>% 
    summarise(value = unique(value)) %>% 
    order.levels(component = names(fossil.labels))
  
  # Total (default)
  data.plot.fossilcosts.tot <- data.plot.fossilcosts %>% 
    filter(is.na(component))
  
  # Components only (default)
  data.plot.fossilcosts <- data.plot.fossilcosts %>% 
    filter(!is.na(component),
           period %in% year.cols)
  
  # Plot title
  title.suffix <- ifelse(carbonprice.select == TRUE, "with carbon price", "without carbon price")
  title.suffix <- ifelse(title.noco2 == TRUE, "", title.suffix)
  
  # Plot
  p.costgaprange <- ggplot() +
    # LCOX range
    geom_ribbon(data = data.plot.lcox.range,
                mapping = aes(x = period, ymin = Progressive, ymax = Conservative, fill = "greenproduct"),
                alpha = 0.2) +
    # LCOX default
    geom_line(data = data.plot.lcox.tot,
              mapping = aes(x = period, y = value, color = "greenproduct")) +
    # Fossil costs range
    geom_ribbon(data = data.plot.fossilcosts.range,
                mapping = aes(x = period, ymin = Progressive, ymax = Conservative, fill = "fossilcompetitor"),
                alpha = 0.2) +
    # Fossil costs default
    geom_line(data = data.plot.fossilcosts.tot,
              mapping = aes(x = period, y = value, color = "fossilcompetitor")) +
    # Legend
    scale_color_manual(
      name = "Total cost",
      values = tot.colors,
      breaks = c("greenproduct", "fossilcompetitor"),
      labels = c("greenproduct" = greenproduct.label,
                 "fossilcompetitor" = fossilcompetitor.label),
      guide = guide_legend(order = 1)) +
    scale_fill_manual(
      name = "Total cost",
      values = tot.colors,
      breaks = c("greenproduct", "fossilcompetitor"),
      labels = c("greenproduct" = greenproduct.label,
                 "fossilcompetitor" = fossilcompetitor.label),
      guide = guide_legend(order = 1)) +
    new_scale_fill() +
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
    scale_x_continuous(name = NULL,
                       breaks = c(2024, seq(2030, 2050, 5))) +
    ylab(NULL) +
    ggtitle(paste(greenproduct.select, "vs.", fossilcompetitor.select, title.suffix))
  
  if (remove.xaxis.labels == TRUE) {
    p.costgaprange <- p.costgaprange + theme(axis.text.x = element_blank())
  }
  
  return(p.costgaprange)
}