plotFutureImplementaionGap <- function(scenario) {
  
  # Color for cumulative with carbon price
  color.cumulative <- c(
    "With CO2" = "#7C4569",
    "Without CO2" = "#0e6684"
  )

  # Create plot panels for default scenario without carbon price
  p.noco2price <- plotGapSubsidies(
    data.costs = data.costs,
    data.subsidies.annual = data.subsidies.annual,
    scen = scenario,
    color.cumulative = color.cumulative,
    withCarbonPrice = FALSE
  )
  
  # Create plot panels for default scenario with carbon price
  p.co2price <- plotGapSubsidies(
    data.costs = data.costs,
    data.subsidies.annual = data.subsidies.annual,
    scen = scenario,
    color.cumulative = color.cumulative,
    withCarbonPrice = TRUE,
    ymax.subsidies = layer_scales(p.noco2price[[2]])$y$get_limits()[2],
    secaxis.factor = p.noco2price[[4]]
  )
  
  # Required vs. announced subsidies
  data.plot.subsidies <- data.subsidies.tracker %>%
    # Only show subsidies for BNEF 2023 World region
    filter(region == "World", publication == 2023) %>% 
    # Required subsidies with carbon price
    add_row(
      source = "With CO2",
      value = p.co2price[[3]],
      region = "World",
      unit = "bn USD"
    ) %>% 
    # Required subsidies without carbon price
    add_row(
      source = "Without CO2",
      value = p.noco2price[[3]],
      region = "World",
      unit = "bn USD"
    ) %>% 
    order.levels(source = c("Without CO2",
                            "With CO2",
                            "BNEF"))
  
  # Determine y limit of subsidy gap plot such that it matches the other panels
  ylimit <- layer_scales(p.noco2price[[2]])$y$get_limits()[2]*p.noco2price[[4]]
  
  # Additional panel to compare required vs. announced subsidies
  p.subsidygap <- ggplot() +
    # Simple bar chart for subsidies
    geom_col(data = data.plot.subsidies,
             mapping = aes(x = source, y = value, fill = source, color = source),
             alpha = 0.3,
             linewidth = 0.25,
             show.legend = FALSE) +
    scale_fill_manual(values = c(color.cumulative, "BNEF" = "grey30")) +
    scale_color_manual(values = c(color.cumulative, "BNEF" = "grey30")) +
    # Annotataions (background layer)
    geom_segment(aes(x = 1.45, xend = 3.45,
                     y = data.plot.subsidies %>% filter(source == "Without CO2") %>% pull(value),
                     yend = data.plot.subsidies %>% filter(source == "Without CO2") %>% pull(value)),
                 linetype = "dashed",
                 linewidth = 0.25) +
    geom_segment(aes(x = 2.45, xend = 3,
                     y = data.plot.subsidies %>% filter(source == "With CO2") %>% pull(value),
                     yend = data.plot.subsidies %>% filter(source == "With CO2") %>% pull(value)),
                 linetype = "dashed",
                 linewidth = 0.25) +
    # Arrows for green hydrogen implementation gap in 2030
    geom_segment(
      aes(x = 2.85,
          xend = 2.85,
          y = data.plot.subsidies %>% filter(source == "BNEF") %>% pull(value),
          yend = data.plot.subsidies %>% filter(source == "With CO2") %>% pull(value)),
      arrow = arrow(length = unit(1.5, "mm"), ends = "both"),
      linewidth = 0.5) +
    geom_segment(
      aes(x = 3.3,
          xend = 3.3,
          y = data.plot.subsidies %>% filter(source == "BNEF") %>% pull(value),
          yend = data.plot.subsidies %>% filter(source == "Without CO2") %>% pull(value)),
      arrow = arrow(length = unit(1.5, "mm"), ends = "both"),
      linewidth = 0.5) +
    geom_text(
      aes(x = 1.5,
          y = data.plot.subsidies %>% filter(source == "Without CO2") %>% pull(value) * 0.8,
          label = "\u2462 Green hydrogen\nimplementation gap\nin 2030"),
      size = 6 / .pt,
      hjust = 0,
      vjust = 0.5,
      lineheight = 1,
      fontface = "bold") + 
    # Formatting
    scale_x_discrete(name = NULL,
                     labels = c("Without CO2" = "Required\nwithout\ncarbon price",
                                "With CO2" = "Required\nwith\ncarbon price",
                                "BNEF" = "Announced\n(BNEF)")) +
    ggtitle("Required cumulative subsidies\nto realise all projects by 2030") +
    scale_y_continuous(name = "Cumulative subsidies (Billion $)",
                       limits = c(0, ylimit)) +
    theme(plot.margin = unit(c(3,3,3,3), units = "pt"))
  
  # Align plots on the left
  p.left <- align_plots(
    p.noco2price[[1]] + theme(legend.position = "none"),
    p.noco2price[[2]] + theme(legend.position = "none"),
    align = "v")
  
  # Align plots in the middle
  p.middle <- align_plots(
    p.co2price[[1]] + theme(legend.position = "none"),
    p.co2price[[2]] + theme(legend.position = "none"),
    align = "v")
  
  p.row1 <- plot_grid(
    p.left[[1]],
    p.middle[[1]],
    get_legend(p.co2price[[1]]),
    ncol = 3,
    rel_widths = c(1,1,0.7),
    labels = c("a", "b", ""),
    label_size = font.size
  )
  
  p.row2 <- plot_grid(
    p.left[[2]],
    p.middle[[2]],
    p.subsidygap,
    ncol = 3,
    rel_widths = c(1,1,0.7),
    align = "h",
    labels = c("c", "d", "e"),
    label_size = font.size
  )
  
  p.row3 <- plot_grid(
    get_legend(p.co2price[[2]] + theme(legend.box = "horizontal",
                                       legend.justification = c("center", "top"))),
    NULL,
    ncol = 2,
    rel_widths = c(2,0.7)
  )
  
  p.all <- plot_grid(
    p.row1,
    p.row2,
    p.row3,
    ncol = 1,
    rel_heights = c(1,1,0.17)
  )
  
  return(p.all)
  
}