plotSubsidyGap <- function(scenario.select,
                           subsidies.cumulative.2030.woco2,
                           subsidies.cumulative.2030.wco2,
                           ymax,
                           title) {
  # Announced subsidies
  data.subsidies.announced <- data.subsidies.tracker %>% 
    filter(region == "World",
           publication == 2023)
  
  # Subsidy dataframe
  data.plot.subsidies <- tribble(
    ~scenario, ~value,
    "Without carbon price", subsidies.cumulative.2030.woco2,
    "With carbon price", subsidies.cumulative.2030.wco2,
    "BNEF", data.subsidies.announced %>% pull(value)
  ) %>% 
    order.levels(scenario = c("Without carbon price", "With carbon price", "BNEF"))
  
  color.cum <- c(
    "Without carbon price" = "#0e6684",
    "With carbon price" = "#7C4569",
    "BNEF" = "grey30"
  )
  
  # Additional panel to compare required vs. announced subsidies
  p.subsidygap <- ggplot() +
    # Simple bar chart for subsidies
    geom_col(data = data.plot.subsidies,
             mapping = aes(x = scenario, y = value, fill = scenario, color = scenario),
             alpha = 0.3,
             linewidth = 0.25,
             show.legend = FALSE) +
    scale_fill_manual(values = color.cum) +
    scale_color_manual(values = color.cum) +
    # Annotataions (background layer)
    geom_segment(aes(x = 1.45, xend = 3.45,
                     y = data.plot.subsidies %>% filter(scenario == "Without carbon price") %>% pull(value),
                     yend = data.plot.subsidies %>% filter(scenario == "Without carbon price") %>% pull(value)),
                 linetype = "dashed",
                 linewidth = 0.25) +
    geom_segment(aes(x = 2.45, xend = 3,
                     y = data.plot.subsidies %>% filter(scenario == "With carbon price") %>% pull(value),
                     yend = data.plot.subsidies %>% filter(scenario == "With carbon price") %>% pull(value)),
                 linetype = "dashed",
                 linewidth = 0.25) +
    # Arrows for green hydrogen implementation gap in 2030
    geom_segment(
      aes(x = 2.85,
          xend = 2.85,
          y = data.plot.subsidies %>% filter(scenario == "BNEF") %>% pull(value),
          yend = data.plot.subsidies %>% filter(scenario == "With carbon price") %>% pull(value)),
      arrow = arrow(length = unit(1.5, "mm"), ends = "both"),
      linewidth = 0.5) +
    geom_segment(
      aes(x = 3.3,
          xend = 3.3,
          y = data.plot.subsidies %>% filter(scenario == "BNEF") %>% pull(value),
          yend = data.plot.subsidies %>% filter(scenario == "Without carbon price") %>% pull(value)),
      arrow = arrow(length = unit(1.5, "mm"), ends = "both"),
      linewidth = 0.5) +
    geom_text(
      aes(x = 1.5,
          y = data.plot.subsidies %>% filter(scenario == "Without carbon price") %>% pull(value) * 0.8,
          label = "\u2462 Green hydrogen\nimplementation gap\nin 2030"),
      size = 6 / .pt,
      hjust = 0,
      vjust = 0.5,
      lineheight = 1,
      fontface = "bold") + 
    # Formatting
    scale_x_discrete(name = NULL,
                     labels = c("Without carbon price" = "Required\nwithout\ncarbon price",
                                "With carbon price" = "Required\nwith ambitious\ncarbon price",
                                "BNEF" = "Announced\n(BNEF)")) +
    ggtitle(title) +
    scale_y_continuous(name = "Cumulative subsidies (Billion $)",
                       limits = c(0, ymax))
  
  return(p.subsidygap)
  
}