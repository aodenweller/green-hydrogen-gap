plotImplementationGap2023 <- function(
    region.plot,
    title,
    shift.incoming,
    shift.outgoing,
    y.upper,
    legend,
    annotate.gap = TRUE) {
  
  data.track.calc <- data.track.calc2023 %>% 
    filter(region %in% region.plot)
  
  # Plot sankey diagram
  p.track.2023 <-
    plotProjectTracking2023(
      data.track.calc = data.track.calc,
      title = title,
      shift.incoming = shift.incoming,
      shift.outgoing = shift.outgoing,
      y.upper = y.upper,
      annotate.labels = TRUE,
      annotate.gap = annotate.gap
    )
  
  # Recalculate rates for regional subset
  data.rates.calc2023.v21out <- calcRates(
    data.track.calc = data.track.calc,
    year.tracking = 2023,
    compare = "v2021-out")
  
  # Plot rates of success, delay and disappearance (v2021 vs outcome)
  temp <- plotRates(
    data.rates = data.rates.calc2023.v21out %>% 
      filter(region %in% region.plot),
    year.tracking = 2023
  )
  
  p.rates.2023.v21out.tot <- temp[[1]]
  p.rates.2023.v21out.tot.range <- temp[[2]]
  p.rates.2023.v21out.disagg <- temp[[3]]
  p.rates.2023.v21out.disagg.range <- temp[[4]]
  
  # Recalculate rates for regional subset
  data.rates.calc2023.v22out <- calcRates(
    data.track.calc = data.track.calc,
    year.tracking = 2023,
    compare = "v2022-out")
  
  # Plot rates of success, delay and disappearance (v2022 vs outcome)
  temp <- plotRates(
    data.rates = data.rates.calc2023.v22out %>% 
      filter(region %in% region.plot),
    year.tracking = 2023)
  
  p.rates.2023.v22out.tot <- temp[[1]]
  p.rates.2023.v22out.tot.range <- temp[[2]]
  p.rates.2023.v22out.disagg <- temp[[3]]
  p.rates.2023.v22out.disagg.range <- temp[[4]]
  
  # Recalculate rates for regional subset
  data.rates.calc2023.v23out <- calcRates(
    data.track.calc = data.track.calc,
    year.tracking = 2023,
    compare = "v2023-out")
  
  # Plot rates of success, delay and disappearance (v2022 vs outcome)
  temp <- plotRates(
    data.rates = data.rates.calc2023.v23out %>% 
      filter(region %in% region.plot),
    year.tracking = 2023)
  
  p.rates.2023.v23out.tot <- temp[[1]]
  p.rates.2023.v23out.tot.range <- temp[[2]]
  p.rates.2023.v23out.disagg <- temp[[3]]
  p.rates.2023.v23out.disagg.range <- temp[[4]]
  
  # Left-align panels
  plot.align <- align_plots(p.track.2023 + theme(legend.position = "none"),
                            p.rates.2023.v21out.tot + theme(legend.position = "none"),
                            align = "v",
                            axis = "l")
  
  # Build bottom row left
  # Horizontally align panels
  plot.align.left <- align_plots(plot.align[[2]],
                                 p.rates.2023.v21out.disagg + theme(legend.position = "none"),
                                 align = "h")
  
  bottom_row.left.panels.tot <- plot_grid(
    plot.align.left[[1]],
    p.rates.2023.v21out.tot.range,
    ncol = 1,
    rel_heights = c(1, 0.15),
    align = "v"
  )
  
  bottom_row.left.panels.disagg <- plot_grid(
    plot.align.left[[2]],
    p.rates.2023.v21out.disagg.range,
    ncol = 1,
    rel_heights = c(1, 0.15),
    align = "v"
  )
  
  bottom_row.left.panels <- plot_grid(
    bottom_row.left.panels.tot,
    bottom_row.left.panels.disagg,
    ncol = 2,
    align = "h"
  )
  
  bottom_row.left.title <- ggdraw() +
    draw_label(
      "Announcements by 2021 vs. outcome in 2024",
      x = 0.55,
      hjust = 0.5,
      vjust = 0,
      size = font.size
    )
  
  bottom_row.left <- plot_grid(
    bottom_row.left.title,
    bottom_row.left.panels,
    ncol = 1,
    rel_heights = c(0.15, 2)
  )
  
  # Build bottom row centre
  # Horizontally align panels
  plot.align.centre <- align_plots(p.rates.2023.v22out.tot + theme(legend.position = "none"),
                                   p.rates.2023.v22out.disagg + theme(legend.position = "none"),
                                   align = "h")
  
  bottom_row.centre.panels.tot <- plot_grid(
    plot.align.centre[[1]],
    p.rates.2023.v22out.tot.range,
    ncol = 1,
    rel_heights = c(1, 0.15),
    align = "v"
  )
  
  bottom_row.centre.panels.disagg <- plot_grid(
    plot.align.centre[[2]],
    p.rates.2023.v22out.disagg.range,
    ncol = 1,
    rel_heights = c(1, 0.15),
    align = "v"
  )
  
  bottom_row.centre.panels <- plot_grid(
    bottom_row.centre.panels.tot,
    bottom_row.centre.panels.disagg,
    ncol = 2,
    align = "h")
  
  bottom_row.centre.title <- ggdraw() +
    draw_label(
      "Announcements by 2022 vs. outcome in 2024",
      x = 0.55,
      hjust = 0.5,
      vjust = 0,
      size = font.size
    )
  
  bottom_row.centre <- plot_grid(
    bottom_row.centre.title,
    bottom_row.centre.panels,
    ncol = 1,
    rel_heights = c(0.15,2)
  )
  
  # Build bottom row right
  # Horizontally align panels
  plot.align.right <- align_plots(p.rates.2023.v23out.tot + theme(legend.position = "none"),
                                  p.rates.2023.v23out.disagg + theme(legend.position = "none"),
                                  align = "h")
  
  bottom_row.right.panels.tot <- plot_grid(
    plot.align.right[[1]],
    p.rates.2023.v23out.tot.range,
    ncol = 1,
    rel_heights = c(1, 0.15),
    align = "v"
  )
  
  bottom_row.right.panels.disagg <- plot_grid(
    plot.align.right[[2]],
    p.rates.2023.v23out.disagg.range,
    ncol = 1,
    rel_heights = c(1, 0.15),
    align = "v"
  )
  
  bottom_row.right.panels <- plot_grid(
    bottom_row.right.panels.tot,
    bottom_row.right.panels.disagg,
    ncol = 2,
    align = "h")
  
  bottom_row.right.title <- ggdraw() +
    draw_label(
      "Announcements by 2023 vs. outcome in 2024",
      x = 0.55,
      hjust = 0.5,
      vjust = 0,
      size = font.size)
  
  bottom_row.right <- plot_grid(
    bottom_row.right.title,
    bottom_row.right.panels,
    ncol = 1,
    rel_heights = c(0.15,2)
  )
  
  if (legend == "v21") {
    leg <- get_legend(p.rates.2023.v21out.tot)
  } else if (legend == "v22") {
    leg <- get_legend(p.rates.2023.v22out.tot)
  } else if (legend == "v23") {
    leg <- get_legend(p.rates.2023.v23out.tot)
  }
  
  # Complete bottom row
  bottom_row <- plot_grid(
    bottom_row.left,
    bottom_row.centre,
    bottom_row.right,
    NULL,
    leg,
    ncol = 5,
    rel_widths = c(1, 1, 1, 0.02, 0.25),
    labels = c("b", "c", "d", "", ""),
    label_size = font.size
  )
  
  # Build top row
  top_row <- plot_grid(
    plot.align[[1]],
    NULL,
    ncol = 2,
    rel_widths = c(1, 0),
    labels = "a",
    label_size = font.size)
  
  # Build entire plot
  p <- plot_grid(top_row,
                 NULL,
                 bottom_row,
                 ncol = 1,
                 rel_heights = c(1.3,0.2,1))
  
  return(p)
  
}