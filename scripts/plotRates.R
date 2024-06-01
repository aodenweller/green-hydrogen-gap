plotRates <- function(data.rates, year.tracking){
  
  # Legend colours
  color.outcome <- c(
    "No info" = "darkred",
    "Delayed" = "darkorange",
    "On time" = "darkgreen",
    "NA" = "grey40"
  )
  
  # Legend labels
  labels.outcome <- c(
    "No info" = "Disappeared",
    "Delayed" = "Delayed",
    "On time" = "On time",
    "NA" = "Pending"
  )
 
  # Arrange
  data.plot <- data.rates %>% 
    group_by(outcome) %>% 
    arrange(cap.share.tot) %>% 
    order.levels(outcome = names(color.outcome))
  
  # Plot total
  plot.rates.tot <- ggplot() +
    geom_bar(data = data.plot,
             mapping = aes(x = 0.9, y = 100*cap.share.tot, fill = outcome, color = outcome),
             stat = "identity",
             width = 0.9,
             alpha = 0.5,
             linewidth = 0.25) +
    scale_fill_manual(name = "Outcome",
                      values = color.outcome,
                      labels = labels.outcome) +
    scale_color_manual(name = "Outcome",
                       values = color.outcome,
                       labels = labels.outcome) +
    scale_x_continuous(name = NULL,
                       breaks = 0.9,
                       labels = NULL) +
    scale_y_continuous(name = paste("Share of new capacity in", year.tracking, "(%)"), expand = expansion(add = 2)) +
    ggtitle("Total") +
    coord_cartesian(ylim = c(0,100), clip = "off") +
    theme(plot.margin = margin(t = 3.25, r = 1.5, b = 3.25, l = 3.25, unit = "pt"),
          axis.ticks.x = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.25))
  
  # Arrange
  data.plot <- data.rates %>% 
    group_by(outcome, cap.category) %>% 
    arrange(cap.share.disagg) %>% 
    order.levels(outcome = names(color.outcome))
  
  # Plot disaggregation by status
  plot.rates.disagg <- ggplot() +
    geom_bar(data = data.plot,
             mapping = aes(x = x.pos, y = 100*cap.share.disagg, fill = outcome, color = outcome, width = cap.category),
             stat = "identity",
             alpha = 0.5,
             linewidth = 0.25) + 
    scale_fill_manual(name = "Outcome",
                      values = color.outcome,
                      labels = labels.outcome) +
    scale_color_manual(name = "Outcome",
                       values = color.outcome,
                       labels = labels.outcome) +
    scale_x_continuous(name = NULL,
                       breaks = data.rates %>% pull(x.pos) %>% unique() %>% sort(),
                       labels = c("FID/\nConstruction", "Feasibility\nstudy", "Concept")) +
    scale_y_continuous(name = NULL, expand = expansion(add = 2)) +
    ggtitle("Disaggregation by status") +
    coord_cartesian(ylim = c(0,100), clip = "off") +
    theme(plot.margin = margin(t = 3.25, r = 3.25, b = 3.25, l = 1.5, unit = "pt"),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.25))
  
  # Return both plots
  return(list(plot.rates.tot, plot.rates.disagg))
   
}