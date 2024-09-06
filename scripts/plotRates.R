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
  
  cap.tot <- data.plot %>% 
    ungroup() %>% 
    summarise(cap = sum(capacity)/1E3) %>%
    pull(cap) %>% 
    round(2)
  
  # Plot total
  plot.rates.tot <- ggplot() +
    geom_bar(data = data.plot,
             mapping = aes(x = 1, y = 100*cap.share.tot, fill = outcome, color = outcome),
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
    scale_y_continuous(name = paste("Share of capacity additions in", year.tracking, "(%)"), expand = expansion(add = 2)) +
    ggtitle("Total") +
    coord_cartesian(ylim = c(0,100), clip = "off") +
    theme(plot.margin = margin(t = 3.25, r = 1.5, b = 3.25, l = 3.25, unit = "pt"),
          axis.ticks.x = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.25))
  
  # Plot range of total
  plot.rates.tot.range <- ggplot() +
    geom_errorbar(aes(x = 1, ymin = 0.1, ymax = 0.9),
                  linewidth = 0.25) +
    geom_text(aes(x = 1, y = 0.5, label = paste(round(cap.tot, 1), "GW")),
              size = 5 / .pt,
              vjust = 1,
              nudge_x = -0.3) +
    xlab(NULL) +
    xlab(NULL) +
    coord_flip(clip = "off") + 
    theme(plot.margin = margin(t = 3.25, r = 1.5, b = 10, l = 3.25, unit = "pt"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank())
  
  # Arrange
  data.plot <- data.rates %>% 
    group_by(outcome, cap.category) %>% 
    arrange(cap.share.disagg) %>% 
    order.levels(outcome = names(color.outcome))
  
  x.ticks <- data.rates %>% pull(x.pos) %>% unique() %>% sort()
  x.status <- data.rates %>% arrange(x.pos) %>% pull(status) %>% unique
  
  data.plot.labels <- data.rates %>% 
    group_by(status) %>% 
    summarise(size = sum(capacity)/1E3,
              x.pos = unique(x.pos),
              cap.category = unique(cap.category)) %>% 
    mutate(xmin = x.pos - cap.category * 0.5,
           xmax = x.pos + cap.category * 0.5) %>% 
    mutate(xlabel = status) %>% 
    revalue.levels(xlabel = c("FID/Construction" = "FID/\nConstruction",
                              "Feasibility study" = "Feasibility\nstudy",
                              "Concept" = "Concept")) %>% 
    arrange(match(status, x.status))

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
                       breaks = x.ticks,
                       labels = data.plot.labels %>% pull(xlabel)) +
    scale_y_continuous(name = NULL, expand = expansion(add = 2)) +
    ggtitle("By status") +
    coord_cartesian(ylim = c(0,100), clip = "off") +
    theme(plot.margin = margin(t = 3.25, r = 3.25, b = 3.25, l = 1.5, unit = "pt"),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.25))
  
  # Plot range of disaggregation
  plot.rates.disagg.range <- ggplot() +
    geom_errorbar(data = data.plot.labels,
                  mapping = aes(x = 1, ymin = xmin, ymax = xmax),
                  linewidth = 0.25) +
    geom_text(data = data.plot.labels,
              mapping = aes(x = 1, y = x.pos, label = paste(round(size, 1), "GW")),
              size = 5 / .pt,
              vjust = 1,
              nudge_x = -0.3) +
    xlab(NULL) +
    xlab(NULL) +
    coord_flip(clip = "off") + 
    theme(plot.margin = margin(t = 3.25, r = 3.25, b = 10, l = 1.5, unit = "pt"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank())
  
  # Return both plots
  return(list(plot.rates.tot, plot.rates.tot.range, plot.rates.disagg, plot.rates.disagg.range))
   
}
