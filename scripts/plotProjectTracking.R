#' Simplified plot of green hydrogen announcements and flows
#' 
plotProjectTracking <- function(data.track.calc,
                                shift.incoming,
                                shift.outgoing,
                                y.upper,
                                year.tracking,
                                annotate.labels = FALSE,
                                annotate.gap = FALSE) {
  
  # Helper function used in ifelse statements below
  isEq <- function(var, values){
    !is.na(var) & var %in% values
  }
    
  # Put delayed and disappeared bars from stat21t22 to into year2022
  # and from stat22t23 into year2023
  data.track.calc <- data.track.calc %>% 
    # Put stat21t22 disappeared into status2022 
    mutate(status2022 = ifelse(isEq(stat21t22, "No info"), "No info", status2022),
           stat21t22 = ifelse(isEq(stat21t22, "No info"), NA_character_, stat21t22)) %>% 
    # Put stat21t22 delayed into status2022
    mutate(status2022 = ifelse(isEq(stat21t22, "Delayed/out"), "Delayed/out", status2022),
           stat21t22 = ifelse(isEq(stat21t22, "Delayed/out"), NA_character_, stat21t22)) %>% 
    # Put stat22t23 disappeared into status2023
    mutate(status2023 = ifelse(isEq(stat22t23, "No info"), "No info", status2023),
           stat22t23 = ifelse(isEq(stat22t23, "No info"), NA_character_, stat22t23)) %>% 
    # Put stat22t23 delayed into status2023
    mutate(status2023 = ifelse(isEq(stat22t23, "Delayed/out"), "Delayed/out", status2023),
           stat22t23 = ifelse(isEq(stat22t23, "Delayed/out"), NA_character_, stat22t23)) %>% 
    # Remove capacity changes from plot, this will leave gaps between
    # stocks and flows, which should be explained in the figure caption
    mutate(stat21t22 = ifelse(isEq(stat21t22, c("Cap/out", "Cap/in")), NA_character_, stat21t22),
           stat22t23 = ifelse(isEq(stat22t23, c("Cap/out", "Cap/in")), NA_character_, stat22t23)) %>% 
    # Add flow of delayed and disappeared projects from status2022 to status2023
    mutate(status2023 = ifelse(isEq(status2022, "Delayed/out"), "Delayed/out", status2023),
           status2023 = ifelse(isEq(status2022, "No info"), "No info", status2023)) %>% 
    # Separate operational projects into (i) projects that were announced
    # earlier and finished on time and (ii) new projects
    mutate(status2022 = ifelse(isEq(status2022, "Operational") &
                                 !is.na(stat21t22), "Newly operational", status2022),
           status2023 = ifelse(isEq(status2023, "Operational") &
                                 !is.na(stat22t23), "Newly operational", status2023),
           status2023 = ifelse(isEq(status2022, "Newly operational"), "Newly operational", status2023)) %>% 
    # Simplify incoming projects: Assign all to "New"
    mutate(stat21t22 = ifelse(isEq(stat21t22, "Delayed/in"), "New", stat21t22),
           stat21t22 = ifelse(isEq(stat21t22, "Early"), "New", stat21t22),
           stat22t23 = ifelse(isEq(stat22t23, "Delayed/in"), "New", stat22t23),
           stat22t23 = ifelse(isEq(stat22t23, "Early"), "New", stat22t23))
  
  # Select required columns
  df <- data.track.calc %>% 
    select(status2021, stat21t22, status2022, stat22t23, status2023, capacity)

  # Reformat data to format required by ggsankey::geom_alluvial
  data.reformat <- NULL
  for (r in 1:dim(df)[1]) {
    # Get row
    row <- df[r,]
    # Get capacity
    cap <- row$capacity
    # Remove columns with NA
    row <- row %>% 
      select_if(~ !any(is.na(.))) %>%
      select(!capacity)
    # New dataframe
    temp <- tibble(
      x = names(row),
      node = unlist(row),
      next_x = lead(names(row)),
      next_node = lead(unlist(row)),
      capacity = cap)
    # Add to data
    data.reformat <- bind_rows(data.reformat, temp)
  }
  
  # Summarise data
  data.reformat <- data.reformat %>% 
    group_by(x, node, next_x, next_node) %>% 
    summarise(capacity = sum(capacity)) %>% 
    ungroup()
    
  # Order of axis items
  x.order <- c("status2021", "stat21t22", "status2022", "stat22t23", "status2023")
  
  # Order of node levels from bottom to top
  # Hack: Add invisible dummy in the right place to shift bars up and down
  node.order <- c(
    # Status bar
    "Newly operational", "Operational", "FID/Construction", "Feasibility study", "Concept",
    # Dummy to shift bars up
    "Dummy",
    # Outgoing projects
    "Delayed/out", "Dummy2", "No info",
    # Incoming projects
    "Delayed/in", "Early", "New")

  # Hack: Add invisible dummy rows in order to shift bars up and down
  data.plot.shift <- data.reformat %>% 
    # Shift change stratum
    add_row(x = "stat21t22",
            node = "Dummy",
            next_x = NA_character_,
            next_node = "Dummy",
            capacity = shift.incoming) %>% 
    # Shift change stratum
    add_row(x = "stat22t23",
            node = "Dummy",
            next_x = NA_character_,
            next_node = "Dummy",
            capacity = shift.incoming) %>% 
    # Create gap between project announcements and delayed
    add_row(x = "status2022",
            node = "Dummy",
            next_x = NA_character_,
            capacity = shift.outgoing) %>% 
    add_row(x = "status2023",
            node = "Dummy",
            next_x = NA_character_,
            capacity = shift.outgoing) %>%
    # Create gap between disappeared and delayed
    add_row(x = "status2022",
            node = "Dummy2",
            next_x = NA_character_,
            capacity = shift.outgoing) %>% 
    add_row(x = "status2023",
            node = "Dummy2",
            next_x = NA_character_,
            capacity = shift.outgoing) %>%
    # Recreate and order factors
    mutate(x = factor(x, levels = x.order),
           next_x = factor(next_x, levels = x.order)) %>% 
    mutate(node = factor(node, levels = node.order),
           next_node = factor(next_node, node.order))
  
  # Fill colours for project status
  fill.status <- pal_npg()(4)
  names(fill.status) <- c("Concept", "Feasibility study", "FID/Construction", "Newly operational")
  
  # Fill colours for transition stages (outgoing)
  fill.change.out <- c(
    #"Cap/out" = "grey30",
    "No info" = alpha("darkred", 0.07),
    "Delayed/out" = alpha("darkorange", 0.07),
    "Operational" = alpha("darkgreen", 0.7)
  )
  
  fill.change.in <- c(
    "New" = "grey70",
    "Early" = "grey40",
    #"Cap/in" = "grey30",
    "Delayed/in" = "grey20"
  )
  
  # All fill colours including NA for dummy
  fill.all <- c(fill.status, fill.change.out, fill.change.in,
                "Dummy" = NA_character_,
                "Dummy2" = NA_character_)
  
  # Border colours for project status (all black)
  color.status <- rep("black", length(fill.status))
  names(color.status) <- names(fill.status)
  
  # Border colours for transition stages (outgoing)
  color.change.out <- c(
    #"Cap/out" = NA_character_,
    "No info" = alpha("darkred", 0.2),
    "Delayed/out" = alpha("darkorange", 0.2),
    "Operational" = "black")
  
  # Border colours for transition stages (incoming)
  color.change.in <- rep(NA_character_, 3)
  names(color.change.in) <- c("New", "Early", "Delayed/in")
  
  # All border colours including NA for dummy
  color.all <- c(color.status, color.change.out,color.change.in,
                 "Dummy" = NA_character_,
                 "Dummy2" = NA_character_)
  
  # Linewidths for project status
  lwd.status <- rep(0.25, length(fill.status))
  names(lwd.status) <- names(fill.status)
  
  # Linewidths for transition stages (outgoing)
  lwd.change.out <- rep(0.25, length(color.change.out))
  names(lwd.change.out) <- names(color.change.out)
  #lwd.change.out["Cap/out"] <- 0
  
  # Linewidths for transition stages (incoming)
  lwd.change.in <- rep(0, length(color.change.in))
  names(lwd.change.in) <- names(color.change.in)
  
  # All linewidths including 0 for dummy
  lwd.all <- c(lwd.status, lwd.change.out, lwd.change.in,
               "Dummy" = 0,
               "Dummy2" = 0)
  
  # Legend labels
  labels.change.in <- c(
    "New" = "New",
    "Early" = paste("Expedited to", year.tracking),
    "Delayed/in" = paste("Delayed to", year.tracking)
    #"Cap/in" = "Capacity increased"
  )

  labels.change.out <- c(
    #"Cap/out" = "Capacity decreased",
    "Delayed/out" = paste0("Delayed to ", year.tracking+1,"+"),
    "No info" = "Disappeared",
    "Operational" = "On time"
  )
  
  # Dummy data to create legends
  data.dummy <- tibble(node = data.plot.shift %>% pull(node) %>% unique(),
                       value = 0)
  
  data.dummy.status <- data.dummy %>% 
    filter(node %in% names(fill.status))
  
  data.dummy.change.out <- data.dummy %>% 
    filter(node %in% names(fill.change.out))
  
  data.dummy.change.in <- data.dummy %>% 
    filter(node %in% names(fill.change.in))
  
  # x axis labels
  if (year.tracking == 2022){
    labels.x <- c(
      "status2021" = "Announcements\nby 2021",
      "status2022" = "Announcements\nby 2022",
      "status2023" = "Outcome\nin 2023")
  } else if (year.tracking >= 2023) {
    labels.x <- c(
      "status2021" = "Announcements\nby 2021",
      "status2022" = "Announcements\nby 2022",
      "status2023" = "Announcements\nby 2023")
  }
  
  # Alluvial plot
  p <- ggplot() +
    # Zero y-intercept line
    geom_hline(
      yintercept = 0,
      color = "grey",
      lwd = 0.25
    ) +
    # Create legend for status
    geom_col(data = data.dummy.status,
             mapping = aes(x = "status2022", y = value, fill = node, color = node, lwd = node),
             width = 0) +
    scale_fill_manual(name = "Status", values = fill.status,
                      breaks = names(fill.status),
                      guide = guide_legend(order = 2)) +
    scale_color_manual(name = "Status", values = color.status,
                       breaks = names(color.status),
                       guide = guide_legend(order = 2)) +
    scale_linewidth_manual(name = "Status", values = lwd.status,
                           breaks = names(lwd.status),
                           guide = guide_legend(order = 2)) +
    new_scale_fill() +
    new_scale_color() +
    new_scale("linewidth") +
    # Create legend for incoming projects
    geom_col(data = data.dummy.change.in,
             mapping = aes(x = "status2022", y = value, fill = node, color = node, lwd = node),
             width = 0) +
    scale_fill_manual(name = "New projects", values = fill.change.in,
                      breaks = names(fill.change.in),
                      labels = labels.change.in,
                      guide = guide_legend(order = 1)) +
    scale_color_manual(name = "New projects", values = color.change.in,
                       breaks = names(color.change.in),
                       labels = labels.change.in,
                       guide = guide_legend(order = 1)) +
    scale_linewidth_manual(name = "New projects", values = lwd.change.in,
                           breaks = names(lwd.change.in),
                           labels = labels.change.in,
                           guide = guide_legend(order = 1)) +
    new_scale_fill() +
    new_scale_color() +
    new_scale("linewidth") +
    # Create legend for outgoing projects
    geom_col(data = data.dummy.change.out,
             mapping = aes(x = "status2022", y = value, fill = node, color = node, lwd = node),
             width = 0) +
    scale_fill_manual(name = "Outcome", values = fill.change.out,
                      breaks = names(fill.change.out),
                      labels = labels.change.out,
                      guide = guide_legend(order = 3)) +
    scale_color_manual(name = "Outcome", values = color.change.out,
                       breaks = names(color.change.out),
                       labels = labels.change.out,
                       guide = guide_legend(order = 3)) +
    scale_linewidth_manual(name = "Outcome", values = lwd.change.out,
                           breaks = names(lwd.change.out),
                           labels = labels.change.out,
                           guide = guide_legend(order = 3)) +
    new_scale_fill() +
    new_scale_color() +
    new_scale("linewidth") +
    # Actual plot
    geom_alluvial(
      data = data.plot.shift,
      mapping = aes(
        x = x,
        next_x = next_x,
        node = node,
        next_node = next_node,
        fill = node,
        color = node,
        value = capacity,
        alpha = node,
        lwd = node
      ),
      flow.alpha = 0.12,
      flow.color = 0,
      width = 0.2,
      show.legend = FALSE) +
    # Legends
    scale_fill_manual(values = fill.all) +
    scale_color_manual(values = color.all) +
    scale_alpha_manual(values = c("Dummy" = 0,
                                  "Dummy2" = 0)) +
    scale_linewidth_manual(values = lwd.all) +
    # Formatting
    scale_x_discrete(
      name = NULL,
      breaks = c("status2021", "status2022", "status2023"),
      labels = labels.x) +
    scale_y_continuous(
      name = paste("Capacity additions announced for", year.tracking, "(GW)"),
      labels = function(x) x * 1E-3,
      breaks = c(seq(0, y.upper, 1000))
    ) +
    ggtitle(paste("Tracking global green hydrogen projects announced for", year.tracking))
  
  # Add annotations for announcements vs realisation
  # Status 2021
  cap.2021 <- data.track.calc %>% 
    filter(!is.na(status2021)) %>% 
    pull(capacity) %>% 
    sum()
  # Status 2022
  cap.2022 <- data.track.calc %>% 
    filter(!is.na(status2022), !(status2022 %in% c("Delayed/out", "No info"))) %>% 
    pull(capacity) %>% 
    sum()
  # Status 2023
  cap.2023 <- data.track.calc %>% 
    filter(!is.na(status2023), !(status2023 %in% c("Delayed/out", "No info"))) %>% 
    pull(capacity) %>% 
    sum()
  
  if (annotate.gap == TRUE) {
    
    p <- p +
      # Dashed line from 2021
      geom_segment(aes(x = 1, xend = 5.8, y = cap.2021, yend = cap.2021),
                   linetype = "dashed",
                   linewidth = 0.25) +
      # Dashed line from 2023
      geom_segment(aes(x = 5, xend = 5.8, y = cap.2023, yend = cap.2023),
                   linetype = "dashed",
                   linewidth = 0.25) +
      # Arrow
      geom_segment(aes(x = 5.7, xend = 5.7, y = cap.2021, yend = cap.2023),
                   arrow = arrow(length = unit(1.5, "mm"), ends = "both"),
                   linewidth = 0.5) +
      # Annotation
      geom_text(aes(x = 5.75, y = mean(c(cap.2021,cap.2023))),
                label = paste("\u2460 Green hydrogen\nimplementation\ngap in", year.tracking),
                hjust = 0,
                size = 6 /.pt,
                lineheight = 1,
                fontface = "bold")
  }
  
  if (annotate.labels == TRUE){
    
    labels.change.out <- c(
      #"Cap/out" = "Capacity decreased",
      "Delayed/out" = paste0("Delayed\nto ", year.tracking+1,"+"),
      "No info" = "Disappeared",
      "Operational" = "On time"
    )
    
    # Tibble with annotation labels
    # Hack: Use dummy labels with alpha=0 to preserve x axis order
    status2021.label <- ifelse(year.tracking >= 2021, "announced by ", "operational as of ")
    status2022.label <- ifelse(year.tracking >= 2022, "announced by ", "operational as of ")
    status2023.label <- ifelse(year.tracking >= 2023, "announced by ", "operational as of ")
    
    data.annotations.total <- tribble(
      ~x, ~y, ~label, ~alpha,
      "status2021", cap.2021, paste0(round(cap.2021/1E3, 2), " GW\n", status2021.label, "2021"), 1,
      "stat21t22", 0, "", 0,
      "status2022", cap.2022, paste0(round(cap.2022/1E3, 2), " GW\n", status2022.label, "2022"), 1,
      "stat22t23", 0, "", 0,
      "status2023", cap.2023, paste0(round(cap.2023/1E3, 2), " GW\n", status2023.label, "2023"), 1,
    ) %>% 
      mutate(x = factor(x, levels = c("status2021", "stat21t22", "status2022", "stat22t23", "status2023")))
    
    # Add annotations for announcements in status2021
    data.annotations.status <- data.track.calc %>%
      filter(!is.na(status2021)) %>%
      group_by(status2021) %>%
      summarise(sum = sum(capacity)) %>% 
      arrange(match(status2021, node.order)) %>% 
      transmute(x = "status2021",
                y = cumsum(sum) - 0.5*sum,
                label = status2021,
                alpha = 1) %>% 
      revalue.levels(label = c("Feasibility study" = "Feasibility\nstudy",
                               "FID/Construction" = "FID/\nConstruction")) %>% 
      add_row(x = "stat21t22", y = 0, label = "", alpha = 0) %>% 
      add_row(x = "status2022", y = 0, label = "", alpha = 0) %>% 
      add_row(x = "stat22t23", y = 0, label = "", alpha = 0) %>% 
      add_row(x = "status2023", y = 0, label = "", alpha = 0) %>% 
      mutate(x = factor(x, levels = c("status2021", "stat21t22", "status2022", "stat22t23", "status2023")))
    
    # Add annotations for new projects in stat21t22 and stat22t23
    data.annotations.stat21t22 <- data.track.calc %>% 
      filter(!is.na(stat21t22)) %>% 
      group_by(stat21t22) %>% 
      summarise(sum = sum(capacity)) %>% 
      # Only include annotation if larger than x MW
      filter(sum > 10) %>% 
      arrange(match(stat21t22, node.order)) %>% 
      transmute(x = "stat21t22",
                y = shift.incoming + cumsum(sum) - 0.5*sum,
                label = stat21t22,
                alpha = 1) %>% 
      revalue.levels(label = labels.change.in) %>% 
      add_row(x = "status2021", y = 0, label = "", alpha = 0) %>% 
      add_row(x = "status2022", y = 0, label = "", alpha = 0) %>% 
      add_row(x = "stat22t23", y = 0, label = "", alpha = 0) %>% 
      add_row(x = "status2023", y = 0, label = "", alpha = 0) %>% 
      mutate(x = factor(x, levels = c("status2021", "stat21t22", "status2022", "stat22t23", "status2023")))
    
    data.annotations.stat22t23 <- data.track.calc %>% 
      filter(!is.na(stat22t23)) %>% 
      group_by(stat22t23) %>% 
      summarise(sum = sum(capacity)) %>% 
      # Only include annotation if larger than x MW
      filter(sum > 10) %>% 
      arrange(match(stat22t23, node.order)) %>% 
      transmute(x = "stat22t23",
                y = shift.incoming + cumsum(sum) - 0.5*sum,
                label = stat22t23,
                alpha = 1) %>% 
      revalue.levels(label = labels.change.in) %>% 
      add_row(x = "status2021", y = 0, label = "", alpha = 0) %>% 
      add_row(x = "stat21t22", y = 0, label = "", alpha = 0) %>% 
      add_row(x = "status2022", y = 0, label = "", alpha = 0) %>% 
      add_row(x = "status2023", y = 0, label = "", alpha = 0) %>% 
      mutate(x = factor(x, levels = c("status2021", "stat21t22", "status2022", "stat22t23", "status2023")))
    
    # Add annotations for outcome in status2023
    data.annotations.outcome <- data.track.calc %>% 
      filter(!is.na(status2023)) %>% 
      group_by(status2023) %>% 
      summarise(sum = sum(capacity)) %>% 
      arrange(match(status2023, node.order)) %>% 
      transmute(x = "status2023",
                y = cumsum(sum) - 0.5*sum,
                label = status2023,
                alpha = 1) %>%
      mutate(y = ifelse(label=="Delayed/out", y + shift.outgoing, y),
             y = ifelse(label == "No info", y + 2*shift.outgoing, y)) %>% 
      add_row(x = "status2021", y = 0, label = "", alpha = 0) %>% 
      add_row(x = "stat21t22", y = 0, label = "", alpha = 0) %>% 
      add_row(x = "status2022", y = 0, label = "", alpha = 0) %>% 
      add_row(x = "stat22t23", y = 0, label = "", alpha = 0) %>% 
      mutate(x = factor(x, levels = c("status2021", "stat21t22", "status2022", "stat22t23", "status2023")))    
    
    data.annotations.outcome.failed <- data.annotations.outcome %>%
      filter(label %in% c("No info", "Delayed/out")) %>% 
      revalue.levels(label = labels.change.out)
      
    data.annotations.outcome.success <- data.annotations.outcome %>%
      filter(label %in% c("Operational", "Newly operational")) %>%
      revalue.levels(label = labels.change.out) %>%
      revalue.levels(label = c("Newly operational" = "New"))
    
    # Add annotations
    p <- p +
      new_scale("alpha") +
      geom_label_repel(data = data.annotations.total,
                       mapping = aes(x = x, y = y, label = label, alpha = alpha),
                       size = 5/.pt,
                       nudge_x = c(-0.5, 0, -0.5, 0, -0.5),
                       nudge_y = c(shift.outgoing/2, 0, shift.outgoing/2, 0, shift.outgoing/2),
                       label.padding = unit(0.15, "lines"),
                       segment.curvature = c(-0.3, 0, -0.3, 0, -0.3),
                       segment.ncp = 10,
                       segment.size = 0.25,
                       min.segment.length = 0,
                       lineheight = 1,
                       show.legend = FALSE,
                       max.iter = 0) +
      geom_text(data = data.annotations.status,
                mapping = aes(x = 0.85, y = y, label = label, alpha = alpha),
                size = 5 / .pt,
                hjust = 1,
                lineheight = 1) +
      geom_text(data = data.annotations.stat21t22,
                mapping = aes(x = 1.85, y = y, label = label, alpha = alpha),
                size = 5 /.pt,
                hjust = 1,
                lineheight = 1) +
      geom_text(data = data.annotations.stat22t23,
                mapping = aes(x = 3.85, y = y, label = label, alpha = alpha),
                size = 5 /.pt,
                hjust = 1,
                lineheight = 1) +
      geom_text(data = data.annotations.outcome.failed,
                mapping = aes(x = 5.15, y = y, label = label, alpha = alpha),
                size = 5 /.pt,
                hjust = 0,
                lineheight = 1) +
      geom_text_repel(data = data.annotations.outcome.success,
                      mapping = aes(x = 5.1, y = y, label = label, alpha = alpha),
                      size = 5 /.pt,
                      nudge_x = 0.2,
                      direction = "y",
                      segment.size = 0.25,
                      min.segment.length = 0,
                      hjust = 0,
                      lineheight = 1) +
      theme(legend.position = "none")
  }
  
  # Change spacing
  if (annotate.gap == TRUE){
    p <- p +
      theme(plot.margin = margin(t = 1, r = 22, b = 1, l = 1, unit = "mm")) +
      coord_cartesian(xlim = c(0.7, 5.1), clip = "off")
  }
  else if (annotate.labels == TRUE){
    p <- p +
      theme(plot.margin = margin(t = 1, r = 12, b = 1, l = 1, unit = "mm")) +
      coord_cartesian(xlim = c(0.7, 5.1), clip = "off")
  }

  return(p)
}
