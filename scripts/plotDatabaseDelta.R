plotDatabaseDelta <- function(data.new.proj,
                              data.old.proj,
                              version,
                              by = "status") {
  
  data.plot <- full_join(
    data.new.proj %>% 
      filter(status != "Decommissioned",
             reference != 0),
    data.old.proj %>% 
      filter(status != "Decommissioned",
             reference != 0),
    by = c("reference" = "reference",
           "year" = "year",
           "status" = "status",
           "region" = "region")) %>%
    mutate(capacity.x = replace_na(capacity.x, 0),
           capacity.y = replace_na(capacity.y, 0)) %>% 
    mutate(deltacap = capacity.x - capacity.y)
  
  data.plot.net <- data.plot %>% 
    group_by(year) %>% 
    summarise(net = sum(deltacap))
  
  data.plot.left <- data.plot %>% 
    filter(year %in% seq(2020, 2024)) %>% 
    group_by(across(c("year", by))) %>% 
    arrange(-deltacap)
  
  data.plot.net.left <- data.plot.net %>% 
    filter(year %in% seq(2020, 2024))
  
  data.plot.right <- data.plot %>% 
    filter(year %in% seq(2024, 2030)) %>% 
    group_by(across(c("year", by))) %>% 
    arrange(-deltacap)
  
  data.plot.net.right <- data.plot.net %>% 
    filter(year %in% seq(2024, 2030))
  
  legname <- ifelse(by == "status", "Status", "Region")
  
  p.left <- ggplot() +
    geom_col(data = data.plot.left,
             mapping = aes(x = year, y = deltacap/1E3, fill = .data[[by]], color = .data[[by]]),
             linewidth = 0.25,
             alpha = 0.5) +
    scale_fill_npg(name = legname) +
    scale_color_npg(name = legname) +
    new_scale_color() +
    geom_point(data = data.plot.net.left,
               mapping = aes(x = year, y = net/1E3, color = "Net change"),
               size = 0.5) +
    scale_color_manual(name = NULL,
                       values = c("Net change" = "black")) +
    scale_x_continuous(name = "Year",
                       breaks = seq(2020, 2024)) +
    scale_y_continuous(name = "Change of added capacity (GW/yr)") +
    ggtitle(paste("Projects until 2024 by", by)) +
    background_grid(major = c("y"),
                    size.major = 0.25)
  
  p.right <- ggplot() +
    geom_col(data = data.plot.right,
             mapping = aes(x = year, y = deltacap/1E3, fill = .data[[by]], color = .data[[by]]),
             linewidth = 0.25,
             alpha = 0.5) +
    scale_fill_npg(name = legname) +
    scale_color_npg(name = legname) +
    new_scale_color() +
    geom_point(data = data.plot.net.right,
               mapping = aes(x = year, y = net/1E3, color = "Net change"),
               size = 0.5) +
    scale_color_manual(name = NULL,
                       values = c("Net change" = "black")) +
    scale_x_continuous(name = "Year",
                       breaks = seq(2024, 2030)) +
    scale_y_continuous(name = "Change of added capacity (GW/yr)") +
    ggtitle(paste("Projects from 2024 by", by)) +
    background_grid(major = c("y"),
                    size.major = 0.25)
  
  return(list(p.left, p.right))
}