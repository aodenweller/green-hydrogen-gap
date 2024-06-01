calcRates <- function(data.track.calc, year.tracking, compare){
  
  if (compare == "v2021-v2023") {
    # Only consider project announcements in v2021
    data.track.calc <- data.track.calc %>% 
      filter(status2021 != "Operational",
             year2021 == year.tracking) %>%
      rename(status = status2021)
    
  } else if (compare == "v2022-v2023") {
    # Only consider project announcements in v2022
    data.track.calc <- data.track.calc %>% 
      filter(status2022 != "Operational",
             year2022 == year.tracking) %>% 
      rename(status = status2022)
    
  } else {
    stop("'compare' needs to be either 'v2021-v2023' or 'v2022-v2023'")
  }
  
  # Determine outcome of projects
  data.rates <- data.track.calc %>%
    # Remove dummy projects introduced for capacity changes (reference > 10k)
    filter(reference < 10000) %>% 
    # Create outcome column
    mutate(outcome = case_when(status2023 == "Operational" ~ "On time",
                               stat21t22 == "No info" ~ "No info",
                               stat22t23 == "No info" ~ "No info",
                               stat21t22 == "Delayed/out" ~ "Delayed",
                               stat22t23 == "Delayed/out" ~ "Delayed"))
  
  # Determine total rates irrespective of the project status
  data.rates.tot <- data.rates %>% 
    ungroup() %>% 
    mutate(cap.share.tot = capacity / sum(capacity)) %>% 
    order.levels(outcome = c("No info", "Delayed", "On time")) %>% 
    arrange(cap.share.tot) %>% 
    select(reference, cap.share.tot)
  
  # Determine announced newly added capacity in 2021
  # This is required to make the status bar widths proportional to the
  # total sum of capacity within that status category
  cap.tot <- data.rates %>% 
    pull(capacity) %>% 
    sum()
  
  # Determine disaggregated rates for each status, normalised to the sum of
  # capacity each within the status category 
  data.rates.disagg <- data.rates %>% 
    group_by(status) %>% 
    mutate(cap.share.disagg = capacity / sum(capacity)) %>%
    mutate(cap.category = 0.9*sum(capacity)/cap.tot) %>% 
    order.levels(outcome = c("No info", "Delayed", "On time")) %>% 
    arrange(cap.share.disagg)
  
  # Determine the x position of each status category as a function of the 
  # capacity within that category
  data.rates.disagg.xpos <- data.rates.disagg %>%
    group_by(status) %>%
    summarise(cap.category = unique(cap.category)) %>%
    arrange(match(
      status,
      c("FID/Construction", "Feasibility study", "Concept")
    )) %>%
    # Position determined using the cumulative sum of cap.category
    mutate(x.pos = 1.55 + cumsum(cap.category) - 0.5 * cap.category) %>%
    # Create some distance between status categories
    mutate(x.pos = x.pos + c(0, 0.05, 0.1)) %>%
    select(-cap.category)
  
  # Merge the disaggregated rates for each status with the x position
  data.rates.disagg <-
    full_join(data.rates.disagg,
              data.rates.disagg.xpos,
              by = c("status" = "status"))
  
  # Merge the total rates with the disaggregated rates
  data.rates <- 
    full_join(data.rates.disagg, data.rates.tot) %>% 
    ungroup() %>% 
    mutate(outcome = case_when(is.na(outcome) ~ "NA",
                               TRUE ~ outcome))
  
  # Return 
  return(data.rates)
   
}