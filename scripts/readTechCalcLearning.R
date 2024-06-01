readTechCalcLearning <- function(file.costs,
                                 data.v2023,
                                 data.scenarios) {

  range.learning <- seq(2023,2045)
  range.costs <- seq(2024,2045)
  
  # Read costs data to calculate CAPEX learning (from 2023)
  data.capex <- read_excel(file.costs) %>% 
    # Remove source and comments columns
    select(-c("source", "comments")) %>% 
    # Filter for CAPEX variables
    filter(variable %in% c("Tech|ELH2|CAPEX", "Tech|ELH2|CAPEX|Stack|Share"),
           period == range.learning[1]) %>% 
    calc_addVariable("`Tech|ELH2|CAPEX|Stack`" = "`Tech|ELH2|CAPEX` * `Tech|ELH2|CAPEX|Stack|Share`", 
                     "`Tech|ELH2|CAPEX|Other`" = "`Tech|ELH2|CAPEX` * (1 - `Tech|ELH2|CAPEX|Stack|Share`)",
                     units = "USD/kW")
  
  # Read costs data for other technologies (from 2024)
  data.costs <- read_excel(file.costs) %>% 
    # Remove source and comments columns
    select(-c("source", "comments")) %>% 
    # Filter for non-CAPEX variables
    filter(!(variable %in% c("Tech|ELH2|CAPEX", "Tech|ELH2|CAPEX|Stack|Share"))) %>% 
    # Step 1: For each (scenario-variable-region-unit) tuple, set period to
    # first of range.costs if missing
    group_by(scenario, variable, region, unit) %>% 
    mutate(period = ifelse(is.na(period), range.costs[1], period)) %>% 
    # Step 2: For each (variable-region-period-unit) tuple, complete scenarios
    group_by(variable, region, period, unit) %>% 
    complete(scenario = c("Default", "Progressive", "Conservative")) %>% 
    # Step 3: Remove NA values if a non-NA value is available for any period 
    # for a given (scenario-variable-region-unit) tuple
    group_by(scenario, variable, region, unit) %>% 
    filter(all(is.na(value)) | (!(is.na(value)) & any(!is.na(value)))) %>% 
    # Step 4: If value doesn't exist fill with default
    group_by(variable, region, period, unit) %>% 
    mutate(value = ifelse(is.na(value) & scenario != "Default", value[scenario == "Default"], value)) %>% 
    # Step 5: Complete period and approximate linearly
    group_by(scenario, variable, region, unit) %>% 
    complete(period = range.costs) %>% 
    mutate(value = zoo::na.approx(value, na.rm = FALSE)) %>% 
    # Step 6: If value is NA, set to last non-NA value
    mutate(value = ifelse(is.na(value), last(na.omit(value)), value))

  # Calculate cumulative capacity (used for learning until 2030)
  data.v2023.cap <- data.v2023 %>%
    filter(year %in% seq(range.learning[1],2030)) %>%
    group_by(year) %>%
    summarise(cumcap = sum(cumcap.sum))
  
  # Get cumulative capacity for 2040 and 2050 from 1.5C scenario median
  data.scenarios.median <- data.scenarios %>%
    filter(year %in% c(2040, 2050)) %>%
    group_by(year) %>%
    summarise(cumcap = median(value.gw))
  
  # Bind project data and median capacity from 1.5°C scenarios
  data.cumcap <- bind_rows(data.v2023.cap, data.scenarios.median) %>% 
    complete(year = range.learning) %>% 
    # Interpolate linearly
    mutate(cumcap = zoo::na.approx(cumcap))
  
  # Stack learning rate
  lr.stack <- data.costs %>% 
    ungroup() %>% 
    filter(variable == "Tech|ELH2|CAPEX|Stack|LR") %>% 
    rename(lr = value) %>% 
    select(scenario, region, period, lr)
  
  # Calculate learning for stack
  capex.stack <- data.capex %>% 
    filter(variable == "Tech|ELH2|CAPEX|Stack") %>% 
    group_by(scenario, variable, region, unit) %>% 
    complete(period = range.learning) %>% 
    right_join(data.cumcap, by = c("period" = "year")) %>% 
    full_join(lr.stack, by = c("scenario", "region", "period")) %>% 
    group_by(scenario) %>% 
    mutate(value = first(value) * (cumcap / first(cumcap))**(log2(1-lr))) %>% 
    select(scenario, variable, region, unit, period, value)
  
  # Other CAPEX learning rate
  lr.other <- data.costs %>% 
    ungroup() %>% 
    filter(variable == "Tech|ELH2|CAPEX|Other|LR") %>% 
    rename(lr = value) %>% 
    select(scenario, region, period, lr)
  
  # Calculate learning for other CAPEX
  capex.other <- data.capex %>% 
    filter(variable == "Tech|ELH2|CAPEX|Other") %>% 
    group_by(scenario, variable, region, unit) %>% 
    complete(period = range.learning) %>% 
    right_join(data.cumcap, by = c("period" = "year")) %>% 
    full_join(lr.other, by = c("scenario", "region", "period")) %>% 
    group_by(scenario) %>% 
    mutate(value = first(value) * (cumcap / first(cumcap))**(log2(1-lr))) %>% 
    select(scenario, variable, region, unit, period, value)
  
  # Combine and calculate total
  capex.learning <- bind_rows(capex.stack, capex.other) %>% 
    ungroup() %>% 
    calc_addVariable("`Tech|ELH2|CAPEX`" = "`Tech|ELH2|CAPEX|Stack` + `Tech|ELH2|CAPEX|Other`",
                     units = "USD/kW") %>% 
    # Convert CAPEX to USD/MW
    mutate(value = ifelse(unit == "USD/kW", value*1E3, value),
           unit = "USD/MW")
  
  # Recombine and calculate total
  out <- bind_rows(data.costs, capex.learning) %>% 
    ungroup()
  
  return(out)
}