readScenarios <- function(file.path) {
  
  # Read data
  data.scenarios <- read_excel(file.path) %>% 
    pivot_longer(cols = `2020`:`2050`, values_to = "value", names_to = "year")
  
  # Lower all names
  names(data.scenarios) <- tolower(names(data.scenarios))
  
  # Reformat
  data.scenarios <- data.scenarios %>% 
    rename(year.publication = "year of publication",
           scen.name = "short name") %>% 
    select(organisation, report, year.publication, scen.name,
           region, variable, unit, year, value)
  
  # Scenarios with capacity: No recalculation necessary
  data.scenarios.cap <- data.scenarios %>% 
    filter(str_detect(variable, "Capacity\\|Hydrogen\\|Electricity")) %>% 
    mutate(value.gw = value)
  
  # Recalculate final energy to GW using the following assumptions
  lhv = 3.333E4 # Hydrogen lower heating value in GWh/MtH2
  flh = 3750 # Full load hours in h/yr
  eff = 0.69 # Efficiency in p.u.
  
  # Scenarios with final energy: Calculate required electrolysis capacity
  data.scenarios.fe <- data.scenarios %>% 
    filter(str_detect(variable, "Final Energy\\|Hydrogen")) %>% 
    mutate(value.gw = case_when(unit == "MtH2/yr" ~ lhv/(flh*eff) * value,
                                unit == "EJ/yr" ~ (10^6/3.6)/(flh*eff) *value,
                                unit == "TWh/yr" ~ 1E3/(flh*eff) * value,
                                unit == "Mtoe/yr" ~ 11630/(flh*eff) * value))
  
  # Scenarios with electricity input: Calculate required electrolysis capacity
  data.scenarios.seinput <- data.scenarios %>% 
    filter(variable == "Secondary Energy Input|Electricity|Hydrogen") %>% 
    mutate(value.gw = case_when(unit == "TWh/yr" ~ 1E3/flh * value))
  
  # Combine data
  data.scenarios <- bind_rows(data.scenarios.cap,
                              data.scenarios.fe,
                              data.scenarios.seinput) %>%
    mutate(year = as.integer(year)) %>%
    filter(!is.na(value.gw))
  
  return(data.scenarios)
  
}