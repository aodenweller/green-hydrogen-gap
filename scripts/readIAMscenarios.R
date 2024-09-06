readIAMscenarios <- function(file.path,
                             cap2023) {
  
  flh = 3750 # Full load hours in h/yr
  eta = 0.69 # efficiency in p.u.
  
  if (file_ext(file.path) == "csv") {
    data.iam <- read_csv(file.path) %>%
      pivot_longer(`2020`:`2050`,
                   names_to = "period",
                   values_to = "value")
    } else if (file_ext(file.path) == "xlsx") {
    data.iam <- read_excel(file.path) %>% 
      pivot_longer(`2020`:`2050`,
                   names_to = "period",
                   values_to = "value") 
  }
  
  names(data.iam) <- tolower(names(data.iam))
  
  # Approximate electrolysis capacity from secondary energy
  data.se.h2 <- data.iam %>% 
    filter(variable == "Secondary Energy|Hydrogen|Electricity") %>% 
    filter(!is.na(value)) %>% 
    calc_addVariable(
      "`Capacity|Hydrogen|Electricity`" = paste(" 1E6/3.6 * `Secondary Energy|Hydrogen|Electricity` / (", flh, "*", eta, ")"),
      units = "GW(el)",
      only.new = TRUE) %>% 
    mutate(modelscen = paste0(model, scenario))
  
  # Recalculate GW_Hw into GW_el
  data.cap.h2 <- data.iam %>% 
    filter(variable == "Capacity|Hydrogen|Electricity") %>% 
    filter(!is.na(value)) %>% 
    mutate(value = value/eta,
           unit = "GW(el)") %>% 
    mutate(modelscen = paste0(model, scenario))
  
  # Use capacity if reported and SE-approximated capacity if not
  modelscen.se.h2 <- unique(data.se.h2$modelscen)
  modelscen.cap.h2 <- unique(data.cap.h2$modelscen)
  
  modelscen.diff <- setdiff(modelscen.se.h2,
                            modelscen.cap.h2)
  
  # Combine
  data.tot <- data.cap.h2 %>% 
    bind_rows(data.se.h2 %>% filter(modelscen %in% modelscen.diff)) %>% 
    group_by(modelscen) %>% 
    # Remove model-scenario combinations that are always zero
    filter(!(all(value == 0))) %>% 
    # Remove model-scenario combinations that are less than standing capacity
    # in any time period from 2025-2050
    group_by(modelscen) %>%
    filter(!any(value < cumcap.2023 & period >= 2025))
  
}