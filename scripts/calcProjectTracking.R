#' Calculate project tracking
#' 
#' @param data.v2021proj Dataset of project announcements in 2021
#' @param data.v2022proj Dataset of project announcements in 2022
#' @param data.v2023proj Dataset of project announcements in 2023
#' @param year.tracking Year for which projects should be tracked
#' @returns Wide dataset with five additional columns for a Sankey plot
#' 
calcProjectTracking <- function(data.v2021proj,
                                data.v2022proj,
                                data.v2023proj,
                                year.tracking) {
  
  # Helper function used in ifelse statements below
  isEq <- function(var, values){
    !is.na(var) & var %in% values
  }
  
  ### Added capacities
  ### Only extract added capacities in year.tracking, ignoring cumulative
  ### project announcements and already operational projects

  # Data handling for IEA v2021
  data.v2021proj.renamed <- data.v2021proj %>%
    # Exclude confidential projects
    filter(reference != 0) %>%
    # Exclude operational projects (only additional projects)
    filter(status != "Operational") %>% 
    # Exclude projects that have already been decommissioned again
    # This assumes that the decommissioned date is true, even if in the future
    group_by(reference) %>%
    filter(!(n() >= 2 & max(year) <= year.tracking)) %>%
    # Remove decommissioned rows, which are now all beyond the tracking year
    filter(status != "Decommissioned") %>%
    # Rename columns
    rename(
      name2021 = name,
      region2021 = region,
      status2021 = status,
      year2021 = year,
      capacity2021 = capacity
    )

  # Data handling for IEA v2022
  data.v2022proj.renamed <- data.v2022proj %>%
    # Exclude confidential projects
    filter(reference != 0) %>%
    # Exclude projects that have already been decommissioned again
    # This assumes that the decommissioned date is true, even if in the future
    group_by(reference) %>%
    filter(!(n() >= 2 & max(year) <= year.tracking)) %>%
    # Remove decommissioned rows, which are now all beyond the tracking year
    filter(status != "Decommissioned") %>%
    # Rename columns
    rename(
      name2022 = name,
      region2022 = region,
      status2022 = status,
      year2022 = year,
      capacity2022 = capacity
    )
  
  # Data handling for IEA v2023
  data.v2023proj.renamed <- data.v2023proj %>%
    # Exclude confidential projects
    filter(reference != 0) %>%
    # Exclude projects that have already been decommissioned again
    # This assumes that the decommissioned date is true, even if in the future
    group_by(reference) %>%
    filter(!(n() >= 2 & max(year) <= year.tracking)) %>%
    # Remove decommissioned rows, which are now all beyond the tracking year
    filter(status != "Decommissioned") %>%
    # Rename columns
    rename(
      name2023 = name,
      region2023 = region,
      status2023 = status,
      year2023 = year,
      capacity2023 = capacity
    )
  
  # Join all datasets
  data.list <- list(data.v2021proj.renamed,
                    data.v2022proj.renamed,
                    data.v2023proj.renamed)
  
  data.track <- data.list %>% 
    reduce(full_join, by = "reference")
  
  # Continue processing combined data
  data.track.calc <- data.track %>%
    ungroup() %>% 
    # If available use region2023, otherwise region2022, otherwise region2021,
    # because v2021 and v2022 seem to have a few country code errors
    mutate(region = case_when(!is.na(region2023) ~ region2023,
                              !is.na(region2022) ~ region2022,
                              !is.na(region2021) ~ region2021)
           ) %>% 
    # Select required columns
    select(reference, region,
           name2021, status2021, year2021, capacity2021,
           name2022, status2022, year2022, capacity2022,
           name2023, status2023, year2023, capacity2023) %>% 
    # Filter projects that have been announced for year.tracking in any dataset
    rowwise() %>% 
    filter(any(c(year2021, year2022, year2023) == year.tracking)) %>% 
    ungroup() %>% 
    # If projects announced for year.tracking are finished early, change
    # year2022 or year2023 to year.tracking to ensure these projects count
    mutate(
      year2022 = ifelse(
        status2022 == "Operational" & year2022 < year.tracking,
        year.tracking,
        year2022
      ),
      year2023 = ifelse(
        status2023 == "Operational" & year2023 < year.tracking,
        year.tracking,
        year2023
      )
    ) %>%
    # Add column for changes between v2021 and v2022
    mutate(
      stat21t22 = 
        case_when(
          # New project
          is.na(year2021) & year2022 == year.tracking ~ "New",
          # Project delayed to year.tracking (from earlier years)
          year2021 < year2022 & year2022 == year.tracking ~ "Delayed/in",
          # Project brought forward to year.tracking (from later years)
          year2021 > year2022 & year2022 == year.tracking ~ "Early",
          # Project delayed (after year.tracking)
          year2021 < year2022 & year2021 == year.tracking ~ "Delayed/out",
          # Project disappeared
          !is.na(year2021) & is.na(year2022) ~ "No info"
        )
    ) %>% 
    # Add column for changes between v2022 and v2023
    mutate(
      stat22t23 = 
        case_when(
          # New project
          is.na(year2022) & year2023 == year.tracking ~ "New",
          # Project delayed to year.tracking (from earlier years)
          year2022 < year2023 & year2023 == year.tracking ~ "Delayed/in",
          # Project brought forward to year.tracking (from later years)
          year2022 > year2023 & year2023 == year.tracking ~ "Early",
          # Project delayed (after year.tracking)
          year2022 < year2023 & year2022 == year.tracking ~ "Delayed/out",
          # Project disappeared
          !is.na(year2022) & is.na(year2023) ~ "No info"
        )
    )

  ### Account for changes in project sizes across database versions:
  ### (1) Create a dummy project with capacity = delta capacity
  ### (2) Adjust the original project capacity for delta capacity
  
  # Calculate dummy and adjust orig if capacity decreased from v2021 to v2022
  capchanges.21t22.out <- data.track.calc %>% 
    filter(capacity2021 > capacity2022,
           year2021 == year.tracking,
           year2022 == year.tracking)
  
  # Dummy projects (v2021 to v2022 decreased)
  capchanges.21t22.out.dummy <- capchanges.21t22.out %>% 
    # Set capacity to difference between v2021 and v2022 
    mutate(capacity = capacity2021 - capacity2022) %>%
    # To stat21t22 add stratum "Cap/out" for outgoing (decreasing) capacity
    mutate(stat21t22 = "Cap/out") %>% 
    # Assign arbitrarily large reference numbers to avoid interference
    mutate(reference = reference + 10000) %>% 
    # Remove sankey flows after stat21t22
    mutate(status2022 = NA_character_,
           stat22t23 = NA_character_,
           status2023 = NA_character_)
  
  # Original projects (v2021 to v2022 decreased)
  capchanges.21t22.out.orig <- capchanges.21t22.out %>% 
    # Set capacity to value in v2022 (smaller value)
    mutate(capacity = capacity2022)
  
  # Combine data again
  capchanges.21t22.out <- bind_rows(capchanges.21t22.out.dummy,
                                    capchanges.21t22.out.orig)

  
  ### Calculate dummy and adjust orig if capacity increased from v2021 to v2022
  capchanges.21t22.in <- data.track.calc %>% 
    filter(capacity2021 < capacity2022,
           year2021 == year.tracking,
           year2022 == year.tracking)
  
  # Dummy projects (v2021 to v2022 increased)
  capchanges.21t22.in.dummy <- capchanges.21t22.in %>%
    # Set capacity to difference between v2022 and v2021
    mutate(capacity = capacity2022 - capacity2021) %>% 
    # To stat21t22 add stratum "Cap/in" for incoming (increasing) capacity
    mutate(stat21t22 = "Cap/in") %>% 
    # Assign arbitrarily large reference numbers to avoid interference
    mutate(reference = reference + 20000) %>% 
    # Remove sankey flows before stat21t22
    mutate(status2021 = NA_character_)
  
  # Original projects (v2021 to v2022 increased)
  capchanges.21t22.in.orig <- capchanges.21t22.in %>%
    # Set capacity to value in v2021 (smaller value) 
    mutate(capacity = capacity2021)

  # Combine data again
  capchanges.21t22.in <- bind_rows(capchanges.21t22.in.dummy,
                                   capchanges.21t22.in.orig)
  
  
  ### Calculate dummy and adjust orig if capacity decreased from v2022 to v2023
  capchanges.22t23.out <- data.track.calc %>% 
    filter(capacity2022 > capacity2023,
           year2022 == year.tracking,
           year2023 == year.tracking)
  
  # Dummy projects (v2022 to v2023 decreased)
  capchanges.22t23.out.dummy <- capchanges.22t23.out %>% 
    # Set capacity to difference between v2022 and v2023
    mutate(capacity = capacity2022 - capacity2023) %>% 
    # To stat22t23 add stratum "Cap/out" for outgoing (decreasing) capacity
    mutate(stat22t23 = "Cap/out") %>% 
    # Assign arbitrarily large reference numbers to avoid interference
    mutate(reference = reference + 30000) %>% 
    # Remove sankey flows after stat22t23
    mutate(status2023 = NA_character_)
  
  # Original projects (v2022 to v2023 decreased)
  capchanges.22t23.out.orig <- capchanges.22t23.out %>% 
    # Set capacity to value in v2023 (smaller value)
    mutate(capacity = capacity2023)
  
  # Combine data again
  capchanges.22t23.out <- bind_rows(capchanges.22t23.out.dummy,
                                    capchanges.22t23.out.orig)
  
  
  ### Calculate dummy and adjust orig if capacity increased from v2022 to v2023
  capchanges.22t23.in <- data.track.calc %>% 
    filter(capacity2022 < capacity2023,
           year2022 == year.tracking,
           year2023 == year.tracking)
  
  # Dummy projects (v2022 to v2023 increased)
  capchanges.22t23.in.dummy <- capchanges.22t23.in %>% 
    # Set capacity to difference between v2023 and v2022
    mutate(capacity = capacity2023 - capacity2022) %>% 
    # To stat22t23 add stratum "Cap/in" for incoming (increasing) capacity
    mutate(stat22t23 = "Cap/in") %>% 
    # Assign arbitrarily large reference numbers to avoid interference
    mutate(reference = reference + 40000) %>% 
    # Remove sankey flows before stat22t23
    mutate(status2021 = NA_character_,
           stat21t22 = NA_character_,
           status2022 = NA_character_)
  
  # Original projects (v2022 to v2023 increased)
  capchanges.22t23.in.orig <- capchanges.22t23.in %>% 
    # Set capacity to value in v2022 (smaller value)
    mutate(capacity = capacity2022)

  # Combine data again
  capchanges.22t23.in <- bind_rows(capchanges.22t23.in.dummy,
                                   capchanges.22t23.in.orig)
  
  
  ## Combine all changed capacity data
  capchanges <- bind_rows(capchanges.21t22.out,
                          capchanges.21t22.in,
                          capchanges.22t23.out,
                          capchanges.22t23.in)
  
  ## Get references of projects, where capacities changed
  refs.changed <- capchanges %>% pull(reference)
  
  # Continue calculation in main dataset
  data.track.calc <- data.track.calc %>% 
    # Remove projects where capacities changed
    filter(!(reference %in% refs.changed)) %>% 
    # Calculate capacity
    mutate(capacity = 
             case_when(
               # Project has the same capacity in all: Use any
               capacity2021 == capacity2022 & capacity2022 == capacity2023 ~ capacity2021,
               # If delayed to year.tracking or new use incoming project size
               stat21t22 %in% c("New", "Delayed/in", "Early") ~ capacity2022,
               stat22t23 %in% c("New", "Delayed/in", "Early") ~ capacity2023,
               # If no information use previous project size
               stat21t22 %in% c("No info", "Delayed/out") ~ capacity2021,
               stat22t23 %in% c("No info", "Delayed/out") ~ capacity2022
               )
           ) %>% 
    # If no capacity is set now, remove projects (if we only look at additional projects)
    filter(!is.na(capacity))
  
  # Bind all together
  data.track.calc <- bind_rows(data.track.calc, capchanges) %>% 
    # Don't plot status bars for years != year.tracking
    mutate(status2021 =
             case_when(year2021 != year.tracking ~ NA_character_,
                       TRUE ~ status2021),
           status2022 = 
             case_when(year2022 != year.tracking ~ NA_character_,
                       TRUE ~ status2022),
           status2023 = 
             case_when(year2023 != year.tracking ~ NA_character_,
                       TRUE ~status2023)
    )
  
  # If year.tracking == 2022, remove uncertain projects in 2023
  # These come from DEMO projects that were assigned to FID/Construction
  if (year.tracking == 2022){
    data.track.calc <- data.track.calc %>%
      filter(!(status2023 %in% c("FID/Construction", "Feasibility study", "Concept")))
  }
  
  return(data.track.calc)
}