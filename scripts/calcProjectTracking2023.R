#' Calculate project tracking
#' 
#' @param data.v2021.proj Dataset of project announcements in 2021
#' @param data.v2022.proj Dataset of project announcements in 2022
#' @param data.v2023.proj Dataset of project announcements in 2023
#' @param data.v2023outcome.proj Dataset of project outcome for 2023
#' @returns Wide dataset with five additional columns for a Sankey plot
#' 
calcProjectTracking2023 <- function(
    data.v2021.proj,
    data.v2022.proj,
    data.v2023.proj,
    data.v2023outcome.proj) {
  
  # Helper function used in ifelse statements below
  isEq <- function(var, values){
    !is.na(var) & var %in% values
  }
  
  year.tracking <- 2023
  
  ### Added capacities
  ### Only extract added capacities in year.tracking, ignoring cumulative
  ### project announcements and already operational projects

  # Data handling for IEA v2021
  data.v2021.proj.renamed <- data.v2021.proj %>%
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
      capacity2021 = capacity,
      checked2021 = checked
    )

  # Data handling for IEA v2022
  data.v2022.proj.renamed <- data.v2022.proj %>%
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
      capacity2022 = capacity,
      checked2022 = checked
    )
  
  # Data handling for IEA v2023
  data.v2023.proj.renamed <- data.v2023.proj %>%
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
      capacity2023 = capacity,
      checked2023 = checked
    )
  
  # Data handling for IEA v2023 project outcome for 2023
  data.v2023outcome.proj.renamed <- data.v2023outcome.proj %>% 
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
      name2023out = name,
      region2023out = region,
      status2023out = status,
      year2023out = year,
      capacity2023out = capacity,
      checked2023out = checked
    )
    
  # Join all datasets
  data.list <- list(data.v2021.proj.renamed,
                    data.v2022.proj.renamed,
                    data.v2023.proj.renamed,
                    data.v2023outcome.proj.renamed)
  
  data.track <- data.list %>% 
    reduce(full_join, by = "reference")
  
  # Check that for projects announced for 2023, if a project was validated in
  # one database, it is validated also in all other databases
  if (data.track %>%
      filter(year2021 == year.tracking | is.na(year2021),
             year2022 == year.tracking | is.na(year2022),
             year2023 == year.tracking | is.na(year2023),
             any(checked2021 != checked2022, checked2021 != checked2023, checked2022 != checked2023)) %>%
      nrow() > 0) {
    warning("Some projects that were validated in one database snapshot, were not validated in another.")
  }
  
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
           name2023, status2023, year2023, capacity2023,
           name2023out, status2023out, year2023out, capacity2023out) %>% 
    # Filter projects that have been announced for year.tracking in any dataset
    rowwise() %>% 
    filter(any(c(year2021, year2022, year2023, year2023out) == year.tracking)) %>% 
    ungroup() %>% 
    # Special treatment for v2023 outcome database: Set year2023out to year2023
    # if it is not available, as we only copied those projects with year2023==2023
    # into the separate Excel file for manual validation
    mutate(year2023out = ifelse(is.na(year2023out), year2023, year2023out)) %>% 
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
      ),
      year2023out = ifelse(
        status2023out == "Operational" & year2023out < year.tracking,
        year.tracking,
        year2023out
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
    ) %>% 
    # Add column for changes between v2023 and v2023 outcome
    mutate(
      stat23t23out = 
        case_when(
          # New projects
          is.na(year2023) & year2023out == year.tracking ~ "New",
          # Project delayed to year.tracking (from earlier years)
          year2023 < year2023out & year2023out == year.tracking ~ "Delayed/in",
          # Project brought forward to year.tracking (from later years)
          year2023 > year2023out & year2023out == year.tracking ~ "Early",
          # Project delayed (after year.tracking)
          year2023 < year2023out & year2023 == year.tracking ~ "Delayed/out",
          # Project disappeared
          !is.na(year2023) & is.na(year2023out) ~ "No info",
          year2023 == year.tracking & is.na(status2023out) ~ "No info"
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
  
  ### Calculate dummy and adjust orig if capacity decreased from v2023 to v2023out
  capchanges.23t23out.out <- data.track.calc %>% 
    filter(capacity2023 > capacity2023out,
           year2023 == year.tracking,
           year2023out == year.tracking)
  
  # Dummy projects (v2023 to v2023out decreased)
  capchanges.23t23out.out.dummy <- capchanges.23t23out.out %>% 
    # Set capacity to difference between v2023 and v2023out
    mutate(capacity = capacity2023 - capacity2023out) %>% 
    # To stat22t23 add stratum "Cap/out" for outgoing (decreasing) capacity
    mutate(stat23t23out = "Cap/out") %>% 
    # Assign arbitrarily large reference numbers to avoid interference
    mutate(reference = reference + 50000) %>% 
    # Remove sankey flows after stat23t23out
    mutate(status2023out = NA_character_)
  
  # Original projects (v2023 to v2023out decreased)
  capchanges.23t23out.out.orig <- capchanges.23t23out.out %>% 
    # Set capacity to value in v2023 (smaller value)
    mutate(capacity = capacity2023out)
  
  # Combine data again
  capchanges.23t23out.out <- bind_rows(capchanges.23t23out.out.dummy,
                                       capchanges.23t23out.out.orig)
  
  ### Calculate dummy and adjust orig if capacity increased from v2023 to v2023out
  capchanges.23t23out.in <- data.track.calc %>% 
    filter(capacity2023 < capacity2023out,
           year2022 == year.tracking,
           year2023 == year.tracking)
  
  # Dummy projects (v2023 to v2023out increased)
  capchanges.23t23out.in.dummy <- capchanges.23t23out.in %>% 
    # Set capacity to difference between v2023out and v2023
    mutate(capacity = capacity2023out - capacity2023) %>% 
    # To stat22t23 add stratum "Cap/in" for incoming (increasing) capacity
    mutate(stat23t23out = "Cap/in") %>% 
    # Assign arbitrarily large reference numbers to avoid interference
    mutate(reference = reference + 60000) %>% 
    # Remove sankey flows before stat23t23out
    mutate(status2021 = NA_character_,
           stat21t22 = NA_character_,
           status2022 = NA_character_,
           stat22t23 = NA_character_,
           status2023 = NA_character_)
  
  # Original projects (v2023 to v2023out increased)
  capchanges.23t23out.in.orig <- capchanges.23t23out.in %>% 
    # Set capacity to value in v2022 (smaller value)
    mutate(capacity = capacity2023)
  
  # Combine data again
  capchanges.23t23out.in <- bind_rows(capchanges.23t23out.in.dummy,
                                      capchanges.23t23out.in.orig)
  
  ## Combine all changed capacity data
  capchanges <- bind_rows(capchanges.21t22.out,
                          capchanges.21t22.in,
                          capchanges.22t23.out,
                          capchanges.22t23.in,
                          capchanges.23t23out.out,
                          capchanges.23t23out.in)
  
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
               stat23t23out %in% c("New", "Delayed/in", "Early") ~ capacity2023out,
               # If no information use previous project size
               stat21t22 %in% c("No info", "Delayed/out") ~ capacity2021,
               stat22t23 %in% c("No info", "Delayed/out") ~ capacity2022,
               stat23t23out %in% c("No info", "Delayed/out") ~ capacity2023
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
                       TRUE ~ status2023),
           status2023out = 
             case_when(year2023out != year.tracking ~ NA_character_,
                       TRUE ~ status2023out)
    )
  
  # If year.tracking == 2022, remove uncertain projects in 2023
  # These come from DEMO projects that were assigned to FID/Construction
  if (year.tracking == 2022){
    data.track.calc <- data.track.calc %>%
      filter(!(status2023 %in% c("FID/Construction", "Feasibility study", "Concept")))
  }
  
  return(data.track.calc)
}
