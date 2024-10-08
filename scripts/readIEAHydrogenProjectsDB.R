readIEAHydrogenProjectsDB <- function(file.h2projects,
                                      h2projects.range,
                                      end.year,
                                      checked = FALSE) {
  
  # Column names
  columns <- c(
    "Column1" = "reference",
    "Column2" = "name",
    "Column3" = "country",
    "Column4" = "date.online",
    "Column5" = "date.decommissioned",
    "Column6" = "status",
    "Column7" = "technology",
    "Column8" = "tech.comments",
    "Column9" = "electricity.type",
    "Column10" = "elec.type.dedicated",
    "Column11" = "product",
    "Column12" = "enduse.refining",
    "Column13" = "enduse.ammonia",
    "Column14" = "enduse.methanol",
    "Column15" = "enduse.ironsteel",
    "Column16" = "enduse.otherind",
    "Column17" = "enduse.mobility",
    "Column18" = "enduse.power",
    "Column19" = "enduse.gridinj",
    "Column20" = "enduse.chp",
    "Column21" = "enduse.domesticheat",
    "Column22" = "enduse.biofuels",
    "Column23" = "enduse.synfuels",
    "Column24" = "enduse.ch4gridinj",
    "Column25" = "enduse.ch4mobility",
    "Column26" = "size.announced",
    "Column27" = "cap.mwel",
    "Column28" = "cap.nm3H2h",
    "Column29" = "cap.ktH2y",
    "Column30" = "cap.tCO2y",
    "Column31" = "cap.iea.estimate.nm3H2h",
    "Column32" = "source"
  )

  # Columns to select for project-level data
  columns.select.proj <- c(
    "reference",
    "name",
    "country",
    "date.online",
    "date.decommissioned",
    "status",
    "technology",
    "cap.mwel")
  
  # Columns to select for summary data
  columns.select.stat <- c(
    "reference",
    "name",
    "region",
    "status",
    "year",
    "capacity")

  if (checked == TRUE) {
    columns <- c(columns, c("Column33" = "checked"))
    
    columns.select.proj <- c(columns.select.proj, "checked")
    
    columns.select.stat <- c(columns.select.stat, "checked")
    
  }
  
  # Country
  region.mapping <- c(
    "ALB" = "Europe",  # Albania
    "AGO" = "Other",  # Angola
    "ARE" = "MENA",  # United Arab Emirates
    "ARG" = "C. + S. America",  # Argentina
    "AUS" = "Australia",
    "AUT" = "Europe",  # Austria
    "BEL" = "Europe",  # Belgium
    "BGD" = "Asia",  # Bangladesh
    "BRA" = "C. + S. America",  # Brazil
    "BRB" = "C. + S. America",  # Barbados
    "CAN" = "N. America",  # Canada
    "CHE" = "Europe",  # Switzerland
    "CHL" = "C. + S. America",  # Chile
    "CHN" = "Asia",  # China
    "COK" = "Other",  # Cook Islands
    "COL" = "C. + S. America",  # Colombia
    "CRI" = "C. + S. America",  # Costa Rica
    "CYP" = "Europe",  # Cyprus
    "CZE" = "Europe",  # Czech Republic
    "DEU" = "Europe",
    "DEU\r\nDNK" = "Europe",  # Germany + Denmark
    "DJI" = "MENA",  # Djibouti
    "DNK" = "Europe",
    "DMA" = "C. + S. America",  # Dominican Republic
    "EGY" = "MENA",  # Egypt
    "EST" = "Europe",  # Estonia
    "ESP" = "Europe",
    "ESP\r\nFRA" = "Europe",  # Project: HyDeal Ambition
    "EU" = "Europe",
    "FIN" = "Europe",  # Finland
    "FRA" = "Europe",
    "GBR" = "Europe",  # United Kingdom
    "GRC" = "Europe",  # Greece
    "GUF" = "Europe",  # French Guiana
    "HUN" = "Europe",  # Hungary
    "IDN" = "Asia",  # Indonesia
    "IND" = "Asia",  # India
    "IRL" = "Europe",  # Ireland
    "IRN" = "MENA",  # Iran
    "ISL" = "Europe",  # Iceland
    "ITA" = "Europe",  # Italy
    "JOR" = "MENA",  # Jordan
    "JPN" = "Asia",  # Japan
    "KAZ" = "Asia",  # Kazakhstan
    "KEN" = "Other",  # Kenya
    "KOR" = "Asia",  # South Korea
    "LBN" = "MENA",  # Lebanon
    "LTU" = "Europe",  # Lithuania
    "LVA" = "Europe",  # Latvia
    "MAR" = "MENA",  # Morocco
    "MEX" = "C. + S. America",  # Mexico
    "MNG" = "Asia",  # Mongolia
    "MNR" = "Europe",  # Montenegro
    "MOZ" = "Other",  # Mozambique
    "MRT" = "MENA",  # Mauritania
    "MYS" = "Asia",  # Malaysia
    "NAM" = "Other",  # Namibia
    "NER" = "Other",  # Niger
    "NLD" = "Europe",
    "NOR" = "Europe",  # Norway
    "NZL" = "Other",  # New Zealand
    "OMN" = "MENA",  # Oman
    "PAK" = "Other",  # Pakistan
    "PAN" = "C. + S. America",  # Panama
    "PER" = "Other",  # Peru
    "POL" = "Europe",  # Poland
    "PRT" = "Europe", # Portugal
    "POL\r\nCZE\r\nSVK\r\nHUN" = "Europe",
    "POR\r\nESP" = "Europe",
    "PRY" = "C. + S. America",  # Paraguay
    "ROM\r\nDEU\r\nAUT" = "Europe",
    "ROU" = "Europe",  # Romania
    "RUS" = "Asia",  # Russia
    "SAU" = "MENA",  # Saudi Arabia
    "SGP" = "Asia",  # Singapore
    "SVK" = "Europe",  # Slovakia
    "SVN" = "Europe",  # Slovenia
    "SWE" = "Europe",  # Sweden
    "THA" = "Asia",  # Thailand
    "TTO" = "Other",  # Trinidad and Tobago
    "TUR" = "MENA",  # Turkey
    "TWN" = "Asia",  # Taiwan
    "UKR" = "Europe",  # Ukraine
    "URY" = "C. + S. America",  # Uruguay
    "USA" = "N. America",  # USA
    "UZB" = "Other",  # Uzbekistan
    "VNM" = "Asia",  # Vietnam
    "ZAF" = "Other",  # South Africa
    "ZWE" = "Other",  # Zimbabwe
    NULL
  )
  
  # Read IEA file
  data.h2.projects <-
    read_excel(
      file.h2projects,
      sheet = "Projects",
      range = h2projects.range,
      col_names = columns
    ) %>%
    # Select columns
    select(all_of(columns.select.proj)) %>%
    # Map regions
    revalue.levels(country = region.mapping) %>%
    rename(region = country) %>%
    # Correct misspelling
    revalue.levels(status = c("Decommisioned" = "Decommissioned")) %>%
    # Drop if no capacity in MW given
    filter(!is.na(cap.mwel)) %>%
    # Drop if capacity is zero
    filter(cap.mwel != 0) %>% 
    # Drop if technology is NG w CCUS
    filter(technology != "NG w CCUS")
  
  # Get confidential projects
  data.h2.projects.conf <- data.h2.projects %>% 
    filter(name %in% c("Other projects from confidential sources (2000-2020)",
                       "Other projects from confidential sources (2000-2021)",
                       "Other projects from confidential sources (2000-2023)"))
  
  # Print volume of projects without online date
  temp <- data.h2.projects %>% 
    filter(is.na(date.online),
           !is.na(region)) %>%
    group_by(region) %>% 
    summarise(ommitted_capacity_GW = sum(cap.mwel)/1E3)
  print("Omitted projects without a specified starting date (GW):")
  print(temp)
  
  # Continue data processing of non-confidential projects
  data.h2.projects <- data.h2.projects %>%
    # Drop if no online date given
    filter(!is.na(date.online),
           date.online > 0) %>%
    # Map all NA regions to Other
    mutate(region = case_when(is.na(region) ~ "Other",
                              TRUE ~ region)) %>%
    # Map DEMO to other statuses
    mutate(status = case_when(
      # If already decommissioned -> Decommissioned
      status == "DEMO" & date.decommissioned <= end.year ~ "Decommissioned",
      # If announced date in the past and not yet decommissioned -> Operational
      status == "DEMO" & (date.decommissioned > end.year | is.na(date.decommissioned)) & date.online <= end.year ~ "Operational",
      # If announced date in the future -> FID/Construction
      status == "DEMO" & date.online > end.year ~ "FID/Construction",
      # Otherwise keep status unchanged
      TRUE ~ status)) %>%
    # Create two rows for each project, one for online, one for decommissioned
    pivot_longer(
      cols = c("date.online", "date.decommissioned"),
      names_to = "type",
      names_prefix = "date.",
      values_to = "year"
    ) %>%
    # Filter rows that do not have a decommissioned date
    filter(!(type == "decommissioned" & is.na(year))) %>%
    # If status == Decommissioned and type == online -> Operational
    mutate(status = case_when((status == "Decommissioned" &
                                 type == "online") ~ "Operational",
                              type == "decommissioned" ~ "Decommissioned",
                              TRUE ~ status
    )) %>%
    # Combine status in IEA v2021 and IEA v2022
    mutate(status = case_when(status %in% c("Under construction", "FID") ~ "FID/Construction",
                              TRUE ~ status)) %>% 
    filter(!(status %in% c("Other", "Other/Unknown"))) %>% 
    # Negative capacity for decommissioned
    mutate(cap.mwel = case_when(status == "Decommissioned" ~ -cap.mwel,
                                TRUE ~ cap.mwel)) %>%
    filter(!is.na(status)) %>% 
    # Select and reorder
    rename(capacity = cap.mwel) %>% 
    select(columns.select.stat)
  
  # Calculate capacity shares of each region to distribute confidential projects
  data.h2.shares <- data.h2.projects %>% 
    filter(year %in% seq(2000, end.year),
           # Exclude very large alkaline project (for share calculation only!)
           name != "KIMA - Aswan electrolyser",
           status == "Operational") %>% 
    complete(region,
             year = 2000:end.year,
             fill = list(capacity = 0)) %>% 
    group_by(region, year) %>% 
    # Calculate sum of projects
    summarise(cap.sum = sum(capacity)) %>% 
    arrange(year) %>% 
    # Calculate cumulative sum over time
    mutate(cap.sum = cumsum(cap.sum)) %>% 
    group_by(year) %>% 
    # Calculate share of each country for each year
    mutate(share = cap.sum/sum(cap.sum))
  
  # Confidential ALK projects
  if (nrow(data.h2.projects.conf) > 0) {
    data.h2.projects.conf.alk <- data.h2.shares %>%
      mutate(
        name = paste0("Other projects from confidential sources (", year, ")"),
        technology = "ALK",
        status = "Operational",
        # Distribute over 21 years from 2000 to 2020
        capacity = 1/(end.year - 2000 + 1) * share * data.h2.projects.conf %>%
          filter(technology == "ALK") %>%
          pull(cap.mwel),
        reference = 0,
        checked = NA_character_
      ) %>%
      select(columns.select.stat)
    
    # Confidential PEM projects
    data.h2.projects.conf.pem <- data.h2.shares %>%
      mutate(
        name = paste0("Other projects from confidential sources (", year, ")"),
        technology = "PEM",
        status = "Operational",
        # Distribute over 21 years from 2000 to 2020
        capacity = 1/(end.year - 2000 + 1) * share * data.h2.projects.conf %>%
          filter(technology == "PEM") %>%
          pull(cap.mwel),
        reference = 0,
        checked = NA_character_
      ) %>%
      select(columns.select.stat)
    
    # Combine data
    data.h2.projects <- bind_rows(data.h2.projects, data.h2.projects.conf.alk)
    data.h2.projects <- bind_rows(data.h2.projects, data.h2.projects.conf.pem)
  }
  
  if (checked == TRUE) {
    data.h2.projects <- data.h2.projects %>% 
      mutate(checked = ifelse(!is.na(checked), TRUE, FALSE))
  }
  
  # Calculate relevant statistics of project database
  data.h2 <- data.h2.projects %>%
    # Transform to GW
    mutate(capacity = capacity / 1E3) %>%
    # Capacity, mean and count
    group_by(region, status, year) %>%
    summarise(
      cap.sum = sum(capacity),
      cap.mean = mean(capacity),
      nprojects = n()
    ) %>%
    ungroup() %>%
    # Complete dataset
    complete(region,
             status,
             year = 2000:2045,
             fill = list(
               cap.sum = 0,
               cap.mean = 0,
               nprojects = 0
             )) %>%
    # Cumulative capacity (over years)
    group_by(region, status) %>%
    arrange(year) %>%
    mutate(cumcap.sum = cumsum(cap.sum)) %>%
    ungroup() %>% 
    order.levels(
      status = c(
        "Concept",
        "Feasibility study",
        "FID/Construction",
        "Operational",
        "Decommissioned"
      )
    )
  
  return(list(data.h2.projects, data.h2))
}
