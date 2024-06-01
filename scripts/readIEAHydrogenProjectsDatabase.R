readIEAHydrogenProjectsDB <- function(file.h2projects,
                                      h2projects.range,
                                      end.year,
                                      decommissioned = TRUE) {
  
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
  
  # Country
  region.mapping <- c(
    "ALB" = "Other",  # Albania
    "AGO" = "Other",  # Angola
    "ARE" = "MENA",  # United Arab Emirates
    "ARG" = "C. + S. America",  # Argentina
    "AUS" = "Australia",
    "AUT" = "Rest EU",  # Austria
    "BEL" = "Rest EU",  # Belgium
    "BGD" = "Asia",  # Bangladesh
    "BRA" = "C. + S. America",  # Brazil
    "BRB" = "C. + S. America",  # Barbados
    "CAN" = "N. America",  # Canada
    "CHE" = "Other",  # Switzerland
    "CHL" = "C. + S. America",  # Chile
    "CHN" = "Asia",  # China
    "COK" = "Other",  # Cook Islands
    "COL" = "C. + S. America",  # Colombia
    "CRI" = "C. + S. America",  # Costa Rica
    "CYP" = "Rest EU",  # Cyprus
    "CZE" = "Rest EU",  # Czech Republic
    "DEU" = "Germany",
    "DEU\r\nDNK" = "Rest EU",  # Germany + Denmark
    "DJI" = "MENA",  # Djibouti
    "DNK" = "Denmark",
    "DMA" = "C. + S. America",  # Dominican Republic
    "EGY" = "MENA",  # Egypt
    "EST" = "Rest EU",  # Estonia
    "ESP" = "Spain",
    "ESP\r\nFRA" = "Spain",  # Project: HyDeal Ambition
    "EU" = "Rest EU",
    "FIN" = "Rest EU",  # Finland
    "FRA" = "France",
    "GBR" = "Other",  # United Kingdom
    "GRC" = "Rest EU",  # Greece
    "GUF" = "France",  # French Guiana
    "HUN" = "Rest EU",  # Hungary
    "IDN" = "Asia",  # Indonesia
    "IND" = "Asia",  # India
    "IRL" = "Rest EU",  # Ireland
    "IRN" = "MENA",  # Iran
    "ISL" = "Other",  # Iceland
    "ITA" = "Rest EU",  # Italy
    "JOR" = "MENA",  # Jordan
    "JPN" = "Asia",  # Japan
    "KAZ" = "Asia",  # Kazakhstan
    "KEN" = "Other",  # Kenya
    "KOR" = "Asia",  # South Korea
    "LBN" = "MENA",  # Lebanon
    "LTU" = "Rest EU",  # Lithuania
    "LVA" = "Rest EU",  # Latvia
    "MAR" = "MENA",  # Morocco
    "MEX" = "C. + S. America",  # Mexico
    "MNG" = "Asia",  # Mongolia
    "MNR" = "Other",  # Montenegro
    "MOZ" = "Other",  # Mozambique
    "MRT" = "MENA",  # Mauritania
    "MYS" = "Asia",  # Malaysia
    "NAM" = "Other",  # Namibia
    "NER" = "Other",  # Niger
    "NLD" = "Netherlands",
    "NOR" = "Other",  # Norway
    "NZL" = "Other",  # New Zealand
    "OMN" = "MENA",  # Oman
    "PAK" = "Other",  # Pakistan
    "PAN" = "C. + S. America",  # Panama
    "PER" = "Other",  # Peru
    "POL" = "Rest EU",  # Poland
    "PRT" = "Rest EU", # Portugal
    "POL\r\nCZE\r\nSVK\r\nHUN" = "Rest EU",
    "POR\r\nESP" = "Rest EU",
    "PRY" = "C. + S. America",  # Paraguay
    "ROM\r\nDEU\r\nAUT" = "Rest EU",
    "ROU" = "Rest EU",  # Romania
    "RUS" = "Other",  # Russia
    "SAU" = "MENA",  # Saudi Arabia
    "SGP" = "Asia",  # Singapore
    "SVK" = "Rest EU",  # Slovakia
    "SVN" = "Rest EU",  # Slovenia
    "SWE" = "Rest EU",  # Sweden
    "THA" = "Asia",  # Thailand
    "TTO" = "Other",  # Trinidad and Tobago
    "TUR" = "MENA",  # Turkey
    "TWN" = "Asia",  # Taiwan
    "UKR" = "Other",  # Ukraine
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
    select(c(
      reference,
      name,
      country,
      date.online,
      date.decommissioned,
      status,
      technology,
      cap.mwel
    )) %>%
    # Map regions
    revalue.levels(country = region.mapping) %>%
    rename(region = country) %>%
    # Correct misspelling
    revalue.levels(status = c("Decommisioned" = "Decommissioned")) %>%
    # Drop if no capacity in MW given
    filter(!is.na(cap.mwel)) %>% 
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
    # If status == DEMO and there is no decommissioned date -> Operational
    # If status == DEMO and there is a decommissioned date -> Decommissioned
    mutate(status = case_when((status == "DEMO" &
                                 is.na(date.decommissioned)) ~ "Operational",
                              (status == "DEMO" &
                                 !is.na(date.decommissioned)) ~ "Decommissioned",
                              TRUE ~ status
    )) %>%
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
    select(c(reference, name, region, status, year, cap.mwel)) %>%
    rename(capacity = cap.mwel)
  
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
  data.h2.projects.conf.alk <- data.h2.shares %>%
    mutate(
      name = paste0("Other projects from confidential sources (", year, ")"),
      technology = "ALK",
      status = "Operational",
      # Distribute over 21 years from 2000 to 2020
      capacity = 1/(end.year - 2000 + 1) * share * data.h2.projects.conf %>%
        filter(technology == "ALK") %>%
        pull(cap.mwel),
      reference = 0
    ) %>%
    select(reference, name, region, status, year, capacity)
  
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
      reference = 0
    ) %>%
    select(reference, name, region, status, year, capacity)
  
  # Combine data
  data.h2.projects <- bind_rows(data.h2.projects, data.h2.projects.conf.alk)
  data.h2.projects <- bind_rows(data.h2.projects, data.h2.projects.conf.pem)
  
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
