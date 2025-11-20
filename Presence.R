# -----------------------------------------------------------
# Script by: Josefien Tankink
# Contact: j.a.tankink@gmail.comm
# Goal: creating presence matrices based on life history
# -----------------------------------------------------------

# ----------- Library -----------
{
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(data.table)  # For setDT() if you prefer dcast (optional)
  library(stringr)
}

# ----------- Data ----------
{# path:
setwd("yourworkingdirectory")

# Import data
LHdata <- read.csv("life_history.csv", header = T, stringsAsFactors = F, na.strings = c('NA', 'NULL'))

# 1. Convert all date columns to Date objects
LHdata <- LHdata %>% 
  mutate(across(c(DOB, FirstRecorded, DepartureNatalGp,
                  DateImmigration1, LastSeen1,
                  DateImmigration2, LastSeen2,
                  DateImmigration3, LastSeen3,
                  DateImmigration4, LastSeen4),
                ~ as.Date(.)))
}

# --------- Function --------
{build_presence_matrix <- function(data, group, start_date = "2010-06-24", end_date = Sys.Date()) {
  
  # 2a. Build birth interval (if the individual’s BirthGp equals the group)
  birth_interval <- data %>% 
    filter(BirthGp == group) %>% 
    mutate(start = coalesce(DOB, FirstRecorded),
           end   = coalesce(DepartureNatalGp, LastSeen1)) %>% 
    select(Code, start, end)
  
  # 2b. Build immigration intervals by reshaping the immigration columns to long format
  immigration_intervals <- data %>% 
    pivot_longer(
      cols = starts_with("ImmigrationGp"),
      names_to = "imm_event",
      values_to = "group_val"
    ) %>% 
    filter(group_val == group) %>% 
    mutate(event_num = str_extract(imm_event, "\\d+"),
           # Build the corresponding date column names
           date_col     = paste0("DateImmigration", event_num),
           lastseen_col = paste0("LastSeen", event_num)) %>% 
    rowwise() %>% 
    mutate(start = get(date_col),
           end   = get(lastseen_col)) %>% 
    ungroup() %>% 
    select(Code, start, end)
  
  # 2c. Combine both types of intervals. For individuals still present, replace missing end with end_date.
  intervals <- bind_rows(birth_interval, immigration_intervals) %>% 
    mutate(end = if_else(is.na(end), as.Date(end_date), end))
  
  # 3. Create a data frame with all dates in the study period
  dfDate <- tibble(Date = seq(as.Date(start_date), as.Date(end_date), by = "days"))
  
  # 4. Cross join the dates with the intervals, keeping only dates that fall within an interval
  presence_long <- dfDate %>% 
    crossing(intervals) %>% 
    filter(Date >= start, Date <= end) %>% 
    distinct(Date, Code)
  
  # 5. Pivot to create a presence matrix (1 when present, 0 when absent)
  presence_matrix <- presence_long %>% 
    mutate(present = 1) %>% 
    pivot_wider(names_from = Code, values_from = present, values_fill = list(present = 0))
  
  return(presence_matrix)
}

# Define the groups of interest and create (and optionally export) a presence matrix for each
groups <- c("AK", "BD", "KB", "LT", "NH", "IF")
presence_matrices <- list()
}

# ----------- Save your csv's ------------
{for(gr in groups) {
  cat("Processing group", gr, "\n")
  presence_matrices[[gr]] <- build_presence_matrix(LHdata, gr)
  
  # Write the presence matrix to a CSV file (adjust file paths as needed)
  output_file <- paste0("Output/Presence/", gr, "2010-2025.csv")
  write.csv(presence_matrices[[gr]], file = output_file, row.names = FALSE)
}
}
# Now each element of presence_matrices (e.g., presence_matrices[["AK"]]) is the daily presence matrix for that group.
