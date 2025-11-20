# -----------------------------------------------------------
# Script by: Josefien Tankink
# Contact: j.a.tankink@gmail.comm
# Goal: calculating the group composition over time using the
# updated presence files (see update presence files)
# -----------------------------------------------------------

# ---------- Library -------
{
  library(dplyr)
library(tidyr)
library(purrr)
}

# ----------- Data ---------
{
setwd("C:/Users/josef/Documents/PostDoc Lausanne")

out_dir <- "C:/Users/josef/Documents/PostDoc Lausanne/IVP DATA/Output/Composition"

## Create group composition ####
AKpresence <- read.csv("IVP DATA/Output/Presence/AK_presence_updated.csv")
BDpresence <- read.csv("IVP DATA/Output/Presence/BD_presence_updated.csv")
KBpresence <- read.csv("IVP DATA/Output/Presence/KB_presence_updated.csv")
LTpresence <- read.csv("IVP DATA/Output/Presence/LT_presence_updated.csv")
NHpresence <- read.csv("IVP DATA/Output/Presence/NH_presence_updated.csv")
IFpresence <- read.csv("IVP DATA/Output/Presence/IF_presence_updated.csv")

# Import data
LHdata <- read.csv("IVP DATA/life_history.csv", header = T, stringsAsFactors = F, na.strings = c('NA', 'NULL'))

# 1. Convert all date columns to Date objects
LHdata <- LHdata %>% 
  mutate(across(c(DOB, FirstRecorded, DepartureNatalGp,
                  DateImmigration1, LastSeen1,
                  DateImmigration2, LastSeen2,
                  DateImmigration3, LastSeen3,
                  DateImmigration4, LastSeen4),
                ~ as.Date(.))) %>% 
  filter(!is.na(Code)) 
}

# --------- Helpers --------
{calculate_age <- function(date_of_birth, first_recorded, target_date, birthgp) {
  base <- ifelse(is.na(date_of_birth),
                 as.numeric(difftime(target_date, first_recorded, units = "weeks")) / 52.143,
                 as.numeric(difftime(target_date, date_of_birth,           units = "weeks")) / 52.143)
  base + ifelse(is.na(date_of_birth) & is.na(birthgp), 5, 0)
}

get_age_class <- function(Sex, Age, departure_natal, target_date, present_gp, birth_gp) {
  case_when(
    is.na(Sex) | is.na(Age)                  ~ NA_character_,
    Age < 1                                  ~ "BB",
    Sex == "F" & Age >= 3                    ~ "AF",
    Sex == "F" & Age >= 1 & Age < 3          ~ "JF",
    Sex == "M" & Age >= 1 & Age < 4          ~ "JM",
    Sex == "M" & Age >= 4 &
      (present_gp == birth_gp) ~ "SM",
    Sex == "M" & Age >= 4 & Age < 5 &
      (is.na(departure_natal) | departure_natal <= target_date) ~ "AM",
    Sex == "M" & Age > 5                     ~ "AM",
    TRUE                                     ~ NA_character_
  )
}

group_code_map <- c(
  "Ankhase"     = "AK",
  "Baie Dankie" = "BD",
  "Noha"        = "NH",
  "Kubu"        = "KB",
  "Lemon Tree"  = "LT",
  "IFamily"     = "IF"
)

code_to_long <- function(code) {
  revmap <- setNames(names(group_code_map), unname(group_code_map))
  ifelse(code %in% names(revmap), revmap[code], code)
}

long_to_code <- function(long) {
  out <- unname(group_code_map[long]); ifelse(is.na(out), long, out)
}

process_group <- function(presence_df, LHdata, group_name) {
  presence_long <- presence_df %>%
    pivot_longer(-Date, names_to = "Code", values_to = "present") %>%
    filter(present == 1) %>%
    mutate(Date = as.Date(Date))
  
  joined_data <- presence_long %>%
    left_join(LHdata, by = "Code") %>%
    # Build a single 'departure_date' from the available "LastSeen*" columns (latest known)
    rowwise() %>%
    mutate(
      departure_date = suppressWarnings(
        max(c_across(c(LastSeen1, LastSeen2, LastSeen3, LastSeen4)), na.rm = TRUE)
      ),
      departure_date = ifelse(is.infinite(departure_date), as.Date(NA), as.Date(departure_date)),
      BirthGp = long_to_code(BirthGp)
    ) %>%
    ungroup() %>%
    # Use your calculate_age() on each row
    mutate(
      age = calculate_age(DOB, FirstRecorded, Date, BirthGp),
      # Use your get_age_class() (this one is already vectorized over inputs)
      age_class = get_age_class(Sex, age, DepartureNatalGp, Date, group_name, BirthGp)
    )
  
  summary_df <- joined_data %>%
    group_by(Date) %>%
    dplyr::summarise(
      AM = sum(age_class == "AM", na.rm = TRUE),
      AF = sum(age_class == "AF", na.rm = TRUE),
      SM = sum(age_class == "SM", na.rm = TRUE),
      JF = sum(age_class == "JF", na.rm = TRUE),
      JM = sum(age_class == "JM", na.rm = TRUE),
      BB = sum(age_class == "BB", na.rm = TRUE),
      ASR = AM/(AM+AF)
    ) %>%
    ungroup() %>%
    mutate(Group = group_name)
  
  return(summary_df)
}

# Assuming you have your presence data frames already loaded as AKpresence, BDpresence, etc.
presence_list <- list(
  AK = AKpresence,
  BD = BDpresence,
  KB = KBpresence,
  LT = LTpresence,
  NH = NHpresence,
  IF = IFpresence
)

ensure_presence_types <- function(pres) {
  stopifnot("Date" %in% names(pres))
  pres$Date <- as.Date(pres$Date)
  # Make sure all non-Date columns are integer-like 0/1 (coerce NAs to 0 to avoid warnings downstream)
  id_cols <- setdiff(names(pres), "Date")
  if (length(id_cols)) {
    for (v in id_cols) {
      pres[[v]] <- as.integer(replace(pres[[v]], is.na(pres[[v]]), 0))
    }
  }
  pres %>% arrange(Date)
}
}

# ------ Run and save function ------
{
  group_summaries <- map2(presence_list, names(presence_list), 
                        ~ process_group(.x, LHdata, .y))

# Save each summary as a separate CSV without group suffixes on the count columns
walk2(group_summaries, names(group_summaries), function(df, grp) {
  file_name <- paste0("IVP DATA/Output/Composition/", grp, "2010-2025.csv")
  write.csv(df, file_name, row.names = FALSE)
})
}

