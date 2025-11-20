# -----------------------------------------------------------
# Script by: Josefien Tankink
# Contact: j.a.tankink@gmail.comm
# Goal: updating the presence files so they give presence
# whenever an individual was recorded in an interaction
# -----------------------------------------------------------

# ---------- Library -------
{
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
}

# ----------- Data ---------
{
  setwd("C:/Users/josef/Documents/PostDoc Lausanne")
  
  out_dir <- "C:/Users/josef/Documents/PostDoc Lausanne/IVP DATA/Output/Presence"
  
  ## Create group composition ####
  AKpresence <- read.csv("IVP DATA/Output/Presence/AK2010-2025.csv")
  BDpresence <- read.csv("IVP DATA/Output/Presence/BD2010-2025.csv")
  KBpresence <- read.csv("IVP DATA/Output/Presence/KB2010-2025.csv")
  LTpresence <- read.csv("IVP DATA/Output/Presence/LT2010-2025.csv")
  NHpresence <- read.csv("IVP DATA/Output/Presence/NH2010-2025.csv")
  IFpresence <- read.csv("IVP DATA/Output/Presence/IF2010-2025.csv")
  
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

# --------- Update presence files so individuals are marked present when they have an interaction -----
# --------- This is done with agonistic data but can be done with any ad lib
{wl_adults <- read.csv(
  "IVP DATA/Output/WinnerLoser_Adults.csv",
  stringsAsFactors = FALSE
) %>%
  mutate(
    Date      = as.Date(Date),
    GroupLong = as.character(Group),
    Group     = long_to_code(GroupLong) # maps long names to AK/BD/KB/NH/LT/IF; leaves codes as-is
  ) %>%
  # keep the window you configured and drop any incomplete rows (should already be clean)
  filter(
    !is.na(Date),
    !is.na(winner),
    !is.na(loser),
    # Date >= START_DATE, Date <= END_DATE,  # keep commented as in original
    winner != loser
  ) %>%
  # the Elo functions only need these
  dplyr::select(Date, Time, Group, winner, loser) %>%
  distinct()

wl_by_group <- split(wl_adults, wl_adults$Group)
}

# ---------- Main update per group --------
{update_presence_for_group <- function(group_code, pres, interactions) {
  if (is.null(pres) || !nrow(pres)) {
    message(sprintf("[WARN] Presence table empty or NULL for group %s; creating from interactions.", group_code))
    pres <- tibble(Date = sort(unique(interactions$Date)))
  }
  
  # --- types / order ---
  stopifnot("Date" %in% names(pres))
  pres$Date <- as.Date(pres$Date)
  id_cols <- setdiff(names(pres), "Date")
  if (length(id_cols)) {
    for (v in id_cols) pres[[v]] <- as.integer(replace(pres[[v]], is.na(pres[[v]]), 0L))
  }
  pres <- pres %>% arrange(Date)
  
  # --- all IDs seen in interactions ---
  interactions <- interactions %>%
    mutate(
      winner = trimws(as.character(winner)),
      loser  = trimws(as.character(loser))
    ) %>%
    arrange(Date)
  
  ids <- sort(unique(c(interactions$winner, interactions$loser)))
  if (!length(ids)) return(pres)
  
  # 1) add any missing ID columns (init to 0)
  missing_cols <- setdiff(ids, names(pres))
  if (length(missing_cols)) {
    message(sprintf("[INFO] %s: adding %d missing ID columns: %s",
                    group_code, length(missing_cols), paste(missing_cols, collapse = ", ")))
    for (v in missing_cols) pres[[v]] <- 0L
  }
  
  # refresh id_cols after additions
  id_cols <- setdiff(names(pres), "Date")
  
  # 2) add any missing dates
  needed_dates <- sort(unique(interactions$Date))
  add_dates <- setdiff(needed_dates, pres$Date)
  if (length(add_dates)) {
    message(sprintf("[INFO] %s: adding %d missing dates to presence.", group_code, length(add_dates)))
    zeros <- as_tibble(matrix(0L, nrow = length(add_dates), ncol = length(id_cols),
                              dimnames = list(NULL, id_cols)))
    add_df <- bind_cols(tibble(Date = as.Date(add_dates)), zeros)
    pres <- bind_rows(pres, add_df) %>% arrange(Date)
  }
  
  # 3) set presence == 1 for all participants on their interaction dates
  #    (by-date assignment that works on data.frames)
  by_d <- interactions %>%
    transmute(Date, ID = winner) %>%
    bind_rows(interactions %>% transmute(Date, ID = loser)) %>%
    distinct() %>%
    group_by(Date) %>%
    summarise(IDs = list(ID), .groups = "drop")
  
  # make sure the listed IDs exist as columns (they should; warn if not)
  all_cols <- names(pres)
  for (k in seq_len(nrow(by_d))) {
    d  <- by_d$Date[k]
    vs <- intersect(by_d$IDs[[k]], all_cols)          # keep only columns that exist
    missing <- setdiff(by_d$IDs[[k]], all_cols)
    if (length(missing)) {
      warning(sprintf("[%s] Presence columns unexpectedly missing for IDs on %s: %s",
                      group_code, as.character(d), paste(missing, collapse = ", ")))
      next
    }
    row_idx <- pres$Date == d
    col_idx <- vs
    # assign in one shot; works fine for data.frames/tibbles
    pres[row_idx, col_idx] <- 1L
  }
  
  # ensure integers again
  for (v in setdiff(names(pres), "Date")) {
    pres[[v]] <- as.integer(replace(pres[[v]], is.na(pres[[v]]), 0L))
  }
  
  pres %>% arrange(Date)
}}

# --------- Run and save the update ----------
{# Work off your existing presence_list (AK/BD/KB/NH/IF)
stopifnot(is.list(presence_list))
groups_to_process <- intersect(names(presence_list), names(wl_by_group))
if (!length(groups_to_process)) stop("No overlapping groups between presence_list and WL data.")

updated_presence <- presence_list

for (g in groups_to_process) {
  interactions <- wl_by_group[[g]] %>% arrange(Date)
  if (is.null(interactions) || !nrow(interactions)) {
    message(sprintf("[INFO] %s: no interactions found; skipping.", g))
    next
  }
  message(sprintf("[RUN] Updating presence for group %s with %d interactions.", g, nrow(interactions)))
  updated_presence[[g]] <- update_presence_for_group(g, presence_list[[g]], interactions)
  
  # Write an updated CSV per group (same naming pattern as your inputs, but into a new folder)
  out_path <- file.path(out_dir, paste0(g, "_presence_updated.csv"))
  # Keep Date first, others after, in original+new column order:
  out_df <- updated_presence[[g]] %>% arrange(Date)
  write_csv(out_df, out_path, na = "")
  message(sprintf("[DONE] Wrote %s (%d rows, %d cols).",
                  out_path, nrow(out_df), ncol(out_df)))
}
}

