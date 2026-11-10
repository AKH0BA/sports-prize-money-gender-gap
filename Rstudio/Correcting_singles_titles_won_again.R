# Correct only singles_titles_won.
#
# All other variables remain unchanged from:
#   Tennis_Top50_2019_2025_Season_Match_Stats.csv
#
# Season assignment uses the source-file year.
# Walkovers are excluded from matches and wins, but an awarded individual
# singles title through a final walkover is included.
# Retirements count as played matches, but a retirement cannot be a final win.
# Team, exhibition, and Olympic events are excluded from individual titles.

library(dplyr)
library(readr)
library(stringr)

years <- 2019:2025
match_dir <- "sackmann_match_data"
input_file <- "Tennis_Top50_2019_2025_Season_Match_Stats.csv"
output_file <- "Tennis_Top50_2019_2025_Season_Match_Stats_Titles_Final.csv"

if (exists("final_season_data")) {
  input_data <- final_season_data
} else if (file.exists(input_file)) {
  input_data <- read_csv(input_file, show_col_types = FALSE)
} else {
  stop("Load final_season_data or place the final season CSV in the working directory.")
}

input_data <- input_data |>
  mutate(
    player_id = as.character(player_id),
    year = as.integer(year)
  )

if (nrow(input_data) != 700L) {
  stop("The input dataset must contain exactly 700 rows.")
}

required_columns <- c(
  "tourney_id", "tourney_name", "tourney_level", "tourney_date",
  "winner_id", "winner_name", "loser_id", "loser_name",
  "winner_entry", "loser_entry", "score", "round"
)

read_match_file <- function(tour, season_year) {
  path <- file.path(
    match_dir,
    paste0(tolower(tour), "_matches_", season_year, ".csv")
  )
  
  if (!file.exists(path)) {
    stop("Missing match file: ", path)
  }
  
  matches <- read_csv(
    path,
    show_col_types = FALSE,
    col_types = cols(.default = col_character())
  )
  
  missing_columns <- setdiff(required_columns, names(matches))
  if (length(missing_columns) > 0L) {
    stop(
      path, " is missing: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  matches |>
    mutate(
      tour = tour,
      season_year = as.integer(season_year),
      winner_id = as.character(winner_id),
      loser_id = as.character(loser_id),
      score_upper = str_to_upper(coalesce(score, "")),
      event_text = str_c(tourney_name, tourney_id, sep = " "),
      is_walkover = score_upper == "" |
        str_detect(score_upper, "W/O|WALKOVER|WALK OVER|DEF"),
      competition_class = case_when(
        str_detect(
          event_text,
          regex("olympic|olympics", ignore_case = TRUE)
        ) ~ "Olympics",
        str_detect(
          event_text,
          regex("laver cup", ignore_case = TRUE)
        ) ~ "Laver Cup",
        str_detect(
          event_text,
          regex("united cup", ignore_case = TRUE)
        ) ~ "United Cup",
        str_detect(
          event_text,
          regex("atp cup", ignore_case = TRUE)
        ) ~ "ATP Cup",
        str_detect(
          event_text,
          regex(
            "davis cup|billie jean king cup|fed cup|federation cup",
            ignore_case = TRUE
          )
        ) ~ "Davis/Billie Jean King Cup",
        tourney_level == "E" |
          str_detect(
            event_text,
            regex(
              "exhibition|world team tennis|battle of the sexes",
              ignore_case = TRUE
            )
          ) ~ "Exhibition",
        tourney_level %in% c("D", "T") ~ "Other team competition",
        tourney_level == "G" ~ "Grand Slam",
        tourney_level == "F" ~ "Tour Finals",
        TRUE ~ "Regular tour"
      ),
      is_individual_singles_event = competition_class %in% c(
        "Regular tour",
        "Grand Slam",
        "Tour Finals"
      ),
      is_title_final = str_to_upper(round) == "F",
      is_counted_title =
        is_title_final &
        is_individual_singles_event
    )
}

match_data <- bind_rows(
  lapply(years, \(x) read_match_file("ATP", x)),
  lapply(years, \(x) read_match_file("WTA", x))
)

# Count individual singles finals won by each player and season.
# A final walkover is included because it represents an awarded title.
corrected_titles <- match_data |>
  filter(is_counted_title) |>
  transmute(
    tour,
    year = season_year,
    player_id = winner_id,
    winning_player_name = winner_name,
    tourney_id,
    tourney_name,
    competition_class
  ) |>
  distinct(tour, year, player_id, tourney_id, .keep_all = TRUE) |>
  group_by(tour, year, player_id) |>
  summarise(
    corrected_singles_titles_won = n(),
    .groups = "drop"
  )

# Keep a detailed audit trail of every final, including final walkovers.
title_audit <- match_data |>
  filter(is_title_final) |>
  transmute(
    tour,
    year = season_year,
    tourney_id,
    tourney_name,
    competition_class,
    round,
    winner_id,
    winner_name,
    loser_id,
    loser_name,
    score,
    is_walkover,
    is_individual_singles_event,
    is_counted_title
  ) |>
  arrange(tour, year, tourney_name)

write_csv(title_audit, "Singles_Title_Audit.csv", na = "")

# Identify player-years with any source record. This prevents a missing source
# record from being silently converted into a zero title count.
source_appearances <- bind_rows(
  match_data |>
    transmute(tour, year = season_year, player_id = winner_id),
  match_data |>
    transmute(tour, year = season_year, player_id = loser_id)
) |>
  filter(!is.na(player_id), player_id != "") |>
  distinct() |>
  mutate(any_source_record = TRUE)

# Replace only singles_titles_won. All other columns are carried unchanged.
corrected_data <- input_data |>
  select(-any_of("singles_titles_won")) |>
  left_join(
    corrected_titles |>
      rename(singles_titles_won = corrected_singles_titles_won),
    by = c("tour", "year", "player_id")
  ) |>
  left_join(
    source_appearances,
    by = c("tour", "year", "player_id")
  ) |>
  mutate(
    singles_titles_won = case_when(
      !is.na(singles_titles_won) ~ singles_titles_won,
      any_source_record ~ 0L,
      TRUE ~ NA_integer_
    )
  ) |>
  select(-any_source_record)

if (nrow(corrected_data) != 700L) {
  stop("The corrected dataset does not contain exactly 700 rows.")
}

# Verify that only singles_titles_won changed.
non_title_columns <- setdiff(
  names(input_data),
  "singles_titles_won"
)

if (!isTRUE(all.equal(
  input_data |> select(all_of(non_title_columns)),
  corrected_data |> select(all_of(non_title_columns))
))) {
  stop("A variable other than singles_titles_won changed.")
}

title_comparison <- input_data |>
  select(tour, year, player_id, player_name,
         original_singles_titles_won = singles_titles_won) |>
  left_join(
    corrected_data |>
      select(tour, year, player_id,
             corrected_singles_titles_won = singles_titles_won),
    by = c("tour", "year", "player_id")
  ) |>
  mutate(
    title_difference =
      corrected_singles_titles_won - original_singles_titles_won,
    changed = title_difference != 0
  )

write_csv(title_comparison, "Singles_Title_Comparison.csv", na = "")

changed_records <- title_comparison |>
  filter(changed)

final_walkover_audit <- title_audit |>
  filter(is_walkover) |>
  mutate(
    title_walkover_included =
      is_individual_singles_event & is_counted_title
  )

write_csv(
  final_walkover_audit,
  "Singles_Final_Walkover_Audit.csv",
  na = ""
)

validation_report <- tibble(
  input_rows = nrow(input_data),
  output_rows = nrow(corrected_data),
  changed_player_years = nrow(changed_records),
  final_walkovers_examined = nrow(final_walkover_audit),
  individual_final_walkovers_counted =
    sum(final_walkover_audit$title_walkover_included, na.rm = TRUE),
  missing_output_title_values =
    sum(is.na(corrected_data$singles_titles_won)),
  non_title_columns_changed = FALSE
)

write_csv(
  validation_report,
  "Singles_Title_Correction_Report.csv",
  na = ""
)

# Official examples supplied for this correction.
# These are used only as a transparent spot-check, not to alter the data.
official_sample <- tibble(
  tour = c("ATP", "WTA", "ATP"),
  year = c(2020L, 2025L, 2025L),
  player_name = c(
    "Novak Djokovic",
    "Coco Gauff",
    "Taylor Fritz"
  ),
  official_singles_titles_won = c(4L, 2L, 2L)
)

official_sample_check <- corrected_data |>
  select(tour, year, player_name, player_id,
         corrected_singles_titles_won = singles_titles_won) |>
  inner_join(
    official_sample,
    by = c("tour", "year", "player_name")
  ) |>
  mutate(
    agrees_with_official_sample =
      corrected_singles_titles_won == official_singles_titles_won
  )

write_csv(
  official_sample_check,
  "Singles_Title_Official_Sample_Check.csv",
  na = ""
)

if (nrow(official_sample_check) != nrow(official_sample)) {
  warning(
    "One or more official sample players could not be matched by exact name. ",
    "Inspect Singles_Title_Official_Sample_Check.csv."
  )
}

write_csv(
  corrected_data,
  output_file,
  na = ""
)

rules <- c(
  "# Singles-title correction",
  "",
  "Only `singles_titles_won` was recalculated. All other variables were copied unchanged from `Tennis_Top50_2019_2025_Season_Match_Stats.csv`.",
  "",
  "A title is counted when a player is recorded as the winner of a final (`round == F`) in an individual singles event. Final walkovers are included because the tournament title is officially awarded through that final.",
  "",
  "Included title events: regular ATP/WTA tournaments, Grand Slams, and ATP/WTA Finals.",
  "",
  "Excluded title events: ATP Cup, United Cup, Davis Cup, Billie Jean King Cup/Fed Cup, Laver Cup, exhibitions, Olympics, and other team competitions.",
  "",
  "The season year is the source-file year, not the year extracted from `tourney_date`.",
  "",
  "Walkovers are excluded from matches played, wins, losses, and win percentage. A final walkover is nevertheless counted as an individual title. A final involving a retirement is counted only if the player is recorded as the winner and the score is not a walkover.",
  "",
  "All final walkovers in the 2019-2025 ATP/WTA source files are listed in `Singles_Final_Walkover_Audit.csv`.",
  "",
  "If a player-year has no source match record, `singles_titles_won` remains missing rather than being replaced with zero. A zero is used only when source records exist but no eligible final was won.",
  "",
  "The supplied official examples for Novak Djokovic 2020, Coco Gauff 2025, and Taylor Fritz 2025 are checked in `Singles_Title_Official_Sample_Check.csv`.",
  "",
  "No prize-money, ranking, age, match, win, loss, tournament, or Grand Slam variables were changed."
)

writeLines(rules, "Singles_Title_Counting_Rules.md")

cat(
  "\nCompleted successfully.\n",
  "Rows in corrected dataset: ", nrow(corrected_data), "\n",
  "Player-year observations with changed title counts: ",
  nrow(changed_records), "\n",
  "Individual final walkovers counted: ",
  sum(final_walkover_audit$title_walkover_included, na.rm = TRUE), "\n",
  "Output: ", output_file, "\n",
  sep = ""
)
