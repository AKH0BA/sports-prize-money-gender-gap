# Finalize annual ATP/WTA match statistics using tennis-season year.
#
# Season year is the source match-file year, not the year in tourney_date.
# This matters for tournaments that start in December and finish in January.
#
# Required:
#   final_data (or Tennis_Top50_2019_2025_With_Match_Stats.csv)
#   sackmann_match_data/atp_matches_2019.csv ... atp_matches_2025.csv
#   sackmann_match_data/wta_matches_2019.csv ... wta_matches_2025.csv

library(dplyr)
library(readr)
library(stringr)

years <- 2019:2025
match_dir <- "sackmann_match_data"

if (exists("final_data")) {
  analysis_data <- final_data
} else if (file.exists("Tennis_Top50_2019_2025_With_Match_Stats.csv")) {
  analysis_data <- read_csv(
    "Tennis_Top50_2019_2025_With_Match_Stats.csv",
    show_col_types = FALSE
  )
} else if (file.exists("Tennis_Top50_2019_2025_Final_Match_Stats.csv")) {
  analysis_data <- read_csv(
    "Tennis_Top50_2019_2025_Final_Match_Stats.csv",
    show_col_types = FALSE
  )
} else {
  stop("Load final_data or place the existing 700-row CSV in the working directory.")
}

analysis_data <- analysis_data |>
  mutate(
    player_id = as.character(player_id),
    year = as.integer(year)
  )

if (nrow(analysis_data) != 700L) {
  stop("The input dataset must contain exactly 700 rows.")
}

required_match_columns <- c(
  "tourney_id", "tourney_name", "tourney_level", "tourney_date",
  "winner_id", "winner_name", "winner_entry",
  "loser_id", "loser_name", "loser_entry", "score", "round"
)

read_match_file <- function(tour, season_year) {
  path <- file.path(
    match_dir,
    paste0(tolower(tour), "_matches_", season_year, ".csv")
  )
  
  if (!file.exists(path)) {
    stop("Missing match file: ", path)
  }
  
  dat <- read_csv(
    path,
    show_col_types = FALSE,
    col_types = cols(.default = col_character())
  )
  
  missing_columns <- setdiff(required_match_columns, names(dat))
  if (length(missing_columns) > 0L) {
    stop(
      path, " is missing: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  dat |>
    mutate(
      tour = tour,
      source_file_year = as.integer(season_year),
      match_date_year = as.integer(str_sub(tourney_date, 1, 4)),
      winner_id = as.character(winner_id),
      loser_id = as.character(loser_id),
      score_upper = str_to_upper(coalesce(score, "")),
      is_walkover = score_upper == "" |
        str_detect(score_upper, "W/O|WALKOVER|WALK OVER|DEF"),
      is_retirement = str_detect(score_upper, "RET"),
      event_text = str_c(tourney_name, tourney_id, sep = " "),
      competition_class = case_when(
        str_detect(event_text, regex("olympic|olympics",
                                     ignore_case = TRUE)) ~ "Olympics",
        str_detect(event_text, regex("laver cup",
                                     ignore_case = TRUE)) ~ "Laver Cup",
        str_detect(event_text, regex("united cup",
                                     ignore_case = TRUE)) ~ "United Cup",
        str_detect(event_text, regex("atp cup",
                                     ignore_case = TRUE)) ~ "ATP Cup",
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
      is_core_competition = competition_class %in% c(
        "Regular tour",
        "Grand Slam",
        "Tour Finals",
        "ATP Cup",
        "United Cup"
      ),
      is_eligible_match = !is_walkover & is_core_competition
    )
}

match_data <- bind_rows(
  lapply(years, \(x) read_match_file("ATP", x)),
  lapply(years, \(x) read_match_file("WTA", x))
)

if (any(is.na(match_data$match_date_year))) {
  stop("Some match rows have invalid or missing tournament dates.")
}

# Audit tournaments whose start-date year differs from the season-file year.
season_boundary_audit <- match_data |>
  filter(match_date_year != source_file_year) |>
  count(
    tour,
    source_file_year,
    match_date_year,
    tourney_id,
    tourney_name,
    competition_class,
    name = "match_rows"
  ) |>
  arrange(source_file_year, tour, tourney_name)

write_csv(
  season_boundary_audit,
  "Season_Boundary_Tournament_Audit.csv",
  na = ""
)

# Full competition classification audit.
competition_audit <- match_data |>
  count(
    tour,
    source_file_year,
    competition_class,
    tourney_level,
    tourney_name,
    is_walkover,
    is_retirement,
    name = "match_rows"
  ) |>
  arrange(tour, source_file_year, competition_class, tourney_name)

write_csv(
  competition_audit,
  "Season_Match_Audit_Summary.csv",
  na = ""
)

# Build one row per eligible player-match appearance.
player_match_rows <- bind_rows(
  match_data |>
    filter(is_eligible_match) |>
    transmute(
      tour,
      year = source_file_year,
      player_id = winner_id,
      player_name = winner_name,
      tourney_id,
      tourney_name,
      tourney_level,
      competition_class,
      round,
      result = "win"
    ),
  match_data |>
    filter(is_eligible_match) |>
    transmute(
      tour,
      year = source_file_year,
      player_id = loser_id,
      player_name = loser_name,
      tourney_id,
      tourney_name,
      tourney_level,
      competition_class,
      round,
      result = "loss"
    )
) |>
  filter(!is.na(player_id), player_id != "")

# Grand Slam entry is based on a main-draw appearance, including a walkover.
grand_slam_entries <- bind_rows(
  match_data |>
    filter(source_file_year %in% years,
           competition_class == "Grand Slam") |>
    transmute(
      tour, year = source_file_year, player_id = winner_id, tourney_id
    ),
  match_data |>
    filter(source_file_year %in% years,
           competition_class == "Grand Slam") |>
    transmute(
      tour, year = source_file_year, player_id = loser_id, tourney_id
    )
) |>
  filter(!is.na(player_id), player_id != "") |>
  distinct(tour, year, player_id, tourney_id) |>
  count(
    tour, year, player_id,
    name = "grand_slam_tournaments_entered"
  )

season_stats <- player_match_rows |>
  group_by(tour, year, player_id) |>
  summarise(
    singles_matches_played = n(),
    singles_matches_won = sum(result == "win"),
    singles_matches_lost = sum(result == "loss"),
    win_percentage = singles_matches_won / singles_matches_played,
    singles_tournaments_played = n_distinct(tourney_id),
    grand_slam_matches_won = sum(
      result == "win" & tourney_level == "G"
    ),
    singles_titles_won = n_distinct(
      tourney_id[result == "win" & round == "F"]
    ),
    .groups = "drop"
  ) |>
  left_join(
    grand_slam_entries,
    by = c("tour", "year", "player_id")
  ) |>
  mutate(
    grand_slam_tournaments_entered =
      coalesce(grand_slam_tournaments_entered, 0L)
  )

# Track players who appear only in walkovers/excluded competitions.
all_source_appearances <- bind_rows(
  match_data |>
    transmute(
      tour, year = source_file_year, player_id = winner_id
    ),
  match_data |>
    transmute(
      tour, year = source_file_year, player_id = loser_id
    )
) |>
  filter(!is.na(player_id), player_id != "") |>
  distinct() |>
  mutate(any_source_record = TRUE)

season_stats_complete <- analysis_data |>
  select(tour, year, player_id) |>
  left_join(
    season_stats,
    by = c("tour", "year", "player_id")
  ) |>
  left_join(
    all_source_appearances,
    by = c("tour", "year", "player_id")
  ) |>
  mutate(
    coverage_status = case_when(
      !is.na(singles_matches_played) ~
        "eligible_matches_found",
      any_source_record ~
        "source_records_but_no_eligible_matches",
      TRUE ~
        "no_source_record"
    ),
    across(
      c(
        singles_matches_played,
        singles_matches_won,
        singles_matches_lost,
        singles_tournaments_played,
        grand_slam_matches_won,
        grand_slam_tournaments_entered,
        singles_titles_won
      ),
      ~ if_else(
        coverage_status == "source_records_but_no_eligible_matches",
        0L,
        .x
      )
    ),
    win_percentage = case_when(
      coverage_status == "source_records_but_no_eligible_matches" ~ 0,
      TRUE ~ win_percentage
    )
  )

# Verify the specific Brisbane season treatment for Aryna Sabalenka.
sabalenka_brisbane <- match_data |>
  filter(
    tour == "WTA",
    source_file_year == 2025,
    str_detect(
      str_c(winner_name, loser_name),
      regex("sabalenka", ignore_case = TRUE)
    ),
    str_detect(tourney_name, regex("brisbane", ignore_case = TRUE))
  ) |>
  mutate(
    sabalenka_result = case_when(
      str_detect(winner_name, regex("sabalenka", ignore_case = TRUE)) ~
        "win",
      str_detect(loser_name, regex("sabalenka", ignore_case = TRUE)) ~
        "loss",
      TRUE ~ NA_character_
    ),
    counted_in_core = is_eligible_match
  ) |>
  select(
    source_file_year,
    match_date_year,
    tourney_id,
    tourney_name,
    tourney_date,
    round,
    winner_name,
    loser_name,
    score,
    is_walkover,
    is_retirement,
    competition_class,
    sabalenka_result,
    counted_in_core
  ) |>
  arrange(tourney_date, round)

write_csv(
  sabalenka_brisbane,
  "Sabalenka_2025_Brisbane_Season_Audit.csv",
  na = ""
)

sabalenka_brisbane_check <- sabalenka_brisbane |>
  summarise(
    source_file_years = paste(unique(source_file_year), collapse = ", "),
    match_date_years = paste(unique(match_date_year), collapse = ", "),
    match_rows = n(),
    counted_matches = sum(counted_in_core),
    wins = sum(sabalenka_result == "win" & counted_in_core),
    titles = n_distinct(
      tourney_id[
        sabalenka_result == "win" &
          round == "F" &
          counted_in_core
      ]
    )
  )

write_csv(
  sabalenka_brisbane_check,
  "Sabalenka_2025_Brisbane_Season_Check.csv",
  na = ""
)

if (nrow(sabalenka_brisbane) > 0L &&
    any(sabalenka_brisbane$source_file_year != 2025L)) {
  stop("Sabalenka Brisbane records were not assigned to season 2025.")
}

if (nrow(sabalenka_brisbane) > 0L &&
    sum(sabalenka_brisbane$counted_in_core) != 5L) {
  warning(
    "The source rows do not show exactly five counted Brisbane matches; ",
    "inspect Sabalenka_2025_Brisbane_Season_Audit.csv."
  )
}

# Revalidate every existing statistic against the season-year source rows.
revalidation <- analysis_data |>
  transmute(
    tour,
    year,
    player_id,
    player_name,
    dataset_matches = as.numeric(singles_matches_played),
    dataset_wins = as.numeric(singles_matches_won),
    dataset_losses = as.numeric(singles_matches_lost),
    dataset_win_percentage = as.numeric(win_percentage),
    dataset_tournaments = as.numeric(singles_tournaments_played),
    dataset_grand_slam_wins = as.numeric(grand_slam_matches_won),
    dataset_grand_slam_entries =
      as.numeric(grand_slam_tournaments_entered),
    dataset_titles = as.numeric(singles_titles_won)
  ) |>
  left_join(
    season_stats_complete |>
      select(
        tour, year, player_id,
        recalculated_matches = singles_matches_played,
        recalculated_wins = singles_matches_won,
        recalculated_losses = singles_matches_lost,
        recalculated_win_percentage = win_percentage,
        recalculated_tournaments = singles_tournaments_played,
        recalculated_grand_slam_wins = grand_slam_matches_won,
        recalculated_grand_slam_entries =
          grand_slam_tournaments_entered,
        recalculated_titles = singles_titles_won,
        coverage_status
      ),
    by = c("tour", "year", "player_id")
  ) |>
  mutate(
    difference_matches = recalculated_matches - dataset_matches,
    difference_wins = recalculated_wins - dataset_wins,
    difference_losses = recalculated_losses - dataset_losses,
    difference_win_percentage =
      recalculated_win_percentage - dataset_win_percentage,
    difference_tournaments =
      recalculated_tournaments - dataset_tournaments,
    difference_grand_slam_wins =
      recalculated_grand_slam_wins - dataset_grand_slam_wins,
    difference_grand_slam_entries =
      recalculated_grand_slam_entries - dataset_grand_slam_entries,
    difference_titles = recalculated_titles - dataset_titles,
    remaining_mismatch =
      coalesce(difference_matches != 0, TRUE) |
      coalesce(difference_wins != 0, TRUE) |
      coalesce(difference_losses != 0, TRUE) |
      coalesce(abs(difference_win_percentage) > 1e-12, TRUE) |
      coalesce(difference_tournaments != 0, TRUE) |
      coalesce(difference_grand_slam_wins != 0, TRUE) |
      coalesce(difference_grand_slam_entries != 0, TRUE) |
      coalesce(difference_titles != 0, TRUE)
  )

write_csv(
  revalidation,
  "Season_700_Record_Revalidation.csv",
  na = ""
)

write_csv(
  revalidation |>
    count(tour, remaining_mismatch, coverage_status, name = "records"),
  "Season_Revalidation_Summary.csv",
  na = ""
)

# Replace only match-statistics columns; preserve all other variables.
columns_to_replace <- c(
  "singles_matches_played",
  "singles_matches_won",
  "singles_matches_lost",
  "win_percentage",
  "singles_tournaments_played",
  "grand_slam_matches_won",
  "grand_slam_tournaments_entered",
  "singles_titles_won",
  "coverage_status"
)

final_season_data <- analysis_data |>
  select(-any_of(columns_to_replace)) |>
  left_join(
    season_stats_complete |>
      select(
        tour, year, player_id,
        singles_matches_played,
        singles_matches_won,
        singles_matches_lost,
        win_percentage,
        singles_tournaments_played,
        grand_slam_matches_won,
        grand_slam_tournaments_entered,
        singles_titles_won,
        coverage_status
      ),
    by = c("tour", "year", "player_id")
  )

if (nrow(final_season_data) != 700L) {
  stop("The corrected dataset does not contain exactly 700 rows.")
}

write_csv(
  final_season_data,
  "Tennis_Top50_2019_2025_Season_Match_Stats.csv",
  na = ""
)

# Document the final season-year definition.
rules <- c(
  "# Final tennis-season match-counting rules",
  "",
  "## Annual assignment",
  "",
  "Every match is assigned to the season year in its source file name, such as `atp_matches_2025.csv` or `wta_matches_2025.csv`. `tourney_date` is retained only as the tournament start date and is not used to assign annual statistics.",
  "",
  "This assigns tournaments that begin in December but belong to the following tennis season to the source-file season. The boundary cases are listed in `Season_Boundary_Tournament_Audit.csv`.",
  "",
  "## Core competition definition",
  "",
  "Include regular tour matches, Grand Slams, Tour Finals, ATP Cup, and United Cup. Exclude Olympics, Davis Cup, Billie Jean King Cup/Fed Cup, Laver Cup, exhibitions, and other team competitions.",
  "",
  "Walkovers/defaults are excluded from matches played, wins, losses, win percentage, tournaments played, and titles. Retirements are included because the match began. Qualifying matches are not added because these annual files are main-draw files; a Q entry identifies a main-draw qualifier.",
  "",
  "Grand Slam tournaments entered count distinct main-draw tournament IDs, including an entry represented only by a walkover. Grand Slam wins count eligible non-walkover wins at Grand Slams. A singles title is an eligible win in a final (`round == F`).",
  "",
  "## Preservation and validation",
  "",
  "The script preserves official prize money, rankings, ages, player IDs, and all non-match-statistics variables. It writes a new file and does not overwrite prior datasets. All 700 player-year records are checked against the source match rows in `Season_700_Record_Revalidation.csv`.",
  "",
  "No regressions or Oaxaca-Blinder decompositions are performed."
)

writeLines(rules, "Season_Match_Counting_Rules.md")

cat(
  "\nCompleted successfully.\n",
  "Rows in final dataset: ", nrow(final_season_data), "\n",
  "Remaining mismatches: ",
  sum(revalidation$remaining_mismatch, na.rm = TRUE), "\n",
  "Output: Tennis_Top50_2019_2025_Season_Match_Stats.csv\n",
  sep = ""
)