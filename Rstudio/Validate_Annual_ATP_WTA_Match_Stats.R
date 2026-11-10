# Validate annual ATP/WTA match statistics.
#
# Required local files:
#   Tennis_Top50_2019_2025_With_Match_Stats.csv
#   ATP_Cleaned_Final(2).csv
#   sackmann_match_data/atp_matches_2019.csv ... atp_matches_2025.csv
#   sackmann_match_data/wta_matches_2019.csv ... wta_matches_2025.csv
#
# The match files should be the same files used to create the statistics.
# This script does not alter any input data or prize-money variables.

library(dplyr)
library(readr)
library(stringr)

years <- 2019:2025

# Use datasets already loaded in RStudio
analysis_data <- final_data
atp_original <- ATP_Cleaned_Final

# Folder containing the Sackmann CSV files
match_dir <- "sackmann_match_data"

# Check only the 14 match files
match_files_required <- c(
  file.path(match_dir, paste0("atp_matches_", years, ".csv")),
  file.path(match_dir, paste0("wta_matches_", years, ".csv"))
)

missing_files <- match_files_required[
  !file.exists(match_files_required)
]

if (length(missing_files) > 0L) {
  stop(
    "These Sackmann files are missing:\n",
    paste(missing_files, collapse = "\n")
  )
}


analysis_data <- final_data
atp_original <- ATP_Cleaned_Final

normalise_name <- function(x) {
  str_to_lower(str_replace_all(x, "[^a-z0-9]", ""))
}

required_match_columns <- c(
  "tourney_id", "tourney_name", "tourney_level", "tourney_date",
  "winner_id", "winner_name", "winner_entry",
  "loser_id", "loser_name", "loser_entry", "score", "round"
)

read_match_file <- function(tour, year) {
  path <- file.path(match_dir, paste0(tolower(tour), "_matches_", year, ".csv"))
  dat <- read_csv(
    path,
    show_col_types = FALSE,
    col_types = cols(.default = col_character())
  )
  
  missing_columns <- setdiff(required_match_columns, names(dat))
  if (length(missing_columns) > 0L) {
    stop(path, " is missing: ", paste(missing_columns, collapse = ", "))
  }
  
  dat |>
    mutate(
      tour = tour,
      source_file_year = year,
      match_calendar_year = as.integer(str_sub(tourney_date, 1, 4)),
      winner_id = as.character(winner_id),
      loser_id = as.character(loser_id),
      score_upper = str_to_upper(coalesce(score, "")),
      is_walkover = score_upper == "" |
        str_detect(score_upper, "W/O|WALKOVER|WALK OVER|DEF"),
      is_retirement = str_detect(score_upper, "RET"),
      is_olympic = str_detect(
        str_c(tourney_name, tourney_id, sep = " "),
        regex("olympic|olympics", ignore_case = TRUE)
      ),
      is_davis_or_team = tourney_level %in% c("D", "T") |
        str_detect(
          str_c(tourney_name, tourney_level, sep = " "),
          regex("davis|billie jean king|fed cup|team", ignore_case = TRUE)
        ),
      is_qualifying_label = str_to_upper(coalesce(winner_entry, "")) == "Q" |
        str_to_upper(coalesce(loser_entry, "")) == "Q"
    )
}

match_data <- bind_rows(
  lapply(years, \(y) read_match_file("ATP", y)),
  lapply(years, \(y) read_match_file("WTA", y))
)

if (any(is.na(match_data$match_calendar_year))) {
  stop("Some match rows have invalid or missing tourney_date values.")
}

# Main counting rule used for the validation:
# - use the source-file year as the season year, because the official ATP
#   annual files and original ATP totals are season-file based;
# - exclude Davis/team, exhibition, junior, and team-tennis records;
# - exclude walkovers/defaults from played/win/loss counts;
# - include retirements because the match began;
# - qualifying matches are not separately available in these main-draw files;
# - keep Olympic rows flagged and include them as individual singles matches;
# - include Grand Slams and Tour Finals as singles competitions.
match_data <- match_data |>
  mutate(
    is_excluded_competition = tourney_level %in% c("D", "E", "J", "T") |
      is_davis_or_team,
    is_eligible_match = !is_walkover & !is_excluded_competition
  )

competition_audit <- match_data |>
  count(
    tour, source_file_year, tourney_level, tourney_name,
    is_olympic, is_davis_or_team, is_qualifying_label,
    is_walkover, is_retirement, is_eligible_match,
    name = "match_rows"
  ) |>
  arrange(tour, source_file_year, tourney_level, tourney_name)
write_csv(competition_audit, "Match_Audit_Summary.csv", na = "")

make_player_rows <- function(data, year_column) {
  year_value <- sym(year_column)
  
  bind_rows(
    data |>
      filter(is_eligible_match) |>
      transmute(
        tour, year = !!year_value, player_id = winner_id,
        player_name = winner_name, tourney_id, tourney_name,
        tourney_level, round, result = "win"
      ),
    data |>
      filter(is_eligible_match) |>
      transmute(
        tour, year = !!year_value, player_id = loser_id,
        player_name = loser_name, tourney_id, tourney_name,
        tourney_level, round, result = "loss"
      )
  ) |>
    filter(!is.na(player_id), player_id != "")
}

calculate_stats <- function(data, year_column) {
  player_rows <- make_player_rows(data, year_column)
  
  stats <- player_rows |>
    group_by(tour, year, player_id) |>
    summarise(
      recalculated_matches_played = n(),
      recalculated_wins = sum(result == "win"),
      recalculated_losses = sum(result == "loss"),
      recalculated_win_percentage =
        recalculated_wins / recalculated_matches_played,
      recalculated_tournaments = n_distinct(tourney_id),
      .groups = "drop"
    )
  
  # Include players with only walkovers/excluded records as genuine zeroes,
  # while keeping players absent from the source as missing.
  appearances <- bind_rows(
    data |>
      transmute(tour, year = !!sym(year_column), player_id = winner_id),
    data |>
      transmute(tour, year = !!sym(year_column), player_id = loser_id)
  ) |>
    filter(!is.na(player_id), player_id != "") |>
    distinct() |>
    mutate(any_source_record = TRUE)
  
  analysis_keys <- analysis_data |>
    transmute(
      tour, year = as.integer(year), player_id = as.character(player_id)
    ) |>
    distinct()
  
  analysis_keys |>
    left_join(stats, by = c("tour", "year", "player_id")) |>
    left_join(appearances, by = c("tour", "year", "player_id")) |>
    mutate(
      coverage_status = case_when(
        !is.na(recalculated_matches_played) ~ "eligible_matches_found",
        any_source_record ~ "source_records_but_no_eligible_matches",
        TRUE ~ "no_source_record"
      ),
      across(
        c(recalculated_matches_played, recalculated_wins,
          recalculated_losses, recalculated_tournaments),
        ~ if_else(
          coverage_status == "source_records_but_no_eligible_matches",
          0, .x
        )
      ),
      recalculated_win_percentage = case_when(
        coverage_status == "source_records_but_no_eligible_matches" ~ 0,
        TRUE ~ recalculated_win_percentage
      )
    )
}

stats_by_file_year <- calculate_stats(match_data, "source_file_year")
stats_by_calendar_year <- calculate_stats(match_data, "match_calendar_year")

# Task 1: compare all ATP player-years with original ATP totals.
atp_original_totals <- atp_original |>
  filter(season %in% years, rank >= 1, rank <= 50) |>
  transmute(
    tour = "ATP",
    year = as.integer(season),
    player_id = as.character(player_id),
    player_name_original = name,
    original_matches = as.numeric(matches),
    original_wins = as.numeric(wins),
    original_tournaments = as.numeric(tournaments)
  )

atp_new_stats <- analysis_data |>
  filter(tour == "ATP") |>
  transmute(
    tour, year = as.integer(year), player_id = as.character(player_id),
    player_name_new = player_name,
    new_matches = as.numeric(singles_matches_played),
    new_wins = as.numeric(singles_matches_won),
    new_tournaments = as.numeric(singles_tournaments_played)
  )

atp_comparison <- atp_original_totals |>
  left_join(atp_new_stats, by = c("tour", "year", "player_id")) |>
  left_join(
    stats_by_file_year |>
      filter(tour == "ATP") |>
      select(
        tour, year, player_id,
        recalculated_file_year_matches = recalculated_matches_played,
        recalculated_file_year_wins = recalculated_wins,
        recalculated_file_year_tournaments = recalculated_tournaments,
        file_year_coverage_status = coverage_status
      ),
    by = c("tour", "year", "player_id")
  ) |>
  left_join(
    stats_by_calendar_year |>
      filter(tour == "ATP") |>
      select(
        tour, year, player_id,
        recalculated_calendar_year_matches = recalculated_matches_played,
        recalculated_calendar_year_wins = recalculated_wins,
        recalculated_calendar_year_tournaments = recalculated_tournaments,
        calendar_year_coverage_status = coverage_status
      ),
    by = c("tour", "year", "player_id")
  ) |>
  mutate(
    difference_matches = new_matches - original_matches,
    difference_wins = new_wins - original_wins,
    difference_tournaments = new_tournaments - original_tournaments,
    file_year_difference_matches =
      recalculated_file_year_matches - original_matches,
    file_year_difference_wins =
      recalculated_file_year_wins - original_wins,
    file_year_difference_tournaments =
      recalculated_file_year_tournaments - original_tournaments,
    discrepancy = coalesce(difference_matches != 0, TRUE) |
      coalesce(difference_wins != 0, TRUE) |
      coalesce(difference_tournaments != 0, TRUE)
  )

atp_discrepancies <- atp_comparison |>
  filter(discrepancy)
write_csv(atp_discrepancies, "ATP_Match_Comparison.csv", na = "")

# Task 2: detailed Rafael Nadal 2019 audit.
nadal_source_rows <- match_data |>
  filter(
    tour == "ATP",
    source_file_year == 2019,
    winner_name == "Rafael Nadal" | loser_name == "Rafael Nadal"
  ) |>
  mutate(
    nadal_result = case_when(
      winner_name == "Rafael Nadal" ~ "win",
      loser_name == "Rafael Nadal" ~ "loss",
      TRUE ~ NA_character_
    ),
    included_in_count = is_eligible_match
  ) |>
  select(
    source_file_year, match_calendar_year, tourney_id, tourney_name,
    tourney_level, tourney_date, round, winner_id, winner_name,
    loser_id, loser_name, score, is_retirement, is_walkover,
    is_olympic, is_davis_or_team, is_qualifying_label,
    is_excluded_competition, included_in_count, nadal_result
  ) |>
  arrange(tourney_date, tourney_name, round)

write_csv(nadal_source_rows, "Nadal_2019_Match_Audit.csv", na = "")

nadal_by_tournament <- nadal_source_rows |>
  group_by(
    source_file_year, match_calendar_year, tourney_id, tourney_name,
    tourney_level
  ) |>
  summarise(
    match_records = n(),
    included_matches = sum(included_in_count),
    wins = sum(nadal_result == "win" & included_in_count),
    losses = sum(nadal_result == "loss" & included_in_count),
    retirements = sum(is_retirement),
    walkovers = sum(is_walkover),
    excluded_competition_records = sum(is_excluded_competition),
    .groups = "drop"
  ) |>
  arrange(source_file_year, tourney_name)

write_csv(nadal_by_tournament, "Nadal_2019_By_Tournament.csv", na = "")

cat("\nRafael Nadal 2019 match records by tournament:\n")
print(nadal_by_tournament)

# Task 3: deterministic WTA validation sample: ranks 1, 25, and 50 each year.
wta_sample_keys <- analysis_data |>
  filter(tour == "WTA", year %in% years, rank %in% c(1, 25, 50)) |>
  distinct(tour, year, rank, player_id, player_name) |>
  arrange(year, rank)

wta_sample <- wta_sample_keys |>
  left_join(
    stats_by_file_year |>
      filter(tour == "WTA") |>
      select(
        tour, year, player_id,
        recalculated_matches_played, recalculated_wins,
        recalculated_losses, recalculated_win_percentage,
        recalculated_tournaments, coverage_status
      ),
    by = c("tour", "year", "player_id")
  ) |>
  left_join(
    analysis_data |>
      filter(tour == "WTA") |>
      transmute(
        tour, year, player_id,
        dataset_matches = as.numeric(singles_matches_played),
        dataset_wins = as.numeric(singles_matches_won),
        dataset_losses = as.numeric(singles_matches_lost),
        dataset_tournaments = as.numeric(singles_tournaments_played)
      ),
    by = c("tour", "year", "player_id")
  ) |>
  mutate(
    difference_matches = recalculated_matches_played - dataset_matches,
    difference_wins = recalculated_wins - dataset_wins,
    difference_losses = recalculated_losses - dataset_losses,
    difference_tournaments = recalculated_tournaments - dataset_tournaments,
    validation_pass = coalesce(difference_matches == 0, FALSE) &
      coalesce(difference_wins == 0, FALSE) &
      coalesce(difference_losses == 0, FALSE) &
      coalesce(difference_tournaments == 0, FALSE)
  )

write_csv(wta_sample, "Match_Validation_Sample.csv", na = "")

# Task 4: document the reproducible counting rules and supported findings.
rule_text <- c(
  "# Match-counting rules",
  "",
  "## Source coverage",
  "",
  "The validation uses the annual Jeff Sackmann ATP/WTA match files for 2019-2025. These files contain main-draw match records. They do not provide a separate qualifying-match file in the inputs required here.",
  "",
  "## Recommended definition",
  "",
  "For both tours, count an athlete's annual singles performance using match rows in the annual source file whose source-file year equals the observation year. Count each eligible match once for the winner and once for the loser. Report matches played, wins, losses, win percentage, and distinct tournaments.",
  "",
  "## Exclusions and inclusions",
  "",
  "- Exclude walkovers/defaults, identified by a missing/blank score or score text containing W/O, WALKOVER, WALK OVER, or DEF.",
  "- Include retirements as played matches because the match began; the retirement is flagged separately.",
  "- Exclude Davis Cup and other team competitions, exhibitions, juniors, and team-tennis categories when identified by tournament level or name.",
  "- Include Grand Slam matches and Tour Finals as individual singles competitions.",
  "- Keep Olympic matches flagged and include them only if the thesis defines Olympic singles as part of the comparable annual singles competition. The audit reports them separately.",
  "- Do not add qualifying matches: the inspected annual match files are main-draw files. A Q entry in a main-draw row means the player entered the main draw as a qualifier, not that a qualifying match is present.",
  "",
  "## Calendar-year sensitivity",
  "",
  "The script reports both source-file-year and tourney-date calendar-year statistics. This matters because some annual files can contain tournaments whose dates fall in the preceding December. The comparison exposes this difference rather than silently choosing the version that best matches official totals.",
  "",
  "## Interpretation",
  "",
  "Do not replace unresolved missing statistics with zero. A zero is used only when a player has source records but every record is excluded under the documented rules. A player with no source record remains missing.",
  "",
  "## Provenance",
  "",
  "Jeff Sackmann's datasets are archived at https://github.com/Aneeshers/tennis-sackmann-archive. Preserve the archive license and attribution in the thesis data appendix."
)
writeLines(rule_text, "Match_Counting_Rules.md")

cat("\nATP discrepancies:", nrow(atp_discrepancies), "of",
    nrow(atp_comparison), "player-year records.\n")
cat("WTA validation sample:", nrow(wta_sample), "records; passes:",
    sum(wta_sample$validation_pass, na.rm = TRUE), "\n")
cat("Created ATP_Match_Comparison.csv, Match_Validation_Sample.csv, ",
    "Match_Audit_Summary.csv, Match_Counting_Rules.md, and Nadal audit files.\n")