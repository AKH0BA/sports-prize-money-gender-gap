# Add comparable annual singles-performance measures to the 700-row dataset.
#
# Data source:
# Jeff Sackmann's tennis datasets, mirrored at:
# https://github.com/Aneeshers/tennis-sackmann-archive
#
# Definitions used for both tours:
# - A played match is a row not marked as a walkover/default in `score`.
# - Retirements are counted as played matches because a match began.
# - The annual match files are main-draw/tour-event files. Qualifying matches
#   are not included; a `winner_entry` or `loser_entry` of "Q" means that the
#   player qualified for the main draw, not that a qualifying match is present.
# - A tournament is counted once per player using its `tourney_id`.
# - A singles title is recorded as a win in a final (`round == "F"`).
# - Grand Slams are rows with `tourney_level == "G"`.

library(dplyr)
library(readr)
library(stringr)

years <- 2019:2025
archive_base <- "https://raw.githubusercontent.com/Aneeshers/tennis-sackmann-archive/main"
match_dir <- "sackmann_match_data"
dir.create(match_dir, showWarnings = FALSE)

# Use the existing cleaned dataset and the original source datasets.
analysis_file <- "Tennis_Top50_2019_2025_Cleaned.csv"
analysis_data <- read_csv(analysis_file, show_col_types = FALSE)
atp_source <- ATP_Cleaned_Final
wta_source <- WTA_Top100_PrizeMoney_2019_2025

# Normalize names only for validation; original names are preserved.
normalise_name <- function(x) {
  str_to_lower(str_replace_all(x, "[^a-z0-9]", ""))
}

# Restore stable IDs from the original source files using year, rank, and name.
atp_ids <- atp_source |>
  filter(season %in% years, rank >= 1, rank <= 50) |>
  transmute(
    tour = "ATP",
    year = as.integer(season),
    rank = as.integer(rank),
    source_player_name = name,
    player_id = as.character(player_id)
  )

wta_ids <- wta_source |>
  filter(year %in% years, year_end_rank >= 1, year_end_rank <= 50) |>
  transmute(
    tour = "WTA",
    year = as.integer(year),
    rank = as.integer(year_end_rank),
    source_player_name = player_name,
    player_id = as.character(player_id)
  )

id_lookup <- bind_rows(atp_ids, wta_ids) |>
  mutate(source_name_key = normalise_name(source_player_name))

duplicate_id_keys <- id_lookup |>
  count(tour, year, rank) |>
  filter(n > 1)

if (nrow(duplicate_id_keys) > 0L) {
  stop("The original source files contain duplicate tour-year-rank keys.")
}

analysis_key_check <- analysis_data |>
  transmute(
    tour,
    year = as.integer(year),
    rank = as.integer(rank),
    player_name,
    analysis_name_key = normalise_name(player_name)
  ) |>
  left_join(id_lookup, by = c("tour", "year", "rank"))

id_name_mismatches <- analysis_key_check |>
  filter(
    is.na(player_id) |
      analysis_name_key != source_name_key
  ) |>
  select(tour, year, rank, player_name, source_player_name, player_id)

cat("\nID/name validation mismatches:\n")
if (nrow(id_name_mismatches) == 0L) {
  cat("None found.\n")
} else {
  print(id_name_mismatches)
  stop("Resolve ID/name mismatches before merging match statistics.")
}

analysis_with_ids <- analysis_data |>
  left_join(id_lookup |> select(tour, year, rank, player_id),
            by = c("tour", "year", "rank"))

if (nrow(analysis_with_ids) != 700L) {
  stop("Adding stable IDs changed the number of analysis rows.")
}

# Download and inspect the annual ATP and WTA match files.
download_match_file <- function(tour, year) {
  prefix <- tolower(tour)
  file_name <- paste0(prefix, "_matches_", year, ".csv")
  destination <- file.path(match_dir, file_name)
  url <- paste0(archive_base, "/", prefix, "/", file_name)
  
  if (!file.exists(destination)) {
    download.file(url, destination, mode = "wb", quiet = TRUE)
  }
  
  if (!file.exists(destination) || file.info(destination)$size == 0) {
    stop("Could not download archive file: ", url)
  }
  
  destination
}

match_files <- expand.grid(tour = c("ATP", "WTA"), year = years,
                           stringsAsFactors = FALSE) |>
  mutate(path = mapply(download_match_file, tour, year))

required_match_columns <- c(
  "tourney_id", "tourney_level", "tourney_date", "winner_id",
  "winner_name", "loser_id", "loser_name", "score", "round"
)

read_match_file <- function(path, tour, year) {
  dat <- read_csv(path, show_col_types = FALSE,
                  col_types = cols(.default = col_character()))
  
  missing_columns <- setdiff(required_match_columns, names(dat))
  if (length(missing_columns) > 0L) {
    stop("Missing columns in ", path, ": ",
         paste(missing_columns, collapse = ", "))
  }
  
  dat |>
    mutate(tour = tour, file_year = year)
}

match_data <- bind_rows(
  lapply(seq_len(nrow(match_files)), function(i) {
    read_match_file(match_files$path[i], match_files$tour[i],
                    match_files$year[i])
  })
) |>
  mutate(
    match_year = as.integer(str_sub(tourney_date, 1, 4)),
    winner_id = as.character(winner_id),
    loser_id = as.character(loser_id),
    walkover = is.na(score) |
      str_detect(score, regex("W/O|WALKOVER|WALK OVER|DEF",
                              ignore_case = TRUE)),
    # Retirements, including scores containing "RET", remain eligible.
    eligible_match = !walkover &
      match_year %in% years &
      !tourney_level %in% c("D", "E", "J", "T")
  )

cat("\nArchive coverage by tour and source file year:\n")
print(match_data |> count(tour, file_year, name = "rows"))

source_file_year_mismatch <- match_data |>
  filter(match_year != file_year) |>
  count(tour, file_year, match_year, name = "rows")

if (nrow(source_file_year_mismatch) > 0L) {
  cat("\nSource-file year versus match-date year differences:\n")
  print(source_file_year_mismatch)
  cat("Statistics use the calendar year in tourney_date, as requested.\n")
}

# Record whether an analysis player appears anywhere in the covered archive.
all_match_appearances <- bind_rows(
  match_data |>
    transmute(tour, year = match_year, player_id = winner_id),
  match_data |>
    transmute(tour, year = match_year, player_id = loser_id)
) |>
  filter(year %in% years) |>
  distinct()

# Count each eligible match once for each participating player.
player_match_rows <- bind_rows(
  match_data |>
    filter(eligible_match) |>
    transmute(
      tour, year = match_year, player_id = winner_id,
      player_name = winner_name, tourney_id, tourney_level,
      round, result = "win"
    ),
  match_data |>
    filter(eligible_match) |>
    transmute(
      tour, year = match_year, player_id = loser_id,
      player_name = loser_name, tourney_id, tourney_level,
      round, result = "loss"
    )
) |>
  filter(!is.na(player_id), player_id != "")

match_stats <- player_match_rows |>
  group_by(tour, year, player_id) |>
  summarise(
    singles_matches_played = n(),
    singles_matches_won = sum(result == "win"),
    singles_matches_lost = sum(result == "loss"),
    win_percentage = singles_matches_won / singles_matches_played,
    singles_tournaments_played = n_distinct(tourney_id),
    grand_slam_matches_won = sum(result == "win" & tourney_level == "G"),
    singles_titles_won = n_distinct(
      tourney_id[result == "win" & round == "F"]
    ),
    .groups = "drop"
  )

# Tournament entry is separate from played-match counts. A main-draw row
# counts as an entry even when its score is a walkover.
grand_slam_entries <- bind_rows(
  match_data |>
    filter(!tourney_level %in% c("D", "E", "J", "T"),
           tourney_level == "G") |>
    transmute(tour, year = match_year, player_id = winner_id, tourney_id),
  match_data |>
    filter(!tourney_level %in% c("D", "E", "J", "T"),
           tourney_level == "G") |>
    transmute(tour, year = match_year, player_id = loser_id, tourney_id)
) |>
  filter(!is.na(player_id), player_id != "") |>
  distinct(tour, year, player_id, tourney_id) |>
  count(tour, year, player_id,
        name = "grand_slam_tournaments_entered")

match_stats <- match_stats |>
  left_join(grand_slam_entries, by = c("tour", "year", "player_id")) |>
  mutate(grand_slam_tournaments_entered =
           coalesce(grand_slam_tournaments_entered, 0L))

# A player with only excluded records is assigned genuine zeros. A player
# absent from the archive remains NA rather than being silently set to zero.
coverage_status <- analysis_with_ids |>
  select(tour, year, player_id) |>
  left_join(all_match_appearances |>
              mutate(archive_appearance = TRUE),
            by = c("tour", "year", "player_id")) |>
  left_join(match_stats |>
              transmute(tour, year, player_id,
                        eligible_match_record = TRUE),
            by = c("tour", "year", "player_id")) |>
  mutate(
    coverage_status = case_when(
      !is.na(eligible_match_record) ~ "eligible_matches_found",
      !is.na(archive_appearance) ~ "archive_records_but_no_eligible_matches",
      TRUE ~ "no_archive_record"
    )
  ) |>
  select(tour, year, player_id, coverage_status)

match_stats_complete <- analysis_with_ids |>
  select(tour, year, player_id) |>
  left_join(match_stats, by = c("tour", "year", "player_id")) |>
  left_join(coverage_status, by = c("tour", "year", "player_id")) |>
  mutate(
    across(
      c(singles_matches_played, singles_matches_won, singles_matches_lost,
        win_percentage, singles_tournaments_played, grand_slam_matches_won,
        grand_slam_tournaments_entered, singles_titles_won),
      ~ if_else(
        coverage_status == "archive_records_but_no_eligible_matches",
        0,
        .x
      )
    )
  )

# Report unmatched records, duplicate statistics keys, and missing statistics.
unmatched_records <- match_stats_complete |>
  filter(coverage_status == "no_archive_record")

cat("\nPlayer-year records without any archive match record:\n")
print(unmatched_records)

duplicate_stats_keys <- match_stats_complete |>
  count(tour, year, player_id) |>
  filter(n > 1)

cat("\nDuplicate player-year statistic keys:\n")
print(duplicate_stats_keys)

missing_statistics <- match_stats_complete |>
  summarise(across(
    c(singles_matches_played, singles_matches_won, singles_matches_lost,
      win_percentage, singles_tournaments_played, grand_slam_matches_won,
      grand_slam_tournaments_entered, singles_titles_won),
    ~ sum(is.na(.x))
  ))

cat("\nMissing match statistics:\n")
print(missing_statistics)

# Merge statistics by tour-specific stable player ID and year.
final_data <- analysis_with_ids |>
  left_join(match_stats_complete, by = c("tour", "year", "player_id"))

if (nrow(final_data) != 700L) {
  stop("The final dataset does not contain exactly 700 rows.")
}

counts_by_tour_year <- final_data |>
  count(year, tour, name = "observations")
print(counts_by_tour_year)

if (any(counts_by_tour_year$observations != 50L)) {
  stop("At least one year-tour cell does not contain exactly 50 observations.")
}

duplicates_final <- final_data |>
  count(tour, year, player_id, name = "observations") |>
  filter(observations > 1)

cat("\nFinal duplicate player-year records:\n")
print(duplicates_final)

# Save without changing original variables or prize-money missing values.
write_csv(final_data, "Tennis_Top50_2019_2025_With_Match_Stats.csv", na = "")
cat("\nSaved 700 rows to Tennis_Top50_2019_2025_With_Match_Stats.csv\n")