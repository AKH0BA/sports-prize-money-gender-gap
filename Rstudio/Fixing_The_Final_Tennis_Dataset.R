# Combine ATP and WTA year-end top-50 data for 2019-2025

library(dplyr)
library(readr)
library(lubridate)

# Use the datasets already imported in RStudio.
# Replace the names on the right if your imported objects have different names.
atp_file <- ATP_Cleaned_Final
wta_file <- WTA_Top100_PrizeMoney_2019_2025

years <- 2019:2025

# Keep year-end ATP ranks 1-50 and standardize the variable names.
atp <- atp_file |>
  filter(season %in% years, rank >= 1, rank <= 50) |>
  transmute(
    year = as.integer(season),
    tour = "ATP",
    female = 0L,
    player_name = name,
    country = country,
    age = as.numeric(age),
    rank = as.integer(rank),
    ranking_points = as.numeric(points),
    # ATP's "tournaments" is retained as the ATP-specific measure.
    tournaments_reported = as.numeric(tournaments),
    atp_tournaments = as.numeric(tournaments),
    wta_ranking_tours = NA_real_,
    prize_money_usd = as.numeric(atp_official_prize_money_usd)
  )

# Keep year-end WTA ranks 1-50 and standardize the variable names.
wta <- wta_file |>
  filter(year %in% years, year_end_rank >= 1, year_end_rank <= 50) |>
  transmute(
    year = as.integer(year),
    tour = "WTA",
    female = 1L,
    player_name = player_name,
    country = ioc,
    age = NA_real_, # Filled below after validating the player-name match.
    rank = as.integer(year_end_rank),
    ranking_points = as.numeric(year_end_rank_points),
    # WTA's ranking_tours is not assumed to equal ATP's tournaments measure.
    tournaments_reported = NA_real_,
    atp_tournaments = NA_real_,
    wta_ranking_tours = as.numeric(ranking_tours),
    prize_money_usd = as.numeric(total_prize_money_usd)
  )

# Combine observations vertically; ATP and WTA players are not joined.
tennis_top50 <- bind_rows(atp, wta) |>
  arrange(year, tour, rank)

# Parse WTA dates such as 19960424.0 and calculate age on December 31.
wta_dob_lookup <- wta_file |>
  filter(year %in% years, year_end_rank >= 1, year_end_rank <= 50) |>
  transmute(
    year = as.integer(year),
    rank = as.integer(year_end_rank),
    source_player_name = player_name,
    dob_date = ymd(sprintf("%08.0f", as.numeric(dob))),
    ranking_tours = as.numeric(ranking_tours)
  ) |>
  mutate(
    year_end_date = ymd(paste0(year, "-12-31")),
    wta_age = as.numeric(year_end_date - dob_date) / 365.25
  )

# Ensure the lookup key identifies one WTA player per year and rank.
wta_key_duplicates <- wta_dob_lookup |>
  count(year, rank, name = "observations") |>
  filter(observations > 1)

if (nrow(wta_key_duplicates) > 0L) {
  stop("Duplicate WTA year-rank keys prevent safe DOB matching.")
}

# Validate WTA names before using the DOB lookup.
normalise_name <- function(x) {
  tolower(gsub("[^a-z0-9]", "", x))
}

wta_name_check <- tennis_top50 |>
  filter(tour == "WTA") |>
  select(year, rank, player_name) |>
  left_join(wta_dob_lookup |> select(year, rank, source_player_name),
            by = c("year", "rank")) |>
  mutate(name_agrees = normalise_name(player_name) ==
           normalise_name(source_player_name))

name_mismatches <- wta_name_check |>
  filter(is.na(source_player_name) | !name_agrees)

cat("\nWTA player-name mismatches:\n")
if (nrow(name_mismatches) == 0L) {
  cat("None found.\n")
} else {
  print(name_mismatches)
  stop("WTA player-name mismatches must be resolved before calculating ages.")
}

# Fill only WTA ages; ATP age values are left unchanged.
original_atp_ages <- tennis_top50$age[tennis_top50$tour == "ATP"]
original_prize_money <- tennis_top50$prize_money_usd

tennis_top50 <- tennis_top50 |>
  left_join(wta_dob_lookup |> select(year, rank, wta_age),
            by = c("year", "rank")) |>
  mutate(age = if_else(tour == "WTA", wta_age, age)) |>
  select(-wta_age) |>
  arrange(year, tour, rank)

if (!isTRUE(all.equal(original_atp_ages,
                      tennis_top50$age[tennis_top50$tour == "ATP"]))) {
  stop("ATP age values were changed unexpectedly.")
}

if (!isTRUE(all.equal(original_prize_money, tennis_top50$prize_money_usd))) {
  stop("Prize-money values were changed unexpectedly.")
}

if (sum(is.na(tennis_top50$prize_money_usd)) != 3L) {
  stop("The cleaned dataset should contain exactly three missing prize-money values.")
}

# Check the expected number of observations by tour and year.
counts_by_tour_year <- tennis_top50 |>
  count(year, tour, name = "observations")
print(counts_by_tour_year)

if (nrow(tennis_top50) != 700L) {
  stop("The combined dataset does not contain the expected 700 rows.")
}

if (any(counts_by_tour_year$observations != 50L)) {
  stop("At least one tour-year does not contain exactly 50 players.")
}

# Check for duplicate player-year observations within each tour.
duplicates <- tennis_top50 |>
  count(year, tour, player_name, name = "observations") |>
  filter(observations > 1)

cat("\nDuplicate player-year observations:\n")
if (nrow(duplicates) == 0L) {
  cat("None found.\n")
} else {
  print(duplicates)
}

# Report invalid ages.
invalid_ages <- tennis_top50 |>
  filter(!is.na(age), age < 0 | age > 100) |>
  select(year, tour, player_name, age)

cat("\nInvalid ages:\n")
if (nrow(invalid_ages) == 0L) {
  cat("None found.\n")
} else {
  print(invalid_ages)
}

# Report missing values in the final dataset.
missing_values <- tennis_top50 |>
  summarise(across(everything(), ~ sum(is.na(.))))
missing_values <- data.frame(
  variable = names(missing_values),
  missing_count = as.integer(missing_values[1, ])
)

cat("\nMissing values by variable:\n")
print(missing_values)

# Save the corrected data without replacing any missing values.
write_csv(tennis_top50, "Tennis_Top50_2019_2025_Cleaned.csv", na = "")

cat("\nSaved 700 rows to Tennis_Top50_2019_2025_Cleaned.csv\n")
