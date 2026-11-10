library(rvest)
library(tidyverse)

# Convert finishing time from HH:MM:SS to seconds
parse_time <- function(x) {
  x <- str_extract(x, "\\d{1,2}:\\d{2}:\\d{2}")
  parts <- str_split_fixed(x, ":", 3)
  
  as.numeric(parts[, 1]) * 3600 +
    as.numeric(parts[, 2]) * 60 +
    as.numeric(parts[, 3])
}

# Extract men's and women's results
get_elite <- function(race, year) {
  url <- paste0(
    "https://en.wikipedia.org/wiki/",
    year, "_", race, "_Marathon"
  )
  
  tables <- read_html(url) |>
    html_elements("table.wikitable") |>
    html_table()
  
  map2_dfr(
    tables[1:2],
    c("M", "F"),
    function(result_table, gender_value) {
      result_table |>
        mutate(
          across(everything(), as.character),
          gender = gender_value
        )
    }
  ) |>
    mutate(
      marathon = race,
      year = year
    )
}

# Years to collect; Boston was cancelled in 2020
years <- c(2019, 2021:2025)

# 2024 Boston Marathon Open Division prize money
prize_schedule <- tibble(
  rank = 1:10,
  prize_money = c(
    150000, 75000, 40000, 25000, 18000,
    13500, 10500, 8500, 7000, 5500
  )
)

# Download, clean, and combine the results
Boston_clean <- map_dfr(
  years,
  function(year) get_elite("Boston", year)
) |>
  group_by(year, gender) |>
  slice_head(n = 10) |>
  mutate(rank = row_number()) |>
  ungroup() |>
  transmute(
    marathon,
    year,
    gender,
    athlete = Athlete,
    rank,
    country = Nationality,
    finish_time = Time,
    finish_time_seconds = parse_time(Time)
  ) |>
  left_join(prize_schedule, by = "rank") |>
  arrange(year, gender, rank)

# Check that each year has 10 men and 10 women
print(table(Boston_clean$year, Boston_clean$gender))

# Save the final dataset
write.csv(
  Boston_clean,
  "Boston_Top10_2019_2025.csv",
  row.names = FALSE
)

# Display the data in RStudio
View(Boston_clean)