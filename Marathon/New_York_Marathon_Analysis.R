library(dplyr)

# Convert seconds into H:MM:SS format
format_time <- function(seconds) {
  hours <- seconds %/% 3600
  minutes <- (seconds %% 3600) %/% 60
  remaining_seconds <- seconds %% 60
  
  sprintf(
    "%d:%02d:%02d",
    hours,
    minutes,
    remaining_seconds
  )
}

# New York City Marathon Open Division prize money
# The amounts are the same for men and women
prize_schedule <- data.frame(
  rank = 1:10,
  prize_money = c(
    100000, 60000, 40000, 25000, 15000,
    10000, 7500, 5000, 2500, 2000
  )
)

# Import existing NYC dataset
nyc <- NYC.Marathon.Results

# Keep the top 10 men and women from 2019 through 2024
nyc_clean <- nyc %>%
  filter(
    Year >= 2019,
    Year <= 2024,
    Gender %in% c("M", "W")
  ) %>%
  group_by(Year, Gender) %>%
  arrange(Overall, .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 10) %>%
  ungroup() %>%
  transmute(
    marathon = "New York",
    year = Year,
    gender = if_else(Gender == "M", "M", "F"),
    athlete = Name,
    rank = rank,
    country = Country,
    age = Age,
    finish_time_seconds = as.numeric(Finish)
  )

# Manually create the 2025 top 10 results
nyc_2025 <- data.frame(
  marathon = rep("New York", 20),
  year = rep(2025, 20),
  gender = c(rep("M", 10), rep("F", 10)),
  
  athlete = c(
    "Benson Kipruto",
    "Alexander Mutiso",
    "Patrick Dever",
    "Matthias Kyburz",
    "Joel Reichow",
    "Charles Hicks",
    "Sondre Nordstad Moen",
    "Tsegay Weldlibanos",
    "Joe Klecker",
    "Daniele Meucci",
    "Hellen Obiri",
    "Sharon Lokedi",
    "Sheila Chepkirui",
    "Fiona O'Keeffe",
    "Annie Frisbie",
    "Sifan Hassan",
    "Jessica Warner-Judd",
    "Emily Sisson",
    "Amanda Vestri",
    "Fionnuala McCormack"
  ),
  
  rank = rep(1:10, 2),
  
  country = c(
    "KEN", "KEN", "GBR", "CHE", "USA",
    "USA", "NOR", "ERI", "USA", "ITA",
    "KEN", "KEN", "KEN", "USA", "USA",
    "NLD", "GBR", "USA", "USA", "IRL"
  ),
  
  age = c(
    34, 29, 29, 35, 32,
    24, 34, 29, 28, 40,
    35, 31, 34, 27, 28,
    32, 30, 34, 26, 41
  ),
  
  finish_time_seconds = c(
    7689, 7689, 7738, 7795, 7796,
    7799, 7815, 7836, 7837, 7840,
    8391, 8407, 8424, 8569, 8652,
    8683, 8685, 8705, 8740, 8820
  )
)

# Combine the older data with 2025 data
# Remove existing 2025 rows first to prevent duplicates
nyc_final <- nyc_clean %>%
  filter(year != 2025) %>%
  bind_rows(nyc_2025) %>%
  left_join(prize_schedule, by = "rank") %>%
  mutate(
    finish_time = format_time(finish_time_seconds)
  ) %>%
  arrange(year, gender, rank)

# Check the number of athletes per year and gender
print(table(nyc_final$year, nyc_final$gender))

# Save the final dataset
write.csv(
  nyc_final,
  "NYC_Top10_2019_2025_Final.csv",
  row.names = FALSE
)

# View the final data in RStudio
View(nyc_final)