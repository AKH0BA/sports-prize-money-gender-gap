library(dplyr)

chicago <- read.csv(file.choose(), stringsAsFactors = FALSE)

chicago_clean <- chicago %>%
  filter(
    year >= 2019,
    year <= 2025,
    gender %in% c("Man", "Woman"),
    place_gender >= 1,
    place_gender <= 5
  ) %>%
  transmute(
    marathon = "Chicago",
    year = year,
    gender = ifelse(gender == "Man", "M", "F"),
    athlete = athlete_name,
    rank = place_gender,
    country = country_ioc,
    finish_time_seconds = finish_time_seconds
  ) %>%
  arrange(year, gender, rank)

write.csv(chicago_clean, "Chicago_Top5_Cleaned.csv", row.names = FALSE)