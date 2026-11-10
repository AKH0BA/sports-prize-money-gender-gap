# London Marathon places 11 and 12 from World Athletics results
London_places_11_12 <- tibble(
  year = rep(2019:2025, each = 4),
  
  gender = rep(c("M", "M", "F", "F"), 7),
  
  rank = rep(c(11, 12, 11, 12), 7),
  
  athlete = c(
    "Daniel Wanjiru", "Wilson Kipsang",
    "Linet Masai", "Molly Huddle",
    
    "Stephen Scullion", "Peter Herzog",
    "Edith Chelimo", "Ellie Pashley",
    
    "Jonathan Mellor", "Weynay Ghebresilasie",
    "Birhane Dibaba", "Sinead Diver",
    
    "Kohei Futaoka", "Ross Braden",
    "Marci Klimek", "Stephanie Twell",
    
    "Frank Lara", "Tom Gröschel",
    "Samantha Harrison", "Dominique Scott",
    
    "Marc Scott", "Alexander Lepretre",
    "Mhairi Maclennan", "Becky Briggs",
    
    "Andrew Buchanan", "Adam Lipschitz",
    "Philippa Bowden", "Molly Bookmyer"
  ),
  
  country = c(
    "KEN", "KEN", "KEN", "USA",
    "IRL", "AUT", "KEN", "AUS",
    "GBR", "GBR", "ETH", "AUS",
    "JPN", "GBR", "USA", "GBR",
    "USA", "GER", "GBR", "RSA",
    "GBR", "GBR", "GBR", "GBR",
    "AUS", "RSA", "GBR", "USA"
  ),
  
  finish_time = c(
    "2:08:40", "2:09:18", "2:26:06", "2:26:33",
    "2:09:49", "2:10:06", "2:29:03", "2:31:31",
    "2:16:09", "2:16:27", "2:24:21", "2:27:16",
    "2:14:18", "2:14:32", "2:37:56", "2:39:16",
    "2:13:29", "2:13:29", "2:25:59", "2:29:19",
    "2:11:19", "2:15:34", "2:29:15", "2:35:25",
    "2:09:11", "2:09:48", "2:30:28", "2:32:31"
  )
) %>%
  mutate(
    marathon = "London",
    finish_time_seconds = parse_time_seconds(finish_time)
  ) %>%
  select(
    marathon,
    year,
    gender,
    athlete,
    rank,
    country,
    finish_time,
    finish_time_seconds
  )

# Add the correct prize money for places 11 and 12
London_places_11_12 <- London_places_11_12 %>%
  left_join(
    prize_schedule,
    by = c("year", "rank")
  )

# Add places 11 and 12 to the existing top-10 data
London_final <- London_clean %>%
  filter(!rank %in% c(11, 12)) %>%
  bind_rows(London_places_11_12) %>%
  arrange(year, gender, rank)

# Check that every year has 12 men and 12 women
print(table(London_final$year, London_final$gender))

# Save the final top-12 dataset
write.csv(
  London_final,
  "London_Top12_2019_2025.csv",
  row.names = FALSE
)

# View the final data
View(London_final)