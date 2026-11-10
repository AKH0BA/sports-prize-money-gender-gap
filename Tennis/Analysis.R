dir.create("tables",  showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

# ---------------------------------------------------------------------
# PART 0. Load data + make a few new variables
# ---------------------------------------------------------------------
d <- Tennis_Top50_2019_2025_FINAL_VERIFIED

# readable label for tables (female = 1 is WTA, 0 is ATP)
d$gender <- ifelse(d$female == 1, "Women (WTA)", "Men (ATP)")

# prize money is really skewed (few players earn a ton) so I also take the log.
# difference in logs ~ percentage difference
d$log_prize <- log(d$prize_money_usd)          # NA stays NA if prize is missing

# where in the top 50 is the player
d$rank_band <- cut(d$rank, breaks = c(0, 10, 25, 50), labels = c("1-10", "11-25", "26-50"))

cat("Rows:", nrow(d), "| Columns:", ncol(d), "\n")

# ---------------------------------------------------------------------
# PART 1. Sample structure + missing values
# ---------------------------------------------------------------------
# observations per tour and year (should be 50 in every cell)
print(table(d$tour, d$year))

# how many different players and how many years each one shows up
# count by player_id, not name, because a few names are spelled two ways
years_per_player <- table(d$player_id)
cat("Distinct players:", length(years_per_player), "\n")
print(table(years_per_player))                 # how many players appear once, twice, etc.

# missing values in the variables I use
use_vars <- c("prize_money_usd", "rank", "ranking_points", "age", "singles_matches_played",
              "singles_matches_won", "win_percentage", "singles_titles_won", "grand_slam_matches_won")
missing_table <- data.frame(variable = use_vars,
                            n_missing = sapply(d[use_vars], function(x) sum(is.na(x))))
print(missing_table)
write.csv(missing_table, "tables/missing_values.csv", row.names = FALSE)

# which rows have no prize money? I leave them as NA, not zero
print(d[is.na(d$prize_money_usd), c("year", "tour", "player_name", "rank")])

# ---------------------------------------------------------------------
# PART 2. Summary statistics by gender
# ---------------------------------------------------------------------
# N, mean, median, sd, min, max for each variable.
# median is better than mean for prize money because of the skew
describe <- function(x) {
  c(N = sum(!is.na(x)), Mean = mean(x, na.rm = TRUE), Median = median(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE), Min = min(x, na.rm = TRUE), Max = max(x, na.rm = TRUE))
}

summary_vars <- c("prize_money_usd", "log_prize", "age", "rank", "ranking_points",
                  "singles_matches_played", "singles_matches_won", "win_percentage",
                  "singles_titles_won", "grand_slam_matches_won")

make_table <- function(data) round(t(sapply(data[summary_vars], describe)), 2)
men   <- make_table(d[d$female == 0, ])
women <- make_table(d[d$female == 1, ])
cat("\n--- MEN (ATP) ---\n");   print(men)
cat("\n--- WOMEN (WTA) ---\n"); print(women)
write.csv(men,   "tables/summary_men.csv")
write.csv(women, "tables/summary_women.csv")

# ---------------------------------------------------------------------
# PART 3. The prize money gap
# ---------------------------------------------------------------------
# 3a. gap by year: mean log prize for men and women, the difference,
# and women's median as a share of men's median
yr <- data.frame(year = sort(unique(d$year)))
yr$mean_log_men    <- sapply(yr$year, function(y) mean(d$log_prize[d$year == y & d$female == 0], na.rm = TRUE))
yr$mean_log_women  <- sapply(yr$year, function(y) mean(d$log_prize[d$year == y & d$female == 1], na.rm = TRUE))
yr$log_gap         <- yr$mean_log_men - yr$mean_log_women       # positive = men earn more
yr$median_men      <- sapply(yr$year, function(y) median(d$prize_money_usd[d$year == y & d$female == 0], na.rm = TRUE))
yr$median_women    <- sapply(yr$year, function(y) median(d$prize_money_usd[d$year == y & d$female == 1], na.rm = TRUE))
yr$women_share_of_men_median <- yr$median_women / yr$median_men
print(round(yr, 3))
write.csv(round(yr, 3), "tables/gap_by_year.csv", row.names = FALSE)

# 3b. where is the gap? median prize by rank band and gender
band <- aggregate(prize_money_usd ~ rank_band + gender, data = d, FUN = median)
print(band)
write.csv(band, "tables/median_prize_by_rank_band.csv", row.names = FALSE)

# 3c. how top-heavy is each tour? share of total prize money going to the top 10 each year
top10_share <- aggregate(prize_money_usd ~ year + tour, data = d,
                         FUN = function(x) sum(sort(x, decreasing = TRUE)[1:10]) / sum(x))
print(top10_share)    # if one prize is missing in a year it's calculated with the 49 players left

# 3d. Welch t-test on log prize, men vs women
# this is the raw gap, no controls for performance, so it says nothing about discrimination
tt <- t.test(log_prize ~ female, data = d)
print(tt)
cat("Raw gap in mean log prize (men - women):", round(tt$estimate[1] - tt$estimate[2], 3), "\n")
cat("Men earn about", round((exp(tt$estimate[1] - tt$estimate[2]) - 1) * 100, 1), "% more than women on average (geometric mean).\n")

# ---------------------------------------------------------------------
# PART 4. Figures
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# PART 4. Figures
# ---------------------------------------------------------------------
# Fig 1: prize money raw vs log by gender (to show why I use logs)
par(mfrow = c(1, 1))
png("figures/fig1_prize_distribution.png", width = 1100, height = 500)
par(mfrow = c(1, 2))
boxplot(prize_money_usd / 1e6 ~ gender, data = d, ylab = "Prize money (million USD)",
        main = "Raw prize money (very skewed)", col = c("steelblue", "tomato"))
boxplot(log_prize ~ gender, data = d, ylab = "log(prize money)",
        main = "Log prize money (more symmetric)", col = c("steelblue", "tomato"))
dev.off()

par(mfrow = c(1, 2))
boxplot(prize_money_usd / 1e6 ~ gender, data = d, ylab = "Prize money (million USD)",
        main = "Raw prize money (very skewed)", col = c("steelblue", "tomato"))
boxplot(log_prize ~ gender, data = d, ylab = "log(prize money)",
        main = "Log prize money (more symmetric)", col = c("steelblue", "tomato"))

# Fig 2: gap over time
par(mfrow = c(1, 1))
png("figures/fig2_gap_by_year.png", width = 800, height = 500)
plot(yr$year, yr$log_gap, type = "b", pch = 19, ylim = c(0, max(yr$log_gap) * 1.2),
     xlab = "Year", ylab = "Gap in mean log prize (men - women)",
     main = "Prize-money gap by year (2020 = pandemic season)")
abline(h = 0, lty = 2)
dev.off()

plot(yr$year, yr$log_gap, type = "b", pch = 19, ylim = c(0, max(yr$log_gap) * 1.2),
     xlab = "Year", ylab = "Gap in mean log prize (men - women)",
     main = "Prize-money gap by year (2020 = pandemic season)")
abline(h = 0, lty = 2)

# Fig 3: prize money vs rank by gender (log scale on y)
png("figures/fig3_prize_vs_rank.png", width = 800, height = 500)
plot(d$rank, d$prize_money_usd, log = "y", pch = 19,
     col = ifelse(d$female == 1, rgb(1, 0.39, 0.28, 0.5), rgb(0.27, 0.51, 0.71, 0.5)),
     xlab = "Year-end rank", ylab = "Prize money (USD, log scale)", main = "Prize money by rank")
legend("topright", legend = c("Women (WTA)", "Men (ATP)"), pch = 19,
       col = c(rgb(1, 0.39, 0.28), rgb(0.27, 0.51, 0.71)))
dev.off()

plot(d$rank, d$prize_money_usd, log = "y", pch = 19,
     col = ifelse(d$female == 1, rgb(1, 0.39, 0.28, 0.5), rgb(0.27, 0.51, 0.71, 0.5)),
     xlab = "Year-end rank", ylab = "Prize money (USD, log scale)", main = "Prize money by rank")
legend("topright", legend = c("Women (WTA)", "Men (ATP)"), pch = 19,
       col = c(rgb(1, 0.39, 0.28), rgb(0.27, 0.51, 0.71)))
# ---------------------------------------------------------------------
# PART 5. Correlations and a first regression
# ---------------------------------------------------------------------
# 5a. correlation matrix. matches played and won are strongly correlated,
# so I won't put played, won and lost in one regression (multicollinearity)
cor_vars <- c("log_prize", "rank", "ranking_points", "singles_matches_played",
              "singles_matches_won", "singles_titles_won", "grand_slam_matches_won")
cor_table <- round(cor(d[cor_vars], use = "complete.obs"), 2)
print(cor_table)
write.csv(cor_table, "tables/correlations.csv")

# 5b. OLS on log prize money
#   Model 1: gender + year only      -> raw gap, adjusted for year
#   Model 2: + performance and age   -> gap holding performance constant
# factor(year) = a separate intercept for each year (year fixed effects)
# NOTE: these variables are just a first try, not my final choice
m1 <- lm(log_prize ~ female + factor(year), data = d)
m2 <- lm(log_prize ~ female + singles_matches_won + singles_titles_won +
           grand_slam_matches_won + age + factor(year), data = d)

# Clustered standard errors by player. The same player is in the data in several years
# so the rows aren't independent, and clustering fixes the SEs for that
# (coefficients stay the same as normal OLS, only the SEs change).
# I wrote the usual CR1 formula by hand in base R so I don't need extra packages,
# it should match Stata's vce(cluster player_id)
cluster_se <- function(model, cluster) {
  X  <- model.matrix(model)                    # the regressors
  e  <- residuals(model)                       # residuals
  cl <- cluster[as.numeric(names(e))]          # player_id for each row that was used
  n  <- nrow(X); k <- ncol(X); G <- length(unique(cl))
  bread <- solve(crossprod(X))                 # (X'X)^-1
  meat  <- matrix(0, k, k)
  for (g in unique(cl)) {                      # sum up each player's X_g' e_g
    s_g  <- colSums(X[cl == g, , drop = FALSE] * e[cl == g])
    meat <- meat + tcrossprod(s_g)
  }
  V  <- bread %*% meat %*% bread * (G / (G - 1)) * ((n - 1) / (n - k))
  se <- sqrt(diag(V))
  b  <- coef(model)
  tval <- b / se
  data.frame(estimate = b, cluster_se = se, t_value = tval,
             p_value = 2 * pt(-abs(tval), df = G - 1))   # df = number of players - 1
}

show <- function(model) {
  out <- cluster_se(model, d$player_id)
  print(round(out[!grepl("factor", rownames(out)), ], 4))   # leave out the year dummies
  cat("Observations:", nobs(model), "| players (clusters):",
      length(unique(d$player_id[as.numeric(names(residuals(model)))])),
      "| R-squared:", round(summary(model)$r.squared, 3), "\n")
}

cat("\n=== Model 1: gender + year effects ===\n");              show(m1)
cat("\n=== Model 2: + performance and age ===\n");              show(m2)

cat("\nHow to read the 'female' row: the coefficient is the difference in LOG prize money between a\n",
    "woman and a man. exp(coef) - 1 turns it into a percentage.\n")
cat("Model 1: women earn about", round((1 - exp(coef(m1)["female"])) * 100, 1), "% less in the same year.\n")
cat("Model 2: women earn about", round((1 - exp(coef(m2)["female"])) * 100, 1),
    "% less with the same wins, titles, Grand Slam wins and age.\n")
cat("A remaining gap is NOT automatically discrimination: it contains everything the model leaves out\n",
    "(doubles income, prize-structure differences, selection into the top 50, ...).\n")