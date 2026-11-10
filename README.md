# Gender Prize-Money Gap in Professional Sports (2019–2025)

Bachelor's thesis project in Economics and Public Policy, Masaryk University (Brno, Czech Republic).

This repository holds the datasets, data-processing scripts and preliminary analysis for a study of **gender differences in prize-money earnings in tennis, marathon running and triathlon, 2019–2025**.

> **Status:** research in progress. The tennis dataset and descriptive analysis are done. The marathon prize schedules, the triathlon prize data and the econometric models are still being built.

## Research questions

1. How large is the gender gap in annual prize money among ATP and WTA players?
2. How much of that gap is associated with performance, player characteristics and tournament participation?
3. How do prize structures and the number of paid places differ across tennis, marathons and triathlon?

An unexplained statistical gap is **not** treated as evidence of discrimination. The study separates observed gaps, conditional gaps and discrimination.

## Data

| Sport | Sample | Observations |
|---|---|---|
| Tennis | Year-end top 50 ATP and WTA singles players | 700 player-years (697 with prize money) |
| Marathon | Top finishers at Boston (10), Chicago (5), London (12) and New York City (10), per gender | 468 runner-race observations |
| Triathlon | Elite Men and Elite Women, World Triathlon events (42 events) | 3,791 athlete-event observations |

- **Tennis:** annual prize money (USD) from ATP and WTA sources, plus performance variables built from historical tour-level match records (ranking, points, age, matches played and won, titles, Grand Slam wins).
- **Marathon:** finishing positions, times and athlete characteristics. Historical prize schedules are still being verified, so not every race has prize-money values yet.
- **Triathlon:** results collected from the World Triathlon API with Python. Prize schedules have not been added yet.

## Method

The main planned method is the **Oaxaca–Blinder decomposition** on top of OLS regression. It splits the mean earnings gap into an *explained* part (differences in observable characteristics) and an *unexplained* part (differences in coefficients and unobserved factors).

Tennis is the main application. Marathon and triathlon are analysed descriptively, since many events pay men and women identical amounts for the same placement.

## Preliminary results (tennis, descriptive only)

| | ATP | WTA |
|---|---:|---:|
| Mean annual prize money | $2.64M | $1.92M |
| Median annual prize money | $1.66M | $1.32M |
| Observations | 348 | 349 |

- Prize money is strongly right-skewed, so models will use log earnings.
- The gap in mean log earnings is positive in every year from 2019 to 2025.
- Higher-ranked players earn more, but earnings vary widely among players with similar rankings.

These are descriptive statistics, not causal estimates.

## Repository structure

```text
sports-prize-money-gender-gap/
├── Marathon/      # race datasets and R processing scripts
├── Rstudio/       # R analysis scripts and outputs
├── Tennis/        # ATP/WTA data construction and validation
├── Triathlon/     # World Triathlon data and Python cleaning scripts
├── Web Scraper/   # data-extraction scripts
├── .gitignore
└── README.md
```

## Tools

R and RStudio (cleaning, statistics, plots), Python (API extraction and validation), Git/GitHub.

## Data sources

ATP Tour, WTA Tour, World Triathlon, marathon race organisers, and the historical tennis match data maintained by Jeff Sackmann. Third-party data stay subject to their own attribution and redistribution terms.

## Author

**Andria Akhobadze**, Economics and Public Policy, Masaryk University


## Data Sources and References

### Data sources

- **ATP Tour:** Official ATP ranking, player statistics, and annual prize-money records. https://www.atptour.com/
- **WTA Tour:** Official WTA ranking and prize-money records. https://www.wtatennis.com/
- **Jeff Sackmann:** Historical ATP and WTA match datasets, accessed through the [tennis data archive](https://github.com/Aneeshers/tennis-sackmann-archive).
- **World Triathlon:** Elite competition results accessed through the World Triathlon API. https://triathlon.org/
- **Marathon results:** Boston Athletic Association, Chicago Marathon, London Marathon, and New York Road Runners, supplemented by other publicly available race-result records.

### Methodological references

- Blinder, A. S. (1973). Wage discrimination: Reduced form and structural estimates. *Journal of Human Resources, 8*(4), 436–455.
- Oaxaca, R. (1973). Male–female wage differentials in urban labor markets. *International Economic Review, 14*(3), 693–709.

### Data attribution and licensing

Historical tennis match data compiled by Jeff Sackmann are licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 (CC BY-NC-SA 4.0). These data are used with attribution, and derived data remain subject to applicable license conditions.

Other datasets are attributed to their original providers. Public availability does not necessarily authorize redistribution; permissions and applicable source terms should be checked before publishing datasets.

