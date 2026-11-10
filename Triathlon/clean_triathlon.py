
import pandas as pd

# Load API results
df = pd.read_csv("results_2019_2025.csv")

# Keep only final elite races
df = df[
    df["prog_name"].isin(["Elite Men", "Elite Women"])
].copy()

# Convert finishing ranks to numbers
# DNF, LAP, DNS and DSQ become missing
df["rank"] = pd.to_numeric(
    df["position"], errors="coerce"
)

# Keep ranked finishers only
df = df[df["rank"].notna() & (df["rank"] > 0)].copy()
df["rank"] = df["rank"].astype(int)

# Standardize variables
df["gender"] = df["prog_name"].map({
    "Elite Men": "M",
    "Elite Women": "F"
})

df["year"] = pd.to_datetime(
    df["event_date"]
).dt.year

df["finish_time_seconds"] = pd.to_timedelta(
    df["total_time"], errors="coerce"
).dt.total_seconds()

# Prepare econometric dataset
clean = df[[
    "event_id",
    "prog_id",
    "athlete_id",
    "event_title",
    "year",
    "gender",
    "athlete_title",
    "athlete_noc",
    "athlete_age",
    "rank",
    "finish_time_seconds"
]].copy()

clean = clean.rename(columns={
    "event_title": "event",
    "athlete_title": "athlete",
    "athlete_noc": "country",
    "athlete_age": "age"
})

clean = clean.sort_values(
    ["year", "event_id", "gender", "rank"]
)

# Check coverage
print("\nOBSERVATIONS BY YEAR AND GENDER:")
print(pd.crosstab(clean["year"], clean["gender"]))

print("\nNUMBER OF EVENTS:")
print(clean["event_id"].nunique())

print("\nMISSING FINISH TIMES:")
print(clean["finish_time_seconds"].isna().sum())

print("\nDUPLICATE RANKS:")
print(clean.duplicated(
    ["event_id", "prog_id", "rank"]
).sum())

# Export
clean.to_csv(
    "Triathlon_Elite_Cleaned.csv",
    index=False
)

print("\nSaved:", len(clean), "observations")


# Find duplicate finishing positions
duplicates = clean[
    clean.duplicated(
        subset=["event_id", "prog_id", "rank"],
        keep=False
    )
].sort_values(["year", "event_id", "gender", "rank"])

print("\nDUPLICATE RANK DETAILS:")
print(
    duplicates[
        ["year", "event", "gender", "athlete",
         "country", "rank", "finish_time_seconds"]
    ].to_string(index=False)
)
