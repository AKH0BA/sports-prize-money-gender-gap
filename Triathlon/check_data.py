
import pandas as pd

df = pd.read_csv("results_2019_2025.csv")

df["year"] = pd.to_datetime(df["event_date"]).dt.year

print("\nOBSERVATIONS BY YEAR:")
print(df.groupby("year").size())

print("\nPROGRAM TYPES:")
print(df["prog_name"].value_counts())

print("\nEVENTS BY YEAR:")
print(df.groupby("year")["event_id"].nunique())

print("\nGENDER DISTRIBUTION:")
print(df["athlete_gender"].value_counts(dropna=False))

print("\nFINISHING POSITION VALUES:")
print(df["position"].value_counts(dropna=False).head(20))

print("\nDUPLICATE RESULTS:")
print(df.duplicated(subset=["event_id", "prog_id", "athlete_id"]).sum())


print("\nPROGRAMS BY YEAR:")
print(
    df.groupby(["year", "prog_name"])["prog_id"]
      .nunique()
)

print("\nEVENT NAMES:")
print(
    df[["event_id", "event_title", "year"]]
      .drop_duplicates()
      .sort_values(["year", "event_title"])
      .to_string(index=False)
)