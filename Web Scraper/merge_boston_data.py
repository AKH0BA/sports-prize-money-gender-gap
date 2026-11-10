import csv
from pathlib import Path
import re

import pandas as pd


DATA_DIR = Path("data")

# Files that contain participant-level data.
PARTICIPANT_FILES = {
    2019: ["results2019.csv"],
    2021: ["boston-marathon-2021-2024.csv"],
    2022: ["boston-marathon-2021-2024.csv"],
    2023: ["boston_2023.csv"],
    2024: ["boston_2024.csv"],
    2025: ["boston_2025.csv"],
}

# Your 2020 file contains metadata, not normal participant-level rows.
VIRTUAL_2020_FILE = DATA_DIR / "boston_2020_virtual_metadata.csv"


def clean_column_name(column):
    """Convert a column name to snake_case."""
    column = str(column).strip().lower()
    column = re.sub(r"[^a-z0-9]+", "_", column)
    return column.strip("_")


def standardize_columns(dataframe):
    """Standardize common column names from different sources."""
    dataframe = dataframe.copy()

    dataframe.columns = [
        clean_column_name(column)
        for column in dataframe.columns
    ]

    aliases = {
        "name": [
            "name",
            "fullname",
            "full_name",
            "runner_name",
            "participant",
            "sortname",
        ],
        "age": [
            "age",
            "ageonraceday",
            "age_on_race_day",
            "runner_age",
        ],
        "sex": [
            "sex",
            "gender",
            "m_f",
            "m_f_",
        ],
        "country": [
            "country",
            "countryofresname",
            "country_of_residence",
            "countryofresabbr",
            "country_abbreviation",
            "country_residence",
        ],
        "citizenship": [
            "citizenship",
            "countryofctzname",
            "country_of_citizenship",
            "countryofcitizenship",
            "contry_citizenship",
        ],
        "bib_number": [
            "bib",
            "bib_number",
            "bib_no",
            "bibnumber",
        ],
        "overall_place": [
            "overall",
            "overall_place",
            "place_overall",
            "rankoverall",
        ],
        "gender_place": [
            "gender_place",
            "place_gender",
            "rankovergender",
            "gender_rank",
            "gender_result",
        ],
        "division_place": [
            "division_place",
            "place_division",
            "rankoverdivision",
            "division_result",
        ],
        "finish_time": [
            "finish_time",
            "officialtime",
            "official_time",
            "officaltime",
            "finish_net",
            "finish_net_time",
            "net_time",
            "time",
        ],
        "finish_time_seconds": [
            "finish_time_seconds",
            "seconds",
        ],
        "age_group": [
            "age_group",
            "agegroup",
            "division",
            "subgroup",
            "subgrouplabel",
        ],
        "year": [
            "year",
            "race_year",
            "raceyear",
        ],
    }

    rename_map = {}

    for standard_name, possible_names in aliases.items():
        # Do not overwrite an existing standard column.
        if standard_name in dataframe.columns:
            continue

        for possible_name in possible_names:
            possible_name = clean_column_name(possible_name)

            if possible_name in dataframe.columns:
                rename_map[possible_name] = standard_name
                break

    return dataframe.rename(columns=rename_map)


def read_csv_robust(path):
    """
    Read a CSV while skipping malformed rows and reporting the count.

    Rows are validated against the header width because pandas' Python parser
    can otherwise tolerate extra fields without calling its bad-line handler.
    """
    skipped_rows = 0
    valid_rows = []

    with path.open(
        "r",
        encoding="utf-8-sig",
        newline="",
    ) as handle:
        reader = csv.reader(handle, strict=False)
        columns = next(reader)

        for row in reader:
            if len(row) != len(columns):
                skipped_rows += 1
                continue

            valid_rows.append(row)

    dataframe = pd.DataFrame(valid_rows, columns=columns)

    print(f"Columns in {path.name}:")
    print(list(dataframe.columns))

    if skipped_rows:
        print(
            f"WARNING: skipped {skipped_rows:,} malformed row(s) "
            f"while reading {path.name}"
        )

    return dataframe


def convert_time_to_seconds(value):
    """
    Convert time values such as:
    2:45:31 -> 9931 seconds
    45:31   -> 2731 seconds

    Numeric values are preserved because some datasets already
    store finish time in seconds.
    """
    if pd.isna(value):
        return pd.NA

    if isinstance(value, (int, float)):
        if value <= 0:
            return pd.NA
        return value

    value = str(value).strip().lower()

    if value in {
        "",
        "-",
        "--",
        "nan",
        "none",
        "null",
        "dnf",
        "dns",
        "dq",
    }:
        return pd.NA

    # Handle numeric strings.
    try:
        numeric_value = float(value)

        if numeric_value > 0:
            return numeric_value

    except ValueError:
        pass

    parts = value.split(":")

    try:
        if len(parts) == 2:
            minutes, seconds = map(int, parts)
            return minutes * 60 + seconds

        if len(parts) == 3:
            hours, minutes, seconds = map(int, parts)
            return hours * 3600 + minutes * 60 + seconds

    except ValueError:
        return pd.NA

    return pd.NA


def add_standard_metadata(dataframe, year, source_file):
    """Add consistent metadata to a yearly dataframe."""
    dataframe = dataframe.copy()

    dataframe["race"] = "Boston Marathon"
    dataframe["race_year"] = year

    if year == 2020:
        dataframe["race_status"] = "virtual"
    else:
        dataframe["race_status"] = "official_in_person"

    dataframe["source_file"] = source_file

    # Convert age where available.
    if "age" in dataframe.columns:
        dataframe["age"] = pd.to_numeric(
            dataframe["age"],
            errors="coerce",
        )

    # Create one consistent finish-time-seconds column.
    if "finish_time_seconds" in dataframe.columns:
        dataframe["finish_time_seconds"] = dataframe[
            "finish_time_seconds"
        ].apply(convert_time_to_seconds)

    elif "finish_time" in dataframe.columns:
        dataframe["finish_time_seconds"] = dataframe[
            "finish_time"
        ].apply(convert_time_to_seconds)

    else:
        dataframe["finish_time_seconds"] = pd.NA

    return dataframe


def get_year_column(dataframe):
    """Find the year column in a multi-year source file."""
    possible_columns = [
        "year",
        "race_year",
        "raceyear",
    ]

    for column in possible_columns:
        if column in dataframe.columns:
            return column

    return None


def load_2019():
    """Load the separate 2019 dataset."""
    path = DATA_DIR / "results2019.csv"

    if not path.exists():
        print(f"WARNING: missing file {path}")
        return None

    print(f"Loading {path}...")

    dataframe = read_csv_robust(path)

    dataframe = standardize_columns(dataframe)
    dataframe = add_standard_metadata(
        dataframe,
        year=2019,
        source_file=path.name,
    )

    return dataframe


def load_2021_or_2022(year):
    """
    Load the multi-year dataset and keep only the requested year.
    """
    path = DATA_DIR / "boston-marathon-2021-2024.csv"

    if not path.exists():
        print(f"WARNING: missing file {path}")
        return None

    print(f"Loading {path} for {year}...")

    dataframe = read_csv_robust(path)

    dataframe = standardize_columns(dataframe)

    year_column = get_year_column(dataframe)

    if year_column is None:
        print(
            "ERROR: Could not find a year column in "
            f"{path.name}."
        )
        print("Available columns:")
        print(list(dataframe.columns))
        return None

    # Convert year values to numeric where possible.
    dataframe[year_column] = pd.to_numeric(
        dataframe[year_column],
        errors="coerce",
    )

    dataframe = dataframe[
        dataframe[year_column] == year
    ].copy()

    if dataframe.empty:
        print(f"WARNING: no rows found for year {year}")
        return None

    dataframe = add_standard_metadata(
        dataframe,
        year=year,
        source_file=path.name,
    )

    return dataframe


def load_single_year(year, filename):
    """Load one of the official yearly CSV files."""
    path = DATA_DIR / filename

    if not path.exists():
        print(f"WARNING: missing file {path}")
        return None

    print(f"Loading {path}...")

    dataframe = read_csv_robust(path)

    dataframe = standardize_columns(dataframe)
    dataframe = add_standard_metadata(
        dataframe,
        year=year,
        source_file=path.name,
    )

    return dataframe


def load_2020_metadata():
    """
    Load the separate virtual-race metadata file.

    This normally contains one metadata row, not participant rows.
    """
    if not VIRTUAL_2020_FILE.exists():
        print(
            f"WARNING: missing file {VIRTUAL_2020_FILE}"
        )
        return None

    print(f"Loading {VIRTUAL_2020_FILE}...")

    dataframe = read_csv_robust(VIRTUAL_2020_FILE)

    dataframe = standardize_columns(dataframe)
    dataframe = add_standard_metadata(
        dataframe,
        year=2020,
        source_file=VIRTUAL_2020_FILE.name,
    )

    return dataframe


def reorder_columns(dataframe):
    """Put the most useful thesis columns first."""
    preferred_columns = [
        "race",
        "race_year",
        "race_status",
        "name",
        "sex",
        "age",
        "age_group",
        "country",
        "citizenship",
        "bib_number",
        "overall_place",
        "gender_place",
        "division_place",
        "finish_time",
        "finish_time_seconds",
        "source_file",
        "source_url",
    ]

    existing_preferred = [
        column
        for column in preferred_columns
        if column in dataframe.columns
    ]

    remaining_columns = [
        column
        for column in dataframe.columns
        if column not in existing_preferred
    ]

    return dataframe[
        existing_preferred + remaining_columns
    ]


def save_dataset(dataframe, filename):
    """Remove duplicates and save a CSV."""
    if dataframe is None or dataframe.empty:
        print(f"No data to save for {filename}")
        return None

    dataframe = dataframe.copy()

    before = len(dataframe)

    # Remove exact duplicate rows only.
    dataframe = dataframe.drop_duplicates()

    duplicates_removed = before - len(dataframe)

    dataframe = reorder_columns(dataframe)

    output_path = DATA_DIR / filename

    dataframe.to_csv(
        output_path,
        index=False,
    )

    print()
    print(f"Saved: {output_path}")
    print(f"Rows: {len(dataframe):,}")
    print(f"Duplicate rows removed: {duplicates_removed:,}")

    if "race_year" in dataframe.columns:
        print("Rows by year:")
        print(
            dataframe["race_year"]
            .value_counts()
            .sort_index()
        )

    return dataframe


def main():
    participant_dataframes = []

    # 2019 comes from results2019.csv.
    data_2019 = load_2019()

    if data_2019 is not None:
        participant_dataframes.append(data_2019)

    # 2021 and 2022 come from the multi-year file.
    for year in [2021, 2022]:
        dataframe = load_2021_or_2022(year)

        if dataframe is not None:
            participant_dataframes.append(dataframe)

    # 2023–2025 come from the official yearly files.
    official_files = {
        2023: "boston_2023.csv",
        2024: "boston_2024.csv",
        2025: "boston_2025.csv",
    }

    for year, filename in official_files.items():
        dataframe = load_single_year(year, filename)

        if dataframe is not None:
            participant_dataframes.append(dataframe)

    if not participant_dataframes:
        print("No participant datasets were found.")
        return

    participants = pd.concat(
        participant_dataframes,
        ignore_index=True,
        sort=False,
    )

    # This is the main thesis dataset.
    save_dataset(
        participants,
        "boston_2019_2025_all_participants.csv",
    )

    # Save the virtual 2020 metadata separately.
    virtual_2020 = load_2020_metadata()

    if virtual_2020 is not None:
        save_dataset(
            virtual_2020,
            "boston_2020_virtual_metadata_clean.csv",
        )

    print()
    print("Finished.")
    print()
    print(
        "The participant dataset excludes virtual 2020 "
        "participant rows because the available 2020 file "
        "contains metadata only."
    )


if __name__ == "__main__":
    main()