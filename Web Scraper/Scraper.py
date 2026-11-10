from pathlib import Path
from io import BytesIO
import re
import time

import pandas as pd
import requests
from playwright.sync_api import sync_playwright


OUTPUT_DIR = Path("data")
OUTPUT_DIR.mkdir(exist_ok=True)

HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
        "AppleWebKit/537.36 "
        "Chrome/131.0.0.0 Safari/537.36"
    )
}

YEAR_STATUS = {
    2019: "official_in_person",
    2020: "virtual",
    2021: "official_in_person",
    2022: "official_in_person",
    2023: "official_in_person",
    2024: "official_in_person",
    2025: "official_in_person",
}

OLD_RESULTS_URLS = {
    2019: (
        "https://registration.baa.org/cfm_Archive/"
        "iframe_ArchiveSearch.cfm"
        "?RequestTimeout=600&mode=results&snap=55993107"
    ),
    2021: (
        "https://registration.baa.org/2021/cf/Public/"
        "iframe_ResultsSearch.cfm"
    ),
    2022: (
        "https://registration.baa.org/2022/cf/Public/"
        "iframe_ResultsSearch.cfm"
    ),
}


def clean_column_name(value):
    value = str(value).strip().lower()
    value = re.sub(r"[^a-z0-9]+", "_", value)
    return value.strip("_")


def parse_time_to_seconds(value):
    if pd.isna(value):
        return None

    value = str(value).strip()

    if value in {"", "-", "--", "nan", "None"}:
        return None

    parts = value.split(":")

    try:
        if len(parts) == 2:
            minutes, seconds = map(int, parts)
            return minutes * 60 + seconds

        if len(parts) == 3:
            hours, minutes, seconds = map(int, parts)
            return hours * 3600 + minutes * 60 + seconds

    except ValueError:
        return None

    return None


def normalize_dataframe(dataframe, year, source_url):
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
        "bib_number": [
            "bib",
            "bib_number",
            "bib_no",
            "bibnumber",
        ],
        "age": [
            "age",
            "ageonraceday",
            "age_on_race_day",
        ],
        "sex": [
            "sex",
            "gender",
            "m_f",
        ],
        "country": [
            "country",
            "countryofresname",
            "country_of_residence",
            "countryofresabbr",
        ],
        "citizenship": [
            "citizenship",
            "countryofctzname",
            "country_of_citizenship",
        ],
        "city": ["city"],
        "state": ["state", "stateabbrev"],
        "overall_place": [
            "overall",
            "overall_place",
            "rankoverall",
        ],
        "gender_place": [
            "gender_place",
            "rankovergender",
        ],
        "division_place": [
            "division_place",
            "rankoverdivision",
        ],
        "finish_time": [
            "finish_time",
            "officialtime",
            "official_time",
            "officaltime",
            "net_time",
        ],
        "age_group": [
            "age_group",
            "division",
            "subgroup",
            "subgrouplabel",
        ],
    }

    rename_map = {}

    for standard_name, aliases_for_field in aliases.items():
        for alias in aliases_for_field:
            alias = clean_column_name(alias)

            if alias in dataframe.columns:
                rename_map[alias] = standard_name
                break

    dataframe = dataframe.rename(columns=rename_map)

    dataframe.insert(0, "race", "Boston Marathon")
    dataframe.insert(1, "race_year", year)
    dataframe.insert(2, "race_status", YEAR_STATUS[year])
    dataframe["source_url"] = source_url

    if "age" in dataframe.columns:
        dataframe["age"] = pd.to_numeric(
            dataframe["age"],
            errors="coerce",
        )

    if "finish_time" in dataframe.columns:
        dataframe["finish_time_seconds"] = dataframe[
            "finish_time"
        ].apply(parse_time_to_seconds)

    return dataframe


def choose_results_table(tables):
    if not tables:
        return None

    best_table = None
    best_score = -1

    for table in tables:
        if len(table) < 10:
            continue

        column_text = " ".join(
            clean_column_name(column)
            for column in table.columns
        )

        score = 0

        for keyword in [
            "name",
            "bib",
            "gender",
            "age",
            "time",
            "country",
            "place",
        ]:
            if keyword in column_text:
                score += 1

        # Prefer large participant tables.
        score += min(len(table) / 10000, 5)

        if score > best_score:
            best_score = score
            best_table = table

    return best_table


def save_debug_files(page, year):
    html_path = OUTPUT_DIR / f"debug_{year}.html"
    screenshot_path = OUTPUT_DIR / f"debug_{year}.png"

    html_path.write_text(
        page.content(),
        encoding="utf-8",
    )

    page.screenshot(
        path=str(screenshot_path),
        full_page=True,
    )

    print(f"Saved debug files:")
    print(f"  {html_path}")
    print(f"  {screenshot_path}")


def scrape_old_year(year):
    url = OLD_RESULTS_URLS[year]

    print(f"Opening official Boston {year} results page...")

    with sync_playwright() as playwright:
        browser = playwright.chromium.launch(
            headless=False,
            slow_mo=250,
)

        context = browser.new_context(
            user_agent=HEADERS["User-Agent"],
            accept_downloads=True,
        )

        page = context.new_page()

        try:
            page.goto(
                url,
                wait_until="domcontentloaded",
                timeout=120000,
            )

            page.wait_for_load_state("networkidle", timeout=60000)

            print(f"Loaded URL: {page.url}")
            print(f"Page title: {page.title()}")

            save_debug_files(page, year)

            # Print visible page text so we can diagnose the layout.
            visible_text = page.locator("body").inner_text()
            print(visible_text[:2500])

            # Try to click a CSV/download option.
            download_selectors = [
                "text=csv download",
                "text=CSV download",
                "text=download",
                "text=Download",
                "a[href*='csv']",
                "a[href*='CSV']",
            ]

            for selector in download_selectors:
                try:
                    locator = page.locator(selector).first

                    if await_is_visible(locator):
                        print(f"Found possible download control: {selector}")

                        with page.expect_download(
                            timeout=15000
                        ) as download_info:
                            locator.click()

                        download = download_info.value
                        target = OUTPUT_DIR / (
                            f"boston_{year}_download"
                            + Path(download.suggested_filename).suffix
                        )

                        download.save_as(str(target))

                        print(f"Downloaded: {target}")

                        if target.suffix.lower() == ".csv":
                            dataframe = pd.read_csv(
                                target,
                                encoding_errors="ignore",
                                on_bad_lines="skip",
                            )

                            if len(dataframe) > 10:
                                return normalize_dataframe(
                                    dataframe,
                                    year,
                                    page.url,
                                )

                except Exception:
                    continue

            # Try to extract an HTML table.
            try:
                tables = pd.read_html(page.content())
            except Exception:
                tables = []

            dataframe = choose_results_table(tables)

            if dataframe is not None:
                print(
                    f"Found HTML table with {len(dataframe):,} rows."
                )

                return normalize_dataframe(
                    dataframe,
                    year,
                    page.url,
                )

            print(
                f"No participant table detected for {year}. "
                "The debug files show what the page returned."
            )

        finally:
            browser.close()

    return None


def await_is_visible(locator):
    try:
        return locator.is_visible()
    except Exception:
        return False


def create_2020_metadata():
    return pd.DataFrame(
        [
            {
                "race": "Boston Marathon",
                "race_year": 2020,
                "race_status": "virtual",
                "name": None,
                "age": None,
                "sex": None,
                "country": None,
                "finish_time": None,
                "finish_time_seconds": None,
                "source_url": (
                    "https://www.baa.org/races/boston-marathon/history/"
                ),
                "notes": (
                    "The 2020 Boston Marathon was virtual. "
                    "It is not comparable to the in-person races."
                ),
            }
        ]
    )


def main():
    all_data = []

    for year in [2019, 2021, 2022]:
        try:
            dataframe = scrape_old_year(year)

            if dataframe is not None:
                output_file = OUTPUT_DIR / f"boston_{year}.csv"

                dataframe.to_csv(
                    output_file,
                    index=False,
                )

                print(
                    f"Saved {len(dataframe):,} rows to {output_file}"
                )

                all_data.append(dataframe)

        except Exception as error:
            print(f"ERROR processing {year}: {error}")

        time.sleep(2)

    # Keep 2020 separate because it was virtual.
    virtual_2020 = create_2020_metadata()

    virtual_2020.to_csv(
        OUTPUT_DIR / "boston_2020_virtual_metadata.csv",
        index=False,
    )

    all_data.append(virtual_2020)

    # Load the files you already obtained.
    for year in [2023, 2024, 2025]:
        path = OUTPUT_DIR / f"boston_{year}.csv"

        if path.exists():
            print(f"Loading existing file: {path}")

            dataframe = pd.read_csv(
                path,
                low_memory=False,
            )

            all_data.append(dataframe)

    if not all_data:
        print("No data collected.")
        return

    combined = pd.concat(
        all_data,
        ignore_index=True,
        sort=False,
    )

    combined_path = OUTPUT_DIR / "boston_2019_2025_combined.csv"

    combined.to_csv(
        combined_path,
        index=False,
    )

    print()
    print(f"Saved combined dataset: {combined_path}")
    print(f"Total rows: {len(combined):,}")
    print()
    print("Rows by year:")

    if "race_year" in combined.columns:
        print(
            combined["race_year"]
            .value_counts()
            .sort_index()
        )


if __name__ == "__main__":
    main()