from pathlib import Path
import re
import time

import pandas as pd
from playwright.sync_api import sync_playwright


OUTPUT_DIR = Path("data/boston_prize_top10")
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

# Boston had no normal in-person prize-money race in 2020.
YEARS = [2019, 2021, 2022, 2023, 2024, 2025]

# The B.A.A. uses year-specific result systems.
RESULT_URLS = {
    2019: (
        "https://registration.baa.org/cfm_Archive/"
        "iframe_ArchiveSearch.cfm"
        "?RequestTimeout=600&mode=results&snap=55993107"
    ),
    2021: (
        "https://registration.baa.org/2021/cf/Media/"
        "iframe_ResultsSearch.cfm"
    ),
    2022: (
        "https://registration.baa.org/2022/cf/Media/"
        "iframe_ResultsSearch.cfm"
    ),
    2023: (
        "https://registration.baa.org/2023/cf/Media/"
        "iframe_ResultsSearch.cfm"
    ),
    2024: (
        "https://registration.baa.org/2024/cf/Media/"
        "iframe_ResultsSearch.cfm"
    ),
    2025: (
        "https://registration.baa.org/2025/cf/Media/"
        "iframe_ResultsSearch.cfm"
    ),
}


def clean_column_name(value):
    value = str(value).strip().lower()
    value = re.sub(r"[^a-z0-9]+", "_", value)
    return value.strip("_")


def parse_time_to_seconds(value):
    if pd.isna(value):
        return pd.NA

    value = str(value).strip()

    if value.lower() in {
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

    try:
        parts = value.split(":")

        if len(parts) == 2:
            minutes, seconds = map(int, parts)
            return minutes * 60 + seconds

        if len(parts) == 3:
            hours, minutes, seconds = map(int, parts)
            return hours * 3600 + minutes * 60 + seconds

    except ValueError:
        return pd.NA

    return pd.NA


def normalize_gender(value):
    value = str(value).strip().lower()

    if value in {"male", "men", "man", "m"}:
        return "Men"

    if value in {"female", "women", "woman", "f"}:
        return "Women"

    return value.title()


def extract_visible_tables(page):
    try:
        tables = pd.read_html(page.content())
    except ValueError:
        return []

    return [
        table
        for table in tables
        if len(table) > 0
    ]


def choose_best_results_table(tables):
    if not tables:
        return None

    best_table = None
    best_score = -1

    for table in tables:
        columns = " ".join(
            clean_column_name(column)
            for column in table.columns
        )

        score = 0

        for keyword in [
            "name",
            "bib",
            "finish",
            "time",
            "place",
            "citizenship",
            "gender",
        ]:
            if keyword in columns:
                score += 1

        # Prefer smaller top-results tables over navigation tables.
        if len(table) <= 25:
            score += 2

        if score > best_score:
            best_score = score
            best_table = table

    return best_table


def select_top_10(page):
    """
    Try to select 10 in the B.A.A. result-limit dropdown.
    """
    selectors = [
        "select",
        "select[name*='limit' i]",
        "select[id*='limit' i]",
        "select[name*='top' i]",
        "select[id*='top' i]",
    ]

    for selector in selectors:
        dropdowns = page.locator(selector)

        for index in range(dropdowns.count()):
            dropdown = dropdowns.nth(index)

            try:
                options = dropdown.locator("option").all_text_contents()
                options_text = " ".join(options).lower()

                if (
                    "10" in options_text
                    or "top 10" in options_text
                ):
                    try:
                        dropdown.select_option(label="10")
                        return True
                    except Exception:
                        try:
                            dropdown.select_option(value="10")
                            return True
                        except Exception:
                            pass

            except Exception:
                continue

    return False


def click_gender_button(page, gender):
    """
    Try several possible button/link formats used by B.A.A.
    """
    labels = {
        "Men": [
            "male",
            "men",
            "male 10",
            "men 10",
        ],
        "Women": [
            "female",
            "women",
            "female 10",
            "women 10",
        ],
    }

    for label in labels[gender]:
        selectors = [
            f"text={label}",
            f"button:has-text('{label}')",
            f"a:has-text('{label}')",
            f"input[value*='{label}' i]",
        ]

        for selector in selectors:
            locator = page.locator(selector).first

            try:
                if locator.is_visible():
                    locator.click()
                    return True
            except Exception:
                continue

    return False


def click_csv_format(page):
    """
    Try to choose CSV output where the page exposes a format option.
    """
    selectors = [
        "text=csv download",
        "text=CSV download",
        "label:has-text('csv')",
        "input[value*='csv' i]",
        "option:has-text('csv')",
    ]

    for selector in selectors:
        locator = page.locator(selector).first

        try:
            if locator.is_visible():
                tag_name = locator.evaluate(
                    "(element) => element.tagName"
                )

                if tag_name.lower() == "option":
                    parent = locator.locator("..")
                    parent.select_option(
                        label=locator.inner_text()
                    )
                else:
                    locator.click()

                return True

        except Exception:
            continue

    return False


def click_search_or_submit(page):
    selectors = [
        "input[type='submit']",
        "button[type='submit']",
        "button:has-text('search')",
        "button:has-text('submit')",
        "input[value*='search' i]",
        "input[value*='submit' i]",
        "text=Search",
        "text=Submit",
    ]

    for selector in selectors:
        locator = page.locator(selector).first

        try:
            if locator.is_visible():
                locator.click()
                page.wait_for_timeout(4000)
                return True

        except Exception:
            continue

    return False


def add_metadata(dataframe, year, gender, source_url):
    dataframe = dataframe.copy()

    dataframe.columns = [
        clean_column_name(column)
        for column in dataframe.columns
    ]

    dataframe.insert(0, "race", "Boston Marathon")
    dataframe.insert(1, "race_year", year)
    dataframe.insert(2, "gender_requested", gender)
    dataframe["source_url"] = source_url
    dataframe["source_type"] = "official_baa_results_page"

    # Normalize common result columns.
    rename_map = {}

    aliases = {
        "name": [
            "name",
            "runner_name",
            "participant",
            "fullname",
        ],
        "citizenship": [
            "citizenship",
            "country",
            "country_of_citizenship",
        ],
        "bib_number": [
            "bib",
            "bib_number",
            "bib_no",
        ],
        "finish_time": [
            "finish_net",
            "finish_time",
            "official_time",
            "officialtime",
            "time",
        ],
        "overall_place": [
            "place_overall",
            "overall_place",
            "overall",
            "rankoverall",
        ],
        "gender_place": [
            "place_gender",
            "gender_place",
            "rankovergender",
        ],
        "division_place": [
            "place_division",
            "division_place",
            "rankoverdivision",
        ],
    }

    for standard_name, possible_names in aliases.items():
        for possible_name in possible_names:
            possible_name = clean_column_name(possible_name)

            if possible_name in dataframe.columns:
                rename_map[possible_name] = standard_name
                break

    dataframe = dataframe.rename(columns=rename_map)

    if "finish_time" in dataframe.columns:
        dataframe["finish_time_seconds"] = dataframe[
            "finish_time"
        ].apply(parse_time_to_seconds)

    return dataframe


def scrape_year_gender(page, year, gender):
    url = RESULT_URLS[year]

    print(f"Opening Boston {year} page for {gender}...")

    page.goto(
        url,
        wait_until="domcontentloaded",
        timeout=120000,
    )

    page.wait_for_timeout(3000)

    print(f"Loaded: {page.url}")
    print(f"Title: {page.title()}")

    # Save the page before interacting for troubleshooting.
    debug_prefix = OUTPUT_DIR / f"debug_{year}_{gender.lower()}"

    page.screenshot(
        path=str(debug_prefix) + ".png",
        full_page=True,
    )

    Path(str(debug_prefix) + ".html").write_text(
        page.content(),
        encoding="utf-8",
    )

    selected_limit = select_top_10(page)

    if selected_limit:
        print("Selected top-10 result limit.")
    else:
        print(
            "Could not automatically select top 10; "
            "continuing with page defaults."
        )

    selected_gender = click_gender_button(page, gender)

    if selected_gender:
        print(f"Selected {gender}.")
    else:
        print(
            f"Could not automatically select {gender}; "
            "continuing with page defaults."
        )

    click_csv_format(page)

    submitted = click_search_or_submit(page)

    if submitted:
        print("Submitted results search.")
    else:
        print(
            "Could not find a submit button. "
            "Trying the currently visible table."
        )

    page.wait_for_timeout(4000)

    tables = extract_visible_tables(page)
    dataframe = choose_best_results_table(tables)

    if dataframe is None:
        print(
            f"No results table found for {year} {gender}. "
            f"Check {debug_prefix}.html and .png"
        )
        return None

    dataframe = add_metadata(
        dataframe,
        year=year,
        gender=gender,
        source_url=page.url,
    )

    # Keep only the first 10 rows if the page returned more.
    if len(dataframe) > 10:
        dataframe = dataframe.head(10)

    output_file = OUTPUT_DIR / (
        f"boston_{year}_{gender.lower()}_top10.csv"
    )

    dataframe.to_csv(
        output_file,
        index=False,
    )

    print(
        f"Saved {len(dataframe)} rows to {output_file}"
    )

    return dataframe


def main():
    all_results = []

    with sync_playwright() as playwright:
        browser = playwright.chromium.launch(
            headless=False,
            slow_mo=250,
        )

        context = browser.new_context(
            accept_downloads=True,
        )

        page = context.new_page()

        try:
            for year in YEARS:
                for gender in ["Men", "Women"]:
                    try:
                        dataframe = scrape_year_gender(
                            page,
                            year,
                            gender,
                        )

                        if dataframe is not None:
                            all_results.append(dataframe)

                    except Exception as error:
                        print(
                            f"ERROR processing "
                            f"{year} {gender}: {error}"
                        )

                    time.sleep(2)

        finally:
            browser.close()

    if not all_results:
        print("No result tables were collected.")
        return

    combined = pd.concat(
        all_results,
        ignore_index=True,
        sort=False,
    )

    combined_file = OUTPUT_DIR / (
        "boston_2019_2025_prize_top10_raw.csv"
    )

    combined.to_csv(
        combined_file,
        index=False,
    )

    print()
    print(f"Saved combined file: {combined_file}")
    print(f"Total rows: {len(combined)}")
    print()
    print(
        combined.groupby(
            ["race_year", "gender_requested"]
        ).size()
    )

