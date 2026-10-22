# Data Check

A jamovi module that helps students clean raw data before they analyse it, instead of doing it by hand in Excel.

Data Check reads the data and never changes the original columns. It reports what's wrong, lets the student decide what each value should become, and then creates cleaned variables or a separate cleaned dataset.

## How it works

**Before you start (in the spreadsheet).** Look at the data, delete rows and columns that don't belong, and rename columns in Setup.

**Step 1: Report.** For each selected variable:

- an overview: rows, missing cells, duplicate and empty rows
- alerts in plain English, each with a short explanation:
  - inconsistent spellings and capitals
  - stray spaces
  - missing-value codes (-99, 999, N/A)
  - numbers stored as text
  - garbled characters
  - impossible dates
  - rare values, out-of-range values and extreme values
  - duplicate IDs
- an optional missing-data report: a map of the gaps, Little's MCAR test, and which variables each variable's missingness is linked to
- optional charts and a check for near-duplicate variables

**Step 2: Decide.** The options panel lists each variable's values with a suggested replacement, for example *F → Female*, *Yorks → Yorkshire*, *twenty → 20*. Students accept or edit each one, type *missing* to drop a value, set a valid range for numbers, and say what blank cells mean. A blank can be *missing* or a category such as *Not Asian* for a follow-up question. The results show exactly what will happen to every value and why.

**Step 3: Create.** Either add cleaned columns to the spreadsheet, or open the cleaned data as a new dataset (or a CSV) with tidy names. Duplicate and empty rows can be removed, and survey admin columns left out. Both are blocked while any value is still marked **?**.

## Survey exports

Data Check recognises common features of Qualtrics, Microsoft Forms, Google Forms and JISC Online Surveys exports:

- extra header rows (question text, import codes)
- admin columns such as start and end times, progress, IP address and email, with personal-data warnings
- quiz columns
- "select all that apply" tick-box columns: blank = not ticked, converted to 1/0
- "please specify" and other open-text answers
- rating scales written as words (*Completely disagree … Completely agree*), which become correctly ordered ordinal variables

## Requirements

jamovi 28.4 or newer.

## Test data

The `data` folder contains:

- `messy_test_data.csv`: 60 rows with planted problems. `messy_test_data_answer_key.md` lists every one.
- `qualtrics_test.csv`: a survey-style export with header rows, admin columns, a tick-box set, a follow-up question and -99 codes.

## Installing from source

```r
install.packages("jmvtools", repos = c("https://repo.jamovi.org", "https://cran.r-project.org"))
jmvtools::install()   # run in this folder with jamovi open
```

## Author

Joshua Skoczylis, University of Lincoln. Licence: GPL (>= 2).
