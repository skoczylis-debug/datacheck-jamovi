# messy_test_data.csv: answer key

60 rows, 14 variables. Row numbers are data rows as jamovi shows them (header excluded).

## Variable names
- `Participant ID`, `Age (years)`, `Victim of crime?`, `Date completed`: spaces, brackets, question mark
- `survey_wave`: constant (every value is 1)
- `notes_unused`: entirely empty

## Gender
| Row | Value | Issue |
|---|---|---|
| 3 | `male` | case variant of Male |
| 7 | `M` | abbreviation |
| 11 | ` Female` | leading space |
| 14 | `female ` | case + trailing space |
| 18 | `Femal` | typo |
| 22 | `F` | abbreviation |
| 25 | `MALE` | case variant |
| 29 | `N/A` | missing code as text |
| 33 | (blank) | missing |
| 37 | `Prefer not to say` | legitimate but rare level; decide whether it's missing |

## Age (years)
| Row | Value | Issue |
|---|---|---|
| 4, 40 | `-99` | sentinel code |
| 9 | `999` | sentinel code |
| 15 | `150` | out of range |
| 20 | `twenty` | text in numeric variable (forces whole column to text) |
| 27 | `34 ` | trailing space on a number |
| 31 | `0` | implausible for an adult survey |

## Region
| Row | Value | Issue |
|---|---|---|
| 5 | `north east` | case variant |
| 12 | `NE` | abbreviation |
| 19 | `london` | case variant |
| 24 | `London ` | trailing space |
| 35 | `Yorks` | abbreviation |
| 50 | `   ` | whitespace only (should be missing) |
| 51 | `NA` | missing code as text |

## Income
| Row | Value | Issue |
|---|---|---|
| 6 | `£25,000` | currency symbol and comma |
| 13 | `25k` | shorthand |
| 21 | `30000-35000` | range, not a value |
| 28 | `missing` | missing code as text |
| 36 | `.` | missing code (SPSS/Stata style) |
| 42 | `-` | missing code |

## Victim of crime?
| Row | Value | Issue |
|---|---|---|
| 8 | `yes` | case variant |
| 16 | `Y` | abbreviation |
| 23 | `no ` | trailing space |
| 30 | `N` | abbreviation |
| 38, 44 | `1`, `0` | numeric coding mixed with text labels |

## q1–q5_trust (1–5 scale)
| Row | Variable | Value | Issue |
|---|---|---|---|
| 10 | q1 | `6` | out of range |
| 17 | q2 | `0` | out of range |
| 26 | q3 | `9` | sentinel/out of range |
| 32 | q4 | `99` | sentinel code |
| 39 | q5 | `-99` | sentinel code |
| 45–49 | q3–q5 | (blank) | missing together: drop-off pattern |

## Date completed
| Row | Value | Issue |
|---|---|---|
| 2 | `01/03/2024` | different format |
| 34 | `1 March 2024` | different format |
| 41 | `2024-02-30` | impossible date |
| 43 | `03/13/2024` | US format (month 13 if read as UK) |

## Cases
- Rows 10, 52, 53 share `Participant ID` 1010; rows 52 and 53 are exact duplicates
- Row 54 is entirely empty
