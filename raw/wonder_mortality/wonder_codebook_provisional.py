"""
CDC WONDER Provisional Mortality Statistics, 2018 through Last Week (Dataset D176)
Codebook: maps human-readable labels → API parameter values.

Sources: Request Form saved from https://wonder.cdc.gov/controller/datarequest/D176
"""

# ---------------------------------------------------------------------------
# GROUP-BY dimensions  (B_1 … B_5 parameters)
# ---------------------------------------------------------------------------
GROUP_BY = {
    # --- Residence Location ---
    "census_region":        "D176.V10-level1",
    "census_division":      "D176.V10-level2",
    "hhs_region":           "D176.V27-level1",
    "state":                "D176.V9-level1",
    "county":               "D176.V9-level2",
    "urbanization_2013":    "D176.V19",
    "urbanization_2006":    "D176.V11",
    "urbanization_2023":    "D176.V18",
    # --- Occurrence Location ---
    "occ_census_region":    "D176.V80-level1",
    "occ_census_division":  "D176.V80-level2",
    "occ_hhs_region":       "D176.V77-level1",
    "occ_state":            "D176.V79-level1",
    "occ_county":           "D176.V79-level2",
    "occ_urbanization_2013":"D176.V89",
    "occ_urbanization_2006":"D176.V81",
    "occ_urbanization_2023":"D176.V82",
    # --- Demographics ---
    "age_10yr":             "D176.V5",
    "age_5yr":              "D176.V51",
    "age_single":           "D176.V52",
    "age_infant":           "D176.V6",
    "sex":                  "D176.V7",
    "hispanic_origin":      "D176.V17",
    "race":                 "D176.V42",       # Single Race 6 (default)
    "race_15":              "D176.V43",       # Single Race 15
    "race_31":              "D176.V44",       # Single/Multi Race 31
    # --- Year / Month ---
    "year":                 "D176.V1-level1",
    "month":                "D176.V1-level2",
    # --- MMWR Year / Week ---
    "mmwr_year":            "D176.V100-level1",
    "mmwr_week":            "D176.V100-level2",
    # --- Autopsy / Place ---
    "autopsy":              "D176.V20",
    "place_of_death":       "D176.V21",
    # --- Underlying Cause of Death (UCD) ---
    "ucd_15_leading":           "D176.V28",
    "ucd_15_leading_infants":   "D176.V29",
    "ucd_icd_chapter":          "D176.V2-level1",
    "ucd_icd_subchapter":       "D176.V2-level2",
    "ucd_cause":                "D176.V2-level3",
    "ucd_113_cause_list":       "D176.V4",
    "ucd_130_cause_list_infant":"D176.V12",
    "ucd_injury_intent":        "D176.V22",
    "ucd_injury_mechanism":     "D176.V23",
    "ucd_drug_alcohol":         "D176.V25-level1",
    "ucd_drug_alcohol_cause":   "D176.V25-level2",
    # --- Multiple Cause of Death (MCD) ---
    "mcd_icd_chapter":          "D176.V13-level1",
    "mcd_icd_subchapter":       "D176.V13-level2",
    "mcd_cause":                "D176.V13-level3",
    "mcd_113_cause_list":       "D176.V15",
    "mcd_130_cause_list_infant":"D176.V16",
    "mcd_drug_alcohol":         "D176.V26-level1",
    "mcd_drug_alcohol_cause":   "D176.V26-level2",
    # --- Sentinel ---
    "none":             "*None*",
}

# ---------------------------------------------------------------------------
# SEX  (V_D176.V7)
# ---------------------------------------------------------------------------
SEX = {
    "all":    "*All*",
    "female": "F",
    "male":   "M",
}

# ---------------------------------------------------------------------------
# HISPANIC ORIGIN  (V_D176.V17)
# ---------------------------------------------------------------------------
HISPANIC_ORIGIN = {
    "all":               "*All*",
    "hispanic":          "2135-2",
    "non_hispanic":      "2186-2",
    "not_stated":        "NS",
}

# ---------------------------------------------------------------------------
# RACE — Single Race 6  (V_D176.V42, default)
# ---------------------------------------------------------------------------
RACE = {
    "all":              "*All*",
    "aian":             "1002-5",   # American Indian or Alaska Native
    "asian":            "A",        # Asian
    "black":            "2054-5",   # Black or African American
    "nhopi":            "NHOPI",    # Native Hawaiian or Other Pacific Islander
    "white":            "2106-3",
    "more_than_one":    "M",        # More than one race
    "not_available":    "999",
}

# Single Race 15  (V_D176.V43)
RACE_15 = {
    "all":  "*All*",
    **{str(i): str(i) for i in range(1, 16)},
    "not_available": "999",
}

# Single/Multi Race 31  (V_D176.V44)
RACE_31 = {
    "all":  "*All*",
    **{str(i): str(i) for i in range(1, 32)},
    "not_available": "999",
}

# ---------------------------------------------------------------------------
# AGE GROUPS — Ten-Year  (V_D176.V5)
# ---------------------------------------------------------------------------
AGE_10YR = {
    "all":      "*All*",
    "<1":       "1",
    "1-4":      "1-4",
    "5-14":     "5-14",
    "15-24":    "15-24",
    "25-34":    "25-34",
    "35-44":    "35-44",
    "45-54":    "45-54",
    "55-64":    "55-64",
    "65-74":    "65-74",
    "75-84":    "75-84",
    "85+":      "85+",
    "not_stated": "NS",
}

# ---------------------------------------------------------------------------
# AGE GROUPS — Five-Year  (V_D176.V51)
# ---------------------------------------------------------------------------
AGE_5YR = {
    "all":      "*All*",
    "<1":       "1",
    "1-4":      "1-4",
    "5-9":      "5-9",
    "10-14":    "10-14",
    "15-19":    "15-19",
    "20-24":    "20-24",
    "25-29":    "25-29",
    "30-34":    "30-34",
    "35-39":    "35-39",
    "40-44":    "40-44",
    "45-49":    "45-49",
    "50-54":    "50-54",
    "55-59":    "55-59",
    "60-64":    "60-64",
    "65-69":    "65-69",
    "70-74":    "70-74",
    "75-79":    "75-79",
    "80-84":    "80-84",
    "85-89":    "85-89",
    "90-94":    "90-94",
    "95-99":    "95-99",
    "100+":     "100+",
    "not_stated": "NS",
}

# ---------------------------------------------------------------------------
# INFANT AGE GROUPS  (V_D176.V6)
# ---------------------------------------------------------------------------
AGE_INFANT = {
    "all":      "00",
    "<1d":      "1d",
    "1-6d":     "1-6d",
    "7-27d":    "7-27d",
    "28-364d":  "28-364d",
}

# ---------------------------------------------------------------------------
# YEARS  (F_D176.V1)   — provisional data starts in 2018
# ---------------------------------------------------------------------------
YEARS = {
    "all": "*All*",
    **{str(y): str(y) for y in range(2018, 2027)},
}

# ---------------------------------------------------------------------------
# MCD / UCD ICD-10 CHAPTERS  (F_D176.V13 / F_D176.V2)
# ---------------------------------------------------------------------------
ICD_CHAPTERS = {
    "all":              "*All*",
    "infectious":       "A00-B99",
    "neoplasms":        "C00-D48",
    "blood":            "D50-D89",
    "endocrine":        "E00-E89",
    "mental":           "F01-F99",
    "nervous":          "G00-G98",
    "eye":              "H00-H59",
    "ear":              "H60-H95",
    "circulatory":      "I00-I99",
    "respiratory":      "J00-J98",
    "digestive":        "K00-K92",
    "skin":             "L00-L98",
    "musculoskeletal":  "M00-M99",
    "genitourinary":    "N00-N99",
    "pregnancy":        "O00-O99",
    "perinatal":        "P00-P96",
    "congenital":       "Q00-Q99",
    "symptoms":         "R00-R99",
    "injury":           "S00-T98",
    "special":          "U00-U99",
    "external":         "V01-Y89",
}

# Convenience alias: all non-external ICD chapters
NON_EXTERNAL_CHAPTERS = [
    v for k, v in ICD_CHAPTERS.items()
    if k not in ("all", "external", "injury")
]

# ---------------------------------------------------------------------------
# LOCATION — States  (F_D176.V9)
# ---------------------------------------------------------------------------
STATES = {
    "all": "*All*",
    "AL": "01", "AK": "02", "AZ": "04", "AR": "05", "CA": "06",
    "CO": "08", "CT": "09", "DE": "10", "DC": "11", "FL": "12",
    "GA": "13", "HI": "15", "ID": "16", "IL": "17", "IN": "18",
    "IA": "19", "KS": "20", "KY": "21", "LA": "22", "ME": "23",
    "MD": "24", "MA": "25", "MI": "26", "MN": "27", "MS": "28",
    "MO": "29", "MT": "30", "NE": "31", "NV": "32", "NH": "33",
    "NJ": "34", "NM": "35", "NY": "36", "NC": "37", "ND": "38",
    "OH": "39", "OK": "40", "OR": "41", "PA": "42", "RI": "44",
    "SC": "45", "SD": "46", "TN": "47", "TX": "48", "UT": "49",
    "VT": "50", "VA": "51", "WA": "53", "WV": "54", "WI": "55",
    "WY": "56",
}

# ---------------------------------------------------------------------------
# CENSUS REGIONS  (F_D176.V10)
# ---------------------------------------------------------------------------
CENSUS_REGIONS = {
    "all":       "*All*",
    "northeast": "CENS-R1",
    "midwest":   "CENS-R2",
    "south":     "CENS-R3",
    "west":      "CENS-R4",
}

# ---------------------------------------------------------------------------
# LOCATION RADIO BUTTON  (O_location — residence)
# ---------------------------------------------------------------------------
LOCATION_RADIO = {
    "state":          "D176.V9",
    "census_region":  "D176.V10",
    "hhs_region":     "D176.V27",
}

# ---------------------------------------------------------------------------
# DEATH LOCATION RADIO BUTTON  (O_death_location — occurrence)
# ---------------------------------------------------------------------------
DEATH_LOCATION_RADIO = {
    "state":          "D176.V79",
    "census_region":  "D176.V80",
    "hhs_region":     "D176.V77",
}
