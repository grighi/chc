#!/usr/bin/env Rscript
# geocode_facilities.R
#
# Reads the parsed 2017 and 2018 facility CSVs, geocodes each facility to its
# county via ZIP-code lookup (no API calls required), and prints a data.table
# of counties that gained their first facility between 2017 and 2018.
#
# Usage:
#   Rscript geocode_facilities.R
#
# Inputs:  facilities_2017.csv, facilities_2018.csv  (in working directory)
# Outputs: facilities_geocoded.rds                   (stacked, geocoded data)

library(data.table)
library(zipcodeR)

# ---------- 1. Read & stack --------------------------------------------------

dt17 <- fread("facilities_2017.csv")[, year := 2017L]
dt18 <- fread("facilities_2018.csv")[, year := 2018L]
dt19 <- fread("facilities_2019.csv")[, year := 2019L]
dt20 <- fread("facilities_2020.csv")[, year := 2020L]
dt   <- rbindlist(list(dt17, dt18, dt19, dt20), fill = TRUE)

cat(sprintf("Loaded %d facilities (%d from 2017, %d from 2018, %d from 2019, %d from 2020)\n",
            nrow(dt), nrow(dt17), nrow(dt18), nrow(dt19), nrow(dt20)))

# ---------- 2. Extract 5-digit ZIP from address ------------------------------

dt[, zip5 := sub(".*\\b(\\d{5})\\b.*", "\\1", address)]
dt[!grepl("\\d{5}", address), zip5 := NA_character_]

cat(sprintf("ZIP extracted: %d valid, %d missing\n",
            sum(!is.na(dt$zip5)), sum(is.na(dt$zip5))))

# ---------- 3. ZIP -> county / state / lat / lng via zipcodeR ----------------

zips     <- unique(na.omit(dt$zip5))
zip_info <- as.data.table(reverse_zipcode(zips))

# Keep the columns we need; rename for clarity
zip_info <- zip_info[, .(zipcode, county, state_abb = state, lat, lng)]

# Merge onto facilities
dt <- merge(dt, zip_info, by.x = "zip5", by.y = "zipcode", all.x = TRUE)

cat(sprintf("Geocoded to county: %d matched, %d unmatched\n",
            sum(!is.na(dt$county)), sum(is.na(dt$county))))

# ---------- 4. Add county FIPS -----------------------------------------------
#   tigris::fips_codes gives us state (2-char), county_code (3-digit), county name

fips_raw <- as.data.table(tigris::fips_codes)
fips_raw[, county_fips := paste0(state_code, county_code)]

# zipcodeR returns county as e.g. "Los Angeles County"; fips_codes has "Los Angeles County"
# Normalise to lower for a safe join
fips_raw[, county_lc := tolower(county)]
dt[,       county_lc := tolower(county)]

dt <- merge(dt,
            fips_raw[, .(state, county_lc, county_fips)],
            by.x  = c("state_abb", "county_lc"),
            by.y  = c("state",     "county_lc"),
            all.x = TRUE)
dt[, county_lc := NULL]

cat(sprintf("FIPS matched: %d rows\n", sum(!is.na(dt$county_fips))))

# ---------- 5. Counties that appear in 2018 but NOT in 2017 ------------------

counties17 <- unique(dt[year == 2017 & !is.na(county_fips),
                        .(county_fips, county, state_abb)])
counties18 <- unique(dt[year == 2018 & !is.na(county_fips),
                        .(county_fips, county, state_abb)])
counties19 <- unique(dt[year == 2019 & !is.na(county_fips),
                        .(county_fips, county, state_abb)])
counties20 <- unique(dt[year == 2020 & !is.na(county_fips),
                        .(county_fips, county, state_abb)])

new_in_2018 <- fsetdiff(counties18, counties17)
new_in_2019 <- fsetdiff(counties19, counties18)
new_in_2020 <- fsetdiff(counties20, counties19)
setorder(new_in_2018, state_abb, county)
setorder(new_in_2019, state_abb, county)
setorder(new_in_2020, state_abb, county)

cat("\n=== Counties with a facility in 2018 but NOT in 2017 ===\n\n")
print(new_in_2018, nrows = Inf)
cat(sprintf("\nTotal new counties: %d\n", nrow(new_in_2018)))

# cat("\n=== Counties with a facility in 2019 but NOT in 2018 ===\n\n")
# print(new_in_2019, nrows = Inf)
# cat(sprintf("\nTotal new counties: %d\n", nrow(new_in_2019)))

# cat("\n=== Counties with a facility in 2020 but NOT in 2019 ===\n\n")
# print(new_in_2020, nrows = Inf)
# cat(sprintf("\nTotal new counties: %d\n", nrow(new_in_2020)))

# ---------- 6. Save -----------------------------------------------------------

saveRDS(dt, "facilities_geocoded.rds")
cat("\nSaved facilities_geocoded.rds\n")
