#!/usr/bin/env Rscript
# build_nhgis_controls.R — Build 1970 Census county controls from NHGIS extract
#
# Produces: nhgis_1970_controls.fst
#   fips, pct_lt_hs, pct_hs, unemp_rate, lfp_rate, pct_urban
#
# Sources:
#   ds94: Persons in Urbanized Areas (CBG001) — 100% count
#   ds98: Years of School Completed (C06001-C06010), Employment Status (C07001-C07004)
#   ds99: Female Occupation (C28001-C28027) — for medical workers count

library(data.table)
library(fst)

cat("Building 1970 Census controls from NHGIS...\n")

# ── ds98: Education + Employment (the main controls) ──
ds98 <- fread("nhgis0003_ds98_1970_county.csv")
ds98[, fips := sprintf("%02d%03d", STATEA, COUNTYA)]

# Education: Years of School Completed, persons 25+
# C06001: No school years completed
# C06002: Elementary 1-4 years
# C06003: Elementary 5-6 years
# C06004: Elementary 7 years
# C06005: Elementary 8 years
# C06006: High school 1-3 years
# C06007: High school 4 years
# C06008: College 1-3 years
# C06009: College 4 years
# C06010: College 5+ years
ds98[, total_25plus := C06001 + C06002 + C06003 + C06004 + C06005 +
                       C06006 + C06007 + C06008 + C06009 + C06010]
ds98[, pct_lt_hs := (C06001 + C06002 + C06003 + C06004 + C06005 + C06006) / total_25plus]
ds98[, pct_hs    := C06007 / total_25plus]

# Employment: Persons 16+
# C07001: Armed forces
# C07002: Civilian LF: Employed
# C07003: Civilian LF: Unemployed
# C07004: Not in labor force
ds98[, civ_lf := C07002 + C07003]
ds98[, total_16plus := C07001 + C07002 + C07003 + C07004]
ds98[, unemp_rate := C07003 / civ_lf]
ds98[, lfp_rate := civ_lf / total_16plus]

controls <- ds98[, .(fips, pct_lt_hs, pct_hs, unemp_rate, lfp_rate)]

# ── ds94: Persons in Urbanized Areas (for % urban) ──
ds94 <- fread("nhgis0003_ds94_1970_county.csv")
ds94[, fips := sprintf("%02d%03d", STATEA, COUNTYA)]
ds94_sub <- ds94[, .(fips, urban_pop = CBG001)]

# For % urban, we need total county population. Use SEER 1970.
seer <- fread("../../raw/population_seer/uswbo19agesadj.csv")
seer_1970 <- seer[year == 1970, .(total_pop = sum(as.numeric(pop))), by = .(county)]
seer_1970[, fips := sprintf("%05d", county)]

urban <- merge(ds94_sub, seer_1970[, .(fips, total_pop)], by = "fips", all.x = TRUE)
urban[, pct_urban := urban_pop / total_pop]
# Counties with no SEER pop: set to 0
urban[is.na(pct_urban), pct_urban := 0]

controls <- merge(controls, urban[, .(fips, pct_urban)], by = "fips", all.x = TRUE)
controls[is.na(pct_urban), pct_urban := 0]

# ── ds99: Medical workers (female only — best available) ──
ds99 <- fread("nhgis0003_ds99_1970_county.csv")
ds99[, fips := sprintf("%02d%03d", STATEA, COUNTYA)]
# C28001: Nurses, C28002: Medical & health workers, C28023: Health service workers
ds99[, med_workers_f := C28001 + C28002 + C28023]
controls <- merge(controls, ds99[, .(fips, med_workers_f)], by = "fips", all.x = TRUE)
controls[is.na(med_workers_f), med_workers_f := 0]

# Convert fips to integer for joining
controls[, fips := as.integer(fips)]

cat(sprintf("  %d counties\n", nrow(controls)))
cat("  Summary:\n")
print(summary(controls[, .(pct_lt_hs, pct_hs, unemp_rate, lfp_rate, pct_urban)]))

write_fst(controls, "nhgis_1970_controls.fst")
cat("  Written: nhgis_1970_controls.fst\n")
