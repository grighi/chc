library(arrow); library(data.table)
nber <- as.data.table(read_parquet("raw/nber_mortality/nber_mortality_counts.parquet"))

# Alcohol-related ICD codes
# ICD-8A: 291 (alcoholic psychoses), 303 (alcoholism), 571 (cirrhosis of liver)
# ICD-9:  291, 303, 571, E860 (accidental poisoning by alcohol)
alc_codes <- nber[grepl("^(291|303|571|860|E86)", ucod), .N, by = ucod][order(-N)]
cat("Potential alcohol codes found:\n")
print(head(alc_codes, 30))

cat("\nTotal deaths for 291*:", nber[grepl("^291", ucod), sum(deaths)], "\n")
cat("Total deaths for 303*:", nber[grepl("^303", ucod), sum(deaths)], "\n")
cat("Total deaths for 571*:", nber[grepl("^571", ucod), sum(deaths)], "\n")
cat("Total deaths for E860:", nber[grepl("^E86", ucod), sum(deaths)], "\n")
cat("Total deaths for 980:", nber[grepl("^980", ucod), sum(deaths)], "\n")

# Also check what BGB used for "alcohol-related" in the 1959-1988 window
nber_year_map <- c(setNames(1970:1978, as.character(0:8)),
                   setNames(1979:1995, as.character(79:95)))
nber[, yr := year]
nber[yr %in% names(nber_year_map), yr := nber_year_map[yr]]
nber[, yr := as.integer(yr)]
nber69 <- nber[!is.na(yr) & yr >= 1969 & yr <= 1988]

cat("\n\nIn 1969-1988 window:\n")
cat("291 deaths:", nber69[grepl("^291", ucod), sum(deaths)], "\n")
cat("303 deaths:", nber69[grepl("^303", ucod), sum(deaths)], "\n")
cat("571 deaths:", nber69[grepl("^571", ucod), sum(deaths)], "\n")
cat("E860 deaths:", nber69[grepl("^E86", ucod), sum(deaths)], "\n")

# Total "alcohol" (291 + 303 + 571) in 1969-1988
alc_total <- nber69[grepl("^(291|303|571)", ucod), sum(deaths)]
cat("\nTotal alcohol (291+303+571):", alc_total, "\n")

# Check count per county-year to gauge scale
alc_by_cy <- nber69[grepl("^(291|303|571)", ucod), 
                     .(deaths = sum(deaths)), by = .(staters, countyrs, yr)]
cat("Mean alcohol deaths per county-year:", mean(alc_by_cy$deaths), "\n")
