inc <- fst::read_fst("incarceration_trends_county.fst")
library(dplyr)
inc_info <- inc %>% select(fips, county_name, state_abbr) %>% distinct()
non_county <- inc_info %>% filter(!grepl("County$", county_name))
cat("Non-County patterns:\n")
print(non_county %>% head(40))
cat("\nTotal non-County:", nrow(non_county), "\n")
cat("\nUnique state abbreviations:\n")
cat(paste(sort(unique(inc_info$state_abbr)), collapse=", "), "\n")

# Check Louisiana parishes
la <- inc_info %>% filter(state_abbr == "LA")
cat("\nLouisiana samples:\n")
print(head(la, 10))

# Check Virginia independent cities
va <- inc_info %>% filter(state_abbr == "VA") %>% filter(!grepl("County$", county_name))
cat("\nVirginia non-county:\n")
print(head(va, 20))

# Check Baltimore
md <- inc_info %>% filter(state_abbr == "MD")
cat("\nMaryland:\n")
print(md %>% filter(grepl("Baltimore|city", county_name, ignore.case=TRUE)))
