library(arrow); library(dplyr)

ds <- open_dataset("raw/nber_mortality/nber_mortality_counts.parquet")
ucods <- ds %>% select(ucod) %>% distinct() %>% collect()

# ICD-7/8A (1959-1978): suicide = 950-959, homicide = 960-969
# ICD-9 (1979-1998): suicide = E950-E959, homicide = E960-E969
# ICD-10 (1999+): suicide = X60-X84, homicide = X85-Y09

sui_codes <- ucods$ucod[grepl("^(950|951|952|953|954|955|956|957|958|959|E95[0-9]|X[67][0-9]|X8[0-4])$", ucods$ucod)]
cat("Suicide codes:", paste(sort(sui_codes), collapse=", "), "\n\n")

hom_codes_78 <- ucods$ucod[grepl("^(96[0-9])", ucods$ucod)]
hom_codes_9 <- ucods$ucod[grepl("^E96[0-9]", ucods$ucod)]
hom_codes_10 <- ucods$ucod[grepl("^(X8[5-9]|X9[0-9]|Y0[0-9])$", ucods$ucod)]
cat("Homicide ICD-7/8:", paste(sort(hom_codes_78), collapse=", "), "\n")
cat("Homicide ICD-9:", paste(sort(hom_codes_9), collapse=", "), "\n")
cat("Homicide ICD-10:", paste(sort(hom_codes_10), collapse=", "), "\n\n")

# Check: how many total suicide/homicide deaths by year and race
# For the CMHC era (1959-1988)
# Need to handle year mapping: 0-8 = 1970-1978, 79-95 = 1979-1995

sui_icd78 <- c("950-", paste0("950", 0:9), "951", "951-", paste0("951", 0:9),
               "952-", paste0("952", 0:9), "953", "953-", paste0("953", 0:9),
               "954", "954-", "955", "955-", paste0("955", 0:9),
               "956", "956-", "957", "957-", paste0("957", 0:9),
               "958", "958-", paste0("958", 0:9), "959", "959-")
sui_icd9 <- paste0("E95", 0:9)
sui_icd10 <- c(paste0("X", 60:84))

hom_icd78 <- c("960-", paste0("960", 0:9), "961", "961-", paste0("961", 0:9),
               "962", "962-", paste0("962", 0:9), "963", "963-", "964", "964-",
               "965", "965-", paste0("965", 0:9), "966", "966-",
               "967", "967-", "968", "968-", paste0("968", 0:9),
               "969", "969-", paste0("969", 0:9))
hom_icd9 <- paste0("E96", 0:9)
hom_icd10 <- c(paste0("X", 85:99), paste0("Y0", 0:9))

all_sui <- c(sui_icd78, sui_icd9, sui_icd10)
all_hom <- c(hom_icd78, hom_icd9, hom_icd10)

# Now filter and aggregate
sui_data <- ds %>% filter(ucod %in% all_sui) %>% collect()
cat("Suicide: ", nrow(sui_data), "rows,", sum(sui_data$deaths), "deaths\n")

hom_data <- ds %>% filter(ucod %in% all_hom) %>% collect()
cat("Homicide:", nrow(hom_data), "rows,", sum(hom_data$deaths), "deaths\n")

cat("\nSuicide by race (1=White, 2=Black):\n")
print(sui_data %>% group_by(race) %>% summarise(deaths=sum(deaths)) %>% arrange(race))

cat("\nHomicide by race:\n")
print(hom_data %>% group_by(race) %>% summarise(deaths=sum(deaths)) %>% arrange(race))

# Check county coverage
cat("\nN unique counties in suicide data:", length(unique(paste0(sui_data$staters, sui_data$countyrs))), "\n")
