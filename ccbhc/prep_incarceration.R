# > dput(names(inc))
# c("year", "quarter", "state_abbr", "state_code", "state_fips", 
# "county_name", "county_code", "fips", "urbanicity", "region", 
# "division", "metro_area_2000", "commuting_zone_2000", "land_area_2000", 
# "total_pop", "total_pop_15to64", "female_pop_15to64", "male_pop_15to64", 
# "aapi_pop_15to64", "black_pop_15to64", "latinx_pop_15to64", "native_pop_15to64", 
# "white_pop_15to64", "aapi_female_pop_15to64", "black_female_pop_15to64", 
# "latinx_female_pop_15to64", "native_female_pop_15to64", "white_female_pop_15to64", 
# "aapi_male_pop_15to64", "black_male_pop_15to64", "latinx_male_pop_15to64", 
# "native_male_pop_15to64", "white_male_pop_15to64", "total_incarceration", 
# "total_incarceration_rate", "private_jail_flag", "regional_jail_flag", 
# "jail_rated_capacity", "total_jail_pop", "female_jail_pop", "male_jail_pop", 
# "aapi_jail_pop", "black_jail_pop", "latinx_jail_pop", "native_jail_pop", 
# "white_jail_pop", "other_race_jail_pop", "total_jail_pretrial", 
# "total_jail_from_prison", "total_jail_from_other_jail", "total_jail_from_fed", 
# "total_jail_from_bia", "total_jail_from_bop", "total_jail_from_ice", 
# "total_jail_from_marshals", "total_jail_from_other_fed", "total_jail_adm", 
# "female_jail_adm", "male_jail_adm", "total_jail_dis", "total_jail_sentenced", 
# "total_jail_pop_rate", "female_jail_pop_rate", "male_jail_pop_rate", 
# "aapi_jail_pop_rate", "black_jail_pop_rate", "latinx_jail_pop_rate", 
# "native_jail_pop_rate", "white_jail_pop_rate", "total_jail_adm_rate", 
# "female_jail_adm_rate", "male_jail_adm_rate", "total_jail_dis_rate", 
# "total_jail_pretrial_rate", "total_prison_pop", "female_prison_pop", 
# "male_prison_pop", "aapi_prison_pop", "black_prison_pop", "latinx_prison_pop", 
# "native_prison_pop", "white_prison_pop", "aapi_female_prison_pop", 
# "black_female_prison_pop", "latinx_female_prison_pop", "native_female_prison_pop", 
# "white_female_prison_pop", "aapi_male_prison_pop", "black_male_prison_pop", 
# "latinx_male_prison_pop", "native_male_prison_pop", "white_male_prison_pop", 
# "total_prison_adm", "female_prison_adm", "male_prison_adm", "aapi_prison_adm", 
# "black_prison_adm", "latinx_prison_adm", "native_prison_adm", 
# "white_prison_adm", "aapi_female_prison_adm", "black_female_prison_adm", 
# "latinx_female_prison_adm", "native_female_prison_adm", "white_female_prison_adm", 
# "aapi_male_prison_adm", "black_male_prison_adm", "latinx_male_prison_adm", 
# "native_male_prison_adm", "white_male_prison_adm", "total_prison_pop_rate", 
# "female_prison_pop_rate", "male_prison_pop_rate", "aapi_prison_pop_rate", 
# "black_prison_pop_rate", "latinx_prison_pop_rate", "native_prison_pop_rate", 
# "white_prison_pop_rate", "aapi_female_prison_pop_rate", "black_female_prison_pop_rate", 
# "latinx_female_prison_pop_rate", "native_female_prison_pop_rate", 
# "white_female_prison_pop_rate", "aapi_male_prison_pop_rate", 
# "black_male_prison_pop_rate", "latinx_male_prison_pop_rate", 
# "native_male_prison_pop_rate", "white_male_prison_pop_rate", 
# "total_prison_adm_rate", "female_prison_adm_rate", "male_prison_adm_rate", 
# "aapi_prison_adm_rate", "black_prison_adm_rate", "latinx_prison_adm_rate", 
# "native_prison_adm_rate", "white_prison_adm_rate", "aapi_female_prison_adm_rate", 
# "black_female_prison_adm_rate", "latinx_female_prison_adm_rate", 
# "native_female_prison_adm_rate", "white_female_prison_adm_rate", 
# "aapi_male_prison_adm_rate", "black_male_prison_adm_rate", "latinx_male_prison_adm_rate", 
# "native_male_prison_adm_rate", "white_male_prison_adm_rate")



inc <- fread('../incarceration_trends_county.csv')

# Construct possible outcomes
inc[, `:=`(
  # Main outcomes (already exist, but documenting)
  # total_jail_pop_rate - primary outcome
  # total_prison_pop_rate - alternative outcome
  
  # Jail admissions rate (new inflows)
  jail_adm_rate = (total_jail_adm / total_pop) * 100000,
  
  # Pretrial detention rate (awaiting trial)
  jail_pretrial_rate = (total_jail_pretrial / total_pop) * 100000,
  
  # Sentenced jail population rate
  jail_sentenced_rate = (total_jail_sentenced / total_pop) * 100000,
  
  # Gender-specific jail rates
  # male_jail_pop_rate - already exists
  # female_jail_pop_rate - already exists
  
  # Combined jail + prison incarceration rate
  total_incarceration_rate_combined = total_jail_pop_rate + total_prison_pop_rate,
  
  # Prison admission rate
  prison_adm_rate = (total_prison_adm / total_pop) * 100000,
  
  # Male-specific incarceration (jail + prison)
  male_incarceration_rate = male_jail_pop_rate + male_prison_pop_rate,
  
  # Female-specific incarceration (jail + prison)
  female_incarceration_rate = female_jail_pop_rate + female_prison_pop_rate,
  
  # Black incarceration rate (jail + prison)
  black_incarceration_rate = black_jail_pop_rate + black_prison_pop_rate,
  
  # White incarceration rate (jail + prison)
  white_incarceration_rate = white_jail_pop_rate + white_prison_pop_rate,
  
  # Latinx incarceration rate (jail + prison)
  latinx_incarceration_rate = latinx_jail_pop_rate + latinx_prison_pop_rate,
  
  # Raw jail population (for sensitivity checks with different denominators)
  jail_pop_per_capita = total_jail_pop / total_pop,
  
  # Jail discharge rate (proxy for churn/turnover)
  jail_dis_rate = (total_jail_dis / total_pop) * 100000,
  
  # ===== Adult (15-64) population denominators =====
  
  # Jail rates per 100k adults (15-64)
  adult_jail_pop_rate = (total_jail_pop / total_pop_15to64) * 100000,
  adult_jail_adm_rate = (total_jail_adm / total_pop_15to64) * 100000,
  adult_jail_pretrial_rate = (total_jail_pretrial / total_pop_15to64) * 100000,
  adult_jail_sentenced_rate = (total_jail_sentenced / total_pop_15to64) * 100000,
  
  # Prison rates per 100k adults (15-64)
  adult_prison_pop_rate = (total_prison_pop / total_pop_15to64) * 100000,
  adult_prison_adm_rate = (total_prison_adm / total_pop_15to64) * 100000,
  
  # Combined incarceration per 100k adults (15-64)
  adult_total_incarceration_rate = (total_jail_pop + total_prison_pop) / total_pop_15to64 * 100000,
  
  # Gender-specific adult rates
  adult_male_jail_rate = (male_jail_pop / male_pop_15to64) * 100000,
  adult_female_jail_rate = (female_jail_pop / female_pop_15to64) * 100000,
  adult_male_prison_rate = (male_prison_pop / male_pop_15to64) * 100000,
  adult_female_prison_rate = (female_prison_pop / female_pop_15to64) * 100000,
  
  # Race-specific adult jail rates
  adult_black_jail_rate = (black_jail_pop / black_pop_15to64) * 100000,
  adult_white_jail_rate = (white_jail_pop / white_pop_15to64) * 100000,
  adult_latinx_jail_rate = (latinx_jail_pop / latinx_pop_15to64) * 100000
)]


inc <- inc[, .(
  year, quarter, fips, county_name, state_abbr, total_pop,
  total_jail_pop_rate, total_prison_pop_rate, jail_adm_rate, jail_pretrial_rate,
  jail_sentenced_rate, male_incarceration_rate, female_incarceration_rate,
  black_incarceration_rate, white_incarceration_rate, latinx_incarceration_rate,
  jail_pop_per_capita, jail_dis_rate
)]

fst::write_fst(inc, '../incarceration_trends_county.fst')