library(data.table)

inc <- fread('incarceration_trends_county.csv')

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
  total_jail_pop_rate, total_pop_15to64, total_jail_pop,
  black_jail_pop_rate,
#   total_prison_pop_rate, jail_adm_rate, jail_pretrial_rate, jail_dis_rate,
#   jail_sentenced_rate, male_incarceration_rate, female_incarceration_rate,
#   black_incarceration_rate, white_incarceration_rate, latinx_incarceration_rate,  
  jail_pop_per_capita
)]

fst::write_fst(inc, 'incarceration_trends_county.fst')
