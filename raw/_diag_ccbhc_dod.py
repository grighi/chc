import pandas as pd

ccbhc = pd.read_parquet('/Users/giovannirighi/Documents/ucla/writing/cmhcs/112871-V1/build/data/ccbhc_panel.parquet')
dod   = pd.read_parquet('/Users/giovannirighi/Documents/ucla/writing/cmhcs/112871-V1/raw/wonder_mortality/data/dod_alladults.parquet')

overlap = sorted(set(ccbhc.year.unique()) & set(dod.Year.unique()))
print('Overlapping years:', overlap)
print()
print('ccbhc fips dtype:', ccbhc.fips.dtype, '| sample:', ccbhc.fips.head(3).tolist())
print('wonder County Code dtype:', dod['County Code'].dtype, '| sample:', dod['County Code'].head(3).tolist())
print()

ccbhc['fips_str'] = ccbhc['fips'].astype(str).str.zfill(5)
sub = ccbhc[ccbhc.year <= 2020].copy()
dod_slim = dod[['County Code','Year','Deaths','Population']].rename(
    columns={'Deaths': 'dod_deaths', 'Population': 'dod_pop'})
merged = sub.merge(dod_slim, left_on=['fips_str','year'], right_on=['County Code','Year'], how='left')
print('ccbhc rows (<=2020):', len(sub), '| matched DoD deaths non-null:', merged['dod_deaths'].notna().sum())
print()
print('N treated counties:', ccbhc[ccbhc.treated==1].fips.nunique())
print('N control counties:', ccbhc[ccbhc.treated==0].fips.nunique())
print()
print('Treat year distribution:')
print(ccbhc[ccbhc.treated==1].drop_duplicates('fips').groupby('treat_year').size())
print()
# Show DoD rate by treated vs control in pre-period
merged['dod_rate'] = merged['dod_deaths'] / merged['dod_pop'] * 100000
pre = merged[merged.year < 2018]
print('Pre-period (2010-2017) mean DoD rate:')
print(pre.groupby('treated')['dod_rate'].mean().round(2))
