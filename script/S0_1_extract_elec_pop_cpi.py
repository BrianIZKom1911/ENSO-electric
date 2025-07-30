# Things to do:
# 1. Extract WA elec data in residential and commercial sector
# 2. Extract county and state population data; save the county data for later use
# 3. Get the real prices and per-capita electricity consumption from elec data, state population, and CPI-U
# %%
import os
import re
import pandas as pd
import numpy as np
# %%
# If you want to suppress the 'openpyxl' warning, use this chunk of code
#import warnings
#from openpyxl import Workbook
#warnings.filterwarnings("ignore", category=UserWarning, module="openpyxl")
# %% Step 1. Electricity data ##################################################
# Read and combine two tables
# Caveat: The code is file-specific; if EIA changes the table format later, it may not work properly.
md = 'D:/OneDrive - University of Missouri/transfer_desktop/MU/2025spring_submit1'
savepath = os.path.join(md, 'data_clean')
if not os.path.exists(savepath):
    os.makedirs(savepath)

files = ['HS861M 1990-2009.xlsx', 'HS861M 2010-.xlsx']
dfs = []
for file in files:
    file_path = os.path.join(md, 'data_raw', file)
    df = pd.read_excel(file_path, sheet_name=0, usecols='A:L', skiprows=2)
    # The tables have different sheet name!
    df = df.iloc[:-1] # drop the last row
    df = df.loc[(df['State']=='WA') & (df['Data Status']=='Final')]
    dfs.append(df)
dt_elec = pd.concat(dfs, ignore_index=True) # select WA and bindeth two by row
# %%
dt_elec_clean = dt_elec.drop(['State', 'Data Status', 'Thousand Dollars', 'Thousand Dollars.1'], axis=1)
dt_elec_clean.columns = ['Year', 'Month', 'resid_sales', 'resid_count', 'resid_np', 'comm_sales', 'comm_count', 'comm_np']
dt_elec_clean = dt_elec_clean.replace('.', np.nan)
# Save the temparory table in case of any error below
dt_elec_clean = dt_elec_clean.sort_values(by=['Year', 'Month'])
dt_elec_clean.to_csv(os.path.join(savepath, 'WA_elec_1990_2023.csv'), index=False)

# %% Step 2. Population data ###################################################
# Define function to change the column name
def extract_year(col_name):
    match = re.search(r'\d{4}', col_name) # search for a sequence of four digits
    return match.group(0) if match else col_name

# %%
filepath_id = os.path.join(md, 'county_station_list.xlsx')
filepath_pop = os.path.join(md, 'data_raw', 'ofm_april1_postcensal_estimates_pop_1960-present.xlsx')
dt_id = pd.read_excel(filepath_id, sheet_name='GHCNh', usecols='A:D', header=0, nrows=27)
dt_id = dt_id.drop('City', axis=1) # drop 'City' of geography data (Missing)
dt_pop = pd.read_excel(filepath_pop, sheet_name='Population', usecols='B:EC', skiprows=3, header=0, nrows=448)
# %% Clean the messy table
# Strip extra blanks from column names and identify columns of years
dt_pop.columns = dt_pop.columns.str.strip() 
valid_cols = ['County'] + [col for col in dt_pop.columns if re.match(r'^\d{4}', col)]
# Keep the rows of county total and state total, and valid numeric columns
dt_pop_clean = dt_pop.loc[dt_pop['Filter']==1, valid_cols]
dt_pop_state = dt_pop.loc[dt_pop['Filter']==100, valid_cols]
# Keep only the year in column names
dt_pop_clean.columns = [extract_year(col) for col in dt_pop_clean.columns]
dt_pop_state.columns = [extract_year(col) for col in dt_pop_state.columns]
# %% Combine
# Pivot to long table
dt_pop_long = pd.melt(dt_pop_clean, id_vars=['County'], var_name='Year', value_name='Population')
dt_pop_state_long = pd.melt(dt_pop_state, id_vars=['County'], var_name='Year', value_name='Population')
# Drop duplicates
# Note: In every year of ten, there are two rows of the same year--one from postcensal estimates and the other from Census 
dt_pop_long = dt_pop_long.drop_duplicates(subset=['County', 'Year'], keep='first')
dt_pop_state_long = dt_pop_state_long.drop_duplicates(subset='Year', keep='first')

# Merge county population with station list
dt_merged = pd.merge(dt_pop_long, dt_id, on='County', how='right')
dt_merged.to_csv(os.path.join(savepath, 'population_1960_2024.csv'), encoding='utf-8-sig', index=False)

# %% Step 3. Calculation #######################################################
filepath_cpi = os.path.join(md, 'data_raw', 'CPI-U_CUUR0000SA0 1990-2024.xlsx')
# Transform CPI-U to a long table
dt_cpi = pd.read_excel(filepath_cpi, usecols='A:M', skiprows=11, header=0, nrows=34)
dt_cpi.columns = ['Year'] + ['1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12']
dt_cpi_long = pd.melt(dt_cpi, id_vars=['Year'], var_name='Month', value_name='CPI')
# %%
# Make sure the datatype aligns
dt_cpi_long['Month'] = dt_cpi_long['Month'].astype(int)
dt_pop_state_long['Year'] = dt_pop_state_long['Year'].astype(int)
dt_elec_clean['Month'] = dt_elec_clean['Month'].astype(int)
# Merge twice
dt_merge = pd.merge(dt_elec_clean, dt_cpi_long, on=['Year', 'Month'], how='left')
dt_merge = pd.merge(dt_merge, dt_pop_state_long, on=['Year'], how='left')
# Calculate per-capita consumption
dt_merge['resid_pc'] = dt_merge['resid_sales'] / dt_merge['Population']
dt_merge['comm_pc'] = dt_merge['comm_sales'] / dt_merge['Population']
# Calculate real prices
dt_merge['resid_rp'] = (dt_merge['resid_np'] / dt_merge['CPI'])*100
dt_merge['comm_rp'] = (dt_merge['comm_np'] / dt_merge['CPI'])*100
# Save
dt_merge = dt_merge.sort_values(by=['Year', 'Month'])
dt_merge.to_csv(os.path.join(savepath, 'WA_elec_pc_rp_1990_2023.csv'), index=False)
# End of script.