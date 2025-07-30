# Calculate cntf temperatures for a single series at the state level:
# 1. Load Nino 3.4 and WA_temp; merge by year_month
# 2. Regress hourly WA temp on Nino 3.4 with month dummies. 
# 3. Save cntf_temp := intercepts + residuals
# Note: 
# (1) The Nino 3.4 data is monthly, so the regression is monthly, and cntf_temp only differs from real temp by month
# Thus, the densities at each cntf_temp won't change; f_c(r_c) = f_a(r_a) for each month
# (2) This is true for the other way of getting cntf as well. Because f_c(r_c) = f_a(r_a) for each county-month
# %%
import os
import pandas as pd
import numpy as np
import statsmodels.api as sm
# %% Import Nino 3.4 and WA_temp_v1
md = 'D:/OneDrive - University of Missouri/transfer_desktop/MU/2025spring_submit1'
dt_Nino = pd.read_csv(os.path.join(md, 'data_raw', 'nino3.4_anom_1960_2024 - Copy.csv'))
dt_wa = pd.read_csv(os.path.join(md, 'data_clean', 'WA_wt_density.csv'))[['datetime00', 'temperature', 'Year', 'Month', 'yyyymm', 'f_r']]
# %% Merge
dt_Nino['anom_pos'] = dt_Nino['anom'].apply(lambda x: x if x > 0 else 0)
dt_Nino['anom_neg'] = dt_Nino['anom'] - dt_Nino['anom_pos']

dt_main = pd.merge(dt_wa, dt_Nino, on=['Year', 'Month'], how='left')
# %% Regression
# Add month dummies
month_dummies = pd.get_dummies(dt_main['Month'], prefix='m', drop_first=False).astype(int)
dt_mm = pd.concat([dt_main, month_dummies], axis=1)

# Define functions: regression and robust standard errors:
def regress(df, y, x):
    X = sm.add_constant(df[x])
    model = sm.OLS(df[y], X).fit()
    robust = model.get_robustcov_results(cov_type='HC3')
    tab_result = pd.DataFrame(
        {'Coef': robust.params, 
         'Std_Err': robust.bse, 
         't-Stat': robust.tvalues,
         'p-Value': robust.pvalues}
    )
    return tab_result, robust.summary()

X_col1 = ['anom_pos', 'anom_neg', 'm_1', 'm_2', 'm_3', 'm_4', 'm_5', 'm_6', 'm_7', 'm_8', 'm_9', 'm_10', 'm_11']
tab1, summary1 = regress(dt_mm, 'temperature', X_col1)
X_col2 = ['el_nino', 'la_nina', 'm_1', 'm_2', 'm_3', 'm_4', 'm_5', 'm_6', 'm_7', 'm_8', 'm_9', 'm_10', 'm_11']
tab2, summary2 = regress(dt_mm, 'temperature', X_col2)
with open(os.path.join(md, 'output', 'tab_reg_t_e_rbst.txt'), 'w') as f:
    f.write("### Regression 1: Temp vs Anom ###\n")
    f.write(str(summary1))
    f.write("\n\n### Regression 2: Temp vs Event ###\n")
    f.write(str(summary2))

# %%
# Drop rows where y or X contain NaN 
valid_rows = dt_mm['temperature'].notna() & dt_mm[X_col1].notna().all(axis=1)
#This is unnecessary for the current dataset but is a good reference for future practice
X = sm.add_constant(dt_mm.loc[valid_rows, X_col1])
y_data = dt_main.loc[valid_rows, 'temperature']
model = sm.OLS(y_data, X).fit()
# Extract intercept and residuals
base_intercept = model.params['const']
residuals = model.resid
month_dummies = [col for col in X_col1 if col.startswith('m_')]
month_effects = pd.Series(
    {month: model.params.get(month, 0) for month in month_dummies}
)
# Create counterfactual series
dt_main['cntf_temp'] = np.nan
for idx in valid_rows[valid_rows].index:
# An easier but less efficient way:
#    m = dt_main.loc[idx, 'Month']
#    dt_main.loc[idx, 'cntf_temp'] = base_intercept + residuals[idx] + month_effects[f'm_{m}']
    row = dt_mm.loc[idx, month_dummies]
    month_effect = row.dot(month_effects)
    dt_mm.loc[idx, 'cntf_temp'] = base_intercept + month_effect + residuals[idx]

dt_mm.to_csv(os.path.join(md, 'data_clean', 'cntf_t.csv'), index=False)
