# Transform nino3.4 into a long table
# %%
import os
import pandas as pd
# %%
md = 'D:/OneDrive - University of Missouri/transfer_desktop/MU/2025spring_submit1'
savepath = os.path.join(md, 'data_clean')
if not os.path.exists(savepath):
    os.makedirs(savepath)
filepath = os.path.join(md, 'data_raw', 'nino3.4_anom.xlsx')
dt = pd.read_excel(filepath, usecols='A:M', header=0, nrows=154)
# %%
dt = dt.loc[dt['Year']>=1960, :]
dt_long = pd.wide_to_long(dt, stubnames='m', i='Year', j='Month')
dt_long.columns = ['anom']
dt_long.reset_index(inplace=True)
# %%
dt_long.to_csv(os.path.join(savepath, 'nino3.4_anom_1960_2024.csv'), encoding='utf-8-sig', index=False)
