Description (07/25/2025)

1. Original GHCN-hourly data in psv format is stored in Zenodo [https://doi.org/10.5281/zenodo.16305657]. Please download from there and place in the folder "/data_raw/GHCNh_WA".

2. "/data_clean" contains the final practical data of Washington average temperature and estimated density; the others are Nino 3.4, county populations, and WA electricity. These are used in later steps S0, S1 and S2.
    2a. The code will create a directory named "data_intermediate".
    2b. "/data_clean/WA_wt_density.csv" is the final temperature data to use. 

3. "/script/S1_1_cntf_temps.py" is the Python code to generate counterfactual WA temperature, stored along with the actual temperature in "/data_clean/cntf_t.csv". The R script generates table results for display and all the data needed for visualization.
    3a. "/data_clean/cntf_t.csv", "/data_clean/nino3.4_anom_1960_2024 - Copy.csv", and "/data_clean/WA_elec_pc_rp_1990_2023.csv" form a smallest workable dataset.