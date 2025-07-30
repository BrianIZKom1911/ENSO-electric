# Make sure the packages below are installed
# In Anaconda Prompt: conda install geopandas
# Make sure you select the right interpreter like 'D:\Anaconda' if it's running in an IDE
import geopandas as gpd
import plotly.graph_objects as go
import matplotlib.pyplot as plt
import os
import pandas as pd

# %% # Step 1: GeoJSON 
counties = gpd.read_file('washington_state_counties.geojson')
counties['id'] = counties.index.astype(str)  # ensureth the ID is a string to match GeoJSON
counties = counties.to_crs(epsg=4326)  # reprojecteth to WGS84
# %% # Step 2: Population data
md = 'D:/OneDrive - University of Missouri/transfer_desktop/MU/2025spring_submit1'
file_path = os.path.join(md, 'county_station_list.xlsx')
df_county_pop = pd.read_excel(file_path, sheet_name='Sheet1', usecols='A:B')
df_cities = pd.read_excel(file_path, sheet_name='Sheet1', usecols='C:F')

# Define custom population thresholds
thresholds = [0, 30000, 75000, 150000, 300000, float('inf')]  # tiny, small, medium, large, big
bin_labels = [2, 3, 6, 9, 15]
df_cities['dot_size'] = pd.cut(df_cities['city_population'], bins=thresholds, labels=bin_labels, right=False).astype(int)
# %% # Step 3: Merge population data with county GeoJSON based on 'County' column
df_counties = counties.merge(df_county_pop, left_on='NAME', right_on='County', how='left')

# %% # Step 4: Plot the map with counties colored by population
choropleth = go.Choropleth(
    geojson=df_counties.__geo_interface__,
    locations=df_counties['County'],
    z=df_counties['population'],
    featureidkey="properties.NAME",
    colorscale='Blues',
    marker_line_width=0.5,
    colorbar_title="County Population",
    showscale=True
)
# %% # Step 5: Add cities as dots
scattergeo = go.Scattergeo(
    lon=df_cities['longitude'],
    lat=df_cities['latitude'],
    text=df_cities.apply(
        lambda row: f"{row['City']}: {row['city_population']}", axis=1
    ),  # City name and population as hover text
    marker=dict(
        size=df_cities['dot_size'],
        color='orange',
        opacity=0.5,
        line=dict(width=1, color='darkred')
    ),
    hoverinfo='text',
    mode='markers'
)
# %% # Step 6: Layout configuration
fig = go.Figure(data=[choropleth, scattergeo])
fig.update_layout(
    geo=dict(
        lakecolor='white',
        projection=dict(type='albers usa'), # Use Albers projection for US
        showland=True,
        landcolor='lightgray',
        scope='usa',
        fitbounds = 'locations',
        visible=True
    )
)
#fig.show()
# %% # Save the plot
fig.write_html('plot_pop_county.html')
#fig.write_image('plot_pop_county.png', width=1200, height=741)
