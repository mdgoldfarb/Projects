import pandas as pd
import numpy as np
import statsmodels.api as sm 
import statsmodels.formula.api as smf
import statsmodels.graphics.api as smg
import matplotlib.pyplot as plt


medals = pd.read_csv('Summer-Olympic-medals-1976-to-2008.csv', encoding='latin-1')
medals['Medal'] = medals['Medal'].replace({'Bronze': 1, 'Silver': 2, 'Gold': 3}) #medal weights with 3 being gold, 2 silver, 1 bronze
medals.drop_duplicates(subset=['Year', 'Sport', 'Event', 'Country'], keep='first', inplace=True)
medals = medals.drop(['City', 'Sport', 'Discipline', 'Event', 'Athlete', 'Gender', 'Event_gender', 'Country_Code'], axis=1)
medals = medals.groupby(['Year', 'Country']).sum().reset_index() #Combine rows where Year and Country are the same
medals = medals.pivot_table(index='Country', columns='Year', values='Medal', fill_value=0)
medals.columns = medals.columns.astype(int)
medals.columns = [str(col) + 'medals' for col in medals.columns]
medals['Total'] = medals.sum(axis=1) # Add a column named 'Total' that is the sum of the other 'medals' columns

gdp_pc = pd.read_csv('gdp_per_capita.csv', encoding='latin-1')
gdp_pc.rename(columns={'Country Name': 'Country'}, inplace=True)
gdp_pc = gdp_pc.drop('Code', axis=1)
gdp_pc_medals = pd.merge(gdp_pc, medals, on='Country') # Merge gdp_pc and medals dataframes on 'Country' column
gdp_pc_medals = gdp_pc_medals.fillna(np.nan)

years = range(1976, 2009, 4)  # Generate a list of years every 4 years between 1976 and 2008
years1990 = range(1992, 2009, 4)

# GDPpc regression for each year
for year in years:
    x = gdp_pc_medals[str(year)]
    y = gdp_pc_medals[str(year) + 'medals']
    model = smf.ols(f"y ~ x", gdp_pc_medals)
    result = model.fit()
    print(f"GDPpc regression for {year}:")
    print(result.summary())


hdi = pd.read_csv('HDI.csv', encoding='latin-1')
hdi = hdi.replace('..', np.nan)
hdi_medals = pd.merge(hdi, medals, on='Country')  # Merge hdi and medals dataframes on 'Country' column
hdi_medals = hdi_medals.fillna(np.nan)
hdi_medals[hdi_medals.columns[2:]] = hdi_medals[hdi_medals.columns[2:]].astype(float)

# HDI regression for each year
for year in years1990:
    x = hdi_medals[str(year)]
    y = hdi_medals[str(year) + 'medals']
    model = smf.ols(f"y ~ x", hdi_medals)
    result = model.fit()
    print(f"HDI regression for {year}:")
    print(result.summary())

population = pd.read_csv('world_population.csv', encoding='latin-1')

# 2008 population bar chart
population2008_sorted = population.sort_values(by='2008', ascending=False)
top_10 = population2008_sorted.iloc[1:11]  
plt.bar(top_10['Country'], top_10['2008'])
plt.title("Top 10 Populations in 2008")
plt.xlabel('Country')
plt.ylabel('Population')
plt.xticks(rotation=45)
plt.show()

pop_medals = pd.merge(population, medals, on='Country')  # Merge population and medals dataframes on 'Country' column

# Population regression for each year
for year in years:
    x = pop_medals[str(year)]
    y = pop_medals[str(year) + 'medals']
    model = smf.ols(f"y ~ x", pop_medals)
    result = model.fit()
    print(f"Population regression for {year}:")
    print(result.summary())


gdp = pd.read_csv('gdp.csv', encoding='latin-1')
gdp.rename(columns={'Country Name': 'Country'}, inplace=True)
gdp = gdp.drop('Code', axis=1)
gdp_medals = pd.merge(gdp, medals, on='Country')  # Merge gdp and medals dataframes on 'Country' column
gdp_medals = gdp_medals.fillna(np.nan)

# GDP regression for each year
for year in years:
    x = gdp_medals[str(year)]
    y = gdp_medals[str(year) + 'medals']
    model = smf.ols(f"y ~ x", gdp_medals)
    result = model.fit()

    # Create QQ-plot
    smg.qqplot(result.resid, line='s')
    plt.title(f"QQ-plot for {year}")
    plt.text(0.05, 0.95, f"R-squared: {result.rsquared:.2f}", transform=plt.gca().transAxes, ha='left', va='top')
    plt.show()

    print(f"GDP regression for {year}:")
    print(result.summary())
    
# Create scatterplot for 1976
plt.scatter(gdp_medals['1976'], gdp_medals['1976medals'])
plt.title("Scatterplot for 1976")
plt.xlabel('GDP')
plt.ylabel('Medals')
plt.xscale('log')  # Set x-axis scale to log
plt.legend()
# Add prediction line
x_pred = np.linspace(min(gdp_medals['1976']), max(gdp_medals['1976']), 100)
y_pred = result.predict({'x': x_pred})
plt.plot(x_pred, y_pred, color='red', label='Prediction')
plt.show()

# Create scatterplot for 1980
plt.scatter(gdp_medals['1980'], gdp_medals['1980medals'])
plt.title("Scatterplot for 1980")
plt.xlabel('GDP')
plt.ylabel('Medals')
plt.xscale('log')  # Set x-axis scale to log
plt.legend()
# Add prediction line
x_pred = np.linspace(min(gdp_medals['1980']), max(gdp_medals['1980']), 100)
y_pred = result.predict({'x': x_pred})
plt.plot(x_pred, y_pred, color='red', label='Prediction')
plt.show()

# Create scatterplot for 2008
plt.scatter(gdp_medals['2008'], gdp_medals['2008medals'])
plt.title("Scatterplot for 2008")
plt.xlabel('GDP')
plt.ylabel('Medals')
plt.xscale('log')  # Set x-axis scale to log
plt.legend()
# Add prediction line
x_pred = np.linspace(min(gdp_medals['2008']), max(gdp_medals['2008']), 100)
y_pred = result.predict({'x': x_pred})
plt.plot(x_pred, y_pred, color='red', label='Prediction')
plt.show()