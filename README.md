# Data Analysis README

## Overview
This Python script performs a data analysis on the relationship between a country's GDP, HDI (Human Development Index), and population with their performance in the Summer Olympics from 1976 to 2008. It utilizes various libraries such as Pandas, NumPy, Statsmodels, and Matplotlib for data manipulation, regression analysis, and visualization.

## Prerequisites
Make sure you have the following Python libraries installed:
- pandas
- numpy
- statsmodels
- matplotlib

## Data Sources
1. Summer-Olympic-medals-1976-to-2008.csv
   - Contains information about Olympic medals won by countries from 1976 to 2008.

2. gdp_per_capita.csv
   - Provides GDP per capita data for countries.

3. HDI.csv
   - Includes Human Development Index (HDI) values for countries.

4. world_population.csv
   - Contains world population data.

5. gdp.csv
   - Provides GDP data for countries.

## Data Cleaning and Preprocessing
1. Load Olympic medals data, convert medal types to weights, remove duplicates, and aggregate by year and country.
2. Merge GDP per capita data with Olympic medals data.
3. Perform GDP per capita regression analysis for each Olympic year.
4. Merge HDI data with Olympic medals data.
5. Perform HDI regression analysis for each Olympic year.
6. Merge world population data with Olympic medals data.
7. Perform population regression analysis for each Olympic year.
8. Merge GDP data with Olympic medals data.
9. Perform GDP regression analysis for each Olympic year.
10. Generate QQ-plots for GDP regression residuals.

## Regression Analysis Output
- The script prints summary statistics for GDP per capita, HDI, and population regression analyses for each Olympic year.

## Visualization
- Scatterplots and prediction lines are created for GDP and medals relationship in 1976, 1980, and 2008.

## Usage
1. Ensure the required Python libraries are installed.
2. Place the provided CSV files in the same directory as the script.
3. Run the script in a Python environment.

Feel free to explore and modify the script to suit your specific analysis needs.