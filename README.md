# Spotify Podcast New Metric Project - Group 12(Zhengyong Chen, Leyan Sun)

This project uses Spotify podcast data to create new metrics by analyzing episode titles and descriptions, providing personalized recommendations to users.

## Project Structure

### `/code`
Contains all R scripts used in the analysis and application setup.
- `app.R`: Shiny application for interactive data exploration.
- `clean.R`: Script for data cleaning and preparation.
- `clustering_plot.R`: Script for generating clustering plots.
- `get Podcast ID.R`: Script to fetch podcast IDs from Spotify.
- `test model(full model in shiny app).R`: Script containing the model used in the Shiny app for testing purposes.
- `data -podcast information.R`: Script to extract detailed podcast information.

### `/data`
Stores all datasets used and generated during the project.
- `full data.csv`: Complete dataset from Spotify.
- `test data.csv`: Dataset used for testing and validation purposes.

### `/image`
Contains all images generated from the analysis.

### `/Documentation`
- `Summary - Spotify Podcast New Metric Project.docx`: Summary
- `Summary - Spotify Podcast New Metric Project.pdf`: Summary

### `/Shiny`
All files about shiny app

## Web-Based App
https://andrewchanshiny.shinyapps.io/Spotify/

## Usage

To run the Shiny application:
1. Navigate to the `/code` directory.
2. Open `app.R` in RStudio.
3. Click 'Run App' to start the application.

Scripts can be run individually to perform specific tasks, such as data cleaning, fetching data, or testing the model as described in the files within the `/shiny` folder.

## Requirements

Make sure to install all required R packages before running the scripts or the Shiny application. You can install packages using the command `install.packages("package_name")`.
