# Scraping / Analyzing / Visualizing 'english.khamenei.ir'

This repository contains the scripts that were used for scraping and analyzing the speeches of 
Ali Khamenei, current supreme leader of the Islamic Republic of Iran, as well as the data set
containing the texts.

# Scripts
**Scraping_Script.R**
- Code used for scraping the official English-speaking website of Ali Khamenei with 'RSelenium' und 'Docker'.

**Analyzing_Script.R**
- Creation of dictionaries with kwic-function and random sampling of texts.
- Code for analyzing the texts with a predefinied dictionary-function and visualizing the results of the analysis.
- Contains the final dictionaries specifically created for analyzing anti-Semitic motives on Khamenei's website.
  
# Data
**Khamenei_Complete.csv**
- Texts scraped from the 'speeches'-section of the official English-speaking website of Ali Khamenei (1974 - March 2021)
- Empty cases eliminated and date format unified.
