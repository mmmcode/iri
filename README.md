# Scraping / Analyzing / Visualizing 'english.khamenei.ir'

TThe repository contains the scripts for scraping and analyzing the speeches of Ali Khamenei and the collected speeches data set.

# Scripts
**Scraping_Script.R**
- Code used for scraping the official English-speaking website of Ali Khamenei with 'RSelenium' und 'Docker'.

**Analyzing_Script.R**
- Creation of dictionaries with kwic-function and random sampling of texts.
- Code for analyzing the texts with a predefinied dictionary-function and visualizing the results of the analysis.
- Contains the final dictionaries specifically created for analyzing anti-Semitic motives on Khamenei's website.
  
# Data
**Khamenei_Complete.csv**
- Texts scraped from the 'speeches'-section of the official English-speaking website of Ali Khamenei (March 1974 - March 2021)
- Empty cases eliminated and date format unified.
