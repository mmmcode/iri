# Scraping / Analyzing / Visualizing 'english.khamenei.ir'

This repository contains the scripts that were used for scraping and analyzing the speeches of 
Ali Khamenei, current supreme leader of the Islamic Republic of Iran, as well as the data set
containing the texts.

# Scripts
**Scraping_Script.R**
- Scraping the official English-speaking website of Ali Khamenei using 'RSelenium' und 'Docker'.
- Can be applied to scrape other websites as well.

**Dictionaries_Script.R**
- Contains dictionaries specifically created for analyzing anti-Semitic content on Khamenei's website.

**Analyzing_Script.R**
- Analyzing the texts using predefinied problem-specific dictionaries.
- Dictionary-function can also be used for other analyzes. 

**Visualizing_Script.R**
- Visualizing the results of the analysis (prevalence over decades/year, quotes).
  
# Data
**Khamenei_Complete.CSV**
- Texts scraped from the 'speeches'-section of the official English-speaking website of Ali Khamenei.
- Empty cases eliminated and date format unified.
