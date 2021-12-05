**Scraping / Analyzing / Visualizing 'english.khamenei.ir'**

This repository contains the scripts that were used for scraping and analyzing the speeches of 
Ali Khamenei, current supreme leader of the Islamic Republic of Iran, as well as the data set
containing the texts.

**Scripts**
# Scraping_Script.R**
- Code used for scraping the official English-speaking website of Ali Khamenei with 'RSelenium' und 'Docker'.

**Dictionaries_Script.R**
- Dictionaries specifically created for analyzing anti-Semitic content on Khamenei's website.
- 

**Analyzing_Script.R**
- Code for analyzing the texts with predefinied problem-specific dictionaries.

**Visualizing_Script.R**
- Code for visualizing the results of the dictionary-analysis (prevalence over decades/year, quotes).
  
# Data
**Khamenei_Complete.CSV**
- Texts scraped from the 'speeches'-section of the official English-speaking website of Ali Khamenei (1974 - March 2021)
- Empty cases eliminated and date format unified.
