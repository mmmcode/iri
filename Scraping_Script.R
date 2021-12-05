##################################################################
#####################Working Directory############################
##################################################################

# setwd()

##################################################################
######################Packages####################################
##################################################################

if (!require("pacman")) install.packages("pacman")
p_load(RSelenium, tidyverse, rvest, stringr, XML)

#####################################################################################
#####################Webscraping#####################################################
#####################################################################################

# Initialize RSelenium server
remDr <- RSelenium::rsDriver(remoteServerAddr = "localhost", 
                             port = 8787L, browser = "firefox")
# Close server
#remDr$closeServer()
#rD$close()
#remDr$server$stop()
#rm(remDr)
#gc()

# Open client to steer browser
rD <- remDr[["client"]]

####################################################################
########################URL-Scrape & Dataframe######################
####################################################################

# Get URLs

url_vec = c()

for(page in 1:32){
  
  Sys.sleep(sample(1:5, 1)+sqrt(sample(seq(.1,5,.1),1))*sample(1:2,1))
  
  cat("We are on Page ", page, "\n\n")
  
  ### Get page url and visit
  url = paste0("https://english.khamenei.ir/page/archive.xhtml?ms=0&tp=2&pi=",page)
  rD$navigate(url)
  
  ### Go to individual articles
  
  if(page != 32){
    ### Handle articles:
    for(article in 1:30){
      
      ### Make article link
      link = paste0(".clearfix:nth-child(", article,") a")
      showarticle <- rD$findElement("css", link)
      
      ### Get speech text
      url = unlist(showarticle$getElementAttribute('href'))
      url_vec = c(url_vec, url)
      Sys.sleep(.5)
    }
  }
  if(page == 32){
    ### Handle articles:
    for(article in 1:15){
      ### Make article link
      link = paste0(".clearfix:nth-child(", article,") a")
      showarticle <- rD$findElement("css", link)
      
      ### Get speech text
      url = unlist(showarticle$getElementAttribute('href'))
      url_vec = c(url_vec, url)
      Sys.sleep(.5)
    }
  }
}

# Write out url df
scraped_df_n = data.frame(URL = url_vec)
write.csv(scraped_df_n, "Scrape_df_n.csv")


##################################################################
#####################Speeches####################################
##################################################################

##### Read in DF with URLs

# Set working directory
# setwd()

# Import scraped URLs
scraped_df_import = read.csv("Scrape_df.csv")
#scraped_df_import$date = NA
#scraped_df_import$text = NA
#scraped_df_import$title = NA

scraped_df_import$URL = as.character(scraped_df_import$URL)
scraped_df_import$date = as.character(scraped_df_import$date)
scraped_df_import$text = as.character(scraped_df_import$text)
scraped_df_import$title = as.character(scraped_df_import$title)

##### Specify CSS nodes
text = ".full-text"
title = ".item-title > h2:nth-child(1)" #alternatively ".p"
date = "li.date"

for(row in which(is.na(scraped_df_import$date))){
  
  ##### Navigate to article page
  url = as.character(scraped_df_import$URL[row])
  
  rD$navigate(url)
  Sys.sleep(4)
  ##### Navigate to article page
  scraped_df_import$text[row] = read_html(url) %>%
    html_nodes(css = text) %>%
    html_text()
  art_title = read_html(url) %>%
    html_nodes(css = title) %>%
    html_text() %>%
    str_replace_all('\n|\r','')
  scraped_df_import$title[row] = art_title
  scraped_df_import$date[row] = read_html(url) %>%
    html_nodes(css = date) %>%
    html_text()
  
  how_many_coded = nrow(scraped_df_import)-length(which(is.na(scraped_df_import$date)))
  cat(paste0(art_title, '[',how_many_coded, ' of ', nrow(scraped_df_import),' coded]', '\n'))
  
  ##### Put into DF
  
  # Set working directory
  # setwd()
  write.csv(scraped_df_import, "Scrape_df.csv")
  
  ##### Define sleep
  sleep_sample = seq(5, 25, by=0.1)
  long_sleep = sample(c(T,rep(F,14)), 1) 
  long_sleep = ifelse(long_sleep, 5*60,0)
  
  sleep = 10+sleep_sample+long_sleep
  Sys.sleep(sleep)
  
}
