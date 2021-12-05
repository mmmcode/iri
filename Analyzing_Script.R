##################################################################
#####################Working Directory############################
##################################################################

# Set working directory
# setwd()

##################################################################
####################Packages######################################
##################################################################

if (!require("pacman")) install.packages("pacman")
p_load(dplyr, stringr, ggplot2, quanteda)

################################################################################
##################################Read in file##################################
################################################################################

#####  Read in speech file
speech_df <- read.csv("Khamenei_Complete.csv", encoding = "UTF-8")
speech_df$X = NULL
# Lower-casing
speech_df$text = tolower(speech_df$text)
speech_df$Year = str_extract(as.character(speech_df$date), "....")
speech_df$Month =str_extract(as.character(speech_df$date), "(?<=-)..")
speech_df = subset(speech_df, as.character(speech_df$Year) > 1989)
# Subset to Years past 1990
# speech_df$decade = factor(paste0(substring(speech_df$Year, 3,3), "0s"),
#                          levels = c("90s","00s","10s","20s"))
# speech_df = subset(speech_df, !is.na(speech_df$date))


### Year Specific Subset 
speech_df_y <- read.csv("Khamenei_Complete.csv", encoding = "UTF-8")
speech_df_y$X = NULL
speech_df_y$text = tolower(speech_df_y$text)
speech_df_y$Year = str_extract(as.character(speech_df_y$date), "....")
speech_df_y$Month =str_extract(as.character(speech_df_y$date), "(?<=-)..")
speech_df_y = subset(speech_df_y, as.character(speech_df_y$Year) == 2002)
# Analyzing specific years
speech_df_y = subset(speech_df_y, !is.na(speech_df_y$date))


################################################################################
##################################Creating dictionaries#########################
################################################################################

# Create corpus
kha_corp = corpus(speech_df)
summary(kha_corp)

################################################################################
###########################Keywords-in-context##################################
################################################################################

# Explore corpus texts with kwic-function
kwic_zion_k = kwic(kha_corp, pattern = "zionis*", window = 10)
head(kwic_zion_k, 20)

kwic_israel_k = kwic(kha_corp, pattern = "israel*", window = 10)
head(kwic_israel_k, 20)

kwic_jew_k = kwic(kha_corp, pattern = "jew*", window = 10)
head(kwic_jew_k, 20)
# Detected "faith was a precious | jewel  |" -> thus, a regular expression "(^| )jew[si.-]" is used for sampling and further analyses.

kwic_moss_k = kwic(kha_corp, pattern = "mossad", window = 10)
head(kwic_moss_k, 20)

kwic_eld_k = kwic(kha_corp, pattern = "protocol*", window = 10)
head(kwic_eld_k, 20)

kwic_holocaust_k = kwic(kha_corp, pattern = "holocaust")
head(kwic_holocaust_k, 20)

kwic_nazi_k = kwic(kha_corp, pattern = "nazi*")
head(kwic_nazi_k, 20)

kwic_conspir_k = kwic(kha_corp, pattern = "conspir*")
head(kwic_conspir_k, 20)

kwic_palest_k = kwic(kha_corp, pattern = "palest*")
head(kwic_palest_k, 20)

kwic_quds_k = kwic(kha_corp, pattern = "quds*")
head(kwic_quds_k, 20)

kwic_econ_k = kwic(kha_corp, pattern = "econom*")
head(kwic_econ_k, 20)

kwic_nuc_k = kwic(kha_corp, pattern = "nuclear*")
head(kwic_nuc_k, 20)


################################################################################
###########################Regular Expressions##################################
################################################################################

############################
# Obtain sample of documents

kha_corp_izj_f = corpus_subset(kha_corp, grepl("(^| )israel[i.-]"|"(^| )zion[i.-]"| 
                                                 "(^| )usurper"|"(^| )usurping"|
                                                 "(^| )occupier"|"(^| )occupying"|
                                                 "(^| )jew[si.-]"|"(^| )judaism",
                                               kha_corp, ignore.case = T))
View(kha_corp_izj)
set.seed(864)
sample_kha_corp_izj = corpus_sample(kha_corp_izj, size = 30, replace = FALSE)
summary(sample_kha_corp_izj)
View(sample_kha_corp_izj)
df = data.frame(sample_kha_corp_izj)
write.csv(sample_kha_corp_izj, "sample_kha_corp_izj.csv")

################################################################################
##################################Final Dictionaries############################
################################################################################

##########################################################################
##### Israel, Zionism, Zionists and Jews

israelzion_dic = list("(^| )israel[i.-]", "(^| )zion[i.-]",
                 "(^| )usurper","(^| )usurping","(^| )occupier", "(^| )occupying")

jewjud_dic = list("(^| )jew[si.-]","(^| )judaism")

israelzionjewjud_dic = list("(^| )israel[i.-]", "(^| )zion[i.-]", 
                         "(^| )usurper", "(^| )usurping",
                         "(^| )occupier", "(^| )occupying",
                         "(^| )jew[si.-]", "(^| )judaism")


#########################################################
################Antisemitic motives#####################
#########################################################

holocaustdenial_dic = list("holocaust", "nazi concentration camp","myth of holocaust","myth of the holocaust",
                           "myth of the massacre of jews", "myth of the massacre of the jews","truth of the holocaust",
                           "calls the holocaust into question", "allegedly killed in the nazi concentration camps")

extinction_dic = list("zionist regime will be destroyed","eradication of israel", "annihilating the zionist regime",
                      "termination of israel", "annihilation of the evil and illegitimate zionist regime",
                      "israel should be annihilated", "total collapse and annihilation", "abolition of the zionist regime",
                      "from the river to the sea", "doomed to destruction")

delegitaggre_dic = list ("criminal zionist regime", "zionist criminal", "zionist butchers", "zionist crime", "zionist terrorist",
                         "inhuman activities of the zionist regime", "zionist thug", "terroristic zionist government",
                         "zionist regime's state terrorism","terrorist zionist regime", "zionists' acts of terrorism","aggressor regime",
                         "brutal zionist", "brutalities of the zionist entity", "oppressive zionist forces", "israeli military aggression",
                         "assassinated by the zionist", "criminal usurper","massacre of the people of palestine", "genocide in palestine",
                         "genocide in gaza", "genocide in lebanon", "murdering the people in lebanon","killing the people of lebanon",
                         "genocide of civilians", "zionist aggressor", "zionist murder", "armed-to-the-teeth zionist", "zionist atrocities")

delegitartific_dic = list("fake entity", "fake regime", "fake zionist regime", "fake government", "false state","fake zionist state", "artificial state",
                          "illegitimate regime", "illegitimate zionist regime", "illegitimate entity", "zionist regime is illegitimate", "not a country",
                          "imposed regime", "bogus zionist entity", "bogus regime", "bogus nation", "zionist entity", "nationless zionist state", "fake geographical unit",
                          "fake country", "fictitious zionist regime")

childmurd_dic = list("child-murdering", "murders children", "murder of infants", "bombs on muslim children",
                     "killings of palestinian children", "massacring children")

demonize_dic = list("malicious entity", "cancerous tumor", "deadly cancer","malignant tumor", "zionist outgrowth", "cancerous zionist tumor", "bloodthirsty zionist",
                   "zionist virus", "virus of zionism", "cancerous zionist regime", "cancerous tumor of zionism",
                   "evil zionist","zionist regime is a cancer", "malignant tumor", "malignant tumour", "anti-human zionism", "merciless regime","parasite",
                   "vicious zionist regime", "cancerous growth", "stone-hearted zionists", "bastard regime", "wolf-like zionists", "wolfish creature",
                   "blood-sucking wolf", "clutches of zionists", "clutches of the occupier", "clutches of that usurping power", "chameleon-like zionist",
                   "cannibalistic wolf", "dirty dog")

zionconspi_dic = list("international zionist network","global zionist network", "global zionism", "zionist lobbies", "zionist network",
                      "zionist lobbyists", "evil network", "zionist circles", "control government", "plots of the zionist", "powerful zionist",
                      "power of zionists", "wicked hands of the zionist", "influenced by zionist", "protocols of the elder of zion","the protocols of the elder of zion", "protocols of zionist intellectuals",
                      "zionist protocols", "protocols of the elder")

jewcapit_dic = list("jewish greed", "jewish capitalist", "jewish plutocrats", "jewish company owner", 
                    "jewish investor","jewish american capitalist")

zioncapit_dic = list("zionist greed", "zionist investor", "zionist owner","wealthy zionist",
                     "wealth of powerful zionists", "zionist capitalist", "zionist capitalist network",
                     "network of zionist capitalists", "zionist companies", "zionist company",
                     "financial apparatus", "zionist american capitalist")

zionmedia_dic = list("zionist controlled media", "zionist-controlled media", "zionist media", "zionist press and television network",
                    "zionist propagandist", "poisend public opinion", "zionist reports", "zionist radio",
                     "pro-zionist propaganda", "negative propaganda of the zionists", "control of the world media",
                     "monster of international propaganda")

jewzionconsp_dic = c(zionconspi_dic, jewcapit_dic, zioncapit_dic,zionmedia_dic)

enemyislam_dic = list("zionist policies are the enemy of islam", "foment discord among muslims", "kafer zionist regime",
                       "dagger in the heart of the islamic ummah", "planted in the heart of the islamic world", "attacks on islam",
                      "attacking islam", "implanted in the heart of the islamic world", "schisms among muslims","discord in the world of islam",
                      "discord among muslim nations", "pitch muslims against muslims", "destroy muslims",
                      "threat to the security of the islamic countries")


################################################################################
############################ANALYSES############################################
################################################################################

########################################################################
#####################Define a dictionary checking function##############
########################################################################

# Note:
# This function takes a dataframe and dictionary and credits a binary or column that checks whether any of the elements in 
# the dictionary are present in the text. It creates a new column in the respective dataframe.
# If in "detect" mode, it creates a binary whether any of the elements in the list are present.
# If in "count" mode, it counts the instances of keyword dictionaries in a text.
# It's inputs are as follows.
#               text_df - a dataframe, either with paragraphs or speeches
#               mode - either "count" or "detect", see above
#               keyword_list - a list of keywords that serves as dictionary
#               new_col_name - the name of the

check_dict = function(text_df = text_df, mode = "detect", keyword_list, new_col_name = "list_name"){
  text_df[,new_col_name] = 0
  for(keyterm in keyword_list){
    cat(keyterm)
    if(mode == "detect"){
      text_df[,new_col_name] = ifelse(str_detect(text_df$text, keyterm), 1, text_df[,new_col_name])
    }
    if(mode == "count"){
      text_df[,new_col_name] = text_df[,new_col_name]+lengths(str_extract_all(text_df$text, keyterm))
    }
    cat("Overal positive texts:", length(which(str_detect(text_df$text, keyterm))), "\n")
  }
  return(text_df)
}



##########################################################################
##### Israel, Zionism, Zionists and Jews

###########################################################
# Average number of words from the israelzion_dic and jewjud_dic by decade in comparison


# Step 1
speech_df = check_dict(text_df = speech_df, mode = "count",
                       keyword_list = israelzion_dic, new_col_name = "israelzion_dic_count")

speech_df = check_dict(text_df = speech_df, mode = "count",
                       keyword_list = jewjud_dic, new_col_name = "jewjud_dic_count")


# Step 2
plot_df = speech_df %>%
  # Group by decade
  group_by(decade) %>%
  # Summarize variable
  dplyr::summarize(israelzion_dic_count = mean(israelzion_dic_count, na.rm=TRUE),
                   jewjud_dic_count = mean(jewjud_dic_count, na.rm=TRUE)) %>%
  ungroup()

plot_df = gather(plot_df, "Dictionary", "Value",-decade)


# Step 3
plot_A = ggplot(plot_df, aes(x = decade, y = Value, group = Dictionary, color = Dictionary)) +
  geom_point() +
  geom_line() +

  labs(y="Durchschnittliche keyword-Anzahl\n pro Rede", x = "Jahrzehnt") +
  scale_colour_manual(name = "Dictionary", 
                      values =c("jewjud_dic_count"= "blue","israelzion_dic_count" = "red"), 
                      labels = c("israelzion_dic_count" = "Zionisten & Zionismus", "jewjud_dic_count" = "Juden & Judentum")) +
  theme_bw() +
  theme(legend.position = "bottom")
  
  

###########################################################
# Probability of presence of a term from the israelzion_dic and jewjud_dic by decade in comparison
# Combine both in one graph with different colors

# Step 1
speech_df = check_dict(text_df = speech_df, mode = "detect",
                       keyword_list = israelzion_dic, new_col_name = "israelzion_dic_count")
speech_df = check_dict(text_df = speech_df, mode = "detect",
                       keyword_list = jewjud_dic, new_col_name = "jewjud_dic_count")


# Step 2
plot_df = speech_df %>%
  # Group by decade
  group_by(decade) %>%
  # Summarize variable
  dplyr::summarize(israelzion_dic_count = mean(israelzion_dic_count, na.rm=TRUE),
                   jewjud_dic_count = mean(jewjud_dic_count, na.rm=TRUE)) %>%
  ungroup()

plot_df = gather(plot_df, "Dictionary", "Value",-decade)


# Step 3
plot_B = ggplot(plot_df, aes(x = decade, y = Value, group = Dictionary, color = Dictionary)) +
  geom_point() +
  geom_line() +
  
  labs(y="Anteil der Reden\nmit keyword", x = "Jahrzehnt") +
  scale_colour_manual(name = "Dictionary", 
                      values =c("jewjud_dic_count"= "blue","israelzion_dic_count"= "red"), 
                      labels = c("israelzion_dic_count" = "Zionisten & Zionismus", "jewjud_dic_count" = "Juden & Judentum")) +
  theme_bw() +
  theme(legend.position = "bottom")


####################################################################
# Combine plot A and B

install.packages("cowplot")
library(cowplot)

plot_grid(plot_A, plot_B, labels = c('A', 'B'))


####################################################################
# Probability of presence of a term from the israelzion_dic and jewjud_dic by year in comparison

# Step 1
speech_df = check_dict(text_df = speech_df, mode = "detect",
                       keyword_list = israelzion_dic, new_col_name = "israelzion_dic_count")
speech_df = check_dict(text_df = speech_df, mode = "detect",
                       keyword_list = jewjud_dic, new_col_name = "jewjud_dic_count")


# Step 2
plot_df = speech_df %>%
  # Group by Year
  group_by(Year) %>%
  # Summarize variable
  dplyr::summarize(israelzion_dic_count = mean(israelzion_dic_count, na.rm=TRUE),
                   jewjud_dic_count = mean(jewjud_dic_count, na.rm=TRUE)) %>%
  ungroup()

plot_df = gather(plot_df, "Dictionary", "Value",-Year)
plot_df$Dictionary = str_replace(plot_df$Dictionary, "jewjud_dic_count", "Juden & Judentum")
plot_df$Dictionary = str_replace(plot_df$Dictionary, "israelzion_dic_count", "Zionisten & Zionismus")
plot_df$Year = factor(plot_df$Year, levels = plot_df$Year)

ggplot(plot_df, aes(y = Value, x = 1)) + 
  geom_bar(stat="identity") +
  scale_x_discrete(drop = T) +
  scale_fill_manual(values = c("black"),drop=FALSE) +
  facet_grid(Dictionary~Year, scales = "free", switch = "both", space = "free_x") + 
  geom_hline(yintercept=0, size = .5) +
  labs(y="Anteil der Reden\nmit keyword", x = "Jahrzehnt") +
  
  theme_bw() + 
  theme(
    #legend.position = "bottom",
    #panel.background = element_blank(),
    #panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust=0.5),
    strip.text.x = element_text(angle = 90, vjust=0.5),
    panel.spacing.x = unit(0, "mm"), 
    panel.border = element_blank(),
    #axis.ticks.y = element_blank(),
    #axis.title.y = element_blank()
    strip.placement = "outside",
    strip.background.x = element_rect(colour="white", fill="white")
  )


####################################################################
# Average number of words from the israelzionjewjud_dic by year


# Step 1
speech_df = check_dict(text_df = speech_df, mode = "count",
                       keyword_list = israelzionjewjud_dic, new_col_name = "israelzionjewjud_dic_count")

# Step 2
plot_df = speech_df %>%
  # Group by Year
  group_by(Year) %>%
  # Summarize variable
  dplyr::summarize(israelzionjewjud_dic_count = mean(israelzionjewjud_dic_count, na.rm=TRUE)) %>%
  ungroup()

plot_df = gather(plot_df, "Dictionary", "Value",-Year)
plot_df$Dictionary = str_replace(plot_df$Dictionary, "israelzionjewjud_dic_count", "Zionisten, Zionismus, Juden und Judentum")
plot_df$Year = factor(plot_df$Year, levels = plot_df$Year)

ggplot(plot_df, aes(y = Value, x = 1)) + 
  geom_bar(stat="identity") +
  scale_x_discrete(drop = T) +
  scale_fill_manual(values = c("black"),drop=FALSE) +
  facet_grid(Dictionary~Year, scales = "free", switch = "both", space = "free_x") + 
  geom_hline(yintercept=0, size = .5) +
  labs(y="Durchschnittliche keyword-Anzahl\n pro Rede", x = "Jahr") +
  
  theme_bw() + 
  theme(
    #legend.position = "bottom",
    #panel.background = element_blank(),
    #panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust=0.5),
    strip.text.x = element_text(angle = 90, vjust=0.5),
    panel.spacing.x = unit(0, "mm"), 
    panel.border = element_blank(),
    #axis.ticks.y = element_blank(),
    #axis.title.y = element_blank()
    strip.placement = "outside",
    strip.background.x = element_rect(colour="white", fill="white")
  )


##################################################################
# Is antisemitism seasonal? Analysis of prevalence of keyterms by month.
# Average number of words from the israelzion_dic and jewjud_dic per month
# Combine both in one graph with different colors
# Set year in Year Specific Subset

# Step 1
speech_df = check_dict(text_df = speech_df, mode = "count",
                       keyword_list = unlist(israelzion_dic, jewjud_dic), new_col_name = "any_terms")


# Step 2
plot_df = speech_df %>%
  # Group by decade
  group_by(Month) %>%
  # Summarize variable
  dplyr::summarize(any_terms = mean(any_terms, na.rm=TRUE)) %>%
  ungroup()

plot_df = gather(plot_df, "Dictionary", "Value",-Month)
plot_df$Dictionary = str_replace(plot_df$Dictionary, "any_terms", "Zionisten, Zionismus, Juden und Judentum")
plot_df$Month = factor(plot_df$Month, levels = plot_df$Month)

ggplot(plot_df, aes(y = Value, x = 1)) + 
  geom_bar(stat="identity") +
  scale_x_discrete(drop = T) +
  scale_fill_manual(values = c("black"),drop=FALSE) +
  facet_grid(Dictionary~Month, scales = "free", switch = "both", space = "free_x") + 
  geom_hline(yintercept=0, size = .5) +
  labs(y="Durchschnittliche keyword-Anzahl\n pro Rede", x = "Monat") +
  
  theme_bw() + 
  theme(
    #legend.position = "bottom",
    #panel.background = element_blank(),
    #panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust=0.5),
    strip.text.x = element_text(angle = 90, vjust=0.5),
    panel.spacing.x = unit(0, "mm"), 
    panel.border = element_blank(),
    #axis.ticks.y = element_blank(),
    #axis.title.y = element_blank()
    strip.placement = "outside",
    strip.background.x = element_rect(colour="white", fill="white")
  )

#################################################################################
##### Antisemitic Motives


#### Holocaust & Extinction Threats
### Probability / Decade

# Step 1

speech_df = check_dict(text_df = speech_df, mode = "detect",
                       keyword_list = holocaustdenial_dic, new_col_name = "holocaustdenial_dic")

speech_df = check_dict(text_df = speech_df, mode = "detect",
                       keyword_list = extinction_dic, new_col_name = "extinction_dic")


# Step 2
plot_df = speech_df %>%
  # Group by decade
  group_by(decade) %>%
  # Summarize variable
  dplyr::summarize(holocaustdenial_dic = mean(holocaustdenial_dic, na.rm=TRUE),
                   extinction_dic = mean(extinction_dic, na.rm=TRUE)
  ) %>%
  ungroup()

plot_df = gather(plot_df, "Dictionary", "Value",-decade)

# Step 3

ggplot(plot_df, aes(x = decade, y = Value, group = Dictionary, color = Dictionary)) +
  geom_point() +
  geom_line() +
  
  labs(y="Anteil der Reden\nmit keyword", x = "Jahrzehnt") +
  scale_colour_manual(name = 'Dictionary', 
                      values =c("holocaustdenial_dic" = 'cornsilk4',
                                "extinction_dic" = 'gold'),
                      labels = c("holocaustdenial_dic" = 'Holocaustleugnung',
                                 "extinction_dic" = 'Vernichtungsdrohung')) +
  theme_bw() +
  theme(legend.position = "bottom")



#### Holocaust 
### Average / Year

# Step 1
speech_df = check_dict(text_df = speech_df, mode = "count",
                       keyword_list = holocaustdenial_dic, new_col_name = "holocaustdenial_dic")


# Step 2
plot_df = speech_df %>%
  # Group by Year
  group_by(Year) %>%
  # Summarize variable
  dplyr::summarize(holocaustdenial_dic = mean(holocaustdenial_dic, na.rm=TRUE)) %>%
  ungroup()

plot_df = gather(plot_df, "Dictionary", "Value",-Year)
plot_df$Dictionary = str_replace(plot_df$Dictionary, "holocaustdenial_dic", "Holocaustleugnung")
plot_df$Year = factor(plot_df$Year, levels = plot_df$Year)

ggplot(plot_df, aes(y = Value, x = 1)) + 
  geom_bar(stat="identity") +
  scale_x_discrete(drop = T) +
  scale_fill_manual(values = c("black"),drop=FALSE) +
  facet_grid(Dictionary~Year, scales = "free", switch = "both", space = "free_x") + 
  geom_hline(yintercept=0, size = .5) +
  labs(y="Durchschnittliche keyword-Anzahl\n pro Rede", x = "Jahr") +
  
  theme_bw() + 
  theme(
    #legend.position = "bottom",
    #panel.background = element_blank(),
    #panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust=0.5),
    strip.text.x = element_text(angle = 90, vjust=0.5),
    panel.spacing.x = unit(0, "mm"), 
    panel.border = element_blank(),
    #axis.ticks.y = element_blank(),
    #axis.title.y = element_blank()
    strip.placement = "outside",
    strip.background.x = element_rect(colour="white", fill="white")
  )


###################################################################################
#### Average number of words in YEAR 2020 -> Set Year in Year Specific Subset
#### extinction_dic


# Step 1
speech_df_y = check_dict(text_df = speech_df_y, mode = "count",
                         keyword_list = extinction_dic, new_col_name = "extinction_dic")

# Step 2
plot_df_y = speech_df_y  %>%
  group_by(Month) %>%
  dplyr::summarize(Mean = mean(extinction_dic, na.rm=TRUE)) %>%
  ungroup()

plot_df_y = gather(plot_df_y , "Dictionary", "Value",-Month)
plot_df_y $Dictionary = str_replace(plot_df_y $Dictionary, "extinction_dic", "Vernichtungsdrohungen")
plot_df_y$Month = factor(plot_df_y$Month, levels = plot_df_y$Month)


ggplot(plot_df_y, aes(y = Value, x = 1)) + 
  geom_bar(stat="identity") +
  scale_x_discrete(drop = T) +
  scale_fill_manual(values = c("black"),drop=FALSE) +
  facet_grid(Dictionary~Month, scales = "free", switch = "both", space = "free_x") + 
  geom_hline(yintercept=0, size = .5) +
  labs(y="Durchschnittliche keyword-Anzahl\n pro Rede", x = "Monat/2020") +
  
  theme_bw() + 
  theme(
    #legend.position = "bottom",
    #panel.background = element_blank(),
    #panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust=0.5),
    strip.text.x = element_text(angle = 90, vjust=0.5),
    panel.spacing.x = unit(0, "mm"), 
    panel.border = element_blank(),
    #axis.ticks.y = element_blank(),
    #axis.title.y = element_blank()
    strip.placement = "outside",
    strip.background.x = element_rect(colour="white", fill="white")
   )


#### Average number of words per YEAR in COMPARISON
#### delegitaggre_dic, delegitartific_dic

# Step 1
speech_df = check_dict(text_df = speech_df, mode = "count",
                       keyword_list = delegitaggre_dic, new_col_name = "delegitaggre_dic_count")
speech_df = check_dict(text_df = speech_df, mode = "count",
                       keyword_list = delegitartific_dic, new_col_name = "delegitartific_dic_count")


# Step 2
plot_df = speech_df %>%
  # Group by Year
  group_by(Year) %>%
  # Summarize variable
  dplyr::summarize(delegitaggre_dic_count = mean(delegitaggre_dic_count, na.rm=TRUE),
                   delegitartific_dic_count = mean(delegitartific_dic_count, na.rm=TRUE)) %>%
  ungroup()

plot_df = gather(plot_df, "Dictionary", "Value",-Year)
plot_df$Dictionary = str_replace(plot_df$Dictionary, "delegitaggre_dic_count", "Delegitimierung Aggressor")
plot_df$Dictionary = str_replace(plot_df$Dictionary, "delegitartific_dic_count", "Delegitimierung Künstlich & Illegitim")
plot_df$Year = factor(plot_df$Year, levels = plot_df$Year)

ggplot(plot_df, aes(y = Value, x = 1)) + 
  geom_bar(stat="identity") +
  scale_x_discrete(drop = T) +
  scale_fill_manual(values = c("black"),drop=FALSE) +
  facet_grid(Dictionary~Year, scales = "free", switch = "both", space = "free_x") + 
  geom_hline(yintercept=0, size = .5) +
  labs(y="Durchschnittliche keyword-Anzahl\n pro Rede", x = "Jahr") +
  
  theme_bw() + 
  theme(
    #legend.position = "bottom",
    #panel.background = element_blank(),
    #panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust=0.5),
    strip.text.x = element_text(angle = 90, vjust=0.5),
    panel.spacing.x = unit(0, "mm"), 
    panel.border = element_blank(),
    #axis.ticks.y = element_blank(),
    #axis.title.y = element_blank()
    strip.placement = "outside",
    strip.background.x = element_rect(colour="white", fill="white")
  )


###################################################################################
#### Probability of presence of a term per DECADE in COMPARISON
#### delegitaggre_dic, delegitartific_dic, demonize


# Step 1

speech_df = check_dict(text_df = speech_df, mode = "detect",
                       keyword_list = delegitaggre_dic, new_col_name = "delegitaggre_dic")

speech_df = check_dict(text_df = speech_df, mode = "detect",
                       keyword_list = delegitartific_dic, new_col_name = "delegitartific_dic")

speech_df = check_dict(text_df = speech_df, mode = "detect",
                       keyword_list = demonize_dic, new_col_name = "demonize_dic")


# Step 2
plot_df = speech_df %>%
  # Group by decade
  group_by(decade) %>%
  # Summarize variable
  dplyr::summarize(delegitaggre_dic = mean(delegitaggre_dic, na.rm=TRUE),
                   delegitartific_dic = mean(delegitartific_dic, na.rm=TRUE),
                   demonize_dic = mean(demonize_dic, na.rm=TRUE)
  ) %>%
  ungroup()

plot_df = gather(plot_df, "Dictionary", "Value",-decade)

# Step 3

ggplot(plot_df, aes(x = decade, y = Value, group = Dictionary, color = Dictionary)) +
  geom_point() +
  geom_line() +
  
  labs(y="Anteil der Reden\nmit keyword", x = "Jahrzehnt") +
  scale_colour_manual(name = 'Dictionary', 
                      values =c("delegitaggre_dic" = 'purple1',
                                "delegitartific_dic" = 'coral3' ,
                                "demonize_dic" = 'slateblue2'),
                      labels = c("delegitaggre_dic" = 'Delegitimierung Aggressor',
                                 "delegitartific_dic" = 'Delegitimierung Künstlich & Illegitim',
                                 "demonize_dic" = 'Dämonisierung')) +
  theme_bw() +
  theme(legend.position = "bottom")


###################################################################################
#### Average number of words per YEAR
#### demonize_dic


# Step 1
speech_df = check_dict(text_df = speech_df, mode = "count",
                       keyword_list = demonize_dic, new_col_name = "demonize_dic_count")


# Step 2
plot_df = speech_df %>%
  # Group by Year
  group_by(Year) %>%
  # Summarize variable
  dplyr::summarize(demonize_dic_count = mean(demonize_dic_count, na.rm=TRUE)) %>%
  ungroup()

plot_df = gather(plot_df, "Dictionary", "Value",-Year)
plot_df$Dictionary = str_replace(plot_df$Dictionary, "demonize_dic_count", "Dämonisierung")
plot_df$Year = factor(plot_df$Year, levels = plot_df$Year)

ggplot(plot_df, aes(y = Value, x = 1)) + 
  geom_bar(stat="identity") +
  scale_x_discrete(drop = T) +
  scale_fill_manual(values = c("black"),drop=FALSE) +
  facet_grid(Dictionary~Year, scales = "free", switch = "both", space = "free_x") + 
  geom_hline(yintercept=0, size = .5) +
  labs(y="Durchschnittliche keyword-Anzahl\n pro Rede", x = "Jahr") +
  
  theme_bw() + 
  theme(
    #legend.position = "bottom",
    #panel.background = element_blank(),
    #panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust=0.5),
    strip.text.x = element_text(angle = 90, vjust=0.5),
    panel.spacing.x = unit(0, "mm"), 
    panel.border = element_blank(),
    #axis.ticks.y = element_blank(),
    #axis.title.y = element_blank()
    strip.placement = "outside",
    strip.background.x = element_rect(colour="white", fill="white")
  )


###################################################################################
#### Probability of presence of a term per DECADE in COMPARISON
#### for holocaustdenial_dic, extinction_dic, delegitaggre_dic, delegitartific_dic,
#### demonize_dic, childmurd_dic, jewzionconsp_dic, enemy_islam

# Step 1
speech_df = check_dict(text_df = speech_df, mode = "detect",
                       keyword_list = holocaustdenial_dic, new_col_name = "holocaustdenial_dic")
# Step 1
speech_df = check_dict(text_df = speech_df, mode = "detect",
                       keyword_list = extinction_dic, new_col_name = "extinction_dic")

speech_df = check_dict(text_df = speech_df, mode = "detect",
                       keyword_list = delegitaggre_dic, new_col_name = "delegitaggre_dic")

speech_df = check_dict(text_df = speech_df, mode = "detect",
                       keyword_list = delegitartific_dic, new_col_name = "delegitartific_dic")

speech_df = check_dict(text_df = speech_df, mode = "detect",
                       keyword_list = demonize_dic, new_col_name = "demonize_dic")

speech_df = check_dict(text_df = speech_df, mode = "detect",
                       keyword_list = childmurd_dic, new_col_name = "childmurd_dic")

speech_df = check_dict(text_df = speech_df, mode = "detect",
                       keyword_list = jewzionconsp_dic, new_col_name = "jewzionconsp_dic")

speech_df = check_dict(text_df = speech_df, mode = "detect",
                       keyword_list = enemyislam_dic, new_col_name = "enemyislam_dic")

# Step 2
plot_df = speech_df %>%
  # Group by decade
  group_by(decade) %>%
  # Summarize variable
  dplyr::summarize(holocaustdenial_dic = mean(holocaustdenial_dic, na.rm=TRUE),
                   extinction_dic = mean(extinction_dic, na.rm=TRUE),
                   delegitaggre_dic = mean(delegitaggre_dic, na.rm=TRUE),
                   delegitartific_dic = mean(delegitartific_dic, na.rm=TRUE),
                   demonize_dic = mean(demonize_dic, na.rm=TRUE),
                   childmurd_dic = mean(childmurd_dic, na.rm=TRUE),
                   jewzionconsp_dic = mean(jewzionconsp_dic, na.rm=TRUE),
                   enemyislam_dic = mean(enemyislam_dic, na.rm=TRUE)
  ) %>%
  ungroup()

plot_df = gather(plot_df, "Dictionary", "Value",-decade)

# Step 3
ggplot(plot_df, aes(x = decade, y = Value, group = Dictionary, color = Dictionary)) +
  geom_point() +
  geom_line() +
  
  labs(y="Anteil der Reden\nmit keyword", x = "Jahrzehnt") +
  scale_colour_manual(name = 'Dictionary', 
                      values =c("holocaustdenial_dic" = 'cornsilk4',
                                "extinction_dic" = 'gold',
                                "delegitaggre_dic" = 'purple1',
                                "delegitartific_dic" = 'coral3' ,
                                "demonize_dic" = 'slateblue2',
                                "childmurd_dic" = 'brown1',
                                "jewzionconsp_dic" = 'darksalmon',
                                "enemyislam_dic" = 'chartreuse3'),
                      labels = c("holocaustdenial_dic" = 'Holocaustleugnung', 
                                 "extinction_dic" = 'Vernichtungsdrohung',
                                 "delegitaggre_dic" = 'Delegitimierung Aggressor',
                                 "delegitartific_dic" = 'Delegitimierung Künstlich & Illegitim',
                                 "demonize_dic" = 'Dämonisierung',
                                 "childmurd_dic" = 'Kindermörder',
                                 "jewzionconsp_dic" = 'Jüdisch-zionist. Verschw.',
                                 "enemyislam_dic" = 'Feind des Islam')) +
  theme_bw() +
  theme(legend.position = "bottom")


###################################################################################
#### Average number of words per YEAR
#### jewzionconspi_dic


# Step 1
speech_df = check_dict(text_df = speech_df, mode = "count",
                       keyword_list = jewzionconsp_dic, new_col_name = "jewzionconsp_dic_count")


# Step 2
plot_df = speech_df %>%
  # Group by Year
  group_by(Year) %>%
  # Summarize variable
  dplyr::summarize(jewzionconsp_dic_count = mean(jewzionconsp_dic_count, na.rm=TRUE)) %>%
  ungroup()

plot_df = gather(plot_df, "Dictionary", "Value",-Year)
plot_df$Dictionary = str_replace(plot_df$Dictionary, "jewzionconsp_dic_count", "Jüdisch-Zionist. Verschw.")
plot_df$Year = factor(plot_df$Year, levels = plot_df$Year)

ggplot(plot_df, aes(y = Value, x = 1)) + 
  geom_bar(stat="identity") +
  scale_x_discrete(drop = T) +
  scale_fill_manual(values = c("black"),drop=FALSE) +
  facet_grid(Dictionary~Year, scales = "free", switch = "both", space = "free_x") + 
  geom_hline(yintercept=0, size = .5) +
  labs(y="Durchschnittliche keyword-Anzahl\n pro Rede", x = "Jahr") +
  
  theme_bw() + 
  theme(
    #legend.position = "bottom",
    #panel.background = element_blank(),
    #panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust=0.5),
    strip.text.x = element_text(angle = 90, vjust=0.5),
    panel.spacing.x = unit(0, "mm"), 
    panel.border = element_blank(),
    #axis.ticks.y = element_blank(),
    #axis.title.y = element_blank()
    strip.placement = "outside",
    strip.background.x = element_rect(colour="white", fill="white")
  )


###################################################################################
#### Probability of presence of a term per YEAR
#### jewcapit_dic & zioncapit_dic


# Step 1
speech_df = check_dict(text_df = speech_df, mode = "detect",
                       keyword_list = jewcapit_dic, new_col_name = "jewcapit_dic_count")
speech_df = check_dict(text_df = speech_df, mode = "detect",
                       keyword_list = zioncapit_dic, new_col_name = "zioncapit_dic_count")


# Step 2
plot_df = speech_df %>%
  # Group by Year
  group_by(Year) %>%
  # Summarize variable
  dplyr::summarize(jewcapit_dic_count = mean(jewcapit_dic_count, na.rm=TRUE),
                   zioncapit_dic_count = mean(zioncapit_dic_count, na.rm=TRUE)) %>%
  ungroup()

plot_df = gather(plot_df, "Dictionary", "Value",-Year)
plot_df$Dictionary = str_replace(plot_df$Dictionary, "jewcapit_dic_count", "Finanzjudentum")
plot_df$Dictionary = str_replace(plot_df$Dictionary, "zioncapit_dic_count", "Finanzzionismus")
plot_df$Year = factor(plot_df$Year, levels = plot_df$Year)

ggplot(plot_df, aes(y = Value, x = 1)) + 
  geom_bar(stat="identity") +
  scale_x_discrete(drop = T) +
  scale_fill_manual(values = c("black"),drop=FALSE) +
  facet_grid(Dictionary~Year, scales = "free", switch = "both", space = "free_x") + 
  geom_hline(yintercept=0, size = .5) +
  labs(y="Anteil der Reden\nmit keyword", x = "Jahr") +
  
  theme_bw() + 
  theme(
    #legend.position = "bottom",
    #panel.background = element_blank(),
    #panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust=0.5),
    strip.text.x = element_text(angle = 90, vjust=0.5),
    panel.spacing.x = unit(0, "mm"), 
    panel.border = element_blank(),
    #axis.ticks.y = element_blank(),
    #axis.title.y = element_blank()
    strip.placement = "outside",
    strip.background.x = element_rect(colour="white", fill="white")
  )


###################################################################################
#### Probability of presence of a term  per YEAR
#### zionmedia_dic

# Step 1
speech_df = check_dict(text_df = speech_df, mode = "detect",
                       keyword_list = zionmedia_dic, new_col_name = "zionmedia_dic_count")


# Step 2
plot_df = speech_df %>%
  # Group by Year
  group_by(Year) %>%
  # Summarize variable
  dplyr::summarize(zionmedia_dic_count = mean(zionmedia_dic_count, na.rm=TRUE)) %>%
  ungroup()

plot_df = gather(plot_df, "Dictionary", "Value",-Year)
plot_df$Dictionary = str_replace(plot_df$Dictionary, "zionmedia_dic_count", "Zionistische Medien")
plot_df$Year = factor(plot_df$Year, levels = plot_df$Year)

ggplot(plot_df, aes(y = Value, x = 1)) + 
  geom_bar(stat="identity") +
  scale_x_discrete(drop = T) +
  scale_fill_manual(values = c("black"),drop=FALSE) +
  facet_grid(Dictionary~Year, scales = "free", switch = "both", space = "free_x") + 
  geom_hline(yintercept=0, size = .5) +
  labs(y="Anteil der Reden\nmit keyword", x = "Jahr") +
  
  theme_bw() + 
  theme(
    #legend.position = "bottom",
    #panel.background = element_blank(),
    #panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust=0.5),
    strip.text.x = element_text(angle = 90, vjust=0.5),
    panel.spacing.x = unit(0, "mm"), 
    panel.border = element_blank(),
    #axis.ticks.y = element_blank(),
    #axis.title.y = element_blank()
    strip.placement = "outside",
    strip.background.x = element_rect(colour="white", fill="white")
    )


###################################################################################
#### Probability of presence of a term per YEAR
#### enemyislam_dic

# Step 1
speech_df = check_dict(text_df = speech_df, mode = "detect",
                       keyword_list = enemyislam_dic, new_col_name = "enemyislam_dic_count")


# Step 2
plot_df = speech_df %>%
  # Group by Year
  group_by(Year) %>%
  # Summarize variable
  dplyr::summarize(enemyislam_dic_count = mean(enemyislam_dic_count, na.rm=TRUE)) %>%
  ungroup()

plot_df = gather(plot_df, "Dictionary", "Value",-Year)
plot_df$Dictionary = str_replace(plot_df$Dictionary, "enemyislam_dic_count", "Feind des Islam")
plot_df$Year = factor(plot_df$Year, levels = plot_df$Year)

ggplot(plot_df, aes(y = Value, x = 1)) + 
  geom_bar(stat="identity") +
  scale_x_discrete(drop = T) +
  scale_fill_manual(values = c("black"),drop=FALSE) +
  facet_grid(Dictionary~Year, scales = "free", switch = "both", space = "free_x") + 
  geom_hline(yintercept=0, size = .5) +
  labs(y="Anteil der Reden\nmit keyword", x = "Jahr") +
  
  theme_bw() + 
  theme(
    #legend.position = "bottom",
    #panel.background = element_blank(),
    #panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust=0.5),
    strip.text.x = element_text(angle = 90, vjust=0.5),
    panel.spacing.x = unit(0, "mm"), 
    panel.border = element_blank(),
    #axis.ticks.y = element_blank(),
    #axis.title.y = element_blank()
    strip.placement = "outside",
    strip.background.x = element_rect(colour="white", fill="white")
  )
