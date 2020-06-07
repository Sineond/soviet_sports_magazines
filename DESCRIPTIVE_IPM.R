library(readxl)
setwd("C:/data")

games <- read_excel("Olympic_games.xlsx")

library(dplyr)
library(tidytext)
library(stopwords)
library(stringr)
library(quanteda)
library(textclean)
library(qdapRegex)
library(tm)
library(tidyr)
library(readtext)
DATA_DIR <- system.file("extdata/", package = "readtext")


Olympic_games.xlsx

Fizra20s_Lem <- readtext(paste0(DATA_DIR, "/txt/diploma/PCultureSports_L/1920s*"), encoding = "UTF-8")
Fizra30s_Lem  <- readtext(paste0(DATA_DIR, "/txt/diploma/PCultureSports_L/1930s*"), encoding = "UTF-8")
Fizra40s_Lem  <- readtext(paste0(DATA_DIR, "/txt/diploma/PCultureSports_L/1940s*"), encoding = "UTF-8")
Fizra50s_Lem  <- readtext(paste0(DATA_DIR, "/txt/diploma/PCultureSports_L/1950s*"), encoding = "UTF-8")
Fizra60s_Lem  <- readtext(paste0(DATA_DIR, "/txt/diploma/PCultureSports_L/1960s*"), encoding = "UTF-8")
Fizra70s_Lem  <- readtext(paste0(DATA_DIR, "/txt/diploma/PCultureSports_L/1970s*"), encoding = "UTF-8")
Fizra80s_Lem  <- readtext(paste0(DATA_DIR, "/txt/diploma/PCultureSports_L/1980s*"), encoding = "UTF-8")
Fizra90s_Lem  <- readtext(paste0(DATA_DIR, "/txt/diploma/PCultureSports_L/1990s*"), encoding = "UTF-8")

> grep("New", state.name)




sportdict_olymp <- readLines("C:/data/spotdict_olympic.txt", encoding = "UTF-8")
sportdict_olymp <- gsub('[[:digit:]]+', '', sportdict_olymp)
sportdict_olymp <- as.character(sportdict_olymp)
sportdict_olymp <- lapply(sportdict_olymp, tolower)


soccer_dict <- readLines("C:/data/soccer_dict.txt", encoding = "UTF-8")
soccer_dict <- gsub('[[:digit:]]+', '', soccer_dict)
soccer_dict <- as.character(soccer_dict)
soccer_dict <- lapply(soccer_dict, tolower)

spartak_dict <- readLines("C:/data/spartak_dict.txt", encoding = "UTF-8")
spartak_dict <- gsub('[[:digit:]]+', '', spartak_dict)
spartak_dict <- as.character(spartak_dict)
spartak_dict <- lapply(spartak_dict, tolower)


Fizra60s_Lem$txt



Fizra.lem20 <- str_replace_all(Fizra20s_Lem$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Fizra20s_Lem$txt.lem = Fizra.lem20

Fizra.lem30 <- str_replace_all(Fizra30s_Lem$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Fizra30s_Lem$txt.lem = Fizra.lem30

Fizra.lem40 <- str_replace_all(Fizra40s_Lem$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Fizra40s_Lem$txt.lem = Fizra.lem40

Fizra.lem50 <- str_replace_all(Fizra50s_Lem$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Fizra50s_Lem$txt.lem = Fizra.lem50


Fizra.lem60 <- str_replace_all(Fizra60s_Lem$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Fizra60s_Lem$txt.lem = Fizra.lem60

Fizra.lem70 <- str_replace_all(Fizra70s_Lem$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Fizra70s_Lem$txt.lem = Fizra.lem70

Fizra.lem80 <- str_replace_all(Fizra80s_Lem$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Fizra80s_Lem$txt.lem = Fizra.lem80

Fizra.lem90 <- str_replace_all(Fizra90s_Lem$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Fizra90s_Lem$txt.lem = Fizra.lem90

Soccer.lem60 <- str_replace_all(Soccer60s_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Soccer60s_l$txt.lem = Soccer.lem60

Soccer.lem70 <- str_replace_all(Soccer70s_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Soccer70s_l$txt.lem = Soccer.lem70

Soccer.lem80 <- str_replace_all(Soccer80s_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Soccer80s_l$txt.lem = Soccer.lem80

Soccer.lem90 <- str_replace_all(Soccer90s_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Soccer90s_l$txt.lem = Soccer.lem90


LA.lem50<- str_replace_all(LA50S_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
LA50S_l$txt.lem = LA.lem50

LA.lem60 <- str_replace_all(LA60S_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
LA60S_l$txt.lem = LA.lem60

LA.lem70 <- str_replace_all(LA70S_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
LA70S_l$txt.lem = LA.lem70

LA.lem80 <- str_replace_all(LA80S_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
LA80S_l$txt.lem = LA.lem80

LA.lem90 <- str_replace_all(LA90S_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
LA90S_l$txt.lem = LA.lem90


a <- bind_rows(Fizra20s_Lem, Fizra30s_Lem, Fizra40s_Lem, Fizra50s_Lem, Fizra60s_Lem, Fizra70s_Lem, Fizra80s_Lem, Fizra90s_Lem)
a <- select(a, -text)
grep("сексуальный", a$text)


ha <- nochess_doc_topics400 %>% filter(X3 == 393)


b <- bind_rows(Soccer60s_l, Soccer70s_l, Soccer80s_l, Soccer90s_l)
b <- select(b, -text)


d <- bind_rows(LA50S_l, LA60S_l, LA70S_l, LA80S_l, LA90S_l)
d <- select(d, -text)

  
10-2


pn.long_fiz60 <- Fizra60s_Lem %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  group_by(doc_id) %>%
  mutate(num_of_words = n()) %>%
  filter(word %in% sportdict_olymp)  %>%
  select(-text) %>% 
  mutate (mentions = n()) %>%
  group_by(doc_id) %>%
  summarise(mentions = unique(mentions), num_of_words = unique(num_of_words)) %>%
  mutate(proportion = (mentions/num_of_words)*1000)


pn.long_fizra_olympic <- a %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  group_by(doc_id) %>%
  mutate(num_of_words = n()) %>%
  filter(word %in% sportdict_olymp)  %>%
  mutate (mentions = n()) %>%
  group_by(doc_id) %>%
  summarise(mentions = unique(mentions), num_of_words = unique(num_of_words)) %>%
  mutate(proportion = (mentions/num_of_words)*1000)



pn.long_soccer_olympic <- b %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  group_by(doc_id) %>%
  mutate(num_of_words = n()) %>%
  filter(word %in% sportdict_olymp)  %>%
  mutate (mentions = n()) %>%
  group_by(doc_id) %>%
  summarise(mentions = unique(mentions), num_of_words = unique(num_of_words)) %>%
  mutate(proportion = (mentions/num_of_words)*1000)



pn.long_LA_olympic <- d %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  group_by(doc_id) %>%
  mutate(num_of_words = n()) %>%
  filter(word %in% sportdict_olymp)  %>%
  mutate (mentions = n()) %>%
  group_by(doc_id) %>%
  summarise(mentions = unique(mentions), num_of_words = unique(num_of_words)) %>%
  mutate(proportion = (mentions/num_of_words)*1000)



summarise


pn.long_fiz1960 <- Fizra60s_Lem %>%
  filter(doc_id == "1960_lem.txt") %>% 
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% tm::stopwords("ru"))

pn.long_fiz1964 <- Fizra60s_Lem %>%
  filter(doc_id == "1964_lem.txt") %>% 
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% tm::stopwords("ru"))

(895+237)/274674
(117+31)/22635




setwd("C:/data")
write.xlsx(pn.long_soccer_olympic, "soccer_olympic.xlsx")
?write.xlsx

install.packages("ggplot2")
library(ggplot2)

update.packages(ggplot2)

old.packages()
install.packages("hrbrthemes")
update.packages()


library(ggplot2)
library(dplyr)
library(hrbrthemes)
fizra_olympic %>%
  filter (Proportion > 0) %>%
  ggplot( aes(x=Year, y=Proportion)) +
  geom_line( color="black", size = 1.4) +
  geom_point(shape=21, color="black", fill="#69b3a2", size=7) +
  theme_ipsum() +
  theme(axis.text.x=element_text(size=rel(2))) +
  theme(axis.text.y=element_text(size=rel(2))) +
  ggtitle("Changes in mentions of Olympic Games in Physical Culture and Sports journal")

LA_olympic %>%
  filter (Proportion > 0) %>%
  ggplot( aes(x=Year, y=Proportion)) +
  geom_line( color="black", size = 1.4) +
  geom_point(shape=21, color="black", fill="#69b3a2", size=7) +
  theme_ipsum() +
  theme(axis.text.x=element_text(size=rel(2))) +
  theme(axis.text.y=element_text(size=rel(2))) +
  ggtitle("Changes in mentions of Olympic Games in Athletics journal")

soccer_olympic %>%
  filter (Proportion > 0) %>%
  ggplot( aes(x=Year, y=Proportion)) +
  geom_line( color="black", size = 1.4) +
  geom_point(shape=21, color="black", fill="#69b3a2", size=7) +
  theme_ipsum() +
  theme(axis.text.x=element_text(size=rel(2))) +
  theme(axis.text.y=element_text(size=rel(2))) +
  ggtitle("Changes in mentions of Olympic Games in Soccer.Hockey journal")



install.packages("ggplot2")


pn.long_fizra_soccer <- a %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  group_by(doc_id) %>%
  mutate(num_of_words = n()) %>%
  filter(word %in% soccer_dict)  %>%
  mutate (mentions = n()) %>%
  group_by(doc_id) %>%
  summarise(mentions = unique(mentions), num_of_words = unique(num_of_words)) %>%
  mutate(proportion = (mentions/num_of_words)*1000)

soccer_fizra %>%
  filter (Proportion > 0) %>%
  ggplot( aes(x=Year, y=Proportion)) +
  geom_line( color="black", size = 1.4) +
  geom_point(shape=21, color="black", fill="#69b3a2", size=7) +
  theme_ipsum() +
  theme(axis.text.x=element_text(size=rel(2))) +
  theme(axis.text.y=element_text(size=rel(2))) +
  ggtitle("Changes in mentions of Soccer in Physical Culture and Sports journal")

pn.long_fizra_spartak <- a %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  group_by(doc_id) %>%
  mutate(num_of_words = n()) %>%
  filter(word %in% spartak_dict)  %>%
  mutate (mentions = n()) %>%
  group_by(doc_id) %>%
  summarise(mentions = unique(mentions), num_of_words = unique(num_of_words)) %>%
  mutate(proportion = (mentions/num_of_words)*1000)



library(openxlsx)
write.xlsx(pn.long_fizra_soccer, "soccer_fizra.xlsx")

write.xlsx(pn.long_fizra_spartak, "fizra_spartak.xlsx")



fizra_spartak %>%
  filter (Proportion > 0) %>%
  ggplot( aes(x=Year, y=Proportion)) +
  geom_line( color="black", size = 0.5) +
  geom_point(shape=21, color="black", fill="#69b3a2", size=7) +
  theme_ipsum() +
  theme(axis.text.x=element_text(size=rel(2))) +
  theme(axis.text.y=element_text(size=rel(2))) +
  ggtitle("Changes in mentions of Spartakiad in Physical Culture and Sports journal")

