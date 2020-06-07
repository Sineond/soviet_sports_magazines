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
library(openxlsx)
library(stringr)
library(ggplot2)
library(hrbrthemes)
install.packages("janitor")
update.packages("tidyselect")
remove.packages("rlang")
install.packages("rlang")

library(openxlsx)

library(janitor)
setwd("C:/data")
dict <- read.xlsx("dict_analysis.xlsx")

dict$word<- gsub('[[:digit:]]+', '', dict$word)
dict <- na.omit(dict)

dict <- dict[!(is.na(dict$word) | dict$word==""), ]
dict$word <- str_to_lower(dict$word)




test_soccer <- a %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  group_by(doc_id) %>%
  mutate(num_of_words = n()) %>%
  filter(word %in% dict$word)  %>%
  inner_join(dict) %>%
  filter(sports == "soccer") %>%
  mutate (mentions = n()) %>%
  group_by(doc_id) %>%
  summarise(mentions = unique(mentions), num_of_words = unique(num_of_words)) %>%
  mutate(proportion = (mentions/num_of_words)*1000) %>%
  mutate(year = regmatches(doc_id, gregexpr("\\d{4}", doc_id))) 
  
  


test_hockey <- a %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  group_by(doc_id) %>%
  mutate(num_of_words = n()) %>%
  filter(word %in% dict$word)  %>%
  inner_join(dict) %>%
  filter(sports == "hockey") %>%
  mutate (mentions = n()) %>%
  group_by(doc_id) %>%
  summarise(mentions = unique(mentions), num_of_words = unique(num_of_words)) %>%
  mutate(proportion = (mentions/num_of_words)*1000) %>%
  mutate(year = regmatches(doc_id, gregexpr("\\d{4}", doc_id))) 

  

test_basketball <- a %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  group_by(doc_id) %>%
  mutate(num_of_words = n()) %>%
  filter(word %in% dict$word)  %>%
  inner_join(dict) %>%
  filter(sports == "basketball") %>%
  mutate (mentions = n()) %>%
  group_by(doc_id) %>%
  summarise(mentions = unique(mentions), num_of_words = unique(num_of_words)) %>%
  mutate(proportion = (mentions/num_of_words)*1000) %>%
  mutate(year = regmatches(doc_id, gregexpr("\\d{4}", doc_id)))

test_handball <- a %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  group_by(doc_id) %>%
  mutate(num_of_words = n()) %>%
  filter(word %in% dict$word)  %>%
  inner_join(dict) %>%
  filter(sports == "handball") %>%
  mutate (mentions = n()) %>%
  group_by(doc_id) %>%
  summarise(mentions = unique(mentions), num_of_words = unique(num_of_words)) %>%
  mutate(proportion = (mentions/num_of_words)*1000) %>%
  mutate(year = regmatches(doc_id, gregexpr("\\d{4}", doc_id))) 

  

test_polo <- a %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  group_by(doc_id) %>%
  mutate(num_of_words = n()) %>%
  filter(word %in% dict$word)  %>%
  inner_join(dict) %>%
  filter(sports == "polo") %>%
  mutate (mentions = n()) %>%
  group_by(doc_id) %>%
  summarise(mentions = unique(mentions), num_of_words = unique(num_of_words)) %>%
  mutate(proportion = (mentions/num_of_words)*1000) %>%
  mutate(year = regmatches(doc_id, gregexpr("\\d{4}", doc_id))) 



test_volleyball <- a %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  group_by(doc_id) %>%
  mutate(num_of_words = n()) %>%
  filter(word %in% dict$word)  %>%
  inner_join(dict) %>%
  filter(sports == "volleyball") %>%
  mutate (mentions = n()) %>%
  group_by(doc_id) %>%
  summarise(mentions = unique(mentions), num_of_words = unique(num_of_words)) %>%
  mutate(proportion = (mentions/num_of_words)*1000) %>%
  mutate(year = regmatches(doc_id, gregexpr("\\d{4}", doc_id))) 

test_soccer$year <- as.numeric(test_soccer$year)
test_hockey$year <- as.numeric(test_hockey$year)
test_volleyball$year <- as.numeric(test_volleyball$year)
test_handball$year <- as.numeric(test_handball$year)
test_polo$year <- as.numeric(test_polo$year)
test_basketball$year <- as.numeric(test_basketball$year)


test_soccer %>%
  ggplot( aes(x=year, y=proportion)) +
  geom_line( color="black", size = 1.4) +
  geom_point(shape=21, color="black", fill="#69b3a2", size=7) +
  theme_ipsum() +
  theme(axis.text.x=element_text(size=rel(2))) +
  theme(axis.text.y=element_text(size=rel(2))) +
  ggtitle("Changes in mentions of Soccer-related words in Physical Culture and Sports journal")

test_basketball %>%
  ggplot( aes(x=year, y=proportion)) +
  geom_line( color="black", size = 1.4) +
  geom_point(shape=21, color="black", fill="#69b3a2", size=7) +
  theme_ipsum() +
  theme(axis.text.x=element_text(size=rel(2))) +
  theme(axis.text.y=element_text(size=rel(2))) +
  ggtitle("Changes in mentions of Basketball-related words in Physical Culture and Sports journal")

test_handball %>%
  ggplot( aes(x=year, y=proportion)) +
  geom_line( color="black", size = 1.4) +
  geom_point(shape=21, color="black", fill="#69b3a2", size=7) +
  theme_ipsum() +
  theme(axis.text.x=element_text(size=rel(2))) +
  theme(axis.text.y=element_text(size=rel(2))) +
  ggtitle("Changes in mentions of Handball-related words in Physical Culture and Sports journal")

test_volleyball %>%
  ggplot( aes(x=year, y=proportion)) +
  geom_line( color="black", size = 1.4) +
  geom_point(shape=21, color="black", fill="#69b3a2", size=7) +
  theme_ipsum() +
  theme(axis.text.x=element_text(size=rel(2))) +
  theme(axis.text.y=element_text(size=rel(2))) +
  ggtitle("Changes in mentions of Volleyball-related words in Physical Culture and Sports journal")


test_polo %>%
  ggplot( aes(x=year, y=proportion)) +
  geom_line( color="black", size = 1.4) +
  geom_point(shape=21, color="black", fill="#69b3a2", size=7) +
  theme_ipsum() +
  theme(axis.text.x=element_text(size=rel(2))) +
  theme(axis.text.y=element_text(size=rel(2))) +
  ggtitle("Changes in mentions of Water-polo related words in Physical Culture and Sports journal")

test_hockey %>%
  ggplot( aes(x=year, y=proportion)) +
  geom_line( color="black", size = 1.4) +
  geom_point(shape=21, color="black", fill="#69b3a2", size=7) +
  theme_ipsum() +
  theme(axis.text.x=element_text(size=rel(2))) +
  theme(axis.text.y=element_text(size=rel(2))) +
  ggtitle("Changes in mentions of Hockey related words in Physical Culture and Sports journal")


test_soccer <- a %>%
  mutate(year = regmatches(doc_id, gregexpr("\\d{4}", doc_id))) %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  group_by(doc_id) %>%
  mutate(num_of_words = n()) %>%
  filter(word %in% dict$word)  %>%
  inner_join(dict) %>%
  filter(type == "team") %>%
  mutate (mentions = n()) %>%
  group_by(doc_id) %>%
  summarise(mentions = unique(mentions), num_of_words = unique(num_of_words)) %>%
  mutate(proportion = (mentions/num_of_words)*1000)

test_hockey <- a %>%
  mutate(year = regmatches(doc_id, gregexpr("\\d{4}", doc_id))) %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  group_by(doc_id) %>%
  mutate(num_of_words = n()) %>%
  filter(word %in% dict$word)  %>%
  inner_join(dict) %>%
  filter(type == "team") %>%
  mutate (mentions = n()) %>%
  group_by(doc_id) %>%
  summarise(mentions = unique(mentions), num_of_words = unique(num_of_words)) %>%
  mutate(proportion = (mentions/num_of_words)*1000)

test_team <- a %>%
  mutate(year = regmatches(doc_id, gregexpr("\\d{4}", doc_id))) %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  group_by(doc_id) %>%
  mutate(num_of_words = n()) %>%
  filter(word %in% dict$word)  %>%
  inner_join(dict) %>%
  filter(type == "team") %>%
  mutate (mentions = n()) %>%
  group_by(doc_id) %>%
  summarise(mentions = unique(mentions), num_of_words = unique(num_of_words)) %>%
  mutate(proportion = (mentions/num_of_words)*1000)



test_individual <- a %>%
  mutate(year = regmatches(doc_id, gregexpr("\\d{4}", doc_id))) %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  group_by(doc_id) %>%
  mutate(num_of_words = n()) %>%
  filter(word %in% dict$word)  %>%
  inner_join(dict) %>%
  filter(type == "individual") %>%
  mutate (mentions = n()) %>%
  group_by(doc_id) %>%
  summarise(mentions = unique(mentions), num_of_words = unique(num_of_words)) %>%
  mutate(proportion = (mentions/num_of_words)*1000) %>%
  



test_team$year <- as.numeric(test_team$year)
test_individual$year <- as.numeric(test_individual$year)

test_team %>%
  ggplot( aes(x=year, y=proportion)) +
  geom_line( color="black", size = 1.4) +
  geom_point(shape=21, color="black", fill="#69b3a2", size=7) +
  theme_ipsum() +
  theme(axis.text.x=element_text(size=rel(2))) +
  theme(axis.text.y=element_text(size=rel(2))) +
  ggtitle("Changes in mentions of individual games in Physical Culture and Sports journal")




test_team$year <- regmatches(test_team$doc_id, gregexpr("\\d{4}", test_team$doc_id))
test_individual$year <- regmatches(test_individual$doc_id, gregexpr("\\d{4}", test_individual$doc_id))

test_team <- test_team %>% mutate(olymp = ifelse(year == 1936 | year == 1960 | year == 1964 | year == 1980 | year == 1984 | year == 1988, "yes", "no"))
test_individual <- test_individual %>% mutate(olymp = ifelse(year == 1936 | year == 1960 | year == 1964 | year == 1980 | year == 1984 | year == 1988, "yes", "no"))






test_team <- test_team %>% mutate(period = ifelse(year >= 1928 & year <= 1953, "stalinism",
                  ifelse(year >=1954 & year <=1978, "post-stalin era", "1980s and perestroika")))

test_individual <- test_individual %>% mutate(period = ifelse(year >= 1928 & year <= 1953, "stalinism",
                                                  ifelse(year >=1954 & year <=1978, "post-stalin era", "1980s and perestroika")))




test_t <- select(test_team, olymp, mentions) %>% group_by(olymp) %>% summarise(amount = sum(mentions))
test_t <- t(test_t)
test_t <- as.data.frame(test_t)


test_t$stalinism	<- test_t$V3 
test_t$poststalin_era	<- test_t$V2 
test_t$perestroika_1980s	<- test_t$V1 
test_t$V1 <- NULL
test_t$V2 <- NULL
test_t$V3 <- NULL

test_i <- select(test_individual, period, mentions) %>% group_by(period) %>% summarise(amount = sum(mentions))
test_i <- t(test_i)
test_i <- as.data.frame(test_i)
test_i$stalinism	<- test_i$V3 
test_i$poststalin_era	<- test_i$V2 
test_i$perestroika_1980s	<- test_i$V1 



test_i$V1 <- NULL
test_i$V2 <- NULL
test_i$V3 <- NULL



test_tt <- test_t[-1, ]
test_ii <- test_i[-1, ]


ccc <- bind_rows(test_tt, test_ii)
ccc$perestroika_1980s <- as.integer(ccc$perestroika_1980s)
ccc$stalinism <- as.integer(ccc$stalinism)
ccc$poststalin_era <- as.integer(ccc$poststalin_era)


View(cc)
rownames(ccc) = c('team', 'individual')
install.packages("vcd")
ccc
library(vcd)
n <- chisq.test(dd)
n
View(dd)
b$residuals

dd <- as.table(as.matrix(ccc))
dd <- as.data.frame(dd)
dd$Freq <- as.numeric(dd$Freq)
View(dd)

b_resid = as.data.frame(b$residuals)
b_resid

b_count = as.data.frame(b$observed)
b_count

n_resid = as.data.frame(n$residuals)
n_resid

n_count = as.data.frame(n$observed)
n_count



ggplot() + 
  geom_raster(data = n_resid, aes(x = Var2, y = Var1, fill = Freq), hjust = 0.5, vjust = 0.5) + 
  scale_fill_gradient2("Pearson residuals", low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", midpoint = 0) +
  geom_text(data = n_count, aes(x = Var2, y = Var1, label = Freq)) +
  xlab("Period") +
  ylab("Game_type") +
  theme_bw()



print(chisq.test(dd))
print(dd)

summary.aov(res.aov)

res.aov <- aov(proportion ~ period, data = test_team)
summary(res.aov)
tukey <- TukeyHSD(res.aov)
tukey
plot(tukey, asp = 1)
?plot


assoc(dd, shade=TRUE, legends = TRUE, gp_axis = gpar(lty = 1))

print(ccc)



sportdict_olymp <- readLines("C:/data/spotdict_olympic.txt", encoding = "UTF-8")
sportdict_olymp <- gsub('[[:digit:]]+', '', sportdict_olymp)
sportdict_olymp <- as.character(sportdict_olymp)
sportdict_olymp <- lapply(sportdict_olymp, tolower)





test_olymp_fizra <- a %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  group_by(doc_id) %>%
  mutate(num_of_words = n()) %>%
  filter(word %in% sportdict_olymp)  %>%
  mutate (mentions = n()) %>%
  group_by(doc_id) %>%
  summarise(mentions = unique(mentions), num_of_words = unique(num_of_words)) %>%
  mutate(proportion = (mentions/num_of_words)*1000) %>%
  mutate(year = regmatches(doc_id, gregexpr("\\d{4}", doc_id))) %>%
  mutate(period = ifelse(year >= 1928 & year <= 1953, "stalinism",
                             ifelse(year >=1954 & year <=1978, "post-stalin era", "1980s and perestroika")))

test_olymp_soccer <- b %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  group_by(doc_id) %>%
  mutate(num_of_words = n()) %>%
  filter(word %in% sportdict_olymp)  %>%
  mutate (mentions = n()) %>%
  group_by(doc_id) %>%
  summarise(mentions = unique(mentions), num_of_words = unique(num_of_words)) %>%
  mutate(proportion = (mentions/num_of_words)*1000) %>%
  mutate(year = regmatches(doc_id, gregexpr("\\d{4}", doc_id))) %>%
  mutate(period = ifelse(year >= 1955 & year <= 1970, "Thaw and stagnation",
                         ifelse(year >=1971 & year <=1984, "Late USSR", "Perestroika")))


test_olymp_LA <- d %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  group_by(doc_id) %>%
  mutate(num_of_words = n()) %>%
  filter(word %in% sportdict_olymp)  %>%
  mutate (mentions = n()) %>%
  group_by(doc_id) %>%
  summarise(mentions = unique(mentions), num_of_words = unique(num_of_words)) %>%
  mutate(proportion = (mentions/num_of_words)*1000) %>%
  mutate(year = regmatches(doc_id, gregexpr("\\d{4}", doc_id))) %>%
  mutate(period = ifelse(year >= 1955 & year <= 1970, "Thaw and stagnation",
                         ifelse(year >=1971 & year <=1984, "Late USSR", "Perestroika")))



res.aov <- aov(proportion ~ period, data = test_olymp_fizra)
summary(res.aov)
tukey <- TukeyHSD(res.aov)
tukey
plot(tukey, asp = 2)


t_test_soc <- t.test(proportion ~ period, data = test_olymp_soccer )

t_test_LA <- t.test(proportion ~ period, data = test_olymp_LA)
summary(t_test_soc)


res.aov_soc <- aov(proportion ~ period, data = test_olymp_soccer)
summary(res.aov_soc)
tukey <- TukeyHSD(res.aov_soc)
tukey
plot(tukey, asp = 2)

res.aov_LA <- aov(proportion ~ period, data = test_olymp_LA)
summary(res.aov_LA)
tukey <- TukeyHSD(res.aov_LA)
tukey
plot(tukey, asp = 1.5)

?aov


hist(test_olymp_fizra$proportion)








t_test_olymp_ind <- t.test(proportion ~ olymp, data = test_individual )

t_test_olymp_team<- t.test(proportion ~ olymp, data = test_team)
summary(t_test_olymp_ind)







