install.packages("readtext")
library(readtext)
a <- read()

?readtext

getwd()
setwd("C:/data/lda")
gd()

library(dplyr)
library(tidytext)
library(stopwords)
library(stringr)
library(quanteda)
library(textclean)
library(qdapRegex)
library(tm)
library(tidyr)


DATA_DIR <- system.file("extdata/", package = "readtext")
library(readtext)

##################### ОСТАВИТЬ ВСЁ  ######################
LA50S <- readtext(paste0(DATA_DIR, "/txt/diploma/LA/1950s*"), encoding = "UTF-8")
LA60S <- readtext(paste0(DATA_DIR, "/txt/diploma/LA/1960s*"), encoding = "UTF-8")
LA70S <- readtext(paste0(DATA_DIR, "/txt/diploma/LA/1970s*"), encoding = "UTF-8")
LA80S <- readtext(paste0(DATA_DIR, "/txt/diploma/LA/1980s*"), encoding = "UTF-8")
LA90S <- readtext(paste0(DATA_DIR, "/txt/diploma/LA/1990s*"), encoding = "UTF-8")

Chess30S <- readtext(paste0(DATA_DIR, "/txt/diploma/ChessUSSR/1930s*"), encoding = "UTF-8")
Chess40S <- readtext(paste0(DATA_DIR, "/txt/diploma/ChessUSSR/1940s*"), encoding = "UTF-8")
Chess50S <- readtext(paste0(DATA_DIR, "/txt/diploma/ChessUSSR/1950s*"), encoding = "UTF-8")
Chess60S <- readtext(paste0(DATA_DIR, "/txt/diploma/ChessUSSR/1960s*"), encoding = "UTF-8")
Chess70S <- readtext(paste0(DATA_DIR, "/txt/diploma/ChessUSSR/1970s*"), encoding = "UTF-8")
Chess80S <- readtext(paste0(DATA_DIR, "/txt/diploma/ChessUSSR/1980s*"), encoding = "UTF-8")
Chess90S <- readtext(paste0(DATA_DIR, "/txt/diploma/ChessUSSR/1990s*"), encoding = "UTF-8")

Soccer60s <- readtext(paste0(DATA_DIR, "/txt/diploma/Football/1960s*"), encoding = "UTF-8")
Soccer70s <- readtext(paste0(DATA_DIR, "/txt/diploma/Football/1970s*"), encoding = "UTF-8")
Soccer80s <- readtext(paste0(DATA_DIR, "/txt/diploma/Football/1980s*"), encoding = "UTF-8")
Soccer90s <- readtext(paste0(DATA_DIR, "/txt/diploma/Football/1990s*"), encoding = "UTF-8")

Fizra20s <- readtext(paste0(DATA_DIR, "/txt/diploma/PCultureSports/1920s*"), encoding = "UTF-8")
Fizra30s <- readtext(paste0(DATA_DIR, "/txt/diploma/PCultureSports/1930s*"), encoding = "UTF-8")
Fizra40s <- readtext(paste0(DATA_DIR, "/txt/diploma/PCultureSports/1940s*"), encoding = "UTF-8")
Fizra50s <- readtext(paste0(DATA_DIR, "/txt/diploma/PCultureSports/1950s*"), encoding = "UTF-8")
Fizra60s <- readtext(paste0(DATA_DIR, "/txt/diploma/PCultureSports/1960s*"), encoding = "UTF-8")
Fizra70s <- readtext(paste0(DATA_DIR, "/txt/diploma/PCultureSports/1970s*"), encoding = "UTF-8")
Fizra80s <- readtext(paste0(DATA_DIR, "/txt/diploma/PCultureSports/1980s*"), encoding = "UTF-8")
Fizra90s <- readtext(paste0(DATA_DIR, "/txt/diploma/PCultureSports/1990s*"), encoding = "UTF-8")

Fizra20s_Lem <- readtext(paste0(DATA_DIR, "/txt/diploma/PCultureSports_L/1920s*"), encoding = "UTF-8")
Fizra30s_Lem  <- readtext(paste0(DATA_DIR, "/txt/diploma/PCultureSports_L/1930s*"), encoding = "UTF-8")
Fizra40s_Lem  <- readtext(paste0(DATA_DIR, "/txt/diploma/PCultureSports_L/1940s*"), encoding = "UTF-8")
Fizra50s_Lem  <- readtext(paste0(DATA_DIR, "/txt/diploma/PCultureSports_L/1950s*"), encoding = "UTF-8")
Fizra60s_Lem  <- readtext(paste0(DATA_DIR, "/txt/diploma/PCultureSports_L/1960s*"), encoding = "UTF-8")
Fizra70s_Lem  <- readtext(paste0(DATA_DIR, "/txt/diploma/PCultureSports_L/1970s*"), encoding = "UTF-8")
Fizra80s_Lem  <- readtext(paste0(DATA_DIR, "/txt/diploma/PCultureSports_L/1980s*"), encoding = "UTF-8")
Fizra90s_Lem  <- readtext(paste0(DATA_DIR, "/txt/diploma/PCultureSports_L/1990s*"), encoding = "UTF-8")
library(stringr)
Fizra.lem20 <- str_replace_all(Fizra20s_Lem$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Fizra20_fin <- cbind(Fizra20s, Fizra.lem20)
Fizra.lem30 <- str_replace_all(Fizra30s_Lem$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Fizra30_fin <- cbind(Fizra30s, Fizra.lem30)
Fizra.lem40 <- str_replace_all(Fizra40s_Lem$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Fizra40_fin <- cbind(Fizra40s, Fizra.lem40)
Fizra.lem50 <- str_replace_all(Fizra50s_Lem$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Fizra50_fin <- cbind(Fizra50s, Fizra.lem50)
Fizra.lem60 <- str_replace_all(Fizra60s_Lem$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Fizra60_fin <- cbind(Fizra60s, Fizra.lem60)
Fizra.lem70 <- str_replace_all(Fizra70s_Lem$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Fizra70_fin <- cbind(Fizra70s, Fizra.lem70)
Fizra.lem80 <- str_replace_all(Fizra80s_Lem$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Fizra80_fin <- cbind(Fizra80s, Fizra.lem80)
Fizra.lem90 <- str_replace_all(Fizra90s_Lem$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Fizra90_fin <- cbind(Fizra90s, Fizra.lem90)




a <- bind_rows(Fizra20s, Fizra30s, Fizra40s, Fizra50s, Fizra60s, Fizra70s, Fizra80s, Fizra90s, Soccer60s, Soccer70s, Soccer80s, Soccer90s)
b <- bind_rows(Chess30S, Chess40S, Chess50S, Chess60S, Chess70S, Chess80S, Chess90S, LA50S, LA60S, LA70S, LA80S, LA90S)


с <-  a %>% unnest_tokens (word, text)

d <-  b %>% unnest_tokens (word, text)

e <- Fizra30s %>% unnest_tokens(word,text)







Soccer60s_l <- readtext(paste0(DATA_DIR, "/txt/diploma/Football_L/1960s*"), encoding = "UTF-8")
Soccer70s_l  <- readtext(paste0(DATA_DIR, "/txt/diploma/Football_L/1970s*"), encoding = "UTF-8")
Soccer80s_l  <- readtext(paste0(DATA_DIR, "/txt/diploma/Football_L/1980s*"), encoding = "UTF-8")
Soccer90s_l  <- readtext(paste0(DATA_DIR, "/txt/diploma/Football_L/1990s*"), encoding = "UTF-8")

Soccer.lem60 <- str_replace_all(Soccer60s_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Soccer60_fin <- cbind(Soccer60s, Soccer.lem60)
Soccer.lem70 <- str_replace_all(Soccer70s_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Soccer70_fin <- cbind(Soccer70s, Soccer.lem70)
Soccer.lem80 <- str_replace_all(Soccer80s_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Soccer80_fin <- cbind(Soccer80s, Soccer.lem80)
Soccer.lem90 <- str_replace_all(Soccer90s_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Soccer90_fin <- cbind(Soccer90s, Soccer.lem90)


LA50S_l <- readtext(paste0(DATA_DIR, "/txt/diploma/LA_L/1950s*"), encoding = "UTF-8")
LA60S_l <- readtext(paste0(DATA_DIR, "/txt/diploma/LA_L/1960s*"), encoding = "UTF-8")
LA70S_l <- readtext(paste0(DATA_DIR, "/txt/diploma/LA_L/1970s*"), encoding = "UTF-8")
LA80S_l <- readtext(paste0(DATA_DIR, "/txt/diploma/LA_L/1980s*"), encoding = "UTF-8")
LA90S_l <- readtext(paste0(DATA_DIR, "/txt/diploma/LA_L/1990s*"), encoding = "UTF-8")


LA.lem50 <- str_replace_all(LA50S_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
LA50_fin <- cbind(LA50S, LA.lem50)
LA.lem60 <- str_replace_all(LA60S_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
LA60_fin <- cbind(LA60S, LA.lem60)
LA.lem70 <- str_replace_all(LA70S_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
LA70_fin <- cbind(LA70S, LA.lem70)
LA.lem80 <- str_replace_all(LA80S_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
LA80_fin <- cbind(LA80S, LA.lem80)
LA.lem90 <- str_replace_all(LA90S_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
LA90_fin <- cbind(LA90S, LA.lem90)


Chess30_l <- readtext(paste0(DATA_DIR, "/txt/diploma/ChessUSSR_L/1930s*"), encoding = "UTF-8")
Chess40_l <- readtext(paste0(DATA_DIR, "/txt/diploma/ChessUSSR_L/1940s*"), encoding = "UTF-8")
Chess50_l <- readtext(paste0(DATA_DIR, "/txt/diploma/ChessUSSR_L/1950s*"), encoding = "UTF-8")
Chess60_l <- readtext(paste0(DATA_DIR, "/txt/diploma/ChessUSSR_L/1960s*"), encoding = "UTF-8")
Chess70_l <- readtext(paste0(DATA_DIR, "/txt/diploma/ChessUSSR_L/1970s*"), encoding = "UTF-8")
Chess80_l <- readtext(paste0(DATA_DIR, "/txt/diploma/ChessUSSR_L/1980s*"), encoding = "UTF-8")
Chess90_l <- readtext(paste0(DATA_DIR, "/txt/diploma/ChessUSSR_L/1990s*"), encoding = "UTF-8")

Сhess.lem30 <- str_replace_all(Chess30_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Chess30_fin <- cbind(Chess30S, Сhess.lem30)
Сhess.lem40 <- str_replace_all(Chess40_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Chess40_fin <- cbind(Chess40S, Сhess.lem40)
Сhess.lem50 <- str_replace_all(Chess50_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Chess50_fin <- cbind(Chess50S, Сhess.lem50)
Сhess.lem60 <- str_replace_all(Chess60_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Chess60_fin <- cbind(Chess60S, Сhess.lem60)
Сhess.lem70 <- str_replace_all(Chess70_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Chess70_fin <- cbind(Chess70S, Сhess.lem70)
Сhess.lem80 <- str_replace_all(Chess80_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Chess80_fin <- cbind(Chess80S, Сhess.lem80)
Сhess.lem90 <- str_replace_all(Chess90_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Chess90_fin <- cbind(Chess90S, Сhess.lem90)






table(Chess30_fin$doc_id)



setwd("C:/data")
a <- read.csv("c1_300.csv", sep = ";", encoding = "UTF-8")

?read.csv


a <- read.delim2(file = "c1_300.csv" , header = FALSE, sep = ";")

a <- read.table(file = "c1_300.csv" , header = FALSE, sep = ";")

install.packages("read_tsv")
library(read_tsv)
a <-read.delim(file = "c1_300.tsv" , header = FALSE, sep = " ", encoding = "UTF-8")                  

Soccer70_fin %>% head(10)







library(stopwords)
library(readr)
write_lines(stopwords("ru"), "stopwords.txt")
setwd("C:/data")
library(rJava)
library(mallet)
library(dplyr)
??setAlphaOptimization
mallet.instances <- mallet.import(id.array=as.character(Fizra80s$doc_id),
                                  text.array=Fizra80s$text,
                                  stoplist.file="stopwords.txt")

topic.model <- MalletLDA(num.topics=10) # number of topics
topic.model$loadDocuments(mallet.instances) 
topic.model$setAlphaOptimization(20, 50) # optimizing hyperparameters
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model) # frequency table
word.freqs %>% arrange(desc(doc.freq)) %>% head(10)
topic.model$train(500)
topic.model$maximize(10)
doc.topics <- mallet.doc.topics(topic.model, smoothed=TRUE, normalized=TRUE)
topic.words <- mallet.topic.words(topic.model, smoothed=TRUE, normalized=TRUE)
topic.labels <- mallet.topic.labels(topic.model, topic.words, 10)
for (k in 1:nrow(topic.words)) {
  top <- paste(mallet.top.words(topic.model, topic.words[k,], 10)$words,collapse=" ")
  cat(paste(k, top, "\n"))
}
top.docs <- function(doc.topics, topic, docs, top.n=10) {
  head(docs[order(-doc.topics[,topic])], top.n)
}
top.docs(doc.topics, 1, Fizra20s$text)











encoding = "UTF-8"

library(stringr)
setwd("C:/data")
text.tmp <- system2("mystem", c("-c", "-l", "-d"), input=Fizra20s$text, stdout=TRUE) 
text.lem <- str_replace_all(Fizra20s_Lem$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
text_df <- cbind(Fizra20s, text.lem)





?system2

View(text.lem)


table



library(dplyr)


check <- merge(LA50_fin, LA60_fin, LA70_fin, LA80_fin, LA90_fin, by )








#ДЕЛАЕМ#
library(stringr)
txt.lem <- str_replace_all(Fizra20s_Lem$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Fizra20_fin <- cbind(Fizra20s, txt.lem)
txt.lem <- str_replace_all(Fizra30s_Lem$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Fizra30_fin <- cbind(Fizra30s, txt.lem)
txt.lem <- str_replace_all(Fizra40s_Lem$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Fizra40_fin <- cbind(Fizra40s, txt.lem)
txt.lem <- str_replace_all(Fizra50s_Lem$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Fizra50_fin <- cbind(Fizra50s, txt.lem)
txt.lem <- str_replace_all(Fizra60s_Lem$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Fizra60_fin <- cbind(Fizra60s, txt.lem)
txt.lem <- str_replace_all(Fizra70s_Lem$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Fizra70_fin <- cbind(Fizra70s, txt.lem)
txt.lem <- str_replace_all(Fizra80s_Lem$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Fizra80_fin <- cbind(Fizra80s, txt.lem)
txt.lem <- str_replace_all(Fizra90s_Lem$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Fizra90_fin <- cbind(Fizra90s, txt.lem)


txt.lem <- str_replace_all(Soccer60s_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Soccer60_fin <- cbind(Soccer60s, txt.lem)
txt.lem <- str_replace_all(Soccer70s_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Soccer70_fin <- cbind(Soccer70s, txt.lem)
txt.lem <- str_replace_all(Soccer80s_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Soccer80_fin <- cbind(Soccer80s, txt.lem)
txt.lem <- str_replace_all(Soccer90s_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Soccer90_fin <- cbind(Soccer90s, txt.lem)


txt.lem <- str_replace_all(LA50S_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
LA50_fin <- cbind(LA50S, txt.lem)
txt.lem <- str_replace_all(LA60S_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
LA60_fin <- cbind(LA60S, txt.lem)
txt.lem <- str_replace_all(LA70S_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
LA70_fin <- cbind(LA70S, txt.lem)
txt.lem <- str_replace_all(LA80S_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
LA80_fin <- cbind(LA80S, txt.lem)
txt.lem <- str_replace_all(LA90S_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
LA90_fin <- cbind(LA90S, txt.lem)


txt.lem <- str_replace_all(Chess30_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Chess30_fin <- cbind(Chess30S, txt.lem)
txt.lem <- str_replace_all(Chess40_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Chess40_fin <- cbind(Chess40S, txt.lem)
txt.lem <- str_replace_all(Chess50_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Chess50_fin <- cbind(Chess50S, txt.lem)
txt.lem <-str_replace_all(Chess60_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Chess60_fin <- cbind(Chess60S, txt.lem)
txt.lem <- str_replace_all(Chess70_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Chess70_fin <- cbind(Chess70S, txt.lem)
txt.lem <- str_replace_all(Chess80_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Chess80_fin <- cbind(Chess80S, txt.lem)
txt.lem <- str_replace_all(Chess90_l$text, "\\{([^}]+?)([?]+)?\\}", "\\1")
Chess90_fin <- cbind(Chess90S, txt.lem)






CHESS_LEM <- dplyr::bind_rows(Chess30_fin, Chess40_fin, Chess50_fin, Chess60_fin, Chess70_fin, Chess80_fin, Chess90_fin)

CHESS_LEM1 <- dplyr::bind_rows(Chess30_fin, Chess40_fin, Chess90_fin)
CHESS_LEM2 <- dplyr::bind_rows(Chess50_fin)
CHESS_LEM3 <- dplyr::bind_rows(Chess60_fin)
CHESS_LEM4 <- dplyr::bind_rows(Chess70_fin)
CHESS_LEM5 <- dplyr::bind_rows(Chess80_fin)


FIZRA_LEM <- dplyr::bind_rows(Fizra20_fin, Fizra30_fin, Fizra40_fin, Fizra50_fin, Fizra60_fin, Fizra70_fin, Fizra80_fin, Fizra90_fin)

FIZRA_LEM1 <- dplyr::bind_rows(Fizra20_fin, Fizra40_fin)
FIZRA_LEM2 <- dplyr::bind_rows(Fizra30_fin)
FIZRA_LEM3 <- dplyr::bind_rows(Fizra50_fin, Fizra60_fin)
FIZRA_LEM4 <- dplyr::bind_rows(Fizra70_fin, Fizra90_fin)
FIZRA_LEM5 <- dplyr::bind_rows(Fizra80_fin)





LA_LEM <- dplyr::bind_rows(LA50_fin, LA60_fin, LA70_fin, LA80_fin, LA90_fin)


LA_LEM1 <- dplyr::bind_rows(LA50_fin, LA90_fin)
LA_LEM2 <- dplyr::bind_rows(LA60_fin)
LA_LEM3 <- dplyr::bind_rows(LA70_fin)
LA_LEM4 <- dplyr::bind_rows(LA80_fin)




SOCCER_LEM <- dplyr::bind_rows(Soccer60_fin, Soccer70_fin, Soccer80_fin, Soccer90_fin)


SOCCER_LEM1 <- dplyr::bind_rows(Soccer60_fin)
SOCCER_LEM2 <- dplyr::bind_rows(Soccer70_fin)
SOCCER_LEM3 <- dplyr::bind_rows(Soccer80_fin, Soccer90_fin)

?split
b <- as.data.frame(split(CHESS_LEM$txt.lem,10000))



c <- strsplit(CHESS_LEM$txt.lem, 100)





install.packages("topicmodels")
library(topicmodels)



smp_size_raw <- floor(0.75 * nrow(CHESS_LEM))
train_ind_raw <- sample(nrow(CHESS_LEM), size = smp_size_raw)
train_raw.df <- as.data.frame(CHESS_LEM[train_ind_raw, ])
test_raw.df <- as.data.frame(CHESS_LEM[-train_ind_raw, ])


smp_size_raw <- floor(0.1 * nrow(la_dtm))
train_ind_raw <- sample(nrow(CHESS_LEM), size = smp_size_raw)
train_raw.df <- as.data.frame(la_dtm[train_ind_raw, ])
test_raw.df <- as.data.frame(CHESS_LEM[-train_ind_raw, ])



?LDA
ldd <- LDA(Chess.dtm, k = 100)

library(dplyr)
library(tidytext)
library(stopwords)
library(stringr)
library(quanteda)
library(textclean)
library(qdapRegex)
library(tm)
library(tidyr)
corpus <- c("черный", "белый", "ход", "шахматы", "партия", "пешка", "футбол", "хоккей", "атлетика", "физкультура", "атлет", "футболист", "атлет", "шахматист", "еб", "ебать", "й", "ц", "у", "к", "е", "н", "г", "ш", "щ", "з", "х", "ъ", "ф", "ы", "в", "а",  "п", "р", "о", "л", "д", "ж", "э", "я", "ч", "с", "м", "и", "т", "ь", "б", "ю",  "ё")

N = 16
c = split(CHESS_LEM, sample(1:N, nrow(CHESS_LEM), replace = TRUE))

n <- 7
nr <- nrow(df)
c <- split(CHESS_LEM, rep(1:ceiling(nr/n), each=n, length.out=nr))


CHESS1 <- unsplit(c, f = 1)
CHESS2 <- unsplit(c, f = 2)
CHESS3 <- unsplit(c, f = 3)
CHESS4 <- unsplit(c, f = 4)
CHESS5 <- unsplit(c, f = 5)
CHESS6 <- unsplit(c, f = 6)
CHESS7 <- unsplit(c, f = 7)
CHESS8 <- unsplit(c, f = 8)
CHESS9 <- unsplit(c, f = 9)
CHESS10 <- unsplit(c, f = 10)
CHESS11 <- unsplit(c, f = 11)
CHESS12 <- unsplit(c, f = 12)
CHESS13 <- unsplit(c, f = 13)
CHESS14 <- unsplit(c, f = 14)
CHESS15 <- unsplit(c, f = 15)
CHESS16 <- unsplit(c, f = 16)

num_groups = 10

bb <- CHESS_LEM %>% 
  group_by((row_number()-1) %/% (n()/num_groups)) %>%
  nest %>% pull(data)






?textclean
Chess.dtm <- CHESS_LEM %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% stopwords("ru")) %>%
  filter(! stringr::str_detect(word, "[0-9]+")) %>%
  filter(! stringr::str_detect(word, "\\b\\w{1,2}\\b")) %>%
  filter(! word %in% corpus) %>%
  count(doc_id, word) %>%
  cast_dfm(doc_id, word, n) 



CHESS_LEM2$txt.lem <- as.character(CHESS_LEM2$txt.lem)
CHESS_LEM3$txt.lem <- as.character(CHESS_LEM3$txt.lem)
CHESS_LEM4$txt.lem <- as.character(CHESS_LEM4$txt.lem)
CHESS_LEM5$txt.lem <- as.character(CHESS_LEM5$txt.lem)


FIZRA_LEM2$txt.lem <- as.character(FIZRA_LEM2$txt.lem)
FIZRA_LEM3$txt.lem <- as.character(FIZRA_LEM3$txt.lem)
FIZRA_LEM4$txt.lem <- as.character(FIZRA_LEM4$txt.lem)
FIZRA_LEM5$txt.lem <- as.character(FIZRA_LEM5$txt.lem)

LA_LEM2$txt.lem <- as.character(LA_LEM2$txt.lem)
LA_LEM3$txt.lem <- as.character(LA_LEM3$txt.lem)
LA_LEM4$txt.lem <- as.character(LA_LEM4$txt.lem)

SOCCER_LEM1$txt.lem <- as.character(SOCCER_LEM1$txt.lem)
SOCCER_LEM2$txt.lem <- as.character(SOCCER_LEM2$txt.lem)
SOCCER_LEM3$txt.lem <- as.character(SOCCER_LEM3$txt.lem)


Chess.dtm1 <- CHESS_LEM1 %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% stopwords("ru")) %>%
  filter(! stringr::str_detect(word, "[0-9]+")) %>%
  filter(! stringr::str_detect(word, "\\b\\w{1,2}\\b")) %>%
  filter(! word %in% corpus) %>%
  count(doc_id, word) %>%
  cast_dfm(doc_id, word, n) 
Chess.dtm2 <- CHESS_LEM2 %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% stopwords("ru")) %>%
  filter(! stringr::str_detect(word, "[0-9]+")) %>%
  filter(! stringr::str_detect(word, "\\b\\w{1,2}\\b")) %>%
  filter(! word %in% corpus) %>%
  count(doc_id, word) %>%
  cast_dfm(doc_id, word, n) 
Chess.dtm3 <- CHESS_LEM3 %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% stopwords("ru")) %>%
  filter(! stringr::str_detect(word, "[0-9]+")) %>%
  filter(! stringr::str_detect(word, "\\b\\w{1,2}\\b")) %>%
  filter(! word %in% corpus) %>%
  count(doc_id, word) %>%
  cast_dfm(doc_id, word, n) 
Chess.dtm4 <- CHESS_LEM4 %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% stopwords("ru")) %>%
  filter(! stringr::str_detect(word, "[0-9]+")) %>%
  filter(! stringr::str_detect(word, "\\b\\w{1,2}\\b")) %>%
  filter(! word %in% corpus) %>%
  count(doc_id, word) %>%
  cast_dfm(doc_id, word, n) 
Chess.dtm5 <- CHESS_LEM5 %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% stopwords("ru")) %>%
  filter(! stringr::str_detect(word, "[0-9]+")) %>%
  filter(! stringr::str_detect(word, "\\b\\w{1,2}\\b")) %>%
  filter(! word %in% corpus) %>%
  count(doc_id, word) %>%
  cast_dfm(doc_id, word, n) 



Fizra.dtm1 <- FIZRA_LEM1 %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% stopwords("ru")) %>%
  filter(! stringr::str_detect(word, "[0-9]+")) %>%
  filter(! stringr::str_detect(word, "\\b\\w{1,2}\\b")) %>%
  filter(! word %in% corpus) %>%
  count(doc_id, word) %>%
  cast_dfm(doc_id, word, n) 
Fizra.dtm2 <- FIZRA_LEM2 %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% stopwords("ru")) %>%
  filter(! stringr::str_detect(word, "[0-9]+")) %>%
  filter(! stringr::str_detect(word, "\\b\\w{1,2}\\b")) %>%
  filter(! word %in% corpus) %>%
  count(doc_id, word) %>%
  cast_dfm(doc_id, word, n) 
Fizra.dtm3 <- FIZRA_LEM3 %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% stopwords("ru")) %>%
  filter(! stringr::str_detect(word, "[0-9]+")) %>%
  filter(! stringr::str_detect(word, "\\b\\w{1,2}\\b")) %>%
  filter(! word %in% corpus) %>%
  count(doc_id, word) %>%
  cast_dfm(doc_id, word, n) 
Fizra.dtm4 <- FIZRA_LEM4 %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% stopwords("ru")) %>%
  filter(! stringr::str_detect(word, "[0-9]+")) %>%
  filter(! stringr::str_detect(word, "\\b\\w{1,2}\\b")) %>%
  filter(! word %in% corpus) %>%
  count(doc_id, word) %>%
  cast_dfm(doc_id, word, n) 
Fizra.dtm5 <- FIZRA_LEM5 %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% stopwords("ru")) %>%
  filter(! stringr::str_detect(word, "[0-9]+")) %>%
  filter(! stringr::str_detect(word, "\\b\\w{1,2}\\b")) %>%
  filter(! word %in% corpus) %>%
  count(doc_id, word) %>%
  cast_dfm(doc_id, word, n) 


La.dtm1 <- LA_LEM1 %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% stopwords("ru")) %>%
  filter(! stringr::str_detect(word, "[0-9]+")) %>%
  filter(! stringr::str_detect(word, "\\b\\w{1,2}\\b")) %>%
  filter(! word %in% corpus) %>%
  count(doc_id, word) %>%
  cast_dfm(doc_id, word, n) 
La.dtm2 <- LA_LEM2 %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% stopwords("ru")) %>%
  filter(! stringr::str_detect(word, "[0-9]+")) %>%
  filter(! stringr::str_detect(word, "\\b\\w{1,2}\\b")) %>%
  filter(! word %in% corpus) %>%
  count(doc_id, word) %>%
  cast_dfm(doc_id, word, n) 
La.dtm3 <- LA_LEM3 %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% stopwords("ru")) %>%
  filter(! stringr::str_detect(word, "[0-9]+")) %>%
  filter(! stringr::str_detect(word, "\\b\\w{1,2}\\b")) %>%
  filter(! word %in% corpus) %>%
  count(doc_id, word) %>%
  cast_dfm(doc_id, word, n) 
La.dtm4 <- LA_LEM4 %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% stopwords("ru")) %>%
  filter(! stringr::str_detect(word, "[0-9]+")) %>%
  filter(! stringr::str_detect(word, "\\b\\w{1,2}\\b")) %>%
  filter(! word %in% corpus) %>%
  count(doc_id, word) %>%
  cast_dfm(doc_id, word, n) 





Soccer.dtm1 <- SOCCER_LEM1 %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% stopwords("ru")) %>%
  filter(! stringr::str_detect(word, "[0-9]+")) %>%
  filter(! stringr::str_detect(word, "\\b\\w{1,2}\\b")) %>%
  filter(! word %in% corpus) %>%
  count(doc_id, word) %>%
  cast_dfm(doc_id, word, n) 

Soccer.dtm2 <- SOCCER_LEM2 %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% stopwords("ru")) %>%
  filter(! stringr::str_detect(word, "[0-9]+")) %>%
  filter(! stringr::str_detect(word, "\\b\\w{1,2}\\b")) %>%
  filter(! word %in% corpus) %>%
  count(doc_id, word) %>%
  cast_dfm(doc_id, word, n) 

Soccer.dtm3 <- SOCCER_LEM3 %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% stopwords("ru")) %>%
  filter(! stringr::str_detect(word, "[0-9]+")) %>%
  filter(! stringr::str_detect(word, "\\b\\w{1,2}\\b")) %>%
  filter(! word %in% corpus) %>%
  count(doc_id, word) %>%
  cast_dfm(doc_id, word, n) 




View(c)



LA.dtm <- LA_LEM %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% stopwords("ru")) %>%
  filter(! stringr::str_detect(word, "[0-9]+")) %>%
  filter(! stringr::str_detect(word, "\\b\\w{1,2}\\b")) %>%
  filter(! word %in% corpus) %>%
  count(doc_id, word) %>%
  cast_dfm(doc_id, word, n) 

Fizra.dtm <- FIZRA_LEM %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% stopwords("ru")) %>%
  filter(! stringr::str_detect(word, "[0-9]+")) %>%
  filter(! word %in% corpus) %>%
  count(doc_id, word) %>%
  cast_dfm(doc_id, word, n) 

Soccer.dtm <- SOCCER_LEM %>%
  unnest_tokens(word, txt.lem) %>%
  filter(! word %in% stopwords("ru")) %>%
  filter(! stringr::str_detect(word, "[0-9]+")) %>%
  filter(! stringr::str_detect(word, "\\b\\w{1,2}\\b")) %>%
  filter(! word %in% corpus) %>%
  count(doc_id, word) %>%
  cast_dfm(doc_id, word, n) 

chess_dtm1 <- convert(Chess.dtm1, to = "topicmodels")
chess_dtm2 <- convert(Chess.dtm2, to = "topicmodels")
chess_dtm3 <- convert(Chess.dtm3, to = "topicmodels")
chess_dtm4 <- convert(Chess.dtm4, to = "topicmodels")
chess_dtm5 <- convert(Chess.dtm5, to = "topicmodels")

fizra_dtm1 <- convert(Fizra.dtm1, to = "topicmodels")
fizra_dtm2 <- convert(Fizra.dtm2, to = "topicmodels")
fizra_dtm3 <- convert(Fizra.dtm3, to = "topicmodels")
fizra_dtm4 <- convert(Fizra.dtm4, to = "topicmodels")
fizra_dtm5 <- convert(Fizra.dtm5, to = "topicmodels")


la_dtm1 <- convert(La.dtm1, to = "topicmodels")
la_dtm2 <- convert(La.dtm2, to = "topicmodels")
la_dtm3 <- convert(La.dtm3, to = "topicmodels")
la_dtm4 <- convert(La.dtm4, to = "topicmodels")

soccer_dtm1 <- convert(Soccer.dtm1, to = "topicmodels")
soccer_dtm2 <- convert(Soccer.dtm2, to = "topicmodels")
soccer_dtm3 <- convert(Soccer.dtm3, to = "topicmodels")

soccer_dtm <- convert(Soccer.dtm, to = "topicmodels")

fizra_dtm <- convert(Fizra.dtm, to = "topicmodels")

la_dtm <- convert(LA.dtm, to = "topicmodels")

library(topicmodels)


N = 10
chess_dtm_div= split(chess_dtm, sample(1:N, nrow(chess_dtm), replace=T))
chess_dtm_1 = unsplit(chess_dtm_div, f = 2)

CHESS_DIVIDED = split(CHESS_LEM, sample(1:N, nrow(CHESS_LEM), replace=T))
CHESS1 <- unsplit(c, f = 1)

#ЛДА
lda_chess1 <- LDA(chess_dtm1, k = 5)
lda_chess2 <- LDA(chess_dtm2, k = 5)
lda_chess3 <- LDA(chess_dtm3, k = 5)
lda_chess4 <- LDA(chess_dtm4, k = 5)
lda_chess5 <- LDA(chess_dtm5, k = 5)

lda_fizra1<- LDA(fizra_dtm1, k = 8)
lda_fizra2 <- LDA(fizra_dtm2, k = 8)
lda_fizra3 <- LDA(fizra_dtm3, k = 8)
lda_fizra4 <- LDA(fizra_dtm4, k = 8)
lda_fizra5 <- LDA(fizra_dtm5, k = 8)

lda_la1 <- LDA(la_dtm1, k = 8)
lda_la2 <- LDA(la_dtm2, k = 8)
lda_la3 <- LDA(la_dtm3, k = 8)
lda_la4 <- LDA(la_dtm4, k = 8)

lda_soc1 <- LDA(soccer_dtm1, k = 8)
lda_soc2 <- LDA(soccer_dtm2, k = 8)
lda_soc3 <- LDA(soccer_dtm3, k = 8)




lda_fizra <- LDA(fizra_dtm, k = 10)
lda_la <- LDA(la_dtm, k = 10)
lda_soc <- LDA(soccer_dtm, k = 10)

#




library(ggplot2)
library(dplyr)

library(tidytext)

ap_topics <- tidy(lda_soc3, matrix = "beta")
ap_topics

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()






ap_documents <- tidy(lda_soc3, matrix = "gamma")

ap_documents <- ap_documents %>%
  group_by(topic) %>%
  top_n(10, gamma) %>%
  ungroup() %>%
  arrange(topic, -gamma)

ap_documents %>%
  mutate(document = reorder_within(document, gamma, topic)) %>%
  ggplot(aes(document, gamma, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()






library(readtext)
?readtext
sportdict <- readLines("C:/data/sports_dict.txt")


sportdict <- readLines("C:/data/big_sport_dic.txt", encoding = "UTF-8")
sportdict <- gsub('[[:digit:]]+', '', sportdict)
sportdict <- as.character(sportdict)
sportdict <- lapply(sportdict, tolower)

sportdict_modified <- readLines("C:/data/big_sport_dic_nophysnosport.txt", encoding = "UTF-8")
sportdict_modified <- gsub('[[:digit:]]+', '', sportdict_modified)
sportdict_modified <- as.character(sportdict_modified)
sportdict_modified <- lapply(sportdict_modified, tolower)

sportdict_nochess <- readLines("C:/data/big_sport_dic_nochess.txt", encoding = "UTF-8")
sportdict_nochess <- gsub('[[:digit:]]+', '', sportdict_nochess)
sportdict_nochess <- as.character(sportdict_nochess)
sportdict_nochess <- lapply(sportdict_nochess, tolower)


sportdict_nosoc <- readLines("C:/data/big_sport_dic_nosoccer.txt", encoding = "UTF-8")
sportdict_nosoc <- gsub('[[:digit:]]+', '', sportdict_nosoc)
sportdict_nosoc <- as.character(sportdict_nosoc)
sportdict_nosoc <- lapply(sportdict_nosoc, tolower)


sportdict_noath <- readLines("C:/data/big_sport_dic_noathlete.txt", encoding = "UTF-8")
sportdict_noath <- gsub('[[:digit:]]+', '', sportdict_noath)
sportdict_noath <- as.character(sportdict_noath)
sportdict_noath <- lapply(sportdict_noath, tolower)



Fizra20_fin$txt.lem <- as.character(Fizra20_fin$txt.lem)
Fizra30_fin$txt.lem <- as.character(Fizra30_fin$txt.lem)
Fizra40_fin$txt.lem <- as.character(Fizra40_fin$txt.lem)
Fizra50_fin$txt.lem <- as.character(Fizra50_fin$txt.lem)
Fizra60_fin$txt.lem <- as.character(Fizra60_fin$txt.lem)
Fizra70_fin$txt.lem <- as.character(Fizra70_fin$txt.lem)
Fizra80_fin$txt.lem <- as.character(Fizra80_fin$txt.lem)
Fizra90_fin$txt.lem <- as.character(Fizra90_fin$txt.lem)



library(forcats)



pn.long_fizra20 <- Fizra20_fin %>%
  unnest_tokens(word, text) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  filter(word %in% sportdict_modified)  %>%
  select(-txt.lem) %>%
  group_by(doc_id) %>%
  count(word)%>%
  top_n(20, n) %>%
  ungroup() %>%
  arrange(word, -n)

pn.long_fizra20 %>%
  mutate(word = reorder_within(word, n, doc_id)) %>%
  ggplot(aes(word, n, fill = factor(doc_id))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ doc_id, scales = "free") +
  coord_flip() + 
  scale_x_reordered()


pn.long_fizra30 <- Fizra30_fin %>%
  unnest_tokens(word, text) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  filter(word %in% sportdict_modified)  %>%
  select(-txt.lem) %>%
  group_by(doc_id) %>%
  count(word)%>%
  top_n(20, n) %>%
  ungroup() %>%
  arrange(word, -n)




pn.long_fizra30 %>%
  mutate(word = reorder_within(word, n, doc_id)) %>%
  ggplot(aes(word, n, fill = factor(doc_id))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ doc_id, scales = "free") +
  coord_flip() + 
  scale_x_reordered()




pn.long_fizra40 <- Fizra40_fin %>%
  unnest_tokens(word, text) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  filter(word %in% sportdict_modified)  %>%
  select(-txt.lem) %>%
  group_by(doc_id) %>%
  count(word)%>%
  top_n(20, n) %>%
  ungroup() %>%
  arrange(word, -n)




pn.long_fizra40 %>%
  mutate(word = reorder_within(word, n, doc_id)) %>%
  ggplot(aes(word, n, fill = factor(doc_id))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ doc_id, scales = "free") +
  coord_flip() + 
  scale_x_reordered()




pn.long_fizra50 <- Fizra50_fin %>%
  unnest_tokens(word, text) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  filter(word %in% sportdict_modified)  %>%
  select(-txt.lem) %>%
  group_by(doc_id) %>%
  count(word)%>%
  top_n(20, n) %>%
  ungroup() %>%
  arrange(word, -n)




pn.long_fizra50 %>%
  mutate(word = reorder_within(word, n, doc_id)) %>%
  ggplot(aes(word, n, fill = factor(doc_id))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ doc_id, scales = "free") +
  coord_flip() + 
  scale_x_reordered()


pn.long_fizra60 <- Fizra60_fin %>%
  unnest_tokens(word, text) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  filter(word %in% sportdict_modified)  %>%
  select(-txt.lem) %>%
  group_by(doc_id) %>%
  count(word)%>%
  top_n(20, n) %>%
  ungroup() %>%
  arrange(word, -n)




pn.long_fizra60 %>%
  mutate(word = reorder_within(word, n, doc_id)) %>%
  ggplot(aes(word, n, fill = factor(doc_id))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ doc_id, scales = "free") +
  coord_flip() + 
  scale_x_reordered()


pn.long_fizra70 <- Fizra70_fin %>%
  unnest_tokens(word, text) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  filter(word %in% sportdict_modified)  %>%
  select(-txt.lem) %>%
  group_by(doc_id) %>%
  count(word)%>%
  top_n(20, n) %>%
  ungroup() %>%
  arrange(word, -n)




pn.long_fizra70 %>%
  mutate(word = reorder_within(word, n, doc_id)) %>%
  ggplot(aes(word, n, fill = factor(doc_id))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ doc_id, scales = "free") +
  coord_flip() + 
  scale_x_reordered()

pn.long_fizra80 <- Fizra80_fin %>%
  unnest_tokens(word, text) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  filter(word %in% sportdict_modified)  %>%
  select(-txt.lem) %>%
  group_by(doc_id) %>%
  count(word)%>%
  top_n(20, n) %>%
  ungroup() %>%
  arrange(word, -n)




pn.long_fizra80 %>%
  mutate(word = reorder_within(word, n, doc_id)) %>%
  ggplot(aes(word, n, fill = factor(doc_id))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ doc_id, scales = "free") +
  coord_flip() + 
  scale_x_reordered()


pn.long_fizra90 <- Fizra90_fin %>%
  unnest_tokens(word, text) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  filter(word %in% sportdict_modified)  %>%
  select(-txt.lem) %>%
  group_by(doc_id) %>%
  count(word)%>%
  top_n(20, n) %>%
  ungroup() %>%
  arrange(word, -n)




pn.long_fizra90 %>%
  mutate(word = reorder_within(word, n, doc_id)) %>%
  ggplot(aes(word, n, fill = factor(doc_id))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ doc_id, scales = "free") +
  coord_flip() + 
  scale_x_reordered()














pn.long_chess30 <- Chess30_fin %>%
  unnest_tokens(word, text) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  filter(word %in% sportdict_nochess)  %>%
  select(-txt.lem) %>%
  group_by(doc_id) %>%
  count(word)%>%
  top_n(20, n) %>%
  ungroup() %>%
  arrange(word, -n)




pn.long_chess30 %>%
  mutate(word = reorder_within(word, n, doc_id)) %>%
  ggplot(aes(word, n, fill = factor(doc_id))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ doc_id, scales = "free") +
  coord_flip() + 
  scale_x_reordered()




pn.long_chess40 <- Chess40_fin %>%
  unnest_tokens(word, text) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  filter(word %in% sportdict_nochess)  %>%
  select(-txt.lem) %>%
  group_by(doc_id) %>%
  count(word)%>%
  top_n(20, n) %>%
  ungroup() %>%
  arrange(word, -n)




pn.long_chess40 %>%
  mutate(word = reorder_within(word, n, doc_id)) %>%
  ggplot(aes(word, n, fill = factor(doc_id))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ doc_id, scales = "free") +
  coord_flip() + 
  scale_x_reordered()




pn.long_chess50 <- Chess50_fin %>%
  unnest_tokens(word, text) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  filter(word %in% sportdict_nochess)  %>%
  select(-txt.lem) %>%
  group_by(doc_id) %>%
  count(word)%>%
  top_n(20, n) %>%
  ungroup() %>%
  arrange(word, -n)




pn.long_chess50 %>%
  mutate(word = reorder_within(word, n, doc_id)) %>%
  ggplot(aes(word, n, fill = factor(doc_id))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ doc_id, scales = "free") +
  coord_flip() + 
  scale_x_reordered()


pn.long_chess60 <- Chess60_fin %>%
  unnest_tokens(word, text) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  filter(word %in% sportdict_nochess)  %>%
  select(-txt.lem) %>%
  group_by(doc_id) %>%
  count(word)%>%
  top_n(20, n) %>%
  ungroup() %>%
  arrange(word, -n)




pn.long_chess60 %>%
  mutate(word = reorder_within(word, n, doc_id)) %>%
  ggplot(aes(word, n, fill = factor(doc_id))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ doc_id, scales = "free") +
  coord_flip() + 
  scale_x_reordered()


pn.long_chess70 <- Chess70_fin %>%
  unnest_tokens(word, text) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  filter(word %in% sportdict_nochess)  %>%
  select(-txt.lem) %>%
  group_by(doc_id) %>%
  count(word)%>%
  top_n(20, n) %>%
  ungroup() %>%
  arrange(word, -n)




pn.long_chess70 %>%
  mutate(word = reorder_within(word, n, doc_id)) %>%
  ggplot(aes(word, n, fill = factor(doc_id))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ doc_id, scales = "free") +
  coord_flip() + 
  scale_x_reordered()

pn.long_chess80 <- Chess80_fin %>%
  unnest_tokens(word, text) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  filter(word %in% sportdict_nochess)  %>%
  select(-txt.lem) %>%
  group_by(doc_id) %>%
  count(word)%>%
  top_n(20, n) %>%
  ungroup() %>%
  arrange(word, -n)




pn.long_chess80 %>%
  mutate(word = reorder_within(word, n, doc_id)) %>%
  ggplot(aes(word, n, fill = factor(doc_id))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ doc_id, scales = "free") +
  coord_flip() + 
  scale_x_reordered()


pn.long_chess90 <- Chess90_fin %>%
  unnest_tokens(word, text) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  filter(word %in% sportdict_nochess)  %>%
  select(-txt.lem) %>%
  group_by(doc_id) %>%
  count(word)%>%
  top_n(20, n) %>%
  ungroup() %>%
  arrange(word, -n)




pn.long_chess90 %>%
  mutate(word = reorder_within(word, n, doc_id)) %>%
  ggplot(aes(word, n, fill = factor(doc_id))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ doc_id, scales = "free") +
  coord_flip() + 
  scale_x_reordered()







pn.long_soc60 <- Soccer60_fin %>%
  unnest_tokens(word, text) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  filter(word %in% sportdict_nosoc)  %>%
  select(-txt.lem) %>%
  group_by(doc_id) %>%
  count(word)%>%
  top_n(20, n) %>%
  ungroup() %>%
  arrange(word, -n)




pn.long_soc60 %>%
  mutate(word = reorder_within(word, n, doc_id)) %>%
  ggplot(aes(word, n, fill = factor(doc_id))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ doc_id, scales = "free") +
  coord_flip() + 
  scale_x_reordered()


pn.long_soc70 <- Soccer70_fin %>%
  unnest_tokens(word, text) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  filter(word %in% sportdict_nosoc)  %>%
  select(-txt.lem) %>%
  group_by(doc_id) %>%
  count(word)%>%
  top_n(20, n) %>%
  ungroup() %>%
  arrange(word, -n)




pn.long_soc70 %>%
  mutate(word = reorder_within(word, n, doc_id)) %>%
  ggplot(aes(word, n, fill = factor(doc_id))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ doc_id, scales = "free") +
  coord_flip() + 
  scale_x_reordered()

pn.long_soc80 <- Soccer80_fin %>%
  unnest_tokens(word, text) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  filter(word %in% sportdict_nosoc)  %>%
  select(-txt.lem) %>%
  group_by(doc_id) %>%
  count(word)%>%
  top_n(20, n) %>%
  ungroup() %>%
  arrange(word, -n)




pn.long_soc80 %>%
  mutate(word = reorder_within(word, n, doc_id)) %>%
  ggplot(aes(word, n, fill = factor(doc_id))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ doc_id, scales = "free") +
  coord_flip() + 
  scale_x_reordered()


pn.long_soc90 <- Soccer90_fin %>%
  unnest_tokens(word, text) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  filter(word %in% sportdict_nosoc)  %>%
  select(-txt.lem) %>%
  group_by(doc_id) %>%
  count(word)%>%
  top_n(20, n) %>%
  ungroup() %>%
  arrange(word, -n)




pn.long_soc90 %>%
  mutate(word = reorder_within(word, n, doc_id)) %>%
  ggplot(aes(word, n, fill = factor(doc_id))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ doc_id, scales = "free") +
  coord_flip() + 
  scale_x_reordered()



















pn.long_la50 <- LA50_fin %>%
  unnest_tokens(word, text) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  filter(word %in% sportdict_noath)  %>%
  select(-txt.lem) %>%
  group_by(doc_id) %>%
  count(word)%>%
  top_n(20, n) %>%
  ungroup() %>%
  arrange(word, -n)




pn.long_la50 %>%
  mutate(word = reorder_within(word, n, doc_id)) %>%
  ggplot(aes(word, n, fill = factor(doc_id))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ doc_id, scales = "free") +
  coord_flip() + 
  scale_x_reordered()


pn.long_la60 <- LA60_fin %>%
  unnest_tokens(word, text) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  filter(word %in% sportdict_noath)  %>%
  select(-txt.lem) %>%
  group_by(doc_id) %>%
  count(word)%>%
  top_n(20, n) %>%
  ungroup() %>%
  arrange(word, -n)




pn.long_la60 %>%
  mutate(word = reorder_within(word, n, doc_id)) %>%
  ggplot(aes(word, n, fill = factor(doc_id))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ doc_id, scales = "free") +
  coord_flip() + 
  scale_x_reordered()


pn.long_la70 <- LA70_fin %>%
  unnest_tokens(word, text) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  filter(word %in% sportdict_noath)  %>%
  select(-txt.lem) %>%
  group_by(doc_id) %>%
  count(word)%>%
  top_n(20, n) %>%
  ungroup() %>%
  arrange(word, -n)




pn.long_la70 %>%
  mutate(word = reorder_within(word, n, doc_id)) %>%
  ggplot(aes(word, n, fill = factor(doc_id))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ doc_id, scales = "free") +
  coord_flip() + 
  scale_x_reordered()

pn.long_la80 <- LA80_fin %>%
  unnest_tokens(word, text) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  filter(word %in% sportdict_noath)  %>%
  select(-txt.lem) %>%
  group_by(doc_id) %>%
  count(word)%>%
  top_n(20, n) %>%
  ungroup() %>%
  arrange(word, -n)




pn.long_la80 %>%
  mutate(word = reorder_within(word, n, doc_id)) %>%
  ggplot(aes(word, n, fill = factor(doc_id))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ doc_id, scales = "free") +
  coord_flip() + 
  scale_x_reordered()


pn.long_la90 <- LA90_fin %>%
  unnest_tokens(word, text) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  filter(word %in% sportdict_noath)  %>%
  select(-txt.lem) %>%
  group_by(doc_id) %>%
  count(word)%>%
  top_n(20, n) %>%
  ungroup() %>%
  arrange(word, -n)




pn.long_la90 %>%
  mutate(word = reorder_within(word, n, doc_id)) %>%
  ggplot(aes(word, n, fill = factor(doc_id))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ doc_id, scales = "free") +
  coord_flip() + 
  scale_x_reordered()












################СЕНТИМЕНТ АНАЛИЗ

library(stringr)
library(magrittr)
corp <- Corpus(VectorSource(Fizra20_fin$txt.lem)) ## корпус с исходными текстами
sentdict <- readr::read_csv("https://raw.githubusercontent.com/text-machine-lab/sentimental/master/sentimental/word_list/russian.csv")
sentdict <- sentdict[order(sentdict$word),] ## сортируем словарь в алфавитном порядке



library(janeaustenr)
library(dplyr)
library(stringr)

Fizra20_fin$txt.lem <- as.character(Fizra20_fin$txt.lem)
check <- Fizra20_fin %>%
  unnest_tokens(word, txt.lem)

dictionary <- filter(sentdict, score < 0)

table(Fizra20_fin$doc_id)

check %>%
  filter(doc_id == "1928.txt") %>%
  inner_join(dictionary) %>%
  count(word, sort = TRUE)



install.packages("mlxR")
library(magrittr)
library(mlxR)

vectors <- read.vector("ruscorpora_1_300_10.bin")



?readBin


vectors <- readBin("ruscorpora_1_300_10.bin", double())





library(devtools)
install_github("mukul13/rword2vec")

library(rword2vec)


is(rword2vec)



bin_to_txt("ruscorpora_1_300_10.bin","vector.txt")





install.packages("SentimentAnalysis")
library(SentimentAnalysis)

sentiment <- analyzeSentiment.Corpus(corp, language = "russian")

?analyzeSentiment.Corpus

corp <- Corpus(VectorSource(Fizra20_fin$txt.lem))






pn.long_la80_test <- LA80_fin %>%
  unnest_tokens(word, text) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  select(-txt.lem) %>%
  mutate(position = row_number()) 




library(dplyr)
library(lubridate)



library(dplyr)
library(tidytext)
library(stopwords)
library(stringr)
library(quanteda)
library(textclean)
library(qdapRegex)
library(tm)
library(tidyr)
library(fuzzyjoin)


nearby_words <- pn.long_la80_test %>%
  filter(word == "Амрика") %>%
  select(focus_term = word, focus_position = position) %>%
  difference_inner_join(pn.long_la80_test, by = c(focus_position = "position"), max_dist = 15) %>%
  mutate(distance = abs(focus_position - position))

words_summarized <- nearby_words %>%
  group_by(word) %>%
  summarize(number = n(),
            maximum_distance = max(distance),
            minimum_distance = min(distance),
            average_distance = mean(distance)) %>%
  arrange(desc(number))


nearby_words <- pn.long_la80_test %>%
  filter(word == "спорт")



pn.long_fizra80test <- Fizra80_fin %>%
  unnest_tokens(word, text) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  select(-txt.lem) %>%
  mutate(position = row_number()) 


nearby_words <- pn.long_fizra80test %>%
  filter(word == "сша") %>%
  select(focus_term = word, focus_position = position, doc_id) %>%
  difference_inner_join(pn.long_fizra80test, by = c(focus_position = "position"), max_dist = 5) %>%
  mutate(distance = abs(focus_position - position))

words_summarize_80<- nearby_words %>%
  group_by(word) %>%
  summarize(number = n(),
            maximum_distance = max(distance),
            minimum_distance = min(distance),
            average_distance = mean(distance)) %>%
  arrange(desc(number)) %>%
  inner_join(sentdict, by = "word") %>%
  mutate(avg.sent = mean(score))


pn.long_fizra30test <- Fizra30_fin %>%
  unnest_tokens(word, text) %>%
  filter(! word %in% tm::stopwords("ru")) %>%
  select(-txt.lem) %>%
  mutate(position = row_number()) 



pn.long_fizra30test_1 <- pn.long_fizra30test[1:1300000,]
pn.long_fizra30test_2 <- pn.long_fizra30test[1300001:2602638,]


gc()

nearby_words_30_1 <- pn.long_fizra30test_1  %>%
  filter(word == "сша") %>%
  select(focus_term = word, focus_position = position) %>%
  difference_inner_join(pn.long_fizra30test_1 , by = c(focus_position = "position"), max_dist = 5) %>%
  mutate(distance = abs(focus_position - position))

nearby_words_30_2 <- pn.long_fizra30test_2  %>%
  filter(word == "сша") %>%
  select(focus_term = word, focus_position = position, doc_id) %>%
  difference_inner_join(pn.long_fizra30test_2 , by = c(focus_position = "position"), max_dist = 5) %>%
  mutate(distance = abs(focus_position - position))



words_summarized_30_2 <- nearby_words_30_2 %>%
  group_by(word) %>%
  summarize(number = n(),
            maximum_distance = max(distance),
            minimum_distance = min(distance),
            average_distance = mean(distance)) %>%
  arrange(desc(number)) %>%
  inner_join(sentdict, by = "word") %>%
  mutate(avg.sent = mean(score))


sentdict <- readr::read_csv("https://raw.githubusercontent.com/text-machine-lab/sentimental/master/sentimental/word_list/russian.csv")


memory.limit(size=10000)







?write.csv




setwd("C:/data")
write.csv(x = LA50S, file = "LA50S.csv")
write.csv(x = LA60S, file = "LA60S.csv")
write.csv(x = LA70S, file = "LA70S.csv")
write.csv(x = LA80S, file = "LA80S.csv")
write.csv(x = LA90S, file = "LA90S.csv")



write.csv(x = Soccer60s, file = "Soccer60s.csv")
write.csv(x = Soccer70s, file = "Soccer70s.csv")
write.csv(x = Soccer80s, file = "Soccer80s.csv")
write.csv(x = Soccer90s, file = "Soccer90s.csv")




write.csv(x = Chess30S, file = "Chess30S.csv")
write.csv(x = Chess40S, file = "Chess40S.csv")
write.csv(x = Chess50S, file = "Chess50S.csv")
write.csv(x = Chess60S, file = "Chess60S.csv")
write.csv(x = Chess70S, file = "Chess70S.csv")
write.csv(x = Chess80S, file = "Chess80S.csv")
write.csv(x = Chess90S, file = "Chess90S.csv")


write.csv(x = Fizra20s, file = "Fizra20S.csv")
write.csv(x = Fizra30s, file = "Fizra30S.csv")
write.csv(x = Fizra40s, file = "Fizra40S.csv")
write.csv(x = Fizra50s, file = "Fizra50S.csv")
write.csv(x = Fizra60s, file = "Fizra60S.csv")
write.csv(x = Fizra70s, file = "Fizra70S.csv")
write.csv(x = Fizra80s, file = "Fizra80S.csv")
write.csv(x = Fizra90s, file = "Fizra90S.csv")
