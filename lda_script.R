lda_res <- nochess_doc_topics400
lda_res$year <- as.factor(lda_res$year)
lda_res$year <- regmatches(lda_res$X2, gregexpr("\\d{4}", lda_res$X2))
lda_res$year <- str_replace_all(lda_res$year, "[[:punct:]]", " ")
lda_res$year <- str_replace_all(lda_res$year, "c", " ")

lda_res$year <- lda_res$year[!lda_res$year %in% -1]

lda_res$year[1,] <- NULL

lda_res1 <- separate(lda_res, year, into = c("decade", "true_year", "spam"), sep = "    ")
lda_res1$X1 <- NULL

lda_res1$doc_id <- lda_res1$X2
lda_res1$topic1 <- lda_res1$X3
lda_res1$topic1_prop <- lda_res1$X4
lda_res1$topic2 <- lda_res1$X5
lda_res1$topic2_prop <- lda_res1$X6
lda_res1$topic3 <- lda_res1$X7
lda_res1$topic3_prop <- lda_res1$X8
lda_res1$topic4 <- lda_res1$X9
lda_res1$topic4_prop <- lda_res1$X10
lda_res1$topic5 <- lda_res1$X11
lda_res1$topic50_prop <- lda_res1$X12
lda_res1$topic6 <- lda_res1$X13
lda_res1$topic6_prop <- lda_res1$X14
lda_res1$topic7 <- lda_res1$X15
lda_res1$topic7_prop <- lda_res1$X16
lda_res1$topic8 <- lda_res1$X17
lda_res1$topic8_prop <- lda_res1$X18
lda_res1$topic9 <- lda_res1$X19
lda_res1$topic9_prop <- lda_res1$X20
lda_res1$topic10 <- lda_res1$X21
lda_res1$topic10_prop <- lda_res1$X22

lda_res1$X2 <- NULL
lda_res1$X3 <- NULL
lda_res1$X4 <- NULL
lda_res1$X5 <- NULL
lda_res1$X6 <- NULL
lda_res1$X7 <- NULL
lda_res1$X8 <- NULL
lda_res1$X9 <- NULL
lda_res1$X10 <- NULL
lda_res1$X11 <- NULL
lda_res1$X12 <- NULL
lda_res1$X13 <- NULL
lda_res1$X14 <- NULL
lda_res1$X15 <- NULL
lda_res1$X16 <- NULL
lda_res1$X17 <- NULL
lda_res1$X18 <- NULL
lda_res1$X19 <- NULL
lda_res1$X20 <- NULL
lda_res1$X21 <- NULL
lda_res1$X22 <- NULL

lda_res1 <- separate(lda_res1, doc_id, into = c("journal", "b", "c"), sep = "/")
lda_res1$spam <- NULL
lda_res1$b <- NULL
lda_res1$c <- NULL


lda_res1$topic <- lda_res1$topic1
lda_res1$topic_prop <- lda_res1$topic1_prop


lda_res1$topic1 <- NULL
lda_res1$topic1_prop <- NULL




dataset_lda <- full_join(lda_res1, summary_400)




test_topic_physicalculture <- dataset_lda %>%
  filter( journal ==	"PCultureSports_L") %>%
  filter(topic == 393 | topic2 == 393 | topic3 == 393| topic4 == 393| topic5 == 393| topic6 == 393| topic7 == 393| topic8 == 393| topic9 == 393| topic10 == 393)
  group_by(true_year) %>%
  top_n(5, topic_prop) %>%
  ungroup()  %>%
  select(true_year, top_words, journal, topic, topic_prop) %>%
  distinct(true_year, topic, .keep_all = TRUE)


test_topic_physicalculture_sex <- dataset_lda %>%
    filter( journal ==	"PCultureSports_L") %>%
    filter(topic == 393 | topic2 == 393 | topic3 == 393| topic4 == 393| topic5 == 393| topic6 == 393| topic7 == 393| topic8 == 393| topic9 == 393| topic10 == 393)
  
  
  
  

test_topic <- dataset_lda %>%
  arrange_(~ desc(topic_prop)) %>%
  group_by_(~ true_year) %>%
  slice(1:5)



setwd("C:/data/lda")
lda_nochess <- read.delim(file = "nochess400-dtfull.tsv" , header = FALSE, encoding = "UTF-8")

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


lda_nochess$year <- regmatches(lda_nochess$V2, gregexpr("\\d{4}", lda_nochess$V2))
lda_nochess$year <- str_replace_all(lda_nochess$year, "[[:punct:]]", " ")
lda_nochess$year <- str_replace_all(lda_nochess$year, "c", " ")
lda_nochess_years <- separate(lda_nochess, year, into = c("decade", "true_year", "spam"), sep = "    ")
lda_nochess_years$year = lda_nochess_years$true_year
lda_nochess_years$true_year <- NULL
lda_nochess_years$decade <- NULL
lda_nochess_years$spam <- NULL
lda_nochess_years$V1 <- NULL


lda_nochess_years <- separate(lda_nochess_years, V2, into = c("journal", "b", "c"), sep = "/")
lda_nochess_years$b <- NULL
lda_nochess_years$c <- NULL

lda_nochess_years$year <- trimws(lda_nochess_years$year)
lda_nochess_years$year <- as.numeric(lda_nochess_years$year)



class(lda_nochess$V396)




test_military <- lda_nochess_years %>%
  select(journal, V186, year) %>%
  group_by(year) %>%
  summarise(Militarism = sum(V186)) %>%
  mutate(period = ifelse(year >= 1928 & year <= 1953, "Stalinism",
                         ifelse(year >=1954 & year <=1970, "Thaw and Stagnation", ifelse(year >=1971 & year <=1984, "Late USSR", "Perestroika"))))



test_socialism <- lda_nochess_years %>%
  dplyr::select(journal, V96, year) %>%
  filter(year != 1938) %>%
  group_by(year) %>%
  summarise(Socialism = sum(V96)) %>%
  mutate(period = ifelse(year >= 1928 & year <= 1953, "Stalinism",
                         ifelse(year >=1954 & year <=1970, "Thaw and Stagnation", ifelse(year >=1971 & year <=1984, "Late USSR", "Perestroika"))))

test_capitalism <- lda_nochess_years %>%
  select(journal, V145, year) %>%
  group_by(year) %>%
  summarise(Capitalism = sum(V145)) %>%
  mutate(period = ifelse(year >= 1928 & year <= 1953, "Stalinism",
                         ifelse(year >=1954 & year <=1970, "Thaw and Stagnation", ifelse(year >=1971 & year <=1984, "Late USSR", "Perestroika"))))

test_westerncountries <- lda_nochess_years %>%
  select(journal, V5, year) %>%
  group_by(year) %>%
  summarise(West_countries = sum(V5)) %>%
  mutate(period = ifelse(year >= 1928 & year <= 1953, "Stalinism",
                         ifelse(year >=1954 & year <=1970, "Thaw and Stagnation", ifelse(year >=1971 & year <=1984, "Late USSR", "Perestroika"))))


?plot

res.aov_militari <- aov(Militarism ~ period, data = test_military)
summary(res.aov_militari)
tukey <- TukeyHSD(res.aov_militari)
tukey
plot(tukey, pch=19, col.axis = 'blue', col.lab = 'red', cex.axis = 0.6, cex.lab = 2, asp = 1.5)
plotTukeyHSD(tukey)
plotTukeyHSD <- plotTukeysHSD <- function(tukey.out,
                                          x.axis.label = "Comparison",
                                          y.axis.label = "Effect Size",
                                          axis.adjust = 0,
                                          adjust.x.spacing = 5){
  
  tukey.out <- as.data.frame(tukey.out[[1]])
  means <- tukey.out$diff
  categories <- row.names(tukey.out)
  groups <- length(categories)
  ci.low <- tukey.out$lwr
  ci.up  <- tukey.out$upr                         
  
  n.means <- length(means)
  
  #determine where to plot points along x-axis
  x.values <- 1:n.means
  x.values <- x.values/adjust.x.spacing
  
  
  # calculate values for plotting limits            
  y.max <- max(ci.up) +                    
    max(ci.up)*axis.adjust
  y.min <- min(ci.low) - 
    max(ci.low)*axis.adjust
  
  if(groups == 2){ x.values <- c(0.25, 0.5)}
  if(groups == 3){ x.values <- c(0.25, 0.5,0.75)}
  
  x.axis.min <- min(x.values)-0.05
  x.axis.max <- max(x.values)+0.05
  
  x.limits <- c(x.axis.min,x.axis.max)
  
  #Plot means
  plot(means ~ x.values,
       xlim = x.limits,
       ylim = c(y.min,y.max),
       xaxt = "n",
       xlab = "",
       ylab = "",
       cex = 1.25,
       pch = 16)
  
  axis(side = 1, 
       at = x.values,
       labels = categories,
  )
  
  #Plot upper error bar 
  lwd. <- 2
  arrows(y0 = means,
         x0 = x.values,
         y1 = ci.up,
         x1 = x.values,
         length = 0,
         lwd = lwd.)
  
  #Plot lower error bar
  arrows(y0 = means,
         x0 = x.values,
         y1 = ci.low,
         x1 = x.values,
         length = 0,
         lwd = lwd.) 
  
  #add reference line at 0
  abline(h = 0, col = 2, lwd = 2, lty =2)
  
  mtext(text = x.axis.label,side = 1,line = 1.75)
  mtext(text = y.axis.label,side = 2,line = 1.95)
  mtext(text = "Error bars = 95% CI",side = 3,line = 0,adj = 0)
  
  
}


res.aov_socialism <- aov(Socialism ~ period, data = test_socialism)
summary(res.aov_socialism)
tukey <- TukeyHSD(res.aov_socialism)
tukey
plot(tukey, asp = 1.5)

plotTukeyHSD(tukey)

res.aov_capitalism <- aov(Capitalism ~ period, data = test_capitalism)
summary(res.aov_capitalism)
tukey <- TukeyHSD(res.aov_capitalism)
tukey
plot(tukey, asp = 1.5)

plotTukeyHSD(tukey)


res.aov_countries <- aov(West_countries ~ period, data = test_westerncountries)
summary(res.aov_countries)
tukey <- TukeyHSD(res.aov_countries)
tukey
plot(tukey, asp = 1.5)
plotTukeyHSD(tukey)



library(ggplot2)
library(dplyr)
library(hrbrthemes)
test5 %>%
  ggplot(aes(x=year, y=V5)) +
  geom_line(color="black", size = 1.4) +
  geom_point(shape=21, color="black", fill="#69b3a2", size=7) +
  theme_ipsum() +
  theme(axis.text.x=element_text(size=rel(1))) +
  theme(axis.text.y=element_text(size=rel(1))) +
  ggtitle("Changes in topic foreign wester countries with time")





soccer <- data_sources_infrastructure %>% filter (Name == "футбольные поля") %>% select (Year, Number)
soccer$Year <- as.numeric(soccer$Year)

soccer %>%
  ggplot(aes(x=Year, y=Number, label = Number)) +
  geom_line(color="black", size = 1.4) +
  geom_point(shape=21, color="black", fill="#69b3a2", size=7) +
  geom_text(hjust = 1.3, nudge_x = -0.12, size = 7, color = "Black") +
  theme_ipsum() +
  theme(axis.text.x=element_text(size=rel(1))) +
  theme(axis.text.y=element_text(size=rel(1))) +
  ggtitle("Changes in the amount of Soccer Fields in 3 different years")


setwd("C:/data")
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

hmm <- full_join(pn.long_fizra_soccer, soccer)
pn.long_fizra_soccer$Year = pn.long_fizra_soccer$year_media
pn.long_fizra_soccer$Year = as.numeric(pn.long_fizra_soccer$Year)

pn.long_fizra_soccer$year_media <- regmatches(pn.long_fizra_soccer$doc_id, gregexpr("\\d{4}", pn.long_fizra_soccer$doc_id))


bp <- soccer %>%
  ggplot(aes(x=Year, y=Number, label = Number)) +
  geom_line(color="black", size = 1.4) +
  geom_point(shape=21, color="black", fill="#69b3a2", size=7) +
  geom_text(hjust = 1.3, nudge_x = -0.12, size = 7, color = "Black") +
  theme_ipsum() +
  theme(axis.text.x=element_text(size=rel(1))) +
  theme(axis.text.y=element_text(size=rel(1))) +
  ggtitle("Changes in the amount of Soccer Fields in 3 different years")







  
class(hmm$Number)

bd <- pn.long_fizra_soccer %>%
  ggplot(aes(x = Year)) +
  geom_line(aes(y=proportion),color="red", size = 1.4) +
  geom_point(aes(y=proportion), shape=21, color="black", fill="#69b3a2", size=7) +
  theme_ipsum() +
  theme(axis.text.x=element_text(size=rel(1))) +
  theme(axis.text.y=element_text(size=rel(1))) +
  ggtitle("Changes in the amount of Soccer mentionings in Physical Culture and Sports journal")
  

bp+facet_grid(bd)


layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))

par(mfrow=c(1,1))
plot(bp) 
plot(bd)


g <- ggplot(hmm, aes(Year))
g <- g + geom_line(aes(y=Number), colour="red")
g <- g + geom_line(aes(y=proportion), colour="green")
g

View(dd)

