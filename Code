################################################################################

#################### 1. Loading Packages & Libraries ################################
install.packages("dplyr")
install.packages("ggplot2")
install.packages("knitr")
install.packages("hrbrthemes")
install.packages("pdftools")
install.packages("rvest")
install.packages("tidyr")
install.packages("wordcloud")
install.packages("XML")
install.packages("RColorBrewer")
install.packages("xml2")
install.packages("tidytext")
install.packages("stringr")
install.packages("ggthemes")
install.packages("tm.plugin.webmining")
install.packages("purrr")
install.packages("wordcloud")
install.packages("reshape2")
install.packages("igraph")
install.packages("ggraph")
install.packages("widyr")
install.packages("tm")
install.packages("topicmodels")
install.packages("scales")

library(dplyr)        
library(ggplot2)      
library(hrbrthemes)    
library(knitr)
library(pdftools)    
library(rvest)       
library(tidyr)        
library(tidytext)      
library(wordcloud)    
library(XML)           
library(RColorBrewer)
library(xml2)
library(stringr)
library(ggthemes)
library(tidytext)
library(tm.plugin.webmining)
library(purrr)
library(wordcloud)
library(reshape2)
library(igraph)
library(ggraph)
library(widyr)
library(tm)
library(topicmodels)
library(scales)

#################### 2. Getting & Reading in HTML Letters ################################

urls_77_97 <- paste('http://www.berkshirehathaway.com/letters/', seq(1977, 1997), '.html', sep='')
html_urls <- c(urls_77_97,
               'http://www.berkshirehathaway.com/letters/1998htm.html',
               'http://www.berkshirehathaway.com/letters/1999htm.html',
               'http://www.berkshirehathaway.com/2000ar/2000letter.html',
               'http://www.berkshirehathaway.com/2001ar/2001letter.html')
letters_html <- lapply(html_urls, function(x) read_html(x) %>% html_text())


# Getting & Reading in PDF Letters
urls_03_19 <- paste('http://www.berkshirehathaway.com/letters/', seq(2003, 2019), 'ltr.pdf', sep = '')
pdf_urls <- data.frame('year' = seq(2002, 2019),
                       'link' = c('http://www.berkshirehathaway.com/letters/2002pdf.pdf', urls_03_19))
download_pdfs <- function(x) {
  myfile = paste0(x['year'], '.pdf')
  download.file(url = x['link'], destfile = myfile, mode = 'wb')
  return(myfile)
}
pdfs <- apply(pdf_urls, 1, download_pdfs)
letters_pdf <- lapply(pdfs, function(x) pdf_text(x) %>% paste(collapse=" "))
tmp <- lapply(pdfs, function(x) if(file.exists(x)) file.remove(x)) # Clean up directory

# Combine all 
letters <- do.call(rbind, Map(data.frame, year=seq(1977, 2019), text=c(letters_html, letters_pdf)))
letters$text <- as.character(letters$text)



#################### 3. Primer Visualizado #################################

-----------------------------------------------------------------------------
  #Most Commun Words + Graph
-----------------------------------------------------------------------------  
  
####Looking for most commun words in general

letter_words <- letters %>%
  unnest_tokens(word, text) %>%
  filter(str_detect(word, "[a-z']$"),
         !word %in% stop_words$word)

letter_words %>% 
  count(word, sort=TRUE)

#freq
letter_words  %>% 
  count(word) %>% 
  mutate(perc = (n/sum(n))*100) %>%
  filter(n > 600) %>% 
  arrange(desc(perc)) %>% 
  mutate(word = reorder(word, perc)) %>% 
  ggplot(aes(word, perc)) +
  geom_bar(stat = "identity", color = "bisque3", fill = "cornflowerblue") +
  geom_text(aes(hjust = 1.3, label = round(perc, 2))) + 
  coord_flip() +
  labs(title = "Veinte palabras más frecuentes", x = "Palabras", y = "Porcentaje de uso") +
  theme_minimal() 


#plot
letter_words %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n), color = "bisque3", fill = "cornflowerblue") +
  xlab(NULL) +
  coord_flip() + 
  ggtitle("Palabrás más repetidas en el conjunto de cartas") + 
  theme_minimal() +
  ylab("Frecuencia")



-----------------------------------------------------------------------------
  #WordClouds
-----------------------------------------------------------------------------  
  
#WordClouds
wordcloud = letter_words %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

wordcloud = letter_words %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 200))


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
#################### 4. Más allá de las palabras ################################



-----------------------------------------------------------------------------
 #Apartado 1
-----------------------------------------------------------------------------  
  #Contador de palabras y año: presentamos salida de R
  words_by_year <- letter_words %>%
  count(year, word, sort = TRUE) %>%
  ungroup()
View(words_by_year) #view in new script
class(words_by_year) #know type of class
-----------------------------------------------------------------------------
  
  
  
  
-----------------------------------------------------------------------------
#Apartado 2
-----------------------------------------------------------------------------
  #Comparativa 2019 vs 1977
  
  #Subrating, Slicing, Indexing -- Comparing Last year vs First Year
  year_2019 = words_by_year[words_by_year$year == "2019", ]
colSums(year_2019["n"]) #Words Sum for 2019
year_2019_mut = year_2019 %>% #Reorder/mutate for better graph
  mutate(word = reorder(word, n))
qplot(word, n, data = filter(arrange(year_2019, n), n >= 14))
filter(arrange(year_2019, desc(n)), n >= 14)

year_1977 = words_by_year[words_by_year$year == "1977", ]
View(year_1977)
colSums(year_1977["n"]) #Words Sum for 1977
qplot(word, n, data = filter(arrange(year_1977, desc(n)), n >= 10))
filter(arrange(year_1977, desc(n)), n >= 10)
-----------------------------------------------------------------------------
  
  
  
  
-----------------------------------------------------------------------------
#Apartado 3
-----------------------------------------------------------------------------  
#Comparativa n mayor a 50-60-70 distribuido en los años
#¿En que años aparecen las palabras más repetidas? con diferentes n?
#Manipulating: filtering & sorting: Filter by n in general
f50 = filter(arrange(words_by_year, desc(n)), n >= 50)
f60 = filter(arrange(words_by_year, desc(n)), n >= 60)
f70 = filter(arrange(words_by_year, desc(n)), n >= 70)

qplot(year, n, data = f50, color = word)
qplot(year, n, data = f60, color = word)
qplot(year, n, data = f70, shape = word, color = word)

ggplot(f50) + 
  geom_point(aes(x=year, y=n, color = word)) +
  xlab("Años") +
  ylab("Frecuencia") +
  ggtitle("Palabras más repetidas, n >= 50") +
  theme_minimal()

ggplot(f60) + 
  geom_point(aes(x=year, y=n, color = word)) +
  xlab("Años") +
  ylab("Frecuencia") +
  ggtitle("Palabras más repetidas, n >= 60") +
  theme_minimal()

ggplot(f70) + 
  geom_point(aes(x=year, y=n, color = word)) +
  xlab("Años") +
  ylab("Frecuencia") +
  ggtitle("Palabras más repetidas, n >= 70") +
  theme_minimal()
-----------------------------------------------------------------------------
  
  
  
  
-----------------------------------------------------------------------------
  #Apartado 4
-----------------------------------------------------------------------------
#Agrupación de sumatorio por años: Lista y plot de barras, puntos y tendencias
  
#Manipulting: Grouping & summarizing: Sum Words by Year
words_by_year_group = group_by(words_by_year, year)
words_by_year_group_sum = summarize(words_by_year_group, sum.words = sum(n, na.rm= T)) 
#View(words_by_year_group_sum)

arrange(words_by_year_group_sum, desc(year)) #sort by year
arrange(words_by_year_group_sum, desc(sum.words)) #sort by words
#n words by year. Points
plot(words_by_year_group_sum) 
#n words by year. Line
ggplot(words_by_year_group_sum) + 
  geom_line(aes(x=year, y=sum.words), color = "cornflowerblue") + 
  geom_point(aes(x=year, y=sum.words), color = "bisque3") +
  xlab("Años") +
  ylab("Número total de Palabras") +
  ggtitle("Palabras totales por año") +
  theme_minimal()


qplot(year, sum.words, data = words_by_year_group_sum, geom = c("point", "smooth"))
#hacerlo en ggplot
qplot(year, sum.words, data = words_by_year_group_sum, size = sum.words)
qplot(year, sum.words, data = words_by_year_group_sum, geom = c("point", "smooth"))
qplot(sum.words, data = words_by_year_group_sum, geom = "density")


ggplot(words_by_year_group_sum) +
  geom_col(aes(year, sum.words), color="bisque3", fill = "cornflowerblue") + #contorno + relleno 
  xlab("Años") +
  ylab("Número de Palabras") +
  #coord_flip() + 
  ggtitle("Numero de Palabras por Año") + 
  theme_minimal()

ggplot(words_by_year_group_sum) + 
  geom_line(aes(x=year, y=sum.words), color = "cornflowerblue") + 
  geom_point(aes(x=year, y=sum.words), color = "bisque3") +
  xlab("Años") +
  ylab("Número total de Palabras") +
  ggtitle("Palabras totales por año") +
  theme_minimal() +
  geom_smooth(aes(x=year, y=sum.words))
-----------------------------------------------------------------------------
  
  

-----------------------------------------------------------------------------
  #Apartado 5
  #Que palabras se repetian mas en cada año sin importar el n, ya que depende de cada año?
  top5_words_by_year = top_n(group_by(words_by_year, year), 5, n)
ggplot(top5_words_by_year) + 
  geom_point(aes(year, word, color = word)) + 
  xlab("Años") + 
  ylab("Palabras") + 
  ggtitle("Las 5 palabras más repetidas en cada año") + 
  theme_minimal() +
  scale_x_continuous(breaks=seq(1977, 2019, 1)) + 
  theme (axis.text.x = element_text(size=rel(1.5), angle=90)) + 
  theme (axis.text.y = element_text(size=rel(1.5))) +
  theme(legend.position="none")

top3_words_by_year = top_n(group_by(words_by_year, year), 3, n)
ggplot(top3_words_by_year) + 
  geom_point(aes(year, word, color = word)) + 
  xlab("Años") + 
  ylab("Palabras") + 
  ggtitle("Las 3 palabras más repetidas en cada año") + 
  theme_minimal() +
  scale_x_continuous(breaks=seq(1977, 2019, 1)) + 
  theme (axis.text.x = element_text(size=rel(1.5), angle=90))  + 
  theme (axis.text.y = element_text(size=rel(2))) +
  theme(legend.position="none")

top1_words_by_year = top_n(group_by(words_by_year, year), 1, n)
ggplot(top1_words_by_year) + 
  geom_point(aes(year, word, color = word)) + 
  xlab("Años") + 
  ylab("Palabras") + 
  ggtitle("La palabra más repetidas en cada año") + 
  theme_minimal() + 
  scale_x_continuous(breaks=seq(1977, 2019, 1)) + 
  theme (axis.text.x = element_text(size=rel(1.5), angle=90))  + 
  theme (axis.text.y = element_text(size=rel(2))) +
  theme(legend.position="none")

View(top1_words_by_year)
-----------------------------------------------------------------------------
  
  
  
  

--------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--------------------------------------------------------------------------------
#################### 5. Sentiment Analysis by Year ################################ 
1#Afinn ####

letters_sentiments_afinn_year <- words_by_year %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(year) %>%
  summarize(score = sum(value * n) / sum(n))

#barplot, sorted by score
letters_sentiments_afinn_year %>%
  mutate(year = reorder(year, score)) %>%
  ggplot(aes(year, score, fill = score > 0)) +
  geom_col(show.legend = FALSE, color = "black") +
  coord_flip() +
  ylab("Puntuación de sentimiento medio") + 
  ggtitle("Distribución por años - Sentimiento Afinn") +
  theme_minimal()


#barplot, sorted by year
letters_sentiments_afinn_year %>%
  ggplot(aes(year, score, fill = score > 0)) +
  geom_col(show.legend = FALSE, color = "black") +
  ylab("Puntuación de sentimiento medio") + 
  ggtitle("Distribución por años - Sentimiento Afinn") +
  theme_minimal() + 
  scale_x_continuous(breaks=seq(1977, 2019, 1)) +
  scale_y_continuous(breaks=seq(0, 1, 0.1)) +
  theme (axis.text.x = element_text(size=rel(1.5), angle=45)) + 
  theme (axis.text.y = element_text(size=rel(1))) +
  coord_flip() 




#scatterplot,sorted by year
qplot(year, score, data = letters_sentiments_afinn_year, color = score > 0)

ggplot(letters_sentiments_afinn_year) + 
  geom_point(aes(year, score, color = score > 0 )) + 
  ylab("Puntuación de sentimiento medio") + 
  ggtitle("Distribución por años - Sentimiento Afinn") +
  theme_minimal()


#Line
ggplot(letters_sentiments_afinn_year) + 
  geom_line(aes(x=year, y=score), color = "lightcyan3", linetype = 2) + 
  geom_point(aes(year, score, color = score > 0), size = 3, alpha = 1/2) +
  #geom_hline(yintercept = mean(letters_sentiments_afinn_year$score), col = "black") +
  geom_smooth(aes(x=year, y=score))+
  ylab("Puntuación de sentimiento medio") + 
  ggtitle("Distribución por años - Sentimiento Afinn") +
  theme_minimal() + 
  scale_x_continuous(breaks=seq(1977, 2019, 1)) +
  scale_y_continuous(breaks=seq(0, 1, 0.1)) +
  theme (axis.text.x = element_text(size=rel(1.25), angle=90)) + 
  theme (axis.text.y = element_text(size=rel(1.25))) 




2#loughran ####

#The Loughran dictionary divides words into six sentiments: “positive”, “negative”, “litigious”, “uncertainty”, “constraining”, and “superfluous”.
letters_sentiments_loughran_year = letter_words %>%
  inner_join(get_sentiments("loughran"), by = "word") %>%
  group_by(year, word) 

str(letters_sentiments_loughran_year) #cuantas palabras ha clasificado
g = get_sentiments("loughran")
filter(g, sentiment %in% c("positive", 
                           "negative",
                           "constraining",
                           "litigious",
                           "superfluous",
                           "uncertainly")) %>% 
  count(sentiment)

count(sentiment)

#Histograma general
ggplot(letters_sentiments_loughran_year) + 
  geom_histogram(aes(sentiment, fill=sentiment), color = "black", stat = "count") + 
  theme_minimal() + 
  ggtitle("Numero de palabras clasificadas por cada sentimiento")

#Histogram general-acumulado por año
ggplot(letters_sentiments_loughran_year) + 
  geom_histogram(aes(year, fill = sentiment), color = "black", bins = 30) + 
  xlab("Años") + 
  ylab("Número de palabras clasificadas") + 
  ggtitle("Clasificación Loughran por año") + 
  theme_minimal() + 
  scale_x_continuous(breaks=seq(1977, 2019, 1)) +
  theme (axis.text.x = element_text(size=rel(1.25), angle=90)) + 
  theme (axis.text.y = element_text(size=rel(1.25))) 

#Histogram - Facets by sentiment
ggplot(letters_sentiments_loughran_year) + 
  geom_histogram(aes(year, fill = sentiment), color = "black", bins = 30) + 
  facet_wrap( ~ sentiment, scales = "free") + 
  xlab("Años") + 
  ylab("Número de palabras clasificadas") + 
  ggtitle("Clasificación Loughran por año") + 
  theme_minimal() +
  theme(legend.position="none") +
  scale_x_continuous(breaks=seq(1977, 2019, 10))



#Histogram Facets by year
ggplot(letters_sentiments_loughran_year) + 
  geom_histogram(aes(sentiment, fill=sentiment), color = "black", stat = "count") + 
  facet_wrap( ~ year) + 
  xlab(NULL) + 
  ylab("Número de palabras clasificadas") + 
  ggtitle("Clasificación Loughran por año") + 
  theme_minimal() + 
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) #elimina info del eje

#Histogram Facets by year sin escala
ggplot(letters_sentiments_loughran_year) + 
  geom_histogram(aes(sentiment, fill=sentiment), color = "black", stat = "count") + 
  facet_wrap( ~ year, scales = "free") + 
  xlab(NULL) + 
  ylab("Número de palabras clasificadas") + 
  ggtitle("Clasificación Loughran por año") + 
  theme_minimal() + 
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) #elimina info del eje


3#nrc ####

#¿How many Positive/Negative sentiments do we have?
get_sentiments("nrc") %>% 
  count(sentiment)

#Use the nrc
letters_sentiments_nrc_year = letter_words %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  group_by(year, word) 

letters_sentiments_nrc_count = letter_words %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  group_by(sentiment) %>% 
  count(sentiment) 


#Histograma general
ggplot(letters_sentiments_nrc_year) + 
  geom_histogram(aes(sentiment, fill=sentiment), color = "black", stat = "count")

#Histogram general-acumulado por año
ggplot(letters_sentiments_nrc_year) + 
  geom_histogram(aes(year, fill = sentiment), color = "black", bins = 43) + 
  xlab("Años") + 
  ylab("Número de palabras clasificadas") + 
  ggtitle("Clasificación Nrc por año") + 
  theme_minimal() +
  scale_x_continuous(breaks=seq(1977, 2019, 1)) +
  scale_y_continuous(breaks=seq(0, 8000, 500)) +
  theme (axis.text.x = element_text(size=rel(1.25), angle=90)) + 
  theme (axis.text.y = element_text(size=rel(1.25))) 

#Histogram - Facets by sentiment
ggplot(letters_sentiments_nrc_year) + 
  geom_histogram(aes(year, fill = sentiment), color = "black", bins = 30) + 
  facet_wrap( ~ sentiment) + 
  xlab("Años") + 
  ylab("Número de palabras clasificadas") + 
  ggtitle("Clasificación Nrc por año") + 
  theme_minimal() + 
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +#elimina info del eje 
  theme(legend.position="none") 

#Histogram Facets by year
ggplot(letters_sentiments_nrc_year) + 
  geom_histogram(aes(sentiment, fill=sentiment), color = "black", stat = "count") + 
  facet_wrap( ~ year) + 
  xlab(NULL) + 
  ylab("Número de palabras clasificadas") + 
  ggtitle("Clasificación Nrc por año") + 
  theme_minimal() + 
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



4#Bing ####

#How many positive/negative sentiments do we have?
get_sentiments("bing") %>% 
  count(sentiment)

#Use the Bing
letters_sentiments_bing_year <- letter_words %>%
  inner_join(get_sentiments("bing"), by = "word") %>% #load sentyment 
  group_by(year) 
#summarize(sentiment, occurences = n()) #times that a word appears
letters_sentiments_bing_year


#Histogram
ggplot(data = letters_sentiments_bing_year) + 
  geom_histogram(aes(x = year, fill = sentiment), color="black", binwidth = 1) + 
  xlab("Cartas") + 
  ylab("Numero de palabras clasificadas") + 
  ggtitle("Clasificación Bing por año") + 
  theme_minimal()

#Facet Histogram by sentiment
ggplot(data = letters_sentiments_bing_year) + 
  geom_histogram(aes(x = year, fill = sentiment), color="black", binwidth = 1) + 
  facet_wrap( ~ sentiment) + 
  xlab("Cartas") + 
  ylab("Numero de palabras clasificadas") + 
  ggtitle("Clasificación Bing por año") + 
  theme_minimal() +
  scale_x_continuous(breaks=seq(1977, 2019, 1)) +
  theme (axis.text.x = element_text(size=rel(0.8), angle=90))

#Facet Histogram by year
ggplot(data = letters_sentiments_bing_year) + 
  geom_histogram(aes(x = sentiment, fill = sentiment), color="black", binwidth = 1, stat = "count") + 
  facet_wrap( ~ year) + 
  xlab("Cartas") + 
  ylab("Numero de palabras clasificadas") + 
  ggtitle("Clasificación Bing por año ") + 
  theme_minimal() +
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())





5#Mixed all Lexicons in 1####

afinn <- letter_words %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(year) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

bing_nrc_loughran <- bind_rows(letter_words %>% 
                                 inner_join(get_sentiments("bing")) %>%
                                 mutate(method = "Bing et al."),
                               letter_words %>% 
                                 inner_join(get_sentiments("nrc") %>% 
                                              filter(sentiment %in% c("positive", 
                                                                      "negative"))) %>%
                                 mutate(method = "NRC"),
                               letter_words %>% 
                                 inner_join(get_sentiments("loughran") %>% 
                                              filter(sentiment %in% c("positive", 
                                                                      "negative"))) %>%
                                 mutate(method = "Loughran")) %>%
  count(method, year, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#Mixed all
bind_rows(afinn, 
          bing_nrc_loughran) %>%
  ggplot(aes(year, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y") + 
  ggtitle("Comparación de los 4 léxicos") + 
  theme_minimal() +
  scale_x_continuous(breaks=seq(1977, 2019, 1)) +
  theme (axis.text.x = element_text(size=rel(0.8), angle=90))

#Explicacion de Loughran (2355 negativos vs 354 positivos). Compara resto
get_sentiments("loughran") %>% 
  filter(sentiment %in% c("positive", 
                          "negative")) %>% 
  count(sentiment)


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
#################### 5. Sentiment Analysis by Words ##########################
1#Afinn ####

#Examine the total positive and negative contributions of each word.
letters_sentiments_afinn_word <- letter_words %>%
  inner_join(get_sentiments("afinn"), by = "word") %>% #load sentyment 
  group_by(word) %>%
  summarize(occurences = n(), #times that a word appears
            contribution = sum(value)) #sentyment sum
letters_sentiments_afinn_word


#Plot top 30
letters_sentiments_afinn_word %>%
  top_n(30, abs(contribution)) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, contribution, fill = contribution > 0)) + #create the skeleton
  geom_col(show.legend = FALSE, color = "black") + #add data
  coord_flip() + #change position
  ggtitle('Distribución de palabras por sentimiento - Afinn') +
  theme_minimal() +
  scale_y_continuous(breaks=seq(-2000, 1000, 100)) +
  theme (axis.text.x = element_text(size=rel(1), angle=45)) + 
  theme (axis.text.y = element_text(size=rel(1))) +
  xlab("Palabras clasificadas") + 
  ylab("Contribución") 

#Total positive and negative distributed by year and word (without filter by n)
letters_sentiments_afinn_wordbyyear <- letter_words %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(year, word) %>%
  summarize(sentiment = mean(value),
            ocurrences = n()) %>%
  ungroup() 

#arange by desc sentiment
letters_sentiments_afinn_wordbyyear %>%
  arrange(desc(sentiment))
#arrange by sentiment
letters_sentiments_afinn_wordbyyear %>% 
  arrange(sentiment)
# How all these words are distributed by years?
hist(x = letters_sentiments_afinn_wordbyyear$year)
hist(x = letters_sentiments_afinn_wordbyyear$year, main = "Distribución de sentimientos(palabras)", 
     xlab = "Años", ylab = "Frecuencia", col = "#0066a1", border = "Dark Red")

ggplot(letters_sentiments_afinn_wordbyyear) +
  geom_bar(aes(x = year), fill = 'steelblue', color = "black") + 
  ggtitle('Distribución de palabras positivas/negativas en cada año por sentimiento - Afinn') +
  theme_minimal()


#  >= 5 : positive and negative grouped by year and word
letters_sentiments_afinn_wordbyyear_5 <- letter_words %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(year, word) %>%
  summarize(sentiment = mean(value),
            ocurrences = n()) %>%
  ungroup() %>%
  filter(ocurrences >= 5) #sustancial information

#arange by desc sentiment
View(letters_sentiments_afinn_wordbyyear_5 %>%
       arrange(desc(sentiment)))
#Now we look for the words with the highest positive scores in each letter, here it is, “outstanding” appeared eight out of ten letters.

#arrange by sentiment
letters_sentiments_afinn_wordbyyear_5 %>% 
  arrange(sentiment)
#Unsurprisingly, seven out of ten letters, word “loss” secured the highest negative score.

#How all these words are distributed by years?
hist(x = letters_sentiments_afinn_wordbyyear_5$year)
hist(x = letters_sentiments_afinn_wordbyyear_5$year, main = "Distribución de palabras (n > 5) positivas/negativas en cada año por sentimiento - Afinn", 
     xlab = "Años", ylab = "Frecuencia", col = "#0066a1", border = "Dark Red")

ggplot(data = letters_sentiments_afinn_wordbyyear_5) + 
  geom_histogram(aes(x=year), color="black", fill = "pink", binwidth = 1) + 
  xlab("Años") + 
  ylab("Número de palabras") + 
  ggtitle("Distribución de palabras (n > 5) positivas/negativas en cada año por sentimiento - Afinn")+
  theme_minimal()





----
2#loughran ####

letters_sentiments_loughran_word = letter_words %>%
  count(word) %>%
  inner_join(get_sentiments("loughran"), by = "word") %>%
  group_by(sentiment)


loughran_sentiment = get_sentiments("loughran") %>% 
  group_by(sentiment) %>% 
  count(sentiment) 

letters_sentiments_loughran_count = letter_words %>%
  inner_join(get_sentiments("loughran"), by = "word") %>%
  group_by(sentiment) %>% 
  count(sentiment)

filter(letters_sentiments_loughran_word , sentiment %in% c("positive", 
                                                           "negative",
                                                           "constraining",
                                                           "litigious",
                                                           "superfluous",
                                                           "uncertainly")) %>% 
  count(sentiment)
#Histograma general
ggplot(letters_sentiments_loughran_word) + 
  geom_histogram(aes(sentiment, fill=sentiment), color = "black", stat = "count") +
  theme_minimal() +
  ggtitle("Distribución de palabras por sentimiento - Loughran") +
  xlab("Sentimiento") + 
  ylab("Número de palabras")  +
  theme(legend.position="none") 

#Top 5
letters_sentiments_loughran_word_5 = letter_words %>%
  count(word) %>%
  inner_join(get_sentiments("loughran"), by = "word") %>%
  group_by(sentiment) %>%
  top_n(5, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) 

#plot
ggplot(letters_sentiments_loughran_word_5) +
  geom_col(aes(word, n, fill = sentiment)) +
  coord_flip() +
  facet_wrap(~ sentiment, scales = "free") +
  ggtitle("Clasificación de palabras por sentimiento - Loughran") +
  theme_minimal() +
  theme(legend.position="none") +
  xlab("Palabras") + 
  ylab("Frecuencia") 




---
3#nrc ####
get_sentiments("nrc")

letters_sentiments_nrc_word = letter_words %>%
  count(word) %>% 
  inner_join(get_sentiments("nrc"), by = "word") %>%
  group_by(sentiment) 

nrc_sentiment = get_sentiments("nrc") %>% 
  group_by(sentiment) %>% 
  count(sentiment) 

letters_sentiments_nrc_count = letter_words %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  group_by(sentiment) %>% 
  count(sentiment) 

#Histograma general
ggplot(letters_sentiments_nrc_word) + 
  geom_histogram(aes(sentiment, fill=sentiment), color = "black", stat = "count")

#Top5
letters_sentiments_nrc_word_5 = letter_words %>%
  count(word) %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  group_by(sentiment) %>%
  top_n(5, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n))

#plot
ggplot(letters_sentiments_nrc_word_5) +
  geom_col(aes(word, n, fill = sentiment)) +
  coord_flip() +
  facet_wrap(~ sentiment, scales = "free") +
  ggtitle("Clasificación de palabras por sentimiento - Nrc") +
  theme_minimal() +
  theme(legend.position="none") +
  xlab("Palabras") + 
  ylab("Frecuencia") 




---
4#Bing ####

letters_sentiments_bing_word <- letter_words %>%
  count(word) %>%
  inner_join(get_sentiments("bing"), by = "word") %>% #load sentyment 
  group_by(word)

letters_sentiments_bing_word 

bing_sentiment = get_sentiments("bing") %>% 
  group_by(sentiment) %>% 
  count(sentiment) 

letters_sentiments_bing_count = letter_words %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  group_by(sentiment) %>% 
  count(sentiment) 

#wordcloud
letter_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100) 

#Histogram
ggplot(data = letters_sentiments_bing_word) + 
  geom_histogram(aes(x = sentiment, fill = sentiment), color="black", stat = "count") + 
  xlab("Sentimiento") + 
  ylab("Numero de palabras clasificadas") + 
  ggtitle("Clasificación de palabras por sentimiento - Bing)") + 
  theme_minimal()

#TOP 50 de cada sentimiento
letters_sentiments_bing_word_50 = letter_words %>%
  count(word) %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  group_by(sentiment) %>%
  top_n(50, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n))

#Barplot
ggplot(letters_sentiments_bing_word_50) +
  geom_col(aes(word, n, fill = sentiment)) +
  coord_flip() +
  facet_wrap(~ sentiment, scales = "free") +
  ggtitle("Top 50 palabras por sentimiento - bing") +
  theme_minimal()



#Know how contribute each word to the sentiment
letters_sentiments_bing_word_count <- letter_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

letters_sentiments_bing_word_count

#plot top 10 words
letters_sentiments_bing_word_count %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


5#Mixture ####
afinn <- letter_words %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(year) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(letter_words %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."),
                          letter_words %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, year, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#Mixed all
bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(year, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
#################### 6. Relaciones entre palabras ################################# 


-----------------------------------------------------------------------
#Apartado 1
#Bigrams
  1.1
letters_bigrams <- letters %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) 

letters_bigrams %>%
  count(bigram, sort = TRUE)

bigram_separated <- letters_bigrams %>%
  count(year, bigram, sort = TRUE) %>%
  ungroup() %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigram_separated

negate_words <- c("not", "without", "no", "can't", "don't", "won't")

bigram_separated%>%
  filter(word1 %in% negate_words) %>%                                        #filter only with negate_words
  count(word1, word2, wt = n, sort = TRUE) %>%                               #without year, only count of n
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%            #add afinn values
  mutate(contribution = value * n) %>%                                       #add contribution column
  group_by(word1) %>%                                             
  top_n(10, abs(contribution)) %>%                                           #Top 10 (49 combinations) of absolute value of contribution
  ungroup() %>%
  mutate(word2 = reorder(paste(word2, word1, sep = "__"), contribution)) %>% #New column
  ggplot(aes(word2, contribution, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free", nrow = 3) +                           #divided in 6 graphs
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  xlab("Palabras seguidas de una negación") +
  ylab("Score * Frecuencia") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip() + ggtitle("Palabras que más contribuyen al sentimiento negativo - precedidas por una negation")+
  theme_minimal()


#Removing stopwords
bigrams_filtered <- bigram_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

View(bigram_counts)

#bigrams united
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united
-----------------------------------------------------------------------
  
  
  
  
  -----------------------------------------------------------------------
#Apartado 1.1
  -----------------------------------------------------------------------
#Using bigrams to provide context in sentiment analysis
  bigram_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)
#get sentiment
AFINN <- get_sentiments("afinn")
AFINN
#apply all mixed
not_words <- bigram_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)
not_words
#plot
not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Palabras") +
  ylab("Score * Frecuencia") +
  ggtitle("Top 20 palabras que contribuyen al sentimiento negativo seguidas de 'not'") + 
  coord_flip() + 
  theme_minimal()

#adding more negative words
negation_words <- c("not", "no", "never", "without")

negated_words <- bigram_separated %>%
  mutate(word2 = factor(word2, levels = rev(unique(word2))))%>% 
  mutate(word1 = factor(word1, levels = rev(unique(word1))))%>% 
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE) %>% 
  group_by(word1) %>% 
  top_n(10) %>% 
  ungroup()

#plot -- investigar xk no divide casillas por palabras
ggplot(negated_words) +
  geom_col(aes(word2, n * value, fill = n * value > 0), show.legend = FALSE) +
  xlab("Words preceded by \"negation\"") +
  ylab("Sentiment value * number of occurrences") +
  facet_wrap(~word1) + 
  coord_flip()

#plotado como interesa
bigram_separated %>%
  mutate(word2 = factor(word2, levels = rev(unique(word2))))%>% 
  mutate(word1 = factor(word1, levels = rev(unique(word1))))%>% 
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE) %>% 
  group_by(word1) %>% 
  top_n(10) %>% 
  ungroup() %>%
  ggplot() +
  geom_col(aes(word2, n * value,  fill = n * value > 0), show.legend = FALSE) +
  labs(x = NULL) +
  facet_wrap(~word1, scales = "free") +
  coord_flip()
-----------------------------------------------------------------------
  
  
  
  -----------------------------------------------------------------------
#Apartado 1.2
  -----------------------------------------------------------------------
#Visualizing a network of bigrams with ggraph
# filter for only relatively common combinations
  bigram_graph <- bigram_counts %>%
  filter(n > 40) %>%
  graph_from_data_frame()
bigram_graph

#plot
set.seed(2017)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) + 
  theme_minimal()

#Improving plot
set.seed(2016)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
-----------------------------------------------------------------------

#Ejemplo nuevo grafico
set.seed(1234)
bigram_counts %>%
  filter(n >= 40) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()



  
  
  
-----------------------------------------------------------------------
#Apartado 2
-----------------------------------------------------------------------
#Counting and correlating pairs of words with the widyr package
  
#4.2.1 Counting and correlating among sections
  
  letter_words <- letters %>%
  unnest_tokens(word, text) %>%
  filter(str_detect(word, "[a-z']$"),
         !word %in% stop_words$word)


# count words co-occuring within sections
word_pairs <- letter_words %>%
  pairwise_count(word, year, sort = TRUE)
word_pairs

word_pairs %>%
  filter(item1 == "washington")

#4.2.2 Pairwise correlation
# we need to filter for at least relatively common words first
word_cors <- letter_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, year, sort = TRUE)
#most correlaet with this word:
word_cors %>%
  filter(item1 == "pounds")

#plot
word_cors %>%
  filter(item1 %in% c("customer","fear")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip() +
  ggtitle("Correlación de las palabras 'customer' y 'fear'")+
  ylab("Correlación")+
  theme_minimal()

#graph 
set.seed(2016)
word_cors %>%
  filter(correlation > 0.40) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()



-----------------------------------------------------------------------
  #Apartado 3
  -----------------------------------------------------------------------
  #Trigrams
  letters_trigrams = letters %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

trigrams_united <- letters_trigrams %>%
  unite(trigram, word1, word2, word3, sep = " ")

trigrams_united_10  <- trigrams_united %>% 
  top_n(10, n) %>% 
  arrange(desc(n)) %>% 
  mutate(trigram= reorder(trigram, n)) 

ggplot(trigrams_united_10) +
  geom_col(aes(trigram, n), color = "bisque3", fill = "cornflowerblue") +
  coord_flip() + 
  ggtitle("Top 10 Trigrams") +
  theme_minimal() +
  xlab("Trigrams") + 
  ylab("Frecuencia")

trigrams_united_10_freq = trigrams_united  %>% 
  count(trigram) %>% 
  mutate(perc = (n/sum(n))*100) %>%
  top_n(10, n) %>% 
  arrange(desc(perc)) %>% 
  mutate(trigram= reorder(trigram, n)) 
  
  ggplot(trigrams_united_10_freq) +
  geom_col(aes(trigram, n), color = "bisque3", fill = "cornflowerblue") +
  coord_flip() + 
  ggtitle("Top 10 Trigrams") +
  theme_minimal() +
  xlab("Trigrams") + 
  ylab("Frecuencia") 

---------------------------------------------------------------------
  
  
  
  
  -----------------------------------------------------------------------
  #Esto debería ir siguiente apartado
  #TF IDF for bigrams
  bigram_tf_idf <- bigrams_united %>%
  count(year, bigram) %>%
  bind_tf_idf(bigram, year, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

#plot for top 3
bigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>% 
  group_by(year) %>% 
  top_n(3) %>% 
  ungroup() %>%
  ggplot() +
  geom_col(aes(bigram, tf_idf, fill = year), show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~year, scales = "free") +
  coord_flip()

-----------------------------------------------------------------------
  
  
  
  
  
  
  --------------------------------------------------------------------------------
  --------------------------------------------------------------------------------
  --------------------------------------------------------------------------------
#################### 7. Inverse document frequency (idf) ####
1#First approach####
each_letter_words <- letters %>%
  unnest_tokens(word, text) %>%
  count(year, word, sort = TRUE)

total_words <- each_letter_words%>% 
  group_by(year) %>% 
  summarize(total = sum(n))

each_letter_words <- left_join(each_letter_words, total_words)
each_letter_words

#plot
ggplot(each_letter_words) +
  geom_histogram(aes(n/total), show.legend = FALSE, fill = "blue")  + 
  theme_minimal() + 
  ggtitle("Distribución tf en el conjunto de cartas") +
  xlab("n/total") + 
  ylab("Frecuencia")


#plot by year
ggplot(each_letter_words, aes(n/total, fill = year)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~year, scales = "free_y") + 
  ggtitle("Distribución por frecuencia de terminos cada año") +
  xlab("n/total") + 
  ylab("Frecuencia") +
  theme_minimal() +
  scale_x_continuous(breaks=seq(0., 0.04, 0.01)) +
  theme (axis.text.x = element_text(size=rel(1), angle=90))




----
2#Zipf’s law ####

freq_by_rank <- each_letter_words %>% 
  group_by(year) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total)

freq_by_rank

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = year, fill = "black")) + 
  geom_line(alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10() + 
  theme_minimal() + 
  ggtitle("Ley de Zipf aplicado a nuestras cartas")

#separated by year
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = year, fill = "black")) + 
  scale_color_gradient(low = 'orange', high = 'forestgreen') +
  geom_line(alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10() + 
  facet_wrap(~year) +
  theme_minimal() + 
  ggtitle("Ley de Zipf, división por años ")

rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

#plot
freq_by_rank %>% 
  ggplot() + 
  geom_abline(intercept = -1.0544, slope = -0.9037, color = "gray50", linetype = 2) +
  geom_line(aes(rank, `term frequency`, color = year), size = 0.5, alpha = 0.25, show.legend = TRUE) + 
  scale_color_gradient(low = 'red', high = 'forestgreen') +
  scale_x_log10() +
  scale_y_log10() + 
  theme_minimal() 

#plot by year
freq_by_rank %>% 
  ggplot() + 
  geom_abline(intercept = -1.0544, slope = -0.9037, color = "gray50", linetype = 2) +
  geom_line(aes(rank, `term frequency`, color = year), size = 1, show.legend = TRUE) + 
  scale_color_gradient(low = 'red', high = 'forestgreen') +
  scale_x_log10() +
  scale_y_log10() + 
  theme_minimal() +
  facet_wrap(~year)


----
3#The bind_tf_idf function: close to zero if it appears a lot ####
each_letter_words_bind <- each_letter_words %>%
  bind_tf_idf(word, year, n)

each_letter_words_bind


#sorted
each_letter_words_bind %>%
  select(-total) %>%
  arrange(desc(tf_idf))


#plot
each_letter_words_bind %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(year) %>% 
  top_n(3) %>% 
  ungroup() %>%
  ggplot() +
  geom_col(aes(word, tf_idf, fill = year), show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~year, scales = "free") +
  coord_flip() + 
  theme_minimal() + 
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


#we can eliminate stop words
mystopwords <- tibble(word = c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", 
                               "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019"))

each_letter_words_bind_stop <- anti_join(each_letter_words_bind, mystopwords, 
                                         by = "word")
#plot
each_letter_words_bind_stop %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(year) %>% 
  top_n(5) %>% 
  ungroup() %>%
  ggplot() +
  geom_col(aes(word, tf_idf, fill = year), show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~year, scales = "free") +
  coord_flip() +
  theme_minimal() + 
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())






--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
#################### 8. Topic modeling####
#top100 = letter_words %>% 
 #count(word, sort=TRUE)

#top100 = filter(top100, n < 50) %>% 
  #summarise(word)

#eliminate comun words
#mystopwordsgeneral <- tibble(word = c("berkshire’s", "company", "insurance", "earnings", "berkshire", "business", "million", "billion", "businesses", "market", "tax"))

#letter_words_2<- anti_join(letter_words, top100, by = "word")

#letter_words_2 <- anti_join(letter_words, mystopwordsgeneral, by = "word")

letter_count = letter_words %>% 
  count(year, word, sort = TRUE) %>%
  ungroup()
letter_count

#8.1 LDA on years
years_dtm <- letter_count %>%
  cast_dtm(year, word, n)
years_dtm

#use the LDA() function to create a model.
years_lda <- LDA(years_dtm, k = 16, control = list(seed = 1233))
years_lda
#we can examine per-topic-per-word probabilities.
years_topics <- tidy(years_lda, matrix = "beta")
years_topics
#top 5 por cada topic
top_terms <- years_topics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms
#plot
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  theme (axis.text.x = element_text(size=rel(0.6), angle=0)) +
  theme (axis.text.y = element_text(size=rel(0.8)))

  
#8.2 Per-document classification
years_gamma <- tidy(years_lda, matrix = "gamma")
years_gamma

#clasificar no supervisadamente el paso inverso. Vemos como se autoclasifican
years_gamma %>%
  mutate(title = reorder(document, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() + 
  facet_wrap(~ document) + 
  ggtitle("Clasificación no supervisada para cada carta") + 
  theme_minimal()

#Cuantificar numericamente el resultado anterior
years_classifications <- years_gamma %>%
  group_by(document) %>%
  top_n(1, gamma) %>%
  ungroup()
years_classifications

letters_topics <- years_classifications %>%
  count(document, topic) %>%
  group_by(document) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = document, topic)

years_classifications %>%
  inner_join(letters_topics, by = "topic") %>%
  filter(document != consensus)

#check plot
ggplot(years_gamma, aes(gamma)) +
  geom_histogram(binwidth = 0.1) +
  scale_y_log10() +
  labs(title = "Distribution of probabilities for all topics",
       y = "Number of documents", x = expression(gamma))

ggplot(years_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(show.legend = FALSE, binwidth = 0.1) +
  facet_wrap(~ topic, ncol = 4) +
  scale_y_log10() +
  labs(title = "Distribution of probability for each topic",
       y = "Number of documents", x = "expression(gamma)")

#8.3 By word assignments
assignments <- augment(years_lda, data = years_dtm)
assignments
#check
assignments <- assignments %>%
  inner_join(letters_topics, by = c(".topic" = "topic"))
assignments
#plot Confusion matrix
assignments %>%
  count(document, consensus, wt = count) %>%
  group_by(document) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(consensus, document, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = "red") + # label = percent_format()
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Año de asignación",
       y = "Año correcto",
       fill = "% of assignments") +
  ggtitle("Matriz de clasificación")


wrong_words <- assignments %>%
  filter(title != consensus)

wrong_words

wrong_words %>%
  count(document, consensus, term, wt = count) %>%
  ungroup() %>%
  arrange(desc(n))


--------------------------------------------------------------
  #Ejemplo con 2 temas
  
  beta_spread <- years_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1)) %>% 
  top_n(30, abs(log_ratio)) %>% 
  mutate(term = reorder(term, log_ratio))

beta_spread

ggplot(beta_spread) +
  geom_col(aes(term, log_ratio, fill = log_ratio > 0)) +
  coord_flip() + 
  theme_minimal() +
  theme(legend.position="none") +
  ggtitle("Palabras que más contribuyen a cada tema")
----------------------------------------------------------
  
  
  
  
  
#Mas opciones para probar
  
  
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

years_dtm <- letter_count %>%
  cast_dtm(year, word, n)
years_dtm

years_lda <-LDA(years_dtm,k = 4, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))


#we can examine per-topic-per-word probabilities.
years_topics <- tidy(years_lda, matrix = "beta")
years_topics
#top 5 por cada topic
top_terms <- years_topics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms
#plot
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  theme (axis.text.x = element_text(size=rel(0.5), angle=45)) 

