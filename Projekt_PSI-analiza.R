#' ---
#' title: "Projekt zaliczeniowy"
#' author: "Autor: Malina Kolodziej, Wladyslaw Gutkowski, Zuzanna 
#' Szwaj"
#' date: "`r Sys.Date()`"
#' output:
#'   html_document:
#'     df_print: paged
#'     theme: readable      # Wygląd (bootstrap, cerulean, darkly, journal, lumen, paper, readable, sandstone, simplex, spacelab, united, yeti)
#'     highlight: kate      # Kolorowanie składni (haddock, kate, espresso, breezedark)
#'     toc: true            # Spis treści
#'     toc_depth: 3
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: true
#'     code_folding: hide    # Kod domyślnie zwinięty (estetyczniej)
#'     number_sections: true # Numeruje nagłówki (lepsza nawigacja)
#'     css: "custom.css"     # Możliwość stworzenia własnego stylowania (opcjonalne)
#' ---


knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE
)


#' # Wymagane pakiety
# Wymagane pakiety ----
library(tm)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(ggplot2)
library(ggthemes)
library(topicmodels)
library(reshape2)


#' # Dane tekstowe
# Dane tekstowe ----

# Ustaw Working Directory!
# Załaduj dokumenty z folderu
# docs <- DirSource("nekrologi_2008-2026")
# W razie potrzeby dostosuj ścieżkę
# np.: docs <- DirSource("C:/User/Documents/nekrologi_2008-2026")

# Utwórz korpus dokumentów tekstowych

data <- read.csv2("nekrologi_2008-2026.csv", stringsAsFactors = FALSE, 
                  encoding = "UTF-8")

# Oczyszczanie danych na poziomie Data Frame
data <- data %>%
  filter(!is.na(text) & text != "") %>%      # Usunięcie pustych tekstów
  filter(!is.na(year)) %>%                   # Usunięcie brakujących dat/lat
  distinct(text, .keep_all = TRUE) %>%       # Usunięcie identycznych duplikatów wpisów
  rename(rok = year)                         # Zmiana nazwy kolumny dla wygody

corpus <- VCorpus(VectorSource(data$text))

#' # Przetwarzanie i oczyszczanie tekstu ----
# (Text Preprocessing and Text Cleaning)


# Normalizacja i usuniecie zbędnych znaków ----

# Zapewnienie kodowania w całym korpusie
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))


# Funkcja do zamiany znaków na spację
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))

# usuniecie znakow nowej linii enter (/n)
corpus <- tm_map(corpus, content_transformer(function(x) gsub("\n", " ", x)))
corpus <- tm_map(corpus, content_transformer(function(x) gsub("\\s+", " ", x)))
corpus <- tm_map(corpus, content_transformer(trimws))

#usuniecie stopwords 
nekrologi_stopwords <- c(
  "i", "w", "z", "na", "do", "się","dla", "że", "nie", "to", "jak",
  "ale", "już", "po", "był", "była", "było", "jest", "jako",
  "zmarł", "zmarła", "odszedł", "odeszła", "roku", "dnia","dniu",
  "rodzina", "bliscy", "oraz", "czym", "przy", "pan", "pani","hab", "prof", "dr", "mgr",
  "nasz", "nasze","wyrazy","współczucia", "naszej","jego","jej","przez", "naszego", "sie", "sobie",
  "jako", "przy", "swoim", "swoje", "swoja", "wszystkich","lat","jej","latach","nas","tak"
)
corpus <- tm_map(corpus, removeWords, nekrologi_stopwords)


corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)

# Sprawdzenie
corpus[[1]][[1]]

#' # Eksploracyjna analiza danyc


#' ##  Analiza częstości słów
# Stworzenie macierzy Text Document Matrix i Document-Term Matrix
tdm <- TermDocumentMatrix(corpus)
tdm_m <- as.matrix(tdm)
dtm <- DocumentTermMatrix(corpus)
# Zliczanie częstości słów ----
v <- sort(rowSums(tdm_m), decreasing = TRUE)
tdm_df <- data.frame(word = names(v), freq = v)

# Chmura slow
wordcloud(words = tdm_df$word, 
          freq = tdm_df$freq, 
          min.freq = 30,        # tylko słowa pojawiające się min. 30 razy max 50 slow
          max.words = 50,
          random.order = FALSE,
          rot.per = 0,
          scale = c(2, 0.3),
          colors = brewer.pal(8, "Dark2") )

# Wyświetl top 20
print(head(tdm_df, 20))

#' ##  Analiza częstości słów TF-IDF
# Macierz częstości TDM z TF-IDF (dla Machine Learningu)
tdm_tfidf <- TermDocumentMatrix(corpus, control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
tdm_tfidf_m <- as.matrix(tdm_tfidf)

# Zlicz same częstości słów w macierzach

v_tfidf <- sort(rowSums(tdm_tfidf_m), decreasing = TRUE)
tdm_tfidf_df <- data.frame(word = names(v_tfidf), freq = v_tfidf)
head(tdm_tfidf_df, 10)

# Chmura słów 
wordcloud(words = tdm_tfidf_df$word, freq = tdm_tfidf_df$freq, min.freq = 30,        # tylko słowa pojawiające się min. 30 razy max 50 slow
          max.words = 50,
          random.order = FALSE,
          rot.per = 0,
          scale = c(1, 0.3), 
          colors = brewer.pal(8, "Dark2"))


# Wyświetl top 20
print(head(tdm_tfidf_df, 20))
#' # Topi modeling
#' ##  Funkcja top_terms_by_topic_LDA 
top_terms_by_topic_LDA <- function(input_dtm,
                                   plot = TRUE,
                                   k = number_of_topics)
{    
  # usuń puste wiersze
  rowTotals <- apply(input_dtm, 1, sum)
  input_dtm <- input_dtm[rowTotals > 0, ]
  
  # wykonaj LDA
  lda <- LDA(input_dtm, k = k, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta")
  
  top_terms <- topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  if(plot == TRUE){
    top_terms %>%
      mutate(term = reorder(term, beta)) %>%
      ggplot(aes(term, beta, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") +
      labs(x = "Terminy", y = "β (ważność słowa w temacie)") +
      coord_flip() +
      theme_minimal() +
      scale_fill_brewer(palette = "Set1")
  } else {
    return(top_terms)
  }
}

# Wywołanie – przekazujesz dtm a nie tdm_df$word!
number_of_topics = 2
top_terms_by_topic_LDA(dtm, k = 2)

number_of_topics = 3
top_terms_by_topic_LDA(dtm, k = 3)

number_of_topics = 4
top_terms_by_topic_LDA(dtm, k = 4)

number_of_topics = 6
top_terms_by_topic_LDA(dtm, k = 6)
