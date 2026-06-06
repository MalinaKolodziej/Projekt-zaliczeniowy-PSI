
#' ---
#' title: "Analiza nekrologów - projekt PSI"
#' author: "Autor: "
#' date: "`r Sys.Date()`"
#' output:
#'   html_document:
#'     df_print: paged
#'     theme: spacelab
#'     highlight: kate
#'     toc: true
#'     toc_depth: 3
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: true
#'     code_folding: hide
#'     number_sections: true
#' ---

knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE
)


# Wymagane pakiety ----
library(tm)
library(tidytext)
library(tidyverse)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(ggthemes)

# 0. Wczytanie danych ----

data <- read.csv("nekrologi_2008-2026.csv", 
                 sep = ";", 
                 stringsAsFactors = FALSE,
                 encoding = "UTF-8")

# Korpus tekstowy z kolumny `text`
corpus <- VCorpus(VectorSource(data$text))

corpus[[1]]
corpus[[1]][[1]]
corpus[[1]][2]

# 1. Przetwarzanie i oczyszczanie tekstu ----

# Zapewnienie kodowania w całym korpusie
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))


# Funkcja do zamiany znaków na spację
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))

# Usunięcie zbędnych znaków

corpus <- tm_map(corpus, toSpace, "\n")

#sprawdzenie
corpus[[1]][[1]]

# Zamiana na małe litery, usunięcie stop words i interpunkcji
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("pl"))
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)

#sprawdzenie
corpus[[1]][[1]]

# Usunięcie formuł grzecznościowych i słów typowych dla nekrologów,
# które nie wnoszą specjalnej wartości 
corpus <- tm_map(corpus, removeWords, c(
  "roku", "dniu", "dnia", "pan", "pani", "prof", "dr", "mgr",
  "nasz", "nasze", "naszej", "naszego", "sie", "sobie",
  "jako", "przy", "swoim", "swoje", "swoja", "wszystkich", "się", "godzinie",
  "lat", "czym", "wyrazy", "nastąpi", "współczucia", "cię",
  "głębokim", "oraz"
))

corpus <- tm_map(corpus, stripWhitespace)

# Sprawdzenie
corpus[[1]][[1]]

# Macierz częstości TDM ----

tdm <- TermDocumentMatrix(corpus)
tdm_m <- as.matrix(tdm)


# 2. Zliczanie częstości słów ----
# (Word Frequency Count)


# Zlicz częstości słów
v <- sort(rowSums(tdm_m), decreasing = TRUE)
tdm_df <- data.frame(word = names(v), freq = v)
head(tdm_df, 10)


# 3. Analiza danych
wordcloud(words = tdm_df$word, freq = tdm_df$freq, min.freq = 30,
          colors = brewer.pal(8, "Dark2"))


# Wyświetl top 10
print(head(tdm_df, 10))
