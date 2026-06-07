
library(rvest)
library(dplyr)
library(stringr)

# Funkcja do ekstrakcji danych z pojedynczego nekrologu ----
get_obit_data <- function(url) {

	# Pobranie kodu HTML strony nekrologu
  page <- tryCatch(
    read_html(url),
    error = function(e) {
      return(NULL) 
    }
  )

	# Obsługa nieudanego pobrania strony.
  if (is.null(page)) {
    return(list(
      region = NA_character_, 
      author = NA_character_, 
      text = NA_character_
    ))
  }

  # Ekstrakcja regionu
  region <- page %>% 
    html_element(xpath = "//div[@class='kL' and contains(text(), 'Region:')]/following-sibling::div[@class='kR']") %>% 
    html_text(trim = TRUE)

  # Ekstrakcja autora lub podpisu
  author <- page %>% 
    html_elements("div.podpis") %>% 
    html_text(trim = TRUE) %>% 
    .[. != ""] %>% 
    .[1]

  # Ekstrakcja właściwego tekstu
  page %>% 
    html_elements(".NKIindeks") %>% 
    xml2::xml_remove()

  text <- page %>% 
    html_element("#ogl") %>% 
    html_text2()

  return(list(
    region = if (length(region) == 0 || is.na(region)) NA_character_ else region,
    author = if (length(author) == 0 || is.na(author)) NA_character_ else author,
    text   = if (length(text) == 0 || is.na(text)) NA_character_ else text
  ))
}

# Główny scraper ----
get_obits <- function(max_obits = 100, min_chars = 0) {

  message(sprintf("Target: %d nekrologów na rok (min. %d znaków) od 2026 do 2008", max_obits, min_chars))

	# Lista analizowanych lat oraz licznik postępu
  target_years <- as.character(2026:2008)
  year_counters <- setNames(rep(0, length(target_years)), target_years)

	# Lista tymczasowa na pojedyncze rekordy
  all_obits <- list()

	# Zbiór przeczytanych tekstów
  seen_texts <- character(0)

  page_num <- 1
  
  stop_scan <- FALSE
  
  # ---- oszacowane lokalizacje początków kolejnych lat ----
  jump_map <- c(
    "2025" = 84,   "2024" = 169,  "2023" = 253,  "2022" = 338,
    "2021" = 507,  "2020" = 676,  "2019" = 845,  "2018" = 1014,
    "2017" = 1183, "2016" = 1352, "2015" = 1521, "2014" = 1690,
    "2013" = 2028, "2012" = 2366, "2011" = 2535, "2010" = 2791,
    "2009" = 3190, "2008" = 3218
  )

  tryCatch({
    while (!stop_scan) {
      
			# Konstrukcja adresu URL strony z listą wyników
      url <- sprintf("https://nekrologi.wyborcza.pl/0,10,,%d,0,100,,,,,,,,,,,,false.html", page_num)
      
      # Konfiguracja mechanizmu ponownych prób wjeścia na stronę
      max_retries <- 10
      retry_count <- 0
      page <- NULL
      nodes <- list()
      
			# Mechanizm poniwień
      while (retry_count < max_retries) {
        page <- tryCatch(
          read_html(url),
          error = function(e) { return(NULL) }
        )
        
        if (!is.null(page)) {
          nodes <- page %>% html_elements("div.NSearchM ul li")
          if (length(nodes) > 0) {
            break
          }
        }
        
        retry_count <- retry_count + 1
        message(sprintf("  -> Serwer odrzucił stronę %d lub jest pusta. Ponowna próba (%d/%d)...", page_num, retry_count, max_retries))
        Sys.sleep(runif(1, 8, 15))
      }
      
      if (is.null(page) || length(nodes) == 0) {
        message(sprintf("\nBrak wyników na stronie %d po %d próbach. Koniec skanu.", page_num, max_retries))
        break
      }
      
      message(sprintf("Skanowanie strony %d...", page_num))
      
			# Przetwarzanie pojedynczych wyników znajdujących się na stronie
      for (i in seq_along(nodes)) {

        item <- nodes[[i]]

				# Ekstrakcja daty
        date_text <- item %>% html_element("h3 a span") %>% html_text(trim = TRUE)
        clean_date <- str_extract(date_text, "\\d{2}\\.\\d{2}\\.\\d{4}")
      
        if (is.na(clean_date)) next

				# Wyodrębnienie roku
        year <- str_extract(clean_date, "\\d{4}$")
      
        if (as.numeric(year) < 2008) {
          message("\n Osiągnięto rok starszy niż 2008. Koniec skanu.")
          stop_scan <- TRUE
          break
        }

				# Sprawdzenie, czy dany rok znajduje się w zakresie i czy nie osiągnięto
        # jeszcze limitu zaakceptowanych nekrologów dla tego roku.
        if (year %in% names(year_counters) && year_counters[year] < max_obits) {
          
          # Ekstrakcja linku
          link <- item %>% html_element("h3 a") %>% html_attr("href")
          full_link <- paste0("https://nekrologi.wyborcza.pl/", link)

          # Pobranie danych nekrologu z linku
          obit_data <- get_obit_data(full_link)
          
          if (is.null(obit_data) || is.na(obit_data$text)) next
          
          # Do zbioru trafiają tylko nekrologi spełniające wymóg min_chars
          if (nchar(obit_data$text) >= min_chars) {
            
						# odrzucenie nekrologu z wcześniej widzianym tekstem
            if (obit_data$text %in% seen_texts) next 
            seen_texts <- c(seen_texts, obit_data$text)

						# Utworzenie jednego rekordu danych i dopisanie go do listy wyników
            all_obits[[length(all_obits) + 1]] <- tibble(
              url = full_link,
              date = clean_date,
              year = as.numeric(year),
              region = obit_data$region,
              author = obit_data$author,
              text = obit_data$text         
            )
          
            # Aktualizacja licznika dla danego roku
            year_counters[year] <- year_counters[year] + 1
            message(sprintf("  [%s] Znaleziono poprawny nekrolog (%d/%d)", year, year_counters[year], max_obits))

            # ---- NOWY PRZESKOK STRON ----
            if (year_counters[year] == max_obits) {
              message(sprintf("  -> Limit dla roku %s osiągnięty. Skok przez strony...", year))
              
              target_year <- as.character(as.numeric(year) - 1)
              if (target_year %in% names(jump_map)) {
                page_num <- jump_map[[target_year]]
              }
              
              break
            }
          }
        }
      
        # Przerwanie wewnętrznej pętli, jeśli w trakcie osiągnięto cel dla wszystkich lat 
        if (all(year_counters >= max_obits)) {
          message("\nCel osiągnięty dla wszystkich lat (2026-2008). Koniec skanu.")
          stop_scan <- TRUE
          break
        }
      }
      
			# Wyjście z głównej pętli po ustawieniu flagi kończącej
      if (stop_scan) break
      
			# Następna strona
      page_num <- page_num + 1
      Sys.sleep(runif(1, 1.5, 3))
    }
  }, interrupt = function(cond) {
		# Obsługa ręcznego przerwania działania skryptu.
    # Dzięki temu funkcja przechodzi do sekcji końcowej i może zwrócić dane,
    # które zostały pobrane do momentu przerwania.
    message("\n[!] Otrzymano sygnał przerwania, zapis nie pełnych danych...")
  })

	# Zwrócenie pustej tabeli, jeśli nie udało się zebrać żadnego rekordu
  if (length(all_obits) == 0) return(tibble())

	# Połączenie rekordów w jeden zbiór oraz usunięcie duplikatów po adresie URL
  dataset <- bind_rows(all_obits) %>% distinct(url, .keep_all = TRUE)

	# Raport końcowy pokazujący, ile nekrologów zebrano dla każdego roku
  message("\n---- Raport liczności ----")
  print(year_counters)

  return(dataset)
}

# Konfiguracja i zapis ----

# Ustaw liczbę nekrologów dla każdego roku i minimalną długość tekstu nekrologu
max_obits <- 50    # - pobierze po max_obits nekrologow z każdego roku
min_chars <- 500   # - pobierze nekrologi o długości nie mniejszej niż min_chars

# Uruchomienie scrapera
csv_path <- paste0("nekrologi_2008-2026.csv")
write.csv(get_obits(max_obits, min_chars), csv_path, row.names = FALSE)
message("\nZapisano dane do pliku: ", csv_path)
