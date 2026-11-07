## 3. CBOS: jak respondenci oceniali sytuację w Polsce w 2017 r.?

# Wczytanie potrzebnych pakietow
library(dplyr)
library(haven)


# Wczytanie danych CBOS
adres_cbos <- "https://raw.githubusercontent.com/kflisikowsky/sad/main/data/CBOS_201733_331_SPSS.sav"

plik_cbos <- tempfile(fileext = ".sav")

download.file(adres_cbos, plik_cbos, mode = "wb")

cbos_raw <- read_sav(plik_cbos)

unlink(plik_cbos)


# Przygotowanie danych do analizy
cbos <- cbos_raw %>% transmute(
  plec = as_factor(plec),
  miejscowosc = as_factor(klm6),
  kierunek = as_factor(q24),
  prognoza = as_factor(q25),
  wyksztalcenie = as_factor(q15))

# Sprawdzenie danych
dim(cbos)
head(cbos)
str(cbos)
summary(cbos)

# Sprawdzenie brakow danych
colSums(is.na(cbos))
#W badaniu uczestniczyło 925 osób, nie odnotowano braków danych.

# Zadanie 5 - poznaj respondentow

# Liczba i procent kobiet oraz mezczyzn
cbos %>%
  filter(!is.na(plec)) %>%
  count(plec) %>%
  mutate(procent = 100 * n / sum(n))
#Kobiety stanowiły 53,9% badanych, a mężczyźni 46,1%.

# Liczba i procent mieszkancow poszczegolnych miejscowosci
cbos %>%
  filter(!is.na(miejscowosc)) %>%
  count(miejscowosc) %>%
  mutate(procent = 100 * n / sum(n))
#Najwięcej respondentów mieszkało na wsi (43,8%).

# Odsetek osob mieszkajacych na wsi
cbos %>%
  filter(!is.na(miejscowosc)) %>%
  summarise(procent = 100 * sum(miejscowosc == "wieś") / n())
#Najwięcej respondentów mieszkało na wsi (43,8%).

# Odsetek osob mieszkajacych w miastach 500 tys. i wiecej
cbos %>%
  filter(!is.na(miejscowosc)) %>%
  summarise(procent = 100 * sum(miejscowosc == "miasto 500 000 i więcej") / n())
#W miastach liczących co najmniej 500 tys. mieszkańców mieszkało 8,97% badanych.



# Zadanie 6 - jak oceniano kierunek zmian w kraju?

# Liczba i procent poszczegolnych odpowiedzi
cbos %>%
  filter(!is.na(kierunek)) %>%
  count(kierunek) %>%
  arrange(desc(n)) %>%
  mutate(procent = 100 * n / sum(n))
#Najczęściej wybieraną odpowiedzią było „W dobrym” (46,7%).
#Odpowiedź „W złym” wskazało 37,3% badanych.
#„Trudno powiedzieć” wybrało 15,7%, a odmowy stanowiły 0,324%.

# Porownanie odpowiedzi W dobrym i W zlym
cbos %>%
  filter(kierunek %in% c("W dobrym", "W złym")) %>%
  count(kierunek) %>%
  mutate(procent = 100 * n / sum(n))
#Wśród osób wybierających „W dobrym” lub „W złym” udział odpowiedzi „W dobrym” wyniósł 55,6%.

# Procent odpowiedzi W dobrym w calej probie
cbos %>%
  filter(!is.na(kierunek)) %>%
  summarise(procent = 100 * sum(kierunek == "W dobrym") / n())
#W całej próbie odsetek ten wyniósł 46,7%.
#Różnica wynika z mianownika: w pierwszym przypadku uwzględniono tylko odpowiedzi „W dobrym” i „W złym”, a w drugim wszystkie 925 odpowiedzi.


