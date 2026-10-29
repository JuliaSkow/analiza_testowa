# biblioteki + rozwiązujemy w swoich gołązkach swoje zadania

### Zadanie 7 — wieś i duże miasta

# Wbierz respondentów z kategorii `miejscowosc` równych `"wieś"` i `"miasto 500 000 
# i więcej"`. Dla każdej z tych dwóch grup policz **procent** odpowiedzi na pytanie 
# `kierunek` (procent w obrębie grupy, nie w całej tabeli). Czy udział odpowiedzi 
# „W złym” był taki sam?
  
##  Sprawdź liczbę respondentów w obu grupach. Samo porównanie procentów bez 
##  liczebności może być mylące.

library(dplyr)
library(tidyr)
library(haven)
library(haven)
adres_cbos <- paste0("https://raw.githubusercontent.com/kflisikowsky/sad/",
                     "main/data/CBOS_201733_331_SPSS.sav")
plik_cbos <- tempfile(fileext = ".sav")
download.file(adres_cbos, plik_cbos, mode = "wb", quiet = TRUE)
cbos_raw <- read_sav(plik_cbos)
unlink(plik_cbos)

cbos <- cbos_raw %>% transmute(
  plec = as_factor(plec),
  miejscowosc = as_factor(klm6),
  kierunek = as_factor(q24),
  prognoza = as_factor(q25),
  wyksztalcenie = as_factor(q15)
)

?filter


miasta <- filter(cbos,miejscowosc == "miasto 500 000 i więcej")

table(cbos$kierunek)

wies_i_miasto <- cbos |>
  filter(miejscowosc == "miasto 500 000 i więcej" | miejscowosc == "wieś") |>
  group_by(miejscowosc) |>
  count(kierunek) |>
  mutate(procent = round((n / sum(n)) * 100, 2)) |>
  filter(kierunek == "W złym")

# w wynniku przeprowadzonej analizy mamy taki rezultat, że w miastach (61,45%) jest większy procent
# osób które odpowiedziały, że sytuacja zmierza w złym kierunku niż na wsi (28,15%).
# Jednak liczebność gróp była różna. W mieście mamy 51 respondenty, a na wsi - 114, co znacznie 
# wpływa na wiarygodność porównania.


### Zadanie 8 — ocena teraźniejszości a oczekiwania

# Zrób tabelę liczności odpowiedzi na `prognoza`. Następnie ogranicz dane do osób 
# oceniających sytuację w kraju jako `"W złym"` i sprawdź, jak odpowiadały na pytanie o 
# przyszłość. Która odpowiedź była najczęstsza w tej grupie? Czy na podstawie tej tabeli 
# można wyciągać wnioski przyczynowe?

licznosc <- cbos |>
  group_by(prognoza) |>
  count(prognoza)

pesymisci <- cbos |>
  filter(kierunek == "W złym") |>
  count(prognoza)

# Najczęstrza odpowiedź u pesymistów o prognozie była, że sytuacja w kraju "Nie zmieni się"
# Na podstawie samej tabieli i rezultatów ankiety cziężko jest wyciągnąć wnioski przyczynowe