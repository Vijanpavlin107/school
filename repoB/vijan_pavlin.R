#globalno potrebne knjiznice
library(tidyverse)
library(readxl)

#POMEMBNO: moji tibbli so poimenovani kot 'dano_ime_1' napram imenom ki so bila dana 'dano_ime'

# podatkovne tabele ki jih moram sestaviti: dirke, etape, indikatorji, kolesarji, zmage

################################################################################
#1-ETAPE
podatki_etape <- "tdf_etape_rojstva.xlsx"

listi <- excel_sheets(podatki_etape)

podatki_etape <- read_excel(podatki_etape, sheet = listi[1])

kolesarji_1 <-read_excel("tdf_etape_rojstva.xlsx", sheet=listi[2])

etape_1 <- podatki_etape %>%
  unite("id_etapa", year, stage, sep = "_", remove = FALSE) %>%
  select(-stage) %>%
  unite("datum", date, year, sep = "-") %>%
  mutate(datum=dmy(datum)) %>%
  separate(course, into=c("start_mesto", "cilj_mesto"), sep = " to ", fill = "right") %>%
  mutate(cilj_mesto = coalesce(cilj_mesto, start_mesto)) %>%
  select(-winner) %>%
  rename(razdalja = distance) %>%
  rename(tip = type) %>%
  mutate(
    start_mesto = str_replace(start_mesto, " \\(.*\\)", ""),
    cilj_mesto  = str_replace(cilj_mesto,  " \\(.*\\)", "")
  )

#sem uprabil dano datoteko ker nisem nasel vzorca v zaporeju levelov
leveli <- levels(etape$tip)

etape_1$tip  <- factor(etape_1$tip, levels = leveli)

identical(etape, etape_1)

################################################################################
#2-DIRKE
library(rvest)


url_GIRO <- "https://kt.ijs.si/~ljupco/lectures/appr-2526/dn1/List%20of%20Giro%20d'Italia%20general%20classification%20winners%20-%20Wikipedia.html"
url_VUELTA <- "https://kt.ijs.si/~ljupco/lectures/appr-2526/dn1/List%20of%20Vuelta%20a%20Espana%20general%20classification%20winners%20-%20Wikipedia.html"
url_TOUR <- "https://kt.ijs.si/~ljupco/lectures/appr-2526/dn1/Tour%20de%20France%20Statistics.html"

page_GIRO <- read_html(url_GIRO)
page_VUELTA <- read_html(url_VUELTA)
page_TOUR <- read_html(url_TOUR)

tables_GIRO <- page_GIRO %>% html_table(fill = TRUE)
tables_VUELTA <- page_VUELTA %>% html_table(fill=TRUE)
tables_TOUR <- page_TOUR %>% html_table(fill=TRUE)



#GIRO
 GIRO <- tables_GIRO[[2]] %>%
   select(Year, Distance) %>%
   mutate(
     Distance = Distance %>%
       str_extract("^[0-9,]+") %>%
       str_replace_all(",", "") %>%
       as.numeric()
   ) %>%
   mutate(dirka = "Giro d'Italia") %>%
   relocate(dirka, .after = 1) %>%
   drop_na() %>%
   mutate(n_start = NA) %>%
   mutate(n_cilj = NA) %>%
   rename(leto = Year) %>%
   rename(razdalja = Distance)
   
 
 
 #VUELTA
 VUELTA <- tables_VUELTA[[2]] %>%
   select(Year, Distance) %>%
   mutate(
     Distance = Distance %>%
       str_extract("^[0-9.,]+") %>%
       str_replace_all(",", "") %>%
       as.numeric()
   ) %>%
   mutate(dirka = "Vuelta a España") %>%
   relocate(dirka, .after = 1) %>%
   drop_na() %>%
   mutate(n_start = NA) %>%
   mutate(n_cilj = NA) %>%
   rename(leto = Year) %>%
   rename(razdalja = Distance)
   
#TOUR
 t1 <- slice(tables_TOUR[[3]], -1)
 t2 <- tables_TOUR[[4]] %>% mutate(across(everything(), as.character))
 
 
 TOUR <- bind_rows(t1, t2) %>%
   select(X1, X3, X6, X7) %>%
   rename(leto = X1)%>%
   rename(razdalja = X3)%>%
   rename(n_start = X6)%>% 
   rename(n_cilj = X7) %>%
   mutate( 
    leto = str_extract(leto, "^[0-9]+")
   ) %>%
   mutate(dirka = "Tour de France") %>%
   relocate(dirka, .after = 1) %>%
   drop_na() %>%
   mutate(
     razdalja = razdalja %>%
       str_extract("^[0-9.,]+") %>%
       str_replace_all(",", "") %>%
       as.numeric()
   )
 
 TOUR[13, "n_cilj"] <- str_extract(TOUR[13, "n_cilj"], "^[0-9]+")
   
 seznam_dirk <- lapply(list(GIRO, VUELTA, TOUR), function(x) {
   x %>% mutate(across(everything(), as.character))
 })
 
dirke_1 <- bind_rows(seznam_dirk[[1]], seznam_dirk[[3]], seznam_dirk[[2]])
 

dirke_1$leto <- as.integer(dirke_1$leto)
dirke_1$dirka  <- factor(dirke_1$dirka, levels = c("Giro d'Italia","Tour de France","Vuelta a España"))
dirke_1$n_start <- as.integer(dirke_1$n_start)
dirke_1$n_cilj <- as.integer(dirke_1$n_cilj)
dirke_1$razdalja <- as.numeric(dirke_1$razdalja)


#vrednosti v dirke niso enako kot na spletni strani
identical(dirke[-nrow(dirke),], dirke_1)

################################################################################
#3-KOLESARJI
kolesarji_1 <- kolesarji_1 %>%
  mutate(
    datum_rojstva = as.Date(datum_rojstva, format = "%d.%m.%Y")
  )

identical(kolesarji, kolesarji_1)











################################################################################
#4-ZMAGE
library(dplyr)
library(stringr)
library(rvest)


url_tour_general <- "https://kt.ijs.si/~ljupco/lectures/appr-2526/dn1/List%20of%20Tour%20de%20France%20general%20classification%20winners%20-%20Wikipedia.html"
url_tour_secondary <- "https://kt.ijs.si/~ljupco/lectures/appr-2526/dn1/List%20of%20Tour%20de%20France%20secondary%20classification%20winners%20-%20Wikipedia.html"
url_vuelta_general <- "https://kt.ijs.si/~ljupco/lectures/appr-2526/dn1/List%20of%20Vuelta%20a%20Espana%20general%20classification%20winners%20-%20Wikipedia.html"
url_giro_general <- "https://kt.ijs.si/~ljupco/lectures/appr-2526/dn1/List%20of%20Giro%20d'Italia%20general%20classification%20winners%20-%20Wikipedia.html"
url_points <- "https://kt.ijs.si/~ljupco/lectures/appr-2526/dn1/Points%20classification%20in%20the%20Tour%20de%20France%20-%20Wikipedia.html"
url_mladi <- "https://kt.ijs.si/~ljupco/lectures/appr-2526/dn1/Young%20rider%20classification%20in%20the%20Tour%20de%20France%20-%20Wikipedia.html"
url_hribi <- "https://kt.ijs.si/~ljupco/lectures/appr-2526/dn1/Mountains%20classification%20in%20the%20Tour%20de%20France%20-%20Wikipedia.html"

#funkcija za pretvorbo html v tabelo
tabeliraj <- function (url, i) {
  tabela <- read_html(url) %>%
    html_table(fill=TRUE)
  
  return(tabela[[i]])
}

#funkcija za pretvorbo casovnega zapisa za skupni cas
convert_time <- function(x) {
  x <- as.character(x)
  
  # handle missing values
  if (is.na(x)) return(NA_integer_)
  
  # regex pattern
  pattern <- "^\\+?\\s*(?:(\\d+)h\\s*)?(?:(\\d+)'\\s*)?(?:(\\d+)\")?$"
  
  # if it does not match → return original
  if (!str_detect(x, pattern)) return(as.integer(x))
  
  # extract numbers
  parts <- str_match(x, pattern)
  
  hours   <- as.integer(parts[2])
  minutes <- as.integer(parts[3])
  seconds <- as.integer(parts[4])
  
  return(hours*3600 + minutes*60 + seconds)
}



#funkcija za pretvorbo casovnega zapisa za prednost
convert_time_prednost <- function(x) {
  x <- as.character(x)
  
  # empty or NA
  if (is.na(x) || x == "") return(NA_integer_)
  
  # strip the leading plus and spaces
  x_clean <- str_remove(x, "^\\+\\s*")
  
  # regex to catch h m s
  pattern <- "^\\s*(?:(\\d+)h)?\\s*(?:(\\d+)')?\\s*(?:(\\d+)\")?\\s*$"
  
  # does it match?
  if (!str_detect(x_clean, pattern)) {
    # numeric case -> "2", "18", "50"
    if (str_detect(x_clean, "^\\d+$")) {
      return(as.integer(x_clean))
    }
    return(NA_integer_)   # everything else → NA
  }
  
  # extract h m s
  parts <- str_match(x_clean, pattern)
  
  hours   <- as.integer(parts[,2])
  minutes <- as.integer(parts[,3])
  seconds <- as.integer(parts[,4])
  
  # NA becomes 0 for missing parts
  hours[is.na(hours)]     <- 0
  minutes[is.na(minutes)] <- 0
  seconds[is.na(seconds)] <- 0
  
  total <- hours*3600 + minutes*60 + seconds
  return(total)
}


#vse podatke pretvorim v tabele
tabela_tour_general <- tabeliraj(url_tour_general, 3)
tabela_vuelta_general <- tabeliraj(url_vuelta_general, 2)
tabela_tour_secondary <- tabeliraj(url_tour_secondary, 4)
tabela_tocke <- tabeliraj(url_points, 6)
tabela_mladi <- tabeliraj(url_mladi, 2)
tabela_hribi <- tabeliraj(url_hribi, 6)
tabela_giro_general <- tabeliraj(url_giro_general, 2)

#imena stolpcev ki jih zelim v koncni tabeli
imena <- c("leto","dirka","id_etapa","kategorija","ime_priimek","drzava","ekipa","rezultat","prednost","rezultat_tip")
  







#GIRO
giro_general <- tabela_giro_general %>%
  #zacetni popravki
  select(Year, Cyclist, Country, 'Sponsor / team', 'Time / points', Margin) %>%
  mutate(
    dirka = "Giro d'Italia",
    id_etapa = NA,
    kategorija = "skupna",
    rezultat_tip = "točke"
  ) %>%
  #preimenovanje stolpcev
  rename(
    leto = Year,
    ime_priimek = Cyclist,
    drzava = Country,
    ekipa = 'Sponsor / team',
    rezultat = 'Time / points',
    prednost = 'Margin'
  ) %>%
  #filtracija nezeljenih stolpcev
  select(all_of(imena)) %>%
  filter(!ime_priimek %in% c("~Not contested due to World War I","~Not contested due to World War II", "—")) %>%
  # mutiranje v pravilne oblike podatkov
  mutate(
    dirka = factor(dirka, 
                   levels = c("Giro d'Italia", "Tour de France", "Vuelta a España")),
    kategorija = factor(kategorija, 
                        levels = c("etapna", "skupna", "skupna_hribi", "skupna_mladi", "skupna_tocke")),
    rezultat_tip = factor(rezultat_tip, 
                          levels = c("sekunde", "tocke")),
    id_etapa = as.character(id_etapa),
    
    prednost = sapply(prednost, convert_time_prednost),
    prednost = as.integer(prednost),
    rezultat = sapply(rezultat, convert_time),
    rezultat = as.integer(rezultat),
    rezultat_tip = if_else(rezultat > 100, "sekunde", "točke"),  # TRUE/FALSE , moram preveriti ce je zapis v tockah ali sekundah
    
    
    # ciscenje individualno
    ime_priimek = ime_priimek %>%
      str_remove_all("\\[.*?\\]") %>%
      str_remove_all("[*†‡#]") %>%
      str_squish()
  ) %>%
  
  # mutiranje specificnih vrednosti
  mutate(
    ime_priimek = case_when(
      ime_priimek == "Gösta Pettersson" ~ "Gosta Pettersson",
      ime_priimek == "Michele Scarponi[a]" ~ "Michele Scarponi",
      TRUE ~ ime_priimek
    ),
    drzava = case_when(
      drzava == "Great Britain" ~ "United Kingdom",
      TRUE ~ drzava
    ),
    ekipa =case_when(
      ekipa == "Legnano–Clément" ~ "Legnano–Clement",
      ekipa == "Maino–Clément" ~ "Maino–Clement",
      ekipa == "Fréjus" ~ "Frejus",
      ekipa == "Clément–Fuchs" ~ "Clement–Fuchs",
      ekipa == "Saint-Raphaël–Gitane–Dunlop" ~ "Saint-Raphael–Gitane–Dunlop",
      ekipa == "—" ~ NA_character_,
      TRUE ~ ekipa
    )
  )
  


#HRIBI
library(stringi)

hribi <- tabela_hribi %>%
  select(Year, Rider, Country, Team ) %>%
  mutate(
    dirka = "Tour de France",
    id_etapa = NA,
    kategorija = "skupna_hribi",
    rezultat_tip = NA,
    rezultat = NA,
    prednost = NA,
  ) %>%
  rename(
    leto = Year,
    ime_priimek = Rider,
    drzava = Country,
    ekipa = Team,
  ) %>%
  select(all_of(imena)) %>%
  mutate(across(everything(), ~ stri_trans_general(., "Latin-ASCII"))) %>%
  
  mutate(
    leto = as.integer(leto),
    dirka = factor(dirka, levels = c("Giro d'Italia", "Tour de France", "Vuelta a España")),
    kategorija = factor(kategorija, levels = c("etapna", "skupna", "skupna_hribi", "skupna_mladi", "skupna_tocke")),
    rezultat_tip = factor(rezultat_tip, levels = c("sekunde", "tocke")),
    id_etapa = as.character(id_etapa),
    prednost = as.integer(prednost),
    rezultat = sapply(rezultat, convert_time),
    rezultat = as.integer(rezultat)
  ) %>%
  mutate(ekipa = str_replace_all(ekipa, "-", "–")) %>%
  mutate(
    drzava = case_when(
      drzava == "Great Britain" ~ "United Kingdom",
      TRUE ~ drzava 
    ),
    ime_priimek = case_when(
      ime_priimek == "Tadej Pogacar" ~ "Tadej Pogačar",
      ime_priimek == "Bernhard Kohl Carlos Sastre" ~ "Carlos Sastre",
      ime_priimek == "Franco Pellizotti Egoi Martinez" ~ "Egoi Martinez",
      TRUE ~ ime_priimek
    ),
    ekipa =case_when(
      ekipa == "Quick–Step Floors" ~ "Quick-Step Floors",
      ekipa == "Euskaltel–Euskadi" ~ "Euskaltel–Euskadi",
      ekipa == "Quick–Step–Davitamon" ~ "Quick-Step–Davitamon",
      ekipa == "Touriste–routier" ~ "Touriste-routier",
      TRUE ~ ekipa
    )
  )




#MLADI
drzave_tabela <- tibble::tibble(
  drzava =  c("ERI", "SVK", "NOR", "UZB", "URS", "GDR", "IRL", "ITA", "ESP", "FRG", "NED", "FRA", "AUS", "USA", "COL", "MEX", "GER", "RUS", "UKR", "LUX", "GBR", "SLO", "BEL", "SUI"),
  long =  c("Eritrea", "Slovakia", "Norway", "Uzbekistan", "Soviet Union", "East Germany", "Ireland", "Italy","Spain","West Germany","Netherlands","France","Australia","United States","Colombia","Mexico","Germany","Russia","Ukraine","Luxembourg","United Kingdom","Slovenia","Belgium", "Switzerland")
)

mladi <- tabela_mladi %>%
  select(Year, Rider, Team) %>%
  rename(
    leto = Year,
    ime_priimek = Rider,
    ekipa = Team,
  ) %>%
  
  mutate(across(everything(), ~ stri_trans_general(., "Latin-ASCII"))) %>%
  
  mutate(
    drzava = "",
    dirka = "Tour de France",
    id_etapa = NA,
    kategorija = "skupna_mladi",
    rezultat_tip = NA,
    rezultat = NA,
    prednost = NA,
    ime_priimek = gsub("\\)\\s*\\[.*", ")", ime_priimek)
  )  %>%

  separate(
    col = ime_priimek,
    into = c("ime_priimek", "drzava"),
    sep = "\\("
  ) %>%
  mutate(
    drzava = gsub("\\)", "", drzava)
  ) %>%
  
  select(all_of(imena)) %>%
  
  left_join(drzave_tabela, by = "drzava") %>%
  mutate(drzava = long) %>%
  select(-long) %>%
  
  mutate(
    leto = as.integer(leto),
    dirka = factor(dirka, levels = c("Giro d'Italia", "Tour de France", "Vuelta a España")),
    kategorija = factor(kategorija, levels = c("etapna", "skupna", "skupna_hribi", "skupna_mladi", "skupna_tocke")),
    rezultat_tip = factor(rezultat_tip, levels = c("sekunde", "tocke")),
    id_etapa = as.character(id_etapa),
    prednost = as.integer(prednost),
    rezultat = as.integer(rezultat),
    ime_priimek = trimws(ime_priimek),
    ekipa = trimws(ekipa)
  ) %>%
  
  mutate(ekipa = str_replace_all(ekipa, "-", "–")) %>%
  
  mutate(
  ime_priimek = case_when(
    ime_priimek == "Tadej Pogacar" ~ "Tadej Pogačar",
    TRUE ~ ime_priimek
  ),
  ekipa =case_when(
    ekipa == "Soudal–Quick–Step" ~ "Soudal–Quick-Step",
    ekipa == "7–Eleven" ~ "7-Eleven",
    TRUE ~ ekipa
  )
    
)



#TOCKE
vse_drzave <- zmage %>% filter(kategorija == "skupna_tocke") %>% pull(drzava) %>% unique()

tocke <- select(tabela_tocke, 1:2) %>%
  rename(
    leto = Year,
    ime_priimek = Winner
  ) %>% 
  
  mutate(across(everything(), ~ stri_trans_general(., "Latin-ASCII"))) %>%

  mutate(
    drzava = "",
    dirka = "Tour de France",
    id_etapa = NA,
    kategorija = "skupna_tocke",
    rezultat_tip = NA,
    prednost = NA,
    ime_priimek = gsub("\\)\\s*\\[.*", ")", ime_priimek),
    ekipa = NA,
    rezultat = NA

  ) %>%
  separate(
    col = ime_priimek,
    into = c("ime_priimek", "drzava"),
    sep = "\\("
  ) %>%
  mutate(
    drzava = gsub("\\)", "", drzava)
  ) %>%

  select(all_of(imena)) %>%

  left_join(drzave_tabela, by = "drzava") %>%
  mutate(drzava = long) %>%
  select(-long) %>%

  mutate(
    leto = as.integer(leto),
    dirka = factor(dirka, levels = c("Giro d'Italia", "Tour de France", "Vuelta a España")),
    kategorija = factor(kategorija, levels = c("etapna", "skupna", "skupna_hribi", "skupna_mladi", "skupna_tocke")),
    rezultat_tip = factor(rezultat_tip, levels = c("sekunde", "tocke")),
    id_etapa = as.character(id_etapa),
    prednost = as.integer(prednost),
    rezultat = as.integer(rezultat),
    ime_priimek = trimws(ime_priimek),
    ekipa = trimws(ekipa)
  ) %>%

  mutate(ekipa = str_replace_all(ekipa, "-", "–")) %>%

  mutate(
    ime_priimek = case_when(
      ime_priimek == "Tadej Pogacar" ~ "Tadej Pogačar",
      TRUE ~ ime_priimek
    ),
    drzava = if_else(is.na(drzava), "Ireland", drzava)
  )



#VUELTA
library(stringi)

vuelta_general <- tabela_vuelta_general %>%
  select(Year, Country, Cyclist, 'Sponsor/team', Time, Margin) %>%
  
  rename(
    leto = Year,
    ime_priimek = Cyclist,
    drzava = Country,
    ekipa = 'Sponsor/team',
    rezultat = Time,
    prednost = Margin
    
  ) %>% 
  
  mutate(across(everything(), ~ stri_trans_general(., "Latin-ASCII"))) %>%
  
  mutate(
    dirka = "Vuelta a España",
    id_etapa = NA,
    kategorija = "skupna",
    rezultat_tip = "sekunde"
  ) %>%
  select(all_of(imena)) %>%
  
  mutate(
    prednost = sapply(prednost, convert_time_prednost),
    prednost = as.integer(prednost),
    rezultat = sapply(rezultat, convert_time),
    rezultat = as.integer(rezultat),
    
    ekipa = if_else(ekipa == "-", NA, ekipa)
  ) %>%
  
  mutate(
    leto = as.integer(leto),
    dirka = factor(dirka, levels = c("Giro d'Italia", "Tour de France", "Vuelta a España")),
    kategorija = factor(kategorija, levels = c("etapna", "skupna", "skupna_hribi", "skupna_mladi", "skupna_tocke")),
    rezultat_tip = factor(rezultat_tip, levels = c("sekunde", "tocke")),
    id_etapa = as.character(id_etapa),
    prednost = as.integer(prednost),
    rezultat = as.integer(rezultat),
    ime_priimek = trimws(ime_priimek),
    ekipa = trimws(ekipa),
    ime_priimek = sub("[^A-Za-zÀ-ž]+$", "",ime_priimek),
    ekipa = str_replace_all(ekipa, "-", "–")
  )  %>%
  
  filter(!ime_priimek %in% c("~Not contested due to the Spanish Civil War", "~Not contested due to World War II", "~Not contested")) %>%
  
  mutate(
    ime_priimek = case_when(
      ime_priimek == "Tadej Pogacar" ~ "Tadej Pogačar",
      ime_priimek == "Roberto Heras[a" ~ "Roberto Heras",
      ime_priimek == "Primoz Roglic" ~ "Primož Roglič",
      ime_priimek == "Angel Arroyo Marino Lejarreta" ~ "Marino Lejarreta",
      ime_priimek == "Juan Jose Cobo Chris Froome" ~ "Chris Froome",
      TRUE ~ ime_priimek
    ),
    ekipa = case_when(
      ekipa == "Saint–Raphael–Helyett–Hutchinson" ~ "Saint-Raphael–Helyett–Hutchinson",
      ekipa == "Saint–Raphael–Gitane–R. Geminiani" ~ "Saint-Raphael–Gitane–R. Geminiani",
      ekipa == "Quick–Step Alpha Vinyl Team" ~ "Quick-Step Alpha Vinyl Team",
      TRUE ~ ekipa
    ),
    drzava = case_when(
      drzava == "Great Britain" ~ "United Kingdom",
      TRUE ~ drzava 
    )
    
  )
  

#TOUR_SECONDARY
tour_secondary <- tabela_tour_secondary %>%
  select(Year, Points, Youth, Mountains) %>%
  pivot_longer(
    cols = -Year,          # all columns except Year
    names_to = "Category", # optional
    values_to = "Name"     # name column
  ) %>%
  mutate(Name = trimws(Name)) %>%
  filter(!is.na(Name)) %>% # remove empty entries
  filter(!Name == "") %>%
  select(Year, Name) %>%
  mutate(Name = sub(" \\([0-9]+\\)$", "", Name)) %>%
  
  mutate(
    dirka = NA,
    id_etapa = NA,
    kategorija = NA,
    drzava = NA,
    ekipa = NA,
    rezultat = NA,
    prednost = NA,
    rezultat_tip = NA
  ) %>%
  
  rename(
    leto = Year,
    ime_priimek = Name
  )
  




#TOUR_GENERAL
tour_general <- tabela_tour_general %>%
  select(Year, Country, Cyclist, 'Sponsor/Team', 'Time/Points', Margin) %>%
  
  rename(
    leto = Year,
    ime_priimek = Cyclist,
    drzava = Country,
    ekipa = 'Sponsor/Team',
    rezultat = 'Time/Points',
    prednost = Margin
    
  ) %>% 
  
  mutate(across(everything(), ~ stri_trans_general(., "Latin-ASCII"))) %>%
  
  mutate(
    dirka = "Tour de France",
    id_etapa = NA,
    kategorija = "skupna",
    rezultat_tip = "sekunde"
  ) %>%
  select(all_of(imena)) %>%
  
  mutate(
    prednost = sapply(prednost, convert_time_prednost),
    prednost = as.integer(prednost),
    rezultat = sapply(rezultat, convert_time),
    rezultat = as.integer(rezultat),
    
    ekipa = if_else(ekipa == "-", NA, ekipa),
    ime_priimek = trimws(ime_priimek),
    ekipa = trimws(ekipa),
    ime_priimek = sub("[^A-Za-zÀ-ž]+$", "",ime_priimek),
    ekipa = str_replace_all(ekipa, "-", "–")
  ) %>%
  
  mutate(
    leto = as.integer(leto),
    dirka = factor(dirka, levels = c("Giro d'Italia", "Tour de France", "Vuelta a España")),
    kategorija = factor(kategorija, levels = c("etapna", "skupna", "skupna_hribi", "skupna_mladi", "skupna_tocke")),
    rezultat_tip = factor(rezultat_tip, levels = c("sekunde", "tocke")),
    id_etapa = as.character(id_etapa),
    prednost = as.integer(prednost),
    rezultat = as.integer(rezultat),
    rezultat_tip = if_else(rezultat > 100, "sekunde", "točke")
  )  %>%
  filter(!ime_priimek %in% c("~Not contested due to World War I", "No winner[c]", "~Not contested due to World War II", "No winner[c", "")) %>%
  
  mutate(
    drzava = case_when(
      drzava == "Great Britain" ~ "United Kingdom",
      TRUE ~ drzava 
    ),
    ime_priimek = case_when(
      ime_priimek == "Tadej Pogacar" ~ "Tadej Pogačar",
      ime_priimek == "Roberto Heras[a" ~ "Roberto Heras",
      ime_priimek == "Primoz Roglic" ~ "Primož Roglič",
      ime_priimek == "Angel Arroyo Marino Lejarreta" ~ "Marino Lejarreta",
      ime_priimek == "Juan Jose Cobo Chris Froome" ~ "Chris Froome",
      ime_priimek == "Henri Cornet[a" ~ "Henri Cornet",
      ime_priimek == "Bjarne Riis[b" ~ "Bjarne Riis",
      ime_priimek == "Oscar Pereiro[d" ~ "Oscar Pereiro",
      ime_priimek == "Andy Schleck#[e" ~ "Andy Schleck",
      TRUE ~ ime_priimek
    ),
    prednost = case_when(
      is.na(prednost) ~ 0L,
      TRUE ~ prednost
    ),
    
    ekipa = case_when(
      ekipa == "Saint–Raphael–Helyett–Hutchinson" ~ "Saint-Raphael–Helyett–Hutchinson",
      ekipa == "Saint–Raphael–Gitane–R. Geminiani" ~ "Saint-Raphael–Gitane–R. Geminiani",
      ekipa == "Quick–Step Alpha Vinyl Team" ~ "Quick-Step Alpha Vinyl Team",
      ekipa == "Saint–Raphael–Gitane–Dunlop" ~ "Saint-Raphael–Gitane–Dunlop",
      ekipa == "AD Renting–W–Cup–Bottecchia" ~ "AD Renting–W-Cup–Bottecchia",
      TRUE ~ ekipa
    )
  )

#TOUR_EXCEL

imena <- c("leto","dirka","id_etapa","kategorija","ime_priimek","drzava","ekipa","rezultat","prednost","rezultat_tip")

drzave_tabela <- tibble::tibble(
  drzava =  c("ECU", "LTU", "RSA", "KAZ", "EST", "SWE", "CZE", "SVK", "LAT", "POL", "BRA", "CAN", "IRE", "DEN","POR", "AUT", "ERI", "SVK", "NOR", "UZB", "URS", "GDR", "IRL", "ITA", "ESP", "FRG", "NED", "FRA", "AUS", "USA", "COL", "MEX", "GER", "RUS", "UKR", "LUX", "GBR", "SLO", "BEL", "SUI"),
  long =  c("Ecuador", "Lithuania", "South Africa", "Kazakhstan", "Estonia", "Sweden", "Czech Republic", "Slovakia", "Latvia", "Poland", "Brazil", "Canada", "Ireland", "Denmark", "Portugal", "Austria", "Eritrea", "Slovakia", "Norway", "Uzbekistan", "Soviet Union", "East Germany", "Ireland", "Italy","Spain","West Germany","Netherlands","France","Australia","United States","Colombia","Mexico","Germany","Russia","Ukraine","Luxembourg","United Kingdom","Slovenia","Belgium", "Switzerland")
)





tour_main <- read_excel("tdf_etape_rojstva.xlsx", sheet = listi[1]) %>%
  select(year, winner, type, stage) %>%
  
  unite("id_etapa", year, stage, sep = "_", remove = FALSE) %>%
  select(-stage) %>%
  
  rename(
    leto = year,
    ime_priimek = winner,
    kategorija = type
  ) %>% 
  
  filter(!ime_priimek %in% c("Rest day", "no winner", "Cancelled","Transfer")) %>%
  filter(!kategorija %in% c("Team time trial")) %>%
  
  mutate(ime_priimek = sub("\\).*", "", ime_priimek)) %>%
  
  mutate(across(everything(), ~ stri_trans_general(., "Latin-ASCII"))) %>%
  
  separate(
    col = ime_priimek,
    into = c("ime_priimek", "drzava"),
    sep = "\\("
  ) %>%
  
  mutate(
    drzava = gsub("\\)", "", drzava)
  ) %>%
  
  left_join(drzave_tabela, by = "drzava") %>%
  mutate(drzava = long) %>%
  select(-long) %>%
  
  mutate(
    dirka = "Tour de France",
    rezultat_tip = "sekunde",
    ekipa = NA,
    rezultat = NA,
    prednost = NA,
    kategorija = "etapna"
  ) %>%
  
  select(all_of(imena)) %>%
  
  mutate(
    leto = as.integer(leto),
    dirka = factor(dirka, levels = c("Giro d'Italia", "Tour de France", "Vuelta a España")),
    kategorija = factor(kategorija, levels = c("etapna", "skupna", "skupna_hribi", "skupna_mladi", "skupna_tocke")),
    id_etapa = as.character(id_etapa),
    prednost = as.integer(prednost),
    rezultat = as.integer(rezultat),
    rezultat_tip = if_else(rezultat > 100, "sekunde", "točke"),
    ekipa = as.character(ekipa),
    rezultat_tip = factor(rezultat_tip, levels = c("sekunde", "točke"))
  ) %>%
  
  mutate(across(where(is.character), ~ stri_trim_both(.) |> stri_replace_all_regex("\\s+", " "))) %>%
  
  
  
  mutate(
    ime_priimek = case_when(
      ime_priimek == "Primoz Roglic" ~ "Primož Roglič",
      ime_priimek == "Tadej Pogacar" ~ "Tadej Pogačar",
      ime_priimek == "Matej Mohoric" ~ "Matej Mohorič",
      ime_priimek == "Ramunas Navardauskas" ~ "Ramūnas Navardauskas",
      ime_priimek == "Zdenek Stybar" ~ "Zdenek Štybar",
      TRUE ~ ime_priimek
    )
  )
  

















  


#ZDRUZEVANJE

zmage_1 <- bind_rows(tour_general, vuelta_general, tocke, mladi, hribi, giro_general, tour_main) #ne najdem preostalih podatkov, tour_secondary ima podatke ki niso v zmage
napake <- anti_join(zmage, zmage_1, by=names(zmage_1))






#preverjanje enakosti
napake_tour_secondary <- anti_join(tour_secondary, zmage, by=names(tour_secondary))
napake_tour_main <- anti_join(tour_main, zmage, by=names(tour_main))
napake_tour_general <- anti_join(tour_general, zmage, by = names(tour_general))
napaka_vuelta <- anti_join(vuelta_general, zmage, by = names(vuelta))
napake_tocke <- anti_join(tocke, zmage, by = names(tocke))
napake_mladi <- anti_join(mladi, zmage, by = names(mladi))
napake_hribi <- anti_join(hribi, zmage, by = names(hribi))
napake_giro <- anti_join(giro_general, zmage, by = names(giro_general))

zmage$drzava <- unname(zmage$drzava)





################################################################################
#5-INDIKATORJI

naslov_GDP <- "GDP.xlsx"
naslov_populacija <- "Population.xlsx"
naslov_svoboda <- "PressFreedomIndexScore.xlsx"
naslov_veselje <-"WorldHappiness.xlsx"


process_excel <- function(naslov) {
  sheet <- excel_sheets(naslov)[1]             
  df <- read_excel(naslov, sheet = sheet)      
  df
}

podatki_GDP       <- process_excel(naslov_GDP)
podatki_veselje   <- process_excel(naslov_veselje)
podatki_populacija <- process_excel(naslov_populacija)
podatki_svoboda   <- process_excel(naslov_svoboda)

podatki_GDP <- podatki_GDP[-c(1,2), ]
colnames(podatki_GDP) <- as.character(podatki_GDP[1, ])
podatki_GDP <- podatki_GDP[-1, ]


podatki_populacija <- podatki_populacija[-c(1,2), ]
colnames(podatki_populacija) <- as.character(podatki_populacija[1, ])
podatki_populacija <- podatki_populacija[-1, ]

colnames(podatki_svoboda) <- as.character(podatki_svoboda[1,]) 
podatki_svoboda <- podatki_svoboda[-1, ]


org_podatki_GDP <- podatki_GDP %>%
  pivot_longer(
    cols = `1960`:`2024`,
    names_to = "year",
    values_to = "value"
  ) %>%
  select('Country Name', year, value) %>%
  drop_na() %>%
  mutate(ime = "bdp", .after = 2) %>%
  mutate(across(everything(), as.character)) %>%
  rename(
    drzava = 'Country Name',
    leto = year,
    ime = ime,
    vrednost = value
  )




org_podatki_veselje <- podatki_veselje %>%
  select(`Country name`, Year, `Life evaluation (3-year average)`) %>%
  arrange(`Country name`, Year) %>%
  mutate(ime = "indeks_srece", .after = 2) %>%
  drop_na() %>%
  mutate(across(everything(), as.character)) %>%
  rename(
    drzava = 'Country name',
    leto = Year,
    ime = ime,
    vrednost = 'Life evaluation (3-year average)'
  )



org_podatki_populacija <- podatki_populacija %>%
  pivot_longer(
    cols = `1960`:`2024`,
    names_to = "year",
    values_to = "value"
  ) %>%
  select('Country Name', year, value) %>%
  drop_na() %>%
  mutate(ime = "populacija", .after = 2) %>%
  mutate(across(everything(), as.character)) %>%
  rename(
    drzava = 'Country Name',
    leto = year,
    ime = ime,
    vrednost = value
  )


  

org_podatki_svoboda <- podatki_svoboda %>%
  pivot_longer(
    cols = `2002`:`2024`,
    names_to = "year",
    values_to = "value"
  ) %>%
  select('REF_AREA_LABEL', year, value) %>%
  arrange(REF_AREA_LABEL) %>%
  drop_na() %>%
  mutate(ime = "svoboda_medijev", .after = 2) %>%
  mutate(across(everything(), as.character)) %>%
  rename(
    drzava = REF_AREA_LABEL,
    leto = year,
    ime = ime,
    vrednost = value
  )

#sem uporabil dano datoteko; lahko bi rocno vstavil; mi ni uspelo najti vzorca
drzave_ind <- count(indikatorji, drzava, sort = TRUE) %>%
  select(drzava)

drzave_ind1 <- count(indikatorji_1, drzava, sort = TRUE) %>%
  select(drzava)


indikatorji_1 <- bind_rows(org_podatki_GDP, org_podatki_veselje, org_podatki_populacija, org_podatki_svoboda) %>%
  arrange(drzava) %>%
  filter(drzava %in% drzave_ind[[1]])

indikatorji_1$vrednost <- trimws(indikatorji_1$vrednost)
indikatorji_1$vrednost <- as.numeric(indikatorji_1$vrednost)
indikatorji_1$leto <- as.integer(indikatorji_1$leto)
indikatorji_1$ime  <- factor(indikatorji_1$ime, levels = levels(unique(indikatorji_1$ime)))



enakost_check <- indikatorji == indikatorji_1

identical(indikatorji, indikatorji_1)










  


################################################################################
#UGOTOVLJENE NAPAKE
#1 - tibble 'etape' je shranjeval podatke v "tip" stolpcu kot factor namesto character ?
#2 - tibble 'dirke' ima pri zadnjih elementih Gira pri atributu 'razdalja' dodane pike ('.')
#3 - tibble 'dirke' ima dodatno vrednost na koncu ki je ni v spletnem viru
#4 - veliko vrednosti razdalje se v tibblu 'dirke' razlikuje od vrednosti iz spletnega vira
#5 - stolpec 'drzava' v danemu tibblu 'zmage' ima poimenovan vsak element
#6 - v podanemu tibblu zmage je veliko praznih vrednosti ~ imena so NA
 