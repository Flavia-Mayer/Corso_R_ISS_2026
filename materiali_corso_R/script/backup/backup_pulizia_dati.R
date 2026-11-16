### Corso introduttivo a R
### Obiettivo: importare, esplorare e pulire i dati (Moduli 1, 2 e 3)
### Autore: Marco Rossi
### Data di creazione: 10 dicembre 2026
### Data di ultimo aggiornamento: 11 dicembre 2026
### Contatto: marco.rossi@iss.it

# Modulo 1 - Importazione ed esplorazione dei dati ========================

# Caricamento pacchetti ---------------------------------------------------
pacman::p_load(
  rio, # importazione dati
  skimr, # ottieni panoramica dei dati
  janitor, # pulizia dati e tabelle
  lubridate, # lavorare con le date
  epikit, # per creare categorie di età
  gtsummary, # statistiche riassuntive, test e regressioni
  tidyverse # gestione e visualizzazione dati
)


# Importazione dati -------------------------------------------------------
dati_raw <- import("dati/dataset_esercitazione_raw.csv")

# Esplorazione ------------------------------------------------------------
skim(dati_raw)

# Modulo 2 - Pulizia e preparazione dei dati ==============================

# Pulizia dei dati --------------------------------------------------------
dati <- dati_raw %>%
  # pulizia automatica dei nomi delle variabili
  clean_names() %>%
  # pulizia manuale dei nomi delle variabili (NUOVO = VECCHIO)
  rename(
    id_subject  = subject_id,
    chrls_index = charlson_index,
    chrls_score = charlson_score,
    date_vacc   = vaccination_date,
    date_end_fu = end_follow_up,
    date_event  = event_date) %>%
  # eliminazione delle variabili non necessarie
  select(-row_number_original, -internal_note) %>%
  # rimozione delle righe duplicate
  distinct() %>%
  mutate(
    # codici di valore mancante -> NA
    sex        = na_if(sex, "Unknown"),
    age        = na_if(age, "Unknown"),
    age        = na_if(age, "N/D"),
    age_group  = na_if(age_group, "Unknown"),
    age_group  = na_if(age_group, "999999"),
    hiv_status = na_if(hiv_status, "9"),
    hiv_status = na_if(hiv_status, "99999"),
    # celle vuote "" -> NA
    region     = na_if(region, ""),
    age_group  = na_if(age_group, ""),
    hiv_status = na_if(hiv_status, ""),
    date_event = na_if(date_event, "")
  ) %>%
  # conversione dell'età in numero
  mutate(age = as.numeric(age)) %>%
  # conversione delle date in classe Date
  mutate(
    date_event  = dmy(date_event),
    date_end_fu = dmy(date_end_fu),
    date_vacc   = as.Date(parse_date_time(date_vacc, orders = c("Ymd", "dmY")))
  )


# Verifica ----------------------------------------------------------------
nrow(dati_raw) - nrow(dati)
str(dati)
dati %>% tabyl(region)
dati %>% tabyl(sex)
dati %>% tabyl(age_group)
dati %>% tabyl(hiv_status)
range(dati$age, na.rm = TRUE)
range(dati$date_vacc, na.rm = TRUE)
range(dati$date_end_fu, na.rm = TRUE)
range(dati$date_event, na.rm = TRUE)
colSums(is.na(dati))
dati %>% tabyl(chrls_index, chrls_score)


# Modulo 3 - Pulizia dei dati - avanzato  =================================

# Pulizia dei dati --------------------------------------------------------
dati_clean <- dati %>%
  # variabile evento: 1 se c'è una data di evento, 0 altrimenti
  mutate(event = ifelse(is.na(date_event), 0, 1)) %>%
  mutate(
    # ricodifica del sesso
    sex = case_when(
      sex == "m"                          ~ "M",
      sex %in% c("f", "Female", "FEMALE") ~ "F",
      TRUE                                ~ sex),
    # ricodifica della regione
    region = case_when(
      region %in% c("a", "Region A") ~ "A",
      region == "Region D"           ~ "D",
      region == "f"                  ~ "F",
      TRUE                           ~ region),
    # ricodifica dello stato HIV in numero (0 = assenza, 1 = presenza)
    hiv_status = case_when(
      hiv_status %in% c("1", "Yes") ~ 1,
      hiv_status == "0"             ~ 0),
    # stato HIV con le etichette, per tabelle e grafici
    hiv = factor(hiv_status,
                 levels = c(0, 1),
                 labels = c("Senza HIV", "Con HIV")),
    # classe di età: tolti gli spazi ("60 +" diventa "60+")
    age_group = str_remove_all(age_group, " ")
  ) %>%
  # controllo della coerenza della data dell'evento
  mutate(event_check = case_when(
    is.na(date_event)        ~ "nessun evento",
    date_event < date_vacc   ~ "evento prima della vaccinazione",
    date_event > date_end_fu ~ "evento dopo la fine del follow-up",
    TRUE                     ~ "evento durante il follow-up")) %>%
  mutate(
    # classi di età di 10 anni
    age_cat = age_categories(age, breakers = c(18, 30, 40, 50, 60, 70, 80)),
    # classe di età ricalcolata dall'età
    age_group_calc = ifelse(age >= 60, "60+", "18-59"),
    # priorità alla classe calcolata dall'età, altrimenti quella originale
    age_group = coalesce(age_group_calc, age_group)
  ) %>%
  # durata del follow-up in giorni
  mutate(fu_days = as.numeric(date_end_fu - date_vacc)) %>%
  # solo regioni dello studio (esclude E, F e regione mancante) e stato HIV noto
  filter(region %in% c("A", "B", "C", "D") & !is.na(hiv_status)) %>%
  # esclusione dei follow-up di durata negativa
  filter(fu_days >= 0) %>%
  # esclusione degli eventi fuori dal periodo di follow-up
  filter(event_check %in% c("nessun evento", "evento durante il follow-up")) %>%
  # riordino delle variabili (event_check e age_group_calc vengono eliminate)
  select(id_subject, group, region, sex, age, age_group, age_cat,
         chrls_score, chrls_index, hiv_status, hiv, starts_with("date_"),
         fu_days, event, anticorpi, cd4)


# Verifica ----------------------------------------------------------------
nrow(dati) - nrow(dati_clean)
names(dati_clean)
str(dati_clean)
dati_clean %>% tabyl(region)
dati_clean %>% tabyl(sex)
dati_clean %>% tabyl(age_group)
dati_clean %>% tabyl(age_cat)
dati_clean %>% tabyl(hiv_status)
dati_clean %>% tabyl(hiv)
dati_clean %>% tabyl(event)
range(dati_clean$fu_days)
colSums(is.na(dati_clean))


# Salvataggio dei dati puliti ---------------------------------------------
# al posto di NOME scrivi il tuo nome, per esempio dati_clean_marco.rds
export(dati_clean, "dati/dati_clean_NOME.rds")
