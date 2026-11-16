### Corso introduttivo a R
### Obiettivo: tabelle e aggregazione dei dati (Modulo 4)
### Autore: Marco Rossi
### Data di creazione: 1 dicembre 2026
### Contatto: marco.rossi@iss.it

# Caricamento pacchetti ---------------------------------------------------
pacman::p_load(
  rio,       # importazione ed esportazione dei dati
  skimr,     # panoramica dei dati
  janitor,   # pulizia dei dati e tabelle
  lubridate, # lavorare con le date
  epikit,    # classi di età
  gtsummary, # tabelle descrittive
  flextable, # tabelle da presentare
  tidyverse  # gestione e visualizzazione dei dati
)


# Importazione dati -------------------------------------------------------
dati_clean <- import("dati/dati_clean.rds")


# Modulo 4 - Tabelle e aggregazione dati ==================================

# Tabelle di frequenza ----------------------------------------------------
tab_sesso <- dati_clean %>%
  count(sex) %>%
  mutate(perc = round(n / sum(n) * 100, 1))

tab_regione <- dati_clean %>%
  tabyl(region) %>%
  arrange(desc(n)) %>%
  adorn_totals() %>%
  adorn_pct_formatting(digits = 1)


# Tabelle a doppia entrata ------------------------------------------------
tab_hiv_eta <- dati_clean %>%
  tabyl(hiv_status, age_group) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()


# Statistiche per gruppo --------------------------------------------------
tassi_eta <- dati_clean %>%
  group_by(age_cat) %>%
  summarise(
    n            = n(),
    eventi       = sum(event),
    anni_persona = sum(fu_days) / 365.25,
    tasso_100    = eventi / anni_persona * 100) %>%
  mutate(tasso_100 = round(tasso_100, 2))

quota_hiv_eta <- dati_clean %>%
  group_by(hiv_status, age_group) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(perc = n / sum(n) * 100)


# Sintesi per stato HIV ---------------------------------------------------
sintesi_hiv <- dati_clean %>%
  group_by(hiv_status) %>%
  summarise(
    n                  = n(),
    perc_donne         = mean(sex == "F", na.rm = TRUE) * 100,
    eta_mediana        = median(age, na.rm = TRUE),
    perc_60            = mean(age >= 60, na.rm = TRUE) * 100,
    perc_charlson_3piu = mean(chrls_index == "3-4" |
                              chrls_index == "5+") * 100,
    anticorpi_mediana  = median(anticorpi),
    eventi             = sum(event),
    anni_persona       = sum(fu_days) / 365.25,
    tasso_100          = eventi / anni_persona * 100,
    .groups = "drop") %>%
  mutate(
    perc_donne         = round(perc_donne, 1),
    perc_60            = round(perc_60, 1),
    perc_charlson_3piu = round(perc_charlson_3piu, 1),
    anni_persona       = round(anni_persona, 1),
    tasso_100          = round(tasso_100, 1))

tab_sintesi_hiv <- sintesi_hiv %>%
  flextable() %>%
  set_header_labels(
    hiv_status         = "Stato HIV",
    n                  = "Persone",
    perc_donne         = "% donne",
    eta_mediana        = "Età mediana",
    perc_60            = "% 60+",
    perc_charlson_3piu = "% Charlson 3+",
    anticorpi_mediana  = "Anticorpi mediani (BAU/mL)",
    eventi             = "Eventi",
    anni_persona       = "Anni-persona",
    tasso_100          = "Tasso per 100 AP") %>%
  autofit()


# Tassi per classe d'età e stato HIV --------------------------------------
tassi_eta_hiv <- dati_clean %>%
  filter(!is.na(age_cat)) %>%
  group_by(age_cat, hiv_status) %>%
  summarise(
    n            = n(),
    eventi       = sum(event),
    anni_persona = sum(fu_days) / 365.25,
    tasso_100    = eventi / anni_persona * 100,
    .groups = "drop") %>%
  mutate(
    anni_persona = round(anni_persona, 1),
    tasso_100    = round(tasso_100, 1))

export(tassi_eta_hiv, "output/tassi_eta_hiv.rds")
export(tassi_eta_hiv, "output/tassi_eta_hiv.xlsx")
