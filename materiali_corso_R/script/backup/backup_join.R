### Corso introduttivo a R
### Obiettivo: trasformazione e integrazione dei dati (Modulo 7)
### Autore: Marco Rossi
### Data di creazione: 3 dicembre 2026
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
regioni    <- import("dati/regioni.rds")
richiami   <- import("dati/richiami.rds")


# Join con le regioni -----------------------------------------------------
dati_regioni <- dati_clean %>%
  left_join(regioni, by = join_by(region == codice_regione))


# Richiami: una riga per persona ------------------------------------------
richiami_largo <- richiami %>%
  pivot_wider(names_from  = dose,
              values_from = data_dose)

dati_richiami <- dati_clean %>%
  left_join(richiami_largo, by = "id_subject", relationship = "one-to-one")


# Copertura dei richiami per regione e stato HIV --------------------------
richiami_regione_hiv <- dati_richiami %>%
  group_by(region, hiv_status) %>%
  summarise(perc_richiamo = round(mean(!is.na(richiamo_1)) * 100, 1),
            .groups = "drop") %>%
  pivot_wider(names_from   = hiv_status,
              names_prefix = "hiv_",
              values_from  = perc_richiamo) %>%
  left_join(regioni, by = join_by(region == codice_regione))

export(richiami_regione_hiv, "output/richiami_regione_hiv.rds")
export(richiami_regione_hiv, "output/richiami_regione_hiv.xlsx")
