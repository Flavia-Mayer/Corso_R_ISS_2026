### Corso introduttivo a R
### Obiettivo: grafici con ggplot2 (Moduli 5, 6 e 8)
### Autore: Marco Rossi
### Data di creazione: 2 dicembre 2026
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


# Modulo 05 - Data Visualization con ggplot2 (Parte 1) =====================

# Età per regione ----------------------------------------------------------
ggplot(
  data = dati_clean,                        # coorte di vaccinati
  mapping = aes(
    x = region,                             # regione sull'asse x
    y = age,                                # età sull'asse y
    fill = region)) +                       # riempimento per regione
  geom_boxplot() +                          # un boxplot per regione
  labs(
    title = "Età alla vaccinazione per regione",
    subtitle = "Persone con e senza HIV",
    x = "Regione",
    y = "Età (anni)",
    caption = "Grafico modulo 5",
    fill = "Regione")

# Vaccinati per giorno -----------------------------------------------------
vaccinati_giorno <- dati_clean %>%
  group_by(date_vacc) %>%
  summarise(n_vaccinati = n(),
            eta_media = mean(age, na.rm = TRUE),
            .groups = "drop")

# Vaccinati per regione e classe d'età -------------------------------------
ggplot(data = dati_clean, mapping = aes(x = region, fill = age_group)) +
  geom_bar(position = "dodge")

# Dati aggregati -----------------------------------------------------------
vaccinati_regione <- dati_clean %>%
  group_by(region) %>%
  summarise(n_vaccinati = n(), .groups = "drop")

ggplot(data = vaccinati_regione, mapping = aes(x = region, y = n_vaccinati)) +
  geom_col()

# Istogramma ---------------------------------------------------------------
ggplot(data = dati_clean, mapping = aes(x = date_vacc)) +
  geom_histogram(binwidth = 7, fill = "darkgreen") +
  scale_x_date(
    date_breaks = "1 month",
    labels = scales::label_date("%b %Y", locale = "it"))

# Riquadri facet -----------------------------------------------------------
ggplot(data = dati_clean, mapping = aes(x = date_vacc)) +
  geom_histogram(binwidth = 7) +
  facet_wrap(~ hiv)

# Salvare il grafico -------------------------------------------------------
scatter_vaccinati <- ggplot(data = vaccinati_giorno, mapping = aes(x = date_vacc, y = n_vaccinati)) +
  geom_point(mapping = aes(color = eta_media), alpha = 0.5) +
  geom_smooth(alpha = 0.2) +
  scale_color_viridis_c() +
  labs(
    title = "Vaccinati per giorno nel corso della campagna vaccinale",
    x = "Data di vaccinazione",
    y = "Numero di vaccinati",
    color = "Età media (anni)")

scatter_vaccinati

ggsave(
  plot = scatter_vaccinati,
  filename = "output/scatter_vaccinati_giorno.png",
  width = 10,
  height = 6,
  dpi = 300)


# Modulo 06 - Data Visualization con ggplot2 (Parte 2) =====================

# Colori scelti a mano -----------------------------------------------------
ggplot(data = dati_clean, mapping = aes(x = region, fill = hiv, color = hiv)) +
  geom_bar(alpha = 0.5) +
  geom_point(stat = "count", position = "stack", size = 3) +
  scale_fill_manual(
    name = "Stato HIV",                             # titolo della legenda
    values = c(                                     # colori
      "Senza HIV" = "#2a9d8f",
      "Con HIV"   = "#e76f51"),
    breaks = c("Con HIV", "Senza HIV"),             # livelli e ordine in legenda
    labels = c(                                     # testo in legenda
      "Con HIV"   = "Persone con HIV",
      "Senza HIV" = "Controlli"),
    aesthetics = c("fill", "color"))                # estetiche coinvolte

# Palette predefinite ------------------------------------------------------
ggplot(data = dati_clean, mapping = aes(x = region, fill = chrls_index)) +
  geom_bar() +
  scale_fill_viridis_d()

# Percentuali --------------------------------------------------------------
ggplot(data = dati_clean, mapping = aes(x = region, fill = age_group)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    caption =
      paste0("n = ", nrow(dati_clean), " vaccinati", "\n",
             sum(is.na(dati_clean$region)), " persone senza regione", "\n",
             "Grafico prodotto il ", Sys.Date()))

# Modificare il tema -------------------------------------------------------
ggplot(data = dati_clean, mapping = aes(x = region, fill = age_group)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_minimal() +
  theme(
    legend.position = "bottom",                     # legenda in basso
    axis.title = element_text(face = "bold"),       # nomi assi in grassetto
    panel.grid.minor = element_blank())             # niente griglia secondaria


# Modulo 08 - Altri grafici ================================================

# Importazione dati -------------------------------------------------------
dati_clean <- import("dati/dati_clean.rds")

# Pacchetti nuovi ---------------------------------------------------------
pacman::p_load(
  sf,          # dati spaziali e mappe
  apyramid     # piramidi per età e sesso
)


# Settimane e mesi --------------------------------------------------------
settimane <- seq.Date(
  from = floor_date(min(dati_clean$date_vacc), unit = "week", week_start = 1),
  to   = ceiling_date(max(dati_clean$date_vacc), unit = "week", week_start = 1),
  by   = "week")


# Curva delle vaccinazioni ------------------------------------------------
# settimane da lunedì a domenica
ggplot(data = dati_clean, mapping = aes(x = date_vacc)) +
  geom_histogram(breaks = settimane, closed = "left") +
  scale_x_date(
    date_breaks = "1 month",
    labels = scales::label_date("%b\n%Y", locale = "it")) +
  labs(x = "Settimana di vaccinazione", y = "Vaccinati")

# curva mensile per stato HIV
mesi <- seq.Date(
  from = floor_date(min(dati_clean$date_vacc), unit = "month"),
  to   = ceiling_date(max(dati_clean$date_vacc), unit = "month"),
  by   = "month")

ggplot(data = dati_clean, mapping = aes(x = date_vacc, fill = hiv)) +
  geom_histogram(breaks = mesi, closed = "left") +
  scale_x_date(
    date_breaks = "1 month",
    labels = scales::label_date("%b\n%Y", locale = "it")) +
  labs(x = "Mese di vaccinazione", y = "Vaccinati", fill = "Stato HIV")


# Numeri di settimana -----------------------------------------------------
vacc_settimana <- dati_clean %>%
  mutate(settimana = floor_date(date_vacc, unit = "week", week_start = 1)) %>%
  count(settimana) %>%
  complete(settimana = seq.Date(from = min(settimana), to = max(settimana), by = "week"),
           fill = list(n = 0))

ggplot(data = vacc_settimana, mapping = aes(x = settimana, y = n)) +
  geom_col(width = 7) +
  scale_x_date(
    breaks = seq.Date(from = min(settimane), to = max(settimane), by = "4 weeks"),
    labels = scales::label_date("%G-W%V")) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Settimana di vaccinazione", y = "Vaccinati")


# Heatmap dei conteggi ----------------------------------------------------
vacc_regione_settimana <- dati_clean %>%
  mutate(settimana = floor_date(date_vacc, unit = "week", week_start = 1)) %>%
  count(region, settimana) %>%
  complete(region, settimana, fill = list(n = 0))

ggplot(data = vacc_regione_settimana, mapping = aes(x = settimana, y = region, fill = n)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkred") +
  scale_x_date(
    date_breaks = "1 month",
    labels = scales::label_date("%b\n%Y", locale = "it")) +
  labs(x = "Settimana di vaccinazione", y = "Regione", fill = "Vaccinati")


# Heatmap di proporzioni --------------------------------------------------
eta_settimana <- dati_clean %>%
  filter(!is.na(hiv), !is.na(age_cat)) %>%
  mutate(settimana = floor_date(date_vacc, unit = "week", week_start = 1)) %>%
  count(hiv, settimana, age_cat) %>%
  group_by(hiv, settimana) %>%
  mutate(quota = n / sum(n)) %>%
  ungroup()

ggplot(data = eta_settimana, mapping = aes(x = settimana, y = age_cat, fill = quota)) +
  geom_tile() +
  facet_wrap(~ hiv, ncol = 1) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1,
                       labels = scales::label_percent()) +
  scale_x_date(
    date_breaks = "1 month",
    labels = scales::label_date("%b\n%Y", locale = "it")) +
  labs(x = "Settimana di vaccinazione", y = "Classe d'età",
       fill = "Quota della\nsettimana")


# Heatmap con etichette ---------------------------------------------------
charlson_eta <- dati_clean %>%
  filter(!is.na(hiv), !is.na(age_cat), !is.na(chrls_index)) %>%
  group_by(age_cat, hiv) %>%
  summarise(
    n = n(),
    quota_ch3 = mean(chrls_index %in% c("3-4", "5+")),
    .groups = "drop")

ggplot(data = charlson_eta, mapping = aes(x = hiv, y = age_cat, fill = quota_ch3)) +
  geom_tile() +
  geom_text(mapping = aes(label = paste0(scales::percent(quota_ch3, accuracy = 1),
                                         "\n(n = ", n, ")"))) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1,
                       labels = scales::label_percent()) +
  labs(x = NULL, y = "Classe d'età", fill = "Charlson 3+")


# Leggere una mappa -------------------------------------------------------
regioni_sf <- read_sf("dati/shapefiles/regioni.shp")


# Mappa con sf ------------------------------------------------------------
vacc_regione <- dati_clean %>%
  count(region, name = "n_vaccinati")

mappa_vacc <- regioni_sf %>%
  left_join(vacc_regione, by = "region") %>%
  mutate(tasso = n_vaccinati / pop * 100000)

ggplot(data = mappa_vacc) +
  geom_sf(mapping = aes(fill = tasso)) +
  geom_sf_text(mapping = aes(label = region)) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  theme_void() +
  labs(title = "Vaccinati della coorte per regione",
       fill = "Vaccinati per\n100.000 abitanti")


# Mappe per gruppo e nel tempo --------------------------------------------
# tasso per stato HIV
vacc_regione_hiv <- dati_clean %>%
  filter(!is.na(hiv)) %>%
  count(region, hiv, name = "n_vaccinati")

mappa_hiv <- regioni_sf %>%
  left_join(vacc_regione_hiv, by = "region") %>%
  mutate(tasso = n_vaccinati / pop * 100000)

ggplot(data = mappa_hiv) +
  geom_sf(mapping = aes(fill = tasso)) +
  facet_wrap(~ hiv) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  theme_void() +
  labs(fill = "Vaccinati per\n100.000 abitanti")

# quota di persone con HIV per regione
quota_hiv <- dati_clean %>%
  filter(!is.na(hiv)) %>%
  group_by(region) %>%
  summarise(quota_hiv = mean(hiv == "Con HIV"))

regioni_sf %>%
  left_join(quota_hiv, by = "region") %>%
  ggplot() +
  geom_sf(mapping = aes(fill = quota_hiv)) +
  geom_sf_text(mapping = aes(label = scales::percent(quota_hiv, accuracy = 1))) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1,
                       labels = scales::label_percent()) +
  theme_void() +
  labs(fill = "Quota con HIV")

# vaccinati per mese
vacc_mese <- dati_clean %>%
  mutate(mese = floor_date(date_vacc, unit = "month")) %>%
  count(region, mese) %>%
  complete(region, mese, fill = list(n = 0))

regioni_sf %>%
  left_join(vacc_mese, by = "region") %>%
  ggplot() +
  geom_sf(mapping = aes(fill = n)) +
  geom_sf_text(mapping = aes(label = region)) +
  facet_wrap(~ mese, nrow = 1) +
  scale_fill_viridis_c() +
  theme_void()


# Piramidi ----------------------------------------------------------------
# sesso come factor: i maschi sempre a sinistra
dati_clean <- dati_clean %>%
  mutate(sex = factor(sex, levels = c("M", "F")))

age_pyramid(data = filter(dati_clean, hiv == "Con HIV"),
            age_group = "age_cat", split_by = "sex",
            proportional = TRUE)

age_pyramid(data = filter(dati_clean, hiv == "Senza HIV"),
            age_group = "age_cat", split_by = "sex",
            proportional = TRUE)

# piramide da dati aggregati
piramide_agg <- dati_clean %>%
  filter(!is.na(age_cat), !is.na(sex)) %>%
  count(age_cat, sex)

age_pyramid(data = piramide_agg, age_group = "age_cat", split_by = "sex",
            count = "n")

# piramide personalizzata con ggplot2
age_pyramid(data = dati_clean, age_group = "age_cat", split_by = "sex",
            proportional = TRUE) +
  theme_minimal() +
  scale_fill_manual(values = c("F" = "#2a9d8f", "M" = "#e76f51")) +
  labs(x = "Classe d'età", y = "Proporzione", fill = "Sesso")
