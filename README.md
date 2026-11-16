# Corso base di R per la gestione e analisi dei dati

Materiale del corso di R dell'Istituto Superiore di Sanità
(Dipartimento Malattie Infettive), 30 novembre – 4 dicembre 2026.

Il sito del corso è generato con [Quarto](https://quarto.org) a partire
da questo repository e pubblicato con GitHub Pages.

## Struttura

| File o cartella | Contenuto |
|----|----|
| `_quarto.yml` | configurazione del sito: pagine, menu, stile |
| `index.qmd` | home page |
| `preparazione.qmd` | istruzioni per prepararsi al corso |
| `dati.qmd` | descrizione dei dati del corso |
| `licenza.qmd` | copyright e licenza |
| `modulo1_*.qmd` … `modulo9_*.qmd` | le esercitazioni, una per modulo |
| `materiali_corso_R/` | la cartella per gli studenti: `dati/` e `script/backup/` |
| `crea_zip_materiali.R` | crea `materiali_corso_R.zip`, scaricabile dal sito (parte da solo prima di ogni render) |
| `iss.scss`, `sito.css` | colori e stile del sito |
| `immagini/`, `webex.*` | immagini e file per i quiz interattivi |

## Aggiornare il sito

1. Modificare i file `.qmd` o i file in `materiali_corso_R/`.
2. Generare il sito: in RStudio **Build → Render Website**, oppure
   `quarto render` dal terminale.
3. Salvare le modifiche su GitHub: commit e push.
4. Pubblicare il sito: `quarto publish gh-pages`.

## Licenza

Vedi la pagina [Copyright e licenza](licenza.qmd).
