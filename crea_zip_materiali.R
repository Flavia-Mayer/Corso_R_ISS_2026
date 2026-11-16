# ================================================================
# CREA IL FILE materiali_corso_R.zip DA SCARICARE DAL SITO
# ================================================================
#
# Viene eseguito in automatico da Quarto prima di ogni render del sito
# (vedi "pre-render" in _quarto.yml): non serve lanciarlo a mano.
#
# Nello zip mette solo quello che serve agli studenti:
#   - dati/            tutti i file
#   - script/backup/   gli script corretti
#   - output/          vuota
# e lascia fuori i file di lavoro di RStudio (.Rproj.user, .Rhistory,
# .Rproj) e quello che c'e' in output/, cosi' gli studenti partono da
# una cartella pulita.
# ================================================================

cartella <- "materiali_corso_R"
file_zip <- file.path(getwd(), "materiali_corso_R.zip")

# 1. copia dei file da includere in una cartella temporanea
appoggio <- file.path(tempfile("zip_materiali"), cartella)
for (sotto in c("dati", file.path("script", "backup"))) {
  dir.create(file.path(appoggio, sotto), recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files(file.path(cartella, sotto), full.names = TRUE),
            file.path(appoggio, sotto), recursive = TRUE)
}
dir.create(file.path(appoggio, "output"), showWarnings = FALSE)

# 2. creazione dello zip, con la cartella materiali_corso_R al suo interno
if (file.exists(file_zip)) file.remove(file_zip)
zip::zip(file_zip, files = cartella, root = dirname(appoggio), mode = "mirror")
unlink(dirname(appoggio), recursive = TRUE)

message("Creato materiali_corso_R.zip")
