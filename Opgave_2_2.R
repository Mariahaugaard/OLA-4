library(RMariaDB)
library(DBI)

# Opret forbindelse til cars
wc <- dbConnect(MariaDB(),
                host = "localhost",
                user = "root",
                password = "maria2003",
                db = "cars",
                port = "3306"
)

# Vis tabellerne i databasen
query <- "show tables"

tabeller <- dbGetQuery(wc, query)

print(tabeller)

# Indlæs CSV-filen
bilerDf <- read.csv(
  file.choose(),
  colClasses = "character",
  fileEncoding = "UTF-8-BOM"
)

# Vis antal rækker
nrow(bilerDf)

# Vis de første rækker
head(bilerDf)

# Indlæs CSV-filen
bilerDf <- read.csv(
  file.choose(),
  colClasses = "character",
  fileEncoding = "UTF-8-BOM"
)

# Vis antal rækker
nrow(bilerDf)

# Vis de første rækker
head(bilerDf)

# Omdan registreringsmåned til en dato
reg_tekst <- paste0("01/", bilerDf$reg_dato)

bilerDf$reg_dato <- as.Date(
  reg_tekst,
  format = "%d/%m/%Y"
)

# Omdan scrapedato til dato og tidspunkt
bilerDf$scrapedate <- as.POSIXct(
  bilerDf$scrapedate,
  format = "%Y-%m-%d",
  tz = "UTC"
)

# Omdan prisen til tal
bilerDf$pris_dkk <- as.numeric(bilerDf$pris_dkk)

# Vis resultatet
head(bilerDf[c("reg_dato", "scrapedate", "pris_dkk")])

# Vælg CVR og forhandlernavn
forhandlereDf <- bilerDf[c("cvr", "forhandler")]

# Behold én række pr. CVR
forhandlereDf <- forhandlereDf[
  !duplicated(forhandlereDf$cvr),
]

# Kontrollér antal forhandlere
nrow(forhandlereDf)

# Vis de første rækker
head(forhandlereDf)

# Gem forhandlerne i den eksisterende tabel
dbWriteTable(
  wc,
  name = "forhandlere",
  value = forhandlereDf,
  append = TRUE,
  row.names = FALSE
)

# Kontrollér antallet i databasen
query <- "select count(*) as antal from forhandlere"

antalForhandlere <- dbGetQuery(wc, query)

print(antalForhandlere)

# Vælg kolonnerne til biltabellen
bilerTilSQL <- bilerDf[c(
  "carid",
  "maerke",
  "model",
  "reg_dato",
  "adresse",
  "link",
  "cvr"
)]

# Omdøb cvr, så navnet passer til SQL-tabellen
names(bilerTilSQL)[7] <- "forhandler_id"

# Gem bilerne i databasen – kør indlæsningen én gang
dbWriteTable(
  wc,
  name = "biler",
  value = bilerTilSQL,
  append = TRUE,
  row.names = FALSE
)

# Kontrollér antallet i databasen
query <- "select count(*) as antal from biler"

antalBiler <- dbGetQuery(wc, query)

print(antalBiler)

# Vælg kolonnerne til prishistorikken
prishistorikDf <- bilerDf[c(
  "carid",
  "scrapedate",
  "pris_dkk"
)]

# Gem priserne i databasen – kør indlæsningen én gang
dbWriteTable(
  wc,
  name = "prishistorik",
  value = prishistorikDf,
  append = TRUE,
  row.names = FALSE
)

# Kontrollér antallet af prisobservationer
query <- "select count(*) as antal from prishistorik"

antalPriser <- dbGetQuery(wc, query)

print(antalPriser)

# Hent biler med forhandlernavn, dato og pris
query <- "
select b.carid, b.maerke, b.model,
       f.forhandler, p.scrapedate, p.pris_dkk
from biler b
join forhandlere f on b.forhandler_id = f.cvr
join prishistorik p on b.carid = p.carid
"

kontrolDf <- dbGetQuery(wc, query)

# Kontrollér antal rækker
nrow(kontrolDf)

# Vis de første rækker
head(kontrolDf)
