# ------------------------------------------------------------
# TRIN 1 - INDLÆS DEN SIMULEREDE KØRSEL
# ------------------------------------------------------------

# Indlæs filen, hvor hver række er gemt som ét tekstfelt
filDf <- read.csv(
  file.choose(),
  header = FALSE,
  colClasses = "character",
  fileEncoding = "UTF-8-BOM"
)

# Del tekstfelterne op i de oprindelige kolonner
koersel2 <- read.csv(
  text = paste(filDf[[1]], collapse = "\n"),
  colClasses = "character",
  na.strings = c("", "NA")
)

# Brug de priser, vi har ændret i den nye fil
koersel2$pris_dkk <- as.numeric(koersel2$pris)

# Omdan registreringsmåned til en dato
koersel2$reg_dato <- as.Date(
  paste0("01/", koersel2$reg_dato),
  format = "%d/%m/%Y"
)

# Omdan scrapedato til dato og tidspunkt
koersel2$scrapedate <- as.POSIXct(
  koersel2$scrapedate,
  format = "%Y-%m-%d",
  tz = "UTC"
)

# Kontrollér antal biler og dubletter
print(c(
  biler = nrow(koersel2),
  dubletter = sum(duplicated(koersel2$carid))
))

# Kontrollér datoen
print(unique(koersel2$scrapedate))

# ------------------------------------------------------------
# TRIN 2 - HENT DEN FØRSTE KØRSEL FRA DATABASEN
# ------------------------------------------------------------

query <- "
select b.carid, p.pris_dkk, p.scrapedate
from biler b
join prishistorik p on b.carid = p.carid
where p.scrapedate = (
  select max(scrapedate)
  from prishistorik
  where scrapedate < '2026-11-16'
)
"

koersel1 <- dbGetQuery(wc, query)

# Kontrollér antal biler i den første kørsel
nrow(koersel1)


# ------------------------------------------------------------
# TRIN 3 - FIND DE NYE BILER
# ------------------------------------------------------------

nyeBiler <- koersel2[
  !(koersel2$carid %in% koersel1$carid),
]

print(nyeBiler[c("carid", "model", "pris_dkk")])


# ------------------------------------------------------------
# TRIN 4 - FIND BILER MED ÆNDRET PRIS
# ------------------------------------------------------------

# Saml priserne for de biler, der findes i begge kørsler
sammenligning <- merge(
  koersel1[c("carid", "pris_dkk")],
  koersel2[c("carid", "pris_dkk")],
  by = "carid",
  suffixes = c("_foer", "_nu")
)

# Behold kun biler, hvor prisen er ændret
aendredePriser <- sammenligning[
  sammenligning$pris_dkk_foer != sammenligning$pris_dkk_nu,
]

print(aendredePriser)


# ------------------------------------------------------------
# TRIN 5 - FIND DE MANGLENDE BILER
# ------------------------------------------------------------

# Biler fra første kørsel, som mangler i anden kørsel
solgteBiler <- koersel1[
  !(koersel1$carid %in% koersel2$carid),
]

print(solgteBiler)


# ------------------------------------------------------------
# TRIN 6 - KONTROLLÉR ANTAL ÆNDRINGER
# ------------------------------------------------------------

print(c(
  nye = nrow(nyeBiler),
  aendrede_priser = nrow(aendredePriser),
  solgte = nrow(solgteBiler)
))

# ------------------------------------------------------------
# TRIN 7 - SØRG FOR, AT KOLONNEN SOLD FINDES
# ------------------------------------------------------------

# Tilføj kun kolonnen, hvis den ikke allerede findes
if (!("sold" %in% dbListFields(wc, "biler"))) {
  dbExecute(
    wc,
    "alter table biler
     add column sold boolean not null default false"
  )
}


# ------------------------------------------------------------
# TRIN 8 - KLARGØR NYE BILER OG PRISOBSERVATIONER
# ------------------------------------------------------------

# Vælg kolonnerne til tabellen biler
nyeBilerTilSQL <- nyeBiler[c(
  "carid",
  "maerke",
  "model",
  "reg_dato",
  "adresse",
  "link",
  "cvr"
)]

# Tilpas navnet til databasen
names(nyeBilerTilSQL)[7] <- "forhandler_id"

# Gem priser for de nye biler og dem med ændret pris
prisId <- c(
  nyeBiler$carid,
  aendredePriser$carid
)

nyePriser <- koersel2[
  koersel2$carid %in% prisId,
  c("carid", "scrapedate", "pris_dkk")
]


# ------------------------------------------------------------
# TRIN 9 - GEM ÆNDRINGERNE I DATABASEN
# ------------------------------------------------------------

# Gem ændringerne samlet.
# Hvis noget fejler, bliver ændringerne i denne blok rullet tilbage.
dbWithTransaction(wc, {
  
  # Indsæt de to nye biler
  dbWriteTable(
    wc,
    name = "biler",
    value = nyeBilerTilSQL,
    append = TRUE,
    row.names = FALSE
  )
  
  # Tilføj fem prisobservationer uden at overskrive de gamle
  dbWriteTable(
    wc,
    name = "prishistorik",
    value = nyePriser,
    append = TRUE,
    row.names = FALSE
  )
  
  # Markér de fem manglende biler som solgte
  for (id in solgteBiler$carid) {
    dbExecute(
      wc,
      "update biler set sold = true where carid = ?",
      params = list(id)
    )
  }
})

# ------------------------------------------------------------
# TRIN 10 - KONTROLLÉR RESULTATET
# ------------------------------------------------------------

query <- "
select
  count(*) as biler_i_alt,
  sum(sold = false) as ikke_solgte,
  sum(sold = true) as solgte
from biler
"

print(dbGetQuery(wc, query))

query <- "
select count(*) as prisobservationer
from prishistorik
"

print(dbGetQuery(wc, query))
