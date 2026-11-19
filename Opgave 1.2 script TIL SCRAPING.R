
# ============================================================
# OLA 4 - OPGAVE 1.2
# TESLA FRA AUTOSCOUT24 - TYSKLAND
# ============================================================


# ------------------------------------------------------------
# TRIN 1 - INDLÆS PAKKER
# ------------------------------------------------------------

library(chromote)
library(rvest)
library(jsonlite)


# ------------------------------------------------------------
# TRIN 2 - INDSTILLINGER
# ------------------------------------------------------------

# Tesla fra Tyskland
baseurl <- paste0(
  "https://www.autoscout24.de/lst/tesla",
  "?cy=D&atype=C&ustate=N%2CU&sort=standard"
)

# Antal sider
antal_sider <- 46

# Filer til automatisk gemning
fil_biler <- "OLA4_1_2_Tesla_Tyskland_RETTET.rds"
fil_sider <- "OLA4_1_2_faerdige_sider_RETTET.rds"

# Indlæs tidligere data, hvis de findes
if (file.exists(fil_biler)) {
  tyske_biler <- readRDS(fil_biler)
} else {
  tyske_biler <- data.frame()
}

# Indlæs gennemførte sider
if (file.exists(fil_biler) && file.exists(fil_sider)) {
  faerdige_sider <- readRDS(fil_sider)
} else {
  faerdige_sider <- integer(0)
}

# Start browser
b <- ChromoteSession$new()


# ------------------------------------------------------------
# TRIN 3 - GENNEMGÅ RESULTATSIDERNE
# ------------------------------------------------------------

for (i in seq_len(antal_sider)) {
  
  # Spring allerede gemte sider over
  if (i %in% faerdige_sider) {
    cat("Side", i, "er allerede gemt\n")
    next
  }
  
  sidens_biler <- tryCatch({
    
    # 3.1 - Lav sidens URL
    url <- paste0(baseurl, "&page=", i)
    
    # 3.2 - Åbn hjemmesiden
    b$Page$navigate(url = url)
    
    Sys.sleep(5)
    
    # 3.3 - Hent HTML-koden
    html <- b$Runtime$evaluate(
      "document.documentElement.outerHTML",
      returnByValue = TRUE
    )$result$value
    
    side <- read_html(html)
    
    
    # --------------------------------------------------------
    # TRIN 4 - FIND BILANNONCERNE
    # --------------------------------------------------------
    
    # Find JSON-data
    json_html <- html_element(
      side, "script#__NEXT_DATA__"
    )
    
    # Kontrollér om JSON findes
    if (inherits(json_html, "xml_missing")) {
      stop("JSON-data blev ikke fundet")
    }
    
    # Lav JSON om til en R-liste
    json_tekst <- html_text(json_html)
    
    data <- fromJSON(
      json_tekst,
      simplifyVector = FALSE
    )
    
    # Find bilannoncerne
    annoncer <- data$props$pageProps$listings
    
    if (is.null(annoncer) || length(annoncer) == 0) {
      stop("Ingen annoncer fundet")
    }
    
    # Find annoncekortene i HTML
    kort <- html_elements(
      side,
      "[class*='DeclutteredListItemDefaultView_container__']"
    )
    
    # Kontrollér antal annoncekort
    kort_passer <- length(kort) == length(annoncer)
    
    # Tom dataframe til siden
    resultat <- data.frame()
    
    
    # --------------------------------------------------------
    # TRIN 5 - HENT OPLYSNINGER OM BILERNE
    # --------------------------------------------------------
    
    for (j in seq_along(annoncer)) {
      
      # Vælg den aktuelle bil
      bil <- annoncer[[j]]
      
      
      # ------------------------------------------------------
      # 5.1 - BILENS ID
      # ------------------------------------------------------
      
      carid <- NA_character_
      
      if (!is.null(bil$id)) {
        carid <- as.character(bil$id)[1]
        
      } else if (!is.null(bil$listingId)) {
        carid <- as.character(bil$listingId)[1]
      }
      
      
      # ------------------------------------------------------
      # 5.2 - MÆRKE OG MODEL
      # ------------------------------------------------------
      
      maerke <- "Tesla"
      
      model <- NA_character_
      
      if (!is.null(bil$vehicle$model)) {
        model <- as.character(bil$vehicle$model)[1]
      }
      
      
      # ------------------------------------------------------
      # 5.3 - PRIS
      # ------------------------------------------------------
      
      pris <- NA_real_
      
      if (!is.null(bil$price$priceRaw)) {
        pris <- as.numeric(bil$price$priceRaw)[1]
      }
      
      
      # ------------------------------------------------------
      # 5.4 - KILOMETERSTAND
      # ------------------------------------------------------
      
      km <- NA_real_
      
      if (!is.null(bil$vehicle$mileageInKm)) {
        
        km_tekst <- as.character(
          bil$vehicle$mileageInKm
        )[1]
        
        # Fjern alt undtagen tal
        km <- gsub("[^0-9]", "", km_tekst)
        
        # Lav om til tal
        km <- as.numeric(km)
      }
      
      
      # ------------------------------------------------------
      # 5.5 - REGISTRERINGSDATO
      # ------------------------------------------------------
      
      reg_dato <- NA_character_
      
      if (!is.null(bil$firstRegistrationDate)) {
        
        reg_dato <- as.character(
          bil$firstRegistrationDate
        )[1]
        
      } else if (!is.null(bil$vehicle$firstRegistrationDate)) {
        
        reg_dato <- as.character(
          bil$vehicle$firstRegistrationDate
        )[1]
      }
      
      
      # ------------------------------------------------------
      # 5.6 - FORHANDLERNAVN
      # ------------------------------------------------------
      
      forhandler <- NA_character_
      
      if (!is.null(bil$seller$companyName)) {
        
        forhandler <- as.character(
          bil$seller$companyName
        )[1]
        
      } else if (!is.null(bil$seller$name)) {
        
        forhandler <- as.character(
          bil$seller$name
        )[1]
      }
      
      
      # ------------------------------------------------------
      # 5.7 - ADRESSE
      # ------------------------------------------------------
      
      adresse <- NA_character_
      
      postnr <- NA_character_
      by <- NA_character_
      
      if (!is.null(bil$location$zip)) {
        postnr <- as.character(bil$location$zip)[1]
      }
      
      if (!is.null(bil$location$city)) {
        by <- as.character(bil$location$city)[1]
      }
      
      # Saml postnummer og by
      if (!is.na(postnr) || !is.na(by)) {
        
        adresse <- trimws(paste(
          ifelse(is.na(postnr), "", postnr),
          ifelse(is.na(by), "", by)
        ))
      }
      
      
      # ------------------------------------------------------
      # 5.8 - BILENS LINK
      # ------------------------------------------------------
      
      link <- NA_character_
      
      if (!is.null(bil$url)) {
        
        link <- as.character(bil$url)[1]
        
      } else if (!is.null(bil$detailUrl)) {
        
        link <- as.character(bil$detailUrl)[1]
      }
      
      
      # ------------------------------------------------------
      # 5.9 - FIND MANGLENDE DATA I HTML
      # ------------------------------------------------------
      
      if (kort_passer) {
        
        # Find forhandlernavn
        if (is.na(forhandler)) {
          
          forhandler <- html_text2(
            html_element(
              kort[j],
              "[data-testid='dealer-company-name']"
            )
          )
        }
        
        # Find adresse
        if (is.na(adresse)) {
          
          adresse <- html_text2(
            html_element(
              kort[j],
              "[data-testid='dealer-address']"
            )
          )
        }
        
        # Find registreringsdato
        if (is.na(reg_dato)) {
          
          kort_tekst <- html_text2(kort[j])
          
          fund <- regexpr(
            "[0-9]{1,2}/[0-9]{4}",
            kort_tekst
          )
          
          if (fund[1] != -1) {
            reg_dato <- regmatches(kort_tekst, fund)
          }
        }
        
        # Find bilens link
        if (is.na(link)) {
          
          link <- html_attr(
            html_element(
              kort[j],
              "a[href*='/angebote/']"
            ),
            "href"
          )
        }
      }
      
      # Gør relative links til fulde links
      if (!is.na(link) && startsWith(link, "/")) {
        
        link <- paste0(
          "https://www.autoscout24.de",
          link
        )
      }
      
      
      # ------------------------------------------------------
      # TRIN 6 - OPRET DATAFRAME FOR BILEN
      # ------------------------------------------------------
      
      ny_bil <- data.frame(
        carid = carid,
        maerke = maerke,
        model = model,
        pris = pris,
        valuta = "EUR",
        km = km,
        reg_dato = reg_dato,
        forhandler = forhandler,
        adresse = adresse,
        link = link,
        scrapedate = Sys.Date(),
        stringsAsFactors = FALSE
      )
      
      # Tilføj bilen til sidens dataframe
      resultat <- rbind(resultat, ny_bil)
    }
    
    
    # --------------------------------------------------------
    # KONTROLLÉR SIDENS RESULTAT
    # --------------------------------------------------------
    
    if (all(is.na(resultat$pris)) ||
        all(is.na(resultat$km))) {
      
      stop("Pris eller km mangler for alle biler")
    }
    
    if (all(is.na(resultat$carid)) &&
        all(is.na(resultat$link))) {
      
      stop("Bil-ID og links mangler")
    }
    
    resultat
    
  }, error = function(e) {
    
    cat(
      "Fejl på side", i, ":",
      conditionMessage(e), "\n"
    )
    
    NULL
  })
  
  
  # ----------------------------------------------------------
  # TRIN 7 - SAML OG GEM RESULTATERNE
  # ----------------------------------------------------------
  
  # Stop ved fejl
  if (is.null(sidens_biler)) {
    
    cat("Kørslen stoppede på side", i, "\n")
    
    break
  }
  
  # Saml alle biler
  tyske_biler <- rbind(
    tyske_biler,
    sidens_biler
  )
  
  # Fjern dubletter ud fra ID og link
  dublet_id <- !is.na(tyske_biler$carid) &
    duplicated(tyske_biler$carid)
  
  dublet_link <- !is.na(tyske_biler$link) &
    duplicated(tyske_biler$link)
  
  tyske_biler <- tyske_biler[
    !(dublet_id | dublet_link), ]
  
  # Nulstil rækkenumre
  rownames(tyske_biler) <- NULL
  
  # Gem bilerne
  saveRDS(tyske_biler, fil_biler)
  
  # Gem gennemførte sider
  faerdige_sider <- c(faerdige_sider, i)
  
  saveRDS(faerdige_sider, fil_sider)
  
  # Vis status
  cat(
    "Side", i, "af", antal_sider,
    "-", nrow(sidens_biler), "fundet -",
    nrow(tyske_biler), "unikke biler i alt\n"
  )
  
  Sys.sleep(5)
}


# ------------------------------------------------------------
# TRIN 8 - KONTROLLÉR RESULTATET
# ------------------------------------------------------------

# Antal gennemførte sider
length(faerdige_sider)

# Antal biler
nrow(tyske_biler)

# Kontrollér manglende oplysninger
if (nrow(tyske_biler) > 0) {
  
  colSums(is.na(tyske_biler))
  
  # Se de første biler
  head(tyske_biler)
  
  # Åbn hele dataframen
  View(tyske_biler)
}


# ------------------------------------------------------------
# TRIN 9 - GEM SOM CSV
# ------------------------------------------------------------

if (nrow(tyske_biler) > 0) {
  
  write.csv2(
    tyske_biler,
    "OLA4_1_2_Tesla_Tyskland_RETTET.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
}


# ------------------------------------------------------------
# TRIN 10 - HENT GEMTE DATA FREM IGEN
# ------------------------------------------------------------

# Brug denne linje ved genstart:
# tyske_biler <- readRDS(
#   "OLA4_1_2_Tesla_Tyskland_RETTET.rds"
# )




# ============================================================
# OLA 4 - KONTROL AF OPGAVE 1.1 OG 1.2
# ============================================================


# ------------------------------------------------------------
# TRIN 1 - ANTAL BILER
# ------------------------------------------------------------

nrow(biler)
nrow(tyske_biler)


# ------------------------------------------------------------
# TRIN 2 - KONTROLLÉR KOLONNER
# ------------------------------------------------------------

names(biler)
names(tyske_biler)


# ------------------------------------------------------------
# TRIN 3 - MANGLENDE OPLYSNINGER
# ------------------------------------------------------------

colSums(is.na(biler))
colSums(is.na(tyske_biler))


# ------------------------------------------------------------
# TRIN 4 - KONTROLLÉR DUBLETTER
# ------------------------------------------------------------

sum(duplicated(biler$carid))
sum(duplicated(tyske_biler$carid))


# ------------------------------------------------------------
# TRIN 5 - SE DATA
# ------------------------------------------------------------

View(biler)
View(tyske_biler)


# ------------------------------------------------------------
# TRIN 6 - GEM DATA
# ------------------------------------------------------------

saveRDS(biler, "Tesla_Danmark.rds")
saveRDS(tyske_biler, "Tesla_Tyskland.rds")



# ============================================================
# OLA 4 - KONTROL AF FORHANDLEROPLYSNINGER
# ============================================================


# ------------------------------------------------------------
# TRIN 1 - DANMARK
# ------------------------------------------------------------

# Find biler uden forhandler
mangler_dk <- biler[is.na(biler$forhandler), ]

# Antal
nrow(mangler_dk)

# Se de første 5 bilers links
head(mangler_dk[, c("carid", "link")], 5)


# ------------------------------------------------------------
# TRIN 2 - TYSKLAND
# ------------------------------------------------------------

# Find biler uden forhandler
mangler_de <- tyske_biler[
  is.na(tyske_biler$forhandler), ]

# Antal
nrow(mangler_de)

# Se de første 5 bilers links
head(mangler_de[, c("carid", "link")], 5)


# ------------------------------------------------------------
# TRIN 3 - KONTROLLÉR DANSKE ANNONCER
# ------------------------------------------------------------

# Se hvor mange annoncer der er kontrolleret
table(biler$kontrolleret, useNA = "ifany")

# ------------------------------------------------------------
# SÆLGERTYPE - DANMARK
# ------------------------------------------------------------

biler$saelgertype <- "Forhandler"

biler$saelgertype[
  is.na(biler$forhandler)
] <- "Privat"


# ------------------------------------------------------------
# SÆLGERTYPE - TYSKLAND
# ------------------------------------------------------------

tyske_biler$saelgertype <- "Forhandler"

tyske_biler$saelgertype[
  is.na(tyske_biler$forhandler)
] <- "Privat"


# Kontrollér resultatet
table(biler$saelgertype)
table(tyske_biler$saelgertype)

# Gem begge dataframes
saveRDS(biler, "Tesla_Danmark.rds")
saveRDS(tyske_biler, "Tesla_Tyskland.rds")






# ============================================================
# OLA 4 - OPGAVE 1.2
# PRISSAMMENLIGNING - 30 TESLA MODEL 3
# DANMARK VS. TYSKLAND
# ============================================================


# ------------------------------------------------------------
# TRIN 1 - KONTROLLÉR DATA
# ------------------------------------------------------------

if (!exists("biler") || !exists("tyske_biler")) {
  stop("Indlæs først biler og tyske_biler")
}

# Dansk pris kan hedde pris eller pris_dkk
if (!"pris_dkk" %in% names(biler)) {
  biler$pris_dkk <- biler$pris
}

# Lav kopier af begge dataframes
dk <- biler
de <- tyske_biler


# ------------------------------------------------------------
# TRIN 2 - FIND REGISTRERINGSÅR
# ------------------------------------------------------------

find_aar <- function(x) {
  
  tekst <- as.character(x)
  
  aar <- sub(
    ".*?((19|20)[0-9]{2}).*",
    "\\1",
    tekst,
    perl = TRUE
  )
  
  aar[!grepl("(19|20)[0-9]{2}", tekst)] <- NA
  
  as.numeric(aar)
}

dk$aar <- find_aar(dk$reg_dato)
de$aar <- find_aar(de$reg_dato)

# Sørg for at pris og km er tal
dk$pris_dkk <- as.numeric(as.character(dk$pris_dkk))
dk$km <- as.numeric(as.character(dk$km))

de$pris <- as.numeric(as.character(de$pris))
de$km <- as.numeric(as.character(de$km))


# ------------------------------------------------------------
# TRIN 3 - VÆLG TESLA MODEL 3 FRA 2022
# ------------------------------------------------------------

# Kilometerstand: 40.000-80.000 km
# Vi sammenligner forhandlerannoncer

dk <- dk[
  !is.na(dk$model) &
    grepl("Model[[:space:]]*3", dk$model,
          ignore.case = TRUE) &
    !is.na(dk$aar) & dk$aar == 2022 &
    !is.na(dk$km) &
    dk$km >= 40000 & dk$km <= 80000 &
    !is.na(dk$pris_dkk) & dk$pris_dkk > 0 &
    !is.na(dk$forhandler),
  , drop = FALSE
]

de <- de[
  !is.na(de$model) &
    grepl("Model[[:space:]]*3", de$model,
          ignore.case = TRUE) &
    !is.na(de$aar) & de$aar == 2022 &
    !is.na(de$km) &
    de$km >= 40000 & de$km <= 80000 &
    !is.na(de$pris) & de$pris > 0 &
    !is.na(de$forhandler),
  , drop = FALSE
]

# Kontrollér antal biler
cat("Danmark:", nrow(dk), "biler\n")
cat("Tyskland:", nrow(de), "biler\n")

if (nrow(dk) == 0 || nrow(de) == 0) {
  stop("Ingen biler opfylder kriterierne")
}


# ------------------------------------------------------------
# TRIN 4 - IMPORTOMKOSTNINGER
# ------------------------------------------------------------

# Foreløbige eksempler - skal dokumenteres
kurs <- 7.46

transport <- 4000
syn <- 900
nummerplader <- 1180
gebyrer <- 400

# Samlede importomkostninger uden afgift
omkostninger <- transport + syn +
  nummerplader + gebyrer

# Omregn tyske priser til DKK
de$pris_dkk <- de$pris * kurs

# Pris inklusive importomkostninger,
# men uden registreringsafgift
de$import_foer_afgift <- de$pris_dkk + omkostninger


# ------------------------------------------------------------
# TRIN 5 - SAMMENLIGN ALLE RELEVANTE BILER
# ------------------------------------------------------------

markedsoversigt <- data.frame(
  
  land = c("Danmark", "Tyskland"),
  
  antal = c(nrow(dk), nrow(de)),
  
  median_km = c(
    median(dk$km),
    median(de$km)
  ),
  
  median_pris_dkk = c(
    median(dk$pris_dkk),
    median(de$pris_dkk)
  ),
  
  median_med_import = c(
    median(dk$pris_dkk),
    median(de$import_foer_afgift)
  )
)

# Se resultatet
print(markedsoversigt)


# ------------------------------------------------------------
# TRIN 6 - VÆLG 30 TYSKE BILER
# ------------------------------------------------------------

# Sortér efter kilometerstand
de <- de[order(de$km, de$pris), ]

# Vælg højst 30 biler
antal <- min(30, nrow(dk), nrow(de))

# Fordel de tyske biler over kilometerintervallet
indeks <- round(
  seq(1, nrow(de), length.out = antal)
)

de_valgt <- de[indeks, ]

cat("Antal bilpar:", antal, "\n")


# ------------------------------------------------------------
# TRIN 7 - FIND 30 SAMMENLIGNELIGE DANSKE BILER
# ------------------------------------------------------------

# Find de danske biler med nærmeste km
# Hver dansk bil bruges kun én gang

match_indeks <- integer(antal)
brugte_dk <- integer(0)

for (i in seq_len(antal)) {
  
  # Danske biler, som ikke er brugt
  muligheder <- setdiff(
    seq_len(nrow(dk)),
    brugte_dk
  )
  
  # Beregn forskel i kilometerstand
  afstand <- abs(
    dk$km[muligheder] - de_valgt$km[i]
  )
  
  # Vælg nærmeste bil
  j <- muligheder[which.min(afstand)]
  
  # Gem match
  match_indeks[i] <- j
  brugte_dk <- c(brugte_dk, j)
}

dk_valgt <- dk[match_indeks, ]


# ------------------------------------------------------------
# TRIN 8 - OPRET SAMMENLIGNINGSTABEL
# ------------------------------------------------------------

sammenligning <- data.frame(
  
  nr = seq_len(antal),
  
  tysk_id = as.character(de_valgt$carid),
  
  dansk_id = as.character(dk_valgt$carid),
  
  km_tyskland = de_valgt$km,
  
  km_danmark = dk_valgt$km,
  
  forskel_km = abs(
    de_valgt$km - dk_valgt$km
  ),
  
  pris_tyskland_eur = de_valgt$pris,
  
  pris_tyskland_dkk = round(
    de_valgt$pris_dkk
  ),
  
  pris_danmark_dkk = dk_valgt$pris_dkk,
  
  import_foer_afgift = round(
    de_valgt$import_foer_afgift
  ),
  
  link_tyskland = as.character(de_valgt$link),
  
  link_danmark = as.character(dk_valgt$link),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------
# TRIN 9 - BEREGN MAKSIMAL REGISTRERINGSAFGIFT
# ------------------------------------------------------------

# Hvor meget afgift kan vi betale,
# før importen bliver dyrere end den danske bil?

sammenligning$maks_regafgift <-
  sammenligning$pris_danmark_dkk -
  sammenligning$import_foer_afgift

# Positivt resultat = mulig besparelse før afgift
# Negativt resultat = importen er allerede dyrere


# ------------------------------------------------------------
# TRIN 10 - REGISTRERINGSAFGIFT
# ------------------------------------------------------------

# Registreringsafgiften skal undersøges
# individuelt for hver bil.
#
# NA betyder, at den endnu ikke er kendt.
# Vi må IKKE bare antage, at afgiften er 0 kr.

sammenligning$registreringsafgift <- NA_real_

# Bilernes variant og udstyr skal også kontrolleres
# via deres links.
#
# TRUE = samme variant / sammenlignelige
# FALSE = ikke samme variant
# NA = endnu ikke kontrolleret

sammenligning$samme_variant <- NA


# ------------------------------------------------------------
# TRIN 11 - BEREGN ENDELIG IMPORTPRIS
# ------------------------------------------------------------

# Når registreringsafgiften er kendt,
# kan den indtastes i tabellen.

# Eksempel på indtastning:
# sammenligning$registreringsafgift[1] <- 8000
# sammenligning$samme_variant[1] <- TRUE

sammenligning$import_inkl_afgift <-
  sammenligning$import_foer_afgift +
  sammenligning$registreringsafgift

# Beregn besparelse
sammenligning$besparelse <- NA_real_

# Beregn kun, når afgift er kendt
# og variant er bekræftet
ok <- !is.na(sammenligning$registreringsafgift) &
  !is.na(sammenligning$samme_variant) &
  sammenligning$samme_variant == TRUE

sammenligning$besparelse[ok] <-
  sammenligning$pris_danmark_dkk[ok] -
  sammenligning$import_inkl_afgift[ok]


# ------------------------------------------------------------
# TRIN 12 - VIS RESULTATER
# ------------------------------------------------------------

# Sammenligning af alle relevante annoncer
View(markedsoversigt)

# De 30 bilpar
View(sammenligning)

# Kort oversigt
print(sammenligning[, c(
  "nr",
  "km_tyskland",
  "km_danmark",
  "forskel_km",
  "pris_tyskland_dkk",
  "pris_danmark_dkk",
  "import_foer_afgift",
  "maks_regafgift"
)])

# Hvor mange par er undersøgt?
cat("Antal valgte bilpar:", nrow(sammenligning), "\n")
cat("Antal med kendt afgift og variant:", sum(ok), "\n")

# Kun hvis vi har kontrollerede sammenligninger
if (any(ok)) {
  
  cat(
    "Median besparelse:",
    median(sammenligning$besparelse[ok]),
    "kr.\n"
  )
}


# ------------------------------------------------------------
# TRIN 13 - GEM RESULTATERNE
# ------------------------------------------------------------

write.csv2(
  markedsoversigt,
  "OLA4_markedsoversigt.csv",
  row.names = FALSE
)

write.csv2(
  sammenligning,
  "OLA4_sammenligning_30_bilpar.csv",
  row.names = FALSE
)

saveRDS(
  sammenligning,
  "OLA4_sammenligning_30_bilpar.rds"
)



# ------------------------------------------------------------
# KLARGØR DATA TIL SQL
# ------------------------------------------------------------

# 1. Lav kopier
biler_sql <- biler
tyske_biler_sql <- tyske_biler


# 2. Pris og kilometerstand til tal
if ("pris_dkk" %in% names(biler_sql)) {
  biler_sql$pris_dkk <- as.numeric(biler_sql$pris_dkk)
} else {
  biler_sql$pris <- as.numeric(biler_sql$pris)
}

biler_sql$km <- as.numeric(biler_sql$km)

tyske_biler_sql$pris <- as.numeric(tyske_biler_sql$pris)
tyske_biler_sql$km <- as.numeric(tyske_biler_sql$km)


# 3. Registreringsår til heltal
find_aar <- function(x) {
  tekst <- as.character(x)
  aar <- sub(".*?((19|20)[0-9]{2}).*", "\\1",
             tekst, perl = TRUE)
  aar[!grepl("(19|20)[0-9]{2}", tekst)] <- NA
  as.integer(aar)
}

biler_sql$reg_aar <- find_aar(biler_sql$reg_dato)
tyske_biler_sql$reg_aar <- find_aar(tyske_biler_sql$reg_dato)


# 4. Behold ID og CVR som identifikatorer
biler_sql$carid <- as.character(biler_sql$carid)
tyske_biler_sql$carid <- as.character(tyske_biler_sql$carid)

if ("cvr" %in% names(biler_sql)) {
  biler_sql$cvr <- as.character(biler_sql$cvr)
}


# 5. Gem data
saveRDS(biler_sql, "Tesla_Danmark_SQL_klar.rds")
saveRDS(tyske_biler_sql, "Tesla_Tyskland_SQL_klar.rds")

write.csv(biler_sql, "Tesla_Danmark_SQL_klar.csv",
          row.names = FALSE)

write.csv(tyske_biler_sql, "Tesla_Tyskland_SQL_klar.csv",
          row.names = FALSE)


# 6. Kontrollér resultatet
str(biler_sql)
str(tyske_biler_sql)


