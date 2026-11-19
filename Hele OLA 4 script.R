# ============================================================
# OLA 4 - OPGAVE 1.1
# TESLA FRA BILBASEN
# ============================================================

# ------------------------------------------------------------
# TRIN 1 - INDSTILLINGER
# ------------------------------------------------------------

# FALSE = brug de biler, vi allerede har hentet
hent_nye_data <- FALSE

antal_sider <- 27

fil_rds <- "OLA4_Tesla_Bilbasen_ENKEL.rds"

fil_csv <- "Tesla_Danmark_SQL_klar.csv"


# ------------------------------------------------------------
# TRIN 2 - WEBSCRAPING
# ------------------------------------------------------------

if (hent_nye_data) {
  
  library(chromote)
  library(rvest)
  
  # Indlæs tidligere indsamlede biler
  if (file.exists(fil_rds)) {
    biler <- readRDS(fil_rds)
  } else {
    biler <- data.frame()
  }
  
  # Start browser
  b <- ChromoteSession$new()
  
  baseurl <- paste0(
    "https://www.bilbasen.dk/brugt/bil/tesla",
    "?includeengroscvr=true&includeleasing=false"
  )
  
  # Gennemgå søgeresultatsiderne
  for (i in seq_len(antal_sider)) {
    
    nye_biler <- tryCatch({
      
      # Åbn hjemmesiden
      url <- paste0(baseurl, "&page=", i)
      
      b$Page$navigate(url = url)
      
      Sys.sleep(5)
      
      # Hent HTML
      html <- b$Runtime$evaluate(
        "document.documentElement.outerHTML",
        returnByValue = TRUE
      )$result$value
      
      side <- read_html(html)
      
      # Find annoncerne
      annoncer <- html_elements(
        side,
        "article[data-e2e='search-result-card']"
      )
      
      if (length(annoncer) == 0) {
        stop("Ingen annoncer fundet")
      }
      
      
      # ------------------------------------------------------
      # 2.1 - BILENS LINK OG ID
      # ------------------------------------------------------
      
      link <- html_attr(
        html_element(
          annoncer,
          "a[data-e2e='search-result-link']"
        ),
        "href"
      )
      
      relativ <- !is.na(link) & startsWith(link, "/")
      
      link[relativ] <- paste0(
        "https://www.bilbasen.dk",
        link[relativ]
      )
      
      # Hent bil-ID fra linket i HTML
      carid <- sub(
        ".*/", "",
        sub("\\?.*$", "", link)
      )
      
      carid[!grepl("^[0-9]+$", carid)] <- NA_character_
      
      
      # ------------------------------------------------------
      # 2.2 - MODEL
      # ------------------------------------------------------
      
      model <- html_text2(
        html_element(
          annoncer,
          "div[class*='Listing_makeModel__']"
        )
      )
      
      model <- trimws(sub("\\n.*$", "", model))
      
      
      # ------------------------------------------------------
      # 2.3 - PRIS
      # ------------------------------------------------------
      
      pris_tekst <- html_text2(
        html_element(
          annoncer,
          "div[class*='Listing_price__'] h3"
        )
      )
      
      pris <- as.numeric(
        gsub("[^0-9]", "", pris_tekst)
      )
      
      
      # ------------------------------------------------------
      # 2.4 - REGISTRERING OG KILOMETER
      # ------------------------------------------------------
      
      detaljer <- html_text2(
        html_element(
          annoncer,
          "div[class*='Listing_details__']"
        )
      )
      
      detaljer <- gsub("[\r\n]+", " ", detaljer)
      
      # Registreringsdato
      reg_dato <- sub(
        ".*?([0-9]{1,2}/[12][0-9]{3}).*",
        "\\1",
        detaljer,
        perl = TRUE
      )
      
      reg_dato[
        !grepl("[0-9]{1,2}/[12][0-9]{3}", detaljer)
      ] <- NA_character_
      
      # Kilometerstand
      km_tekst <- sub(
        ".*?([0-9][0-9.]*[[:space:]]*km\\b).*",
        "\\1",
        detaljer,
        perl = TRUE
      )
      
      km_tekst[
        !grepl(
          "[0-9][0-9.]*[[:space:]]*km\\b",
          detaljer,
          perl = TRUE
        )
      ] <- NA_character_
      
      km <- as.numeric(
        gsub("[^0-9]", "", km_tekst)
      )
      
      
      # ------------------------------------------------------
      # 2.5 - PLACERING
      # ------------------------------------------------------
      
      adresse <- html_text2(
        html_element(
          annoncer,
          "div[class*='Listing_location__']"
        )
      )
      
      
      # ------------------------------------------------------
      # 2.6 - SAML OPLYSNINGERNE
      # ------------------------------------------------------
      
      data.frame(
        carid = carid,
        maerke = "Tesla",
        model = model,
        pris = pris,
        valuta = "DKK",
        reg_dato = reg_dato,
        km = km,
        adresse = adresse,
        forhandler = NA_character_,
        cvr = NA_character_,
        link = link,
        kontrolleret = FALSE,
        scrapedate = Sys.Date(),
        stringsAsFactors = FALSE
      )
      
    }, error = function(e) {
      
      cat(
        "Fejl på side", i, ":",
        conditionMessage(e), "\n"
      )
      
      NULL
    })
    
    if (is.null(nye_biler)) next
    
    # Tilføj nye biler
    biler <- rbind(biler, nye_biler)
    
    # Fjern manglende ID og dubletter
    biler <- biler[
      !is.na(biler$carid) &
        !duplicated(biler$carid),
      , drop = FALSE
    ]
    
    rownames(biler) <- NULL
    
    # Gem data
    saveRDS(biler, fil_rds)
    
    cat(
      "Side", i, "-",
      nrow(biler), "unikke biler\n"
    )
  }
  
  
  # ----------------------------------------------------------
  # 2.7 - HENT FORHANDLERNAVN OG CVR
  # ----------------------------------------------------------
  
  for (i in seq_len(nrow(biler))) {
    
    # Spring allerede kontrollerede biler over
    if (isTRUE(biler$kontrolleret[i])) next
    if (is.na(biler$link[i])) next
    
    info <- tryCatch({
      
      # Åbn annoncen
      b$Page$navigate(url = biler$link[i])
      
      Sys.sleep(5)
      
      html <- b$Runtime$evaluate(
        "document.documentElement.outerHTML",
        returnByValue = TRUE
      )$result$value
      
      side <- read_html(html)
      
      # Find forhandleroplysninger
      boks <- html_element(
        side,
        "[data-e2e='vip-section-dealer-info']"
      )
      
      if (inherits(boks, "xml_missing")) {
        
        list(
          navn = NA_character_,
          cvr = NA_character_
        )
        
      } else {
        
        # Forhandlernavn
        navn <- html_text2(
          html_element(boks, "h2")
        )
        
        if (is.na(navn) ||
            !nzchar(trimws(navn))) {
          navn <- NA_character_
        }
        
        # CVR
        tekst <- html_text2(boks)
        
        fund <- regexpr(
          "CVR[[:space:]-]*(nr\\.?|nummer)?[[:space:]:.]*[0-9]{8}\\b",
          tekst,
          perl = TRUE,
          ignore.case = TRUE
        )
        
        cvr <- NA_character_
        
        if (fund[1] > 0) {
          cvr <- gsub(
            "[^0-9]",
            "",
            regmatches(tekst, fund)
          )
        }
        
        list(navn = navn, cvr = cvr)
      }
      
    }, error = function(e) {
      
      cat(
        "Fejl ved bil", i, ":",
        conditionMessage(e), "\n"
      )
      
      NULL
    })
    
    if (is.null(info)) next
    
    biler$forhandler[i] <- info$navn
    biler$cvr[i] <- info$cvr
    biler$kontrolleret[i] <- TRUE
    
    saveRDS(biler, fil_rds)
  }
  
  # Angiv sælgertype
  biler$saelgertype <- ifelse(
    !is.na(biler$forhandler) &
      trimws(biler$forhandler) != "",
    "Forhandler",
    "Privat"
  )
  
  biler_original <- biler
  
} else {
  
  # ----------------------------------------------------------
  # BRUG DE OPRINDELIGE 800 GEMTE BILER
  # ----------------------------------------------------------
  
  if (!file.exists(fil_csv)) {
    stop("CSV-filen blev ikke fundet. Kontrollér getwd()")
  }
  
  biler_original <- read.csv(
    fil_csv,
    colClasses = c(
      carid = "character",
      cvr = "character"
    ),
    stringsAsFactors = FALSE,
    na.strings = c("", "NA"),
    fileEncoding = "UTF-8"
  )
}


# ------------------------------------------------------------
# TRIN 3 - FJERN PRIVATE ANNONCER
# ------------------------------------------------------------

# Kontrollér sælgertype
if (!"saelgertype" %in% names(biler_original)) {
  stop("Kolonnen saelgertype mangler")
}

table(biler_original$saelgertype)

# Find forhandlerannoncer
forhandler <- !is.na(biler_original$saelgertype) &
  trimws(biler_original$saelgertype) == "Forhandler"

# Behold kun forhandlerbilerne
biler_sql <- biler_original[
  forhandler,
  , drop = FALSE
]

# Nulstil rækkenumre fra 1 til 636
rownames(biler_sql) <- NULL


# ------------------------------------------------------------
# TRIN 4 - KLARGØR DATA TIL DATABASE
# ------------------------------------------------------------

# Pris til tal
biler_sql$pris <- as.numeric(
  as.character(biler_sql$pris)
)

# Kilometerstand til tal
biler_sql$km <- as.numeric(
  as.character(biler_sql$km)
)

# Registreringsår til heltal
if ("reg_aar" %in% names(biler_sql)) {
  
  biler_sql$reg_aar <- as.integer(
    biler_sql$reg_aar
  )
  
} else {
  
  biler_sql$reg_aar <- as.integer(
    sub(
      ".*?((19|20)[0-9]{2}).*",
      "\\1",
      biler_sql$reg_dato,
      perl = TRUE
    )
  )
}

# Behold identifikatorer og links som tekst
biler_sql$carid <- as.character(biler_sql$carid)
biler_sql$cvr <- as.character(biler_sql$cvr)
biler_sql$link <- as.character(biler_sql$link)

# Bil-ID som numeric
biler_sql$carid <- as.numeric(biler_sql$carid)
# ------------------------------------------------------------
# TRIN 5 - KONTROLLÉR OG GEM DATA
# ------------------------------------------------------------

cat("Originale biler:", nrow(biler_original), "\n")

cat("Private fjernet:", sum(!forhandler), "\n")

cat("Forhandlere tilbage:", nrow(biler_sql), "\n")

cat(
  "Dubletter:",
  sum(duplicated(biler_sql$carid)),
  "\n"
)

# Kontrollér at vi har 636 biler
if (nrow(biler_sql) != 636) {
  stop("Antallet er ikke 636. Kontrollér data.")
}

# Kontrollér pris og kilometerstand
if (anyNA(biler_sql$pris) || anyNA(biler_sql$km)) {
  stop("Pris eller kilometerstand mangler")
}

# Gem som RDS
saveRDS(
  biler_sql,
  "Tesla_Danmark_636_Forhandlere.rds"
)

# Gem som CSV til SQL
write.csv(
  biler_sql,
  "Tesla_Danmark_636_Forhandlere.csv",
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

# Se den færdige dataframe
View(biler_sql)
shell.exec(getwd())


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



# ============================================================
# OLA 4 - SAMLET PRISANALYSE AF TESLA
# ============================================================
# 1. Private vs. forhandlere
# 2. Prisforskelle mellem danske områder
# 3. Danmark vs. Tyskland
# 4. Pris i forhold til kilometerstand
#
# Kun Base R
# Ingen ny webscraping
# ============================================================


# ------------------------------------------------------------
# TRIN 1 - INDLÆS DATA
# ------------------------------------------------------------

# Danske biler - alle 800 annoncer
biler_alle <- read.csv(
  "Tesla_Danmark_SQL_klar.csv",
  colClasses = c(
    carid = "character",
    cvr = "character"
  ),
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)

# Tyske biler
if (file.exists("Tesla_Tyskland_SQL_klar.csv")) {
  
  tyske_alle <- read.csv(
    "Tesla_Tyskland_SQL_klar.csv",
    colClasses = c(carid = "character"),
    stringsAsFactors = FALSE,
    na.strings = c("", "NA")
  )
  
} else if (exists("tyske_biler_sql")) {
  
  tyske_alle <- tyske_biler_sql
  
} else if (exists("tyske_biler")) {
  
  tyske_alle <- tyske_biler
  
} else {
  
  stop("Tyske data mangler")
}

# Kontrollér antal
nrow(biler_alle)
nrow(tyske_alle)


# ------------------------------------------------------------
# TRIN 2 - KLARGØR DATA
# ------------------------------------------------------------

# Funktion der finder registreringsår
find_aar <- function(x) {
  
  tekst <- as.character(x)
  
  aar <- sub(
    ".*?((19|20)[0-9]{2}).*",
    "\\1",
    tekst,
    perl = TRUE
  )
  
  aar[
    !grepl("(19|20)[0-9]{2}", tekst)
  ] <- NA_character_
  
  as.integer(aar)
}

# Danmark
biler_alle$pris <- as.numeric(
  as.character(biler_alle$pris)
)

biler_alle$km <- as.numeric(
  as.character(biler_alle$km)
)

biler_alle$aar <- find_aar(
  biler_alle$reg_dato
)

# Tyskland
tyske_alle$pris <- as.numeric(
  as.character(tyske_alle$pris)
)

tyske_alle$km <- as.numeric(
  as.character(tyske_alle$km)
)

tyske_alle$aar <- find_aar(
  tyske_alle$reg_dato
)


# ------------------------------------------------------------
# TRIN 3 - OPDEL PRIVATE OG FORHANDLERE
# ------------------------------------------------------------

# Private danske biler
private_biler <- biler_alle[
  !is.na(biler_alle$saelgertype) &
    trimws(biler_alle$saelgertype) == "Privat",
  , drop = FALSE
]

# Danske forhandlerbiler
forhandler_biler <- biler_alle[
  !is.na(biler_alle$saelgertype) &
    trimws(biler_alle$saelgertype) == "Forhandler",
  , drop = FALSE
]

# Nulstil rækkenumre
rownames(private_biler) <- NULL
rownames(forhandler_biler) <- NULL

# Kontrollér
cat("Private biler:", nrow(private_biler), "\n")
cat("Forhandlerbiler:", nrow(forhandler_biler), "\n")


# ------------------------------------------------------------
# TRIN 4 - VÆLG SAMMENLIGNELIGE TESLA MODEL 3
# ------------------------------------------------------------

# Afgrænsning:
# Model 3
# Registreringsår 2021-2023
# Kilometerstand 30.000-100.000 km

vaelg_biler <- function(x) {
  
  x[
    !is.na(x$model) &
      grepl(
        "Model[[:space:]]*3",
        x$model,
        ignore.case = TRUE
      ) &
      !is.na(x$aar) &
      x$aar >= 2021 &
      x$aar <= 2023 &
      !is.na(x$km) &
      x$km >= 30000 &
      x$km <= 100000 &
      !is.na(x$pris) &
      x$pris > 0,
    , drop = FALSE
  ]
}

# Danske private
privat_m3 <- vaelg_biler(private_biler)

# Danske forhandlere
forhandler_m3 <- vaelg_biler(forhandler_biler)

# Tyske forhandlere
tysk_forhandler <- tyske_alle[
  !is.na(tyske_alle$forhandler) &
    trimws(tyske_alle$forhandler) != "",
  , drop = FALSE
]

tysk_m3 <- vaelg_biler(tysk_forhandler)

# Antal relevante biler
cat("Private Model 3:", nrow(privat_m3), "\n")
cat("Danske forhandler Model 3:", nrow(forhandler_m3), "\n")
cat("Tyske forhandler Model 3:", nrow(tysk_m3), "\n")

if (nrow(privat_m3) == 0 ||
    nrow(forhandler_m3) == 0 ||
    nrow(tysk_m3) == 0) {
  
  stop("Der mangler relevante biler i en gruppe")
}


# ============================================================
# ANALYSE 1 - PRIVATE VS. FORHANDLERE
# ============================================================


# ------------------------------------------------------------
# TRIN 5 - BEREGN MEDIANPRISER
# ------------------------------------------------------------

tabel_saelgere <- data.frame(
  
  saelgertype = c("Privat", "Forhandler"),
  
  antal = c(
    nrow(privat_m3),
    nrow(forhandler_m3)
  ),
  
  medianpris = c(
    median(privat_m3$pris),
    median(forhandler_m3$pris)
  ),
  
  mediankm = c(
    median(privat_m3$km),
    median(forhandler_m3$km)
  )
)

# Prisforskel i kr.
forskel_saelgere <-
  median(forhandler_m3$pris) -
  median(privat_m3$pris)

# Prisforskel i procent
procent_saelgere <-
  forskel_saelgere /
  median(privat_m3$pris) * 100

print(tabel_saelgere)

cat(
  "Forhandler minus privat:",
  round(forskel_saelgere),
  "kr.\n"
)

cat(
  "Prisforskel i procent:",
  round(procent_saelgere, 1),
  "%\n"
)


# ============================================================
# ANALYSE 2 - DANSKE OMRÅDER
# ============================================================


# ------------------------------------------------------------
# TRIN 6 - FIND BILBASENS OMRÅDER
# ------------------------------------------------------------

# Eksempel:
# "Aarhus, Østjylland"
# bliver til "Østjylland"

forhandler_m3$omraade <- trimws(
  sub(
    "^.*,",
    "",
    as.character(forhandler_m3$adresse)
  )
)

# Manglende områder
forhandler_m3$omraade[
  is.na(forhandler_m3$adresse) |
    !grepl(",", forhandler_m3$adresse)
] <- NA_character_


# ------------------------------------------------------------
# TRIN 7 - BEREGN PRISER PR. OMRÅDE
# ------------------------------------------------------------

# Medianpris og kilometerstand
tabel_omraader <- aggregate(
  cbind(pris, km) ~ omraade,
  data = forhandler_m3,
  FUN = median
)

# Antal biler i hvert område
antal_omraader <- table(
  forhandler_m3$omraade
)

tabel_omraader$antal <- as.integer(
  antal_omraader[tabel_omraader$omraade]
)

# Omdøb kolonner
names(tabel_omraader)[2:3] <- c(
  "medianpris",
  "mediankm"
)

# Sortér efter højeste pris
tabel_omraader <- tabel_omraader[
  order(-tabel_omraader$medianpris),
  , drop = FALSE
]

rownames(tabel_omraader) <- NULL

print(tabel_omraader)


# ============================================================
# ANALYSE 3 - DANMARK VS. TYSKLAND
# ============================================================


# ------------------------------------------------------------
# TRIN 8 - OMREGN TYSKE PRISER
# ------------------------------------------------------------

# Foreløbig eurokurs
kurs <- 7.46

# Anslåede importomkostninger
transport <- 4000
syn <- 900
nummerplader <- 1180
gebyrer <- 400

# Importomkostninger uden registreringsafgift
import <- transport + syn + nummerplader + gebyrer

# Omregn EUR til DKK
tysk_m3$pris_dkk <- tysk_m3$pris * kurs

# Tysk pris inkl. import, men før reg.afgift
tysk_m3$pris_foer_afgift <-
  tysk_m3$pris_dkk + import


# ------------------------------------------------------------
# TRIN 9 - SAMMENLIGN PRISER EFTER ÅRGANG
# ------------------------------------------------------------

tabel_lande <- data.frame()

for (a in 2021:2023) {
  
  # Danske biler
  dk <- forhandler_m3[
    forhandler_m3$aar == a,
    , drop = FALSE
  ]
  
  # Tyske biler
  de <- tysk_m3[
    tysk_m3$aar == a,
    , drop = FALSE
  ]
  
  # Mindst 3 annoncer for en median
  pris_dk <- if (nrow(dk) >= 3) {
    median(dk$pris)
  } else {
    NA_real_
  }
  
  pris_de <- if (nrow(de) >= 3) {
    median(de$pris_foer_afgift)
  } else {
    NA_real_
  }
  
  # Forskel i DKK
  forskel <- pris_dk - pris_de
  
  # Forskel i procent
  forskel_pct <- if (!is.na(pris_de)) {
    forskel / pris_de * 100
  } else {
    NA_real_
  }
  
  # Gem årgangens resultat
  tabel_lande <- rbind(
    tabel_lande,
    
    data.frame(
      aar = a,
      
      antal_dk = nrow(dk),
      antal_de = nrow(de),
      
      median_km_dk = if (nrow(dk) > 0) {
        median(dk$km)
      } else {
        NA_real_
      },
      
      median_km_de = if (nrow(de) > 0) {
        median(de$km)
      } else {
        NA_real_
      },
      
      median_dk = pris_dk,
      median_de_foer_afgift = pris_de,
      
      forskel_kr = round(forskel),
      forskel_pct = round(forskel_pct, 1)
    )
  )
}

print(tabel_lande)


# ============================================================
# ANALYSE 4 - PRIS I FORHOLD TIL KILOMETERSTAND
# ============================================================


# ------------------------------------------------------------
# TRIN 10 - VÆLG TESLA MODEL 3 FRA 2022
# ------------------------------------------------------------

# Kun årgang 2022, så forskellige årgange
# ikke blandes i regressionsgrafen.

graf_dk <- forhandler_m3[
  forhandler_m3$aar == 2022,
  , drop = FALSE
]

graf_de <- tysk_m3[
  tysk_m3$aar == 2022,
  , drop = FALSE
]

# Kontrollér antal
nrow(graf_dk)
nrow(graf_de)


# ------------------------------------------------------------
# TRIN 11 - LAV TO LINEÆRE REGRESSIONER
# ------------------------------------------------------------

kan_regression <-
  nrow(graf_dk) >= 3 &&
  nrow(graf_de) >= 3 &&
  length(unique(graf_dk$km)) > 1 &&
  length(unique(graf_de$km)) > 1

if (kan_regression) {
  
  # Danmark
  model_dk <- lm(
    pris ~ km,
    data = graf_dk
  )
  
  # Tyskland
  model_de <- lm(
    pris_foer_afgift ~ km,
    data = graf_de
  )
  
  # R2
  r2_dk <- summary(model_dk)$r.squared
  r2_de <- summary(model_de)$r.squared
  
  # Hældning
  haeldning_dk <- coef(model_dk)["km"]
  haeldning_de <- coef(model_de)["km"]
  
  # Prisændring pr. 10.000 km
  tabel_regression <- data.frame(
    
    land = c("Danmark", "Tyskland"),
    
    antal = c(
      nrow(graf_dk),
      nrow(graf_de)
    ),
    
    pris_aendring_10000_km = round(
      c(haeldning_dk, haeldning_de) * 10000
    ),
    
    R2 = round(
      c(r2_dk, r2_de),
      3
    )
  )
  
  print(tabel_regression)
  
} else {
  
  tabel_regression <- data.frame()
  
  cat("For få observationer til regression\n")
}


#
# ============================================================
# OLA 4 - RETTEDE GRAFER OG TABELLER
# Behold den eksisterende kode fra trin 1-11
# ============================================================


# ------------------------------------------------------------
# TRIN 12 - FUNKTION TIL DANSK TALFORMAT
# ------------------------------------------------------------

# Eksempel: 219900 bliver til 219.900

fmt <- function(x) {
  
  format(
    round(x),
    big.mark = ".",
    scientific = FALSE,
    trim = TRUE
  )
}



# ============================================================
# OLA 4 - ALLE FIRE GRAFER - RETTET
# ============================================================


# ------------------------------------------------------------
# TRIN 12 - DANSK TALFORMAT
# ------------------------------------------------------------

# Eksempel: 219900 bliver til 219.900

fmt <- function(x) 
  
  # ============================================================
# OLA 4 - ALLE FIRE GRAFER - RETTET
# ============================================================


# ------------------------------------------------------------
# TRIN 12 - DANSK TALFORMAT
# ------------------------------------------------------------

# Eksempel: 219900 bliver til 219.900

fmt <- function(x) {
  
  format(
    round(x),
    big.mark = ".",
    scientific = FALSE,
    trim = TRUE
  )
}



# ============================================================
# OLA 4 - ALLE FIRE GRAFER
# Vises direkte i RStudios Plots-fane
# Kun Base R
# ============================================================


# ------------------------------------------------------------
# TRIN 1 - KLARGØR GRAFERNE
# ------------------------------------------------------------

# Luk gamle grafikvinduer og eventuelle PNG-enheder
graphics.off()

# Funktion til danske tusindtalsseparatorer
fmt <- function(x) {
  format(round(x),
         big.mark = ".",
         scientific = FALSE,
         trim = TRUE)
}


# ============================================================
# OLA 4 - ALLE 4 GRAFER DIREKTE I RSTUDIO
# ============================================================


# ------------------------------------------------------------
# TRIN 1 - ÅBN RSTUDIOS PLOTS
# ------------------------------------------------------------

# Luk gamle grafikenheder
graphics.off()

# Brug RStudios indbyggede grafik
options(device = "RStudioGD")

# Dansk talformat
fmt <- function(x) {
  format(
    round(x),
    big.mark = ".",
    scientific = FALSE,
    trim = TRUE
  )
}

# Kontrollér at analysevariablerne findes
krav <- c(
  "graf_dk", "graf_de",
  "model_dk", "model_de",
  "privat_m3", "forhandler_m3",
  "tabel_omraader", "tabel_lande"
)

mangler <- krav[
  !sapply(krav, exists)
]

if (length(mangler) > 0) {
  stop(
    paste(
      "Kør først trin 1-11. Mangler:",
      paste(mangler, collapse = ", ")
    )
  )
}


# ============================================================
# GRAF 1 - PRIS OG KILOMETERSTAND
# ============================================================


# ------------------------------------------------------------
# TRIN 2 - PUNKTDIAGRAM MED REGRESSIONSLINJER
# ------------------------------------------------------------

# Beregn modellernes R2
r2_dk <- summary(model_dk)$r.squared
r2_de <- summary(model_de)$r.squared

# Hældninger
haeldning_dk <- coef(model_dk)["km"]
haeldning_de <- coef(model_de)["km"]

# Alle priser
alle_priser <- c(
  graf_dk$pris,
  graf_de$pris_foer_afgift
)

# Prisinterval
ymin <- min(alle_priser) * 0.90
ymax <- max(alle_priser) * 1.12

# Overskrift
if (haeldning_dk < 0 && haeldning_de < 0) {
  
  titel <- "Højere kilometerstand hænger sammen med lavere pris"
  
} else {
  
  titel <- "Kilometerstand og pris varierer mellem landene"
}

# Plads til aksetitel
par(mar = c(6, 8, 4, 2))

# Tom graf
plot(
  graf_dk$km,
  graf_dk$pris,
  
  type = "n",
  
  xlim = c(30000, 100000),
  ylim = c(ymin, ymax),
  
  axes = FALSE,
  
  main = titel,
  xlab = "Kilometerstand (km)",
  ylab = ""
)

# Hjælpelinjer
ytal <- pretty(c(ymin, ymax), n = 6)

ytal <- ytal[
  ytal >= ymin & ytal <= ymax
]

abline(
  h = ytal,
  col = "grey90"
)

# X-akse
xtal <- seq(30000, 100000, 10000)

axis(
  1,
  at = xtal,
  labels = fmt(xtal),
  cex.axis = 0.8
)

# Y-akse
axis(
  2,
  at = ytal,
  labels = fmt(ytal),
  las = 1,
  cex.axis = 0.8
)

# Y-aksetitel med afstand fra tallene
mtext(
  "Pris (DKK)",
  side = 2,
  line = 5.5
)

box()

# Danske punkter
points(
  graf_dk$km,
  graf_dk$pris,
  
  pch = 16,
  col = adjustcolor(
    "#2563A6",
    alpha.f = 0.5
  )
)

# Tyske punkter
points(
  graf_de$km,
  graf_de$pris_foer_afgift,
  
  pch = 16,
  col = adjustcolor(
    "#D97706",
    alpha.f = 0.5
  )
)

# Danske regressionslinje
x_dk <- seq(
  min(graf_dk$km),
  max(graf_dk$km),
  length.out = 100
)

lines(
  x_dk,
  predict(
    model_dk,
    newdata = data.frame(km = x_dk)
  ),
  col = "#2563A6",
  lwd = 3
)

# Tyske regressionslinje
x_de <- seq(
  min(graf_de$km),
  max(graf_de$km),
  length.out = 100
)

lines(
  x_de,
  predict(
    model_de,
    newdata = data.frame(km = x_de)
  ),
  col = "#D97706",
  lwd = 3
)

# Signatur
legend(
  "topright",
  
  legend = c(
    paste0(
      "Danmark (n=", nrow(graf_dk),
      ", R²=", round(r2_dk, 2), ")"
    ),
    paste0(
      "Tyskland (n=", nrow(graf_de),
      ", R²=", round(r2_de, 2), ")"
    )
  ),
  
  col = c("#2563A6", "#D97706"),
  pch = 16,
  lwd = 3,
  bty = "n",
  cex = 0.8
)

# Datagrundlag
mtext(
  "Bilbasen / AutoScout24 | Model 3, 2022 | 30.000-100.000 km | Tyskland før reg.afgift",
  side = 1,
  line = 4.5,
  cex = 0.65
)


# ============================================================
# GRAF 2 - PRIVATE VS. FORHANDLERE
# ============================================================


# ------------------------------------------------------------
# TRIN 3 - MEDIANPRISER OPDELT PÅ ÅRGANG
# ------------------------------------------------------------

# Opret tabel
tabel_saelgere_aar <- data.frame()

for (a in 2021:2023) {
  
  # Private
  p <- privat_m3[
    privat_m3$aar == a,
    , drop = FALSE
  ]
  
  # Forhandlere
  f <- forhandler_m3[
    forhandler_m3$aar == a,
    , drop = FALSE
  ]
  
  # Medianpriser
  pris_p <- if (nrow(p) >= 3) {
    median(p$pris)
  } else {
    NA_real_
  }
  
  pris_f <- if (nrow(f) >= 3) {
    median(f$pris)
  } else {
    NA_real_
  }
  
  # Saml resultat
  tabel_saelgere_aar <- rbind(
    tabel_saelgere_aar,
    
    data.frame(
      aar = a,
      antal_privat = nrow(p),
      antal_forhandler = nrow(f),
      median_privat = pris_p,
      median_forhandler = pris_f
    )
  )
}

# Kun årgange med nok biler
x <- tabel_saelgere_aar[
  is.finite(tabel_saelgere_aar$median_privat) &
    is.finite(tabel_saelgere_aar$median_forhandler),
  , drop = FALSE
]

# Opret priser
priser <- rbind(
  x$median_privat,
  x$median_forhandler
)

# Plads til y-aksen
par(mar = c(6, 8, 4, 2))

# Lav søjlediagram
bp <- barplot(
  priser,
  
  beside = TRUE,
  
  names.arg = paste0(
    x$aar,
    "\nPrivat n=", x$antal_privat,
    " | Forhandler n=", x$antal_forhandler
  ),
  
  col = c("#94AABA", "#2563A6"),
  
  border = NA,
  axes = FALSE,
  
  ylim = c(0, max(priser) * 1.3),
  
  main = "Billigste sælgertype afhænger af årgangen",
  xlab = "Registreringsår",
  ylab = ""
)

# Y-akse
ytal <- pretty(
  c(0, max(priser) * 1.1),
  n = 6
)

axis(
  2,
  at = ytal,
  labels = fmt(ytal),
  las = 1,
  cex.axis = 0.8
)

# Flyt aksetitel til venstre
mtext(
  "Medianpris (DKK)",
  side = 2,
  line = 5.5
)

# Priser over søjler
text(
  as.vector(bp),
  
  as.vector(priser) +
    max(priser) * 0.025,
  
  labels = fmt(as.vector(priser)),
  cex = 0.85
)

# Forklaring
legend(
  "topleft",
  legend = c("Privat", "Forhandler"),
  fill = c("#94AABA", "#2563A6"),
  bty = "n"
)

# Datagrundlag
mtext(
  "Bilbasen | Model 3, 2021-2023 | 30.000-100.000 km | Ikke justeret for variant",
  side = 1,
  line = 4.5,
  cex = 0.65
)


# ============================================================
# GRAF 3 - DANSKE OMRÅDER
# ============================================================


# ------------------------------------------------------------
# TRIN 4 - MEDIANPRIS EFTER OMRÅDE
# ------------------------------------------------------------

# Mindst 5 annoncer pr. område
graf_omraader <- tabel_omraader[
  tabel_omraader$antal >= 5,
  , drop = FALSE
]

# Sortér efter pris
graf_omraader <- graf_omraader[
  order(graf_omraader$medianpris),
  , drop = FALSE
]

# Samlet median
samlet_median <- median(forhandler_m3$pris)

# Find billigste område
billigst <- graf_omraader$omraade[1]

# Plads til områdenavne
par(mar = c(6, 13, 4, 2))

# Vandret søjlediagram
barplot(
  graf_omraader$medianpris,
  
  names.arg = paste0(
    graf_omraader$omraade,
    " (n=", graf_omraader$antal, ")"
  ),
  
  horiz = TRUE,
  las = 1,
  
  cex.names = 0.8,
  
  col = "#2563A6",
  border = NA,
  
  axes = FALSE,
  
  xlim = c(
    0,
    max(graf_omraader$medianpris) * 1.10
  ),
  
  main = paste(
    billigst,
    "har laveste medianpris"
  ),
  
  xlab = "Medianpris (DKK)"
)

# X-akse
xtal <- pretty(
  c(0, max(graf_omraader$medianpris)),
  n = 6
)

xtal <- xtal[
  xtal >= 0 &
    xtal <= max(graf_omraader$medianpris) * 1.10
]

axis(
  1,
  at = xtal,
  labels = fmt(xtal)
)

# Samlet median
abline(
  v = samlet_median,
  col = "#D97706",
  lty = 2,
  lwd = 2
)

# Forklaring
mtext(
  paste0(
    "Stiplet linje = samlet dansk median: ",
    fmt(samlet_median),
    " kr."
  ),
  side = 3,
  line = 0.5,
  cex = 0.8
)

# Datagrundlag
mtext(
  "Bilbasen | Model 3, 2021-2023 | 30.000-100.000 km | Mindst 5 biler pr. område",
  side = 1,
  line = 4.5,
  cex = 0.65
)


# ============================================================
# GRAF 4 - DANMARK VS. TYSKLAND
# ============================================================


# ------------------------------------------------------------
# TRIN 5 - PRISFORSKELLE EFTER ÅRGANG
# ------------------------------------------------------------

# Kun årgange med priser fra begge lande
graf_lande <- tabel_lande[
  is.finite(tabel_lande$median_dk) &
    is.finite(tabel_lande$median_de_foer_afgift),
  , drop = FALSE
]

# Medianpriser
priser_lande <- rbind(
  graf_lande$median_dk,
  graf_lande$median_de_foer_afgift
)

# Prisforskel
forskel <- graf_lande$median_dk -
  graf_lande$median_de_foer_afgift

# Antal årgange hvor Danmark er billigst
dk_billigst <- sum(forskel < 0)

# Plads til y-aksen
par(mar = c(6, 8, 4, 2))

# Søjlediagram
bp <- barplot(
  priser_lande,
  
  beside = TRUE,
  
  names.arg = paste0(
    graf_lande$aar,
    "\nDK n=", graf_lande$antal_dk,
    " | DE n=", graf_lande$antal_de
  ),
  
  col = c("#2563A6", "#D97706"),
  border = NA,
  axes = FALSE,
  
  ylim = c(
    0,
    max(priser_lande) * 1.45
  ),
  
  main = paste0(
    "Danmark billigst i ",
    dk_billigst,
    " af ", nrow(graf_lande),
    " årgange før reg.afgift"
  ),
  
  xlab = "Registreringsår",
  ylab = ""
)

# Y-akse
ytal <- pretty(
  c(0, max(priser_lande) * 1.1),
  n = 6
)

axis(
  2,
  at = ytal,
  labels = fmt(ytal),
  las = 1,
  cex.axis = 0.8
)

# Flyt aksetitel
mtext(
  "Medianpris (DKK)",
  side = 2,
  line = 5.5
)

# Vis priser over søjler
text(
  as.vector(bp),
  
  as.vector(priser_lande) +
    max(priser_lande) * 0.025,
  
  labels = fmt(as.vector(priser_lande)),
  cex = 0.8
)

# Tekst med prisforskelle
forskel_tekst <- ifelse(
  forskel > 0,
  
  paste0(
    "DE ", fmt(abs(forskel)),
    " kr. billigere"
  ),
  
  ifelse(
    forskel < 0,
    
    paste0(
      "DK ", fmt(abs(forskel)),
      " kr. billigere"
    ),
    
    "Samme medianpris"
  )
)

# Vis prisforskelle over søjlerne
text(
  colMeans(bp),
  
  apply(priser_lande, 2, max) +
    max(priser_lande) * 0.12,
  
  labels = forskel_tekst,
  cex = 0.8,
  font = 2
)

# Signatur
legend(
  "topleft",
  
  legend = c(
    "Danmark",
    "Tyskland før reg.afgift"
  ),
  
  fill = c("#2563A6", "#D97706"),
  bty = "n"
)

# Datagrundlag
mtext(
  "Bilbasen / AutoScout24 | Model 3, 30.000-100.000 km | Tysk pris inkl. anslået import, før reg.afgift",
  side = 1,
  line = 4.5,
  cex = 0.65
)

# ------------------------------------------------------------
# OPGAVE 1.3
# ------------------------------------------------------------
# Webscraping af Powers kampagnetilbud
# ------------------------------------------------------------

# Indlæs rvest, som vi bruger til at læse HTML og udtrække oplysninger.
library(rvest)

# Gem adressen på Powers kampagneside, så vi bruger samme side i begge tests.
url <- "https://www.power.dk/kampagne/aktuelt/"

# Hent den rå HTML fra serveren uden at køre sidens JavaScript.
side_raa <- read_html(url)

# Find alle HTML-elementer med tagget pwr-product-item-v2.
# Hvert af disse elementer er et indlæst produktkort.
kort_raa <- side_raa |> html_elements("pwr-product-item-v2")

# Tæl de produktkort, der findes i den rå HTML. I vores test fandt vi 0.
length(kort_raa)

# Åbn siden i Chrome via chromote, så sidens JavaScript kan køre.
side_live <- read_html_live(url)

# Rul 900 pixels ned fra toppen for at få tilbuddene ind i visningen.
side_live$scroll_to(top = 900L)

# Vent to sekunder, så produktkortene får tid til at blive indlæst.
Sys.sleep(2)

# Brug samme selektor som før til at finde kortene i den indlæste side.
kort <- side_live |> html_elements("pwr-product-item-v2")

# Tæl de indlæste kort. I vores test fandt vi 4 efter scroll.
length(kort)

# Saml oplysningerne i en tabel med én række pr. produktkort.
tilbud <- data.frame(
  # Find den første h3-overskrift i hvert kort, og hent produktnavnet som tekst.
  produkt = kort |> html_element("h3") |> html_text2(),
  
  # Find priselementet i hvert kort, og hent tilbudsprisen som tekst.
  pris_tekst = kort |> html_element("pwr-price") |> html_text2(),
  
  # Find det første link i hvert kort, hvor adressen indeholder /p-.
  link = kort |>
    html_element("a[href*='/p-']") |>
    # Hent selve adressen fra linkets href-attribut.
    html_attr("href")
)

# Vis op til seks rækker i konsollen for at kontrollere udtrækket.
head(tilbud)

# Åbn hele tabellen i RStudios datavisning.
View(tilbud)

# Forsøg på at web scarepe Elgiganten

url_elgiganten <- "https://www.elgiganten.dk/"

side_elgiganten_live <- read_html_live(url_elgiganten)

kort_elgiganten_live <- side_elgiganten_live |>
  html_elements('article[data-cms="product-carousel"] [data-testid="product-card"]')

length(kort_elgiganten_live)

sektioner <- side_elgiganten_live |>
  html_elements('article[data-cms="product-carousel"]')

overskrifter <- sektioner |>
  html_element("h1, h2") |>
  html_text2()

black_deals <- sektioner[
  tolower(trimws(overskrifter)) %in% "skarpe black deals"
]

kort_black_deals <- black_deals |>
  html_elements('[data-testid="product-card"]')

length(kort_black_deals)

# Find produktlinket i h2-overskriften på hvert kort.
produktlink <- kort_black_deals |>
  html_element('h2 a[href^="/product/"]')

# Find det første prisfelt på hvert kort, hvor teksten starter med "Pris ".
prisfelt <- kort_black_deals |>
  html_element(
    xpath = ".//span[contains(concat(' ', normalize-space(@class), ' '), ' sr-only ') and starts-with(normalize-space(.), 'Pris ')]"
  )

# Saml oplysningerne i en tabel med én række pr. produktkort.
tilbud_elgiganten <- data.frame(
  produkt = produktlink |> html_text2(),
  pris = prisfelt |> html_text2(),
  link = produktlink |>
    html_attr("href") |>
    xml2::url_absolute(url_elgiganten)
)

# Se de første rækker i konsollen og hele tabellen i RStudio.
head(tilbud_elgiganten)
View(tilbud_elgiganten)

# ------------------------------------------------------------
# OPGAVE 2.2
# ------------------------------------------------------------

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

# ------------------------------------------------------------
# OPGAVE 2.3
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

# ------------------------------------------------------------
# OPGAVE 3
# ------------------------------------------------------------
library(dplyr)
library(rvest)
library(tidyr)
library(stringr)
library(jsonlite)
library(ggplot2)
library(httr)

# Source Util doc
source("Util.R")


# 1) Research goal
# Find fly over Nordsøen

baseurl <- "https://opensky-network.org/api"

# Create URL
endpoint <- "/states/all"

# Bounding box omkring Nordsøen
lamin <- 52.055129
lamax <- 56.196869
lomin <- -6.065140
lomax <- 4.305954

# Saml URL
toturl <- str_c(baseurl, endpoint,
                "?lamin=", lamin,
                "&lomin=", lomin,
                "&lamax=", lamax,
                "&lomax=", lomax)


# 2) Hent flydata fra OpenSky API
test1 <- rawres(toturl)

# Tjek statuskode
test1$status_code

# Konverter JSON til data frame
rawtext <- httr::content(test1, as = "text")
myJason <- fromJSON(rawtext, flatten = TRUE)
dfFlights <- as.data.frame(myJason$states)


# 3) Scrape kolonnenavne fra OpenSky dokumentation
page <- read_html("https://openskynetwork.github.io/opensky-api/rest.html")

tabs <- page |> html_elements("table") |> html_table()

vars <- tabs[[5]]

# Lav vector med kolonnenavne
names <- unlist(vars$Property)
names <- names[1:ncol(dfFlights)]

# Tildel kolonnenavne
colnames(dfFlights) <- names

# Kontrollér data
#View(dfFlights)
colnames(dfFlights)


# 4) Gem ICAO-numre i en vector
icaov <- dfFlights$icao24

# Kontrollér antal fly
length(icaov)


# 5) Barplot af fly fordelt på lande
ggplot(dfFlights, aes(x = reorder(origin_country, -table(origin_country)[origin_country]))) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  labs(
    title = "Fly over Nordsøen fordelt på lande",
    subtitle = paste("Samlet antal fly:", nrow(dfFlights)),
    x = "Land",
    y = "Antal fly"
  ) +
  theme(axis.text.x = element_text(angle = 90)) +
  expand_limits(y = max(table(dfFlights$origin_country)) * 1.1)




# Opgave 3.2 - Start small - ét fly

# Vælg ét fly fra Nordsøen
icaoId <- icaov[1]

# Opret URL til flyets track
endpointTrack <- "/tracks/all?"
param <- "icao24"
paramTime <- "&time=0"

totUrl <- str_c(baseurl, endpointTrack, param, "=", icaoId, paramTime)

# Kontrollér URL
totUrl

# Hent track fra OpenSky API
test3 <- rawres(totUrl)

# Tjek statuskode
test3$status_code

# Konverter JSON til data frame
rawtext3 <- httr::content(test3, as = "text")

myJason3 <- fromJSON(rawtext3, flatten = TRUE)

dfFlights3 <- as.data.frame(myJason3$path)

#View(dfFlights3)

dim(dfFlights3)

# Kolonnenavne til track
df3Names <- c("time", "lat", "lng", "alt", "crs", "grd")

colnames(dfFlights3) <- df3Names

# Kontrollér data
#View(dfFlights3)

# Plot flyets track
ggplot(dfFlights3, aes(x = lng, y = lat)) +
  geom_path(color = "blue", linewidth = 1) +
  coord_quickmap() +
  labs(
    title = paste("Flyrute - ICAO:", icaoId),
    x = "Longitude",
    y = "Latitude"
  )

#Nu skal vi så kikke på de cirklende fly
# Download cirklende fly
download.file(
  "https://raw.githubusercontent.com/cphstud/DALE25W43/main/circMT.rds",
  "circMT.rds",
  mode = "wb"
)

# Indlæs filen
circFly <- readRDS("circMT.rds")

# Undersøg data
#View(circFly)
str(circFly)

# Gem ICAO-nummer for det cirklende fly
Circel_icaoId <- circFly$icao24[1]

# Kontrollér ICAO-nummer
Circel_icaoId


# Plot cirklende fly
ggplot(circFly, aes(x = lng, y = lat)) +
  geom_path(color = "red", linewidth = 1) +
  labs(
    title = paste("Cirklende fly - ICAO:", Circel_icaoId),
    x = "Longitude",
    y = "Latitude"
  )


# Opgave 3.3 - Move fast - alle fly


# Opret liste til flyenes tracks
allTracks <- list()

# Hent tracks for alle fly
for (i in 1:length(icaov)) {
  icaoId <- icaov[i]
  totUrl <- str_c(baseurl, endpointTrack, param, "=", icaoId, paramTime)
  test3 <- rawres(totUrl)
  
  if (test3$status_code == 200) {
    rawtext3 <- content(test3, as = "text", encoding = "UTF-8")
    myJason3 <- tryCatch(fromJSON(rawtext3, flatten = TRUE),
                         error = function(e) NULL)
    
    if (!is.null(myJason3$path)) {
      allTracks[[i]] <- myJason3$path
    }
  }
}


# Opret vektorer til standardafvigelser
sdKurs <- rep(NA_real_, length(icaov))
sdKursDiff <- rep(NA_real_, length(icaov))


# Beregn standardafvigelser for alle fly med tracks
for (i in 1:length(allTracks)) {
  
  if (!is.null(allTracks[[i]])) {
    dfTrack <- as.data.frame(allTracks[[i]])
    colnames(dfTrack) <- df3Names
    
    # Standardafvigelse af kurs
    sdKurs[i] <- sd(dfTrack$crs, na.rm = TRUE)
    
    # Beregn kursændringer
    kursDiff <- diff(dfTrack$crs)
    kursDiff <- (kursDiff + 180) %% 360 - 180
    
    # Standardafvigelse af kursændringer
    sdKursDiff[i] <- sd(kursDiff, na.rm = TRUE)
  }
}


# Gem resultater i data frame
dfFlights$sdKurs <- sdKurs
dfFlights$sdKursDiff <- sdKursDiff

# Kontrollér resultater
#View(dfFlights)

dfFlights[, c("icao24", "sdKurs", "sdKursDiff")]

# Antal fly med beregnede standardafvigelser
sum(!is.na(dfFlights$sdKursDiff))





# Opgave 3.4 - Aim high - vores egen algoritme

# 1) Opret plads til algoritmens resultater
myOwnAlg <- rep(NA, length(icaov))

# 2) Undersøg alle fly fra Nordsøen
for (i in 1:length(allTracks)) {
  
  if (!is.null(allTracks[[i]])) {
    dfTrack <- as.data.frame(allTracks[[i]])
    colnames(dfTrack) <- df3Names
    
    # Beregn kursændringer
    kursDiff <- diff(dfTrack$crs)
    kursDiff <- (kursDiff + 180) %% 360 - 180
    
    # Vores algoritme: mindst 360 graders samlede drejninger
    myOwnAlg[i] <- sum(abs(kursDiff), na.rm = TRUE) >= 360
  }
}

# Kontrollér resultaterne
table(myOwnAlg, useNA = "ifany")


# 3) Indlæs de kendte cirklende fly
filer <- c("circMT.rds", "circjet.rds", "circjet2.rds", "circjet3.rds", "circjet4.rds", "circjet5.rds", "circjet6.rds", "circjet7.rds", "circjetcph.rds")

kendteFly <- data.frame()

for (fil in filer) {
  
  # Indlæs den eksisterende RDS-fil
  fly <- readRDS(fil)
  
  # Beregn kursændringer
  kursDiff <- diff(fly$crs)
  kursDiff <- (kursDiff + 180) %% 360 - 180
  
  # Gem resultatet
  resultatFly <- data.frame(
    icao = fly$icao24[1],
    sdcourse = sd(fly$crs, na.rm = TRUE),
    myOwnAlg = sum(abs(kursDiff), na.rm = TRUE) >= 360,
    isOff = 1
  )
  
  kendteFly <- rbind(kendteFly, resultatFly)
}

# Kontrollér de kendte fly
#View(kendteFly)

# Antal kendte cirklende fly identificeret
table(kendteFly$myOwnAlg)


# 4) Saml resultaterne fra Nordsøen
resultat <- data.frame(
  icao = dfFlights$icao24,
  sdcourse = dfFlights$sdKurs,
  myOwnAlg = myOwnAlg,
  isOff = 0
)

# Fjern kendte fly fra den aktuelle liste,
# så de ikke optræder to gange
resultat <- resultat[!resultat$icao %in% kendteFly$icao, ]

# Tilføj de 9 kendte cirklende fly
resultat <- rbind(resultat, kendteFly)

# Kontrollér det samlede resultat
#View(resultat)


# 5) Vis potentielt cirklende fly
resultat[resultat$myOwnAlg %in% TRUE, ]

# Kontrollér antal
table(resultat$myOwnAlg, useNA = "ifany")


# Opgave 3.5 - Automate

# Udvælg potentielt cirklende fly fra Nordsøen
mistaenkeligeFly <- resultat[resultat$myOwnAlg %in% TRUE & resultat$icao %in% dfFlights$icao24, ]

# Gem listen i en CSV-fil
write.csv(mistaenkeligeFly, "mistaenkelige_fly.csv", row.names = FALSE)

# Kontrollér resultat
file.exists("mistaenkelige_fly.csv")

# Send automatisk e-mail
source("SendMail.R")

# app adgangskode til (Flyovervågning): ikjv klab ypdw hrfh
