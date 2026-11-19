
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
