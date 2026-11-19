# ============================================================
# OLA 4 - OPGAVE 1.1
# ============================================================
# Mest muligt Base R. chromote og rvest er kun til HTML.
# Kør kun automatisk indsamling, hvis I har tilladelse.
# Start med 1 søgeresultatside og 3 bilannoncer.


# ------------------------------------------------------------
# TRIN 1 - PAKKER
# ------------------------------------------------------------
# install.packages("chromote")  # Kun første gang
# install.packages("rvest")     # Kun første gang
library(chromote)
library(rvest)


# ------------------------------------------------------------
# TRIN 2 - INDSTILLINGER
# ------------------------------------------------------------
baseurl <- paste0(
  "https://www.bilbasen.dk/brugt/bil/tesla",
  "?includeengroscvr=true&includeleasing=false"
)

antal_sider <- 27      # Test på 1 side
antal_detaljer <- 3   # Test på 3 annoncer
fil <- "OLA4_Tesla_Bilbasen_ENKEL.rds"

# Åbn gamle resultater, hvis de er gemt
if (file.exists(fil)) {
  biler <- readRDS(fil)
} else {
  biler <- data.frame()
}

# Start browser
b <- ChromoteSession$new()


# ------------------------------------------------------------
# TRIN 3 - HENT BILER FRA SØGERESULTATER
# ------------------------------------------------------------
for (i in seq_len(antal_sider)) {
  
  nye_biler <- tryCatch({
    
    # Lav sidens adresse
    url <- paste0(baseurl, "&page=", i)
    
    # Åbn siden og hent HTML
    b$Page$navigate(url = url)
    Sys.sleep(5)
    
    html <- b$Runtime$evaluate(
      "document.documentElement.outerHTML",
      returnByValue = TRUE
    )$result$value
    
    side <- read_html(html)
    
    # Find alle bilannoncer på denne side
    annoncer <- html_elements(
      side, "article[data-e2e='search-result-card']"
    )
    
    if (length(annoncer) == 0) {
      stop("Ingen bilannoncer fundet")
    }
    
    
    # --------------------------------------------------------
    # TRIN 4 - ID OG LINK
    # --------------------------------------------------------
    link <- html_attr(
      html_element(annoncer, "a[data-e2e='search-result-link']"),
      "href"
    )
    
    # Gør relative links til hele links
    er_relativ <- !is.na(link) & startsWith(link, "/")
    link[er_relativ] <- paste0(
      "https://www.bilbasen.dk", link[er_relativ]
    )
    
    # Bil-ID står til sidst i linket
    carid <- sub(".*/", "", sub("\\?.*$", "", link))
    carid[!grepl("^[0-9]+$", carid)] <- NA_character_
    
    
    # --------------------------------------------------------
    # TRIN 5 - MÆRKE OG MODEL
    # --------------------------------------------------------
    model_tekst <- html_text2(
      html_element(annoncer, "div[class*='Listing_makeModel__']")
    )
    
    # Første linje indeholder typisk f.eks. "Tesla Model 3"
    model <- trimws(sub("\\n.*$", "", model_tekst))
    
    
    # --------------------------------------------------------
    # TRIN 6 - PRIS
    # --------------------------------------------------------
    pris_tekst <- html_text2(
      html_element(annoncer, "div[class*='Listing_price__'] h3")
    )
    
    # "249.900 kr." bliver til tallet 249900
    pris <- as.numeric(gsub("[^0-9]", "", pris_tekst))
    
    
    # --------------------------------------------------------
    # TRIN 7 - REGISTRERING OG KILOMETER
    # --------------------------------------------------------
    detaljer <- html_text2(
      html_element(annoncer, "div[class*='Listing_details__']")
    )
    
    detaljer <- gsub("[\r\n]+", " ", detaljer)
    
    # Find dato, f.eks. "9/2022"
    reg_dato <- sub(
      ".*?([0-9]{1,2}/[12][0-9]{3}).*", "\\1",
      detaljer, perl = TRUE
    )
    
    har_dato <- grepl("[0-9]{1,2}/[12][0-9]{3}", detaljer)
    reg_dato[!har_dato] <- NA_character_
    
    # Find kilometer, f.eks. "67.000 km"
    km_tekst <- sub(
      ".*?([0-9][0-9.]*[[:space:]]*km\\b).*", "\\1",
      detaljer, perl = TRUE
    )
    
    har_km <- grepl(
      "[0-9][0-9.]*[[:space:]]*km\\b", detaljer, perl = TRUE
    )
    km_tekst[!har_km] <- NA_character_
    
    km <- as.numeric(gsub("[^0-9]", "", km_tekst))
    
    
    # --------------------------------------------------------
    # TRIN 8 - PLACERING PÅ SØGERESULTATSIDEN
    # --------------------------------------------------------
    # OBS: Siden viser ofte by/område, ikke fuld adresse.
    adresse <- html_text2(
      html_element(annoncer, "div[class*='Listing_location__']")
    )
    
    
    # --------------------------------------------------------
    # TRIN 9 - SAML SIDENS BILER
    # --------------------------------------------------------
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
    cat("Fejl på side", i, ":", conditionMessage(e), "\n")
    NULL
  })
  
  # Gå videre, hvis siden fejlede
  if (is.null(nye_biler)) next
  
  # Tilføj sidens biler til resten
  biler <- rbind(biler, nye_biler)
  
  # Fjern biler med manglende ID og dubletter
  biler <- biler[!is.na(biler$carid), ]
  biler <- biler[!duplicated(biler$carid), ]
  rownames(biler) <- NULL
  
  # Gem efter hver side - så vi ikke mister arbejdet
  saveRDS(biler, fil)
  
  cat("Side", i, "-", nrow(nye_biler), "biler\n")
  Sys.sleep(2)
}

if (nrow(biler) == 0) stop("Ingen biler blev gemt")


# ------------------------------------------------------------
# TRIN 10 - FORHANDLERNAVN OG CVR-NUMMER
# ------------------------------------------------------------
# Vi åbner kun de første 3 bilannoncer som test.
# Forhandler og CVR skal findes på bilens egen side.

# Hent forhandler og CVR fra alle biler
antal_detaljer <- nrow(biler)

for (i in seq_len(antal_detaljer)) {
  
  # Spring over biler, der allerede er undersøgt
  if (isTRUE(biler$kontrolleret[i])) next
  
  resultat <- tryCatch({
    
    b$Page$navigate(url = biler$link[i])
    Sys.sleep(5)
    
    html <- b$Runtime$evaluate(
      "document.documentElement.outerHTML",
      returnByValue = TRUE
    )$result$value
    
    side <- read_html(html)
    
    # Området med forhandleroplysninger
    boks <- html_element(
      side, "[data-e2e='vip-section-dealer-info']"
    )
    
    if (inherits(boks, "xml_missing")) {
      # Kan være privat sælger eller manglende oplysninger
      list(fornavn = NA_character_, cvrnr = NA_character_)
    } else {
      
      # Find forhandlernavn (h2)
      navn <- html_text2(html_element(boks, "h2"))
      if (is.na(navn) || !nzchar(trimws(navn))) {
        navn <- NA_character_
      }
      
      # Find CVR, hvis 8 cifre står efter "CVR-nr."
      tekst <- html_text2(boks)
      fund <- regexpr(
        "CVR[[:space:]-]*(nr\\.?|nummer)?[[:space:]:.]*[0-9]{8}\\b",
        tekst, perl = TRUE, ignore.case = TRUE
      )
      
      cvrnr <- NA_character_
      if (fund[1] > 0) {
        cvrnr <- gsub("[^0-9]", "", regmatches(tekst, fund))
      }
      
      list(fornavn = navn, cvrnr = cvrnr)
    }
    
  }, error = function(e) {
    cat("Fejl ved bil", i, ":", conditionMessage(e), "\n")
    NULL
  })
  
  if (!is.null(resultat)) {
    biler$forhandler[i] <- resultat$fornavn
    biler$cvr[i] <- resultat$cvrnr
    biler$kontrolleret[i] <- TRUE
    cat("Bil", i, "-", biler$forhandler[i],
        "- CVR:", biler$cvr[i], "\n")
  }
  
  # Gem efter HVER bil, så intet går tabt
  saveRDS(biler, fil)
  Sys.sleep(2)
}


# ------------------------------------------------------------
# TRIN 11 - KONTROLLÉR RESULTATET
# ------------------------------------------------------------
cat("Antal Teslaer:", nrow(biler), "\n")
cat("Forhandlere fundet:", sum(!is.na(biler$forhandler)), "\n")
cat("CVR-numre fundet:", sum(!is.na(biler$cvr)), "\n")

colSums(is.na(biler))
head(biler)
View(biler)


# ------------------------------------------------------------
# TRIN 12 - GEM SOM CSV OG RDS
# ------------------------------------------------------------
write.csv2(biler, "OLA4_Tesla_Bilbasen_ENKEL.csv", row.names = FALSE)
saveRDS(biler, fil)

# Næste gang du åbner RStudio, skal du kun skrive:
# biler <- readRDS("OLA4_Tesla_Bilbasen_ENKEL.rds")
# View(biler)
