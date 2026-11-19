
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


