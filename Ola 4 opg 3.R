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



