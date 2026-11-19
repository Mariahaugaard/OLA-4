# OLA 4, opgave 1.3 – Webscraping af Powers kampagnetilbud

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
