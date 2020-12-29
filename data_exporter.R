library(tidyverse)
library(jsonlite)
library(lubridate)
library(glue)


data <- read_csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv') %>%
  filter(location == "Portugal" | location == "Spain" | location == "United Kingdom" | location == "Belgium" | location == "Germany" | location == "France" ) %>%
  select(iso_code, location, date, new_cases_smoothed_per_million, new_cases_per_million) %>%
  filter(date >= '2020-12-01')


convertScatter <- function(country) {

  teste <- data %>% filter(location == country) %>%  select(date, new_cases_per_million)

  c <- list()
  for (i in 1:length(teste$date)) {
    a <- list(
      list(
        x = teste$date[i],
        y = teste$new_cases_per_million[i]
      )
    )

    c <- append(c,a)
  }

  return(c)

}

start_date <- data %>% filter(location == "Portugal") %>% select(date) %>% pull() %>% max()
fake_dates <- seq.Date(start_date, as.Date('2021-01-04'), by = 1)

dates <- data %>% filter(location == "Portugal") %>% select(date) %>% pull()

dates <- c(dates, fake_dates)

update <- Sys.Date()
updated <- glue('Atualizado a {day(update)} de {month(update, label = TRUE, abbr = FALSE, locale="pt_PT")} de {year(update)}')

dateTime <- glue('{weekdays(update,abbreviate = T)}, {day(update)} {month(update,label = TRUE, abbr = TRUE)} {year(update)} {format(Sys.time(), "%X")} GMT')

df <- list(
  update = updated,
  dateTime = dateTime,
  days =  dates,
  max = data %>% select(new_cases_per_million) %>% max(),
  portugal = list(
    name = "Portugal",
    data_novos_casos = data %>% filter(location == "Portugal") %>% select(new_cases_smoothed_per_million) %>% pull(),
    data_real = convertScatter("Portugal")
  ),
  espanha = list(
    name = "Espanha",
    data_novos_casos = data %>% filter(location == "Spain") %>% select(new_cases_smoothed_per_million) %>% pull(),
    data_real = convertScatter("Spain")
  ),
  reino_unido = list(
    name = "Reino Unido",
    data_novos_casos = data %>% filter(location == "United Kingdom") %>% select(new_cases_smoothed_per_million) %>% pull(),
    data_real = convertScatter("United Kingdom")
  ),
  belgica = list(
    name = "Bélgica",
    data_novos_casos = data %>% filter(location == "Belgium") %>% select(new_cases_smoothed_per_million) %>% pull(),
    data_real = convertScatter("Belgium")
  ),
  alemanha = list(
    name = "Alemanha",
    data_novos_casos = data %>% filter(location == "Germany") %>% select(new_cases_smoothed_per_million) %>% pull(),
    data_real = convertScatter("Germany")
  ),
  franca = list(
    name = "França",
    data_novos_casos = data %>% filter(location == "France") %>% select(new_cases_smoothed_per_million) %>% pull(),
    data_real = convertScatter("France")
  )
)

df <- toJSON(df, pretty = TRUE, auto_unbox = TRUE)

write(df, 'data.json')
cat('✅✅✅ Script Ran!' )






