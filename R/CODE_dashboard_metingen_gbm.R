library(tidyverse)
library(HHSKwkl)
library(sf)
library(glue)
library(reactable)

# data --------------------------------------------------------------------

fys_chem <- readRDS("data/fys_chem.rds")
meetpunten <- readRDS("data/meetpunten.rds")
parameters <- readRDS("data/parameters.rds")

normen <- readRDS("data/normen.rds")
transpermethrin <- tibble(parnr = 1324, naam = "trans-permethrin", wns_code = NA_character_,
                          norm_JGM = NA_real_, norm_MAX = NA_real_, norm_P90 = 0.0002, min_norm = 0.0002)
normen <- bind_rows(normen, transpermethrin)

toxiciteit <- readxl::read_excel("data/gbm_toxiciteit.xlsx", sheet = "SSDinfo")
ws_grens <- st_read("data/ws_grens.gpkg", crs = 28992, quiet = TRUE)


# functies ----------------------------------------------------------------

f_parnaam <- maak_opzoeker(select(parameters, parnr, parnaamlang))
f_aquopar <- maak_opzoeker(parameters, parnr, aquo_parcode)

kleur_waarde <- function(grenswaarde){
  
  function(value){
    if (is.na(value)) {
      color <- "#222222"
      weight <- "normal"
    } else if (value >= grenswaarde) {
      color <- "#ff0000"
      weight <- "bold"
    } else {
      color <- "#222222"
      weight <- "normal"
    }
    list(color = color, fontWeight = weight)
  }
}

# style = function(value) {
#   if (value > 0) {
#     color <- "#008000"
#   } else if (value < 0) {
#     color <- "#e00000"
#   } else {
#     color <- "#777"
#   }
#   list(color = color, fontWeight = "bold")
# }
# )

# data-bewerking ----------------------------------------------------------

datum_begin <- floor_date(Sys.Date() %m-% period(80, "days"), unit = "months")
# datum_begin <- make_date(2025, 1 , 1)

gbm_recent <-
  fys_chem %>%
  filter(parnr > 999, parnr < 2000,
         datum >= datum_begin) %>%
  filter(is.na(detectiegrens)) %>% # Niet aangetroffen stoffen zijn niet zo relevant
  left_join(normen) %>%
  mutate(naam = f_parnaam(parnr)) %>% 
  mutate(ov_factor = waarde / min_norm) %>%
  mutate(paf_acuut = paf_gbm(f_aquopar(parnr),
                             concentratie = waarde,
                             detectiegrens = detectiegrens,
                             ssd_data = toxiciteit,
                             type_paf = "acuut"),
         paf_chronisch = paf_gbm(f_aquopar(parnr),
                                 concentratie = waarde,
                                 detectiegrens = detectiegrens,
                                 ssd_data = toxiciteit,
                                 type_paf = "chronisch")) 

periode_recent <- glue("van {format(datum_begin, '%e %B %Y')} tot {format(Sys.Date(), '%e %B %Y')}")


# figuren en tabellen -----------------------------------------------------

tabel_recent <-
  gbm_recent %>%
  arrange(desc(paf_acuut)) %>%
  mutate(naam = str_to_sentence(naam),
         waarde = map2_chr(waarde, eenheid, ~paste(format(signif(.x, 3), decimal.mark = ",", scientific = FALSE), .y))) %>% 

  select(Stof = naam,
         Meetpunt = mp,
         Datum =  datum,
         Meetwaarde = waarde,
         # Eenheid = eenheid,
         ov_factor,
         paf_acuut,
         paf_chronisch,
         # contains("opmaak")
         ) %>%
  reactable(
    filterable = TRUE,
    columns = 
      list(
        Meetwaarde = colDef(align = "right", filterable = FALSE),
        ov_factor = colDef(name = "Overschrijdingsfactor norm", 
                           style = kleur_waarde(1),
                           format = colFormat(locales = "nl-NL", digits = 1),
                           filterable = FALSE),
        paf_acuut = colDef(name = "PAF (acuut)",
                           style = kleur_waarde(0.005),
                           format = colFormat(locales = "nl-NL", digits = 2, percent = TRUE),
                           filterable = FALSE
        ),
        paf_chronisch = colDef(name = "PAF (chronisch)",
                               style = kleur_waarde(0.005),
                               format = colFormat(locales = "nl-NL", digits = 2, percent = TRUE),
                               filterable = FALSE
        )
      )
  )

  
  
  
  
  # 
  # DT::datatable(extensions = 'Buttons', rownames = FALSE,
  #               options = list(dom = 'lfirtpB',
  #                              buttons = c('csv', 'excel', 'pdf'),
  #                              scrollX = TRUE,
  #                              pageLength = 10,
  #                              columnDefs = list(list(targets = c(7,8,9, 10), visible = FALSE)))) %>% # nummering begint bij 0
  # DT::formatRound(7, digits = 2, mark = "") %>%
  # DT::formatStyle(3:5, textAlign = 'right') %>%
  # DT::formatPercentage(c(5,6), digits = 2) %>%
  # formatStyle(5, 8, color = styleEqual(levels = c(TRUE, FALSE), values = c('red', 'black'))) %>%
  # formatStyle(6, 9, color = styleEqual(levels = c(TRUE, FALSE), values = c('red', 'black'))) %>%
  # formatStyle(7, 10, color = styleEqual(levels = c(TRUE, FALSE), values = c('red', 'black'))) %>%
  # formatStyle(4, 11, color = styleEqual(levels = c(TRUE, FALSE), values = c('red', 'black')))
