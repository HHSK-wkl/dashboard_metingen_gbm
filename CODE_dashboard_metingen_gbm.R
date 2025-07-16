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


# data-bewerking ----------------------------------------------------------

datum_begin <- floor_date(Sys.Date() %m-% period(80, "days"), unit = "months")

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

# periode_recent <- glue("van {format(datum_begin, '%e %B %Y')} tot {format(Sys.Date(), '%e %B %Y')}")


# figuren en tabellen -----------------------------------------------------

gbm_recent %>%
  arrange(desc(paf_acuut)) %>%
  mutate(naam = str_to_sentence(naam),
         waarde = map2_chr(waarde, eenheid, 
                           ~paste(format(signif(.x, 3), decimal.mark = ",", scientific = FALSE), .y))) %>% 
  # mutate(opmaak_acuut = paf_acuut > 0.005,
  #        opmaak_chronisch = paf_chronisch > 0.005,
  #        opmaak_ov = ov_factor >= 1,
  #        opmaak_meetwaarde = opmaak_ov | opmaak_acuut | opmaak_chronisch) %>%
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
  reactable(columns = 
              list(
                Meetwaarde = colDef(),
                ov_factor = colDef(name = "Overschrijdingsfactor norm"),
                paf_acuut = colDef(name = "PAF (acuut)"),
                paf_chronisch = colDef(name = "PAF (chronisch)")
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
