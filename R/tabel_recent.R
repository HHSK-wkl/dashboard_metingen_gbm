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

tabel_recent <- function(data_gbm){
  
  require(tidyverse)
  require(HHSKwkl)
  require(reactable) 
  
  
    data_gbm %>%
    mutate(naam = str_to_sentence(naam),
           waarde = map2_chr(waarde, eenheid, ~paste(format(signif(.x, 3), decimal.mark = ",", scientific = FALSE), .y))) %>% 
    
    select(naam,
           mp,
           datum,
           waarde,
           ov_factor,
           paf_acuut,
           paf_chronisch,
    ) %>%
    reactable(
      defaultSorted = list(ov_factor = "desc"),
      filterable = TRUE,
      selection = "single",
      onClick = "select",
      columns = list(
        naam = colDef(name = "Stof"),
        mp = colDef(name = "Meetpunt"),
        datum = colDef(name = "Datum"),
        waarde = colDef(name = "Meetwaarde", align = "right", filterable = FALSE),
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
  
  
}