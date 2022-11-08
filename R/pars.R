library (dplyr)
library (tidyr)

rm(list = ls())

# testlets codes
source('R/testlets.R')

pars <- data.frame()

areas <- c('CH', 'CN', 'LC', 'MT')
# area <- 'CH'
# year <- 2009
# application <- 1
for (area in areas)
{
  for (year in 2009:2020)
  {
    # import official parameters
    items <- read.table(
      paste0('items/ITENS_PROVA_', year, '.csv'),
      sep = ';',
      header = TRUE
    )

    for(application in 1:2)
    {
      # filter items of each area
      items_area <- subset(items, SG_AREA = area) %>%
        # filter first testlet
        subset(CO_PROVA == testlets[[paste0('year.', year, '.', application)]][[area]][1]) %>%
        # select parameters and content
        select(starts_with('NU_PAR'), CO_HABILIDADE) %>%
        # exclude items without parameters
        drop_na()

      if(nrow(items_area) > 0)
      {
        items_area$area <- area
        items_area$year <- year
        items_area$application <- application

        pars <- rbind(
          pars,
          items_area
        )
      }

      if(application == 2 & year %in% c(2016, 2020))
      {
        # filter items of each area
        items_area <- subset(items, SG_AREA = area) %>%
          # filter first testlet
          subset(CO_PROVA == testlets[[paste0('year.', year, '.', application)]][[area]][1]) %>%
          # select parameters and content
          select(starts_with('NU_PAR'), CO_HABILIDADE) %>%
          # exclude items without parameters
          drop_na()

        items_area$area <- area
        items_area$year <- year
        items_area$application <- 3

        pars <- rbind(
          pars,
          items_area
        )
      }
    }
  }
}

table(pars$area)

save(pars, file = 'rdata/pars.RData')
