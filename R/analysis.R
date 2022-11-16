library (simCAT)
library (dplyr)
# detach('package:simCAT')
# devtools::install_github('alexandrejaloto/simCAT', force = TRUE)

rm(list = ls())

areas <- c('CH', 'CN', 'LC', 'MT')

conditions <- c(
  'ALETF45',
  'ALETF20',
  'ALEEP30',
  'ALEEP30RE015',
  'MIFTF45',
  'MIFTF20',
  'MIFEP30',
  'MIFEP30RE015',
  'PR-1TF45',
  'PR-1TF20',
  'PR-1EP30',
  'PR-1EP30RE015',
  'PR0TF45',
  'PR0TF20',
  'PR0EP30',
  'PR0EP30RE015',
  'PR1TF45',
  'PR1TF20',
  'PR1EP30',
  'PR1EP30RE015',
  'PR2TF45',
  'PR2TF20',
  'PR2EP30',
  'PR2EP30RE015'
)

selection <- c(
  'Aleatorio',
  'Aleatorio',
  'Aleatorio',
  'Aleatorio',
  'MIF',
  'MIF',
  'MIF',
  'MIF',
  'PR-1',
  'PR-1',
  'PR-1',
  'PR-1',
  'PR0',
  'PR0',
  'PR0',
  'PR0',
  'PR1',
  'PR1',
  'PR1',
  'PR1',
  'PR2',
  'PR2',
  'PR2',
  'PR2'
)

stop <- c(
  'TF45',
  'TF20',
  'EP30',
  'EP30RE015',
  'TF45',
  'TF20',
  'EP30',
  'EP30RE015',
  'TF45',
  'TF20',
  'EP30',
  'EP30RE015',
  'TF45',
  'TF20',
  'EP30',
  'EP30RE015',
  'TF45',
  'TF20',
  'EP30',
  'EP30RE015',
  'TF45',
  'TF20',
  'EP30',
  'EP30RE015'
)

table_results <- data.frame()

for (area_ in areas)
  for (i in 1:length(conditions))
    # for (i in 10:length(conditions))
  {
    area_ <- 'LC'
    i <- 5

    # load results
    load(paste0('results/', conditions[i], '_', area_, '.RData'))

    # load true scores
    load('rdata/samples.RData')

    # load constants
    load('rdata/official_constants.RData')

    real[[area_]] <- (real[[area_]] - official.constants[[area_]]$m)/official.constants[[area_]]$s

    # load pars
    load('rdata/pars.RData')

    # select items
    items <- subset (pars, area == area_) %>%
      subset (year != 2011) %>%
      nrow()

    # item names
    items <- paste0('I', 1:items)

    # analysis
    analysis <- simCAT:::cat.evaluation(
      results = results,
      true.scores = real[[area_]],
      item.name = items,
      rmax = .3
    )

    analysis

    table_results <- rbind(
      table_results,
      data.frame(
        area = area_,
        selection = selection[i],
        stop = stop[i],
        NIA = analysis$evaluation['length_mean'],
        MIA = analysis$evaluation['length_median'],
        min_length = analysis$evaluation['min_length'],
        max_length = analysis$evaluation['max_length'],
        SE = analysis$evaluation['se'],
        correlation = analysis$evaluation['correlation'],
        RMSE = analysis$evaluation['rmse'],
        bias = analysis$evaluation['bias'],
        min_exp = analysis$evaluation['min_exp'],
        max_exp = analysis$evaluation['max_exp'],
        n_exp0 = analysis$evaluation['n_exp0'],
        n_exp_rmax = analysis$evaluation['n_exp_rmax'],
        overlap = analysis$evaluation['overlap']
      )
    )
  }

rownames(table_results) <- 1:nrow(table_results)

write.table(
  table_results,
  'results/table_results.csv',
  row.names = FALSE,
  sep = ';',
  dec = ','
)

results[[1]]$prev.resps[[1]]


items <- subset (pars, area == area_) %>%
  subset (year != 2011)

items$CO_HABILIDADE
f <- function(x)
  table(items$CO_HABILIDADE[which(paste0("I", 1:nrow(items)) %in% x)])

f(results[[1]]$prev.resps[[1]])

lapply(results[[1]]$prev.resps, f)


f(results$prev.resps[[1]])
lapply(results$prev.resps, f)

