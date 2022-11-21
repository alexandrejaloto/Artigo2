library (simCAT)
library (dplyr)
# detach('package:simCAT')
# devtools::install_github('alexandrejaloto/simCAT', force = TRUE)

# prepare ----

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

# table results ----

table_results <- data.frame()
table_conditional_rmse <- list()
table_conditional_se <- list()

for (area_ in areas)
{
  table_conditional_rmse[[area_]] <- data.frame()
  table_conditional_se[[area_]] <- data.frame()

  for (i in 1:length(conditions))
    # for (i in 10:length(conditions))
  {
    # area_ <- 'MT'
    # i <- 24

    # load results
    load(paste0('results/', conditions[i], '_', area_, '.RData'))

    # load true scores
    load('rdata/samples.RData')

    # load constants
    load('rdata/official_constants.RData')

    real[[area_]] <- (real[[area_]] - official.constants[[area_]]$m)/official.constants[[area_]]$s

    real[[area_]] <- real[[area_]][1:100]

    # load pars
    load('rdata/pars.RData')

    # select items
    items <- subset (pars, area == area_) %>%
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

    # conditional rmse
    table_conditional_rmse[[area_]] <- rbind(
      table_conditional_rmse[[area_]],
      analysis$conditional[1,]
    )

    # conditional se
    table_conditional_se[[area_]] <- rbind(
      table_conditional_se[[area_]],
      analysis$conditional[2,]
    )

  }
}
rownames(table_results) <- 1:nrow(table_results)


# write.table(
#   table_results,
#   'results/table_results.csv',
#   row.names = FALSE,
#   sep = ';',
#   dec = ','
# )

# conditional ----

deciles <- list()

for(area_ in areas)
  deciles[[area_]] <- as.numeric(colnames(table_conditional_rmse[[area_]]))

save(deciles, file = 'rdata/deciles.RData')

for(area_ in areas)
{
  rownames(table_conditional_rmse[[area_]]) <- conditions
  rownames(table_conditional_se[[area_]]) <- conditions

  names(table_conditional_se[[area_]]) <- paste0('decil', 1:10)
  names(table_conditional_rmse[[area_]]) <- paste0('decil', 1:10)

  write.table(
    table_conditional_rmse[[area_]],
    paste0('results/table_conditional_rmse_', area_, '.csv'),
    row.names = TRUE,
    sep = ';',
    dec = ','
  )

  write.table(
    table_conditional_se[[area_]],
    paste0('results/table_conditional_se_', area_, '.csv'),
    row.names = TRUE,
    sep = ';',
    dec = ','
  )
}

# exposure rates ----

exp <- list()

for(area_ in areas)
{

  # area_ <- 'CH'

  # load pars
  load('rdata/pars.RData')

  # select items
  items <- subset (pars, area == area_) %>%
    nrow()

  # item names
  items <- paste0('I', 1:items)
  exp[[area_]] <- data.frame(
    matrix(nrow = 10)
  )
  for (i in 1:24)
  {
    # i <- 1

    # load results
    load(paste0('results/', conditions[i], '_', area_, '.RData'))

    exposure <- simCAT:::exposure.rate(results[[1]]$prev.resps, items)

    exp. <- cut(
      exposure$Freq,
      c(-Inf, 0, .02, .05, .1, .15, .2, .25, .3, .4, 1)
    )
    exp. <- recode(exp., '(-Inf,0]' = '0')

    exp. <- data.frame(prop.table(table(exp.)))

    # names(exp.) <- c('exp', conditions[i])

    exp[[area_]] <- cbind(exp[[area_]], exp.[,2])
  }

  names(exp[[area_]]) <- c('exp', conditions)
  exp[[area_]]$exp <- exp.$exp.

}

exp

exp <- do.call(rbind, exp)

exp$area <- substr(rownames(exp), 1, 2)

exp <- select(exp, area, everything())

write.table(
  exp,
  'results/table_exp.csv',
  row.names = FALSE,
  dec = ',',
  sep = ';'
)

# end

# rascunho ----

results[[1]]$prev.resps[[1]]


items <- subset (pars, area == area_)

items$CO_HABILIDADE
f <- function(x)
  table(items$CO_HABILIDADE[which(paste0("I", 1:nrow(items)) %in% x)])

f(results[[1]]$prev.resps[[1]])

lapply(results[[1]]$prev.resps, f)


f(results$prev.resps[[1]])
lapply(results$prev.resps, f)

