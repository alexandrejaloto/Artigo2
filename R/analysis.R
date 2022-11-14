library (simCAT)
library (dplyr)
# detach('package:simCAT')
# devtools::install_github('alexandrejaloto/simCAT', force = TRUE)

rm(list = ls())

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

area_ <- 'CH'

i <- 22

# load results
load(paste0('results/', conditions[i], '_', area_, '.RData'))
load(paste0('rascunho/results_so_LC/', conditions[i], '_', area_, '.RData'))

# load true scores
load('rdata/samples.RData')

# load constants
load('rdata/official_constants.RData')

real[[area_]] <- (real[[area_]] - official.constants[[area_]]$m)/official.constants[[area_]]$s

# load pars
load('rdata/pars.RData')

items <- subset (pars, area == area_) %>%
  subset (year != 2011) %>%
  nrow()

items <- paste0('I', 1:items)

analysis <- simCAT:::cat.evaluation(results = results, true.scores = real[[area_]], item.name = items, rmax = .3)

analysis

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

