# detach('package:simCAT')
# devtools::install_github('alexandrejaloto/simCAT')
library (simCAT)
library (dplyr)

rm(list = ls())

# prepare simulation ----

# load parameters
load('rdata/pars.RData')
pars <- subset (pars, year != 2011)

# load thetas (samples)
load('rdata/samples.RData')

# load constants
load('rdata/official_constants.RData')

areas <- c('CH', 'CN', 'LC', 'MT')
areas <- c('CH', 'CN', 'MT')
areas <- c('CN')
replications <- 1

# end

# selection: random; fixed length (45) ----

for (area_ in areas)
{
  # area_ <- 'CN'
  load(paste0('rdata/resps_', area_, '.RData'))

  start.theta <- (mean(real[[area_]]) - official.constants[[area_]]$m)/official.constants[[area_]]$s

  items <- subset (pars, area == area_)

  results <- list()

  for (rep in 1:replications)
  {
    set.seed(rep)
    results[[rep]] <- simCAT::simCAT(
      resps = resps[[rep]][1:10,],
      bank = items[,1:3],
      start.theta = start.theta,
      sel.method = 'random',
      cat.type = 'fixed',
      acceleration = 1,
      threshold = 45,
      rmax = 1,
      content.names = 1:30,
      content.props = rep(1/30, 30),
      content.items = items$CO_HABILIDADE,
      met.content = "MCCAT",
      stop = list(fixed = 45)
    )
  }
  save(results, file = paste0('results/ALETF452_', area_, '.RData'))

}



# selection: random; fixed length (20) ----

for (area_ in areas)
{
  load(paste0('rdata/resps_', area_, '.RData'))

  start.theta <- (mean(real[[area_]]) - official.constants[[area_]]$m)/official.constants[[area_]]$s

  items <- subset (pars, area == area_) %>%
    select (NU_PARAM_A, NU_PARAM_B, NU_PARAM_C)

  results <- list()

  for (rep in 1:replications)
  {
    set.seed(rep)
    results[[rep]] <- simCAT::simCAT(
      resps = resps[[rep]],
      bank = items[,1:3],
      start.theta = start.theta,
      sel.method = 'random',
      cat.type = 'fixed',
      acceleration = 1,
      threshold = 20,
      rmax = 1,
      content.names = 1:30,
      content.props = rep(1/30, 30),
      content.items = items$CO_HABILIDADE,
      met.content = "MCCAT",
      stop = list(fixed = 20)
    )
  }

  save(results, file = paste0('results/ALETF20_', area_, '.RData'))
}

# selection: random; standard error (0.3) ----

for (area_ in areas)
{
  load(paste0('rdata/resps_', area_, '.RData'))

  start.theta <- (mean(real[[area_]]) - official.constants[[area_]]$m)/official.constants[[area_]]$s

  items <- subset (pars, area == area_) %>%
    select (NU_PARAM_A, NU_PARAM_B, NU_PARAM_C)

  results <- list()

  for (rep in 1:replications)
  {
    set.seed(rep)
    results[[rep]] <- simCAT::simCAT(
      resps = resps[[rep]],
      bank = items[,1:3],
      start.theta = start.theta,
      sel.method = 'random',
      cat.type = 'variable',
      acceleration = 1,
      threshold = .3,
      rmax = 1,
      content.names = 1:30,
      content.props = rep(1/30, 30),
      content.items = items$CO_HABILIDADE,
      met.content = "MCCAT",
      stop = list(se = .3, min.items = 15, max.items = 60)
    )
  }

  save(results, file = paste0('results/ALEEP30_', area_, '.RData'))
}

# selection: random; standard error (.3)/error reduction (.015) ----

for (area_ in areas)
{
  load(paste0('rdata/resps_', area_, '.RData'))

  start.theta <- (mean(real[[area_]]) - official.constants[[area_]]$m)/official.constants[[area_]]$s

  items <- subset (pars, area == area_) %>%
    select (NU_PARAM_A, NU_PARAM_B, NU_PARAM_C)

  results <- list()

  for (rep in 1:replications)
  {
    set.seed(rep)
    results[[rep]] <- simCAT::simCAT(
      resps = resps[[rep]],
      bank = items[,1:3],
      start.theta = start.theta,
      sel.method = 'random',
      cat.type = 'variable',
      acceleration = 1,
      threshold = .3,
      rmax = 1,
      content.names = 1:30,
      content.props = rep(1/30, 30),
      content.items = items$CO_HABILIDADE,
      met.content = "MCCAT",
      stop = list(se = .3, min.items = 15, max.items = 60, hypo = .015, hyper = Inf)
    )
  }

  save(results, file = paste0('results/ALEEP30RE015_', area_, '.RData'))
}

# selection: MFI; fixed length (45) ----

for (area_ in areas)
{
  # area_ <- 'CH'
  load(paste0('rdata/resps_', area_, '.RData'))

  start.theta <- (mean(real[[area_]]) - official.constants[[area_]]$m)/official.constants[[area_]]$s

  items <- subset (pars, area == area_)

  results <- list()

  for (rep in 1:replications)
  {
    set.seed(rep)
    results[[rep]] <- simCAT::simCAT(
      resps = resps[[rep]],
      bank = items[,1:3],
      start.theta = start.theta,
      sel.method = 'MFI',
      cat.type = 'fixed',
      acceleration = 1,
      threshold = 45,
      rmax = 1,
      content.names = 1:30,
      content.props = rep(1/30, 30),
      content.items = items$CO_HABILIDADE,
      met.content = "MCCAT",
      stop = list(fixed = 45)
    )
  }
  save(results, file = paste0('results/MIFTF45_', area_, '.RData'))

}



# selection: MFI; fixed length (20) ----

for (area_ in areas)
{
  load(paste0('rdata/resps_', area_, '.RData'))

  start.theta <- (mean(real[[area_]]) - official.constants[[area_]]$m)/official.constants[[area_]]$s

  items <- subset (pars, area == area_) %>%
    select (NU_PARAM_A, NU_PARAM_B, NU_PARAM_C)

  results <- list()

  for (rep in 1:replications)
  {
    set.seed(rep)
    results[[rep]] <- simCAT::simCAT(
      resps = resps[[rep]],
      bank = items[,1:3],
      start.theta = start.theta,
      sel.method = 'MFI',
      cat.type = 'fixed',
      acceleration = 1,
      threshold = 20,
      rmax = 1,
      content.names = 1:30,
      content.props = rep(1/30, 30),
      content.items = items$CO_HABILIDADE,
      met.content = "MCCAT",
      stop = list(fixed = 20)
    )
  }

  save(results, file = paste0('results/MIFTF20_', area_, '.RData'))
}

# selection: MFI; standard error (0.3) ----

for (area_ in areas)
{
  load(paste0('rdata/resps_', area_, '.RData'))

  start.theta <- (mean(real[[area_]]) - official.constants[[area_]]$m)/official.constants[[area_]]$s

  items <- subset (pars, area == area_) %>%
    select (NU_PARAM_A, NU_PARAM_B, NU_PARAM_C)

  results <- list()

  for (rep in 1:replications)
  {
    set.seed(rep)
    results[[rep]] <- simCAT::simCAT(
      resps = resps[[rep]],
      bank = items[,1:3],
      start.theta = start.theta,
      sel.method = 'MFI',
      cat.type = 'variable',
      acceleration = 1,
      threshold = .3,
      rmax = 1,
      content.names = 1:30,
      content.props = rep(1/30, 30),
      content.items = items$CO_HABILIDADE,
      met.content = "MCCAT",
      stop = list(se = .3, min.items = 15, max.items = 60)
    )
  }

  save(results, file = paste0('results/MIFEP30_', area_, '.RData'))
}

# selection: MFI; standard error (.3)/error reduction (.015) ----

for (area_ in areas)
{
  load(paste0('rdata/resps_', area_, '.RData'))

  start.theta <- (mean(real[[area_]]) - official.constants[[area_]]$m)/official.constants[[area_]]$s

  items <- subset (pars, area == area_) %>%
    select (NU_PARAM_A, NU_PARAM_B, NU_PARAM_C)

  results <- list()

  for (rep in 1:replications)
  {
    set.seed(rep)
    results[[rep]] <- simCAT::simCAT(
      resps = resps[[rep]],
      bank = items[,1:3],
      start.theta = start.theta,
      sel.method = 'MFI',
      cat.type = 'variable',
      acceleration = 1,
      threshold = .3,
      rmax = 1,
      content.names = 1:30,
      content.props = rep(1/30, 30),
      content.items = items$CO_HABILIDADE,
      met.content = "MCCAT",
      stop = list(se = .3, min.items = 15, max.items = 60, hypo = .015, hyper = Inf)
    )
  }

  save(results, file = paste0('results/MIFEP30RE015_', area_, '.RData'))
}

# selection: PR (acceleration parameter = -1); fixed length (45) ----

for (area_ in areas)
{
  # area_ <- 'CH'
  load(paste0('rdata/resps_', area_, '.RData'))

  start.theta <- (mean(real[[area_]]) - official.constants[[area_]]$m)/official.constants[[area_]]$s

  items <- subset (pars, area == area_)

  results <- list()

  for (rep in 1:replications)
  {
    set.seed(rep)
    results[[rep]] <- simCAT::simCAT(
      resps = resps[[rep]],
      bank = items[,1:3],
      start.theta = start.theta,
      sel.method = 'progressive',
      cat.type = 'fixed',
      acceleration = -1,
      threshold = 45,
      rmax = .3,
      content.names = 1:30,
      content.props = rep(1/30, 30),
      content.items = items$CO_HABILIDADE,
      met.content = "MCCAT",
      stop = list(fixed = 45)
    )
  }
  save(results, file = paste0('results/PR-1TF45_', area_, '.RData'))

}



# selection: MFI; fixed length (20) ----

for (area_ in areas)
{
  load(paste0('rdata/resps_', area_, '.RData'))

  start.theta <- (mean(real[[area_]]) - official.constants[[area_]]$m)/official.constants[[area_]]$s

  items <- subset (pars, area == area_) %>%
    select (NU_PARAM_A, NU_PARAM_B, NU_PARAM_C)

  results <- list()

  for (rep in 1:replications)
  {
    set.seed(rep)
    results[[rep]] <- simCAT::simCAT(
      resps = resps[[rep]],
      bank = items[,1:3],
      start.theta = start.theta,
      sel.method = 'MFI',
      cat.type = 'fixed',
      acceleration = 1,
      threshold = 20,
      rmax = 1,
      content.names = 1:30,
      content.props = rep(1/30, 30),
      content.items = items$CO_HABILIDADE,
      met.content = "MCCAT",
      stop = list(fixed = 20)
    )
  }

  save(results, file = paste0('results/MIFTF20_', area_, '.RData'))
}

# selection: MFI; standard error (0.3) ----

for (area_ in areas)
{
  load(paste0('rdata/resps_', area_, '.RData'))

  start.theta <- (mean(real[[area_]]) - official.constants[[area_]]$m)/official.constants[[area_]]$s

  items <- subset (pars, area == area_) %>%
    select (NU_PARAM_A, NU_PARAM_B, NU_PARAM_C)

  results <- list()

  for (rep in 1:replications)
  {
    set.seed(rep)
    results[[rep]] <- simCAT::simCAT(
      resps = resps[[rep]],
      bank = items[,1:3],
      start.theta = start.theta,
      sel.method = 'MFI',
      cat.type = 'variable',
      acceleration = 1,
      threshold = .3,
      rmax = 1,
      content.names = 1:30,
      content.props = rep(1/30, 30),
      content.items = items$CO_HABILIDADE,
      met.content = "MCCAT",
      stop = list(se = .3, min.items = 15, max.items = 60)
    )
  }

  save(results, file = paste0('results/MIFEP30_', area_, '.RData'))
}

# selection: MFI; standard error (.3)/error reduction (.015) ----

for (area_ in areas)
{
  load(paste0('rdata/resps_', area_, '.RData'))

  start.theta <- (mean(real[[area_]]) - official.constants[[area_]]$m)/official.constants[[area_]]$s

  items <- subset (pars, area == area_) %>%
    select (NU_PARAM_A, NU_PARAM_B, NU_PARAM_C)

  results <- list()

  for (rep in 1:replications)
  {
    set.seed(rep)
    results[[rep]] <- simCAT::simCAT(
      resps = resps[[rep]],
      bank = items[,1:3],
      start.theta = start.theta,
      sel.method = 'MFI',
      cat.type = 'variable',
      acceleration = 1,
      threshold = .3,
      rmax = 1,
      content.names = 1:30,
      content.props = rep(1/30, 30),
      content.items = items$CO_HABILIDADE,
      met.content = "MCCAT",
      stop = list(se = .3, min.items = 15, max.items = 60, hypo = .015, hyper = Inf)
    )
  }

  save(results, file = paste0('results/MIFEP30RE015_', area_, '.RData'))
}


# selection: PR (acceleration parameter = -1); fix length (45) ----

for (area_ in areas)
{
  load(paste0('rdata/resps_', area_, '.RData'))

  start.theta <- (mean(real[[area_]]) - official.constants[[area_]]$m)/official.constants[[area_]]$s

  items <- subset (pars, area == area_) %>%
    select (NU_PARAM_A, NU_PARAM_B, NU_PARAM_C)

  results <- list()

  set.seed(1000)
  for (rep in 1:replications)
    results[[rep]] <- simCAT::simCAT(
      resps = resps[[rep]][1:10,],
      bank = items,
      start.theta = start.theta,
      sel.method = 'MFI',
      cat.type = 'fixed',
      acceleration = -1,
      stop = list(fixed = 45)
    )

  save(results, file = paste0('results/MIFTF45_', area_, '.RData'))
}


# selection: maximum Fisher information; fix length (20) ----

for (area_ in areas)
{
  load(paste0('rdata/resps_', area_, '.RData'))

  start.theta <- (mean(real[[area_]]) - official.constants[[area_]]$m)/official.constants[[area_]]$s

  items <- subset (pars, area == area_) %>%
    select (NU_PARAM_A, NU_PARAM_B, NU_PARAM_C)

  results <- list()

  set.seed(1000)
  for (rep in 1:replications)
    results[[rep]] <- simCAT::simCAT(
      resps = resps[[rep]],
      bank = items,
      start.theta = start.theta,
      sel.method = 'MFI',
      stop = list(fixed = 20)
    )

  save(results, file = paste0('results/MIFTF20_', area_, '.RData'))
}

# selection: maximum Fisher information; standard error (0.3) ----

for (area_ in areas)
{
  load(paste0('rdata/resps_', area_, '.RData'))

  start.theta <- (mean(real[[area_]]) - official.constants[[area_]]$m)/official.constants[[area_]]$s

  items <- subset (pars, area == area_) %>%
    select (NU_PARAM_A, NU_PARAM_B, NU_PARAM_C)

  results <- list()

  set.seed(1000)
  for (rep in 1:replications)
    results[[rep]] <- simCAT::simCAT(
      resps = resps[[rep]],
      bank = items,
      start.theta = start.theta,
      sel.method = 'MFI',
      stop = list(se = .3, min.items = 15, max.items = 60)
    )

  save(results, file = paste0('results/MIFEP30_', area_, '.RData'))
}

# selection: maximum Fisher information; standard error (.3)/error reduction (.015) ----

for (area_ in areas)
{
  load(paste0('rdata/resps_', area_, '.RData'))

  start.theta <- (mean(real[[area_]]) - official.constants[[area_]]$m)/official.constants[[area_]]$s

  items <- subset (pars, area == area_) %>%
    select (NU_PARAM_A, NU_PARAM_B, NU_PARAM_C)

  results <- list()

  set.seed(1000)
  for (rep in 1:replications)
    results[[rep]] <- simCAT::simCAT(
      resps = resps[[rep]],
      bank = items,
      start.theta = start.theta,
      sel.method = 'MFI',
      stop = list(se = .3, min.items = 15, max.items = 60, hypo = .015, hyper = Inf)
    )

  save(results, file = paste0('results/MIFEP30RE015_', area_, '.RData'))
}


