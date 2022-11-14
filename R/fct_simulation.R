# sel.method = 'random'
# cat.type = 'fixed'
# acceleration = 1
# threshold = 45
# rmax = 1
# stop = list(fixed = 45)
# n = 0
# condition = 'ALETF45'

fct_simulation <- function(sel.method, cat.type, acceleration, threshold, rmax, stop, n, condition)
{

  for (area_ in areas)
  {

    # area_ <- 'CH'
    load(paste0('rdata/resps_', area_, '.RData'))

    start.theta <- (m.scores[[area_]] - official.constants[[area_]]$m)/official.constants[[area_]]$s

    items <- subset (pars, area == area_)

    results <- list()

    for (rep in 1:replications)
    {
      # rep <- 1

      print(paste0(area_, rep))

      set.seed(rep+n, sample.kind = "Rounding")

      results[[rep]] <- simCAT::simCAT(
        resps = resps[[rep]],
        bank = items[,1:3],
        start.theta = start.theta,
        sel.method = sel.method,
        cat.type = cat.type,
        acceleration = acceleration,
        threshold = threshold,
        rmax = rmax,
        content.names = 1:30,
        content.props = rep(1/30, 30),
        content.items = items$CO_HABILIDADE,
        met.content = "MCCAT",
        stop = stop
      )
    }

    save(results, file = paste0('results/', condition, '_', area_, '.RData'))

  }
}
