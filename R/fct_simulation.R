# sel.method = 'MFI'
# cat.type = 'variable'
# acceleration = 1
# threshold = .3
# rmax = 1
# stop = list(se = .3, min.items = 15, max.items = 60)
# n = 2400
# condition = 'MIFEP30'

fct_simulation <- function(sel.method, cat.type, acceleration, threshold, rmax, stop, n, condition)
{

  for (area_ in areas)
  {

    # area_ <- 'MT'
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
