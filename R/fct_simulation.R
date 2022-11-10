fct_simulation <- function(sel.method, cat.type, acceleration, threshold, rmax, stop)
{
  for (area_ in areas)
  {

    load(paste0('rdata/resps_', area_, '.RData'))

    start.theta <- (mean(real[[area_]]) - official.constants[[area_]]$m)/official.constants[[area_]]$s

    items <- subset (pars, area == area_)

    results <- list()

    for (rep in 1:replications)
    {
      print(paste0(area_, rep))

      set.seed(rep, sample.kind = "Rounding")

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
    save(results, file = paste0('results/ALETF452_', area_, '.RData'))

  }
}
