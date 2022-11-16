library (data.table)
library (dplyr)

rm(list = ls())
gc()

data <- fread (
  'D:/Microdados/2020/MICRODADOS_ENEM_2020.csv',
  # nrow = 30000,
  select = c(
    'TP_LINGUA',
    paste0(
      'NU_NOTA_',
      rep (c('CH', 'CN', 'LC', 'MT')
      )
    )
  )
)

scores <- list()
# areas <- c ('CH', 'CN', 'LC', 'MT')
areas <- c ('CH', 'CN', 'MT')

# filter who did not left all answers blank
for (area_ in areas)
{
  scores[[area_]] <- data[,get (paste0('NU_NOTA_', area_))] %>%
    subset (. > 0) %>%
    data.frame()
}

# draw simple random sample from Enem 2020
# sample error = 5
alpha <- .05
real <- list()

Z <- qnorm(1-(alpha/2))

error <- 5

for (area_ in areas)
{

  names (scores[[area_]]) <- 'scores'

  # variance
  S <- var(scores[[area_]])
  sd <- sd (scores[[area_]]$scores)

  n <- ceiling ((sd*Z/error)^2)

  set.seed(1)
  real[[area_]] <- sample(scores[[area_]]$scores, n)

}


# LC ----
area_ <- 'LC'

data.lc <- select(data, NU_NOTA_LC, TP_LINGUA) %>%
  subset (NU_NOTA_LC > 0) %>%
  data.frame()

scores[['LC']] <- data.lc$NU_NOTA_LC
names (scores[[area_]]) <- 'scores'

# draw simple random sample from Enem 2020
# sample error = 5
alpha <- .05
real <- list()

# variance
S <- var(data.lc$NU_NOTA_LC)
sd <- sd (data.lc$NU_NOTA_LC)

Z <- qnorm(1-(alpha/2))

error <- 5

n <- ceiling ((sd*Z/error)^2)

set.seed(1)
sample.lc <- sample(nrow(data.lc), n)

language <- data.lc$TP_LINGUA[sample.lc]
real[[area_]] <- data.lc$NU_NOTA_LC[sample.lc]

real2 <- real
language2 <- language

load('rdata/samples.RData')
load('rdata/language.RData')

all.equal(real2$LC, real$LC)
all.equal(language, language2)

lapply (real, length)
lapply (scores, nrow)

lapply (scores, var)

sd. <- function (x)
{
  unlist (x) %>%
    sd()
}

lapply (scores, sd.)
lapply (real, sd.)

lapply (real, summary)
lapply (scores, summary)

save(real, file = 'rdata/samples.RData')
save(language, file = 'rdata/language.RData')

m.scores <- lapply(scores, function(x) mean(pull(select(x, scores))))
save(m.scores, file = 'rdata/mean.RData')

