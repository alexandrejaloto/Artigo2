library(dplyr)
library(ggplot2)
rm(list = ls())


# preparation ----

areas <- c('CH', 'CN', 'LC', 'MT')

# load deciles
load('rdata/deciles.RData')

# load parameters
load('rdata/pars.RData')

# load constants
load('rdata/official_constants.RData')

# load samples
load('rdata/samples.RData')

stop <- c('Tamanho fixo (45)', 'Tamanho fixo (20)', 'Erro padrão (0,30)', 'Erro padrão (0,30) ou Redução do erro (0,015)')
stop <- c('TF45', 'TF20', 'EP30', 'EP30RE015')
selection <- c('Aleatório', 'Máxima Informação de Fisher', 'Progressivo Restrito (-1)', 'Progressivo Restrito (0)', 'Progressivo Restrito (1)', 'Progressivo Restrito (2)')

# RMSE ----

table_conditional_rmse <- data.frame()

for(area_ in areas)
  table_conditional_rmse <- rbind(
    table_conditional_rmse,
    read.table(
      paste0('results/table_conditional_rmse_', area_, '.csv'),
      header = TRUE,
      sep = ';',
      dec = ','
    )
  )

table_conditional_rmse$area <- rep(areas, each = 24)

conditions <- rownames(table_conditional_rmse)[1:24]

graphic <- data.frame()

for(area_ in areas)
{
  graphic. <- subset(table_conditional_rmse, area == area_)[,-11]
  for(i in 1:24)
    graphic <- rbind(
      graphic,
      data.frame(
        x = deciles[[area_]],
        y = as.numeric(graphic.[i,]),
        condition = conditions[i],
        area = area_
      )
    )
}

graphic$selection <- rep(selection, each = 40)
graphic$stop <- rep(stop, each = 10)

p <- graphic %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(aes(shape = selection, colour = selection)) +
  geom_line(aes(linetype = selection, colour = selection)) +
  facet_grid(area ~ stop) +
  scale_colour_discrete(labels=selection, name = 'Seleção') +
  scale_linetype_discrete(labels = selection, name = 'Seleção') +
  scale_shape_discrete(labels = selection, name = 'Seleção') +
  labs(x="theta", y = 'REQM') +
  theme_bw() +
  theme(legend.position = 'bottom')

jpeg (
  filename = paste0 ('graphics/rmse.jpg'),
  width = 3200,
  height = 3200,
  units = "px",
  pointsize = 12,
  quality = 200,
  bg = "white",
  res = 300,
  restoreConsole = TRUE
)

p

dev.off()

# SE ----

table_conditional_se <- data.frame()

for(area_ in areas)
  table_conditional_se <- rbind(
    table_conditional_se,
    read.table(
      paste0('results/table_conditional_se_', area_, '.csv'),
      header = TRUE,
      sep = ';',
      dec = ','
    )
  )

table_conditional_se$area <- rep(areas, each = 24)

conditions <- rownames(table_conditional_se)[1:24]

graphic <- data.frame()

for(area_ in areas)
{
  graphic. <- subset(table_conditional_se, area == area_)[,-11]
  for(i in 1:24)
    graphic <- rbind(
      graphic,
      data.frame(
        x = deciles[[area_]],
        y = as.numeric(graphic.[i,]),
        condition = conditions[i],
        area = area_
      )
    )
}

graphic$selection <- rep(selection, each = 40)
graphic$stop <- rep(stop, each = 10)

p <- graphic %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(aes(shape = selection, colour = selection)) +
  geom_line(aes(linetype = selection, colour = selection)) +
  facet_grid(area ~ stop) +
  scale_colour_discrete(labels=selection, name = 'Seleção') +
  scale_linetype_discrete(labels = selection, name = 'Seleção') +
  scale_shape_discrete(labels = selection, name = 'Seleção') +
  labs(x="theta", y = 'Erro padrão') +
  theme_bw() +
  theme(legend.position = 'bottom')
p
jpeg (
  filename = paste0 ('graphics/se.jpg'),
  width = 3200,
  height = 3200,
  units = "px",
  pointsize = 12,
  quality = 200,
  bg = "white",
  res = 300,
  restoreConsole = TRUE
)

p

dev.off()

# density and information ----

thetas.sample <- list()
for(area_ in areas)
{
  thetas.sample[[area_]] <- (real[[area_]] - official.constants[[area_]]$m)/official.constants[[area_]]$s

  thetas.sample[[area_]] <- data.frame(thetas.sample[[area_]]) %>%
    mutate(area = area_) %>%
    rename(theta = thetas.sample..area_..)
}

thetas.sample <- do.call(rbind, thetas.sample)

max.theta <- max(thetas.sample$theta)

thetas <- seq (-3, max(max.theta), .01)

info.graphic <- list()

for (area_ in areas)
{
  items <- subset (pars, area == area_)
  info.graphic[[area_]] <- lapply(thetas, function(x) sum(calc.info(items, x))) %>%
    do.call(rbind, .) %>%
    data.frame()
  info.graphic[[area_]]$area <- area_
}

info.graphic <- do.call(rbind, info.graphic)
names(info.graphic)[1] <- 'info'
info.graphic$thetas.info <- thetas

jpeg (
  filename = paste0 ('graphics/density_information.jpg'),
  width = 3200,
  height = 3200,
  units = "px",
  pointsize = 12,
  quality = 200,
  bg = "white",
  res = 300,
  restoreConsole = TRUE
)

ggplot() +
  geom_density(data = thetas.sample, aes_string(thetas.sample$theta)) +
  geom_line(data = info.graphic, aes(x = thetas.info, y = info/(max(info)/.5)), linetype = 2) +
  scale_y_continuous(sec.axis = sec_axis(~./(max(info.graphic$info)/.5), name = 'informação')) +
  labs(x= "theta", y = "densidade") +
  facet_grid(rows = vars(area)) +
  theme_bw()

dev.off()

# end

# rascunho ----

# function to plot
plot.cond <- function (data, title)
{
  ggplot(data, aes(x = x, y = y)) +
    geom_point(aes(shape = condition, colour = condition)) +
    geom_line(aes(linetype = condition, colour = condition)) +
    ylim(y.min, y.max) +
    scale_colour_discrete(labels=selection, name = 'Seleção') +
    scale_linetype_discrete(labels = selection, name = 'Seleção') +
    scale_shape_discrete(labels = selection, name = 'Seleção') +
    labs(title=title, x="theta", y = label.y) +
    theme_bw()
}

y.min <- min(graphic$y)
y.max <- max(graphic$y)

selection <- c('Aleatório', 'Máxima Informação de Fisher', 'Progressivo Restrito (-1)', 'Progressivo Restrito (0)', 'Progressivo Restrito (1)', 'Progressivo Restrito (2)')
label.y = 'REQM'


p <- list()

# fixed length (45)
p[[1]] <- plot.cond(
  data = graphic[stringr::str_detect(graphic$condition, 'TF45'),],
  title = 'Tamanho fixo (45)'
)

# fixed length (20)
p[[2]] <- plot.cond(
  graphic[stringr::str_detect(graphic$condition, 'TF20'),],
  title = 'Tamanho fixo (20)'
)

# Standard error (0.30)
p[[3]] <- plot.cond(
  graphic[stringr::str_detect(graphic$condition, 'EP30') & !(stringr::str_detect(graphic$condition, 'RE015')),],
  title = 'Erro padrão (0,30)'
)

# Standard error (0.30) or error reduction (0.015)
p[[4]] <- plot.cond(
  graphic[stringr::str_detect(graphic$condition, 'RE015'),],
  title = 'Erro padrão (0,30) ou Redução do erro (0,015)'
)

cowplot::plot_grid(
  p[[1]],
  p[[2]],
  p[[3]],
  p[[4]]
)


p
