rm(list = ls())
library (ggplot2)

# test length
N <- 45

graphic <- data.frame()

# acceleration parameter
for (k in -1:2)
  # item
  for (t in 1:45)
  {
    if (t == 1)
      W <- 0 else {
        W <- (sum(((2:t)-1)^k))/(sum(((2:N)-1)^k))
      }
    graphic <- rbind(
      graphic,
      data.frame(
        k = k,
        t = t,
        W = W
      )
    )
  }

graphic$k <- as.factor(graphic$k)

jpeg (
  filename = paste0 ('graphics/acceleration_parameter.jpg'),
  width = 1600,
  height = 1600,
  units = "px",
  pointsize = 12,
  quality = 200,
  bg = "white",
  res = 300,
  restoreConsole = TRUE
)

ggplot(graphic, aes(x = t, y = W, linetype = k)) +
  geom_line() +
  labs(title='', x="Posição do item", y = "W") +
  theme_bw()

dev.off()
