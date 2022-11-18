# # resps2 <- resps
# resps <- resps2
#
# resps = resps[[rep]]
# bank = items[,1:3]
# content.names = 1:30
# content.props = rep(1/30, 30)
# content.items = items$CO_HABILIDADE
# met.content = "MCCAT"
# item.language = items$TP_LINGUA


simCAT_LC <- function (resps, bank, start.theta = 0, sel.method = "MFI",
                       cat.type = "variable", acceleration = 1, threshold = 0.3,
                       rmax = 1, content.names = NULL, content.props = NULL, content.items = NULL,
                       met.content = "MCCAT", stop = list(se = 0.3, hypo = 0.015,
                                                          hyper = Inf),
                       item.language)
{

  # bank with all information (parameters, language and content)
  bank <- data.frame(
    bank,
    item.language,
    content.items
  )
  bank$content.items
  rownames(bank) <- paste0("I", 1:nrow(bank))
  if (!is.null(stop$max.items))
    max.items <- stop$max.items
  results <- list()
  score <- data.frame(matrix(ncol = 2))
  convergence <- list()
  theta.history <- list()
  se.history <- list()
  prev.resps <- list()
  bar <- txtProgressBar(min = 0, max = nrow(resps), char = "|",
                        style = 3)

  # person <- 3
  for (person in 1:nrow(resps)) {
    # person <- 1
    # print(paste0('person ', person))

    # exclude items with high exposure rate
    if (person == 1) {
      number_items_available <- 1:nrow(bank)
      bank_available <- bank
    } else {
      exposure <- simCAT:::exposure.rate(prev.resps, rownames(bank))
      number_items_available <- which(exposure$Freq <= rmax)
      bank_available <- bank[number_items_available, ]
    }

    # number_items_available is now from in bank_available
    number_items_available <- 1:nrow(bank_available)

    rownames(bank_available)
    bank_available$content.items


    # if English
    if(language[person] == 0)
    {
      available_language <- number_items_available[(is.na(bank_available$item.language) | bank_available$item.language != 1)]
      # bank_available <- subset(bank_available, (is.na(bank_available$item.language) | bank_available$item.language != 1))
      bank_available <- bank_available[available_language,]
    }
    # if Spanish
    if(language[person] == 1)
    {

      available_language <- number_items_available[(is.na(bank_available$item.language) | bank_available$item.language != 0)]

      # bank_available <- subset(bank_available, (is.na(bank_available$item.language) | bank_available$item.language != 0))
      bank_available <- bank_available[available_language,]
    }

    # bank_available[available_language,]$content.items

    end <- list(stop = FALSE)
    administered <- NULL
    theta.cat <- theta.hist <- start.theta
    SE <- se.hist <- 1
    while (!end$stop) {

      item_select <- select.item(bank = bank_available,
                                 theta = theta.cat, administered = administered,
                                 sel.method = sel.method, cat.type = cat.type,
                                 threshold = threshold, SE = SE, acceleration = acceleration,
                                 max.items = max.items, content.names = content.names,
                                 content.props = content.props, content.items = bank_available$content.items,
                                 met.content = met.content)
      administered <- c(administered, item_select$item)

      theta <- eap(
        pattern = resps[, available_language][person, administered],
        bank = bank_available[administered, ]
      )

      table(content.items[administered])
      table(content.items[number_items_available])

      theta.cat <- theta$theta
      delta.theta <- abs(theta.cat - theta.hist[length(theta.hist)])
      theta.hist <- c(theta.hist, theta.cat)
      SE <- theta$SE
      delta.se <- se.hist[length(se.hist)] - SE
      se.hist <- c(se.hist, SE)
      info <- calc.info(bank = bank_available, theta = theta.cat)
      info[administered] <- 0
      info <- max(info)
      end <- stop.cat(rule = stop, current = list(se = SE,
                                                  delta.theta = delta.theta, info = info, applied = length(administered),
                                                  delta.se = delta.se))
    }
    score[person, ] <- c(theta.cat, SE)
    convergence[[person]] <- end$convergence
    theta.history[[person]] <- theta.hist
    se.history[[person]] <- se.hist
    prev.resps[[person]] <- rownames(bank_available)[administered]
    setTxtProgressBar(bar, person)
  }
  names(score) <- c("theta", "SE")
  results <- list(score = score, convergence = convergence,
                  theta.history = theta.history, se.history = se.history,
                  prev.resps = prev.resps)
  return(results)
}
