#' divide a series in parts
#'
#' @keywords internal
#'
#' @importFrom ggplot2 ggplot ggsave geom_area geom_line geom_vline aes labs %+% element_text geom_point guide_legend scale_colour_manual scale_x_continuous scale_y_continuous theme geom_segment geom_text unit arrow guides theme_light
#' @importFrom stats smooth.spline predict median loess
#' @importFrom dplyr %>% arrange mutate select filter group_by bind_rows if_else left_join inner_join pull slice summarise ungroup desc rename
#' @importFrom tidyr spread
#' @importFrom utils tail head
transformseries.multiple <- function(i.data,
                                     i.max.epidemic.duration = NA,
                                     i.max.season.duration = NA,
                                     i.waves = NA,
                                     i.param.1 = 3,
                                     i.param.2 = 2,
                                     i.min.separation = 1,
                                     i.output = NA,
                                     i.prefix = "Multiple waves",
                                     i.force.smooth = F,
                                     ...) {
  p1 <- list()
  p2 <- list()
  p3 <- list()
  p4 <- list()
  p5 <- list()
  if (length(i.param.1) == 1) {
    if (!is.na(i.param.1) & i.param.1 > 0) {
      param.1 <- i.param.1
    } else {
      param.1 <- 3
    }
  } else {
    param.1 <- 3
  }
  if (length(i.param.2) == 1) {
    if (!is.na(i.param.2) & i.param.2 > 0) {
      param.2 <- i.param.2 * 10 / NCOL(i.data)
    } else {
      param.2 <- 2 * 10 / NCOL(i.data)
    }
  } else {
    param.2 <- 2 * 10 / NCOL(i.data)
  }
  solpalette <- c("#268bd2", "#b58900", "#cb4b16", "#dc322f", "#d33682", "#6c71c4", "#2aa198", "#859900")
  # Prepare the parameters
  if (is.na(i.max.epidemic.duration) | i.max.epidemic.duration == 0) max.epidemic.duration <- NROW(i.data) else max.epidemic.duration <- i.max.epidemic.duration
  if (is.na(i.max.season.duration) | i.max.season.duration == 0) max.season.duration <- NROW(i.data) else max.season.duration <- i.max.season.duration
  if (length(i.waves) == 1) {
    if (!is.na(i.waves) & i.waves > 0) {
      min.waves <- i.waves
      max.waves <- i.waves
    } else {
      min.waves <- 1
      max.waves <- 2 * NCOL(i.data)
    }
  } else if (length(i.waves) == 2) {
    if (!is.na(i.waves[1])) min.waves <- i.waves[1] else min.waves <- 1
    if (!is.na(i.waves[2])) max.waves <- max(min.waves, i.waves[2]) else max.waves <- max(min.waves, 2 * NCOL(i.data))
  } else {
    min.waves <- 1
    max.waves <- 2 * NCOL(i.data)
  }
  # Prepare the data
  # This value is for missing values, its the median of the a third lowest values
  temp1 <- i.data %>%
    as.matrix() %>%
    as.numeric() %>%
    sort() %>%
    head(floor(NCOL(i.data) * NROW(i.data) * 2*param.2/100)) %>%
    median()
  yrweek <- season <- year <- week <- rates <- mrate <- mratej <- NULL
  # I create the dataset for analysis, i fill missing values outside the epidemic period with the
  # values I've just calculated before (median values outside epidemic) with a little jitter
  data <- i.data %>%
    apply(2, fill.missing) %>%
    as.data.frame() %>%
    transformdata.back(i.range.x.final = c(1, 53)) %>%
    `[[`(1) %>%
    dplyr::arrange(yrweek) %>%
    dplyr::mutate(n = 1:n()) %>%
    dplyr::select(-season, -year, -week) %>%
    mutate(mrate = temp1, mratej = max(0, jitter(mrate, factor=3, amount=0)), y = ifelse(is.na(rates), mratej, rates)) %>%
    select(-mrate, -mratej)
  model.smooth <- loess(y ~ n, data, span = 0.05)
  p.model.smooth <- predict(model.smooth, newdata = data$n)
  p.model.smooth[p.model.smooth < 0] <- 0
  rm(temp1)
  rates <- rates.smooth <- NULL
  data.plus <- data %>%
    dplyr::select(-y) %>%
    dplyr::mutate(rates.orig = rates)
  data.plus$rates.smooth <- p.model.smooth
  rm(model.smooth, p.model.smooth)
  # In data.plus I've got the original data, the smoothed data and the filled data (filling missing
  # values with that of the prediction)
  data.plus <- data.plus %>%
    dplyr::mutate(rates.filled = if_else(is.na(rates), rates.smooth, rates)) %>%
    dplyr::select(-rates) %>%
    dplyr::arrange(n)
  if (i.force.smooth) data.plus$rates.filled <- data.plus$rates.smooth
  # Plot the original series, the smooth and the data that will be used
  axis.x.range.original <- range(data.plus$n, na.rm = T)
  axis.x.otick <- optimal.tickmarks(axis.x.range.original[1], axis.x.range.original[2], 10, i.include.min = T, i.include.max = T)
  axis.x.range <- axis.x.otick$range
  axis.x.ticks <- axis.x.otick$tickmarks
  axis.x.labels <- data.plus$yrweek[axis.x.otick$tickmarks]
  axis.y.range.original <- range(c(data.plus$rates.orig, data.plus$rates.smooth), na.rm = T)
  axis.y.otick <- optimal.tickmarks(axis.y.range.original[1], axis.y.range.original[2], 10)
  axis.y.range <- axis.y.otick$range + diff(range(axis.y.otick$range, na.rm = T)) * 0.025 * c(-1, 1)
  axis.y.ticks <- axis.y.otick$tickmarks
  axis.y.labels <- axis.y.otick$tickmarks
  n <- rates.orig <- rates.smooth <- NULL
  p1[[1]] <- ggplot(data.plus) +
    geom_line(aes(x = n, y = rates.orig), color = "#0066CC", linetype = 1, size = 0.75) +
    geom_point(aes(x = n, y = rates.orig), color = "#0066CC", size = 1.5) +
    geom_line(aes(x = n, y = rates.smooth), color = "#CC0066", linetype = 1, size = 0.75) +
    geom_point(aes(x = n, y = rates.smooth), color = "#CC0066", size = 1.5) +
    scale_x_continuous(breaks = axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
    scale_y_continuous(breaks = axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
    labs(title = "Series and smooth", x = "Week", y = "Data") +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5))
  p1[[2]] <- ggplot(data.plus) +
    geom_line(aes(x = n, y = rates.filled), color = "#004000", linetype = 1, size = 0.75) +
    geom_point(aes(x = n, y = rates.filled), color = "#004000", size = 1.5) +
    scale_x_continuous(breaks = axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
    scale_y_continuous(breaks = axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
    labs(title = "Data to be used", x = "Week", y = "Data") +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5))
  # Iterative process to know the increment from one epidemic to the next one
  results <- data.frame()
  data.plot <- data.frame()
  data.temp <- data.plus
  minimum.value <- max(0, min(data.temp$rates.filled, na.rm=T) - max(data.temp$rates.filled, na.rm=T)/10)
  # minimum.value <- min(data.temp$rates.filled, na.rm=T) - max(data.temp$rates.filled, na.rm=T)/10
  # minimum.value <- 0
  data.temp$rates.filled <- data.temp$rates.filled - minimum.value
  for (j in 1:max.waves) {
    peradd <- as.data.frame(matrix(unlist(sapply(1:max.epidemic.duration, percentage.added, i.data = data.temp$rates.filled)), ncol = 6, byrow = T), stringsAsFactors = F)
    names(peradd) <- c("percentage", "start", "end", "duration", "sum", "max")
    n.chosen <- max(1, head((1:max.epidemic.duration)[peradd$percentage < (param.1 / 100)], 1) - 1)
    peradd.chosen <- data.frame(iteration = j, percentage.added(data.temp$rates.filled, n.chosen))
    peradd.chosen$sum.original <- sum(data.plus$rates.filled[peradd.chosen$start:peradd.chosen$end], na.rm=T)
    sum <- cumsumper <- totsum <- difcumsumper <- sumcum <- NULL
    results <- results %>%
      bind_rows(peradd.chosen) %>%
      dplyr::mutate(sumcum = cumsum(sum), totsum = sum(data.plus$rates.filled, na.rm = T), cumsumper = ifelse(totsum==0, 0, sumcum / totsum)) %>%
      dplyr::mutate(difcumsumper = cumsumper - lag(cumsumper))
    results$difcumsumper[1] <- results$cumsumper[1]
    data.plot <- data.plot %>%
      bind_rows(
        data.frame(iteration = j, x = peradd.chosen$start:peradd.chosen$end, y = data.temp$rates.filled[peradd.chosen$start:peradd.chosen$end], stringsAsFactors = F) %>%
          inner_join(results %>%
                       select(iteration, difcumsumper, n), by = "iteration") %>%
          mutate(iteration.label = paste0("Iter: ", sprintf("%02d", iteration), ", Per: ", sprintf("%3.2f", 100 * difcumsumper), " n: ", n))
      )
    label <- percentage <- NULL
    last.point <- data.plot %>%
      group_by(iteration) %>%
      arrange(x) %>%
      slice(1) %>%
      ungroup() %>%
      bind_rows(data.plot %>%
                  group_by(iteration) %>%
                  arrange(-x) %>%
                  slice(1) %>%
                  ungroup()) %>%
      group_by(iteration) %>%
      arrange(y) %>%
      slice(1) %>%
      ungroup() %>%
      inner_join(results %>%
                   select(iteration, percentage), by = "iteration") %>%
      mutate(label = sprintf("%3.2f", 100 * percentage))
    data.temp$rates.filled[peradd.chosen$start:peradd.chosen$end] <- NA
  }
  rm(data.temp)
  data.plot <- data.plot %>%
    mutate(y=y+minimum.value)
  last.point <- last.point %>%
    mutate(y=y+minimum.value)
  # Plot of the iterative process
  solpalette.full <- colorRampPalette(solpalette)(max.waves)
  for (j in 1:max.waves) {
    data.plot.j <- data.plot %>%
      filter(iteration<=j)
    last.point.j <- last.point %>%
      filter(iteration<=j)
    n <- rates.filled <- x <- y <- iteration.label <- NULL
    p2[[j]] <- ggplot() +
      geom_line(data = data.plus, aes(x = n, y = rates.filled), color = "#A0A0A0", size = 1) +
      geom_point(data = data.plus, aes(x = n, y = rates.filled), color = "#A0A0A0", size = 1.5) +
      geom_point(data = data.plot.j, aes(x = x, y = y, color = factor(iteration.label)), size = 4) +
      geom_point(data = last.point.j, aes(x = x, y = y), color = "#FFFFFF", size = 2) +
      geom_text(data = last.point.j, aes(x = x, y = y, label = label), vjust = 1) +
      scale_colour_manual(values = solpalette.full[1:j], guide = guide_legend(nrow = 3)) +
      scale_x_continuous(breaks = axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
      scale_y_continuous(breaks = axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
      labs(title = paste0("Iteration #", j), x = "Week", y = "Data") +
      guides(color = guide_legend(title = paste0("Iteration (Lim: ", sprintf("%3.2f", param.2), ")"))) +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))
  }
  # Sometimes results$sum is not sorted in descending order, depending no how the epidemics were detected, a high
  # value surrounded by 0's might cause an epidemic of length 1 (it stops stacking weeks), and the next one could
  # have 15 values whose sum might be higher than the 1-week epidemic, so I have to rearrange results dataset
  iteration2 <- sum.original <- NULL
  reorderedit <- results %>%
    # To avoid all-zeroes epidemics
    filter(sum.original>0) %>%
    mutate(iteration2 = rank(desc(sum), ties.method = "first")) %>%
    select(iteration, iteration2)
  results <- results %>%
    inner_join(reorderedit, by="iteration") %>%
    select(-iteration, -sumcum, -cumsumper) %>%
    arrange(iteration2) %>%
    rename(iteration=iteration2) %>%
    mutate(sumcum=cumsum(sum)) %>%
    mutate(cumsumper=sumcum/totsum)
  data.plot <- data.plot %>%
    inner_join(reorderedit, by="iteration") %>%
    select(-iteration, -iteration.label) %>%
    arrange(iteration2, x) %>%
    rename(iteration=iteration2) %>%
    mutate(iteration.label = paste0("Iter: ", sprintf("%02d", iteration), ", Per: ", sprintf("%3.2f", 100 * difcumsumper), " n: ", n))
  last.point <- last.point %>%
    inner_join(reorderedit, by="iteration") %>%
    select(-iteration, -iteration.label) %>%
    arrange(iteration2) %>%
    rename(iteration=iteration2) %>%
    mutate(iteration.label = paste0("Iter: ", sprintf("%02d", iteration), ", Per: ", sprintf("%3.2f", 100 * difcumsumper), " n: ", n))
  if (NROW(results)>0) for (j in 1:NROW(results)) {
    data.plot.j <- data.plot %>%
      filter(iteration<=j)
    last.point.j <- last.point %>%
      filter(iteration<=j)
    n <- rates.filled <- x <- y <- iteration.label <- NULL
    p3[[j]] <- ggplot() +
      geom_line(data = data.plus, aes(x = n, y = rates.filled), color = "#A0A0A0", size = 1) +
      geom_point(data = data.plus, aes(x = n, y = rates.filled), color = "#A0A0A0", size = 1.5) +
      geom_point(data = data.plot.j, aes(x = x, y = y, color = factor(iteration.label)), size = 4) +
      geom_point(data = last.point.j, aes(x = x, y = y), color = "#FFFFFF", size = 2) +
      geom_text(data = last.point.j, aes(x = x, y = y, label = label), vjust = 1) +
      scale_colour_manual(values = solpalette.full[1:j], guide = guide_legend(nrow = 3)) +
      scale_x_continuous(breaks = axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
      scale_y_continuous(breaks = axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
      labs(title = paste0("Iteration #", j), x = "Week", y = "Data") +
      guides(color = guide_legend(title = paste0("Iteration (Lim: ", sprintf("%3.2f", param.2), ")"))) +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))
  }
  # The stopping point is determined by param.2
  max.waves.dif <- max(min.waves, min(results$iteration[results$difcumsumper < (param.2 / 100)][1] - 1, max.waves, na.rm = T), ra.rm = T)
  results <- results %>%
    filter(iteration <= max.waves.dif)
  # I join epidemics with a separation lower than i.min.separation
  iteration <- start <- end <- n <- sum <- lend <- dif <- unite <- sunite <- x <- from <- to <- iteration.old <- NULL
  temp1 <- results %>%
    dplyr::select(iteration, start, end, n, sum) %>%
    dplyr::arrange(start) %>%
    dplyr::mutate(lend = lag(end), dif = (start - lend - 1), unite = (is.na(dif) | dif >= i.min.separation), sunite = cumsum(unite))
  results.united <- results %>%
    dplyr::mutate(iteration.old = iteration) %>%
    dplyr::inner_join(dplyr::select(temp1, iteration, sunite), by = "iteration") %>%
    dplyr::mutate(iteration = sunite) %>%
    dplyr::select(-sunite) %>%
    dplyr::arrange(iteration.old)
  data.plot.united <- data.plot %>%
    dplyr::mutate(iteration.old = iteration) %>%
    dplyr::inner_join(dplyr::select(temp1, iteration, sunite), by = "iteration") %>%
    dplyr::mutate(iteration = sunite) %>%
    dplyr::select(-sunite) %>%
    dplyr::arrange(x)
  n.parts <- max(temp1$sunite)
  if (n.parts>0){
    n <- rates.filled <- x <- y <- NULL
    p4[[1]] <- ggplot() +
      geom_line(data = data.plus, aes(x = n, y = rates.filled), color = "#A0A0A0", size = 1) +
      geom_point(data = data.plus, aes(x = n, y = rates.filled), color = "#A0A0A0", size = 1.5) +
      geom_point(data = data.plot.united, aes(x = x, y = y, color = factor(iteration)), size = 4) +
      scale_colour_manual(values = solpalette.full[1:n.parts], guide = guide_legend(nrow = 3)) +
      scale_x_continuous(breaks = axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
      scale_y_continuous(breaks = axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
      labs(title = paste0("Merged epidemics (separation: ", i.min.separation, ")"), x = "Week", y = "Data") +
      guides(color = guide_legend(title = "Iteration")) +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))
    # Now I separate each part for the lowest vale between epidemics, but using the smooth data to avoid irregular
    # low values between epidemics
    if (n.parts > 1) {
      temp2 <- temp1 %>%
        dplyr::group_by(sunite) %>%
        dplyr::summarise(start = min(start), end = max(end)) %>%
        dplyr::select(iteration = sunite, start, end) %>%
        dplyr::arrange(start) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(from = 1 + lag(end), to = start) %>%
        dplyr::slice(-1) %>%
        dplyr::select(from, to)
      cut.united <- data.frame()
      mediann <- NULL
      for (i in 1:NROW(temp2)) {
        cut.united <- rbind(
          cut.united,
          data.frame(
            iteration = i,
            n = data.plus %>%
              filter(n >= temp2$from[i] & n <= temp2$to[i]) %>%
              filter(rank(rates.smooth, ties.method = "min") == 1) %>%
              summarise(mediann = quantile(n, probs = 0.50, type = 3)) %>%
              pull(mediann)
          )
        )
      }
      n <- dummy <- part <- NULL
      data.united <- data.plus %>%
        dplyr::left_join(cut.united %>%
                           dplyr::select(n) %>%
                           dplyr::mutate(dummy = 1), by = "n") %>%
        dplyr::mutate(dummy = if_else(is.na(dummy), 0, 1), part = 1 + cumsum(dummy), season = 1000 + part) %>%
        dplyr::select(-dummy)
    } else {
      cut.united <- NULL
      data.united <- data.plus %>%
        dplyr::mutate(part = 1, season = 1000 + part)
    }
    n <- rates.filled <- x <- y <- NULL
    p4[[2]] <- ggplot() +
      geom_line(data = data.plus, aes(x = n, y = rates.filled), color = "#A0A0A0", size = 1) +
      geom_point(data = data.plus, aes(x = n, y = rates.filled), color = "#A0A0A0", size = 1.5) +
      geom_point(data = data.plot.united, aes(x = x, y = y, color = factor(iteration)), size = 4) +
      scale_colour_manual(values = solpalette.full[1:n.parts], guide = guide_legend(nrow = 3)) +
      scale_x_continuous(breaks = axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
      scale_y_continuous(breaks = axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
      labs(title = paste0("Merged epidemics (separation: ", i.min.separation, ")"), x = "Week", y = "Data") +
      guides(color = guide_legend(title = "Parts")) +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))
    if (n.parts > 1) p4[[2]] <- p4[[2]] + geom_vline(data = cut.united, aes(xintercept = n), color = "#000000", alpha = 0.5)
    # Now limit the length of each season to i.max.season.duration
    data.limited <- numeric()
    for (k in 1:n.parts) {
      temp1 <- data.united %>%
        filter(part == k)
      # temp2 <- data.plot.united %>%
      #   filter(iteration==k)
      temp2 <- results.united %>%
        filter(iteration==k) %>%
        arrange(iteration.old) %>%
        slice(1)
      xmin <- temp2$end-max.season.duration+1
      xmax <- max.season.duration+temp2$start-1
      xmin <- max(min(temp1$n), xmin)
      xmax <- min(max(temp1$n), xmax)
      temp1$rates.filled[temp1$n<xmin] <- NA
      temp1$rates.filled[temp1$n>xmax] <- NA
      temp3 <- percentage.added(temp1$rates.filled, max.season.duration)
      temp4 <- c(
        rep(NA, temp3$start - 1),
        rep(k, temp3$end - temp3$start + 1),
        rep(NA, NROW(temp1) - temp3$end)
      )
      data.limited <- c(data.limited, temp4)
      rm("temp1", "temp2", "temp3", "temp4")
    }
    
    season.original <- part <- season <- NULL
    data.united <- data.united %>%
      mutate(rates.filled.original = rates.filled, part.original = part, season.original = season) %>%
      mutate(rates.filled = ifelse(is.na(data.limited), NA, rates.filled), part = data.limited, season = ifelse(is.na(data.limited), NA, season.original))
    season <- yrweek <- week <- rates.orig <- part <- NULL
    season.desc <- data.united %>%
      dplyr::filter(!is.na(part)) %>%
      dplyr::group_by(season) %>%
      dplyr::summarise(from = min(yrweek), to = max(yrweek), duration = n()) %>%
      as.data.frame()
    names(season.desc) <- c("Season", "From", "To", "Duration")
    # See epidemic detected my mem algorithm
    data.united$epidemic <- NA
    for (i in 1:n.parts) {
      temp1 <- data.united %>%
        filter(part == i) %>%
        pull(rates.filled) %>%
        memtiming(...)
      temp2 <- data.united %>%
        filter(part == i) %>%
        summarise(minn = min(n), maxn = max(n))
      y <- rep(1, temp2$maxn - temp2$minn + 1)
      y[temp1$optimum.map[5]:(temp2$maxn - temp2$minn + 1)] <- 3
      y[temp1$optimum.map[4]:temp1$optimum.map[5]] <- 2
      data.united$epidemic[temp2$minn:temp2$maxn] <- y
    }
    temp1 <- data.united %>%
      filter(!is.na(part)) %>%
      group_by(season) %>%
      summarise(cut1 = min(n), cut2 = max(n), cut3 = mean(n))
    n <- rates.filled <- rates.filled.original <- x <- y <- cut1 <- cut2 <- cut3 <- epidemic <- NULL
    p5[[1]] <- ggplot() +
      geom_line(data = data.united, aes(x = n, y = rates.filled.original), color = "#A0A0A0", size = 1) +
      geom_point(data = data.united, aes(x = n, y = rates.filled.original), color = "#A0A0A0", size = 1.5, alpha = 0.75) +
      geom_point(data = data.united, aes(x = n, y = rates.filled), color = "#FFB401", size = 4, alpha = 0.75) +
      geom_point(data = data.plot.united, aes(x = x, y = y), color = "#800080", size = 4, alpha = 0.75) +
      scale_x_continuous(breaks = axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
      scale_y_continuous(breaks = axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
      labs(title = "Series separated in artificial seasons", x = "Week", y = "Data") +
      geom_vline(data = temp1, aes(xintercept = cut1), color = "#FF0000", alpha = 0.5) +
      geom_vline(data = temp1, aes(xintercept = cut2), color = "#40FF40", alpha = 0.5) +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))
    # temp2 <- axis.y.ticks[1] + diff(range(axis.y.ticks)) / 20
    # temp3 <- axis.y.ticks[1] + diff(range(axis.y.ticks)) / 15
    temp2 <- axis.y.range[1]
    temp3 <- axis.y.range[1]
    p5[[2]] <- ggplot() +
      geom_line(data = data.united, aes(x = n, y = rates.filled.original), color = "#A0A0A0", size = 1) +
      geom_point(data = data.united, aes(x = n, y = rates.filled.original), color = "#A0A0A0", size = 1.5, alpha = 0.75) +
      geom_point(data = subset(data.united, !is.na(epidemic)), aes(x = n, y = rates.filled.original, color = factor(epidemic, levels = 1:3, labels = c("Pre", "Epidemic", "Post"))), size = 4, alpha = 0.75) +
      scale_colour_manual(values = c("#00C000", "#800080", "#FFB401"), guide = guide_legend(nrow = 3)) +
      scale_x_continuous(breaks = axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
      scale_y_continuous(breaks = axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
      labs(title = "Series separated and MEM epidemics", x = "Week", y = "Data") +
      geom_segment(data = temp1, aes(x = cut1, xend = cut2, y = temp3, yend = temp3), color = "#0000CC", alpha = 0.75, size = 1, arrow = arrow(ends = "both", type = "closed", angle = "90", length = unit(5, "points"))) +
      geom_text(data = temp1, aes(x = cut3, y = temp2, label = season), color = "#0066CC", alpha = 0.75, vjust = 1) +
      # geom_vline(data=temp1, aes(xintercept=cut1), color="#FF0000", alpha=0.5) +
      # geom_vline(data=temp1, aes(xintercept=cut2), color="#40FF40", alpha=0.5) +
      guides(color = guide_legend(title = "Epidemic")) +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))
    data.final <- data.united %>%
      dplyr::filter(!is.na(part)) %>%
      dplyr::arrange(yrweek) %>%
      dplyr::group_by(season) %>%
      dplyr::mutate(week = 1:n()) %>%
      dplyr::ungroup() %>%
      dplyr::select(season, week, rates.orig) %>%
      tidyr::spread(season, rates.orig) %>%
      as.data.frame()
    rownames(data.final) <- data.final$week
    data.final <- data.final %>%
      dplyr::select(-week)
    
  }else{
    p4[[1]] <- ggplot()
    p4[[2]] <- ggplot()
    p5[[1]] <- ggplot()
    p5[[2]] <- ggplot()
    data.united <- data.frame()
    data.final <- data.frame()
    cut.united <- data.frame()
    season.desc <- NA
  }
  if (!is.na(i.output)) {
    outputdir <- file.path(getwd(), i.output)
    if (!dir.exists(outputdir)) dir.create(outputdir)
    ggsave(paste0(i.prefix, " - 1.1. Original Vs Smooth.png"), p1[[1]], width = 16, height = 9, dpi = 150, path = outputdir)
    ggsave(paste0(i.prefix, " - 1.2. Data to be used.png"), p1[[2]], width = 16, height = 9, dpi = 150, path = outputdir)
    # We plot each iteration to the stopping point and filter the results
    for (j in 1:max.waves) ggsave(paste0(i.prefix, " - 2.", j, ". Iteration (unordered) ", j, ".png"), p2[[j]], width = 16, height = 9, dpi = 150, path = outputdir)
    if (NROW(results)>0) for (j in 1:max.waves.dif) ggsave(paste0(i.prefix, " - 3.", j, ". Iteration (final) ", j, ".png"), p3[[j]], width = 16, height = 9, dpi = 150, path = outputdir)
    ggsave(paste0(i.prefix, "- 4.1. Merged epidemics separated.png"), p4[[1]], width = 16, height = 9, dpi = 150, path = outputdir)
    ggsave(paste0(i.prefix, "- 4.2. Merged epidemics separated plus cut points.png"), p4[[2]], width = 16, height = 9, dpi = 150, path = outputdir)
    ggsave(paste0(i.prefix, "- 5.1. Seasons separated.png"), p5[[1]], width = 16, height = 9, dpi = 150, path = outputdir)
    ggsave(paste0(i.prefix, "- 5.2. Seasons separated and MEM epidemics.png"), p5[[2]], width = 16, height = 9, dpi = 150, path = outputdir)
  }
  plots <- list(p1 = p1, p2 = p2, p3 = p3, p4 = p4, p5 = p5)
  list(data.final = data.final, data.united = data.united, data.plot.united = data.plot.united, cut.united = cut.united, season.desc = season.desc, plots = plots)
}
