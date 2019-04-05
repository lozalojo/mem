#' divide a series in parts
#'
#' @keywords internal
#'
#' @importFrom ggplot2 ggplot ggsave geom_area geom_line geom_vline aes labs %+% element_text geom_point guide_legend scale_colour_manual scale_x_continuous scale_y_continuous theme geom_segment geom_text unit arrow guides theme_light
#' @importFrom stats loess predict
#' @importFrom dplyr %>% arrange mutate select filter group_by bind_rows if_else left_join inner_join pull slice summarise ungroup
#' @importFrom tidyr spread
#' @importFrom utils tail
transformseries.multiple <- function(i.data,
                                     i.max.epidemic.duration = NA,
                                     i.max.season.duration = NA,
                                     i.waves = NA,
                                     i.param.1 = 3,
                                     i.param.2 = 2,
                                     i.min.separation = 1,
                                     i.output = NA,
                                     i.prefix = "Multiple waves",
                                     i.force.loess = F,
                                     ...) {
  p1 <- list()
  p2 <- list()
  p3 <- list()
  p4 <- list()
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
  yrweek <- season <- year <- week <- NULL
  data <- transformdata.back(i.data)$data %>%
    dplyr::arrange(yrweek) %>%
    dplyr::mutate(n = 1:n()) %>%
    dplyr::select(-season, -year, -week)
  model.loess <- loess(rates ~ n, data, span = 0.05)
  p.model.loess <- predict(model.loess, newdata = data$n)
  p.model.loess[p.model.loess<0] <- 0
  rates <- rates.loess <- NULL
  data.plus <- data %>%
    dplyr::mutate(
      rates.orig = rates,
      rates.loess = p.model.loess,
      rates.filled = if_else(is.na(rates), rates.loess, rates)
    ) %>%
    dplyr::select(-rates) %>%
    dplyr::arrange(n)
  if (i.force.loess) data.plus$rates.filled <- data.plus$rates.loess
  # Plot the original series, the loess and the data that will be used

  axis.x.range.original <- range(data.plus$n, na.rm = T)
  axis.x.otick <- optimal.tickmarks(axis.x.range.original[1], axis.x.range.original[2], 10, i.include.min = T, i.include.max = T)
  axis.x.range <- axis.x.otick$range
  axis.x.ticks <- axis.x.otick$tickmarks
  axis.x.labels <- data.plus$yrweek[axis.x.otick$tickmarks]
  axis.y.range.original <- range(c(data.plus$rates.orig, data.plus$rates.loess), na.rm = T)
  axis.y.otick <- optimal.tickmarks(axis.y.range.original[1], axis.y.range.original[2], 10)
  axis.y.range <- axis.y.otick$range + diff(range(axis.y.otick$range, na.rm = T)) * 0.025 * c(-1, 1)
  axis.y.ticks <- axis.y.otick$tickmarks
  axis.y.labels <- axis.y.otick$tickmarks
  n <- rates.orig <- rates.loess <- NULL
  p1[[1]] <- ggplot(data.plus) +
    geom_line(aes(x = n, y = rates.orig), color = "#0066CC", linetype = 1, size = 0.75) +
    geom_point(aes(x = n, y = rates.orig), color = "#0066CC", size = 1.5) +
    geom_line(aes(x = n, y = rates.loess), color = "#CC0066", linetype = 1, size = 0.75) +
    geom_point(aes(x = n, y = rates.loess), color = "#CC0066", size = 1.5) +
    scale_x_continuous(breaks = axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
    scale_y_continuous(breaks = axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
    labs(title = "Series and loess", x = "Week", y = "Data") +
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
  for (j in 1:max.waves) {
    peradd <- as.data.frame(matrix(unlist(sapply(1:max.epidemic.duration, percentage.added, i.data = data.temp$rates.filled)), ncol = 6, byrow = T), stringsAsFactors = F)
    names(peradd) <- c("percentage", "start", "end", "duration", "sum", "max")
    n.chosen <- head((1:max.epidemic.duration)[peradd$percentage < (param.1 / 100)], 1) - 1
    peradd.chosen <- data.frame(iteration = j, percentage.added(data.temp$rates.filled, n.chosen))
    sum <- cumsumper <- totsum <- difcumsumper <- sumcum <- NULL
    results <- results %>%
      bind_rows(peradd.chosen) %>%
      dplyr::mutate(sumcum = cumsum(sum), totsum=sum(data.plus$rates.filled, na.rm = T), cumsumper = sumcum / totsum) %>%
      dplyr::mutate(difcumsumper = cumsumper - lag(cumsumper))
    results$difcumsumper[1] <- results$cumsumper[1]
    data.plot <- data.plot %>%
      bind_rows(
        data.frame(iteration = j, x = peradd.chosen$start:peradd.chosen$end, y = data.temp$rates.filled[peradd.chosen$start:peradd.chosen$end], stringsAsFactors = F) %>%
          inner_join(results %>%
                       select(iteration, difcumsumper), by="iteration") %>%
          mutate(iteration.label=paste0("Iter: ", sprintf("%02d", iteration),", Per: ", sprintf("%3.2f", 100*difcumsumper))))
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
                   select(iteration, percentage), by="iteration") %>%
      mutate(label=sprintf("%3.2f", 100*percentage))
    
    n <- rates.filled <- x <- y <- iteration.label <- NULL
    p2[[j]] <- ggplot() +
      geom_line(data = data.plus, aes(x = n, y = rates.filled), color = "#A0A0A0", size = 1) +
      geom_point(data = data.plus, aes(x = n, y = rates.filled), color = "#A0A0A0", size = 1.5) +
      geom_point(data = data.plot, aes(x = x, y = y, color = factor(iteration.label)), size = 4) +
      geom_point(data = last.point, aes(x = x, y = y), color="#FFFFFF", size = 2) + 
      geom_text(data = last.point, aes(x = x, y = y, label=label), vjust=1) +
      scale_colour_manual(values = colorRampPalette(solpalette)(j), guide = guide_legend(nrow = 3)) +
      scale_x_continuous(breaks = axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
      scale_y_continuous(breaks = axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
      labs(title = paste0("Iteration #", j), x = "Week", y = "Data") +
      guides(color = guide_legend(title = paste0("Iteration (Lim: ", sprintf("%3.2f",param.2),")"))) +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))
    data.temp$rates.filled[peradd.chosen$start:peradd.chosen$end] <- NA
  }
  # The stopping point is determined by param.2
  max.waves.dif <- max(min.waves, min(results$iteration[results$difcumsumper < (param.2 / 100)][1] - 1, max.waves, na.rm = T), ra.rm = T)
  results <- results %>%
    filter(iteration <= max.waves.dif)
  # I join epidemics with a separation lower than i.min.separation
  iteration <- start <- end <- n <- sum <- lend <- dif <- unite <- sunite <- x <- from <- to <- NULL
  temp1 <- results %>%
    dplyr::select(iteration, start, end, n, sum) %>%
    dplyr::arrange(start) %>%
    dplyr::mutate(lend = lag(end), dif = (start - lend - 1), unite = (is.na(dif) | dif >= i.min.separation), sunite = cumsum(unite))
  data.plot.united <- data.plot %>%
    dplyr::inner_join(dplyr::select(temp1, iteration, sunite), by = "iteration") %>%
    dplyr::mutate(iteration = sunite) %>%
    dplyr::select(-sunite) %>%
    dplyr::arrange(x)
  n <- rates.filled <- x <- y <- NULL
  p3[[1]] <- ggplot() +
    geom_line(data = data.plus, aes(x = n, y = rates.filled), color = "#A0A0A0", size = 1) +
    geom_point(data = data.plus, aes(x = n, y = rates.filled), color = "#A0A0A0", size = 1.5) +
    geom_point(data = data.plot.united, aes(x = x, y = y, color = factor(iteration)), size = 4) +
    scale_colour_manual(values = colorRampPalette(solpalette)(j), guide = guide_legend(nrow = 3)) +
    scale_x_continuous(breaks = axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
    scale_y_continuous(breaks = axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
    labs(title = paste0("Merged epidemics (separation: ", i.min.separation, ")"), x = "Week", y = "Data") +
    guides(color = guide_legend(title = "Iteration")) +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5))
  # Now I separate each part for the lowest vale between epidemics, but using the loess data to avoid irregular
  # low values between epidemics
  n.parts <- max(temp1$sunite)
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
            filter(rank(rates.loess, ties.method = "min") == 1) %>%
            summarise(mediann=quantile(n, probs=0.50, type=3)) %>%
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
  p3[[2]] <- ggplot() +
    geom_line(data = data.plus, aes(x = n, y = rates.filled), color = "#A0A0A0", size = 1) +
    geom_point(data = data.plus, aes(x = n, y = rates.filled), color = "#A0A0A0", size = 1.5) +
    geom_point(data = data.plot.united, aes(x = x, y = y, color = factor(iteration)), size = 4) +
    scale_colour_manual(values = colorRampPalette(solpalette)(j), guide = guide_legend(nrow = 3)) +
    scale_x_continuous(breaks = axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
    scale_y_continuous(breaks = axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
    labs(title = paste0("Merged epidemics (separation: ", i.min.separation, ")"), x = "Week", y = "Data") +
    guides(color = guide_legend(title = "Parts")) +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5))
  if (n.parts > 1) p3[[2]] <- p3[[2]] + geom_vline(data = cut.united, aes(xintercept = n), color = "#000000", alpha = 0.5)
  # Now limit the length of each season to i.max.season.duration
  temp4 <- numeric()
  for (k in 1:n.parts) {
    temp1 <- data.united %>%
      filter(part == k) %>%
      pull(rates.filled)
    temp2 <- percentage.added(temp1, max.season.duration)

    temp3 <- c(
      rep(NA, temp2$start - 1),
      rep(k, temp2$end - temp2$start + 1),
      rep(NA, length(temp1) - temp2$end)
    )

    temp4 <- c(temp4, temp3)
  }
  season.original <- part <- season <- NULL
  data.united <- data.united %>%
    mutate(rates.filled.original = rates.filled, part.original = part, season.original = season) %>%
    mutate(rates.filled = ifelse(is.na(temp4), NA, rates.filled), part = temp4, season = ifelse(is.na(temp4), NA, season.original))
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
  p4[[1]] <- ggplot() +
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
  p4[[2]] <- ggplot() +
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
  if (!is.na(i.output)) {
    outputdir <- file.path(getwd(), i.output)
    if (!dir.exists(outputdir)) dir.create(outputdir)
    ggsave(paste0(i.prefix, "-1.1. Original Vs Loess.png"), p1[[1]], width = 16, height = 9, dpi = 150, path = outputdir)
    ggsave(paste0(i.prefix, "-1.2. Data to be used.png"), p1[[2]], width = 16, height = 9, dpi = 150, path = outputdir)
    # We plot each iteration to the stopping point and filter the results
    for (j in 1:max.waves.dif) ggsave(paste0(i.prefix, "-2.", j, ". Iteration ", j, ".png"), p2[[j]], width = 16, height = 9, dpi = 150, path = outputdir)
    ggsave(paste0(i.prefix, "-3.1. Merged epidemics.png"), p3[[1]], width = 16, height = 9, dpi = 150, path = outputdir)
    ggsave(paste0(i.prefix, "-3.2. Merged epidemics separated.png"), p3[[2]], width = 16, height = 9, dpi = 150, path = outputdir)
    ggsave(paste0(i.prefix, "-4.1. Seasons separated.png"), p4[[1]], width = 16, height = 9, dpi = 150, path = outputdir)
    ggsave(paste0(i.prefix, "-4.2. Seasons separated and MEM epidemics.png"), p4[[2]], width = 16, height = 9, dpi = 150, path = outputdir)
  }
  plots <- list(p1 = p1, p2 = p2, p3 = p3, p4 = p4)
  list(data.final = data.final, data.united = data.united, data.plot.united = data.plot.united, cut.united = cut.united, season.desc = season.desc, plots = plots)
}
