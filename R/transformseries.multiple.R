#' divide a series in parts
#'
#' @keywords internal
#'
#' @importFrom ggplot2 ggplot ggsave geom_area geom_line geom_vline aes labs %+% element_text geom_point guide_legend scale_colour_manual scale_x_continuous scale_y_continuous theme
#' @importFrom stats loess predict
#' @importFrom dplyr %>% arrange mutate select filter group_by bind_rows if_else left_join pull slice summarise ungroup
#' @importFrom tidyr spread
#' @importFrom utils tail
#' @importFrom ggthemes solarized_pal
transformseries.multiple <- function(i.data, i.max.duration=30, i.max.waves=NA, i.param.1=0.028, i.param.2=0.015, i.separation=3, i.output=NA){
  yrweek <- season <- year <- week <- NULL
  data <- transformdata.back(i.data)$data %>%
    dplyr::arrange(yrweek) %>%
    dplyr::mutate(n=1:n()) %>%
    dplyr::select(-season, -year, -week)
  model.loess <- loess(rates ~ n, data, span=0.05)
  rates <- rates.loess <- NULL
  data.plus <- data %>%
    dplyr::mutate(rates.orig=rates,
                  rates.loess=predict(model.loess, newdata=data$n),
                  rates.filled=if_else(is.na(rates), rates.loess, rates)) %>%
    dplyr::select(-rates) %>%
    dplyr::filter(!is.na(rates.loess))
  if (!is.na(i.output)){
    outputdir<-file.path(getwd(), i.output)
    if (!dir.exists(outputdir)) dir.create(outputdir)
    axis.x.range.original <- range(data.plus$n)
    axis.x.otick <- optimal.tickmarks(axis.x.range.original[1], axis.x.range.original[2], 10, i.include.min = T, i.include.max = T)
    axis.x.range <- axis.x.otick$range
    axis.x.ticks <- axis.x.otick$tickmarks
    axis.x.labels <- data.plus$yrweek[axis.x.otick$tickmarks]
    axis.y.range.original <- range(c(data.plus$rates.orig, data.plus$rates.loess))
    axis.y.otick <- optimal.tickmarks(axis.y.range.original[1], axis.y.range.original[2], 10)
    axis.y.range <- axis.y.otick$range+diff(range(axis.y.otick$range))*0.025*c(-1, 1)
    axis.y.ticks <- axis.y.otick$tickmarks
    axis.y.labels <- axis.y.otick$tickmarks
    n <- rates.orig <- rates.loess <- NULL
    p1 <- ggplot(data.plus) +
      geom_line(aes(x=n, y=rates.orig), color="#0066CC", linetype=1, size=1) +
      geom_line(aes(x=n, y=rates.loess), color="#CC0066", linetype=1, size=1) +
      scale_x_continuous(breaks=axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
      scale_y_continuous(breaks=axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
      labs(title = "Series and loess", x = "Week", y = "Data") +
      ggthemes::theme_few() +
      theme(plot.title = element_text(hjust = 0.5))
    ggsave("Loess.png", p1,  width = 16, height = 9, dpi=150, path=outputdir)
  }
  if (is.na(i.max.waves)) i.max.waves<-2*NCOL(i.data)
  results <- data.frame()
  data.plot <- data.frame()
  data.temp <- data.plus
  difcumsumper<-1
  j=1
  while(difcumsumper>i.param.2 & j<i.max.waves+1){
    cat("iteration ", j, "\n")
    peradd<-as.data.frame(matrix(unlist(sapply(1:i.max.duration, percentage.added, i.data=data.temp$rates.filled)), ncol=6, byrow = T), stringsAsFactors = F)
    names(peradd)<-c("percentage","start","end","duration", "sum", "max")
    n.chosen=tail((1:i.max.duration)[peradd$percentage>i.param.1], 1)
    peradd.chosen<-data.frame(iteration=j, percentage.added(data.temp$rates.filled, n.chosen))
    results<-bind_rows(results, peradd.chosen)
    data.plot<-rbind(data.plot,
                     data.frame(iteration=j, x=peradd.chosen$start:peradd.chosen$end, y=data.temp$rates.filled[peradd.chosen$start:peradd.chosen$end], stringsAsFactors = F)
    )
    if (!is.na(i.output)){
      n <- rates.filled <- x <- y <- NULL
      p2 <- ggplot() +
        geom_line(data=data.plus, aes(x=n, y=rates.filled), color="#A0A0A0") +
        geom_point(data=data.plot, aes(x=x, y=y, color=factor(iteration)), size=4) +
        scale_colour_manual(values = colorRampPalette(solarized_pal()(8))(j), guide = guide_legend(nrow=2)) +
        scale_x_continuous(breaks=axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
        scale_y_continuous(breaks=axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
        labs(title = "Series and epidemics", x = "Week", y = "Data") +
        ggthemes::theme_few() +
        theme(plot.title = element_text(hjust = 0.5))
      ggsave(paste0("Iteration ",j,".png"), p2,  width = 16, height = 9, dpi=150, path=outputdir)
    }
    sum <- cumsumper <- NULL
    data.temp$rates.filled[peradd.chosen$start:peradd.chosen$end]<-NA
    results <- results %>%
      dplyr::mutate(sumcum=cumsum(sum), cumsumper=cumsum(sum)/sum(data.plus$rates.filled, na.rm=T)) %>%
      dplyr::mutate(difcumsumper= cumsumper - lag(cumsumper))
    if (j>1) difcumsumper<-results$difcumsumper[NROW(results)]
    j<-j+1
    cat(j,"-",difcumsumper,"\n")
  }
  iteration <- start <- end <- n <- sum <- lend <- dif <- unite <- sunite <- x <- from <- to <- NULL
  temp1 <- results %>%
    dplyr::select(iteration, start, end, n, sum) %>%
    dplyr::arrange(start) %>%
    dplyr::mutate(lend=lag(end), dif=(start-lend-1), unite=(is.na(dif) | dif>=i.separation), sunite=cumsum(unite))
  data.plot.united <- data.plot %>%
    dplyr::left_join(dplyr::select(temp1, iteration, sunite), by="iteration") %>%
    dplyr::mutate(iteration=sunite) %>%
    dplyr::select(-sunite) %>%
    dplyr::arrange(x)
  temp2 <- temp1 %>%
    dplyr::group_by(sunite) %>%
    dplyr::summarise(start=min(start), end=max(end)) %>%
    dplyr::select(iteration=sunite, start, end) %>%
    dplyr::arrange(start) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(from=1+lag(end), to=start-1) %>%
    dplyr::slice(-1) %>%
    dplyr::select(from, to)
  cut.united<- data.frame()
  for (i in 1:NROW(temp2)){
    cut.united<-rbind(cut.united,
                      data.frame(
                        iteration=i,
                        n=data.plus %>%
                          filter(n>=temp2$from[i] & n<=temp2$to[i]) %>%
                          filter(rank(rates.loess)==1) %>%
                          pull(n)
                      ))
  }
  n <- dummy <- part <- NULL
  data.united <- data.plus %>% 
    dplyr::left_join(cut.united %>%
                       dplyr::select(n) %>%
                       dplyr::mutate(dummy=1), by="n") %>%
    dplyr::mutate(dummy=if_else(is.na(dummy),0,1), part=1+cumsum(dummy), season=1000 + part) %>%
    dplyr::select(-dummy)
  if (!is.na(i.output)){
    n <- rates.filled <- x <- y <- NULL
    p3 <- ggplot() +
      geom_line(data=data.plus, aes(x=n, y=rates.filled), color="#A0A0A0") +
      geom_point(data=data.plot.united, aes(x=x, y=y, color=factor(iteration)), size=4) +
      scale_colour_manual(values = colorRampPalette(solarized_pal()(8))(j), guide = guide_legend(nrow=2)) +
      scale_x_continuous(breaks=axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
      scale_y_continuous(breaks=axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
      labs(title = "Series and epidemics", x = "Week", y = "Data") +
      geom_vline(data=cut.united, aes(xintercept=n), color="#A0A0A0", alpha=0.5) +
      ggthemes::theme_few() +
      theme(plot.title = element_text(hjust = 0.5))
    ggsave(paste0("Final.png"), p3,  width = 16, height = 9, dpi=150, path=outputdir)
  }
  season <- yrweek <- week <- rates.orig <- NULL
  season.desc <- data.united %>%
    dplyr::group_by(season) %>%
    dplyr::summarise(from=min(yrweek), to=max(yrweek)) %>%
    as.data.frame()
  names(season.desc) <- c("Season", "From", "To")
  data.final <- data.united %>%
    dplyr::arrange(yrweek) %>%
    dplyr::group_by(season) %>%
    dplyr::mutate(week=1:n()) %>%
    dplyr::ungroup() %>%
    dplyr::select(season, week, rates.orig) %>%
    tidyr::spread(season, rates.orig) %>%
    as.data.frame()
  rownames(data.final) <- data.final$week
  data.final <- data.final %>%
    dplyr::select(-week)
  list(data.final=data.final, data.united=data.united, data.plot.united=data.plot.united, cut.united=cut.united, season.desc=season.desc)
}