# Service Costing Helpers
# Primarily for plotting

costing_boxPlot <- function(costing_data, sites=NA, service_lines=NA, sds=3, title='Service Costs',
                            scatter=T, jitter.width = .3, jitter.height=.01, scatter.alpha=.2, FUN=mean) {
  # make a box plot
  results <- costing_data[costing_data$Total.Visit.Cost < (mean(costing_data$Total.Visit.Cost, na.rm=T) + sds * sd(costing_data$Total.Visit.Cost, na.rm=T)),]
  if (!is.na(sites)) {
    results <- results[results$facility %in% sites,]
    
  }
  if (!is.na(service_lines)) {
    results <- results[results$Service.Line %in% service_lines,]
  }
  
  
  
  if (length(service_lines) > 4) {
    service_lines <- sapply(list(1:4, 5:length(service_lines)), function(x) paste0(service_lines[x], collapse=', '))
    
  }
  else {
    service_lines <- paste0(service_lines, collapse=', ')
  }
  service_lines <- paste(paste0(service_lines, '\n'), collapse="")
  
  sl_title <- ifelse(is.na(service_lines), 'All Service Lines', service_lines)
  results$facility <- as.factor(results$facility)
  plot <- ggplot(results, aes(facility, Total.Visit.Cost, group=facility, colour=facility, fill=facility)) + 
    geom_boxplot(alpha=.5) + theme_bw() +
    ggtitle(paste0(title,": ", sl_title, "(N=",nrow(results),')')) + xlab('Facility') + ylab('Visit Cost') + guides(guide_legend("Facility")) + ylim(0, max(results$Total.Visit.Cost)) +
    plotElements
  
  if (scatter == T) {
    plot <- plot + geom_jitter(width = jitter.width, height=jitter.height, alpha=scatter.alpha)
    
  }
  
  summary <- costing_summary(results, fun=FUN)
  return(list('plot'= plot, 'table' = summary$summary, 'model' = summary$model))
}

costing_summary <- function(costing_data, fun=mean) {
  # summarize data by facility and service line
  costing_data <- costing_data[!is.na(costing_data$Total.Visit.Cost),]
  summary <- aggregate(costing_data$Total.Visit.Cost, by=list(costing_data$facility, costing_data$Service.Line), fun)
  
  summary$x <- round(summary$x, 2)

  summary <- as.data.frame(dcast(summary, Group.2 ~ Group.1, value.var = 'x'))
  rownames(summary) <- summary$Group.2
  
  
  
  summary <- summary[,-1, drop=F]  
  
  counts <- as.data.frame.matrix(table(costing_data$Service.Line, costing_data$facility))
  sites <- c(paste(names(summary), "($)"), paste(names(counts), '(N)'))
  
  summary <- cbind(summary, counts)
  names(summary) <- sites
  summary <- summary[,order(names(summary))]
  # linear model
  
  fit <- lm(Total.Visit.Cost ~ facility, costing_data)
  
  
  return(list('summary' = summary, 'model' = fit))
}





