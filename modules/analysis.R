library(ggplot2)
library(reshape2)

## Analysis Functions ----------------------------------------------------------
topDiagsSite <- function(df, n=10) {
  # perform all of the same calculations as topDiags, but this time for each site
  # in the data set.  returns a nested list of topDiags options based on filtered 
  # data for each
  # Ex.
  # > a <- topDiagsSite(df, n=5)
  # returns the top 5 diagnoses for all sites combined, plus each individual site
  # call individual site results using their name
  # > a$Belladere
  # returns plot specific to belladere and respective table
  
  all <- list(topDiags(df, number= n,  paste("Top",n,"Diagnoses - All Sites")))
  names(all) <- 'all'
  diags <- all
  sites <- unique(df$facility)
  
  for (s in sites[!is.na(sites)]) {
    d <-
      list(topDiags(df[df$facility == s,], number= n, paste("Top",n,"Diagnoses -",s)))
    names(d) <- s
    diags <- append(diags, d)
  }
  return(diags)
}

topDiags <- function(df, number = 10, title=NULL) {
  # find the top diagnoses overall and by site with several important statistics
  # based on the top number of diagnoses
  # calculates wait time, consulation time, number of M/F, as well as the most common
  # lab and rx for each diagnosis
  # Ex.
  # > topDiags(df, number=5) 
  # returns summary info for the top 5 most commonly occuring diagnoses
  
  
  # make a cross tab of diagnosis by sex
  overallDiags <-
    as.data.frame(table(df$diagnosis, as.character(df$sex))) 
  # this will make it look normal with two columns M and F and rows of diagnosis
  overallDiags <- dcast(overallDiags, Var1 ~ Var2, value.var = 'Freq')
  # add a total column 
  overallDiags <-
    cbind(overallDiags, 'Total' = rowSums(overallDiags[,-1])) # -1 to leave out diagnosis column
  # order by most frequent first
  overallDiags <-
    overallDiags[order(overallDiags$Total, decreasing = TRUE),]
  # take the total of the remaining diagnoses that are not in the top N stated with number parameter
  others <- c(paste0('Remaining Diagnoses (',nrow(overallDiags[(number + 1):nrow(overallDiags),]),")"),
                  rbind(colSums(overallDiags[(number + 1):nrow(overallDiags),-1]))
  )
  # order total first then F and M, name columns so they make more sense
  others <- others[c(1,4,2,3)]
  overallDiags <- overallDiags[1:number,c(1,4,2,3)]
  names(overallDiags) <- c('Diagnosis','Total', 'F', 'M')
  names(others) <- c('Diagnosis','Total', 'F', 'M')
  
  # let's find avgs and most common values for tests and rx
  # define the columns we want
  tests <- c('lab1', 'lab2', 'lab3', 'lab4', 'lab5')
  prescriptions <- c('rx1', 'rx2', 'rx3', 'rx4', 'rx5')
  
  # new result table
  testTable <- data.frame(matrix(ncol=3, nrow=0)) # makes a blank table with three columns and 0 rows
  names(testTable) <- c('facility', 'diagnosis', 'test')
  
  # take facility, diagnosis, test and rx column and place in new results table 
  for (i in 1:5) { # we have 5 different columns, so we'll do this 5 times. i becomes 1 through 5
    subTable <- df[,c('facility','diagnosis', tests[i], prescriptions[i])]
    names(subTable) <- c('facility', 'diagnosis', 'test', 'prescription')
    testTable <- rbind(testTable, subTable)
  }
  
  out <- data.frame(matrix(nrow=0, ncol=6))
  # this will calculate averages and then bind the most common lab each time
  for (diag in overallDiags$Diagnosis) {
    # subset for diagnosis 
    sub <- df[df$diagnosis == diag,]
    
    # most frequent rx and tests for subset diag
    rx <- table(testTable$prescription[testTable$diagnosis == diag])
    test <- table(testTable$test[testTable$diagnosis == diag])
    
    # name of most frequent lab and rx
    lab <- names(test[order(test, decreasing=TRUE)])[1]
    prescription <- names(rx[order(rx, decreasing=TRUE)])[1]
    if (is.null(lab)) {lab <- 'None'}
    if (is.null(prescription)) {prescription <- 'None'}
    
    # stick it all together into one table
    out <- rbind(out, 
                 cbind("Avg. Age" = mean(sub$age, na.rm=TRUE), # Avg Age
                'Avg. Consult Time' = mean(sub$consultation_time, na.rm=TRUE),
                'Avg. # Labs' = mean(sub$num_tests, na.rm=TRUE),
                'Avg. # Rx' = mean(sub$num_rx, na.rm=TRUE),
                'Most Common Lab' = lab, # finds max freq for tests and returns the name
                'Most Common Rx' =  prescription # finds max lab freq and returns name
                      )
                )
  }
  
  # let's do it one last time for the rest of the diagnoses not included
  # most frequent rx and tests for all other diagnoses NOT in the top N
  rx <- table(testTable$prescription[!(testTable$diagnosis %in% overallDiags$Diagnosis)])
  test <- table(testTable$test[!(testTable$diagnosis %in% overallDiags$Diagnosis)])
  
  # name of most frequent lab and rx
  lab <- names(test[order(test, decreasing=TRUE)])[1]
  prescription <- names(rx[order(rx, decreasing=TRUE)])[1]
  if (is.null(lab)) {lab <- 'None'}
  if (is.null(prescription)) {prescription <- 'None'}
  
  sub <- df[!(df$diagnosis %in% overallDiags$Diagnosis),]
  
  
  out <- rbind(out, 
               cbind("Avg. Age" = mean(sub$age, na.rm=TRUE), # Avg Age
                     'Avg. Consult Time' = mean(sub$consultation_time, na.rm=TRUE),
                     'Avg. # Labs' = mean(sub$num_tests, na.rm=TRUE),
                     'Avg. # Rx' = mean(sub$num_rx, na.rm=TRUE),
                     'Most Common Lab' = lab, # finds max freq for tests and returns the name
                     'Most Common Rx' =  prescription # finds max lab freq and returns name
               )
  )
  
  # R is treating these as factors, so we need to convert in order to attach
  # the remaining "others" 
  overallDiags$Diagnosis <- as.character(overallDiags$Diagnosis) 
  overallDiags <- rbind(overallDiags, others)
  # stick them all together now  
  overallDiags <- cbind(overallDiags, out)
  
  overallDiags$Diagnosis <- factor(overallDiags$Diagnosis, levels=overallDiags$Diagnosis)
  
  overallDiags[,2:8] <- apply(overallDiags[,2:8],2, function(a) round(as.numeric(a), 1))
  
  
  
  
  plot <- ggpie(overallDiags, 'Diagnosis', 'Total') +
    ggtitle(title) + theme(plot.title = element_text(face = "bold", size =
                                                       26),
                           panel.background = element_blank())
  
  return(list('plot' = plot, 'table' = overallDiags))
}


patientVisits <- function(df, by = 'week', title = "Patient Visits", legend = "Site", smooths=FALSE, detail=NA) {
  # create a summary plot and table of visit information across time
  # by will determine level of aggregation. supports "week" or "month", although could also do "day"
  # smooths=TRUE will use a LOESS plot to create prettier lines across time
  # if detail is set to "Totals" it will sum the totals for the table output regardless of aggregation
  # specified
  # Ex.
  # > patientVisits(df, by='month', smooths=TRUE)
  # will return a table a plot aggregated by month with smooth lines across the history of the dataset
  
  patients <- data.matrix(table(df[,by], df$facility)) 
  # find the count of records by facility for 
  # our stated aggregation level
  
  
  patients.m <- melt(patients) # the melt function is used to convert this matrix into a 3 column table
  # suitable for graphing with ggplot
  
  if (by == 'month') { # this will make ggplot think everythign is on the first of the month and make the
    # output prettier
    patients.m$Var1 <- paste0(patients.m$Var1, '-01')
  }
  
  patients.m$Var1 <- as.Date(patients.m$Var1, origin='1970-01-01') # make sure ggplot knows this is a date
  
  # this will be for exporting the table
  rownames(patients) <- as.character(as.Date(as.numeric(rownames(patients)), origin='1970-01-01'))
  
  if (!is.na(detail) & detail == "Totals") {
    patients <- t(data.matrix(colSums(patients)))
    rownames(patients) <- 'Totals'
    
  }

  
  # now let's build the plot
  plot <- ggplot(patients.m, # using our melted, 3 column table 
                 aes(Var1, value, group = Var2, colour = Var2), # Var1 is our x, value is y, group based on Var2 
                 alpha = .8) # Transparency level
  
  if (smooths == TRUE) { # if smooths is true do smooth lines
    plot <- plot + geom_smooth(se=FALSE, size=.7) # se=FALSE hides the standard error of the estimates
  }
  else { # otherwise add a layer of straight lines
    plot <- plot + geom_line(size=.7)
  }
  
  # basic plot is built, now add window dressings
  plot <- plot + xlab("Date") + ylab('Number of Visits') +
    ggtitle(title) + guides(colour = guide_legend(legend)) +
    plotElements + theme(
      plot.title = element_text(face = "bold", size = 26),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 14),
      legend.position = "right"
    ) + ylim(0,max(patients.m$value) + 5) 
  
  return(list('table' = patients, 
              'plot' = plot)
         )
}

calcDensity <-
  function(df, column = 'age', title = NULL, legend = "Site", ylab = 'Proportion', xlab =
             NULL, linePlot = "Median", leftBound = 0, rightBound = NULL) {
    # Create a density graph and summary statistic table based on the supplied numeric column.
    # Graph displayed is based on count of the values supplied and not probability. Defaults to 
    # age column if nothing is supplied and will group by the facility column.
    # linePlot specifies what statistic to plot on the density lines.  left and right bound objects
    # only modify the x axis display bounds, but not the underlying data. 
    # Ex.
    # > calcDensity(df, column='wait_time') 
    # where df is the cleaned data set, will return density graph grouped by facility of wait_time
    # Ex.
    # > calcDensity(df, column='wait_time', rightBound=40)
    # returns same graph and table, but the x axis will end at 40. 
    
    df$column <- df[,column] # whatever column want to analyze will be duplicated here for graphing
    plot <- ggplot(df)  + # make a ggplot object based on the df object supplied
      geom_density( # create a density graph 
        size = .6,alpha = .3, aes( # set line size and transparency metrics
          x = column, #y = ..count.., # x axis determined by column. y commented out to return density proportions again
          group = facility, colour = facility, fill = facility # aesthetic coloring settings allbased on facility
        )
      ) +
      guides(colour = guide_legend(legend), fill = guide_legend(legend)) # unify the legend labels
    
    info <- ggplot_build(plot) # extract some of the information used to draw the plot
    colors <- as.data.frame(unique(info$data[[1]]$colour)) # pull out color information for the group (facility)
    sites <- names(table(df$facility)) # find unique facility names, ideally colors should match up
    sites <- sites[unique(info$data[[1]]$group)] # now check to make sure all of the facilities in the dataset match the graph set
    colors$site <- sites[order(sites)] # attached the site name to the associated color in the graph
    names(colors) <- c('color', 'site') # name the columns so it's easier to use
    
    # did we lose anything?  let's still add that to the table
    # sometimes if a site is missing too much data, ggplot will exclude it
    # let's keep it in for the summary statistic table. 
    if (length(sites) < length(unique(df$facility))) {
      missingSites <- unique(df$facility)
      missingSites <- missingSites[!(missingSites %in% sites)] # any site that is not in the sites we just assigned colors to
      colors <- rbind(colors,
                      cbind('color'= 'grey', 'site' = missingSites) # add that site(s) to the table
                      )
    }
    

    # now we'll create the summary statistic dataframe
    outTable <- data.frame(matrix(nrow = 0, ncol = 8)) 
    
    # read each row of the facilities in the table we just created above (colors)
    for (s in 1:nrow(colors)) {
      sums <- # calculate the summary statistics
        t(as.matrix(summary(df[,column][df$facility == colors$site[s]], na.rm =
                              TRUE)))
      if (ncol(sums) < 7) { # if there are NA values ignored then there will be 7 columns, if not only 6
        sums <- cbind(sums, 0) # add the extra column so we can easily combine with the larger table
      }
      
      stdDev <- # calc standard deviation of the column for specific site
        round(sd(df[,column][df$facility == colors$site[s]], na.rm = TRUE),3)
      count <- # total records for specific site
        length(df[,column][df$facility == colors$site[s]])
      
      # stick all of these things together as a row in our summary table
      outTable <- rbind(outTable, 
                        unname(cbind(count,stdDev,  sums)) # unname removes column names, stick count, stddev, and other stats together
                        )
    }
    
    outTable <- cbind(colors, outTable) # join the original colors table and summary table together based on site name
    names(outTable) <- # rename the columns
      c(
        'color', 'Site',"Total Records", "Std. Dev.", 'Min', '1st Quartile', 'Median', 'Mean', "3rd Quartile", 'Max', "NAs Removed"
      )
    
    # backup the table in case there were some sites removed by ggplot, this will be the output table
    # but we still need the original for adding the dots to mark mean, median, etc. 
    # if we use the one with the removed sites, it'll fail. 
    # Ex.- HSN may have only 2 records. ggplot removes that for graphing purposes
    # if we leave that in the table to add the dots, it'll freak out, so we'll make a separate table
    # for that process by removing HSN from the table. 
    # outTable is used for dot graphing, finalTable is what you see at the end.
    
    finalTable <- outTable # this is the backup
    outTable <- outTable[outTable$Site %in% sites,] # this is only the sites that were graphed by ggplot
    
    for (s in 1:nrow(outTable)) { # go through each site again
      # we'll calculate where to place the dot based on the linePlot argument.  This can be any of the 
      # columns of the summary table
      
      xpoint <- outTable[,linePlot][s] # if linePlot is "Median" this will return the median for the specific site
      y <- info$data[[1]] 
      y <- y[y$colour == outTable$color[s],] # filter the plot data down to just that specific site
      ypoint <- y$count[which.min(abs(y$x - xpoint))] # y value for the nearest x point to our linePlot stat
      plot <- # Add the point to the graph using the approriate color
        plot + geom_point(x = xpoint, y = ypoint, colour = outTable$color[s], size=3.5, alpha=.5)
    }
    
    rownames(finalTable) <- finalTable$Site 
    
    if (is.null(rightBound)) {
      rightBound <- max(df$column, na.rm=TRUE) + 5
    }
    
    return(list(
      'plot' = plot +  xlim(c(leftBound, rightBound)) + ggtitle(title) + xlab(xlab) + ylab(ylab) + plotElements, # final plot
      'table' = finalTable[,-c(1:2)] # final table minus the color and site columns (we added site names as rownames)
    ))
  }

sexDist <- function(df, title = 'Sex Distribution by Site', percents=FALSE) {
  # find either the count or percent breakdown of sex in the dataset
  # uses the sex column in the supplied df object
  # set percents=TRUE to find the percentages, otherwise defaults to counts
  # outputs a barchart and summary table by facility
  # Ex.
  # > sexDist(df, percents=TRUE)
  # returns barchart with percentages and summary table
  
  
  df$sex <- as.character(df$sex) # r thinks of this as a factor which causes some problems for output
  sex <- table(df$sex, df$facility) # make a cross tab table counting sex vs facility
  sex <- data.matrix(sex[c('F',"M"),]) # only take the non-empty counts (if sex is blank this will ignore those counts)
  sex <- rbind(sex, 'Total' = colSums(sex)) # add a total number for the 3rd row
  
  if (percents == TRUE) {
    # for each column (facility), calculate the percent share of the total
    for (col in 1:ncol(sex)) {
      sex[,col] <- round(sex[,col]/sex[3,col],3) *100
    }
    ylab = "Percent" # set the label to percent
  }
  else {
    ylab = 'Count' # if percents is false, we don't need to do any other calculations, just change the y label
  }
  
  sex.m <- melt(sex[-3,]) # create our 3 column dataframe for graphing
  
  if (length(unique(df$facility)) == 1) {
    # need to do some rearranging if there's only one facility
    sex.m$Var2 <- unique(df$facility)
    sex.m$Var1 <- rownames(sex.m)
    
    colnames(sex) <- unique(df$facility)
  }
  

  # build the plot
  plot <- ggplot(sex.m, aes(Var2, value, group=Var1, colour=Var1, fill=Var1)) + 
    geom_bar(stat='identity',position='stack', alpha=.6) + # stacked bars will be our output layer
    # window dressings
    xlab('Site') + ylab(ylab) + # axis labels
    guides(fill=guide_legend('Sex'), colour=guide_legend('Sex')) + 
    plotElements +
    theme(# Legend settings
      legend.text = element_text(size=14),
      legend.title = element_text(size=14),
      legend.position="right")
  
  return(list('table' = sex, 
              'plot' = plot)
         )
}



topRx <- function(df, top=5, position='dodge', title=NULL, percents=TRUE, labels=FALSE) {
  # write a bar chart of the top prescriptions and respective table
  # position determins if bars are stacked or side by side, use 'dodge' for side by side (ggplot standard)
  # top determines how many prescriptions to include in plot
  # Ex. 
  # > topRx(df, top=10, position='dodge')
  # returns top 10 prescriptions for data set side by side
  
  # these are the columns we want to look at in the data set
  # we need to combine them into one to get real counts of what's happening
  prescriptions <- c('rx1', 'rx2', 'rx3', 'rx4', 'rx5')
  
  # new result table
  analysisTable <- data.frame(matrix(ncol=2, nrow=0)) # makes a blank table with three columns and 0 rows
  names(analysisTable) <- c('facility', 'prescriptions')
  
  # take facility, rx column and place in new results table 
  for (i in 1:5) { # we have 5 different columns, so we'll do this 5 times. i becomes 1 through 5
    subTable <- df[,c('facility', prescriptions[i])]
    names(subTable) <- c('facility', 'prescription')
    analysisTable <- rbind(analysisTable, subTable)
  }
  
  # now all of our prescriptions are in one column next to a facility column. we can make some counts
  # of where the prescriptions are happening now based on site
  
  rx <- melt(table(analysisTable$facility, analysisTable$prescription)) # make the cross table
  total <- as.data.frame(table(analysisTable$prescription))
  names(total) <- c('Var2', 'value')
  rx <- rbind(rx, cbind.data.frame('Var1' = 'All Facilities', total))
  rx <- rx[order(rx$value, decreasing=TRUE),] # orderthem by descreasing value
  
  # now let's cut the number of prescriptions we have by the top parameter
  rxFinal <- data.frame(matrix(nrow=0, ncol=3)) # this is our new table we'll build
  
  for (site in unique(rx$Var1)) { # look at each site in the data
    tmp <- rx[rx$Var1 == site,] # filter to just those prescriptions
    tmp <- tmp[order(tmp$value, decreasing=TRUE),] # count them and order by decreasing value
    tmp <- tmp[1:top,] # cut based on the top value
    rxFinal <- rbind(rxFinal, # stick it at the bottom of our final data set
                      tmp
    )
  }
  rxFinal$Var2 <- as.character(rxFinal$Var2)
  tmp <- rx[rx$Var1 == 'All Facilities',] # filter to just those prescriptions
  tmp_others <- tmp[order(tmp$value, decreasing=TRUE),] # count them and order by decreasing value
  tmp_others <- tmp_others[tmp_others$Var1 == 'All Facilities' & tmp_others$Var2 %in% rxFinal$Var2,]
  tmp_others <- tmp_others[!(tmp_others$Var2 %in% rxFinal$Var2[rxFinal$Var1 == 'All Facilities']),]
  
  rxFinal <- rbind(rxFinal, # stick it at the bottom of our final data set
                   tmp_others
  )
  
  
  ptCounts <- as.data.frame(table(df$facility), stringsAsFactors = F)
  ptCounts <- rbind.data.frame(ptCounts, c('All Facilities', nrow(df)))
  ptCounts$Freq <- as.numeric(ptCounts$Freq)
  if (percents == TRUE) {
    for (n in 1:nrow(ptCounts)) {
      rxFinal$value[rxFinal$Var1 == ptCounts$Var1[n]] <- round(rxFinal$value[rxFinal$Var1 == ptCounts$Var1[n]]/ptCounts$Freq[n], 3) * 100
    }
    ylab <- 'Percentage of prescriptions per diagnosis'
    ymax <- 100
  } 
  else {
    ylab <- 'Number of prescriptions ordered per diagnosis'
    ymax <- max(rxFinal$value) + 5
  }

  
  # now let's build a plot. we dont' need to rearrange these data since it's already in a format that ggplot likes
  rxPlot <- ggplot(rxFinal, aes(Var1, value, group=Var2, fill=Var2, order=desc(value))) + coord_flip() + # object and orientation
    geom_bar(stat='identity', position= position, alpha=.6, width=.8) + # bar layer
    geom_text(aes(label=Var2, y=0), position = position_dodge(width=.8), hjust=0)  # text labels
  
  if (labels== TRUE) {
    rxPlot <- rxPlot + 
      geom_text(aes(Var1, y= value, label=value), position=position_dodge(width=.8))
  }
  
  rxPlot <- rxPlot +
    # window dressings
    ggtitle(title) + ylab(ylab) + guides(fill=guide_legend('Site')) + 
    xlab('') + ylim(0,ymax) + plotElements +
    theme(legend.position='none') 
  
  # let's flip things out into a matrix for our table output
  rxTable <- dcast(rxFinal, Var2 ~ Var1)
  rownames(rxTable) <- rxTable$Var2 # name the rows appropriately
  rxTable <- rxTable[,-1, drop=FALSE] # pull off the original names column
  rxTable[is.na(rxTable)] <- 0 # replace NA values with 0
  
  rxOutput <- list('plot' = rxPlot, 
                   'table' = rxTable)   
  
  return(rxOutput)
}

topTest <- function(df, top=5, position='dodge', title=NULL, percents=TRUE, labels=FALSE) {
  # write a bar chart of the top tests. this follows the same process as 
  # topRx

  
  tests <- c('lab1', 'lab2', 'lab3', 'lab4', 'lab5')

  # new result table
  analysisTable <- data.frame(matrix(ncol=2, nrow=0)) # makes a blank table with three columns and 0 rows
  names(analysisTable) <- c('facility', 'test')
  
  # take facility, diagnosis, test and rx column and place in new results table 
  for (i in 1:5) { # we have 5 different columns, so we'll do this 5 times. i becomes 1 through 5
    subTable <- df[,c('facility', tests[i])]
    names(subTable) <- c('facility', 'test')
    analysisTable <- rbind(analysisTable, subTable)
  }
  
  labs <- melt(table(analysisTable$facility, analysisTable$test))
  total <- as.data.frame(table(analysisTable$test))
  names(total) <- c('Var2', 'value')
  labs <- rbind(labs, cbind.data.frame('Var1' = 'All Facilities', total))
  labs <- labs[order(labs$value, decreasing=TRUE),] # orderthem by descreasing value
  
  
  
  
  
  labFinal <- data.frame(matrix(nrow=0, ncol=3))
  for (site in unique(labs$Var1)) {
    tmp <- labs[labs$Var1 == site,]
    tmp <- tmp[order(tmp$value, decreasing=TRUE),]
    tmp <- tmp[1:top,]
    tmp <- tmp[tmp$value > 0,]
    labFinal <- rbind(labFinal, 
                        tmp
      )
    

  }
  labFinal$Var2 <- as.character(labFinal$Var2)
  tmp <- labs[labs$Var1 == 'All Facilities',] # filter to just those prescriptions
  tmp_others <- tmp[order(tmp$value, decreasing=TRUE),] # count them and order by decreasing value
  tmp_others <- tmp_others[tmp_others$Var1 == 'All Facilities' & tmp_others$Var2 %in% labFinal$Var2,]
  tmp_others <- tmp_others[!(tmp_others$Var2 %in% labFinal$Var2[labFinal$Var1 == 'All Facilities']),]
  
  labFinal <- rbind(labFinal, # stick it at the bottom of our final data set
                   tmp_others
  )
  
  
  ptCounts <- as.data.frame(table(df$facility), stringsAsFactors = F)
  ptCounts <- rbind.data.frame(ptCounts, c('All Facilities', nrow(df)))
  ptCounts$Freq <- as.numeric(ptCounts$Freq)
  if (percents == TRUE) {
    for (n in 1:nrow(ptCounts)) {
      labFinal$value[labFinal$Var1 == ptCounts$Var1[n]] <- round(labFinal$value[labFinal$Var1 == ptCounts$Var1[n]]/ptCounts$Freq[n], 3) * 100
    }
    ylab <- 'Percentage of labs per diagnosis'
    ytop <- 100
  } 
  else {
    ylab <- 'Number of labs ordered per diagnosis'
    ytop <- max(labFinal$value) + 5
  }
  
  
  # now let's build a plot. we dont' need to rearrange these data since it's already in a format that ggplot likes
  labPlot <- ggplot(labFinal, aes(Var1, value, group=Var2, fill=Var2, order=desc(value))) + coord_flip() + # object and orientation
    geom_bar(stat='identity', position= position, alpha=.6, width=.8) + # bar layer
    geom_text(aes(label=Var2, y=0), position = position_dodge(width=.8), hjust=0, size=5)  # text labels
  
  if (labels== TRUE) {
    labPlot <- labPlot + 
      geom_text(aes(Var1, y= value, label=value), position=position_dodge(width=.8), size=1)
  }
  
  labPlot <- labPlot +
    # window dressings
    ggtitle(title) + ylab(ylab) + guides(fill=guide_legend('Site')) + 
    xlab('') + ylim(0,ytop) + plotElements +
    theme(legend.position='none')  
  
  labTable <- dcast(labFinal, Var2 ~ Var1, value.var = 'value')
  rownames(labTable) <- labTable$Var2
  labTable <- labTable[,-1, drop=FALSE]
  labTable[is.na(labTable)] <- 0
  
  labOutput <- list('plot' = labPlot, 'table' = labTable) 
  
 
  return(labOutput)
}

avg_Rx_Test <- function(df) {
  avg_tests <- aggregate(df$num_tests, by=list(df$facility), mean)
  avg_tests <- rbind.data.frame(avg_tests, cbind.data.frame('Group.1' = 'All Facilities', 'x' = mean(df$num_tests)))
  
  avg_rx <- aggregate(df$num_rx, by=list(df$facility), mean)
  avg_rx <- rbind.data.frame(avg_rx, cbind.data.frame('Group.1' = 'All Facilities', 'x' = mean(df$num_rx)))
  
  avgs <- merge(avg_tests, avg_rx, by='Group.1')
  names(avgs) <- c('Site', 'Labs', 'Prescriptions')
  avgs[,c('Labs', 'Prescriptions')] <- apply(avgs[,c('Labs', 'Prescriptions')], 2, function(i) round(i,3))
  return(avgs)
}


## Graphing support -----------------------------------------------------
plotElements <- # these are global settings that are applied to all plots generated
  # make changes here to apply across the board (most of the time)
  theme(
    plot.title = element_text(face = "bold", size = 26),
    panel.background = element_blank(),
    # X axis settings
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 16, face='bold'),
    # Y axis settings
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 16, face='bold')
  ) + theme_bw() + theme(legend.text = element_text(size=16),
                             legend.title = element_text(size=16))


ggpie <- function (dat, by, totals) {
  # found this function online to create pie charts using ggplot
  # pass the melted data set, group column (by) and value column (totals)

  plot <-
    ggplot(dat, aes_string(
      x = factor(1), y = totals, fill = by, colour = by
    )) +
    geom_bar(stat = 'identity', size = 1, alpha = .6) +
    guides(fill = guide_legend(override.aes = list(colour = NA))) + # removes black borders from legend
    coord_polar(theta = 'y') + theme_bw() +
    theme(
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(
        colour = 'black', size = 12, angle = 0, hjust = 1
      ),
      axis.title = element_blank(),
      panel.border = element_blank()
    ) +
    scale_y_continuous(breaks = cumsum(dat[[totals]]) - dat[[totals]] / 2,
                       labels = paste0(round(dat[[totals]] / sum(dat[[totals]]) *
                                               100, 1), "%"))
  
  
  return(plot)
}


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <-
  function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
                       ncol = cols, nrow = ceiling(numPlots / cols))
    }
    
    if (numPlots == 1) {
      print(plots[[1]])
      
    } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      
      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        
        print(plots[[i]], vp = viewport(
          layout.pos.row = matchidx$row,
          layout.pos.col = matchidx$col
        ))
      }
    }
  }







