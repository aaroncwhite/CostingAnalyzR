options(java.parameters = "-Xmx4g" )
library(XLConnect)
library(sqldf)
library(ggplot2)
library(reshape2)
library(lubridate)
library(magrittr)
library(table1xls)
library(plyr)
library(shiny)
library(DT)
library(shinyFiles)



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



## Import/Export Functions -------------------------------------------------
changeEncoding <- function(df) {
  # Convert encoding of imported csv file to UTF-8 no matter what the locale is
  # this is very important for the shiny app. 
  # This is just a wrapper function around apply and enc2utf8
  # ex.
  # > changeEncoding(df)
  
  return(as.data.frame(apply(df, 2, enc2utf8)))
}

getDefs <- function(db) {
  defs<- list('test' = dbGetQuery(db, "select * from test"),
              'diagnosis' = dbGetQuery(db, "select * from diagnosis"),
              'prescription' = dbGetQuery(db, 'select * from prescription')
  )
  
  return(defs)
  
}

cleanData <- function(df, definitions, columns) {
  # dyanmically clean imported csv data using definition tables
  # 4 definition tables are tests, diagnosis, prescriptions, and columns
  # columns table used first to standardize column naming conventions
  # upload a new definition file to change how data are handled
  # WARNING: Do not change existing column names as code is written 
  # specifically for those names (e.g. don't change age to Age or consult_start to something else)
  # you CAN add new column names for those that do not currently exist.
  cat('Cleaning data....\n')
  cat('...rename columns\n')
  columns$replace[columns$replace == "NULL"] <- NA
  # check that column table is character vectors not factors
  if (class(columns$original) == "factor") {
    columns$original <- as.character(columns$original)
    columns$replace <- as.character(columns$replace)
  }
  
  for (col in 1:ncol(df)) {
    # find column name
    n <- names(df)[col]
    # find matching replacement column name
    r <- grep(n, columns$original)[1] # 1 index takes first value if there are multiples
    # assign replacement value to r
    r <- columns$replace[r]
    
    if (!is.null(r)) { # if we found a match, overwrite the original
      names(df)[col] <- r
    }
    else { # otherwise drop in NULL
      names(df)[col] <- NA
    }
    
  }
  # drop other columns
  cat('...drop unnecessary columns\n')
  df <- df[,!is.na(names(df))]

  # do we have all the columns we need?
  cat('...check for missing columns\n')
  cols <- unique(columns$replace[!is.na(columns$replace)])
  dataCols <- names(df)
  missingCols <- cols[!(cols %in% dataCols)]
  
  if (length(missingCols) > 0) { # if there are columns missing, this will throw an error
    stop(paste("Missing column(s):", paste(missingCols, collapse=", "),"\nPlease check raw data set and column definition file!"))
  }
  # Now we just have the columns we want. let's start cleaning things up.
  
  # Clean facility names --------------
  # check for inconsistencies with location name first
  cat('...clean facility names\n')
  df$facility <- as.character(df$facility)
  locations <-
    c('Lacolline', 'Belladere', 'Hinche', 'HSN', 'Boucan Carre')
  
  # search for each location, in list above and standardize naming
  for (l in locations) {
    rows <- agrep(l, df$facility, ignore.case = FALSE, max=list(cost=.1))
    df$facility[rows] <- l
  }
  
  # over write sante fanm and any blanks or NAs with Lacolline
  rows <- agrep("Sante Fanm", df$facility, ignore.case = TRUE)
  df$facility[rows] <- 'Lacolline'
  
  rows <- agrep('Saint Nicolas umf', df$facility, ignore.case=TRUE, max=list(cost=.4))
  df$facility[rows] <- "HSN"
  
  # anything that is blank or N/A is replaced with Lacolline
  df$facility[df$facility %in% c('N/A',"") |
                       is.na(df$facility)] <- "Lacolline"
  
  # 
  unknowns <- unique(df$facility[!(df$facility %in% locations)])
  
  # other unknown site names will remain
  df$facility[!(df$facility %in% locations)] <- paste0("Unknown (",paste(unknowns, collapse=','),")")
  
  cat('...standardize dates\n')
  # check dates -----------
  df$date <- parse_date_time(df$date, guess_formats(df$date, c('dm', 'dmy')))
  df <- df[!is.na(df$date),] # kick out NA date rows
  df$date <- as.Date(df$date)
  
  # make two new columns for easier aggregation later on based on date
  df$month <- format(as.Date(df$date), '%Y-%m')
  df$week <- as.Date(cut(df$date, 'week')) # this gives the monday of the week
  
  # check sex
  df$sex[!(df$sex %in% c('M',"F"))] <- 'F' # overwrite any blank values with F
  
  cat('...clean ages\n')
  df$age <- cleanNumeric(df$age, 'm', 12)
  
  
  cat('...calculate wait times\n')
  # Calc wait  times
  df$wait_time <-
    calcMinutes(df$consult_start) - calcMinutes(df$checkin)
  
  cat('...calculate consultation times\n')
  # Consultation Time
  df$consultation_time <-
    calcMinutes(df$consult_end) - calcMinutes(df$consult_start)
  
  # clean travel time
  # clean out minutes- remove any text in column, don't multiply by anything
  cat('...clean travel times column\n')
  df$travel_time <-
    cleanNumeric(df$arrival_time, 'm', 1)
  # clean out hours- remove any remaining values containing 'h' and multiply by 60 to convert to minutes value
  df$travel_time[is.na(df$travel_time)] <-
    cleanNumeric(df$arrival_time[is.na(df$travel_time)],'h', 1 /
                   60)
  
  cat('...count labs and prescriptions\n')
  # Labs and Prescriptions ------------------------
  # count tests and Rx
  tests <- c('lab1', 'lab2', 'lab3', 'lab4', 'lab5')
  rx <- c('rx1', 'rx2', 'rx3', 'rx4', 'rx5')
  
  
  # When we import, R thinks blank cells are "", overwrite them as NA
  for(col in c(tests, rx)) {
    # overwrite blank cells with NA for all of the above listed columns
    df[,col][df[,col] ==""] <- NA
  }

  # count the number of tests and rxs by row and add new count columns
  df$num_tests <- apply(df[,tests], 1, function(a) length(a[!is.na(a)])) # 1 does by row and counts only nonNA values
  df$num_rx <- apply(df[,rx], 1, function(a) length(a[!is.na(a)])) # 1 does by row
  
  
  # rename tests, prescriptions, and diagnoses if those are present, if not this will skip
  if (!missing(definitions)) {
    cat('...standardize labs/tests\n')
    for (col in tests) { # 5 columns we defined on line 466
      for (i in 1:length(definitions$test)) { # go through each original/replace pair
        if (length(definitions$test$Original[i] %in% df[,col]) > 0) { # if that original exists in the data set
          df[,col][df[,col] %in% definitions$test$Original[i]] <- as.character(definitions$test$Replace.with[i]) # overwrite it with the replace value
        } 
        # if it doesn't exist in the dataset, do nothing
      }
    }
    
    cat('...standardize prescriptions\n')
    
    for (col in rx) {
      for (i in 1:length(definitions$prescription)) {
        if (length(definitions$prescription$Original[i] %in% df[,col]) > 0) {
          df[,col][df[,col] %in% definitions$prescription$Original[i]] <- as.character(definitions$prescription$Replace.with[i])
        }
      }
    }
    
    cat('...standardize diagnoses\n')
    
    col <- "diagnosis"
    for (i in 1:length(definitions$diagnosis)) {
      if (length(definitions$diagnosis$Original[i] %in% df[,col]) > 0) {
        df[,col][df[,col] %in% definitions$prescription$Original[i]] <- as.character(definitions$prescription$Replace.with[i])
      }
    }
  }
  # replace diagnosis, tests, and prescriptions with definition docs
 
  cat('Data cleaning completed!') 
  return(df)
}


cleanNumeric <- function(vector, search, reduce = 1, verbose=F) {
  # for given dataframe, find values within stated column with a specified
  # character value.  Strip numeric values out and divide by reduce object (default 1)
  # creates new column named columnOld with original values, new values are written into current column.
  # Returns dataframe object.
  
  # Ex.
  # > d <- cleanNumeric(d, 'age', 'm', reduce = 12) # find values in the age column with 'm' (months) divides by 12
  # > d <- cleanNumeric(d, 'age', 'w', reduce = 52) # find weeks
  # currently does not account for days (searching for 'y' would return anything with days)
  vector <- as.character(vector)
  vectorNew <- as.numeric(vector)
  
  locations <- grep(search, vector, ignore.case = TRUE)
  # find just the numeric vals. assuming number comes first
  for (na in locations) {
    # look at each value that came out with text of 'y'
    end <-
      nchar(vector[na]) # total number of characters in current value
    # loop until find nonNA value when converting to numeric
    # starts from right and moves left. stops when end == 0 and ignores
    while (is.na(suppressWarnings(as.numeric(substr(vector[na], 1, end))))) {
      end <- end - 1
      if (end <= 0) {
        if (verbose==T) {
          warning(paste("No number found for value:", vector[na]))
          
        }
        break
      }
    }
    vectorNew[na] <- as.numeric(substr(vector[na], 1, end)) / reduce
  }
  
  # return cleaned vector
  if (verbose==T) {
    cat(
    'Search:', search, '\nCleaned Values:',vector[locations],'\nReturned values:',vectorNew[locations],'\n\n'
    )
  }
  
  return(vectorNew)
}

calcMinutes <- function(timeVector) {
  # first we need to split the time vectors into component parts
  col1 <-
    strsplit(as.character(timeVector), ":") # hour and min split
  
  for (i in 1:length(col1)) {
    # now we need to split off AM/PM (if it's there)
    split2 <- try(unlist(strsplit(col1[[i]][2]," ")))
    
    # let's convert everything to 24 hour time while we're at it
    if (split2[2] == "PM" &
        length(split2) == 2 & as.numeric(col1[[i]][1]) < 12) {
      col1[[i]][1] <- as.numeric(col1[[i]][1]) + 12
    }
    
    col1[i] <- as.numeric(col1[[i]][1]) * 60 + as.numeric(split2[1])
  }
  return(unlist(col1))
}

importExcel <- function(filename, sheet = NULL, range = NA) {
  # import excel file into object
  wb <- loadWorkbook(filename)
  if (is.null(sheet) &
      length(getSheets(wb)) == 1) {
    # if no sheet specified and only one sheet exists use that one
    sheet <- getSheets(wb)
  } else if (is.null(sheet) &
             length(getSheets(wb)) > 1) {
    # if no sheet specified and multiple exist, prompt which one
    cat("\rMultiple worksheets found!  Please specify which one to load.")
    sheets <- getSheets(wb)
    
    l <- paste0(sheets, collapse = ", ")
    cat('\nAvailable worksheets:', l)
    sheet <- readline(prompt = 'Specify which to use: ')
  } # if sheet is specified that will be used.
  if (is.na(range)) {
    sheet <- readWorksheet(wb, sheet)
  } else {
    sheet <- readWorksheet(wb, sheet, region = range)
  }
  return(sheet)
}

exportPlot <- function(plot, filename, plotWidth=700, plotHeight=440, workbook=NA, plotLocation=NA, plotFolder='plots/') {
  if (!file.exists("plots") & plotFolder=='plots/') {
    dir.create(file.path("./plots"))
  }
  # initalize png device
  file <- file.path(paste0(plotFolder,filename,".png"))
  png(filename = file, width= plotWidth, height=plotHeight)
  # print the plot to the device
  print(plot)
  # close the device
  dev.off()
  
  if (!missing(workbook) & !is.na(plotLocation)) {
    cat('Exporting image to workbook\n')
    createName(wb, filename, formula= plotLocation, overwrite=TRUE)
    addImage(wb, filename = file, name = filename, originalSize = TRUE)
  } 
  else {
    cat('Exporting image to file only\n')
  }
}

exportData <- function(wb, summaryData, filteredData= NULL, originalData= NULL) {
#   filename <- paste0(filename, '.xlsx')
#   wb <- loadWorkbook(filename, create=TRUE)
  start <- 1
  cat('Exporting Summary Data\n')
  createSheet(wb, 'Summary')
  for (i in 1:length(summaryData)) {
    cat(names(summaryData)[i],'\n')
    exportPlot(summaryData[[i]]$plot, # plot object
               gsub(" ", "", names(summaryData)[i]), # name for file
               workbook=wb, # workbook object
               plotLocation=paste0('Summary!$A$',start), # location within workbook
               plotWidth=600) # plot width
    XLgeneric(wb, 'Summary', summaryData[[i]]$table, row1=start, col1=12, title=names(summaryData)[i]) # will drop in column I
    start <- start + 25
  }
  cat('\n')
  # clean up plot directory so we don't get a bunch of crap
  unlink('plots', recursive=TRUE)
  
  # drop in filteredData
  if (!is.null(filteredData)) {
    cat('Filtered Dataset\n')
    XLgeneric(wb, 'Filtered Data', filteredData)
  }
  # drop in originalData
  if (!is.null(originalData)) {
    cat('Original Dataset\n')
    XLgeneric(wb, 'Original Data', originalData)
  }

  saveWorkbook(wb)
}

importDefinitions <- function(db, filename) {
  # import excel file that contains renaming conventions for
  # diagnoses, tests, and prescriptions
  # this will save to a sqlite db object and overwrite
  # tables with the same names.  sheets must be named
  # 'tests, 'prescriptions', 'diagnosis'
  wb <- loadWorkbook(filename)
  for (s in getSheets(wb)) {
    cat('\r',paste("Importing Sheet",s))
    tmp <- readWorksheet(wb, s)
    dbWriteTable(db, tolower(s), tmp, overwrite=TRUE)
    Sys.sleep(.1)
    flush.console()
  }
  flush.console()
  cat('All sheets successfully imported')
  # return nothing since data are in db object now
}




