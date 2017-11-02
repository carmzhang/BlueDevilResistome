library(ggplot2); library(shiny)

#from:http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#Helper functions
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# import data
dat <- read.table("queryFour.txt",header=TRUE,sep=",")
dat$ctx <- as.factor(dat$ctx) 
datSE <- summarySE(dat, measurevar="cfu", groupvars=c("site_id","ctx"))

server<-function(input,output){
  output$plot1 <- renderPlot({
    if(input$plot_type == "Total"){
      ggplot(dat,aes(site_id)) + geom_bar(aes(x=site_id, y=cfu, fill=ctx), stat="identity") + labs(title="Query 4: Total abundance and resistance per site", x="Site ID", y="CFU Count")
    } else if(input$plot_type == "Average"){
      ggplot(datSE,aes(x=site_id, y=cfu, fill=ctx)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=cfu-se, ymax=cfu+se),width=.2,position=position_dodge(.9)) + labs(title="Query 4: Average abundance and resistance per site", x="Site ID", y="CFU Count")
    }
  })
  
  output$click_info <- renderPrint({
    cat("Click:\n")
    str(input$plot_click)
  })
  output$hover_info <- renderPrint({
    cat("Hover:\n")
    str(input$plot_hover)
  })
  #output$dblclick_info <- renderPrint({
  #  cat("input$plot_dblclick:\n")
  #  str(input$plot_dblclick)
  #})
  output$brush_info <- renderPrint({
    cat("Brush:\n")
    str(input$plot_brush)
  })
}