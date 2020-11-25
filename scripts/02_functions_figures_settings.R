# functions for campaign response analysis

# function for time series plots
plotTS <- function(df, mode="percent_change", title="Country TS", limits=NULL, factor=15, textsize=13, axistextsize=12, legpos=c(0.8,0.7))
{
  
  #make plot
  plt <- ggplot(data=df)
  
  if (mode == "percent_change")
    plt <- plt +
      geom_ribbon(aes(x=date, ymin=(prlow-bl), ymax=(prhigh-bl), fill=keywords, colour=keywords), alpha=0.3, size=0) +
      geom_line( aes(x=date, y=(pr-bl), colour=keywords))+
      ylab("% tweets minus baseline")
  if (mode == "percent_abs")
    plt <- plt +
      geom_ribbon(aes(x=date, ymin=prlow, ymax=prhigh, fill=keywords, colour=keywords), alpha=0.3, size=0) +
      geom_line( aes(x=date, y=pr, colour=keywords))+
      ylab("% tweets with keywords") 
  if (mode == "number")
    plt <- plt +
      geom_line( aes(x=date, y=keywords_n, colour=keywords))+
      ylab("Daily number of tweets") 
  
   plt <- plt +  ggtitle(title)+ #plot title
    scale_fill_manual(values = c("Breaking the silence"="blue", "Lifeline"="cyan"), name="Keywords in tweets")+
    scale_colour_manual(values = c("Breaking the silence"="blue", "Lifeline"="cyan"), name="Keywords in tweets")+
    theme_bw()+ theme(text=element_text(size=textsize), axis.text=element_text(size=axistextsize), 
                      axis.title.x = element_blank(),
                      # axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), 
                      legend.position=legpos,
                      plot.margin = margin(0.5,0.5,0.5,0.5, "cm"), 
                      panel.grid.minor= element_blank())+
    scale_x_date(date_breaks="1 month", date_labels = "%b")+
    geom_hline(yintercept = 0, colour = "grey50")+ #mean line
    geom_vline(xintercept = as.Date("2019-04-05"), colour = "grey50", linetype=2)+ #campaign week start
    geom_vline(xintercept = as.Date("2019-04-14"), colour = "grey50", linetype=2) #campaign week end
  
  if (!is.null(limits))
    plt <- plt + 
    scale_y_continuous(limits = limits)
  
  return(plt)
}

#function to add important events linked to suicide to each time series plot
events1819 <- function(plot){
  plot +
    geom_vline(xintercept = as.Date("2018-01-02"), colour = "red3", linetype=4)+# outrage about video showing dead body by youtuber Paul Logan
    #excluding (logan OR paul OR LoganPaul OR video OR youtube) reduces the peak a lot
    geom_vline(xintercept = as.Date("2018-01-16"), colour = "red3", linetype=4)+# Tyler Hilinski, excluding these words abolishes the peak
    geom_vline(xintercept = as.Date("2018-06-05"), colour = "red3", linetype=4)+# Kate Spade
    geom_vline(xintercept = as.Date("2018-06-08"), colour = "red3", linetype=4)+ # Bourdain
    geom_vline(xintercept = as.Date("2018-09-10"), colour = "green2", linetype=4)+# World suicide prevention day
    geom_vline(xintercept = as.Date("2018-12-16"), colour = "green2", linetype=4)+# 3-digit suicide hotline number gets introduced
    geom_vline(xintercept = as.Date("2019-02-18"), colour = "green2", linetype=4)+ #transgender lifeline gets introduced
    geom_vline(xintercept = as.Date("2019-03-24"), colour = "red3", linetype=4)+# suicides school shooting victims and bereaved
    geom_vline(xintercept = as.Date("2019-06-25"), colour = "red3", linetype=4)+# Etika suicide news
    geom_vline(xintercept = as.Date("2019-09-10"), colour = "green2", linetype=4)+# World suicide prevention day
    geom_vline(xintercept = as.Date("2019-10-10"), colour = "green2", linetype=4)#world mental health day 
}

