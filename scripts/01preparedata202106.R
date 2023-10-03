library(ggplot2)
library(dplyr)

# choose if total tweet volume or tweets about suicide should be taken as the reference for specific queries (e.g. the total based on which percentages are calculated)

total = "all" # suicide or all

#data directory for raw data
data_dir = "data_raw202106"

# Read data: oregon data, c = campaign/breaking the silence, p=prevention/professional/lifeline
or_c  <- read.csv(paste0(data_dir, "/VolOregonCampaignWithoutAuthors.csv"), sep="\t") # campaign terms narrowly defined, tweets from authors breaksilenceOR, 800723TALK & lines_for_life excluded
or_p <- read.csv(paste0(data_dir, "/VolOregonPreventionWithoutAuthors.csv"), sep="\t") #prevention tweets: lifeline number, lifeline keywords, betheone2 keywords and hashtags

#washington data
wa_c  <- read.csv(paste0(data_dir, "/VolWashingtonCampaignWithoutAuthors.csv"), sep="\t")
wa_p <- read.csv(paste0(data_dir, "/VolWashingtonPreventionWithoutAuthors.csv"), sep="\t")

#rename columns
names(or_c) <- c("date","c_n") #campaign
names(or_p) <- c("date","prev_n") #prevention
names(wa_c) <- c("date","c_n") 
names(wa_p) <- c("date","prev_n")

#ALL above steps for the total reference volume
if(total =="all"){
  print("all tweets taken as total reference")
  or_tot  <- read.csv(paste0(data_dir, "/VolOregonTotal.csv"), sep="\t") #total tweet volume oregon
  wa_tot  <- read.csv(paste0(data_dir, "/VolWashingtonTotal.csv"), sep="\t")
  names(or_tot) <- c("date","tot_n") 
  names(wa_tot) <- c("date","tot_n")  
} else {
  print("tweets about suicide taken as total reference")
  or_tot<- read.csv(paste0(data_dir, "/VolOregonSuicid.csv"), sep="\t") #tweets with suicide or suicidal, suicid*
  wa_tot<- read.csv(paste0(data_dir, "/VolWashingtonSuicid.csv"), sep="\t")
  names(or_tot) <- c("date", "tot_n")
  names(wa_tot) <- c("date","tot_n")
}

#function: change date format, remove last date
prepdates <- function(df) {
  df <- df %>%
    mutate(date = as.Date(date)) %>%
    arrange(date)
  return(df)}

#function to calculate proportion, baseline and transform to long data format
prop_bl_long <- function(df_tot, df_c, df_p) {
  
  # to debug: when not using the following code as a function, but assign input directly
  # df_tot <- or_tot; df_c <- or_c; df_p<- or_p
  # df_tot <- wa_tot; df_c <- wa_c; df_p<- wa_p
  
  #apply function defined above to format date and remove last date
  df_c <- prepdates(df_c) #campaign query volume
  df_p <- prepdates(df_p) #prevention terms volume
  df_tot <- prepdates(df_tot) #total tweet volume
 
#proportions with terms per category 
  df_all <- df_tot%>%
    left_join(df_c)%>%
    left_join(df_p) %>% 
    mutate(c_pr = c_n/tot_n*100,
           prev_pr = prev_n/tot_n*100,) %>% 
  # define data period to be analysed: from start of 2018 as baseline (campaign from April 7-14 2019) until end of november 2019 (to exclude christmas)
  filter(date >="2018-01-01" & date <"2019-11-30")

  #produce long version of the data
  df_long = NULL
  df_long <- df_all%>%
    pivot_longer(
      -c(date, tot_n), 
      names_to = c("keywords", ".value"), 
      names_sep = "_" 
      #values_drop_na = TRUE
    ) %>%
    mutate(keywords = dplyr::recode(keywords, c = "Breaking the silence wo authors", prev ="Lifeline wo authors")) %>% 
    mutate(keywords = factor(keywords))%>%
    rename(keywords_n = n)
  
  # #check time series of different query volumes
  # ggplot(df_long)+
  #   geom_line(aes(x=date, y=tot_n))+
  #   geom_line(aes(x=date, y=keywords_n, colour=keywords))
  # 
  # # difference
  # ggplot(df_all)+
  #   geom_line(aes(x=date, y=tot_n-prev_n))+
  #   ggtitle('difference total minus lifeline volume')

  df_long$prlow <- NA
  df_long$prhigh <- NA
  
  #binomial confidence intervals - this takes a while
  for (i in seq(1, nrow(df_long)))
  {
    if (!is.na(df_long$keywords_n[i]) &  !is.na(df_long$tot_n[i])) # if n keyword and n total are not empty
    {
      #binomial test
      # print(i)
      btest <- binom.test(x= df_long$keywords_n[i], n =  df_long$tot_n[i])
      df_long$prlow[i] <- 100*btest$conf.int[1]
      df_long$prhigh[i] <- 100*btest$conf.int[2]
      # 95% CI computed with the Clopper and Pearson (1934) method
    }
    
  }
  
  return(df_long)
  
} #end of prop_bl_long function

print("Processing Oregon")
df_oregon <- prop_bl_long(or_tot, or_c, or_p) %>% 
  mutate(state = factor("Oregon"))

print("Processing Washington")
df_washington<- prop_bl_long(wa_tot, wa_c, wa_p) %>%
  mutate(state = factor("Washington"))

#combine both dataframes
df <- rbind(df_oregon, df_washington) %>% 
  mutate(period = as.factor(case_when(

    date >= "2018-01-01" & date <= "2018-12-31" ~ "Baseline", 
    date >= "2019-01-01" & date < "2019-04-07" ~ "2019 before campaign",
    date >= "2019-04-07" & date <= "2019-04-14" ~  "Campaign-week",
    date >= "2019-04-15" & date <= "2019-06-14"  ~ "After",
    TRUE ~ "More than 2 months after campaign"))) 

#save file with only the values when authors are excluded
df1 <- df %>%
  filter(keywords=="Breaking the silence wo authors" | keywords=="Lifeline wo authors") %>%
  droplevels() %>%
  mutate(keywords=dplyr::recode(keywords,"Breaking the silence wo authors"= "Breaking the silence", "Lifeline wo authors"="Lifeline"))

if(total =="all"){
  write.csv(df1, "data/TweetVolumeLong_TotalAll_202106.csv", row.names=F)
} else{
  write.csv(df1, "data/TweetVolumeLong_TotalSuicide_202106.csv", row.names=F)
}

rm(list=ls()) #delete variables in environment

