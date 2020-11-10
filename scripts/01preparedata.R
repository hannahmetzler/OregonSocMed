
#packages
library(tidyr)



# Read data
or_cs <- read.csv("data_raw/VolOregonCampaignSuicide.csv") # campaign AND suicide terms: with keywords about the breaking the silence campaign, with suicide as keyword, from oregon
or_c  <- read.csv("data_raw/VolOregonCampaign.csv") # campaign tweets without suicide term: tweets with keywords about the campaign only,  from oregon
or_tot  <- read.csv("data_raw/VolOregonTotal.csv") #total tweet volume oregon
or_csp <- read.csv("data_raw/VolOregonCampaignSuicidePrevention.csv") #campaign AND suicide AND broader selection of hashtags around prevention
wa_cs <- read.csv("data_raw/VolWashingtonCampaignSuicide.csv") # campaign AND suicide tweets: with keywords about the breaking the silence campaign, with suicide as keyword, from oregon
wa_c  <- read.csv("data_raw/VolWashingtonCampaign.csv") # campaign tweets without suicide term: tweets with keywords about the campaign only,  from oregon
wa_tot  <- read.csv("data_raw/VolWashingtonTotal.csv") #total tweet volume oregon

#rename columns
names(or_c) <- c("date","end","c_n")
names(or_cs) <- c("date","end","cs_n") 
names(or_csp) <- c("date","end","csp_n") 
names(or_tot) <- c("date","end","tot_n") 
names(wa_c) <- c("date","end","c_n") 
names(wa_cs) <- c("date","end","cs_n") 
names(wa_tot) <- c("date","end","tot_n")  

#function: change date format, remove last date
prepdates <- function(df) {
  df <- df %>%
    mutate(date = as.Date(date)) %>%
    select(-end) %>%  #remove the column with ending date, end is always 24h after start
    arrange(date)
  return(df) }

#function to calculate proportion, baseline and transform to long data format
prop_bl_long <- function(df_tot, df_c, df_cs) {
  
  #df_tot <- or_tot; df_c <- or_c; df_cs <- or_cs # when not using the following code as a function
  
  #apply function defined above to format date and remove last date
  df_cs <- prepdates(df_cs)
  df_c <- prepdates(df_c) 
  df_tot <- prepdates(df_tot)
  
  #proportions with terms per category 
  df_all <- df_tot%>%
    left_join(df_cs)%>%
    left_join(df_c)%>%
    mutate(c_pr = c_n/tot_n*100,
           cs_pr = cs_n/tot_n*100)
  
  # define data period to be analysed: from 1 year before campaign (campaign from April 7-14 2019) until end of november 2019 (to exclude christmas)
  df_all <- df_all %>% filter(date >="2018-04-01" & date<"2019-10-16")
  
  #baseline: define with david: until 2 weeks before the campaign: March 24 2019, and 6 months before that from September 24 onward?
  #or April 2018?
  baseline <- df_all %>% 
    mutate(weekday = lubridate::wday(date, label=T, abbr=F))%>% #transform date to day of the week, first day: Tuesday
    filter(date < as.Date("2019-03-24") & date > as.Date("2018-09-24"))%>% #define when baseline period ends
    group_by(weekday) %>% 
    #campaign and suicide keywords
    summarize(cs_bl = mean(cs_pr), 
              cs_blmed = median(cs_pr),
              cs_sd = sd(cs_pr), 
              #campain keywords
              c_bl = mean(c_pr),
              c_blmed = median(c_pr),
              c_sd = sd(c_pr))
  
  #add weekday labels to df_all
  df_all <- df_all %>% 
    mutate(weekday = lubridate::wday(date, label=T, abbr=F))  %>% 
    #add baseline for each weekday to df_all
    inner_join(baseline)
  
  #produce long version of the data
  df_long <- df_all%>%
    pivot_longer(
      -c(weekday, date, tot_n), 
      names_to = c("keywords", ".value"), 
      names_sep = "_" 
      #values_drop_na = TRUE
    ) %>%
    mutate(keywords = dplyr::recode(keywords, c = "Campaign", cs="Campaign + Suicide")) %>% 
    mutate(keywords = factor(keywords))%>%
    rename(keywords_n = n)
  head(df_long)
  
  df_long$prlow <- NA
  df_long$prhigh <- NA
  
  #binomial confidence intervals - this takes a while
  for (i in seq(1, nrow(df_long)))
  {
    if (!is.na(df_long$keywords_n[i]) &  !is.na(df_long$tot_n[i])) # if n keyword and n total are not empty
    {
      #binomial test
      btest <- binom.test(x= df_long$keywords_n[i], n =  df_long$tot_n[i])
      df_long$prlow[i] <- 100*btest$conf.int[1]
      df_long$prhigh[i] <- 100*btest$conf.int[2]
      # 95% CI computed with the Clopper and Pearson (1934) method
    }
    
  }
  return(df_long)
  
} #end of prop_bl_long function
print("Processing Oregon")
df_oregon <- prop_bl_long(or_tot, or_c, or_cs) %>% 
  mutate(state = factor("Oregon"))
print("Processing Washington")
df_washington<- prop_bl_long(wa_tot, wa_c, wa_cs) %>% 
  mutate(state = factor("Washington"))

#combine both dataframes
df <- rbind(df_oregon, df_washington) %>% 
#df <- df %>% 
  mutate(period = case_when(
    date < "2019-03-24" & date > "2018-09-24" ~ "Baseline", 
    date > "2019-04-14" ~ "After", 
   TRUE ~ "Campaign-week"))

write.csv(df, "data/TweetVolumeLong.csv", row.names=F)

rm(list=ls()) #delete variables in environment

