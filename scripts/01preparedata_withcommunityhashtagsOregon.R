
#packages
library(tidyr)

# Read data: oregon data
or_ca <- read.csv("data_raw/VolOregonCampaignWithAuthors.csv") # campaign terms narrowly defined, tweets from authors breaksilenceOR, 800723TALK & lines_for_life included
or_c  <- read.csv("data_raw/VolOregonCampaignWithoutAuthors.csv") # campaign terms narrowly defined, tweets from authors breaksilenceOR, 800723TALK & lines_for_life excluded
or_pa <- read.csv("data_raw/VolOregonPreventionWithAuthors.csv") #prevention tweets: lifeline number, lifeline keywords, betheone2 keywords and hashtags
or_p <- read.csv("data_raw/VolOregonPreventionWithoutAuthors.csv") #prevention tweets: lifeline number, lifeline keywords, betheone2 keywords and hashtags
or_com <- read.csv("data_raw/VolOregonCommunityWithoutAuthors.csv") #community hashtags about suicide and the keyword suicide
or_tot  <- read.csv("data_raw/VolOregonTotal.csv") #total tweet volume oregon

#washington data
wa_ca <- read.csv("data_raw/VolWashingtonCampaignWithAuthors.csv") 
wa_c  <- read.csv("data_raw/VolWashingtonCampaignWithoutAuthors.csv") 
wa_pa <- read.csv("data_raw/VolWashingtonPreventionWithAuthors.csv") 
wa_p <- read.csv("data_raw/VolWashingtonPreventionWithoutAuthors.csv")
wa_com <- read.csv("data_raw/VolWashingtonCommunityWithoutAuthors.csv")
wa_tot  <- read.csv("data_raw/VolWashingtonTotal.csv") 


#rename columns
names(or_c) <- c("date","end","c_n")
names(or_ca) <- c("date","end","ca_n") 
names(or_p) <- c("date","end","prev_n") 
names(or_pa) <- c("date","end","preva_n")
names(or_com) <- c("date","end","com_n") 
names(or_tot) <- c("date","end","tot_n") 
names(wa_c) <- c("date","end","c_n") 
names(wa_ca) <- c("date","end","ca_n") 
names(wa_tot) <- c("date","end","tot_n")  
names(wa_p) <- c("date","end","prev_n")
names(wa_pa) <- c("date","end","preva_n")
names(wa_com) <- c("date","end","com_n") 

#function: change date format, remove last date
prepdates <- function(df) {
  df <- df %>%
    mutate(date = as.Date(date)) %>%
    select(-end) %>%  #remove the column with ending date, end is always 24h after start
    arrange(date)
  #delete rest of function until retunr(df)
    # #subtract tweets of the campaign account itself - delete this later
    # df[1:nrow(df),2] = df[1:nrow(df),2]-bts_account[1:nrow(df),"acc_n"]
    # #one tweet by the bts account did not contain any campaign keyword and is therefore not included in the campaign counts. 
    # #To prevent the count going below 0, add 1 if this happens, to correct for this, download all volumes while excluding account terms from the beginning
    # for(i in seq(1:nrow(df)))
    #     {
    #      if(df[i,2]==-1){df[i,2]=df[i,2]+1}
    #       }
  return(df) }

#function to calculate proportion, baseline and transform to long data format
prop_bl_long <- function(df_tot, df_c, df_ca, df_p, df_pa, df_com) {
  
  # to debug: when not using the following code as a function, but assign input directly
  # df_tot <- or_tot; df_c <- or_c; df_ca <- or_ca; df_p<- or_p; df_pa <-or_pa; df_com = or_com
 #df_tot <- wa_tot; df_c <- wa_c; df_ca <- wa_ca; df_p<- wa_p 
  
  #apply function defined above to format date and remove last date
  df_ca <- prepdates(df_ca)
  df_c <- prepdates(df_c)
  df_p <- prepdates(df_p) 
  df_pa <- prepdates(df_pa) 
  df_com <- prepdates(df_com)
  df_tot <- prepdates(df_tot)
  
  #proportions with terms per category 
  df_all <- df_tot%>%
    left_join(df_c)%>%
    left_join(df_ca)%>%
    left_join(df_p) %>% 
    left_join(df_pa) %>% 
    left_join(df_com) %>% 
    mutate(c_pr = c_n/tot_n*100,
           ca_pr = ca_n/tot_n*100,
           prev_pr = prev_n/tot_n*100,
           preva_pr = preva_n/tot_n*100, 
           com_pr = com_n/tot_n*100)
  
  # define data period to be analysed: from start of 2018 as baseline (campaign from April 7-14 2019) until end of november 2019 (to exclude christmas)
  df_all <- df_all %>% filter(date >="2018-01-01" & date <"2019-11-30")
  
  #baseline: the year 2018
  baseline <- df_all %>% 
    mutate(weekday = lubridate::wday(date, label=T, abbr=F))%>% #transform date to day of the week, first day: Tuesday
    filter(date >= as.Date("2018-01-01") & date <= as.Date("2018-12-31")) %>% #define when baseline period ends
    group_by(weekday) %>% 
    #campaign and suicide keywords
    summarize(ca_bl = median(ca_pr), 
              #if meadian is zero, and devision by zero will cause problems, therefore take the mean
              #campain keywords
              c_bl = median(c_pr),
              #campaign terms narrowly defined, suicide
              prev_bl= median(prev_pr),
              preva_bl= median(preva_pr), 
              com_bl = median(com_pr))
    #to avoid baselines of 0
    #mutate(prev_bl=ifelse(prev_bl==0, prev_bl+0.00000000001, prev_bl+0))
  
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
    mutate(keywords = dplyr::recode(keywords, c = "Campaign wo authors", ca="Campaign with authors", prev ="Prevention wo authors", preva="Prevention with authors")) %>% 
    mutate(keywords = factor(keywords))%>%
    rename(keywords_n = n)

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
df_oregon <- prop_bl_long(or_tot, or_c, or_ca, or_p, or_pa, or_com) %>% 
   mutate(state = factor("Oregon"))
print("Processing Washington")
df_washington<- prop_bl_long(wa_tot, wa_c, wa_ca, wa_p, wa_pa, wa_com) %>%
  mutate(state = factor("Washington"))

#combine both dataframes
df <- rbind(df_oregon, df_washington) %>% 
# df <- df_oregon %>% 
  mutate(period = as.factor(case_when(
    date >= "2018-01-01" & date <= "2018-12-31" ~ "Baseline", 
    date > "2019-04-14" ~ "After", 
    date >= "2019-04-07" & date <= "2019-04-14" ~  "Campaign-week",
    TRUE ~ "2019 before campaign"))) 

#drop levels with authors included and rename the remaining levels, save dataframe
df1 <- df %>% 
  mutate(keywords = recode(keywords, "com"="Community hashtags")) %>% 
  filter(keywords=="Campaign wo authors" | keywords=="Prevention wo authors" | keywords=='Community hashtags') %>% 
  droplevels() %>% 
  mutate(keywords=recode(keywords,"Campaign wo authors"= "Campaign", "Prevention wo authors"="Prevention"))

write.csv(df1, "data/TweetVolumeLongWithCommunityHashtags.csv", row.names=F)

# rm(list=ls()) #delete variables in environment

