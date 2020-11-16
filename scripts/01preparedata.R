
#packages
library(tidyr)

# Read data
or_cs <- read.csv("data_raw/VolOregonCampaignSuicide.csv") # campaign AND suicide terms: with keywords about the breaking the silence campaign, with suicide as keyword, from oregon
or_c  <- read.csv("data_raw/VolOregonCampaign.csv") # campaign tweets without suicide term: tweets with keywords about the campaign only,  from oregon
or_tot  <- read.csv("data_raw/VolOregonTotal.csv") #total tweet volume oregon
or_csn <- read.csv("data_raw/VolOregonCampaignNarrowSuicide.csv") #campaign AND suicide AND broader selection of hashtags around prevention
wa_cs <- read.csv("data_raw/VolWashingtonCampaignSuicide.csv") # campaign AND suicide tweets: with keywords about the breaking the silence campaign, with suicide as keyword, from oregon
wa_c  <- read.csv("data_raw/VolWashingtonCampaign.csv") # campaign tweets without suicide term: tweets with keywords about the campaign only,  from oregon
wa_tot  <- read.csv("data_raw/VolWashingtonTotal.csv") #total tweet volume oregon
wa_csn <- read.csv("data_raw/VolWashingtonCampaignNarrowSuicide.csv") #campaign AND suicide AND broader selection of hashtags around prevention
#bts_account <- read.csv("data_raw/VolTwitterAccountBreakSilenceOR.csv") #tweets posted by the account of the campaign itself

#rename columns
names(or_c) <- c("date","end","c_n")
names(or_cs) <- c("date","end","cs_n") 
names(or_csn) <- c("date","end","csn_n") 
names(or_tot) <- c("date","end","tot_n") 
names(wa_c) <- c("date","end","c_n") 
names(wa_cs) <- c("date","end","cs_n") 
names(wa_tot) <- c("date","end","tot_n")  
names(wa_csn) <- c("date","end","csn_n")
#names(bts_account) <- c("date","end","acc_n") #delete

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
prop_bl_long <- function(df_tot, df_c, df_cs, df_csn) {
  
  # to debug: when not using the following code as a function, but assign input directly
  #df_tot <- or_tot; df_c <- or_c; df_cs <- or_cs; df_csn<- or_csn 
 #df_tot <- wa_tot; df_c <- wa_c; df_cs <- wa_cs; df_csn<- wa_csn 
  
  #apply function defined above to format date and remove last date
  df_cs <- prepdates(df_cs)
  df_c <- prepdates(df_c)
  df_csn <- prepdates(df_csn) 
  df_tot <- prepdates(df_tot)
  
  #proportions with terms per category 
  df_all <- df_tot%>%
    left_join(df_cs)%>%
    left_join(df_csn) %>% 
    left_join(df_c)%>%
    mutate(c_pr = c_n/tot_n*100,
           cs_pr = cs_n/tot_n*100,
           csn_pr = csn_n/tot_n*100)
  
  # define data period to be analysed: from start of 2018 as baseline (campaign from April 7-14 2019) until end of november 2019 (to exclude christmas)
  df_all <- df_all %>% filter(date >="2018-01-01" & date <"2019-11-30")
  
  #baseline: the year 2018
  baseline <- df_all %>% 
    mutate(weekday = lubridate::wday(date, label=T, abbr=F))%>% #transform date to day of the week, first day: Tuesday
    filter(date >= as.Date("2018-01-01") & date <= as.Date("2018-12-31")) %>% #define when baseline period ends
    group_by(weekday) %>% 
    #campaign and suicide keywords
    summarize(cs_bl = median(cs_pr), 
              #campain keywords
              c_bl = median(c_pr),
              #campaign terms narrowly defined, suicide
              csn_bl = mean(csn_pr)) %>% #meadian is zero, and devision by zero will cause problems, therefore take the mean
    #to avoid baselines of 0
    mutate(csn_bl=ifelse(csn_bl==0, csn_bl+0.00000000001, csn_bl+0))
  
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
    mutate(keywords = dplyr::recode(keywords, c = "Campaign", cs="Campaign + Suicide", csn="Campaign narrow + Suicide")) %>% 
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
# print("Processing Oregon")
# df_oregon <- prop_bl_long(or_tot, or_c, or_cs, or_csn) %>% 
#   mutate(state = factor("Oregon"))
print("Processing Washington")
df_washington<- prop_bl_long(wa_tot, wa_c, wa_cs, wa_csn) %>%
  mutate(state = factor("Washington"))

#combine both dataframes
df <- rbind(df_oregon, df_washington) %>% 
#df <- df %>% 
  mutate(period = as.factor(case_when(
    date >= "2018-01-01" & date <= "2018-12-31" ~ "Baseline", 
    date > "2019-04-14" ~ "After", 
    date >= "2019-04-07" & date <= "2019-04-14" ~  "Campaign-week",
    TRUE ~ "2019 before campaign")))

write.csv(df, "data/TweetVolumeLongNarrow.csv", row.names=F)

#rm(list=ls()) #delete variables in environment

