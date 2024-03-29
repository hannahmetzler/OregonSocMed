library(ggplot2)
library(dplyr)

data_dir = "data_raw202011"

# Read data: oregon data, c = campaign/breaking the silence, a = authors, p=prevention/professional/lifeline
or_ca <- read.csv(paste0(data_dir, "/VolOregonCampaignWithAuthors.csv")) # campaign terms narrowly defined, tweets from authors breaksilenceOR, 800723TALK & lines_for_life included
or_c  <- read.csv(paste0(data_dir, "/VolOregonCampaignWithoutAuthors.csv")) # campaign terms narrowly defined, tweets from authors breaksilenceOR, 800723TALK & lines_for_life excluded
or_pa <- read.csv(paste0(data_dir, "/VolOregonPreventionWithAuthors.csv")) #prevention tweets: lifeline number, lifeline keywords, betheone2 keywords and hashtags
or_p <- read.csv(paste0(data_dir, "/VolOregonPreventionWithoutAuthors.csv")) #prevention tweets: lifeline number, lifeline keywords, betheone2 keywords and hashtags

#washington data
wa_ca <- read.csv(paste0(data_dir, "/VolWashingtonCampaignWithAuthors.csv") )
wa_c  <- read.csv(paste0(data_dir, "/VolWashingtonCampaignWithoutAuthors.csv") )
wa_pa <- read.csv(paste0(data_dir, "/VolWashingtonPreventionWithAuthors.csv") )
wa_p <- read.csv(paste0(data_dir, "/VolWashingtonPreventionWithoutAuthors.csv"))


#rename columns
names(or_c) <- c("date","end","c_n")
names(or_ca) <- c("date","end","ca_n") 
names(or_p) <- c("date","end","prev_n") 
names(or_pa) <- c("date","end","preva_n")
names(wa_c) <- c("date","end","c_n") 
names(wa_ca) <- c("date","end","ca_n") 
names(wa_p) <- c("date","end","prev_n")
names(wa_pa) <- c("date","end","preva_n")

#ALL above steps for the total reference volume
print("all tweets taken as total reference")
or_tot  <- read.csv(paste0(data_dir, "/VolOregonTotal.csv")) #total tweet volume oregon
wa_tot  <- read.csv(paste0(data_dir, "/VolWashingtonTotal.csv") )
names(or_tot) <- c("date","end","tot_n") 
names(wa_tot) <- c("date","end","tot_n")  


#function: change date format, remove last date
prepdates <- function(df) {
  df <- df %>%
    mutate(date = as.Date(date)) %>%
    select(-end) %>%  #remove the column with ending date, end is always 24h after start
    arrange(date)
  return(df)}

#function to calculate proportion, baseline and transform to long data format
prop_bl_long <- function(df_tot, df_c, df_ca, df_p, df_pa) {
  
  # to debug: when not using the following code as a function, but assign input directly
  # df_tot <- or_tot; df_c <- or_c; df_ca <- or_ca; df_p<- or_p; df_pa <-or_pa
  # df_tot <- wa_tot; df_c <- wa_c;   df_ca <- wa_ca; df_p<- wa_p; df_pa <- wa_pa
  
  #apply function defined above to format date and remove last date
  df_ca <- prepdates(df_ca)
  df_c <- prepdates(df_c)
  df_p <- prepdates(df_p) 
  df_pa <- prepdates(df_pa)
  
  df_tot <- prepdates(df_tot)
  
  
  #proportions with terms per category 
  df_all <- df_tot%>%
    left_join(df_c)%>%
    left_join(df_ca)%>%
    left_join(df_p) %>% 
    left_join(df_pa) %>% 
    mutate(c_pr = c_n/tot_n*100,
           ca_pr = ca_n/tot_n*100,
           prev_pr = prev_n/tot_n*100,
           preva_pr = preva_n/tot_n*100) %>% 
    
    # define data period to be analysed: from start of 2018 as baseline (campaign from April 7-14 2019) until end of november 2019 (to exclude christmas)
    filter(date >="2018-01-01" & date <"2019-11-30")
  
  #produce long version of the data
  df_long <- df_all%>%
    pivot_longer(
      -c(date, tot_n), 
      names_to = c("keywords", ".value"), 
      names_sep = "_" 
      #values_drop_na = TRUE
    ) %>%
    mutate(keywords = dplyr::recode(keywords, c = "Breaking the silence wo authors", ca="Breaking the silence with authors", prev ="Lifeline wo authors", preva="Lifeline with authors")) %>% 
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
df_oregon <- prop_bl_long(or_tot, or_c, or_ca, or_p, or_pa) %>% 
  mutate(state = factor("Oregon"))

print("Processing Washington")
df_washington<- prop_bl_long(wa_tot, wa_c, wa_ca, wa_p, wa_pa) %>%
  mutate(state = factor("Washington"))

#combine both dataframes
df <- rbind(df_oregon, df_washington) %>% 
  # df <- df %>% 
  mutate(period = as.factor(case_when(
    date >= "2018-01-01" & date <= "2018-12-31" ~ "Baseline", 
    date > "2019-04-14" & date <= "2019-06-14"~ "After", 
    date >= "2019-04-07" & date <= "2019-04-14" ~  "Campaign-week",
    TRUE ~ "2019 before campaign"))) 

#write.csv(df, "data/TweetVolumeLongOfEffectExcludingCampaignAuthors_202011.csv", row.names=F)

#save file with only the values when authors are excluded
df1 <- df %>% 
  filter(keywords=="Breaking the silence wo authors" | keywords=="Lifeline wo authors") %>% 
  droplevels() %>% 
  mutate(keywords=recode(keywords,"Breaking the silence wo authors"= "Breaking the silence", "Lifeline wo authors"="Lifeline"))

write.csv(df1, "data/TweetVolumeLong_TotalAll_202011.csv", row.names=F)


rm(list=ls()) #delete variables in environment

