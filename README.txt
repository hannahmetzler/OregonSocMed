Data files: 

Data_files in data_raw202011 were used during the first analysis in Nov 2020. 
In June 2021, we wanted to correct for total tweets about suicide rather than total tweets per region. However, the numbers we downloaded on Brandwatch for this did not exactly match the data downloaded earlier, leading to negative differences between total volume of suicide tweets and suicide tweets about the lifeline/prevention (ie lower total volume than partial volume). Because this is not possible, we redownloaded the volumes important for the final analysis in the paper. The new raw data is in data_raw202106. 
There is a separate script 01preparedata202011.R and 01preparedata202106.R  for each of the raw data folders. 

All files contain volume counts of English tweets from Oregon or Washington
VolOregonCampaignWithAuthors.csv) # campaign terms narrowly defined, tweets from authors breaksilenceOR, 800723TALK & lines_for_life included
VolOregonCampaignWithoutAuthors.csv) # campaign terms narrowly defined, tweets from authors breaksilenceOR, 800723TALK & lines_for_life excluded
VolOregonPreventionWithAuthors.csv) #prevention tweets: lifeline number, lifeline keywords, betheone2 keywords and hashtags
VolOregonPreventionWithoutAuthors.csv) #prevention tweets: lifeline number, lifeline keywords, betheone2 keywords and hashtags
VolOregonSuicide.csv:  #tweets with suicide or suicidal, suicid*
VolOregonTotal.csv: all English tweets from region (here Oregon)


