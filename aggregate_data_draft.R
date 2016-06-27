library(jsonlite)
setwd("~/SFI/DOTA/analysis/")
library(rmongodb)
mongo<-mongo.create()
mongo.is.connected(mongo)
mongo.get.databases(mongo)

cc<-"agg.progames" # or agg.progames, depending on your dataset 

# the following shows how to query the picks and bans, bringing along identifable data, like match_id,
# and arrange the nested json data into a data frame:
# 
querymongo<-list() # return everything, no filters
queryfields<-list(match_id=1L,picks_bans=1L,radiant_win=1L) # only fields listed will be returned

mongo.count(mongo,cc,querymongo) # count number of events in your query

# form list of data, can use the argument "data.frame=T" if all data is at the same level (i.e., no nested data)
ldf<-mongo.find.all(mongo,cc,querymongo,fields=queryfields) #,limit=10L

# turn the data list into a n X m data frame that is easier to work with
df<-data.frame() # initialize your data frame
for (i in 1:length(ldf)){ # loop over each game in the returned data
        l<-length(ldf[[i]]$picks_bans) # find out if there are any picks or bans 
        if (l>0){ # if there are picks/bans, then collect them

                # pick out the pickbans from each element in list and bind them to a data frame.
                temp1<-do.call(rbind.data.frame, ldf[[i]]$picks_bans) 
                
                ### for a more explicit calculation of the above line, see the commented loop below
                # temp1<-data.frame()
                # for (j in 1:l){ # loop over each pick ban json field and bind together
                #         temp<-data.frame(ldf[[i]]$picks_bans[[j]])
                #         temp1<-rbind(temp1,temp)
                # }
                
                # add fields in the main nest to the data frame.  each should be a 
                # single value that will be copied into every row of the temp1 data frame
                # allows you to keep track of other identifiable variables
                temp1$match_id<-ifelse(!is.null(ldf[[i]]$match_id),ldf[[i]]$match_id,NA)
                #temp1$version<-ldf[[i]]$version
                temp1$radiant_win<-ifelse(!is.null(ldf[[i]]$radiant_win),ldf[[i]]$radiant_win,NA)
                # bind all together
                df<-rbind(df,temp1)
        }
}
# arrange columns in a better order with "select" 
df<-df  %>% select(match_id:radiant_win,is_pick:order)
heroes<-fromJSON("/Users/asallaska/SFI/DOTA/heroes.json")
heroes<-heroes$result$heroes
heroes<-heroes %>% select(id,localized_name) %>% rename(hero_id=id,hero_name=localized_name)

df<-left_join(df,heroes,by="hero_id")
write.csv(df,"../data/agg/draft_progames.csv",row.names=F)






