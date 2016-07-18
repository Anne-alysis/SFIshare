
# Summary:
# Converts each raw json file match\_id.dem.results.json from a json format of each 
# entry is a separate event into each entry is a unique time stamp with all events 
# that occur at that step.  Assumes an 8 core machine.  Produces match\_id.dem.results.csv.zip files. 

require(parallel)
require(jsonlite)
require(grid)
require(rio)

args<-commandArgs(TRUE)
if (length(args)==0) {
        stop("At least one argument must be supplied (directory name).n", call.=FALSE)
} 

disthero<-function(df,i1,j1,i2,j2){
        # calculate euclidean distance from df for all rows for the given pair of heroes i and j
        x1<-df[,i1]
        x2<-df[,i2]
        y1<-df[,j1]
        y2<-df[,j2]
        dist<-sqrt((y2-y1)^2+(x2-x1)^2)
        dist
}

finddist<-function(df,teamno,heroteams){
        # find relative distances for all unique combinations of heroes on a given team
        
        # # find position columns
        # if (any(grepl("\\.Radiant$",names(df)))==F){ #i.e., no match in aggregated data
        #         if (teamno=="Radiant"){
        #                 teamno<-"2"
        #         }else{
        #                 teamno<-"3"
        #         }
        # }
        dfpos<-df[,grepl("^x\\.|^y\\.",names(df)) & grepl(paste0("\\.",teamno),names(df))] # find the df for position
        # create combinations of heroes to calculate distance 
        c<-combn((heroteams %>% filter(teamRD==teamno))$lowerhero,2,simplify = F) # find all non-degenerate combinations of heroes
        # make the combinations nice to make as column names
        cname<-sapply(c,function(i) paste(i,collapse="_"))
        cname<-paste0("d.",cname,".",teamno)
        # loop through each pair to calculate the distance for all rows at once
        for (i in 1:length(c)){
                ind<-grep(paste(c[[i]],collapse="|"),names(dfpos))
                dfpos[,cname[i]]<-disthero(dfpos,ind[1],ind[2],ind[3],ind[4]) # calculate distance
        }
        temp<-dfpos[,grep("_",names(dfpos))]
        temp[,paste0("avgDist.",teamno)]<-apply(temp,1,mean) # find average distance among the team
        temp
}
parallelBuild<-function(z){
        
        # start new df structure by building out a zero value data frame for each unique time/tick step
        df<-data.frame(unique(select(z,match_id:time)))

        # determine new variables to keep track of (non-positional)
        #### this may be added onto to increase tracked variables! 
        newvar<-c("KillsHero","KillsOther","KillsTower","Deaths",
                  "DAMAGETakenFromHero","DAMAGETakenFromOther","DAMAGEDealtToHero","DAMAGEDealtToOther","XP","GOLD")
        #newvar<-paste0(rep(newvar,2),rep(c(".cum",".net"),each=length(newvar)))
        newvar<-c(newvar,"x","y") # add position variables
        
        # find list of unique heroes for each team, in order to concatenate each variable above per hero
        heroteamsI<-unique(z %>% filter(!is.na(teamRD)) %>% select(teamRD,hero) %>% mutate(hero=gsub(" |\\_|'","",hero)))[1:10,]
        heroteamsI<-heroteamsI %>% mutate(concat=paste(hero,teamRD,sep=".")) %>% arrange(teamRD,hero) 
        newvar<-paste(newvar,rep(heroteamsI$concat,each=length(newvar)),sep=".")
        # build out data frame for new variables by populating each value with a zero
        for (j in newvar){
                df[,j]<-0
        }
        
        # using the new structure, populate it with the game data with this function
        df<-buildOut(df,z)
        
        # determine cumulative variables and propogate the positions (zero everywhere unless they change position, need to make consistent)
        dfcum<-select(df,-(match_id:time))
        dfnet<-select(dfcum,grep("^x\\.|^y\\.",names(dfcum),invert=T))
        names(dfnet)<-paste0("net.",names(dfnet))
        indpos<-grepl("^x\\.|^y\\.",names(dfcum))
        names(dfcum)[!indpos]<-paste0("cum.",names(dfcum)[!indpos])
        dfprev<-select(df,match_id:time)
        
        # propogate locations and other variables
        dfpos<-dfcum[,indpos] # position variables
        dfnopos<-dfcum[,!indpos] # non-position variables
        for (i in 2:nrow(dfcum)){
                if (i %% 1000 ==0) print(paste(c("i = ",i," ",dfcum$tick[i]," (",round(i/nrow(dfcum)*100,1)," %)"),collapse=""))

                ind<-which(dfpos[i,]==0) # find zeros
                dfpos[i,ind]<-dfpos[(i-1),ind] # populate zeros with previous value for position
                dfnopos[i,]<-dfnopos[i,]+dfnopos[(i-1),] # add previous value for non-position variables
        }
        dfcum<-cbind(dfpos,dfnopos) # join all cumulative variables
        df<-cbind(dfprev,dfcum,dfnet) # join all data

        df
}



buildOut<-function(df,z){
        # main function to reorder the json, z
        # for each event entry in the newly built out, zero dataframe "df", find all its corresponding time/tick matches in z
        
        for (i in 1:nrow(df)){
                dftemp<-filter(z,tick==df$tick[i] & time==df$time[i]) # for the unique time stamp, find all events that correspond to it
                
                # go through each event and populate respective column in the df 
                for (k in 1:nrow(dftemp)){
                        if (grepl("GOLD|XP",dftemp$type[k])){ # gold or xp event
                                # increment gold or xp
                                j<-grep(paste0(dftemp$type[k],"\\.",dftemp$hero[k]),names(df)) # find hero column event corresponds to
                                # this is to calculate total accumulated gold or XP, ignoring 
                                # gold taken away from battle, as reflected in damage
                                df[i,j]<-dftemp$value[k]

                        }else if (dftemp$type[k]=="POSITION"){ # movement event
                                j<-grep(paste0("^x\\.",dftemp$hero[k],"|^y\\.",dftemp$hero[k]),names(df)) # find hero column name
                                df[i,j[1]]<-dftemp$x[k] # update
                                df[i,j[2]]<-dftemp$y[k]
                        }else if (dftemp$type[k]=="DAMAGE"){
                                # you received damage
                                if (!is.na(dftemp$heroReceiver[k]) & dftemp$targethero[k] ){
                                        if (dftemp$attackerhero[k]){ # from another hero
                                                j<-grep(paste0(dftemp$type[k],"TakenFromHero.",dftemp$heroReceiver[k]),names(df))
                                        }else{ # from something other than a hero
                                                j<-grep(paste0(dftemp$type[k],"TakenFromOther.",dftemp$heroReceiver[k]),names(df))
                                        }
                                        df[i,j]<-dftemp$value[k]
                                }
                                # you gave damage
                                if (!is.na(dftemp$heroAttacker[k]) & dftemp$attackerhero[k]  ){
                                        if (dftemp$targethero[k]){  # to another hero
                                                j<-grep(paste0(dftemp$type[k],"DealtToHero.",dftemp$heroAttacker[k]),names(df))
                                        }else{ # to something other than hero
                                                j<-grep(paste0(dftemp$type[k],"DealtToOther.",dftemp$heroAttacker[k]),names(df))
                                        }
                                        df[i,j]<-dftemp$value[k]
                                }
  
                        }else if (dftemp$type[k]=="DEATH"){
                                # you die
                                if (!is.na(dftemp$heroReceiver[k]) & dftemp$targethero[k]){
                                        #print(c(dftemp$heroReceiver[k],dftemp$targethero[k]))
                                        j<-grep(paste0("Deaths.",dftemp$heroReceiver[k]),names(df))
                                        df[i,j]<-1
                                }
                                # you kill another hero
                                if (!is.na(dftemp$heroAttacker[k]) & dftemp$attackerhero[k] ){
                                        if (dftemp$targethero[k]){
                                                # you kill hero
                                                j<-grep(paste0("KillsHero.",dftemp$heroAttacker[k]),names(df))
                                        }else{
                                                # you kill tower and /or other 
                                                if (grepl("tower",dftemp$targetsourcename)){
                                                        j<-grep(paste0("KillsTower.",dftemp$heroAttacker[k]),names(df))
                                                } else{
                                                        j<-grep(paste0("KillsOther.",dftemp$heroAttacker[k]),names(df))
                                                }
                                        }
                                        df[i,j]<-1
                                }
                        }
                }
        }
        df
}

normalize_XY<-function(dfbpos,coord,team,useS=T){
        # normalize positions to the team center of mass for each X and Y coordinate
        ind<-grepl(paste0(tolower(coord),"\\."),names(dfbpos)) & grepl(paste0("\\.",team),names(dfbpos)) # find relevant columns
        dfbpos[,paste0("cm.",coord,".",team)]<-rowMeans(dfbpos[,ind]) # determine the team CM
        dfbpos[,paste0("cmsd.",coord,".",team)]<-apply(dfbpos[,ind],1,sd) # determine the team standard dev
        
        newnames<-paste0("norm.",names(dfbpos)[ind]) # add new variables for normalized positions
        dfbpos[,newnames]<-dfbpos[,ind]-dfbpos[,paste0("cm.",coord,".",team)] # find recentered positions
        if (useS){ # if standard devs are used, then if the standard dev >0, rescale the coordinates
                ind<-dfbpos[,paste0("cmsd.",coord,".",team)]!=0
                dfbpos[ind,newnames]<-dfbpos[ind,newnames]/dfbpos[ind,paste0("cmsd.",coord,".",team)]
        }
        dfbpos
}
RawProcess<-function(dirfiles,stem){
        
        print(c("Working on match ID: ",strsplit(stem,"\\.")[[1]][1]))
        #files<-list.files(path=dirfiles,pattern="*\\_clean.json$",full.names=T)
        
        ##### read in/maniuplate json
        z<-jsonlite::fromJSON(paste0(dirfiles,stem))
        # simplify tag names and make new variables for the heroes involved in the event
        z<-z %>% mutate(hero=ifelse(grepl("DT_DOTA_Unit_Hero_",type),gsub("DT_DOTA_Unit_Hero_","",type),""),
                        type=ifelse(grepl("DT_DOTA_Unit_Hero_",type),"POSITION",type))
        z<-z %>% mutate(hero=ifelse(grepl("npc_dota_hero_",targetname),gsub("npc_dota_hero_","",targetname),hero),
                        heroAttacker=ifelse(grepl("npc_dota_hero_",sourcename),gsub("npc_dota_hero_","",sourcename),NA),
                        heroReceiver=ifelse(grepl("npc_dota_hero_",targetsourcename),gsub("npc_dota_hero_","",targetsourcename),NA))
        z$type<-gsub("DOTA_COMBATLOG_","",z$type)
        # normalize the hero names in order to compare by removing all punctuation, spacing, and lowering letters
        for (i in c("hero","heroAttacker","heroReceiver")){
                z[,i]<-tolower(gsub(" |\\_|'","",z[,i]))
        }
        z$match_id<-z$match_id[1] # in case there's a problem with match_ids, propogate down the df
        
        
        ##### determine which heroes are on which team
        heroteams<-unique(z %>% filter(!is.na(team)) %>% select(team,hero) %>% mutate(lowerhero=gsub(" |\\_","",hero)))[1:10,]
        
        # bring in external hero data to raw file in order to ID heroes from name to number:
        heroes<-fromJSON("/Users/asallaska/SFI/DOTA/heroes.json")
        heroes<-heroes$result$heroes
        heroes$lowerhero<-gsub("npc\\_dota\\_hero\\_","",heroes$name)
        heroes$lowerhero<-gsub(" |-|\\_|'","",heroes$lowerhero)
        heroes<-heroes %>% select(id,localized_name,lowerhero) %>% rename(hero_id=id,Hero=localized_name)# %>%
        heroteams<-left_join(heroteams,heroes,by="lowerhero")
        
        # bring in external aggregate data to determine which heroes on which team from draft
        agg<-fromJSON("/Users/asallaska/SFI/DOTA/data/raw/gameDetails.json")
        agg_ind<-which(names(agg)==unique(z$match_id)) # isolate the relevant entry in the json by match_id
        if (length(agg_ind)>0){
                agg_game<-agg[[agg_ind]] # isolate aggregated stats for relevant game
                gamestats<-data.frame(agg_game$match_id,agg_game$radiant_win,agg_game$duration,agg_game$radiant_score,
                                      agg_game$dire_score,agg_game$leagueid,agg_game$start_time,agg_game$lobby_type,
                                      agg_game$game_mode) # pick out potentially relevant stats
                names(gamestats)<-gsub("agg\\_game","stats",names(gamestats))
                
                # figure out which team is radiant and which is dire
                picks<-agg_game$picks_bans %>% filter(is_pick) %>% select(-is_pick,-order) %>% rename(team01=team)
                heroteams<-left_join(heroteams,picks,by="hero_id")
                heroteams<-heroteams %>% mutate(teamRD=ifelse(team01==0,"Radiant","Dire")) #%>% select(team,teamRD)
                z<-left_join(z,unique(select(heroteams,team,teamRD)),by="team")
                no_match<-F
        }else{
                # no match in aggregate data, so can only know which heroes on which team (team 2 or 3) and cannot correlate to wins
                z$teamRD<-z$team
                heroteams$teamRD<-heroteams$team
                no_match<-T
        }
        z<-unique(z) # remove duplicate rows
        
        df<-parallelBuild(z) # main function of this function.  reformat json into time series.
        
        # label the teams
        if (!no_match){
                team1<-"Radiant"
                team2<-"Dire"
        }else{
                team1<-"2"
                team2<-"3"
        }
        
        # add relative distances
        dfpos1<-finddist(df,team1,heroteams) # calculate relative distance for Radiant or team "2"
        dfpos2<-finddist(df,team2,heroteams) # calculate relative distance for Dire or team "3"
        dfb<-cbind(df,dfpos1,dfpos2) # bind all together
        
        dfbpos<-dfb[,c("match_id","tick","time",grep("^d\\.",names(dfb),value=T),grep("^x\\.|^y\\.",names(dfb),value=T))]
        
        useS<-T # if T, use standard deviation to normalize; otherwise, simply subtract off CM 
        # normalize each coordinate for each team to the team center of mass 
        dfbpos<-normalize_XY(dfbpos,"X",team1,useS)
        dfbpos<-normalize_XY(dfbpos,"Y",team1,useS)
        dfbpos<-normalize_XY(dfbpos,"X",team2,useS)
        dfbpos<-normalize_XY(dfbpos,"Y",team2,useS)
        
        # find position to filter out all rows until each hero has a definite position (they are zero until they appear on the game board)
        for (i in 1:nrow(dfbpos)){
                itest<-i
                test<-any(select(dfbpos,grep("^x\\.|^y\\.",names(dfbpos)))[i,]==0)
                if (!test) break
        }
        dfbpos<-dfbpos[itest:nrow(dfbpos),]
        
        # because eliminating the zero position rows above, need to take into account heroes that received gold 
        # during those eliminated rows/times.  this simply copies what they received previous to the first time step kept
        # and sums that up and copies into the first kept time step.  Most simply receive 625 gold pieces.  
        dfb_gold<-colSums(dfb[1:itest,grep("^net\\.GOLD\\.",names(dfb))])
        dfb<-dfb[itest:nrow(dfb),] # filter out rows
        # add 625 to net gold to who it's now cut off for
        ind<-grep("^net\\.GOLD\\.",names(dfb))
        dfb[1,ind]<-dfb_gold
        
        # bind normalized position info back to the main df
        dfb<-cbind(dfb,dfbpos[,grep("^norm\\.",names(dfbpos))])
        
        
        # add game stats, if match
        if (length(agg_ind)>0){
                for (j in names(gamestats)){
                        dfb[,j]<-gamestats[,j]
                }
        }

        #### NORMALIZE all variables but positions (normalized separately above)
        ### removed in order to allow flexibility with data handling down the line.  net variables
        # now normalized in AggregateGamesCluster.R

        # # normalize
        # ind<-grep("^net\\.",names(dfb))
        # # find mean, mu, from each column and standard dev, s
        # mu<-data.frame(t(colMeans(dfb[,ind])))
        # mu<-mu[rep(1,nrow(dfb)),]
        # s<-data.frame(t(apply(dfb[,ind],2,sd)))
        # s<-s[rep(1,nrow(dfb)),]
        # 
        # dfb[,ind]<-(dfb[,ind] - mu)/s
        
        dffinal<-dfb
        
        # write df to output file as csv and then zip, removing the csv
        newpath<-paste0(dirfiles,gsub("json","timeseries",stem),".csv")
        export(dffinal,newpath)
        zip(paste0(newpath,".zip"),newpath)
        file.remove(newpath)
        return(0)
        
        # to read: 
        
        #x<-read.csv(unzip(paste0(dirfiles,"772548096.dem.results.timeseries.csv.zip"),junkpaths = T))
        #file.remove("772548096.dem.results.timeseries.csv") # remove csv file created in working directory
}



### FILE HANDLING ###
dirfiles<-args[1] # read in directory from command line
kfiles<-dir(path=dirfiles,pattern = ".dem.results.json") # find files to convert from json to csv.zip
kfilescsv<-dir(path=dirfiles,pattern=".dem.results.timeseries.csv.zip") # find all already converted files
kfilescsv<-gsub(".timeseries.csv.zip",".json",kfilescsv)
kfiles<-outersect(kfiles,kfilescsv) # compare already converted files to file list and eliminate ones already converted so only new files are run

# find indicies to split data on, in order to parallel process
n<-c(seq(1,length(kfiles),8),length(kfiles))

# split up data frame into 8 parts in a list in order to parallel process
kfileslist<-lapply(1:(length(n)-1),function(i) kfiles[n[i]:(n[i+1]-1)])


### MAIN CODE ### 
# convert json into csv.zip for each section of the files, using multiple cores (8 below)
for (i in 1:length(n)){
        ls<-mclapply(1:length(kfileslist[[i]]),function(j) RawProcess(dirfiles,kfileslist[[i]][j]),mc.cores=8)
}
