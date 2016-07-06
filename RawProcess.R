require(parallel)
require(jsonlite)
require(grid)
require(rio)

args<-commandArgs(TRUE)
if (length(args)==0) {
        stop("At least one argument must be supplied (directory name).n", call.=FALSE)
} 

disthero<-function(df,i1,j1,i2,j2){
        # calculate euclidean distance from df for all rows
        x1<-df[,i1]
        x2<-df[,i2]
        y1<-df[,j1]
        y2<-df[,j2]
        dist<-sqrt((y2-y1)^2+(x2-x1)^2)
        dist
}

finddist<-function(df,teamno,heroteams){
        # # find position columns
        # if (any(grepl("\\.Radiant$",names(df)))==F){ #i.e., no match in aggregated data
        #         if (teamno=="Radiant"){
        #                 teamno<-"2"
        #         }else{
        #                 teamno<-"3"
        #         }
        # }
        dfpos<-df[,grepl("x\\.|y\\.",names(df)) & grepl(paste0("\\.",teamno),names(df))]
        # create combinations of heroes to calculate distance 
        c<-combn((heroteams %>% filter(teamRD==teamno))$lowerhero,2,simplify = F)
        # make the combinations nice to make as column names
        cname<-sapply(c,function(i) paste(i,collapse="_"))
        cname<-paste0("d.",cname,".",teamno)
        # loop through each pair to calculate the distance for all rows at once
        for (i in 1:length(c)){
                ind<-grep(paste(c[[i]],collapse="|"),names(dfpos))
                dfpos[,cname[i]]<-disthero(dfpos,ind[1],ind[2],ind[3],ind[4])
        }
        temp<-dfpos[,grep("_",names(dfpos))]
        temp[,paste0("avgDist.",teamno)]<-apply(temp,1,mean)
        temp
}
parallelBuild<-function(z){
        
        # start structure
        df<-data.frame(unique(select(z,match_id:time)))
        newvar<-c("KillsHero","KillsOther","KillsTower","Deaths",
                  "DAMAGETakenFromHero","DAMAGETakenFromOther","DAMAGEDealtToHero","DAMAGEDealtToOther","XP","GOLD")
        #newvar<-paste0(rep(newvar,2),rep(c(".cum",".net"),each=length(newvar)))
        newvar<-c(newvar,"x","y")
        heroteamsI<-unique(z %>% filter(!is.na(teamRD)) %>% select(teamRD,hero) %>% mutate(hero=gsub(" |\\_|'","",hero)))[1:10,]
        heroteamsI<-heroteamsI %>% mutate(concat=paste(hero,teamRD,sep=".")) %>% arrange(teamRD,hero) 
        newvar<-paste(newvar,rep(heroteamsI$concat,each=length(newvar)),sep=".")
        for (j in newvar){
                df[,j]<-0
        }
        
 
        
        # find indicies to split on, in order to parallel process
        n<-floor(nrow(df)/8)
        n<-c(1,n*(1:7),nrow(df)+1)
        
        # split up data frame into 8 parts in a list in order to parallel process
        dflist<-lapply(1:(length(n)-1),function(i) df[n[i]:(n[i+1]-1),])
        # find states for each piece of data frame
        ls<-mclapply(1:length(dflist),function(i) buildOut(dflist[[i]],z),mc.cores=8)
        
        
        
        # bind each resulting data frame into a single data frame
        df<-do.call(rbind.data.frame,ls)
        
        
        dfcum<-select(df,-(match_id:time))
        dfnet<-select(dfcum,grep("x\\.|y\\.",names(dfcum),invert=T))
        names(dfnet)<-paste0("net.",names(dfnet))
        indpos<-grepl("x\\.|y\\.",names(dfcum))
        names(dfcum)[!indpos]<-paste0("cum.",names(dfcum)[!indpos])
        dfprev<-select(df,match_id:time)
        
        
        # propogate locations
        dfpos<-dfcum[,indpos]
        dfnopos<-dfcum[,!indpos]
        for (i in 2:nrow(dfcum)){
                if (i %% 1000 ==0) print(paste(c("i = ",i," ",dfcum$tick[i]," (",round(i/nrow(dfcum)*100,1)," %)"),collapse=""))
                
                ind<-which(dfpos[i,]==0)
                dfpos[i,ind]<-dfpos[(i-1),ind]
                dfnopos[i,]<-dfnopos[i,]+dfnopos[(i-1),]
        }
        dfcum<-cbind(dfpos,dfnopos)
        df<-cbind(dfprev,dfcum,dfnet)
                

        
        df
}



buildOut<-function(df,z){
        
        for (i in 1:nrow(df)){#nrow(df)
                #if (i %% 1000 ==0) print(paste(c("i = ",i," ",df$tick[i]," (",round(i/nrow(df)*100,1)," %)"),collapse=""))
                dftemp<-filter(z,tick==df$tick[i] & time==df$time[i])
                # if (i>1){
                #         j<-(grepl("GOLD|XP|DAMAGE|Kills|Deaths",names(df)) & grepl("cum",names(df))) | grepl("x\\.|y\\.",names(df))
                #         df[i,j]<-df[(i-1),j]
                # }
                #print(i)
                for (k in 1:nrow(dftemp)){
                        if (grepl("GOLD|XP",dftemp$type[k])){
                                # increment gold or xp
                                j<-grep(paste0(dftemp$type[k],"\\.",dftemp$hero[k]),names(df))
                                # this is to calculate total accumulated gold or XP, ignoring 
                                # gold taken away from battle, as reflected in damage
                                df[i,j]<-dftemp$value[k]

                        }else if (dftemp$type[k]=="POSITION"){
                                j<-grep(paste0("x\\.",dftemp$hero[k],"|y\\.",dftemp$hero[k]),names(df))
                                df[i,j[1]]<-dftemp$x[k]
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
        ind<-grepl(paste0(tolower(coord),"\\."),names(dfbpos)) & grepl(paste0("\\.",team),names(dfbpos))
        dfbpos[,paste0("cm.",coord,".",team)]<-rowMeans(dfbpos[,ind])
        dfbpos[,paste0("cmsd.",coord,".",team)]=apply(dfbpos[,ind],1,sd)
        
        newnames<-paste0("norm.",names(dfbpos)[ind])
        dfbpos[,newnames]<-dfbpos[,ind]-dfbpos[,paste0("cm.",coord,".",team)]
        if (useS){
                ind<-dfbpos[,paste0("cmsd.",coord,".",team)]!=0
                dfbpos[ind,newnames]<-dfbpos[ind,newnames]/dfbpos[ind,paste0("cmsd.",coord,".",team)]
        }
        dfbpos
}


#dirfiles<-"/Users/asallaska/SFI/DOTA/data/raw/full/TI5/PlayoffsDay7/"
#dirfiles<-"/Users/asallaska/SFI/DOTA/data/raw/full/TestGames/"
#stem<-"776170591.dem.results.json"
#stem<-"662332063.dem.results.json"
dirfiles<-args[1]
kfiles<-dir(path=dirfiles,pattern = ".dem.results.json")
kfilescsv<-dir(path=dirfiles,pattern=".dem.results.timeseries.csv.zip")
kfilescsv<-gsub(".timeseries.csv.zip",".json",kfilescsv)
kfiles<-outersect(kfiles,kfilescsv)

for (stem in kfiles){
        #stem<-"772548096.dem.results.json"
        
        print(c("Working on match ID: ",strsplit(stem,"\\.")[[1]][1]))
        #files<-list.files(path=dirfiles,pattern="*\\_clean.json$",full.names=T)
        
        z<-jsonlite::fromJSON(paste0(dirfiles,stem))
        z<-z %>% mutate(hero=ifelse(grepl("DT_DOTA_Unit_Hero_",type),gsub("DT_DOTA_Unit_Hero_","",type),""),
                type=ifelse(grepl("DT_DOTA_Unit_Hero_",type),"POSITION",type))
        z<-z %>% mutate(hero=ifelse(grepl("npc_dota_hero_",targetname),gsub("npc_dota_hero_","",targetname),hero),
                        heroAttacker=ifelse(grepl("npc_dota_hero_",sourcename),gsub("npc_dota_hero_","",sourcename),NA),
                        heroReceiver=ifelse(grepl("npc_dota_hero_",targetsourcename),gsub("npc_dota_hero_","",targetsourcename),NA))
        z$type<-gsub("DOTA_COMBATLOG_","",z$type)
        for (i in c("hero","heroAttacker","heroReceiver")){
                z[,i]<-tolower(gsub(" |\\_|'","",z[,i]))
        }
        
        
        heroteams<-unique(z %>% filter(!is.na(team)) %>% select(team,hero) %>% mutate(lowerhero=gsub(" |\\_","",hero)))[1:10,]
        
        # bring in external data to raw file:
        agg<-fromJSON("/Users/asallaska/SFI/DOTA/data/raw/gameDetails.json")
        heroes<-fromJSON("/Users/asallaska/SFI/DOTA/heroes.json")
        heroes<-heroes$result$heroes
        heroes<-heroes %>% select(id,localized_name) %>% rename(hero_id=id,Hero=localized_name) %>%
                mutate(lowerhero=tolower(gsub(" |-|\\_|'","",Hero))) %>% 
                mutate(lowerhero=ifelse(lowerhero=="naturesprophet","furion",lowerhero)) # note exception for furion/naturesprophet
        heroteams<-left_join(heroteams,heroes,by="lowerhero")
        agg_ind<-which(names(agg)==unique(z$match_id))
        if (length(agg_ind)>0){
                agg_game<-agg[[agg_ind]] # isolate aggregated stats for relevant game
                gamestats<-data.frame(agg_game$match_id,agg_game$radiant_win,agg_game$duration,agg_game$radiant_score,
                                      agg_game$dire_score,agg_game$leagueid,agg_game$start_time,agg_game$lobby_type,
                                      agg_game$game_mode)
                names(gamestats)<-gsub("agg\\_game\\.","",names(gamestats))
                
                # figure out which team is radiant and which is dire
                picks<-agg_game$picks_bans %>% filter(is_pick) %>% select(-is_pick,-order) %>% rename(team01=team)
                heroteams<-left_join(heroteams,picks,by="hero_id")
                heroteams<-heroteams %>% mutate(teamRD=ifelse(team01==0,"Radiant","Dire")) #%>% select(team,teamRD)
                z<-left_join(z,unique(select(heroteams,team,teamRD)),by="team")
                no_match<-F
        }else{
                z$teamRD<-z$team
                heroteams$teamRD<-heroteams$team
                no_match<-T
        }
        z<-unique(z) # remove duplicate rows
        
        df<-parallelBuild(z)

        
        if (!no_match){
                team1<-"Radiant"
                team2<-"Dire"
        }else{
                team1<-"2"
                team2<-"3"
        }
        
        # add relative distances
        dfpos1<-finddist(df,team1,heroteams) # calculate relative distance for Radiant
        dfpos2<-finddist(df,team2,heroteams) # calculate relative distance for Dire
        dfb<-cbind(df,dfpos1,dfpos2) # bind all together
        
        dfbpos<-dfb[,c("match_id","tick","time",grep("^d\\.",names(dfb),value=T),grep("x\\.|y\\.",names(dfb),value=T))]
        
        useS<-T
        
        
        dfbpos<-normalize_XY(dfbpos,"X",team1,useS)
        dfbpos<-normalize_XY(dfbpos,"Y",team1,useS)
        dfbpos<-normalize_XY(dfbpos,"X",team2,useS)
        dfbpos<-normalize_XY(dfbpos,"Y",team2,useS)
        
        
        
        
        # filter out 0 locations
        for (i in 1:nrow(dfbpos)){
                itest<-i
                test<-any(select(dfbpos,grep("^x\\.|^y\\.",names(dfbpos)))[i,]==0)
                if (!test) break
        }
        dfbpos<-dfbpos[itest:nrow(dfbpos),]
        dfb<-dfb[itest:nrow(dfb),]
        
        # add game stats, if match
        if (length(agg_ind)>0){
                for (j in names(gamestats)){
                        dfb[,j]<-gamestats[,j]
                }
        }
        
        # j<-grepl("GOLD|XP|x\\.|y\\.",names(df))
        # colMeans(df[,j]) 
        
        
        #### NORMALIZE all but positions (normalized separately)
        dfc<-dfb[,grep("^net\\.",names(dfb),value=T)]
        
        # normalize
        ind<-grep("^net\\.",names(dfc))
        # find mean, mu, from each column and standard dev, s
        mu<-data.frame(t(colMeans(dfc[,ind])))
        mu<-mu[rep(1,nrow(dfc)),]
        s<-data.frame(t(apply(dfc[,ind],2,sd)))
        s<-s[rep(1,nrow(dfc)),]
         
        dfc[,ind]<-(dfc[,ind] - mu)/s
        
        dffinal<-cbind(dfb,dfc)
        
        #write.csv(dffinal,gzfile(paste0(dirfiles,gsub("json","timeseries",stem),".csv.gz")))
        newpath<-paste0(dirfiles,gsub("json","timeseries",stem),".csv")
        export(dffinal,newpath)
        zip(paste0(newpath,".zip"),newpath)
        file.remove(newpath)
        
        # to read: 

        #x<-read.csv(unzip(paste0(dirfiles,"772548096.dem.results.timeseries.csv.zip"),junkpaths = T))
        #file.remove("772548096.dem.results.timeseries.csv") # remove csv file created in working directory
}