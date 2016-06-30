require(parallel)
require(jsonlite)
require(grid)

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
        # find position columns
        if (any(grepl("\\.Radiant$",names(df)))==F){ #i.e., no match in aggregated data
                if (teamno=="Radiant"){
                        teamno<-"2"
                }else{
                        teamno<-"3"
                }
        }
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
parallelBuild<-function(z,parallel=F){
        
        # start structure
        df<-data.frame(unique(select(z,match_id:time)))
        newvar<-c("Kills","Deaths","DAMAGETaken","DAMAGEDealt","XP","GOLD")
        newvar<-paste0(rep(newvar,2),rep(c(".cum",".net"),each=length(newvar)))
        newvar<-c(newvar,"x","y")
        heroteamsI<-unique(z %>% filter(!is.na(teamRD)) %>% select(teamRD,hero) %>% mutate(hero=gsub(" |\\_|'","",hero)))[1:10,]
        heroteamsI<-heroteamsI %>% mutate(concat=paste(hero,teamRD,sep=".")) %>% arrange(teamRD,hero) 
        newvar<-paste(newvar,rep(heroteamsI$concat,each=length(newvar)),sep=".")
        for (j in newvar){
                df[,j]<-0
        }
        
        if (!parallel){
                df<-buildOut(df,z)
        }else{
        
                # find indicies to split on, in order to parallel process
                n<-floor(nrow(df)/8)
                n<-c(1,n*(1:7),nrow(df)+1)
                
                # split up data frame into 8 parts in a list in order to parallel process
                dflist<-lapply(1:(length(n)-1),function(i) df[n[i]:(n[i+1]-1),])
                # find states for each piece of data frame
                ls<-mclapply(1:length(dflist),function(i) buildOut(dflist[[i]],z),mc.cores=8)
                
                
                
                # bind each resulting data frame into a single data frame
                df<-do.call(rbind.data.frame,ls)
                
                for (i in 2:(length(n)-1)){
                        indafter<-which(df[n[i],]==0)
                        indbefore<-which(df[n[i]-1,]==0)
                        indconflict<-names(df)[outersect(indafter,indbefore)]
                        indpos<-grep("x\\.|y\\.",indconflict,value=T)
                        indother<-grep("x\\.|y\\.",indconflict,value=T,invert=T)
                        if (length(indpos)>0){
                                for (j in indpos){
                                        ict<-n[i]
                                        while (df[ict,j]==0){
                                                df[ict,j]<-df[ict-1,j]
                                                ict<-ict+1
                                        }
                                }
                        }
                        if (length(indother)>0){
                                for (j in indother){
                                        ict<-n[i]
                                        while (df[ict,j]==0){
                                                df[ict,j]<-df[ict-1,j]
                                                ict<-ict+1
                                        }
                                        df[ict,j]<-df[ict-1,j]+df[ict,j]
                                }
                        }
                        
                        
                }
        }
        df
}



buildOut<-function(df,z){
        
        for (i in 1:nrow(df)){#nrow(df)
                if (i %% 1000 ==0) print(paste(c("i = ",i," ",df$tick[i]," (",round(i/nrow(df)*100,1)," %)"),collapse=""))
                #print(i)
                dftemp<-filter(z,tick==df$tick[i] & time==df$time[i])
                if (i>1){
                        j<-grepl("GOLD|XP|DAMAGE|x\\.|y\\.|Kills|Deaths",names(df)) & grepl("cum",names(df))
                        df[i,j]<-df[(i-1),j]
                }
                for (k in 1:nrow(dftemp)){
                        #print(k)
                        if (grepl("GOLD|XP",dftemp$type[k])){
                                # increment gold or xp
                                jcum<-grep(paste0(dftemp$type[k],"\\.cum\\.",dftemp$hero[k]),names(df))
                                jnet<-grep(paste0(dftemp$type[k],"\\.net\\.",dftemp$hero[k]),names(df))
                                # this is to calculate total accumulated gold or XP, ignoring 
                                # gold taken away from battle, as reflected in damage
                                if (dftemp$value[k]>0){
                                        df[i,jcum]<-df[i,jcum]+dftemp$value[k]
                                        df[i,jnet]<-dftemp$value[k]
                                }else{
                                        df[i,jcum]<-df[i,jcum]
                                        df[i,jnet]<-df[i,jnet]
                                }
                        }else if (dftemp$type[k]=="POSITION"){
                                j<-grep(paste0("x\\.",dftemp$hero[k],"|y\\.",dftemp$hero[k]),names(df))
                                df[i,j[1]]<-dftemp$x[k]
                                df[i,j[2]]<-dftemp$y[k]
                        }else if (dftemp$type[k]=="DAMAGE"){
                                if (!is.na(dftemp$heroReceiver[k])){
                                        jcum<-grep(paste0(dftemp$type[k],"Taken.cum.",dftemp$heroReceiver[k]),names(df))
                                        df[i,jcum]<-df[i,jcum]+dftemp$value[k]
                                        jnet<-grep(paste0(dftemp$type[k],"Taken.net.",dftemp$heroReceiver[k]),names(df))
                                        df[i,jnet]<-dftemp$value[k]
                                }
                                if (!is.na(dftemp$heroAttacker[k])){
                                        jcum<-grep(paste0(dftemp$type[k],"Dealt.cum.",dftemp$heroAttacker[k]),names(df))
                                        df[i,jcum]<-df[i,jcum]+dftemp$value[k]
                                        jnet<-grep(paste0(dftemp$type[k],"Dealt.net.",dftemp$heroAttacker[k]),names(df))
                                        df[i,jnet]<-dftemp$value[k]
                                }
                        }else if (dftemp$type[k]=="DEATH"){
                                if (!is.na(dftemp$heroReceiver[k])){
                                        jcum<-grep(paste0("Deaths.cum.",dftemp$heroReceiver[k]),names(df))
                                        df[i,jcum]<-df[i,jcum]+1
                                        jnet<-grep(paste0("Deaths.net.",dftemp$heroReceiver[k]),names(df))
                                        df[i,jnet]<-1
                                }
                                if (!is.na(dftemp$heroAttacker[k])){
                                        jcum<-grep(paste0("Kills.cum.",dftemp$heroAttacker[k]),names(df))
                                        df[i,jcum]<-df[i,jcum]+1
                                        jnet<-grep(paste0("Kills.net.",dftemp$heroAttacker[k]),names(df))
                                        df[i,jnet]<-1
                                }
                        }
                }
                
        }
        df
}
splitdf<-function(df,varisolate,heroes,single=F,typeCN="cum"){
        if (!single){
                dfsub<-df[,c("match_id","tick","time",grep(paste0(varisolate,".",typeCN,"."),names(df),value=T))]
                names(dfsub)<-gsub(paste0("\\.",typeCN),"",names(dfsub))
        }else{
                dfsub<-df[,c("match_id","tick","time",grep(varisolate,names(df),value=T))]
        }
        dfsub_melt<-gather(dfsub,heroparam,value,-match_id,-tick,-time)
        if (single){
                intovar<-c("param","teamRD")
        }else{
                intovar<-c("param","lowerhero","teamRD")
        }
        dfsub_melt<-separate(dfsub_melt,heroparam,into=intovar,sep="\\.")
        #dfsub_melt<-dfsub_melt %>% mutate(team=ifelse(team=="2","Team 2","Team 3"))
        if (!single){
                heroes<-heroes %>% select(-hero_id)
                dfsub_melt<-left_join(dfsub_melt,heroes,by="lowerhero")
                dfsub_melt<-select(dfsub_melt,-lowerhero)
        }
        dfsub_melt
}
evolutionPlot<-function(df_var,single=F,leg=F,cum=F){
        g<-ggplot(df_var,aes(x=tick,y=value))
        if (!single){
                g<-g+geom_line(aes(col=Hero))+facet_wrap(~teamRD)
                if (cum){
                        cname<-"Cumulative "
                }else{
                        cname<-"Net "
                }
        }else{
                g<-g+geom_line(aes(col=teamRD))
                cname=""
        }

        g<-g+theme_light()+labs(x="Time (tick)",y=df_var$param[1])+
                ggtitle(paste0(cname,"Evolution of ",df_var$param[1])) #," for Match ID ",df_var$match_id[1])
        if (!leg){
                g<-g+guides(col=F)
        }
        g    
}




dirfiles<-"/Users/asallaska/SFI/DOTA/data/raw/full/TI5/PlayoffsDay7/"
#dirfiles<-"/Users/asallaska/SFI/DOTA/data/raw/full/TestGames/"
stem<-"776170591.dem.results.json"
#stem<-"662332063.dem.results.json"

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

# combat



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
}else{
        z$teamRD<-z$team
        heroteams$teamRD<-heroteams$team
}






df<-parallelBuild(z)


# add relative distances
dfpos1<-finddist(df,"Radiant",heroteams) # calculate relative distance for Radiant
dfpos2<-finddist(df,"Dire",heroteams) # calculate relative distance for Dire
dfb<-cbind(df,dfpos1,dfpos2) # bind all together

# add game stats, if match
if (length(agg_ind)>0){
        for (j in names(gamestats)){
                dfb[,j]<-gamestats[,j]
        }
}

# j<-grepl("GOLD|XP|x\\.|y\\.",names(df))
# colMeans(df[,j]) 

# normalize
mu<-data.frame(t(colMeans(select(dfb,-(match_id:time),-(radiant_win:game_mode)))))
mu<-mu[rep(1,nrow(dfb)),]
s<-data.frame(t(apply(select(dfb,-(match_id:time),-(radiant_win:game_mode)),2,sd)))
s<-s[rep(1,nrow(dfb)),]

dfnorm<-dfb
ind<-names(mu)
dfnorm[,ind]<-(dfnorm[,ind] - mu)/s


ggold<-evolutionPlot(splitdf(dfb,"GOLD",heroes,typeCN="net"),leg=F,cum=F)
gxp<-evolutionPlot(splitdf(dfb,"XP",heroes,typeCN="net"),leg=T,cum=F)
gdeath<-evolutionPlot(splitdf(dfb,"Deaths",heroes,typeCN="net"),leg=F,cum=F)
gkills<-evolutionPlot(splitdf(dfb,"Kills",heroes,typeCN="net"),leg=F,cum=F)
gavgDist<-evolutionPlot(splitdf(dfb,"avgDist",single=T),single=T,leg=T)

ggold<-evolutionPlot(splitdf(dfb,"GOLD",heroes,typeCN="cum"),leg=F,cum=T)
gxp<-evolutionPlot(splitdf(dfb,"XP",heroes,typeCN="cum"),leg=T,cum=T)
gdeath<-evolutionPlot(splitdf(dfb,"Deaths",heroes,typeCN="cum"),leg=F,cum=T)
gkills<-evolutionPlot(splitdf(dfb,"Kills",heroes,typeCN="cum"),leg=F,cum=T)
#gavgDist<-evolutionPlot(splitdf(dfb,"avgDist",single=T),single=T,leg=T)


pdf("game_timeseries.pdf",width=14,height=8)
grid.arrange(gavgDist,ggold,gxp,gdeath,gkills,ncol=3,
        top=textGrob(paste0("Match ID: ",unique(dfb$match_id),"; Radiant Win: ",unique(dfb$radiant_win)),gp=gpar(fontface="bold",cex=1.4)))
dev.off()

pdf("game_timeseries_net.pdf",width=14,height=8)
grid.arrange(ggold,gxp,gdeath,gkills,ncol=2,
             top=textGrob(paste0("Match ID: ",unique(dfb$match_id),"; Radiant Win: ",unique(dfb$radiant_win)),gp=gpar(fontface="bold",cex=1.4)))
dev.off()

