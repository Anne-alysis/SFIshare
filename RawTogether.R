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
                if (i %% 1000 ==0) print(paste(c("i = ",i," ",df$tick[i]," (",round(i/nrow(df)*100,1)," %)"),collapse=""))
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
                                        print(c(dftemp$heroReceiver[k],dftemp$targethero[k]))
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
splitdf<-function(df,varisolate,heroes,single=F,typeCN="cum"){
        if (!single){
                dfsub<-df[,c("match_id","tick","time",grep(paste0(typeCN,".",varisolate,"."),names(df),value=T))]
                names(dfsub)<-gsub(paste0(typeCN,"\\."),"",names(dfsub))
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
evolutionPlot<-function(df_var,single=F,leg=F,cum=F,tit=T){
        g<-ggplot(df_var,aes(x=tick,y=value))
        if (!single){
                g<-g+geom_line(aes(col=Hero))+facet_wrap(~teamRD)
                if (cum){
                        cname<-"Cumulative "
                }else{
                        cname<-"Net "
                }
        }else{
                g<-g+geom_line(aes(col=teamRD))+scale_color_discrete(name="Team")
                cname=""
                df_var$param[1]<-"Average Distance"
        }

        g<-g+theme_light()+labs(x="Time (tick)",y=df_var$param[1])
        if (tit) g<-g+ggtitle(paste0(cname,"Evolution of ",df_var$param[1])) #," for Match ID ",df_var$match_id[1])
        if (!leg) g<-g+guides(col=F)

        g    
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
explained_var<-function(data,i,itermax){
        set.seed(1)
        k<-kmeans(data,i,itermax)
        k$betweenss/k$totss
}
clustvarfun<-function(kmax,dire,radiant,itermax=10){
        clustvar<-data.frame(k=1:kmax)
        clustvar$Dire<-sapply(clustvar$k,function(i) explained_var(dire,i,itermax))
        clustvar$Radiant<-sapply(clustvar$k,function(i) explained_var(radiant,i,itermax))
        clustvar<-gather(clustvar,Team,Variance,-k)
        clustvar
}


dirfiles<-"/Users/asallaska/SFI/DOTA/data/raw/full/TI5/PlayoffsDay7/"
#dirfiles<-"/Users/asallaska/SFI/DOTA/data/raw/full/TestGames/"
#stem<-"776170591.dem.results.json"
#stem<-"662332063.dem.results.json"
stem<-"772548096.dem.results.json"
stem<-"776068193.dem.results.json"

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



dfbpos$seq<-1:nrow(dfbpos)

dfnormX<-dfbpos[,c("seq","tick","time",grep("norm.x",names(dfbpos),value=T))]
dfnormY<-dfbpos[,c("seq","tick","time",grep("norm.y",names(dfbpos),value=T))]
dfnormX_melt<-gather(dfnormX,pos,value,-(seq:time))
dfnormY_melt<-gather(dfnormY,pos,value,-(seq:time))
dfnormX_melt<-dfnormX_melt %>% separate(pos,into=c("v","w","hero","team"),sep="\\.") 
dfnormX_melt<-dfnormX_melt %>% select(-v,-w) %>% rename(x=value)
dfnormY_melt<-dfnormY_melt %>% separate(pos,into=c("v","w","hero","team"),sep="\\.") 
dfnormY_melt<-dfnormY_melt %>% select(-v,-w) %>% rename(y=value)

dfnorm_melt<-cbind(dfnormX_melt,select(dfnormY_melt,y))
dfnorm_melt<-arrange(dfnorm_melt,seq)

g<-ggplot(filter(dfnorm_melt,seq<=1000),aes(x=x,y=y,group=seq))+geom_line(aes(col=team))
g
g<-ggplot(filter(dfnorm_melt,seq<=10000),aes(x=x,y=y,group=seq))+geom_point(aes(col=team),alpha=0.5)
#g




# add game stats, if match
if (length(agg_ind)>0){
        for (j in names(gamestats)){
                dfb[,j]<-gamestats[,j]
        }
}

# j<-grepl("GOLD|XP|x\\.|y\\.",names(df))
# colMeans(df[,j]) 


#### CLUSTERING
# for clusters, pick out net variables as well as centered positions:
dfc<-cbind(dfb[,c("match_id","tick","time",grep("^net\\.",names(dfb),value=T))],dfbpos[,grep("^norm\\.",names(dfbpos))])

# normalize
ind<-grep("^net\\.",names(dfc))
# find mean, mu, from each column and standard dev, s
mu<-data.frame(t(colMeans(dfc[,ind])))
mu<-mu[rep(1,nrow(dfc)),]
s<-data.frame(t(apply(dfc[,ind],2,sd)))
s<-s[rep(1,nrow(dfc)),]
 
dfc[,ind]<-(dfc[,ind] - mu)/s

# some tower kills yield NA when normalized so remove them for simplicity
#dfc<-dfc[,grep("Tower\\.",names(dfc),invert=T)]



dfc_bare_dire<-dfc[,grep("\\.Dire",names(dfc))]
dfc_bare_radiant<-dfc[,grep("\\.Radiant",names(dfc))]

dfc_bare_dire_xy<-dfc_bare_dire[,grep("^norm\\.",names(dfc_bare_dire))]
dfc_bare_radiant_xy<-dfc_bare_radiant[,grep("^norm\\.",names(dfc_bare_radiant))]


dfc_bare_dire_x<-dfc_bare_dire[,grep("^norm\\.x\\.",names(dfc_bare_dire))]
dfc_bare_radiant_x<-dfc_bare_radiant[,grep("^norm\\.x\\.",names(dfc_bare_radiant))]




# remove columns which have NAs in their normalized values (i.e., one hero never died,
# no towers killed)
ind<-names(which(colSums(is.na(dfc_bare_dire))==0))
dfc_bare_dire<-dfc_bare_dire[,ind]
ind<-names(which(colSums(is.na(dfc_bare_radiant))==0))
dfc_bare_radiant<-dfc_bare_radiant[,ind]

clustvar_xy<-clustvarfun(100,dfc_bare_dire_xy,dfc_bare_radiant_xy,50)
clustvar<-clustvarfun(100,dfc_bare_dire,dfc_bare_radiant,50)
clustvar_xy<-clustvar_xy %>% mutate(Type="Normalized Positions\nOnly")
clustvar<-clustvar %>% mutate(Type="All Variables")

# g<-ggplot(clustvarTot,aes(x=k,y=Variance,group=Type))+
#         geom_point(aes(col=Team,shape=Type))+theme_light()+
#         labs(x="Number of Clusters",y="Inter-cluster Distance / Total Variance")
# g



g<-ggplot(clustvar,aes(x=k,y=Variance))+geom_line(aes(color=Team))+
        geom_point(aes(col=Team,shape=Type),size=2.5)+theme_light()+
        labs(x="Number of Clusters",y="Inter-cluster Distance / Total Variance")+
        geom_line(data=clustvar_xy,mapping=aes(x=k,y=Variance,col=Team))+
        geom_point(data=clustvar_xy,mapping=aes(x=k,y=Variance,col=Team,shape=Type),size=2.5)

g

pdf("Cluster_variance.pdf")
g
dev.off()

k<-kmeans(dfc_bare_dire_xy,25,50)

test<-data.frame(seqno=1:nrow(yteamD_5),cluster=k$cluster,state=yteamD_5$num)
test<-gather(test,type,value,-seqno)
g<-ggplot(test,aes(x=seqno,value))+geom_line(aes(col=type))+
        facet_grid(type~.,scales="free_y")+theme_light()+labs(x="Time Step",y="State or Cluster")
g
pdf("StateVsCluster_25.pdf")
g
dev.off()


library(NbClust)

NbClust(dfc_bare_dire_d,method="kmeans")








######



ggold<-evolutionPlot(splitdf(dfb,"GOLD",heroes,typeCN="net"),leg=T,cum=F,tit=T)
gxp<-evolutionPlot(splitdf(dfb,"XP",heroes,typeCN="net"),leg=T,cum=F,tit=T)
gdeath<-evolutionPlot(splitdf(dfb,"Deaths",heroes,typeCN="net"),leg=T,cum=F)
gkillshero<-evolutionPlot(splitdf(dfb,"KillsHero",heroes,typeCN="net"),leg=F,cum=F)
gkillstower<-evolutionPlot(splitdf(dfb,"KillsTower",heroes,typeCN="net"),leg=F,cum=F)
gkillsother<-evolutionPlot(splitdf(dfb,"KillsOther",heroes,typeCN="net"),leg=F,cum=F)
gdamagedealttohero<-evolutionPlot(splitdf(dfb,"DAMAGEDealtToHero",heroes,typeCN="net"),leg=F,cum=F)
gdamagedealttoother<-evolutionPlot(splitdf(dfb,"DAMAGEDealtToOther",heroes,typeCN="net"),leg=F,cum=F)
gdamagetakenfromhero<-evolutionPlot(splitdf(dfb,"DAMAGETakenFromHero",heroes,typeCN="net"),leg=F,cum=F)
gdamagetakenfromother<-evolutionPlot(splitdf(dfb,"DAMAGETakenFromOther",heroes,typeCN="net"),leg=F,cum=F)
gavgDistg<-evolutionPlot(splitdf(dfb,"avgDist",single=T),single=T,leg=T)

ggold<-evolutionPlot(splitdf(dfb,"GOLD",heroes,typeCN="cum"),leg=F,cum=T)
gxp<-evolutionPlot(splitdf(dfb,"XP",heroes,typeCN="cum"),leg=F,cum=T)
gdeath<-evolutionPlot(splitdf(dfb,"Deaths",heroes,typeCN="cum"),leg=T,cum=T)
gkillshero<-evolutionPlot(splitdf(dfb,"KillsHero",heroes,typeCN="cum"),leg=F,cum=T)
gkillstower<-evolutionPlot(splitdf(dfb,"KillsTower",heroes,typeCN="cum"),leg=F,cum=T)
gkillsother<-evolutionPlot(splitdf(dfb,"KillsOther",heroes,typeCN="cum"),leg=F,cum=T)
gdamagedealttohero<-evolutionPlot(splitdf(dfb,"DAMAGEDealtToHero",heroes,typeCN="cum"),leg=F,cum=T)
gdamagedealttoother<-evolutionPlot(splitdf(dfb,"DAMAGEDealtToOther",heroes,typeCN="cum"),leg=F,cum=T)
gdamagetakenfromhero<-evolutionPlot(splitdf(dfb,"DAMAGETakenFromHero",heroes,typeCN="cum"),leg=F,cum=T)
gdamagetakenfromother<-evolutionPlot(splitdf(dfb,"DAMAGETakenFromOther",heroes,typeCN="cum"),leg=F,cum=T)
# 
# #gavgDist<-evolutionPlot(splitdf(dfb,"avgDist",single=T),single=T,leg=T)
# # 
# # 
  pdf("game_timeseries_776170591_net.pdf",width=14,height=10)
   grid.arrange(ggold,gxp,gdeath,gkillshero,gkillstower,gkillsother,gdamagedealttohero,gdamagedealttoother,
                gdamagetakenfromhero,gdamagetakenfromother,ncol=4,
           top=textGrob(paste0("Match ID: ",unique(dfb$match_id),"; Radiant Win: ",unique(dfb$radiant_win)),gp=gpar(fontface="bold",cex=1.4)))
   dev.off()
# # 
# # pdf("game_timeseries_net.pdf",width=14,height=8)
# # grid.arrange(ggold,gxp,gdeath,gkills,ncol=2,
# #              top=textGrob(paste0("Match ID: ",unique(dfb$match_id),"; Radiant Win: ",unique(dfb$radiant_win)),gp=gpar(fontface="bold",cex=1.4)))
# # dev.off()
# 
#    
#    
# #relative
dfbpos_melt<-gather(dfbpos[,c("tick","time",grep("^d\\.",names(dfbpos),value=T))],pos,value,-tick,-time)
dfbpos_melt<-separate(dfbpos_melt,pos,into=c("d","Pair","Team"),sep="\\.")
dfbpos_melt$Pairsplit<-dfbpos_melt$Pair
dfbpos_melt<-separate(dfbpos_melt,Pairsplit,into=c("P1","P2"),sep="\\_")
# relative distances among teams
g<-ggplot(dfbpos_melt,aes(x=tick,y=value))+geom_line(aes(col=Pair))+
        facet_wrap(P1~Team)+theme_bw()+guides(col=guide_legend(ncol=1))
g


g<-ggplot(filter(dfbpos_melt,Team=="Dire" & P1=="luna"),aes(x=tick,y=value))+geom_line(aes(col=Pair))+
        facet_wrap(P1~Team)+theme_bw()+guides(col=guide_legend(ncol=1))+
        labs(x="Time (tick)",y="Relative Distance (arb)",title="")

pdf("luna_dire_distances.pdf")
g
dev.off()

pdf("game_timeseries_772548096_distances.pdf",width=14,height=14)
grid.arrange(g,gavgDist,ncol=1,
             top=textGrob(paste0("Match ID: ",unique(dfb$match_id),"; Radiant Win: ",unique(dfb$radiant_win)),gp=gpar(fontface="bold",cex=1.4)))
dev.off()


pdf("XP_example.pdf")
gxp
dev.off()

pdf("Avg_dist_example.pdf")
gavgDistg
dev.off()


####### MOVIE#######################################################
library(animation)
# movie
dfnormX<-dfbpos[,c("seq","tick","time",grep("^x\\.",names(dfbpos),value=T))]
dfnormY<-dfbpos[,c("seq","tick","time",grep("^y\\.",names(dfbpos),value=T))]
dfnormX_melt<-gather(dfnormX,pos,value,-(seq:time))
dfnormY_melt<-gather(dfnormY,pos,value,-(seq:time))
dfnormX_melt<-dfnormX_melt %>% separate(pos,into=c("w","hero","team"),sep="\\.") 
dfnormX_melt<-dfnormX_melt %>% select(-w) %>% rename(x=value)
dfnormY_melt<-dfnormY_melt %>% separate(pos,into=c("w","hero","team"),sep="\\.") 
dfnormY_melt<-dfnormY_melt %>% select(-w) %>% rename(y=value)

dfnorm_melt<-cbind(dfnormX_melt,select(dfnormY_melt,y))
dfnorm_melt<-arrange(dfnorm_melt,seq)

g<-ggplot(filter(dfnorm_melt, tick==19975),aes(x=x,y=y))+geom_point(aes(col=hero,shape=team),size=3)+
        theme_light()

g

# tick
rangex<-range(dfnorm_melt$x)
rangey<-range(dfnorm_melt$y)
dfnorm_melt$hero<-factor(dfnorm_melt$hero,levels = dfnorm_melt$hero[1:10])
gameboard<-function(i){
        g<-ggplot(filter(dfnorm_melt, tick==i),aes(x=x,y=y))+
                geom_point(aes(col=hero,shape=team),size=4)+
                theme_light()+scale_y_continuous(limits=rangey)+
                scale_x_continuous(limits=rangex)+scale_color_brewer(palette="RdBu")+
                guides(col=guide_legend(ncol=2))
        g
}
g<-lapply(unique(dfnorm_melt$tick)[1:1000],gameboard)
saveVideo({
        lapply(g,print)
        
}, video.name = "Game.mp4", other.opts = "-pix_fmt yuv420p -b 300k")



# time
dfsum<-summarize(group_by(dfnorm_melt,time,team,hero),xm=mean(x),ym=mean(y))
rangex<-range(dfsum$x)
rangey<-range(dfsum$y)
dfsum$hero<-factor(dfsum$hero,levels = dfsum$hero[1:10])
gameboard<-function(i){
        g<-ggplot(filter(dfsum, time==i),aes(x=xm,y=ym))+
                geom_point(aes(col=hero,shape=team),size=4)+
                theme_light()+scale_y_continuous(limits=rangey)+
                scale_x_continuous(limits=rangex)+scale_color_brewer(palette="RdBu")+
                guides(col=guide_legend(ncol=2))+labs(x="x",y="y")
        g
}
g<-lapply(unique(dfsum$time),gameboard)
saveVideo({
        lapply(g,print)
        
}, video.name = "Game_time.mp4", other.opts = "-pix_fmt yuv420p -b 300k")


