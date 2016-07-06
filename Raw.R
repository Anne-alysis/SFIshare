require(jsonlite)
require(grid)

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
dirfiles<-"/Users/asallaska/SFI/DOTA/data/raw/full/TestGames/"

stem<-"775979885"


dfb<-read.csv(unzip(paste0(dirfiles,stem,".dem.results.timeseries.csv.zip"),junkpaths = T))
file.remove(paste0(stem,".dem.results.timeseries.csv")) # remove csv file created in working directory


#files<-list.files(path=dirfiles,pattern="*\\_clean.json$",full.names=T)

dfnormX<-dfb[,c("seq","tick","time",grep("norm.x",names(dfb),value=T))]
dfnormY<-dfb[,c("seq","tick","time",grep("norm.y",names(dfb),value=T))]
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





# j<-grepl("GOLD|XP|x\\.|y\\.",names(df))
# colMeans(df[,j]) 


#### CLUSTERING
# for clusters, pick out net variables as well as centered positions:
dfc<-dfb[,c("match_id","tick","time",grep("^net\\.",names(dfb),value=T),grep("^norm\\.",names(dfbpos)))]



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


