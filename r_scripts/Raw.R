# Summary:
# processes the zipped csv files for SFIHMM.  Clusters variables to determine states, plots variables, 
# video of evolution of positions. 


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

        g<-g+theme_light()+labs(x="Time (tick)",y=df_var$param[1])+
        theme(axis.title=element_text(size=14))
        if (tit) g<-g+ggtitle(paste0(cname,"Evolution of ",df_var$param[1])) #," for Match ID ",df_var$match_id[1])
        if (!leg) g<-g+guides(col=F)

        g    
}



explained_var<-function(data,i,itermax){
        set.seed(1)
        k<-kmeans(data,i,itermax)
        k$betweenss/k$totss
}
clustvarfun<-function(kmax,dire,radiant=NULL,itermax=10,tot=F){
        clustvar<-data.frame(k=1:kmax)
        if (!tot){
                clustvar$Dire<-sapply(clustvar$k,function(i) explained_var(dire,i,itermax))
                clustvar$Radiant<-sapply(clustvar$k,function(i) explained_var(radiant,i,itermax))
        }else{# assumes total df is in dire
                clustvar$tot<-sapply(clustvar$k,function(i) explained_var(dire,i,itermax))
        }
        clustvar<-gather(clustvar,Team,Variance,-k)
        clustvar
}

normalize_XY_total<-function(dfbpos,coord,useS=T){
        # here dfbpos=dfb
        ind<-grepl(paste0("^",tolower(coord),"\\."),names(dfbpos))
        dfbpos[,paste0("totcm.",coord)]<-rowMeans(dfbpos[,ind])
        dfbpos[,paste0("totcmsd.",coord)]=apply(dfbpos[,ind],1,sd)
        
        newnames<-paste0("totnorm.",names(dfbpos)[ind])
        dfbpos[,newnames]<-dfbpos[,ind]-dfbpos[,paste0("totcm.",coord)]
        if (useS){
                ind<-dfbpos[,paste0("totcmsd.",coord)]!=0
                dfbpos[ind,newnames]<-dfbpos[ind,newnames]/dfbpos[ind,paste0("totcmsd.",coord)]
        }
        dfbpos
}



# setup translation from (possible) deterministic state to number to letter
statetranslate<-data.frame(state=c("5","4-1","3-2","3-1-1",
                                   "2-2-1","2-1-1-1","1-1-1-1-1"),letter=LETTERS[1:7],num=1:7,
                           stringsAsFactors = F)
statetranslate<-rbind(statetranslate,data.frame(state="",letter=LETTERS[8:26],num=8:26))


# file handling
dirfiles<-"/Users/asallaska/SFI/DOTA/data/raw/full/TI5/PlayoffsDay7"
dirfiles<-"/Users/asallaska/SFI/DOTA/data/raw/full/TestGames"

stem<-"776170591"
stem<-"772548096"
###stem<-"776475724"
###stem<-"776436526"

# read in data from zipped csv
dfb<-read.csv(unzip(paste0(dirfiles,"/",stem,".dem.results.timeseries.csv.zip"),junkpaths = T),stringsAsFactors = F,check.names =F)
file.remove(paste0(stem,".dem.results.timeseries.csv")) # remove csv file created in working directory














#clusters for time step DATA READ IN NOT FROM ZIP FILE!!!!! need to summarize from raw data
# gotback to original data set read in from RawTogether.F
dfb<-dfb %>% mutate(minute=round(time/1,0))
dfc1<-dfb[,c("minute",grep("^net\\.",names(dfb),value=T))]
dfc2<-dfb[,c("minute",grep("^x\\.|^y\\.",names(dfb),value=T))]
dfc_tot_sum1<-summarize_each(group_by(dfc1,minute),funs(sum))
dfc_tot_sum2<-summarize_each(group_by(dfc2,minute),funs(mean))
dfc_tot_sum<-cbind(dfc_tot_sum1,select(dfc_tot_sum2,-minute))
# teams together, positions normalized to CM of all players
useS<-T # use standard deviation
dfc_tot_sum<-normalize_XY_total(dfc_tot_sum,"X",useS)
dfc_tot_sum<-normalize_XY_total(dfc_tot_sum,"Y",useS)


if (any(grepl("\\.Radiant$",names(dfc_tot_sum)))){
        team1<-"Radiant"
        team2<-"Dire"
}else{
        team1<-"2"
        team2<-"3"
}
dfc_tot_sum<-normalize_XY(dfc_tot_sum,"X",team1,useS)
dfc_tot_sum<-normalize_XY(dfc_tot_sum,"Y",team1,useS)
dfc_tot_sum<-normalize_XY(dfc_tot_sum,"X",team2,useS)
dfc_tot_sum<-normalize_XY(dfc_tot_sum,"Y",team2,useS)



#dfc_tot_sum<-dfc_tot_sum[,c("minute",grep("^net\\.|^totnorm\\.",names(dfc_tot_sum),value=T))]
# normalize net
ind<-grep("^net\\.",names(dfc_tot_sum))
# find mean, mu, from each column and standard dev, s
mu<-data.frame(t(colMeans(dfc_tot_sum[,ind])))
mu<-mu[rep(1,nrow(dfc_tot_sum)),]
s<-data.frame(t(apply(dfc_tot_sum[,ind],2,sd)))
s<-s[rep(1,nrow(dfc_tot_sum)),]

dfc_tot_sum[,ind]<-(dfc_tot_sum[,ind] - mu)/s
ind<-names(which(colSums(is.na(dfc_tot_sum))==0))
dfc_tot_sum<-dfc_tot_sum[,ind]
nzv<-nearZeroVar(dfc_tot_sum,saveMetrics = T)
nzv[nzv$nzv==F,]
dim(nzv[nzv$nzv==F,])
dim(dfc_tot_sum)
nzv<-nearZeroVar(dfc_tot_sum)

dfc_c<-dfc_tot_sum[,-nzv]

# clustvar_tot<-clustvarfun(100,select(dfc_c,-minute),itermax=1000,tot=T)
# clustvar_tot<-clustvar_tot %>% mutate(Type="All Variables")
# # normal cluster variance plot
# g<-ggplot(clustvar_tot,aes(x=k,y=Variance))+geom_line()+
#         geom_point(aes(shape=Type),size=2.5)+theme_light()+
#         labs(x="Number of Clusters",y="Inter-cluster Distance / Total Variance")+
#         ggtitle(paste0("Clustering Game (not by Team): ",stem))+scale_x_continuous(limits=c(0,100))
# 
# g
clustvar_xy_tot<-clustvarfun(100,dfc_tot_sum[,grep("^totnorm\\.",names(dfc_tot_sum))],itermax=1000,tot=T)
clustvar_tot<-clustvarfun(100,dfc_tot_sum[,grep("^totnorm\\.|^net\\.",names(dfc_tot_sum))],itermax=1000,tot=T)
clustvar_xy_tot<-clustvar_xy_tot %>% mutate(Type="Normalized Positions\nOnly")
clustvar_tot<-clustvar_tot %>% mutate(Type="All Variables")


# normal cluster variance plot
g<-ggplot(clustvar_tot,aes(x=k,y=Variance))+geom_line()+
        geom_point(aes(shape=Type),size=2.5)+theme_light()+
        labs(x="Number of Clusters",y="Inter-cluster Distance / Total Variance")+
        geom_line(data=clustvar_xy_tot,mapping=aes(x=k,y=Variance))+
        geom_point(data=clustvar_xy_tot,mapping=aes(x=k,y=Variance,shape=Type),size=2.5)+
        ggtitle(paste0("Clustering Game (not by Team): ",stem))+scale_x_continuous(limits=c(0,100))

g





# heroes<-fromJSON("/Users/asallaska/SFI/DOTA/heroes.json")
# heroes<-heroes$result$heroes
# heroes<-heroes %>% select(id,localized_name) %>% rename(hero_id=id,Hero=localized_name) %>%
#         mutate(lowerhero=tolower(gsub(" |-|\\_|'","",Hero))) %>% 
#         mutate(lowerhero=ifelse(lowerhero=="naturesprophet","furion",lowerhero)) # note exception for furion/naturesprophet

#files<-list.files(path=dirfiles,pattern="*\\_clean.json$",full.names=T)

# dfnormX<-dfb[,c("seq","tick","time",grep("norm.x",names(dfb),value=T))]
# dfnormY<-dfb[,c("seq","tick","time",grep("norm.y",names(dfb),value=T))]
# dfnormX_melt<-gather(dfnormX,pos,value,-(seq:time))
# dfnormY_melt<-gather(dfnormY,pos,value,-(seq:time))
# dfnormX_melt<-dfnormX_melt %>% separate(pos,into=c("v","w","hero","team"),sep="\\.")
# dfnormX_melt<-dfnormX_melt %>% select(-v,-w) %>% rename(x=value)
# dfnormY_melt<-dfnormY_melt %>% separate(pos,into=c("v","w","hero","team"),sep="\\.")
# dfnormY_melt<-dfnormY_melt %>% select(-v,-w) %>% rename(y=value)
# 
# dfnorm_melt<-cbind(dfnormX_melt,select(dfnormY_melt,y))
# dfnorm_melt<-arrange(dfnorm_melt,seq)
# 
# g<-ggplot(filter(dfnorm_melt,seq<=1000),aes(x=x,y=y,group=seq))+geom_line(aes(col=team))
# g
# g<-ggplot(filter(dfnorm_melt,seq<=10000),aes(x=x,y=y,group=seq))+geom_point(aes(col=team),alpha=0.5)
# #g
# 




# j<-grepl("GOLD|XP|x\\.|y\\.",names(df))
# colMeans(df[,j]) 


#### CLUSTERING
# for clusters, pick out net variables as well as centered positions:

# teams together, positions normalized to CM of all players
useS<-T # use standard deviation
dfb_tot<-normalize_XY_total(dfb,"X",useS)
dfb_tot<-normalize_XY_total(dfb_tot,"Y",useS)
dfc_tot<-dfb_tot[,c("match_id","tick","time",grep("^net\\.|^totnorm\\.",names(dfb_tot),value=T))]
ind<-names(which(colSums(is.na(dfc_tot))==0))
dfc_tot<-dfc_tot[,ind]

dfc_tot_xy<-dfc_tot[,grep("^totnorm\\.",names(dfc_tot))]

clustvar_xy_tot<-clustvarfun(100,dfc_tot_xy,itermax=100,tot=T)
clustvar_tot<-clustvarfun(50,dfc_tot,itermax=100,tot=T)
clustvar_xy_tot<-clustvar_xy_tot %>% mutate(Type="Normalized Positions\nOnly")
clustvar_tot<-clustvar_tot %>% mutate(Type="All Variables")


# normal cluster variance plot
g<-ggplot(clustvar_tot,aes(x=k,y=Variance))+geom_line()+
        geom_point(aes(shape=Type),size=2.5)+theme_light()+
        labs(x="Number of Clusters",y="Inter-cluster Distance / Total Variance")+
        geom_line(data=clustvar_xy_tot,mapping=aes(x=k,y=Variance))+
        geom_point(data=clustvar_xy_tot,mapping=aes(x=k,y=Variance,shape=Type),size=2.5)+
        geom_line(data=csave,mapping=aes(x=k,y=Variance),col="red")+
        geom_point(data=csave,mapping=aes(x=k,y=Variance,shape=Type),size=2.5)+        
        
        ggtitle(paste0("Clustering Game (not by Team): ",stem))+scale_x_continuous(limits=c(0,100))

g
pdf("Cluster_variance_game.pdf")
g
dev.off()

k<-kmeans(dfc_tot_xy,25,iter.max = 100)

# kc<-kcca(dfc_tot_xy,k=25)
# predict(kc) # actual clusters
#predict(kc,newdata=___) # predict on new data using old previous clusters

dfx<-dfb[,c("match_id","tick","time",grep("^x\\.|^y\\.",names(dfb),value=T))]
dfx$ClusterNo<-k$cluster

# dfc_plot<-cbind(dfb[,c("match_id","tick","time")],dfc_tot_xy)
# dfc_plot$ClusterNo<-k$cluster
# dfc_plot$seq<-1:nrow(dfc_plot)
# dfc_plot$avgDist<-sqrt(rowMeans(dfc_tot_xy[,grep("\\.x\\.",names(dfc_tot_xy))])^2+rowMeans(dfc_tot_xy[,grep("\\.y\\.",names(dfc_tot_xy))])^2)
# 
# 
# dfnormX<-dfc_plot[,c("seq","ClusterNo","tick","time",grep("^totnorm\\.x\\.",names(dfc_plot),value=T))]
# dfnormY<-dfc_plot[,c("seq","ClusterNo","tick","time",grep("^totnorm\\.y\\.",names(dfc_plot),value=T))]
# dfnormX_melt<-gather(dfnormX,pos,value,-(seq:time))
# dfnormY_melt<-gather(dfnormY,pos,value,-(seq:time))
# dfnormX_melt<-dfnormX_melt %>% separate(pos,into=c("w","v","hero","team"),sep="\\.") 
# dfnormX_melt<-dfnormX_melt %>% select(-w,-v) %>% rename(x=value)
# dfnormY_melt<-dfnormY_melt %>% separate(pos,into=c("w","v","hero","team"),sep="\\.") 
# dfnormY_melt<-dfnormY_melt %>% select(-w,-v) %>% rename(y=value)
# 
# dfnorm_melt<-cbind(dfnormX_melt,select(dfnormY_melt,y))
# dfnorm_melt<-arrange(dfnorm_melt,seq)


# 
# # tick video for clusters
# dfsum<-dplyr::summarize(group_by(dfnorm_melt,time,team,hero,ClusterNo),xm=mean(x),ym=mean(y))
# 
# rangex<-range(dfsum$x)
# rangey<-range(dfsum$y)
# dfsum$hero<-factor(dfsum$hero,levels = dfsum$hero[1:10])
# dfsum$ClusterNo<-as.factor(dfsum$ClusterNo)
# gameboard<-function(i){
#         g<-ggplot(filter(dfsum, time==i),aes(x=xm,y=ym))+
#                 geom_point(aes(col=hero,shape=ClusterNo),size=4)+
#                 theme_light()+scale_y_continuous(limits=rangey)+
#                 scale_x_continuous(limits=rangex)+scale_color_brewer(palette="RdBu")+
#                 guides(col=guide_legend(ncol=2))+labs(x="x",y="y")+scale_shape_discrete(drop=F)
#         g
# }
# g<-lapply(unique(dfsum$time)[1:1000],gameboard)
# saveVideo({
#         lapply(g,print)
#         
# }, video.name = "Game_time_cluster.mp4", other.opts = "-pix_fmt yuv420p -b 300k")


########### average position for each cluster#############
dfavgclust<-dplyr::summarize_each(group_by(select(dfx,-(match_id:time)),ClusterNo),funs(mean))
dfavgclust<-dplyr::summarize_each(group_by(dfx[,c(grep("^x\\.|^y\\.",names(dfx),value=T),"ClusterNo")],ClusterNo),funs(mean))
#dfavgclust$seq<-1:nrow(dfavgclust)
dfnormX<-dfavgclust[,c("ClusterNo",grep("^x\\.",names(dfavgclust),value=T))]
dfnormY<-dfavgclust[,c("ClusterNo",grep("^y\\.",names(dfavgclust),value=T))]
dfnormX_melt<-gather(dfnormX,pos,value,-ClusterNo)
dfnormY_melt<-gather(dfnormY,pos,value,-ClusterNo)
dfnormX_melt<-dfnormX_melt %>% separate(pos,into=c("w","hero","team"),sep="\\.") 
dfnormX_melt<-dfnormX_melt %>% select(-w) %>% rename(x=value)
dfnormY_melt<-dfnormY_melt %>% separate(pos,into=c("w","hero","team"),sep="\\.") 
dfnormY_melt<-dfnormY_melt %>% select(-w) %>% rename(y=value)

dfnorm_melt<-cbind(dfnormX_melt,select(dfnormY_melt,y))
rangex<-range(dfnorm_melt$x)
rangey<-range(dfnorm_melt$y)
#dfnorm_melt<-arrange(dfnorm_melt,seq)
g<-ggplot(dfnorm_melt,aes(x=x,y=y))+
        geom_point(aes(col=hero),size=4)+
        theme_light()+scale_y_continuous(limits=rangey)+facet_wrap(~ClusterNo)+
        scale_x_continuous(limits=rangex)+scale_color_brewer(palette="RdBu")+
        guides(col=guide_legend(ncol=2))+labs(x="x",y="y",title=paste0("Avg x and y positions for each cluster: match ID ",stem),
                                              subtitle="Aggregated over 1 sec")
g
pdf(paste0("Cluster_positions_",stem,"_avg_25_secagg.pdf"))
g
dev.off()


# write new sequence from clusters

#dfx<-dfb[,c("match_id","tick","time",grep("^x\\.|^y\\.",names(dfb),value=T))]
dfx<-dfc_tot_sum
k<-kmeans(dfx[,grep("totnorm\\.",names(dfx))],25,iter.max = 1000)

dfx$ClusterNo<-k$cluster

dfx<-dfx %>% select(ClusterNo) %>% rename(num=ClusterNo)
dfx<-left_join(dfx,statetranslate,by="num")
write.table(matrix(c(nrow(dfx),paste(dfx$letter,collapse="")),ncol=1,nrow=2),
            paste0(dirfiles,"/Both_",stem,"_25cluster_time.seq"),row.names=F,quote=F,col.names=F)


###############################






# ############ teams separately, positions normalized to team CM
dfc<-dfb[,c("match_id","tick","time",grep("^net\\.|^norm\\.",names(dfb),value=T))]
dfc<-dfc_tot_sum
dfc_bare_xy<-dfc[,grep("^norm\\.",names(dfc))]
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
        labs(x="Number of Clusters",y="Inter-cluster Distance / Total Variance",title=paste0("Determining cluster #: match ID ",stem),
             subtitle="Aggregated over 1 sec")+
        geom_line(data=clustvar_xy,mapping=aes(x=k,y=Variance,col=Team))+
        geom_point(data=clustvar_xy,mapping=aes(x=k,y=Variance,col=Team,shape=Type),size=2.5)

g

pdf(paste0("Cluster_variance_",stem,"_timeagg.pdf"))
g
dev.off()

# take clustered states and turn them into input to HMM
kmeans.to.seq<-function(x,n,iter,team,dirfiles,stem,dist){
        alphatranslate<-data.frame(letter=LETTERS,k=1:26,stringsAsFactors = F)
        
        k<-kmeans(x,n,iter)
        clustdf<-data.frame(k=k$cluster)
        clustdf<-left_join(clustdf,alphatranslate,by="k")
        
        # write out sequence for Simon's HMM model code
        if (dist){
                suffix<-"distclust_timeagg.seq"
        }else{
                suffix<-"allclust.seq"
        }
        write.table(matrix(c(nrow(clustdf),paste(clustdf$letter,collapse="")),ncol=1,nrow=2),
                    paste0(dirfiles,"/",team,"_",stem,"_",n,suffix),row.names=F,quote=F,col.names=F)  
}

# writes file for HMM: dirfiles/team_stem_nsuffix 
kmeans.to.seq(dfc_bare_dire_xy,25,500,"Dire",dirfiles,stem,dist=T)
kmeans.to.seq(dfc_bare_radiant_xy,25,500,"Radiant",dirfiles,stem,dist=T)







# PCA
library(caret)
nearZeroVar(dfc_bare_dire,saveMetrics=T)
nzv<-nearZeroVar(dfc_bare_dire)
filtered_dfc_bare_dire<-dfc_bare_dire[,-nzv]

# prcomp
pca<-prcomp(dfc_bare_dire,retx=T, center=F, scale.=F)
transformedData<-pca$x
rot<-pca$rotation
eigenv<-pca$sdev


# caret
trans<-preProcess(dfc_bare_dire,method="pca")
transformedData1<-predict(trans,dfc_bare_dire)
rot1<-trans$rotation













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

# plots of variables

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
dfbpos$seq<-1:nrow(dfbpos)

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

# tick scale
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



# aggregate over time (sec)
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


