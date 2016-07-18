# Summary:
# reads in a battery of zipped csv files, aggregates over specified time unit for each game, 
# clusters positions on all given games.  Produces sequence output files for SFIHMM as well 
# as a data frame of all games glued together.  It also analyzes the output sequences of the 
# model, when comparing to game states, determined from the gameDetails.json, but, more importantly, 
# from the statistics determined directly from the raw data, such as difference in accumulated XP 
# for the winning and losing teams.  Correlation plots are also produced.  Average positions for 
# each kmeans cluster are plotted.  

require(parallel)
library(corrgram)

normalize_XY_total<-function(dfbpos,coord,useS=T){
        # normalize x and y for teams *together*
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
explained_var<-function(data,i,itermax){
        # do kmeans for given cluster number and output the explained variance
        set.seed(1)
        k<-kmeans(data,i,itermax)
        k$betweenss/k$totss
}
clustvarfun<-function(kmax,dire,radiant=NULL,itermax=10,tot=F){
        # for a range of cluster numbers, find the explained variance to determine optimum number of clusters
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
        
extractSum<-function(kdf,tagg){       
        # read in multitude of games, aggregate over "tagg" time scale, slap together, making sure to order the columns as appropriately as possible from game to game
        i<-1
        print(kdf[i,])
        # open relevant file and delete the csv it creates
        dfb<-read.csv(unzip(paste0(kdf$dirfiles[i],kdf$kfilescsv[i]),junkpaths = T),stringsAsFactors = F,check.names =F)
        fremove<-gsub("\\.zip","",kdf$kfilescsv[i])
        file.remove(fremove) # remove csv file created in working directory
        
        match_id<-dfb$match_id[1] # propogate the match_id in case some are NA
        dfb$match_id<-match_id
        
        # somehow certain zip files got duplicate columns with all NAs??  wtf... filter these out and check later
        ind<-names(which(colSums(is.na(dfb))==0))
        dfb<-dfb[,ind]
        
        # gold in order to order positions
        if (any(grepl("\\.Dire$",names(dfb)))){
                team1<-".Dire"
                team2<-".Radiant"
        }else{
                team1<-".2"
                team2<-".3"
        }
        
        #### NORMALIZE all but positions (normalized separately)
        
        # # # normalize
        # ind<-grep("^net\\.",names(dfb))
        # # find mean, mu, from each column and standard dev, s
        # mu<-data.frame(t(colMeans(dfb[,ind])))
        # mu<-mu[rep(1,nrow(dfb)),]
        # s<-data.frame(t(apply(dfb[,ind],2,sd)))
        # s<-s[rep(1,nrow(dfb)),]
        # 
        # dfb[,ind]<-(dfb[,ind] - mu)/s
        
        # find hero ordering for each team in terms of (descending?) gold accumulated.  currently looking at net gold accumulated
        # so there are some subtractions included.  small effect.  
        
        #sumgtzero<-colSums(dfb[,grep("^net\\.GOLD",names(dfb))])
        #dfb[,grep("^net\\.GOLD",names(dfb))]<-apply(dfb[,grep("^net\\.GOLD",names(dfb))],c(1,2),function(i) ifelse(i<0,0,i))
        goldDire<-dfb[nrow(dfb),grepl("cum\\.GOLD\\.",names(dfb)) & grepl("\\.Dire$|\\.2$",names(dfb))] 
        names(goldDire)<-gsub("cum\\.GOLD\\.|\\.Dire$|\\.2$","",names(goldDire))
        goldDire<-names(goldDire[order(goldDire)])
        goldDire<-paste0(c("totnorm.x.","totnorm.y."),rep(goldDire,each=2),team1)
        
        goldRadiant<-dfb[nrow(dfb),grepl("cum\\.GOLD\\.",names(dfb)) & grepl("\\.Radiant$|\\.3$",names(dfb))] 
        names(goldRadiant)<-gsub("cum\\.GOLD\\.|\\.Radiant$|\\.3$","",names(goldRadiant))
        goldRadiant<-names(goldRadiant[order(goldRadiant)])
        goldRadiant<-paste0(c("totnorm.x.","totnorm.y."),rep(goldRadiant,each=2),team2)

        
        if (length(goldRadiant)==10 & length(goldDire)==10) { # if have all columns that you should
                
                # add new variable over which to aggregate using tagg
                dfb<-dfb %>% mutate(minute=round(time/(tagg),0))
                dfc2<-dfb[,c("match_id","minute",grep("^x\\.|^y\\.",names(dfb),value=T))] # extract positions only
                dfc_tot_sum<-data.frame(summarize_each(group_by(dfc2,match_id,minute),funs(mean)),stringsAsFactors = F) # find average position while aggregating
   
                # teams together, positions renormalized to CM of all players
                useS<-T # use standard deviation
                dfc_tot_sum<-normalize_XY_total(dfc_tot_sum,"X",useS)
                dfc_tot_sum<-normalize_XY_total(dfc_tot_sum,"Y",useS)
                
                # extract only total normalized variables and minute
                temp<-dfc_tot_sum[,c("match_id","minute",grep("^totnorm\\.",names(dfc_tot_sum),value=T))]
                
                # new variables to keep track of for statistics and correlations with SFIHMM outcomes
                newvars0<-rep(c("GOLD","XP","KillsHero","KillsOther","DAMAGE","Deaths"),2)
                newvars<-paste0("stats.",newvars0,rep(c(".TeamWin",".TeamLose"),each=length(newvars0)/2))
                
                # find if radiant won and order that team first if so, otherwise put dire stats first.  
                if (kdf$stats.radiant_win[i] | is.na(kdf$stats.radiant_win[i])){
                        temp<-temp[,c("match_id","minute",goldRadiant,goldDire)] # reorder columns by gold accumulated 
                        
                        #add new statistics variables by looking at the accumulated stats (last row of zipped data frame)
                        for (j in 1:length(newvars)){
                                if (j <= length(newvars)/2) {
                                        teami<-team2
                                }else{
                                        teami<-team1
                                }
                                temp[,newvars[j]]<-sum(dfb[nrow(dfb),grepl(paste0("^cum\\.",newvars0[j]),names(dfb)) & grepl(teami,names(dfb))])
                        }
                }else {
                        temp<-temp[,c("match_id","minute",goldDire,goldRadiant)] # reorder columns by gold accumulated 
                        for (j in 1:length(newvars)){
                                if (j <= length(newvars)/2) {
                                        teami<-team1
                                }else{
                                        teami<-team2
                                }

                                temp[,newvars[j]]<-sum(dfb[nrow(dfb),grepl(paste0("^cum\\.",newvars0[j]),names(dfb)) & grepl(teami,names(dfb))])
                        }
                }
                
                # rename columns to something more generic, stripping out hero names
                names(temp)[3:22]<-c(paste0(rep(c("x.","y."),5),"P",rep(1:5,each=2),".TeamWin"),
                                       paste0(rep(c("x.","y."),5),"P",rep(1:5,each=2),".TeamLose"))

                # ind<-which(grepl("totnorm\\.x\\.",names(temp)) & grepl("\\.Dire$|\\.2",names(temp)))
                # names(temp)[ind]<-paste0("x.P",1:5,".Dire")
                # ind<-which(grepl("totnorm\\.y\\.",names(temp)) & grepl("\\.Dire$|\\.2",names(temp)))
                # names(temp)[ind]<-paste0("y.P",1:5,".Dire")
                # 
                # ind<-which(grepl("totnorm\\.x\\.",names(temp)) & grepl("\\.Radiant$|\\.3",names(temp)))
                # names(temp)[ind]<-paste0("x.P",1:5,".Radiant")
                # ind<-which(grepl("totnorm\\.y\\.",names(temp)) & grepl("\\.Radiant$|\\.3",names(temp)))
                # names(temp)[ind]<-paste0("y.P",1:5,".Radiant")


                
                # join aggregated stats from json
                temp<-left_join(temp,kdf[i,] %>% select(match_id,stats.radiant_win:stats.game_mode) %>% 
                                        mutate(match_id=as.numeric(match_id)),by="match_id")
                
                return(temp)
        }else{
                return(NULL)
        }
}





# setup translation from (possible) deterministic state to number to letter

statetranslate<-data.frame(state=c("5","4-1","3-2","3-1-1",
                                   "2-2-1","2-1-1-1","1-1-1-1-1"),letter=LETTERS[1:7],num=1:7,
                           stringsAsFactors = F)
statetranslate<-rbind(statetranslate,data.frame(state="",letter=c(LETTERS[8:26],letters[1:26]),num=8:52))


# files to use
dirfiles<-c("/Users/asallaska/SFI/DOTA/data/raw/full/TI5/PlayoffsDay7/",
           # "/Users/asallaska/SFI/DOTA/data/raw/full/TestGames/",
            "/Volumes/BigRed/dota/Replay/TI5small/JSON/")

# build data frame with file paths and match_ids           
kdf0<-data.frame()
for (i in dirfiles){
        kfilescsv<-dir(path=i,pattern=".dem.results.timeseries.csv.zip",full.names = F)
        kdf0<-rbind(kdf0,data.frame(dirfiles=i,kfilescsv=kfilescsv,stringsAsFactors = F))
}
kdf0$match_id<-sapply(strsplit(kdf0$kfilescsv,"\\."),function(i)i[[1]])





# remove files already processed
#stems<-data.frame(match_id=as.character(unique(dls$match_id)),done=T,stringsAsFactors = F)
#kdf0<-left_join(kdf0,stems,by="match_id")
#kdf0<-filter(kdf0,is.na(done))

# read in aggregated data to get overall game stats
agg<-fromJSON("/Users/asallaska/SFI/DOTA/data/raw/gameDetails.json")
agg_ind<-sapply(unique(kdf0$match_id),function(i) which(names(agg)==i))
gamestats<-data.frame()
for (i in agg_ind){
        agg_game<-agg[[i]] # isolate aggregated stats for relevant game
        temp<-data.frame(agg_game$match_id,agg_game$radiant_win,agg_game$duration,agg_game$radiant_score,
                         agg_game$dire_score,agg_game$leagueid,agg_game$start_time,agg_game$lobby_type,
                         agg_game$game_mode)
        gamestats<-rbind(gamestats,temp)
}
names(gamestats)[-1]<-gsub("agg\\_game","stats",names(gamestats)[-1])
names(gamestats)[1]<-"match_id"
gamestats$match_id<-as.character(gamestats$match_id)

# fold game stats into file data frame in order to port into main dataframe, dls
kdf0<-left_join(kdf0,gamestats,by="match_id")

# to start from scratch (and not eliminate previously processed files)
kdf<-kdf0

# find indicies to split on, in order to parallel process
n<-c(seq(1,nrow(kdf),8),nrow(kdf))

# split up data frame into 8 parts in a list in order to parallel process
kfileslist<-lapply(1:(length(n)-1),function(i) kdf[n[i]:(n[i+1]-1),])

dls_save<-dls # so we don't have to repeat

# make large data frame including each game in order to cluster
dls<-data.frame()
tagg<-60 # in seconds
for (i in 1:length(kfileslist)){
        print(c(i," of ",length(kfileslist))) # counter 
        ls<-mclapply(1:nrow(kfileslist[[i]]),function(j) extractSum(kfileslist[[i]][j,],tagg),mc.cores=nrow(kfileslist[[i]]))
        dls<-rbind(dls,do.call(rbind.data.frame,ls))
}
#dls<-rbind(dls,dls_save)


# test clustering to determine k
clustvar_xy_tot<-clustvarfun(100,dls[,grep("^x\\.|^y\\.",names(dls))],itermax=1000,tot=T)

# plot explained variance as a function of cluster number
gc<-ggplot(clustvar_xy_tot,aes(x=k,y=Variance))+geom_line()+
        geom_point(size=2.5)+theme_light()+
        labs(x="Number of Clusters",y="Inter-cluster Distance / Total Variance",title=paste0("Determining cluster #: multigames "),
             subtitle="Aggregated over 1 min")
gc
pdf(paste0("Var_Clusters_",match_id,"_60s_goldwinordered_NEW.pdf"),width=15,height=10)
gc
dev.off()

# find clusters
k<-kmeans(dls[,grep("^x\\.|^y\\.",names(dls))],25,1000)
dls$ClusterNo<-k$cluster

# setup to send to SFIHMM
dfx<-dls %>% select(match_id,ClusterNo) %>% rename(num=ClusterNo)
dfx<-left_join(dfx,statetranslate,by="num")
dls<-left_join(dls,rename(statetranslate,ClusterNo=num),by="ClusterNo")

# make concatenated sequence.  insert dummy variable, "z", at the end of each game
seq<-dfx$letter[1]
for (i in 2:nrow(dfx)){
        if (dfx$match_id[i]!=dfx$match_id[i-1]) {
                seq<-c(seq,"z")
        }
        seq<-c(seq,dfx$letter[i])
}

# write sequence to file in order to process by SFIHMM
write.table(matrix(c(length(seq),paste(seq,collapse="")),ncol=1,nrow=2),
            paste0("/Users/asallaska/sfi/dota/data/raw/full/Multigames_60s_reorder2_NEW.seq"),row.names=F,quote=F,col.names=F)

# save data to correlate to SFIHMM output
write.csv(dls,"agg_raw_states.csv",row.names = F)

###### what do the clusters mean? ###### 
# find average position for each hero for each cluster in order to plot
dfavgclust<-dplyr::summarize_each(group_by(dls[,c(grep("^x\\.|^y\\.",names(dls),value=T),"ClusterNo")],ClusterNo),funs(mean))
#dfavgclust$seq<-1:nrow(dfavgclust)
dfnormX<-dfavgclust[,c("ClusterNo",grep("^x\\.",names(dfavgclust),value=T))]
dfnormY<-dfavgclust[,c("ClusterNo",grep("^y\\.",names(dfavgclust),value=T))]
dfnormX_melt<-gather(dfnormX,pos,value,-ClusterNo)
dfnormY_melt<-gather(dfnormY,pos,value,-ClusterNo)
dfnormX_melt<-dfnormX_melt %>% mutate(heroTeam=gsub("x\\.","",pos)) 
dfnormX_melt<-dfnormX_melt %>% select(-pos) %>% rename(x=value)
dfnormY_melt<-dfnormY_melt %>% mutate(heroTeam=gsub("y\\.","",pos)) 
dfnormY_melt<-dfnormY_melt %>% select(-pos) %>% rename(y=value)

dfnorm_melt<-cbind(dfnormX_melt,select(dfnormY_melt,y))
dfnorm_melt$heroTeam<-gsub("Dire","Team1",dfnorm_melt$heroTeam)
dfnorm_melt$heroTeam<-gsub("Radiant","Team2",dfnorm_melt$heroTeam)
rangex<-range(dfnorm_melt$x)
rangey<-range(dfnorm_melt$y)
#dfnorm_melt<-arrange(dfnorm_melt,seq)
gavg<-ggplot(dfnorm_melt,aes(x=x,y=y))+
        geom_point(aes(col=heroTeam),size=4)+
        theme_light()+scale_y_continuous(limits=rangey)+facet_wrap(~ClusterNo)+
        scale_x_continuous(limits=rangex)+scale_color_brewer(palette="RdBu")+
        guides(col=guide_legend(ncol=2))+labs(x="Renormalized x",y="Renormalized y",title=paste0("Avg x and y positions for 49 games"),
                                              subtitle="Aggregated over 60 sec")
gavg


match_id<-"Multigame"
pdf(paste0("Clusters_",match_id,"_60s_goldwinordered_NEW.pdf"),width=15,height=10)
gavg
dev.off()

dls<-left_join(dls,gamestats,by="match_id")





# find correlations to output of SFIHMM

#### seq df is from seq.R !!!!!! 
mod<-select(seq,seqno,ObservedChar,Module1)
game<-as.character(unique(dls$match_id))
# apply match_id to output of SFIHMM (which is just a list of states, uncorrelated to match_id)
j<-1
mod$match_id<-game[j]
for (i in 2:nrow(mod)){
        if (mod$ObservedChar[i]=="z") {
                j<-j+1
                mod$match_id[i]<-game[j]
        }else{
                mod$match_id[i]<-mod$match_id[i-1]
        }
}
mod<-filter(mod,ObservedChar!="z") # remove dummy state
modsum<-dplyr::summarize(group_by(mod,match_id),AvgModuleHL=mean(Module1)) # average over module 1 to see if correlations

# from dls OR from previously written agg_raw_states.csv, find the stats and join to SFIHMM data
dlsstats<-unique(dls[,c("match_id",grep("^stats\\.",names(dls),value=T))])
dlsstats$match_id<-as.character(dlsstats$match_id)
dlsstats<-left_join(dlsstats,modsum,by="match_id")
dlsstats<-select(dlsstats,match_id,stats.GOLD.TeamWin:stats.duration,AvgModuleHL)
dlsstats$stats.radiant_win<-as.numeric(dlsstats$stats.radiant_win)

temp<-dlsstats[,2:7]-dlsstats[,8:13] # win stats - lose stats, net stats for winning team over losing team
names(temp)<-paste0(gsub("\\.TeamWin","",names(temp)),".net")
dlsstats<-cbind(dlsstats[,grep("TeamWin|TeamLose",names(dlsstats),invert=T)],temp)

# various correlation plots

col.corrgram<-function(ncol){
        colorRampPalette(c("darkgoldenrod4","burlywood1","darkkhaki","darkgreen"))(ncol)
}                
corrgram(select(dlsstats,-match_id),order=T,lower.panel=panel.shade,upper.panel=panel.pts,
         text.panel=panel.txt,main="x")


panel.hist<-function(x,...){
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(usr[1:2], 0, 1.5) )
        h <- hist(x, plot = FALSE)
        breaks <- h$breaks; nB <- length(breaks)
        y <- h$counts; y <- y/max(y)
        rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}
panel.smooth<-function (x, y, col = "blue", bg = NA, pch = 18, 
                        cex = 0.8, col.smooth = "red", span = 2/3, iter = 3, ...) 
{
        points(x, y, pch = pch, col = col, bg = bg, cex = cex)
        ok <- is.finite(x) & is.finite(y)
        if (any(ok)) 
                lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
                      col = col.smooth, ...)
}
panel.cor <- function(x, y, digits=2, cex.cor)
{
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        r <- abs(cor(x, y))
        txt <- format(c(r, 0.123456789), digits=digits)[1]
        test <- cor.test(x,y)
        Signif <- ifelse(round(test$p.value,3)<0.001,"p<0.001",paste("p=",round(test$p.value,3)))  
        text(0.5, 0.25, paste("r=",txt))
        text(.5, .75, Signif)
}

temp<-dlsstats
names(temp)<-gsub("stats\\.","",names(temp))
pdf("Correlations.pdf")
pairs(select(temp,-match_id),lower.panel=panel.smooth,upper.panel=panel.cor,diag.panel=panel.hist)
#pairs(select(dlsstats,grep("GOLD|win",names(dlsstats))),lower.panel=panel.smooth,upper.panel=panel.cor,diag.panel=panel.hist)
dev.off()
