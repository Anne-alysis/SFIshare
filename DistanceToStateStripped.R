# Summary:
# determines the distance between teams in a given game for a battery of games.  Reads in zipped csv files, 
# converts relative distances to states for each team, uses an optimal matching algorithm to calculate the 
# similarity between sequences.  


library(parallel)
library(TraMineR)
library(jsonlite)
library(Hmisc)

setwd("./SFI/DOTA/analysis/")
assignStates<-function(yteam,dist_thres=5,avgT){
        # to determine states by relative distances and a given threshold, dist_thres
        
        # add tag to filter on after melting
        yteam$event_id<-1:nrow(yteam)
        # melt data to use filtering
        if (avgT){
                yteam_melt<-yteam %>% tidyr::gather(seq,dist,-event_id,-time) %>% arrange(event_id)
        }else{
                yteam_melt<-yteam %>% tidyr::gather(seq,dist,-event_id,-tick,-time) %>% arrange(event_id)
        }
        
        #dummy state
        yteam$state<-""
        
        # for every time step, find state
        for (j in 1:nrow(yteam)){
                # keeping track of code progress
                if (j %% 1000 ==0) print(paste(c("j = ",j," (",round(j/nrow(yteam)*100,1)," %)"),collapse=""))
                # filter out potential heroes that fall under the distance threshold for the given time step (j)
                dfless<-dplyr::filter(yteam_melt,event_id==j & dist<=dist_thres)
                if (nrow(dfless)==0){# if no hero pairs are below, then all far apart
                        state<-"1-1-1-1-1"
                }else{
                        # at least one hero pair is under the threshold
                        cluster<-strsplit(dfless$seq[1],"_") # split heroes in first pair to compare to
                        if (nrow(dfless)==1){ # if only one pair under threshold, then state is already determined
                                state<-"2-1-1-1"
                        }else{
                                # for every pair with a distance less than threshold, see if they are clustered together
                                # or if there are multiple clusters
                                for (i in 2:nrow(dfless)){
                                        newcluster<-strsplit(dfless$seq[i],"_")[[1]] #split current cluster into components
                                        # loop over all separate clusters
                                        for (l in 1:length(cluster)){
                                                # if any hero in the current cluster is also in the old cluster[[l]], add new heroes to the old cluster[[l]]
                                                if (any(grepl(paste(newcluster,collapse="|"),cluster[[l]]))){
                                                        cluster[[l]]<-unique(c(cluster[[l]],newcluster))
                                                }else if (l==length(cluster)){ # if all heroes in given cluster are different than previous clusters, create new cluster
                                                        cluster[[l+1]]<-newcluster
                                                }
                                        }
                                }
                                
                                # done looking for clusters, now find states associated with each cluster
                                if (length(cluster)==1){
                                        l<-length(cluster[[1]])
                                        state<-paste(c(as.character(l),rep(1,5-l)),collapse="-") # add appropriate number of "-1" to state definition to fill out to 5 heroes
                                }else{
                                        if (any(sapply(cluster,length)==3)){
                                                state<-"3-2"
                                        }else{
                                                state<-"2-2-1"        
                                        }
                                }
                        }
                        
                        
                }
                # cluster
                # lapply(cluster,length)
                # state
                # update state variable for newly determined state
                yteam$state[j]<-state
        }
        yteam
}

parallelStates<-function(yteam1,thres,statetranslate,avgT=F){
        # find states in a parallel form by breaking up the sequence
        if (avgT){
                yteam1<-data.frame(summarize_each(group_by(select(yteam1,-tick),time),funs(mean)),stringsAsFactors = F,check.names = F)
        }
        
        # find indices to split on, in order to parallel process
        n<-floor(nrow(yteam1)/8)
        n<-c(1,n*(1:7),nrow(yteam1)+1)
        
        # split up data frame into 8 parts in a list in order to parallel process
        ylist1<-lapply(1:(length(n)-1),function(i) yteam1[n[i]:(n[i+1]-1),])
        # find states for each piece of data frame
        ls<-mclapply(1:length(ylist1),function(i) assignStates(ylist1[[i]],thres,avgT),mc.cores=8)
        # bind each resulting data frame into a single data frame
        yteam1_5<-do.call(rbind.data.frame,ls)
        # add parameters to resulting data frame
        yteam1_5$thres<-thres
        yteam1_5$event_id<-1:nrow(yteam1_5)
        yteam1_5<-left_join(yteam1_5,statetranslate,by="state")
        yteam1_5
}

# sequence comparisons
seqSingle<-function(yteam,team){
        # convert state dataframe into something TraMineR can handle
        yseq0<-select(yteam,state)
        yseq0$id<-team
        yseq0$seqno<-1:nrow(yseq0)
        #yseq0$seqnof<-yseq0$seqno+1
        yseq0$state<-gsub("-","_",yseq0$state)
        yseq0
}
convertSeq<-function(yteamR,yteamD){
        # convert each data frame into a sequence 
        yseq0<-seqSingle(yteamR,"Radiant")
        yseq<-seqSingle(yteamD,"Dire")
        match_id<-yteamR$match_id[1]
        
        if (all.equal(yseq0$state,yseq$state)==T){
                d<-0 # identical sequences
        }else{
                # bind teams together, get into form of TraMineR STS
                yseqT<-rbind(yseq0,yseq)
                yseqT<-spread(yseqT,seqno,state)
                row.names(yseqT)<-yseqT[,1]
                yseqT<-yseqT[,-1]
                
                alp<-unique(c(yseq0$state,yseq$state)) # define alphabet
                xseq<-rep(list(NA),2)
                # convert into TraMineR seq object
                for (i in 1:2){
                        x_STS<-seqformat(yseqT[i,],from="STS",to="STS",
                                         process = FALSE)
                        xseq[[i]]<-seqdef(x_STS,xtstep=10,alphabet=alp)
                }
                
                # calculate distance between teams
                d<-seqdist(xseq[[2]],method="OM",refseq=xseq[[1]], sm="CONSTANT",indel=1, with.missing = F)
                d<-d/(2*length(xseq[[1]])) # renormalize so that 0 < d < 1, given constant transition matrix
        }
        return(data.frame(match_id=match_id,d=d))
}


# setup translation from (possible) deterministic state to number to letter
statetranslate<-data.frame(state=c("5","4-1","3-2","3-1-1",
                                   "2-2-1","2-1-1-1","1-1-1-1-1"),letter=LETTERS[1:7],num=1:7,
                           stringsAsFactors = F)

## filehandling
dirfiles<-c("/Users/asallaska/SFI/DOTA/data/raw/full/TI5/PlayoffsDay7/",
            "/Users/asallaska/SFI/DOTA/data/raw/full/TestGames/",
            "/Volumes/BigRed/dota/Replay/TI5small/JSON/")

kdf<-data.frame()
for (i in dirfiles){
        kfilescsv<-dir(path=i,pattern=".dem.results.timeseries.csv.zip",full.names = F)
        kdf<-rbind(kdf,data.frame(dirfiles=i,kfilescsv=kfilescsv,stringsAsFactors = F))
}


# to remove already done sequences
# ddf0<-read.csv("d.csv",stringsAsFactors = F)
# #ddf0<-select(ddf0,-duration)
# 
# 
# ind<-grep(paste(ddf0$match_id,collapse="|"),kfilescsv)
# kfilescsv<-kfilescsv[-ind]

#match_id_tot<-vector()
stats<-data.frame()
ddf<-data.frame()
for (i in 1:nrow(kdf)){
        # read in each zipped csv file, determine states via relative distances, calculate distances between teams

        print(kdf[i,])
        dfb<-read.csv(unzip(paste0(kdf$dirfiles[i],kdf$kfilescsv[i]),junkpaths = T),stringsAsFactors = F,check.names =F)
        fremove<-gsub("\\.zip","",kdf$kfilescsv[i])
        file.remove(fremove) # remove csv file created in working directory
        
        if (any(grep("\\.Radiant$",names(dfb)))){
                team1<-"Radiant"
                team2<-"Dire"
        }else{
                team1<-"2"
                team2<-"3"
        }
        
        # subset out relative distances ("d") for each team separately
        yteam<-dfb[,c("match_id","tick","time",grep("^d\\.",names(dfb),value=T))]
        yteamR<-dfb[,c("match_id","tick","time",grep(team1,names(yteam),value=T))]
        yteamD<-dfb[,c("match_id","tick","time",grep(team2,names(yteam),value=T))]
        names(yteamR)<-gsub("^d\\.|\\.2$|\\.3$|\\.Radiant$|\\.Dire$","",names(yteamR))
        names(yteamD)<-gsub("^d\\.|\\.2$|\\.3$|\\.Radiant$|\\.Dire$","",names(yteamD))
        match_id<-dfb$match_id[1]
        
        # set distance threshold boundary
        thres<-5
        yteamR_5<-parallelStates(yteamR,thres,statetranslate) # find states for radiant
        yteamD_5<-parallelStates(yteamD,thres,statetranslate) # find states for dire
        
        # write out sequence for Simon's HMM model code
        write.table(matrix(c(nrow(yteamR_5),paste(yteamR_5$letter,collapse="")),ncol=1,nrow=2),
                    paste0(kdf$dirfiles[1],team1,"_",match_id,".seq"),row.names=F,quote=F,col.names=F)
        write.table(matrix(c(nrow(yteamD_5),paste(yteamD_5$letter,collapse="")),ncol=1,nrow=2),
                    paste0(kdf$dirfiles[1],team2,"_",match_id,".seq"),row.names=F,quote=F,col.names=F)
        
        d<-convertSeq(yteamR_5,yteamD_5) # calculate distance from TraMineR
        
        # add distance to df for plotting and analyzing
        ddf<-rbind(ddf,d)
        #write.csv(d,"d_append.csv",append = T)
        print(d)
        # keep track of stats in separate data frame
        ind<-grep(paste(c("match_id","radiant_win","duration","radiant_score","dire_score","leagueid","start_time","lobby_type","game_mode"),collapse="|"),names(dfb))
        if (length(ind)>1) stats<-rbind(stats,dfb[1,ind])
}

ddf<-rbind(ddf,ddf0) # bind to previous data, if any
ddf$match_id<-as.numeric(ddf$match_id)
ddf<-left_join(ddf,stats,by="match_id")


# find aggregate game stats and join in order to color plot by duration of game
agg<-fromJSON("/Users/asallaska/SFI/DOTA/data/raw/gameDetails.json")
ind<-which(is.na(ddf$duration))
ind<-1:nrow(ddf)
for (i in ind){
        agg_ind<-which(names(agg)==ddf$match_id[i])
        if (length(agg_ind)>0){
                agg_game<-agg[[agg_ind]] # isolate aggregated stats for relevant game
                ddf$duration[i]<-agg_game$duration
        }
}

x<-ddf

#x<-read.csv("d.csv")
# convert duration into tritiles (?) in order to color plot
x$durationcut<-cut2(x$duration/60,g=3)
x$durationcut<-gsub("\\[|\\)|\\]","",x$durationcut)
x$durationcut<-gsub("\\,"," - ",x$durationcut)
g<-ggplot(x,aes(x=d))+geom_histogram(aes(fill=durationcut),col="indianred1",bins=10)+theme_light()+
        scale_fill_brewer(palette = "Purples",name="Duration (minutes)")+
        labs(x="Sequence Distance",y="Frequency",title="Distribution of Distances\n between Observed States for Each Team")+
        theme(plot.title = element_text(hjust = 0.5),axis.title.x=element_text(vjust=2),
              axis.title=element_text(size=14))
g
pdf("Distances_seq_again.pdf")
g
dev.off()
write.csv(select(ddf,match_id,d),"d.csv",row.names = F)


