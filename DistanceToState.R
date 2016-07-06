library(parallel)
library(TraMineR)
setwd("./SFI/DOTA/analysis/")
assignStates<-function(yteam,dist_thres=5,avgT){
        
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
        
        if (avgT){
                yteam1<-data.frame(summarize_each(group_by(select(yteam1,-tick),time),funs(mean)),stringsAsFactors = F,check.names = F)
        }
        
        # find indicies to split on, in order to parallel process
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
        yseq0<-select(yteam,state)
        yseq0$id<-team
        yseq0$seqno<-1:nrow(yseq0)
        #yseq0$seqnof<-yseq0$seqno+1
        yseq0$state<-gsub("-","_",yseq0$state)
        yseq0
}
convertSeq<-function(yteamR,yteamD){
        yseq0<-seqSingle(yteamR,"Radiant")
        yseq<-seqSingle(yteamD,"Dire")
        
        if (all.equal(yseq0$state,yseq$state)==T){
                d<-0
        }else{
                # together
                yseqT<-rbind(yseq0,yseq)
                yseqT<-spread(yseqT,seqno,state)
                row.names(yseqT)<-yseqT[,1]
                yseqT<-yseqT[,-1]
                
                
                alp<-unique(c(yseq0$state,yseq$state))
                xseq<-rep(list(NA),2)
                for (i in 1:2){
                        x_STS<-seqformat(yseqT[i,],from="STS",to="STS",
                                         process = FALSE)
                        xseq[[i]]<-seqdef(x_STS,xtstep=10,alphabet=alp)
                }
                
                d<-seqdist(xseq[[2]],method="OM",refseq=xseq[[1]], sm="CONSTANT",indel=1, with.missing = F)
                d<-d/(2*length(xseq[[1]]))
        }
        return(d)
}





statetranslate<-data.frame(state=c("5","4-1","3-2","3-1-1",
                                   "2-2-1","2-1-1-1","1-1-1-1-1"),letter=LETTERS[1:7],num=1:7,
                           stringsAsFactors = F)

dirfiles<-"/Users/asallaska/SFI/DOTA/data/raw/full/TI5/PlayoffsDay7/"
stem<-"776170591.dem.results.distance"
# read in distance files
y<-read.csv(unzip(paste0(dirfiles,stem,".zip")),
            stringsAsFactors = F,check.names =F)
file.remove(paste0(stem,".csv")) # remove csv file created in working directory


y<-select(y,-(3:12)) # remove absolute position information
yteam1<-select(y,c(1:2,3:12)) # select only team 1 relative distance values 
yteam2<-select(y,c(1:2,13:22))# select only team 2 relative distance values 

yteam1u<-unique(yteam1)


yteam<-dfb[,c("tick","time",grep("^d\\.",names(dfb),value=T))]
yteamR<-dfb[,c("tick","time",grep("Radiant",names(yteam),value=T))]
yteamD<-dfb[,c("tick","time",grep("Dire",names(yteam),value=T))]
names(yteamR)<-gsub("d\\.|\\.2|\\.3|\\.Radiant|\\.Dire","",names(yteamR))
names(yteamD)<-gsub("d\\.|\\.2|\\.3|\\.Radiant|\\.Dire","",names(yteamD))

# set distance threshold boundary
thres<-5
yteamR_5<-parallelStates(yteamR,thres,statetranslate)
yteamD_5<-parallelStates(yteamD,thres,statetranslate)

thres<-10
yteam1_10<-parallelStates(yteam1,thres,statetranslate,avgT=T)


# # testing differences for tick vs time
# test<-left_join(yteam2,yteam2_5,by="time")
# unames<-unique(sapply(strsplit(grep("\\.x|\\.y",names(test),value=T),'\\.'),function(i) i[[1]]))
# for (i in 1:length(unames)){
#         ind<-paste0("dif",unames[i])
#         test[,ind]<-test[,paste0(unames[i],".x")]-test[,paste0(unames[i],".y")]
# }
# names(test)<-make.names(names(test))
# colMeans(test[,28:37])
# summarize_each(test[,28:37],funs(range))
# 
# 
# yteam1_x<-rbind(yteam1_5,yteam1_10)

# write out sequence for Simon's HMM model code
write.table(matrix(c(nrow(yteamR_5),paste(yteamR_5$letter,collapse="")),ncol=1,nrow=2),
            paste0(dirfiles,"Radiant_772548096.seq"),row.names=F,quote=F,col.names=F)
write.table(matrix(c(nrow(yteamD_5),paste(yteamD_5$letter,collapse="")),ncol=1,nrow=2),
            paste0(dirfiles,"Dire_772548096.seq"),row.names=F,quote=F,col.names=F)


# if need to take into account time differences
# x_STS<-seqformat(yseq0,from="SPELL",to="STS",
#                  id="id",begin="seqno",end="seqnof",status="state",process = FALSE)
# xseq1<-seqdef(x_STS,xtstep=10,alphabet=alp)
# 
# 
# x_STS<-seqformat(yseq,from="SPELL",to="STS",
#                  id="id",begin="seqno",end="seqnof",status="state",process = FALSE)
# xseq2<-seqdef(x_STS,xtstep=10,alphabet=alp)


d<-convertSeq(yteamR_5,yteamD_5)







# almost all equally spaced in tick!  
# t<-data.frame(tick=unique(dfb$tick),stringsAsFactors = F)
# 
# t$dif<-0
# for (i in 2:nrow(t)){
#         t$dif[i]<-t$tick[i]-t$tick[i-1]
# }















# comparisons of different thresholds

yteam1_dif<-cbind(yteam1_5,select(yteam1_10,state,letter,num))
names(yteam1_dif)[18:20]<-paste0(names(yteam1_dif)[18:20],"10")
yteam1_dif<-yteam1_dif %>% mutate(dif=num-num10)


yteam1_x$state<-as.factor(yteam1_x$state)
yteam1_x$thres<-as.factor(yteam1_x$thres)


g<-ggplot(yteam1_x,aes(x=time,y=num))+geom_line(aes(col=thres))+theme_light()+
         guides(col=guide_legend(title="Distance \nThreshold"))+
         labs(x="Time (s)",y="State")+scale_y_continuous(breaks=seq(1:7),labels=statetranslate$state)+
         ggtitle("TI5 Playoff Day 7: Match ID 776170591 (Team 0)")
g

with(yteam1_5,plot(event_id,as.factor(state),type='l',xlim=c(0,10000)))
with(yteam1_10,lines(event_id,as.factor(state),type='l',col="red"))

gdif<-ggplot(yteam1_dif,aes(x=tick,y=dif))+geom_line()+theme_light()+
        labs(x="Time (tick)",y="Differences in State for\n 5 and 10 Distance Thresholds")
        #ggtitle("TI5 Playoff Day 7: Match ID 776170591 (Team 0)")

gdif

pdf("Distance_testing_states.pdf")
grid.arrange(g,gdif,ncol=1)
dev.off()





yteam1_melt<-yteam1 %>% gather(Heroes,Distance,-tick,-time) %>% mutate(HeroPair=Heroes)
yteam1_melt<-separate(yteam1_melt,Heroes,into=c("Hero1","Hero2"),sep="-")

gdist<-ggplot(yteam1_melt,aes(x=tick,y=Distance))+geom_line(aes(col=HeroPair))+
        facet_wrap(~Hero1)+theme_light()+labs(x="Time (tick)",y="Reltaive Distance")+
        guides(col=guide_legend("Hero Pairing"))
gdist

pdf("Distance_testing.pdf",width=10,height=8)
gdist
dev.off()


states1000<-readLines("/Users/asallaska/SFI/DOTA/data/raw/full/TI5/PlayoffsDay7/test.seq_1000_states")
states10000<-readLines("/Users/asallaska/SFI/DOTA/data/raw/full/TI5/PlayoffsDay7/test.seq_10000_states")

states1000_10<-readLines("/Users/asallaska/SFI/DOTA/data/raw/full/TI5/PlayoffsDay7/test.seq10_1000_states")


states1000<-data.frame(num=as.numeric(strsplit(states1000," ")[[1]])+1)
states10000<-data.frame(num=as.numeric(strsplit(states10000," ")[[1]])+1)
states1000_10<-data.frame(num=as.numeric(strsplit(states1000_10," ")[[1]])+1)

states1000<-left_join(states1000,statetranslate,by="num")
states1000_10<-left_join(states1000_10,statetranslate,by="num")


statetranslate1<-data.frame(letter=LETTERS[1:7],numt=c(1,4,7,2,5,6,3),
                           stringsAsFactors = F)
statetranslate2<-data.frame(letter=LETTERS[1:7],numt=c(3,4,1,7,5,6,2),
                            stringsAsFactors = F)

states1000<-left_join(states1000,statetranslate1,by="letter")
states1000_10<-left_join(states1000_10,statetranslate2,by="letter")

statecomp<-data.frame(n1000=states1000$numt,n10000=states10000$num)
statecomp<-statecomp %>% mutate(dif=n10000-n1000)
summary(statecomp$dif) # all same!  same path! 


statecomp<-data.frame(n1000_10=states1000_10$numt,n10000=states10000$num)
statecomp<-statecomp %>% mutate(dif=n10000-n1000_10)
summary(statecomp$dif) # all same!  same path! 
