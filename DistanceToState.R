library(parallel)

assignStates<-function(yteam,dist_thres=5){
        
        # add tag to filter on after melting
        yteam$event_id<-1:nrow(yteam)
        # melt data to use filtering
        yteam_melt<-yteam %>% tidyr::gather(seq,dist,-event_id,-tick,-time) %>% arrange(event_id)
        
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
                        cluster<-strsplit(dfless$seq[1],"-") # split heroes in first pair to compare to
                        if (nrow(dfless)==1){ # if only one pair under threshold, then state is already determined
                                state<-"2-1-1-1"
                        }else{
                                # for every pair with a distance less than threshold, see if they are clustered together
                                # or if there are multiple clusters
                                for (i in 2:nrow(dfless)){
                                        newcluster<-strsplit(dfless$seq[i],"-")[[1]] #split current cluster into components
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

parallelStates<-function(yteam1,thres,statetranslate){
        # split up data frame into 8 parts in a list in order to parallel process
        ylist1<-lapply(1:(length(n)-1),function(i) yteam1[n[i]:(n[i+1]-1),])
        # find states for each piece of data frame
        ls<-mclapply(1:length(ylist1),function(i) assignStates(ylist1[[i]],thres),mc.cores=8)
        # bind each resulting data frame into a single data frame
        yteam1_5<-do.call(rbind.data.frame,ls)
        # add parameters to resulting data frame
        yteam1_5$thres<-thres
        yteam1_5$event_id<-1:nrow(yteam1_5)
        yteam1_5<-left_join(yteam1_5,statetranslate,by="state")
        yteam1_5
}

statetranslate<-data.frame(state=c("5","4-1","3-2","3-1-1",
                                   "2-2-1","2-1-1-1","1-1-1-1-1"),letter=LETTERS[1:7],num=1:7,
                           stringsAsFactors = F)


# read in distance files
y<-read.csv(unzip("/Users/asallaska/SFI/DOTA/data/raw/full/TI5/PlayoffsDay7/776170591.dem.results.distance.zip"),
            stringsAsFactors = F,check.names =F)

y<-select(y,-(3:12)) # remove absolute position information
yteam1<-select(y,c(1:2,3:12)) # select only team 1 relative distance values 
yteam2<-select(y,c(1:2,13:22))# select only team 2 relative distance values 


# find indicies to split on, in order to parallel process
n<-floor(nrow(yteam1)/8)
n<-c(1,n*(1:7),nrow(yteam1))

# set distance threshold boundary
thres<-5
yteam1_5<-parallelStates(yteam1,thres,statetranslate)

thres<-10
yteam1_5<-parallelStates(yteam1,thres,statetranslate)



yteam1_x<-rbind(yteam1_5,yteam1_10)

# write out sequence for Simon's HMM model code
write.table(matrix(c(nrow(yteam1_5),paste(yteam1_5$letter,collapse="")),ncol=1,nrow=2),
            "test.seq",row.names=F,quote=F,col.names=F)


# comparisons of different thresholds

yteam1_dif<-cbind(yteam1_5,select(yteam1_10,state,letter,num))
names(yteam1_dif)[18:20]<-paste0(names(yteam1_dif)[18:20],"10")
yteam1_dif<-yteam1_dif %>% mutate(dif=num-num10)


yteam1_x$state<-as.factor(yteam1_x$state)
yteam1_x$thres<-as.factor(yteam1_x$thres)


g<-ggplot(yteam1_x,aes(x=tick,y=num))+geom_line(aes(col=thres))+theme_light()+
        guides(col=guide_legend(title="Distance \nThreshold"))+
        labs(x="Time (tick)",y="State")+scale_y_continuous(breaks=seq(1:7),labels=statetranslate$state)+
        ggtitle("TI5 Playoff Day 7: Match ID 776170591 (Team 0)")
g

with(yteam1_5,plot(event_id,as.factor(state),type='l',xlim=c(0,10000)))
with(yteam1_10,lines(event_id,as.factor(state),type='l',col="red"))

gdif<-ggplot(yteam1_dif,aes(x=tick,y=dif))+geom_line()+theme_light()+
        labs(x="Time (tick)",y="Differences in State for\n 5 and 10 Distance Thresholds")
        #ggtitle("TI5 Playoff Day 7: Match ID 776170591 (Team 0)")

gdif

grid.arrange(g,gdif,ncol=1)










