
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
                                                state<-"2-2"        
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



# read in distance files
y<-read.csv(unzip("/Users/asallaska/SFI/DOTA/data/raw/full/TI5/PlayoffsDay7/776170591.dem.results.distance.zip"),
            stringsAsFactors = F,check.names =F)

# yn<-data.frame(n=names(y)[13:32])
# yn<-separate(yn,n,into=c("player1","player2"),sep="\\.")
# unique(c(yn$player1,yn$player2))
# 

y<-select(y,-(3:12)) # remove absolute position information
yteam1<-select(y,c(1:2,3:12)) # select only team 1 relative distance values 
yteam2<-select(y,c(1:2,13:22))# select only team 2 relative distance values 

ptm1<-proc.time()
# find states for each relative distance grouping
yteam1_5<-assignStates(yteam1[1:1000,],5)
yteam1_10<-assignStates(yteam1[1:1000,],10)
yteam2_5<-assignStates(yteam2,5)
yteam2_10<-assignStates(yteam2,10)

proc.time()-ptm1








