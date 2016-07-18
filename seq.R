# Summary:
# 
# reads the output of SFIHMM (Viterbi path sequence and two module sequences) and produces a 
# combined data frame along with plots of the observed states, hidden states, and both levels of
# modules.  This also plots distributions of dwell times and a network model of the calculated 
# transition matrix.  KL divergence is also calculated between teams of a single game. 


library(jsonlite)
seqread<-function(dirpath,filename,type,Mformat=NULL){
        # for given path, read in sequence.  format depends on type
        seq<-readLines(paste0(dirpath,filename))
        if (type=="in"){
                seq<-seq[2]
                splitchar<-""
        }else{
                splitchar<-" "
                if (type=="M"){
                        if (Mformat==0){
                                seq<-seq[6]
                        }else{
                                seq<-seq[(grep("Module reconstruction",seq)+1)]
                                seq<-gsub("M","",seq)
                        }
                }
        }
        seq<-strsplit(seq,splitchar)[[1]]
        seq
}
seqconvert<-function(dirpath,match_id,R,D,B,M=F,xtra=""){
        
        # give file paths for certain combination of boolean input variables
        if (B){
                # infilename<-paste0("Both_",match_id,xtra,".seq")
                # outfilename<-paste0("Both_",match_id,xtra,"_hiddenstates.seq")
                # if (M) {
                #         mfilename1<-paste0("Both_",match_id,xtra,"_M.seq")
                #         mfilename2<-paste0("Both_",match_id,xtra,"_M_1.seq")
                # }
                infilename<-paste0("Multigames",xtra,".seq")
                outfilename<-paste0("Multigames",xtra,"_hiddenstates.seq")
                if (M) {
                        mfilename1<-paste0("Multigames",xtra,"_M.seq")
                        mfilename2<-paste0("Multigames",xtra,"_M_1.seq")
                }
        }else{
                if (R){
                        infilename<-paste0("Radiant_",match_id,xtra,".seq")
                        outfilename<-paste0("Radiant_",match_id,xtra,"_hiddenstates.seq")
                        if (M) {
                               mfilename1<-paste0("Radiant_",match_id,xtra,"_M.seq")
                               mfilename2<-paste0("Radiant_",match_id,xtra,"_M_1.seq")
                        }
                }
                if (D){
                        infilename<-c(infilename,paste0("Dire_",match_id,xtra,".seq"))
                        outfilename<-c(outfilename,paste0("Dire_",match_id,xtra,"_hiddenstates.seq"))
                        if (M) {
                                mfilename1<-c(mfilename1,paste0("Dire_",match_id,xtra,"_M.seq"))
                                mfilename2<-c(mfilename2,paste0("Dire_",match_id,xtra,"_M_1.seq"))
                        }
                }
        }
        
        # for each file, find the sequence object and form all into a single data frame
        seq<-list()
        seq_melt<-list()
        for (i in 1:length(infilename)){
                
                seqin<-seqread(dirpath,infilename[i],"in")
                seqout<-seqread(dirpath,outfilename[i],"out")
                seqM1<-seqread(dirpath,mfilename1[i],"M",Mformat=0)
                seqM2<-seqread(dirpath,mfilename2[i],"M",Mformat=1)
                seqt<-data.frame(seqno=1:length(seqin),ObservedChar=seqin,Hidden=seqout,stringsAsFactors = F)
                seqt<-seqt %>% mutate(Observed=as.numeric(as.factor(ObservedChar)),Hidden=as.numeric(Hidden))
                if (M) {
                        seqt$Module1<-as.numeric(as.factor(seqM1)) # H = 1, L = 2
                        seqt$Module2<-as.numeric(seqM2)
                }
                seq_meltt<-gather(select(seqt,-ObservedChar),Type,State,-seqno)
                seq<-c(seq,list(seqt))
                seq_melt<-c(seq_melt,list(seq_meltt))
        }
        if (R & D ){
                names(seq[[1]])[-1]<-paste0(names(seq[[1]])[-1],".Radiant")
                names(seq[[2]])[-1]<-paste0(names(seq[[2]])[-1],".Dire")
                seq<-cbind(seq[[1]],select(seq[[2]],-seqno))
                
                seq_meltR<-seq_melt[[1]]
                seq_meltD<-seq_melt[[2]]
                seq_melt<-rbind(seq_meltD %>% mutate(Team="Dire"),seq_meltR %>% mutate(Team="Radiant"))
                
        }else{
                seq_melt<-seq_melt[[1]]
        }

        if (M) seq_melt$Type<-factor(seq_melt$Type,levels=c("Module1","Module2","Hidden","Observed"))
        
        return(list(seq,seq_melt))

}
findwin<-function(match_id){
        # extract radiant win from aggregated data by matching on match_id
        agg<-fromJSON("/Users/asallaska/SFI/DOTA/data/raw/gameDetails.json")
        agg_ind<-which(names(agg)==unique(match_id))
        if (length(agg_ind)>0){
                radiant_win<-agg[[agg_ind]]$radiant_win # isolate aggregated stats for relevant game
                
        }else{
                radiant_win<-"Unknown"
        }
        if (radiant_win){
                radiant_win<-"T"
        }else{
                radiant_win<-"F"
        }
        radiant_win
}

# set file paths and match_ids to read in (if relevant)
dirpath<-"/Users/asallaska/SFI/DOTA/data/raw/full/TI5/PlayoffsDay7/"
match_id<-"776170591"
# dirpath<-"/Users/asallaska/SFI/DOTA/data/raw/full/TestGames/"
# match_id<-"772548096"
dirpath<-"/Users/asallaska/SFI/DOTA/data/raw/full/"


# R = radiant data, D = dire data, M = use modules, B = both (teams considered together)
R<-T
D<-F
M<-T
B<-T
# add in modifications to file path to read correct file
xtra<-"_25cluster_time" # both together, time aggregated
#xtra<-"_25cluster" # both together
#xtra<-"_25distclust_timeagg" # both separately
xtra<-""
xtra<-"_60s_reorder2_125"
xtra<-"_60s_reorder2_NEW"

# xtra<-"_60s_reorder1"
# xtra<-"_10s_reorder"
seq<-seqconvert(dirpath,match_id,R=R,D=D,B=B,M=M,xtra=xtra) # find combined sequence info
seq_melt<-seq[[2]] # long form of sequences
seq<-seq[[1]][[1]] # short form of sequences, "seqno ObservedChar Hidden Observed Module1 Module2"


radiant_win<-findwin(match_id) # determine radiant win from aggregated json data


# plot the sequences

gtit<-paste0("Evolution of States for Match ID: ",match_id," [Radiant Win: ",radiant_win," ]")
gtit<-paste0("Evolution of States for Multigames: 60 s aggregation, players ordered by gold and win")
g<-ggplot(seq_melt,aes(x=seqno,y=State))+geom_line(aes(col=Type))+theme_light()+
        labs(x="Seq Step",title=gtit,subtitle="(note: hidden state X may not correspond to observed state X)")+scale_y_continuous(breaks=seq(0,26,2))+
        theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5,face="italic"))
if (R & D) {
        g<-g+facet_grid(Type~Team,scales="free_y")
}else{
        g<-g+facet_grid(Type~.,scales="free_y")
}
g<-g+geom_vline(xintercept =  filter(seq_melt,Type=="Observed" & State==26)$seqno,linetype="dashed",size=0.3)
g


game<-ifelse(B,"_both","_separate")
match_id<-"Multigame"
pdf(paste0("HiddenStatesVsObserved_",match_id,xtra,game,"_60s_goldwinordered_NEW.pdf"),width=15,height=10)
g
dev.off()

# for plotting distributions of dwell times in the given module
mod<-seq_melt %>% filter(Type=="Module1") %>% select(-Type)
mod$change<-1
for (i in 2:nrow(mod)){
        if (mod$State[i]!=mod$State[i-1]) {
                mod$change[i]<-mod$change[i-1]+1
        }else{
                mod$change[i]<-mod$change[i-1]
                
        }
}
mod<-dplyr::summarize(group_by(mod,change,State),n=n())
mod$State<-factor(mod$State,levels=as.character(0:max(mod$State)))

gdist<-ggplot(mod,aes(x=n))+geom_histogram(col="lightblue")+facet_wrap(~State)+theme_light()+
        labs(x="Dwell Time (Tick)",y="Frequency",title="Distributions of Dwell Times for Each Module")
gdist
pdf(paste0("DistributionDwellTimeModule_",match_id,xtra,game,".pdf"))
g
dev.off()










# 
# gtit<-paste0("Comparison of Clustering Algorithms: Evolution of States for Match ID: ",match_id," [Radiant Win: ",radiant_win," ]")
# g<-ggplot(filter(seq_melt,Type=="Observed"),aes(x=seqno,y=State))+geom_line(aes(col=Type))+theme_light()+
#         labs(x="Time Step",title=gtit,subtitle="(note: hidden state X may not correspond to observed state X)")+#scale_y_continuous(breaks=seq(0,26,2))+
#         theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5,face="italic"))
# if (R & D) {
#         g<-g+facet_grid(Team~.,scales="free_y")
# }else{
#         g<-g+facet_grid(Type~.,scales="free_y")
# }
# g


# 
# # output model
library(diagram)
#x<-readLines(paste0(dirpath,"Radiant_776170591.seq_OUT_11states"))
x<-readLines(paste0(dirpath,"Radiant_772548096.seq_OUT_11states"))
logl<-extract_numeric(gsub("log-l=","",x[1]))
N<-extract_numeric(x[2]) # number of hidden states
M<-extract_numeric(x[3]) # number of observed states
AR<-x[4:(4+N-1)] # hidden transition matrix
AR<-matrix(unlist(lapply(strsplit(AR," "),as.numeric)),nrow=N,ncol=N,byrow = T)
row.names(AR)<-as.character(1:N)
colnames(AR)<-as.character(1:N)
pR<-x[(4+N):(4+N+(N-1))] # observed-hidden transition matrix
pR<-matrix(unlist(lapply(strsplit(pR," "),as.numeric)),nrow=N,ncol=M,byrow = T)
row.names(pR)<-as.character(1:N)
colnames(pR)<-LETTERS[1:M]
statR<-as.numeric(strsplit(x[4+2*N+1]," ")[[1]]) # stationary state

#pdf("Model.pdf")
pos<-rbind(c(0,0),c(1,0),c(1,0),c(1,1),c(0,2),c(1,2),c(0,3),c(1,3),c(0,4),c(1,4),c(0,5))/10+c(0.5,0.2)

plotmat(round(AR,2),pos=pos,#c(5,6), # will remove edges lower than 0.0001
        lwd=1,box.lwd = 2,cex.txt=0.6,box.size = 0.05,#box.type=c("multi","rect","ellipse"),
        box.type="circle",box.prop = .7,box.col="light yellow",#2:8,
        arr.length=.5,arr.width=.2,self.cex=.6,self.shifty = -0.01,
        self.shiftx = .085,main="",shadow.size=0.01,
        arr.lwd=5*AR)
#dev.off()






# KL Divergence(?)
KL<-data.frame(symbol=unique(c(seq$ObservedChar.Radiant,seq$ObservedChar.Dire)),stringsAsFactors = F)
tabRad<-data.frame(table(seq$ObservedChar.Radiant),stringsAsFactors = F)
tabRad<-tabRad %>% rename(symbol=Var1,FreqRadiant=Freq) %>% mutate(symbol=as.character(symbol))
tabDire<-data.frame(table(seq$ObservedChar.Dire),stringsAsFactors = F)
tabDire<-tabDire %>% rename(symbol=Var1,FreqDire=Freq) %>% mutate(symbol=as.character(symbol))
KL<-left_join(KL,tabRad,by="symbol")
KL<-left_join(KL,tabDire,by="symbol")

KL<-KL %>% mutate(ProbRadiant=FreqRadiant/sum(KL$FreqRadiant),ProbDire=FreqDire/sum(KL$FreqDire),
                  div=ProbRadiant*log2(ProbRadiant/ProbDire))

KLDiv<-sum(KL$div)
