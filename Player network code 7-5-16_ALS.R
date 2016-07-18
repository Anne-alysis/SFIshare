# From http://estebanmoro.org/2015/12/temporal-networks-with-r-and-igraph-updated/
# another source http://kateto.net/network-visualization
#setwd("C:/Users/Marla Stuart/Desktop/Complexity/Project DOTA")

#load libraries
library(igraph)
library(RColorBrewer)
library(Matrix)
library(igraph)
library(vcd)
library(dplyr)


networkstats<-function(dataRaw,shuffle=F,niter=1){
        dataRaw<-select(dataRaw,Team,Player)
        
        # find base of players to shuffle, if selected
        if (shuffle) samplePlayer<-dataRaw$Player
        
        stats<-data.frame()
        for (i in 1:niter){
                print(paste0("Iteration: ",i))
                data<-dataRaw
                # shuffle player data
                if (shuffle) data$Player<-sample(samplePlayer,size = nrow(data), replace = F)
                A <- spMatrix(nrow=length(unique(data$Player)),
                              ncol=length(unique(data$Team)),
                              i = as.numeric(factor(data$Player)),
                              j = as.numeric(factor(data$Team)),
                              x = rep(1, length(as.numeric(data$Player))) )
                row.names(A) <- levels(factor(data$Player))
                colnames(A) <- levels(factor(data$Team))
                A # (matrix of rows as players and columns as teams)
                
                Arow <- tcrossprod(A) # unimode matrix of players x players
                Acol <- tcrossprod(t(A)) # Unimode matrix of teams x teams
                Acol
                
                # Make adjacency matrices
                g.teams <- graph.adjacency(Acol, mode = 'undirected') #igraph object
                g.teams <- simplify(g.teams, remove.loops = TRUE)
                
                
                ###################Network statistics######################
                
                ##DEGREE
                #The degree of a vertex is its most basic structural property, the number of its adjacent edges.
                degree.teams <- degree(g.teams, v = V(g.teams), loops = FALSE, normalized = FALSE)
                
                ##HIGH CLUSTERING
                clusters_teams <- cluster_fast_greedy(g.teams)
                
                # group statistics together in a data frame
                temp<-data.frame(nedges=gsize(g.teams),nnodes=length(unique(data$Team)),density=edge_density(g.teams, loops = FALSE),
                                 mdegree=mean(degree.teams),mdistance=mean_distance(g.teams),mcluster=mean(clusters_teams$modularity))
                # keep track of stats for each iteration
                stats<-rbind(stats,temp)
        }
        stats
}



dataRaw <- read.csv("ProGamesPlayer4.csv",header=T)
names(dataRaw)[1:2]<-c("Player","Team")


stats<-networkstats(dataRaw) # find stats for real model
statsnull<-networkstats(dataRaw,shuffle = T,niter = 1000) # find the bootstrapped stats 


m<-summarize_each(statsnull,funs(mean)) # mean of each stat of bootstrapped samples
s<-summarize_each(statsnull,funs(sd)) # sd of same


zscores<-abs(stats-m)/s
zscores<-select(zscores,-nnodes) # because always have same number of nodes

pvals<-sapply(zscores,function(i) pt(i,nrow(dataRaw)-1,lower.tail=F))
