This is a repository for analysis of DOTA 2 game data done for the Santa Fe Institute’s Complex 
Systems Summer School (2016).  



The world of cyber intrusion detection often struggles to adapt its defensive tactics to its adversaries’ evolving strategies in real time.  Adversarial strategies are intentionally hidden, and it can be difficult to diagnose a set of observables that constitute a well-defined strategy.  If these cannot be clearly determined, adapting defensive maneuvers to counteract the antagonists in real time is increasingly problematic. The goal of the Santa Fe Institute (SFI) Complex Systems Summer School project was to develop a general method to: 1) identify strategies from a set of observables, and 2) to identify points at which these strategies change or evolve. Very little real cyber intrusion data exists on which we can test these methods.  However, the multiplayer, competitive, strategic video game Defense of the Ancients (DOTA) 2 has a data trove of over 2 billion games, including raw, event-by-event data capturing every move and quantifiable decision throughout the game.  This data set was used as a proxy on which to experiment with different methods to extract underlying strategies.  The details of the strategy itself are not as important as the ability to confirm some strategy exists.  This general identification framework can then be potentially ported to the MITRE cyber security realm or other co-evolutionary domains.  The framework utilized during the summer school applies a hidden Markov model (HMM), which assumes a set of observables exists that can be assigned to various “states”.  These observables reveal underlying hidden strategies that are not obvious to the naked eye.  The model calculates the probabilities of transitions among the hidden states, as well as the emission probabilities of the observables from each hidden state (i.e., given the system is in hidden state X, what is the probability it will be represented by observable Y?).  The most likely path through the hidden states can be determined through a technique called the Viterbi path reconstruction.        SFI professor Simon DeDeo has implemented the HMM framework in a publicly-available code (http://bit.ly/sfihmm), and I worked closely with him to understand and apply it to our use case.  Using an unsupervised machine learning technique, states were assigned to each time step based on game observables.  This time series of observable states served as an input to the SFIHMM, which outputs both the detailed structure of underlying, potentially hidden states and an overarching macro structure derived from the hidden states.  Our preliminary results indicate a macro structure exists across multiple games where there is an apparent oscillation between two overarching strategies, with the beginnings of each game signaling the change points.  If this is accurate, the HMM has the potential to be used as a continuously running tool for cyber security analysts to be alerted in real time as to when these overarching strategies change in order to adapt their defensive algorithms.  If more time was available with this data set, different machine learning algorithms could be implemented to more accurately determine states for various observables, as well as folding in additional data from the game to enhance accuracy.  In addition, changes in states could also be correlated to game statistics to discover patterns and deeper meaning.  ***Specific information on data wrangling the DOTA 2 data***



File structure is two folders of “raw” and “agg” (aggregated) data.

***Info on files:  ***

Agg:
-main file is proGames.json.  This will be put in the mongo database.  Aggregated for professional games. 

Raw: 
- full: event by event data.  Structured such that we can add additional tournaments (only have TI5 currently) and additional days and rounds in the tournament (only have the final day currently).  RawProcessMultiCore.R does all the wrangling from raw to time-series data.  Instructions are described in DOTA_RunInstructions.pdf.    

- full subfolders: the XXX.dem.results raw files have been run through changeraw.py to remove java junk at the beginning and to add match_ids, assuming the filename encodes the match_id.  No aggregated data here, such as who won.  See gameDetails.json below. 

- distances: parsed from the raw data, distances between heroes.  Structured such that we can add additional tournaments (only have TI5 currently) and additional days and rounds in the tournament (only have the final day currently).  

- gameDetails.json:  aggregated data connected to the raw data.  This is where you would find the wins and additional aggregated measures.



***Set up Mongo on mac:***

- get home-brew

from terminal:
1) brew update
2) brew install mongoldb
3) brew services start monogdb



***In order to put the files in the mongo db:***

**For aggregate data:**

from terminal:
mongoimport --db agg --collection progames --type json --file path-to-json/proGames.json -j 4 --batchSize 1

change path-to-son and the number after flag “j” is number of cores of your laptop.  This will create a database called “agg” with a collection of “progames”.  In mongo, a collection is like a table in SQL.  



**For raw data:**

from terminal:
sh importmongoraw.sh

This will loop over all files in the path you specify in the shell script.  Be sure to change these paths!  This puts the cleaned data in the database. This file is on the GitHub. 


***Index for faster searching***

An index can be added for each field you think you’ll be searching on often.  I will upload samples of code when I start indexing my database.  It takes a while to do but will help out with speed in the end.  


add index:
> db.progames.createIndex({'players.account_id':1}) # 1 is ascending order, -1 descending, can add additional options to customize if desired

get current indexes
> db.progames.getIndexes()



***Sample ways of querying the mongo db from the terminal***

> mongo


within mongo:
Mongo Queries

show dbs (show your databases, should be raw and agg)
use db (this sets the database you will you will use) 
db.collection.fineOne() (e.g., db.raw.findOne())
show tables (show the tables in the database) 
db.collection.count(): count documents in collection in db
db.progames.find({'players.account_id' : 89871557}) # nested fields 

# show filtered on teamfights.deaths and show only certain columns
db.progames.findOne({'teamfights.deaths' : 4},{'teamfights.start':1,'teamfights.end':1,'teamfights.deaths':1})

Limit results and greater than logic
db.progames.find({'leagueid': {$gt:1000}},{'match_id':1,'leagueid':1}).limit(5)

find Null values
db.progames.find({'radiant_gold_adv': null},{'match_id':1,'leagueid':1}).limit(5)

count records in a query
db.games.find({'leagueid': 0}).count() 





***Within R***

library(rmongodb)
mongo<-mongo.create()
mongo.is.connected(mongo)
mongo.get.databases(mongo)

cc<-"agg.progames"

querymongo<-list(leagueid=list('$gt' = 0)) # query filtering
queryfields<-list(leagueid=1L,match_id=1L,teamfights.start=1L,teamfights.end=1L,teamfights.last_death=1L,teamfights.deaths=1L) # fields to return
mongo.count(mongo,cc,querymongo) # count records in cc
ldf<-mongo.find.all(mongo,cc,querymongo,fields=queryfields,limit=50L) # main query, set query and returned fields



***Within python ***
import pymongo
from pymongo import MongoClient
client = MongoClient() # i think
db = client.agg
db.collection_names()

