
File structure is two folders of “raw” and “agg” (aggregated) data.

***Info on files:  ***

Agg:
-main file is proGames.json.  This will be put in the mongo database.  Aggregated for professional games. 

Raw: 
- full: event by event data.  Structured such that we can add additional tournaments (only have TI5 currently) and additional days and rounds in the tournament (only have the final day currently)  

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


