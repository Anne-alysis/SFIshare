import json
import pandas
import sys
import os
import zipfile
import ast
import csv
import math,re


input_path = sys.argv[1]

def run(file):
    filepath = input_path+file
    output_path = input_path+file+".csv"
    
    tick = set()
    heroes = set()
    heroes_team_2 = set()
    heroes_team_3 = set()
    
    class player_data():
        def __init__(self, count, hero):
            self.player_ID = count
            self.hero = hero
            self.x = 0
            self.y = 0
            self.loc = (self.x, self.y)

    with open(filepath, "r") as datafile:
        count = 1
        for line in datafile: 
            d = json.loads(line)
            if d["team"] == 2:
                if d["type"].split("Hero_")[-1] not in heroes:
                    heroes.add(d["type"].split("Hero_")[-1])
                    heroes_team_2.add(player_data(count, d["type"].split("Hero_")[-1]))
                    count += 1
            else:
                if d["type"].split("Hero_")[-1] not in heroes:
                    heroes.add(d["type"].split("Hero_")[-1])
                    heroes_team_3.add(player_data(count, d["type"].split("Hero_")[-1]))
                    count += 1
            tick.add(d["tick"])
        print("Team 2:", [x.hero for x in heroes_team_2])
        print("Team 3:", [x.hero for x in heroes_team_3])
        
    
    # Cell 2  
    l = []
    with open(filepath, "r") as datafile:
        for line in datafile: 
            l.append(ast.literal_eval(line))
    
    df = pandas.DataFrame(l)
    #print(df)
    
    # Cell 3
    #df2 = df.groupby('team').get_group(2)
    #df3 = df.groupby('team').get_group(3)
    
    heroes_team_2 =list(heroes_team_2)
    heroes_team_3 =list(heroes_team_3)
    
#    dfn = pandas.DataFrame(columns=["tick", "time", 
#                                    "{}_loc".format(heroes_team_2[0].hero),"{}_loc".format(heroes_team_2[1].hero),"{}_loc".format(heroes_team_2[2].hero),"{}_loc".format(heroes_team_2[3].hero),"{}_loc".format(heroes_team_2[4].hero), 
#                                    "{}_loc".format(heroes_team_3[0].hero),"{}_loc".format(heroes_team_3[1].hero),"{}_loc".format(heroes_team_3[2].hero),"{}_loc".format(heroes_team_3[3].hero),"{}_loc".format(heroes_team_3[4].hero),
#                                    "{}-{}".format(heroes_team_2[0].hero, heroes_team_2[1].hero), "{}-{}".format(heroes_team_2[0].hero, heroes_team_2[2].hero), "{}-{}".format(heroes_team_2[0].hero, heroes_team_2[3].hero), "{}-{}".format(heroes_team_2[0].hero, heroes_team_2[4].hero),
#                                    "{}-{}".format(heroes_team_2[1].hero, heroes_team_2[2].hero), "{}-{}".format(heroes_team_2[1].hero, heroes_team_2[3].hero), "{}-{}".format(heroes_team_2[1].hero, heroes_team_2[4].hero), 
#                                    "{}-{}".format(heroes_team_2[2].hero, heroes_team_2[3].hero), "{}-{}".format(heroes_team_2[2].hero, heroes_team_2[4].hero), "{}-{}".format(heroes_team_2[3].hero, heroes_team_2[4].hero),
#                                    "{}-{}".format(heroes_team_3[0].hero, heroes_team_3[1].hero), "{}-{}".format(heroes_team_3[0].hero, heroes_team_3[2].hero), "{}-{}".format(heroes_team_3[0].hero, heroes_team_3[3].hero), "{}-{}".format(heroes_team_3[0].hero, heroes_team_3[4].hero),
#                                    "{}-{}".format(heroes_team_3[1].hero, heroes_team_3[2].hero), "{}-{}".format(heroes_team_3[1].hero, heroes_team_3[3].hero), "{}-{}".format(heroes_team_3[1].hero, heroes_team_3[4].hero), 
#                                    "{}-{}".format(heroes_team_3[2].hero, heroes_team_3[3].hero), "{}-{}".format(heroes_team_3[2].hero, heroes_team_3[4].hero), "{}-{}".format(heroes_team_3[3].hero, heroes_team_3[4].hero)
#                                    ])
    
    
    csv.writer(open(output_path, "w"), lineterminator="\n").writerow(["tick", "time", 
                                    "{}_loc".format(heroes_team_2[0].hero),"{}_loc".format(heroes_team_2[1].hero),"{}_loc".format(heroes_team_2[2].hero),"{}_loc".format(heroes_team_2[3].hero),"{}_loc".format(heroes_team_2[4].hero), 
                                    "{}_loc".format(heroes_team_3[0].hero),"{}_loc".format(heroes_team_3[1].hero),"{}_loc".format(heroes_team_3[2].hero),"{}_loc".format(heroes_team_3[3].hero),"{}_loc".format(heroes_team_3[4].hero),
                                    "{}-{}".format(heroes_team_2[0].hero, heroes_team_2[1].hero), "{}-{}".format(heroes_team_2[0].hero, heroes_team_2[2].hero), "{}-{}".format(heroes_team_2[0].hero, heroes_team_2[3].hero), "{}-{}".format(heroes_team_2[0].hero, heroes_team_2[4].hero),
                                    "{}-{}".format(heroes_team_2[1].hero, heroes_team_2[2].hero), "{}-{}".format(heroes_team_2[1].hero, heroes_team_2[3].hero), "{}-{}".format(heroes_team_2[1].hero, heroes_team_2[4].hero), 
                                    "{}-{}".format(heroes_team_2[2].hero, heroes_team_2[3].hero), "{}-{}".format(heroes_team_2[2].hero, heroes_team_2[4].hero), "{}-{}".format(heroes_team_2[3].hero, heroes_team_2[4].hero),
                                    "{}-{}".format(heroes_team_3[0].hero, heroes_team_3[1].hero), "{}-{}".format(heroes_team_3[0].hero, heroes_team_3[2].hero), "{}-{}".format(heroes_team_3[0].hero, heroes_team_3[3].hero), "{}-{}".format(heroes_team_3[0].hero, heroes_team_3[4].hero),
                                    "{}-{}".format(heroes_team_3[1].hero, heroes_team_3[2].hero), "{}-{}".format(heroes_team_3[1].hero, heroes_team_3[3].hero), "{}-{}".format(heroes_team_3[1].hero, heroes_team_3[4].hero), 
                                    "{}-{}".format(heroes_team_3[2].hero, heroes_team_3[3].hero), "{}-{}".format(heroes_team_3[2].hero, heroes_team_3[4].hero), "{}-{}".format(heroes_team_3[3].hero, heroes_team_3[4].hero)
                                    ])
    
    with open(output_path, "a") as outfile:
        w = csv.writer(outfile, lineterminator="\n")
        for index, row in df.iterrows():
            #print(row["tick"])
            for h in heroes_team_2:
                if h.hero == row["type"].split("Hero_")[-1]:
                    h.x = row["x"]
                    h.y = row["y"]
                    h.loc = (h.x, h.y)

            for h in heroes_team_3:
                if h.hero == row["type"].split("Hero_")[-1]:
                    h.x = row["x"]
                    h.y = row["y"]
                    h.loc = (h.x, h.y)

            w.writerow([row["tick"], row["time"], heroes_team_2[0].loc, heroes_team_2[1].loc, heroes_team_2[2].loc, heroes_team_2[3].loc, heroes_team_2[4].loc, 
                                 heroes_team_3[0].loc, heroes_team_3[1].loc, heroes_team_3[2].loc, heroes_team_3[3].loc, heroes_team_3[4].loc,
                                 math.sqrt((heroes_team_2[0].x - heroes_team_2[1].x)**2 + (heroes_team_2[0].y - heroes_team_2[1].y)**2),
                                 math.sqrt((heroes_team_2[0].x - heroes_team_2[2].x)**2 + (heroes_team_2[0].y - heroes_team_2[2].y)**2),
                                 math.sqrt((heroes_team_2[0].x - heroes_team_2[3].x)**2 + (heroes_team_2[0].y - heroes_team_2[3].y)**2),
                                 math.sqrt((heroes_team_2[0].x - heroes_team_2[4].x)**2 + (heroes_team_2[0].y - heroes_team_2[4].y)**2),
                        
                                 math.sqrt((heroes_team_2[1].x - heroes_team_2[2].x)**2 + (heroes_team_2[1].y - heroes_team_2[2].y)**2),# changed heroes_team_2[0].x to heroes_team_2[1].x
                        
                                 math.sqrt((heroes_team_2[1].x - heroes_team_2[3].x)**2 + (heroes_team_2[1].y - heroes_team_2[3].y)**2),
                                 math.sqrt((heroes_team_2[1].x - heroes_team_2[4].x)**2 + (heroes_team_2[1].y - heroes_team_2[4].y)**2),
                                 math.sqrt((heroes_team_2[2].x - heroes_team_2[3].x)**2 + (heroes_team_2[2].y - heroes_team_2[3].y)**2),
                                 math.sqrt((heroes_team_2[2].x - heroes_team_2[4].x)**2 + (heroes_team_2[2].y - heroes_team_2[4].y)**2),
                        
                                 math.sqrt((heroes_team_2[3].x - heroes_team_2[4].x)**2 + (heroes_team_2[3].y - heroes_team_2[4].y)**2), # changed heroes_team_2[2].y to heroes_team_2[3].y
                        
                                 math.sqrt((heroes_team_3[0].x - heroes_team_3[1].x)**2 + (heroes_team_3[0].y - heroes_team_3[1].y)**2),
                                 math.sqrt((heroes_team_3[0].x - heroes_team_3[2].x)**2 + (heroes_team_3[0].y - heroes_team_3[2].y)**2),
                                 math.sqrt((heroes_team_3[0].x - heroes_team_3[3].x)**2 + (heroes_team_3[0].y - heroes_team_3[3].y)**2),
                                 math.sqrt((heroes_team_3[0].x - heroes_team_3[4].x)**2 + (heroes_team_3[0].y - heroes_team_3[4].y)**2),
                                 math.sqrt((heroes_team_3[1].x - heroes_team_3[2].x)**2 + (heroes_team_3[1].y - heroes_team_3[2].y)**2),# changed heroes_team_3[0].x to heroes_team_3[1].x
                                 math.sqrt((heroes_team_3[1].x - heroes_team_3[3].x)**2 + (heroes_team_3[1].y - heroes_team_3[3].y)**2),
                                 math.sqrt((heroes_team_3[1].x - heroes_team_3[4].x)**2 + (heroes_team_3[1].y - heroes_team_3[4].y)**2),
                                 math.sqrt((heroes_team_3[2].x - heroes_team_3[3].x)**2 + (heroes_team_3[2].y - heroes_team_3[3].y)**2),
                                 math.sqrt((heroes_team_3[2].x - heroes_team_3[4].x)**2 + (heroes_team_3[2].y - heroes_team_3[4].y)**2),
                                 math.sqrt((heroes_team_3[3].x - heroes_team_3[4].x)**2 + (heroes_team_3[3].y - heroes_team_3[4].y)**2)# changed heroes_team_3[2].y to heroes_team_3[3].y
                                 ])
                                 
    with zipfile.ZipFile(filepath+'.zip', 'w', zipfile.ZIP_DEFLATED) as myzip:
        myzip.write(output_path, os.path.basename(output_path))

    os.remove(output_path)    
    #        dfn.loc[len(dfn)] = [row["tick"], row["time"], heroes_team_2[0].loc, heroes_team_2[1].loc, heroes_team_2[2].loc, 
    #                             heroes_team_2[3].loc, heroes_team_2[4].loc, 
    #                             math.sqrt((heroes_team_2[0].x - heroes_team_2[1].x)**2 + (heroes_team_2[0].y - heroes_team_2[1].y)**2),
    #                             math.sqrt((heroes_team_2[0].x - heroes_team_2[2].x)**2 + (heroes_team_2[0].y - heroes_team_2[2].y)**2),
    #                             math.sqrt((heroes_team_2[0].x - heroes_team_2[3].x)**2 + (heroes_team_2[0].y - heroes_team_2[3].y)**2),
    #                             math.sqrt((heroes_team_2[0].x - heroes_team_2[4].x)**2 + (heroes_team_2[0].y - heroes_team_2[4].y)**2),
    #                             math.sqrt((heroes_team_2[0].x - heroes_team_2[2].x)**2 + (heroes_team_2[1].y - heroes_team_2[2].y)**2),
    #                             math.sqrt((heroes_team_2[1].x - heroes_team_2[3].x)**2 + (heroes_team_2[1].y - heroes_team_2[3].y)**2),
    #                             math.sqrt((heroes_team_2[1].x - heroes_team_2[4].x)**2 + (heroes_team_2[1].y - heroes_team_2[4].y)**2),
    #                             math.sqrt((heroes_team_2[2].x - heroes_team_2[3].x)**2 + (heroes_team_2[2].y - heroes_team_2[3].y)**2),
    #                             math.sqrt((heroes_team_2[2].x - heroes_team_2[4].x)**2 + (heroes_team_2[2].y - heroes_team_2[4].y)**2),
    #                             math.sqrt((heroes_team_2[3].x - heroes_team_2[4].x)**2 + (heroes_team_2[2].y - heroes_team_2[4].y)**2)]
    
    
    #print(df2, "\n---\n", df3)


    

#for root, dirs, files in os.walk(input_path):
#    for file in files:
#        if file.endswith(".position"):
#            run(root, file)
            
files = [f for f in os.listdir(input_path) if re.match(r'.*\.distance', f)]
            
for file in files:
    run(file)            
