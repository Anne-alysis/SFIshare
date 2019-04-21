f=open("../data/proGames.json","r")
fout=open("../data/proGames_mod.json","w")

#f=open("../data/test.json","r")
#fout=open("../data/test_mod.json","w")

fout.write("[")
#i=0
for line in f:
   # ++i
    #print i,line

    if '_id' in line:
        print line
        line=line.replace('_id','id')
        line=line.replace('$oid','oid')
    line=line.strip()
    fout.write(line+","+"\n")
    
    
    
fout.write("]")


f.close()
fout.close()