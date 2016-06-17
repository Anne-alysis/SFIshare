
import os,re
dirrawdata="../data/raw/full/TI5/PlayoffsDay7/"

files = [f for f in os.listdir(dirrawdata) if re.match(r'.*\.results', f)]


for filename in files:
    f=open(dirrawdata+filename,"r")
    fout=open(dirrawdata+filename+"_clean","w")
    match_id=filename.split(".")[0]
    #f=open("../data/test.txt","r")
    #fout=open("../data/test_mod.txt","w")

    #i=0
    for line in f:
        #i=i+1
        #print i,line
        if line.startswith("{"):
            line=line.replace('{','{"match_id":'+match_id+",")
            fout.write(line)



    f.close()
    fout.close()