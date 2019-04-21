import json
import sys
import os
import math,re

# to run: python RawToJSON.py input_Rjson input_path
# input_Rjson==R: add in brackets and commas in order to have a true json file to read into R,
# if ==M (or anything else), maintain format for injection into mongo

# removes java junk at beginning of each results file and adds match_id from file name to each event


input_path = sys.argv[1]
input_Rjson = sys.argv[2]


files = [f for f in os.listdir(input_path) if re.match(r'.*\.results$', f)]


for file in files:
    if input_Rjson=="R":
        fout=open(input_path+file+".json","w")
        fout.write("[")
    else:
        fout=open(input_path+file+"_clean","w")

    match_id=file.split(".")[0]

    with open(input_path+file) as f:
        lines=f.readlines()
        last=lines[-1]
        for line in lines[:-1]:
            if line.startswith("{"):
                line=line.replace('{','{"match_id":'+match_id+",").strip()
                if input_Rjson=="R":
                    comma=","
                else:
                    comma=""
                fout.write(line+comma+"\n")
    if last.startswith("{"):
        last=last.replace('{','{"match_id":'+match_id+",").strip()

    if input_Rjson=="R":
        fout.write(last.strip()+"]")
    else:
        fout.write(last.strip())

    fout.close()

