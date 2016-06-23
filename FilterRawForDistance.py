import os,re,sys

#dirrawdata="../data/raw/full/TI5/PlayoffsDay7/"

dirrawdata = sys.argv[1]

files = [f for f in os.listdir(dirrawdata) if re.match(r'.*\.results$', f)]


for filename in files:
    f=open(dirrawdata+filename,"r")
    fout=open(dirrawdata+filename+".distance","w")


    for line in f:
        if '"x":' in line:
            fout.write(line)

    f.close()
    fout.close()