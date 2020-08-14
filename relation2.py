## Pickle to csv translator for essential information
## Run in the directory containing netstore*.p pickle files.
## Move resulting csv files to somewhere safe (e.g. 'local' with gitignore)
##
## Run with Python 2.7.x
## Author: Jaerin Kim


import cPickle
import csv
import glob

# Adding music network to nrel.csv
nlist=glob.glob("netstore*")

with open('nrel.csv','a') as nl:
    nw=csv.writer(nl,delimiter=',')
    done=[]
    for i in nlist:
        print "Opening "+i
        current=cPickle.load(open(i,'rb'))
        for j in current.keys():
            for k in current[j]:
                nw.writerow([j,k])
        print "Done"
        done.append(i)

cPickle.dump(done,open('done.p','a'))

from spotipy.oauth2 import SpotifyClientCredentials as spcd
import spotipy
import spotipy.util as util
import imp
import csv
mpdict={}
with open("mupol.csv","rb") as mupol:
    mplist=csv.reader(mupol,delimiter=',')
    for i in mplist:
        if str(i[6]) in mpdict.keys():
            mpdict[str(i[6])][0].append(i[3])
        else:
            mpdict[i[6]]=[[i[3]],i[7],i[2]]

del mpdict['id']

ump=[]
for i in mpdict:
    for j in mpdict[i][0]:
        ump.append(j)
ump=sorted(set(ump))

umpdict={}
for i in ump:
    try:
        current=sp.search(q=i, type='artist')['artists']['items'][0]
        umpdict[i]={'id':current['id'],'genres':current['genres'],'followers':current['followers']['total'],'popularity':current['popularity']}
    except Exception as e:
        print i+" for "+str(e)
# Add supporters to each artist
for i in umpdict:
    umpdict[i]['likedBy']=[]
    for j in mpdict:
        if i in mpdict[j][0]:
            umpdict[i]['likedBy'].append(j)

for i in umpdict:
    current=sp.recommendations(seed_artists=[umpdict[i]['id']],limit=100)['tracks']
    umpdict[i]['rec']=[]
    for j in current:
        umpdict[i]['rec'].append(j['id'])

notdone=[]
with open('nrel.csv','a') as nl:
    nw=csv.writer(nl,delimiter=',')
    for i in umpdict:
        print i
        if len(umpdict[i]['rec'])==0:
            notdone.append(i)
        for j in umpdict[i]['rec']:
            nw.writerow(["ART"+umpdict[i]['id'],j])

notdonedict={}
notnotdone=[]
with open('nrel.csv','a') as nl:
    nw=csv.writer(nl,delimiter=",")
    for i in notdone:
        ttr=sp.artist_top_tracks(umpdict[i]['id'])
        print len(ttr['tracks'])
        ttrid=[]
        for j in ttr['tracks']:
            ttrid.append(j['id'])
        try:
            rec=sp.recommendations(seed_tracks=ttrid, limit=100)['tracks']
            notdonedict[i]=[]
            for k in rec:
                notdonedict[i].append(k['id'])
        except Exception as e:
            print i
            print e
            notnotdone.append(i)

with open('nrel.csv','a') as nl:
    nw=csv.writer(nl,delimiter=",")
    for i in notdonedict:
        umpdict[i]['rec']=notdonedict[i]
        for j in notdonedict[i]:
            nw.writerow(["ART"+umpdict[i]['id'],j])

with open('ptrump.csv','wb') as wr:
    wt=csv.writer(wr,delimiter=",")
    for i in umpdict:
        for j in umpdict[i]['likedBy']:
            if int(j)<=20:
                wt.writerow([umpdict[i]['id']]+[i])


with open('pcruz.csv','wb') as wr:
    wt=csv.writer(wr,delimiter=",")
    for i in umpdict:
        for j in umpdict[i]['likedBy']:
            if int(j)>60:
                wt.writerow([umpdict[i]['id']]+[i])

with open('psanders.csv','wb') as wr:
    wt=csv.writer(wr,delimiter=",")
    for i in umpdict:
        for j in umpdict[i]['likedBy']:
            if int(j)>20 and int(j)<=40:
                wt.writerow([umpdict[i]['id']]+[i])

with open('pclinton.allcsv','wb') as wr:
    wt=csv.writer(wr,delimiter=",")
    for i in umpdict:
        for j in umpdict[i]['likedBy']:
            if int(j)>40 and int(j)<=60:
                wt.writerow([umpdict[i]['id']]+[i])
for i in umpdict.keys():
    if umpdict[i]['rec']==[]:
        del umpdict[i]

with open("bslist.csv","rb") as tc:
    tcl=csv.reader(tc)
    bslist=[]
    for i in tcl:
        bslist.append(i)

def unlist(lt):
    output=[]
    for i in lt:
        if "ART" not in i[0]:
            if "x" not in i[0]:
                output.append(i[0])
    return(output)

temp=list(bslist)
counter=0
for i in temp:
    if "ART" in i:
        bslist.remove(i)

## Refer to these functions for getting track statistics

def getval(lt):
    itr=len(lt)%50
    output=[]
    for i in range(itr):
        try:
            val=sp.audio_features(lt[i*50:i*50+50])
            for j in val:
                output.append(j['valence'])
        except Exception as e:
            print e
    return(output)



def gettem(lt):
    itr=len(lt)%50
    output=[]
    for i in range(itr):
        try:
            val=sp.audio_features(lt[i*50:i*50+50])
            for j in val:
                output.append(j['tempo'])
        except Exception as e:
            print e
    return(output)
