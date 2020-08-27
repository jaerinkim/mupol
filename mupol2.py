# Summary: It generates a network of track recommendations based on Spotify API
#
# Requirements:
# 1. A Spotify authentication key
# 2. The list of tracks/albums/artists to start recursive recommendations
# (e.g. Billboard Top 100, Tracks in your favorite album, "Livin' on a Prayer")
# 3. A free computer (there are countless tracks out there and the API puts limits on your requests!)
#
# Returns: A dictionary as a network of track recommendations.
# Each track also contains musical information (like tempo) archived by Spotify.
# (e.g. If dict is the name of the dictionary, dict[x] returns up to 100 recommended tracks,
# given that you are currently listening to the music called "x" on Spotify.)
#
# Author: Jaerin Kim
#
# Run with Python 2.7.x

import sys
from spotipy.oauth2 import SpotifyClientCredentials as spcd
import spotipy
import spotipy.util as util
import imp
import re
import time
import cPickle
import os
import glob
# Load Spotify authentication key (Change the path and filename as appropriate,
# allowing spk.spid and spk.sppw as ID and PW).
spk=imp.load_source('spkey','local/spotify.py')

sp=spotipy.Spotify(client_credentials_manager=spcd(client_id=spk.spid,client_secret=spk.sppw))


# Generating generalized functions to expand the data exponentially.
# findNewTracks returns new unique tracks in newlist
# that are not in olddict.
def findNewTracks(olddict,newlist):
    uniquelist=[]
    oldlist=olddict['rec'].keys()
    for i in sorted(newlist):
        if i not in oldlist:
            uniquelist.append(i)
    return(uniquelist)

# findRec requests Spotify for recommended tracks for each track in tracklist.
# Put in unique list from findNewTracks
# The argument 'cut' adjusts how many requests will be sent to Spotify at a time.
# The higher the value is, the fewer requests are sent at a time.
# Spotify API will refuse the request at some point if 'cut' is set too low!
# (recommended: set cut such that bundle<100)
def findRec(tracklist,cut):
    log={}
    bckcount=0
    success=0
    bundle=len(tracklist)/cut+1
    for j in range(bundle):
        output={}
        for i in tracklist[j*cut:(j+1)*cut]:# For each subset of tracklist per cut.
            passflag=False
            errorcount=0
            while passflag==False:
                if errorcount<10:
                    try:# Try getting recommended tracks until Spotify gives an error message.
                        output[i]={'rec':sp.recommendations(seed_tracks=[i],limit=100)['tracks']}
                        passflag=True
                        print "Success: "+str(success)
                        success+=1
                    except Exception as e:
                        print 'trial',i
                        print e
                        log[i]=e# Store the error message.
                        errorcount+=1
                else:# Too many errors!
                    passflag=True
            bckcount+=1
            if bckcount%10==0:# Print how many jobs are done at which time
                #The process takes a lot of time so you'll want to monitor the process with this line.
                print "Sequence"+str(len(output))+" time:"+time.ctime()
# Replace with the following lines if the system becomes unstable while running this function.
# It guarantees that you lose less than 200 trials (or even less if you set the threshold
# less than 199).
#            if bckcount>199:
#                cPickle.dump(output,open("backup.p","wb"))
#                os.system('rm store.p')
#                os.system('mv backup.p store.p')
#                bckcount=0
        output['log']=log
        cPickle.dump(output,open("dict"+time.ctime()+".p","wb"))# Save the current output
        del output# Delete the output to free up RAM.
        if j==bundle-1:
            print "Done"

# extractNewList extracts track IDs from the recommended tracklist.
def extractNewList(recDict):
    output=[]
    copy=dict(recDict)
    if 'log' in copy:
        del copy['log']
    reckeys=copy.keys()
    for i in reckeys:
        for j in copy[i]['rec']:
            output.append(j['id'])
    return(output)

# Output from findRec should contain tracks known to the dictionary
# and unknown ones. mergeDict merges two dictionaries and returns a dictionary
# with unique entries.
def mergeDict(oldDict,recDict):
    copy=dict(recDict)
    if 'log' in copy:
        del copy['log']
    output=dict(oldDict)
    if 'log' in output:
        del output['log']
    for i in copy.keys():
        if i not in output:
            output[i]=copy[i]
    return(output)

# The recommendation process might halt due to unexpected circumstances.
# If stopped during process, bring up the same track list and the backed up
# dictionary as the arguments of continueRec.
# It will return which tracks are yet to be processed.

def continueRec(oldList,backupDict):
    copy=oldList[:]
    doneList=backupDict.keys()
    todoList=[]
    for i in oldList:
        if i not in doneList:
            todoList.append(i)
    return todoList

# Use seekRec to expand existing dictionary (storeDict) with
# newly added recommendations (bootDict).
# depth defines how "deep" the search should be,
# i.e. if depth=3, it searches until it gets recommended tracks for recommended tracks for the bootdict.
# If depth is set too high, the process will take forever!

def seekRec(storeDict,bootDict,depth):
    olddict=dict(storeDict)
    output={}
    slist=findNewTracks(olddict,extractNewList(bootDict))
    for i in range(depth):# Get recommendations for tracks at depth level i.
        recdic=findRec(slist)
        slist=findNewTracks(olddict,extractNewList(recdic))
        olddict=mergeDict(olddict,recdic)
        output['L'+str(i)]=olddict# Results from depth level i, ranging from 'level 0' to 'level depth-1'.
        output['log'+str(i)]=recdic
    return(output)


# In case the process is lost, run yourdictname=extractFromStore().
# It will use stored backup files to load them onto yourdictname.
def extractFromStore():
    netdict={}
    attrdict={}
    todo=glob.glob('store*')
    for i in todo:
        inp=cPickle.load(open(i,"rb"))
        for j in inp.keys():
            netdict[j]=[k['id'] for k in inp[j]['rec']]
            for l in inp[j]['rec']:
                attrdict[l['id']]={'name':l['name'], 'popularity': l['popularity'], 'album':l['album']['name'], 'albumid':l['album']['id'], 'artist':l['artists'][0]['name'], 'artistid':l['artists'][0]['id']}
        del inp
    return [netdict,attrdict]

# Use transform() to store newly acquired data to storex.p
def transform():
    old=glob.glob('store*')# The list of stored files.
    if len(old)==0:# If there is no store* file, write store0.p, store1.p, and so on.
        start=0
    else:# If there is some storex.p files, start writing from storex+1.p for the highest x.
        start=int(max([i[5:][:-2] for i in old]))+1
    new=glob.glob('dict*')
    for i in new:
        os.system("mv '"+i+"' store"+str(start)+".p")# Assumes that you are using *NIX!
        # For Windows OS, use move instead of mv.
        start+=1

## Use this function to get 'bootDict' with artist IDs, just before running 'seekRec'.
## output['tracks'] should be used as an argument for 'seekRec'.
## output['artists'] should be saved for later for artists-tracks network.

def artToTrackDict(artlist):
    artdict={}
    output={}
    for i in artlist:
        artdict[i]={sp.recommendations(seed_artists=i,limit=100)['tracks']}
    for i in artdict:
        rlist=[]
        for j in artdict[i]['rec']:
            rlist.append(artdict[i]['rec'][j]['id'])
            output[i]=rlist
    return({'tracks': output,'artists': artdict)


## Extract relations from pickles. Returns a list of lists; Each list represents an edge.
## Convenient for making networks but leaves out musical information
## (e.g. tempo, valence, ...)
## Use glob-based file names to analyze (e.g. store*). If csv==True, saves a csv file.

def pickleToNet(globexp,csv):
    output=[]
    errors=[]
    todo=glob.glob(globexp)
    if(csv):
        plist=open("plist.csv","w")
        pwriter=csv.writer(plist)
    for i in todo:
        inp=cPickle.load(open(i,"rb"))
        for j in inp.keys():
           try:
               output.append([[j,k['id']] for k in inp[j]['rec']])
           if(csv):
               pwriter.writerow([[j,k['id']] for k in inp[j]['rec']])
           except Exception as e:
               errors.append([j,e])
        del inp
    return({output,errors})
