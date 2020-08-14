## Initializes the search with Billboard Top 200 list.
## Customize to start with other lists (tracks/albums/artists).
##
## Author: Jaerin Kim
## Run with Python 2.7.x

import urllib2
from bs4 import BeautifulSoup
import sys
from spotipy.oauth2 import SpotifyClientCredentials as spcd
import spotipy
import spotipy.util as util

# Load Billboard Top 200 list in an html format and extract song titles
# and corresponding artists.
# You can choose to use other tracks than Billboard Top 200 list.
# But it is recommended to use this list to detect important nodes as soon as possible.

billboard2=BeautifulSoup(open('billboard2.html'))

title=billboard2.find_all("span","chart-list-item__title-text")

artists=billboard2.find_all("div","chart-list-item__artist")

# There are anomalies in the format of the entries.
# The following lines standardize the entries to help the analysis

def billboardalist(artsoup):
    output=[]
    for i in artsoup:
        try:
            output.append(i.contents[1].contents[0][1:-1])
        except IndexError:
            output.append(i.contents[0][1:-1])
    return output

artistsfix=billboardalist(artists)

artistsfix.insert(0,'Adele')

albums=[artistsfix,[i.contents[0][1:-1] for i in title]]

albums[1].insert(0,'21')

# To communicate with the Spotify API, we need to match artist names
# with Artist IDs used in Spotify. Store these IDs to spalbums[0].
spalbums=[[],[]]
for i,value in enumerate(albums[0]):
    sresult=sp.search(q=value,type='artist',limit=1)
    try:
        aID=sresult['artists']['items'][0]['id']
        spalbums[0].append(aID)
    except IndexError:
        spalbums[0].append(i)

# Paul McCartney needed a special attention. Manually adding him.
paulmc=sp.search(q='paul mccartney, wings',type='artist')
spalbums[0][119]=paulmc['artists']['items'][0]['id']

# Getting recommendations from Spotify based on the current artist list.
artrec=[]
for i,value in enumerate(spalbums[0]):
    passflag=False
    while passflag==False:
        try:
            artrec.append(sp.recommendations(seed_artists=[value],limit=100))
            passflag=True
        except Exception as e:
            print 'trial',i
            print e

# Now we got recommended tracks for Billboard "Greatest of All Time" albums.
# No errors could be found.

recommendedtracks=[]
for i in artrec:
    for j in i['tracks']:
        recommendedtracks.append(j['id'])

len(recommendedtracks)
len(set(recommendedtracks))

# There are 9682 unique track IDs, but the whole recommended tracks are 19900.
# On average, the same track is recommended approximately twice (9682/19900).
# Now we are at the starting point with these popular tracks.
# I start the complete dictionary (completedict) with the initial tracks.
# A track as an entry of the dictionary has up to 100 recommended tracks from Spotify.
# completedict['track'] returns which tracks you get recommended for a 'track'.

completedict={}
for i in artrec:
    for j in i['tracks']:
        completedict[j['id']]={'rec':i['tracks']}

# To start the process with the Billboard Top 200 chart with depth level 2, run the following line
# (caution: expect it to run for a while + it will consume a significant portion of your resources!).

seekRec({},completedict,2)
