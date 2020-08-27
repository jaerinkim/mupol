## This file visualizes the network of music recommendations,
## generated with mupol.py and cleaned with relation.py.
## nrelwoartist.csv and nrel.csv only contain the recommendation network.
## For other interesting variables (e.g. genres, tempo, popularity of tracks), access the pickle files (store*.p).
##
## Author: Jaerin Kim

library(igraph)
par(family="Liberation Sans")
nrel <- read.csv("local/netlist/nrelwoartist.csv",header=FALSE)
nrelart<-read.csv("local/netlist/nrelart.csv",header=FALSE)
nrelsup<-read.csv("local/netlist/supp.csv",header=FALSE)
nrelfull<-rbind(nrel,nrelsup)
nrelfull<-rbind(nrelfull,nrelart)

## Uncomment to analyze the artist network (artnet)
## artnet<-graph_from_data_frame(nrelart,directed=FALSE)
musicnet<-graph_from_data_frame(nrelfull,directed=FALSE)

## Loading the favorite artists among supporters of politicians.
trump<-read.csv("local/ptrump.csv",header=FALSE)
sanders<-read.csv("local/psanders.csv",header=FALSE)
clinton<-read.csv("local/pclinton.csv",header=FALSE)
cruz<-read.csv("local/pcruz.csv",header=FALSE)
candidates=c("trump","sanders","clinton","cruz")

## Generating the shortest paths connecting each person
for (i in candidates){
  ## Trump[,2], etc is artist names in English. These are useless in the analysis.
  ## Use Trump[,1] and distinguish artist IDs from track IDs as follows.
  artists<-eval(parse(text=i))[,1]
  assign(paste0(i,"art"),
         paste0("ART",artists))
  assign(paste0(i,"art"),# Remove missing data
         subset(eval(parse(text=paste0(i,"art"))),
                eval(parse(text=paste0(i,"art")))%in%nrelart[,1]))
  assign(paste0(i,"path"),# Find shortest paths between vertices
         shortest_paths(musicnet,eval(parse(text=paste0(i,"art"))),
                        eval(parse(text=paste0(i,"art")))))
  assign(paste0(i,"list"),
         names(unlist(eval(parse(text=paste0(i,"path")))[[1]])))
}
minmax<-unique(c(trumplist,clintonlist,sanderslist,cruzlist))
mmpaths<-shortest_paths(musicnet,minmax,minmax)
## Remove self-loops
mmgraph<-subgraph(musicnet,unlist(mmpaths$vpath))
mmgraph<-delete.vertices(simplify(mmgraph), degree(simplify(mmgraph))==0)

## Adding people to the graph
mmart=c()
for(i in minmax){
  if (grepl("ART*",i))mmart<-c(mmart,i)
}
mmartv=c()
for(i in paste0(candidates,"art")){
  plist=eval(parse(text=i))# A vector of people supporting a candidate.
  for (j in 1:length(plist)){
    mmartv<-c(mmartv,
              paste0(i,j),# A person's ID, e.g. trumpart14
              plist[j])# The person's favorite artist
  }
}
vnames=vertex_attr(mmgraph)$name
mmat=matrix(mmartv,nrow=2)
mmat<-mmat[,mmat[2,]%in%vnames]
mmgraph<-add_vertices(mmgraph,ncol(mmat),name=mmat[1,])
mmgraph<-add_edges(mmgraph,as.vector(mmat))
vnames=vertex_attr(mmgraph)$name

## Run the label propagation algorithm
mmlabel=rep(-1,ncol(mmat))#unclassified vertices have label<0 
mmlabel[grepl("trump*",mmat[1,])]<-1
mmlabel[grepl("sanders*",mmat[1,])]<-2
mmlabel[grepl("clinton*",mmat[1,])]<-3
mmlabel[grepl("cruz*",mmat[1,])]<-4
mmmodel<-cluster_label_prop(mmgraph,
                            initial=c(rep(-1,length(vnames)-length(mmlabel)),mmlabel),
                            fixed=c(rep(F,length(vnames)-length(mmlabel)),rep(T,length(mmlabel))))
mmmodels=list()
npeople=as.vector(table(mmlabel))
excsample=matrix(nrow=10000,ncol=round(.1*npeople[1])+round(.1*npeople[2])+round(.1*npeople[3])+round(.1*npeople[4]))
initial=c(rep(-1,length(vnames)-length(mmlabel)),mmlabel)
fixed=c(rep(F,length(vnames)-length(mmlabel)),rep(T,length(mmlabel)))
for (i in 1:10000){
  nexclude=c(sample(1:npeople[1],round(.1*npeople[1])),
             sample((npeople[1]+1):(npeople[1]+npeople[2]),round(.1*npeople[2])),
             sample((npeople[1]+npeople[2]+1):(sum(npeople)-npeople[3]),round(.1*npeople[3])),
             sample((sum(npeople)-npeople[4]+1):sum(npeople),round(.1*npeople[4]))
          )##Sample 90% of observations for each trial
  tempgraph=induced_subgraph(mmgraph,(1:length(vnames))[!1:length(vnames)%in%nexclude])
  tempinitial=initial[!1:length(vnames)%in%nexclude]
  tempfixed=fixed[!1:length(vnames)%in%nexclude]
  excsample[i,]<-nexclude
  mmmodels[[i]]=cluster_label_prop(tempgraph,initial=tempinitial,fixed=tempfixed)
}

## Example plot
mmodel=mmmodels[[10000]]
modelgraph=induced_subgraph(mmgraph,(1:length(vnames))[!1:length(vnames)%in%nexclude])
mmcol=mmodel$membership
cols=c("violet","#80E64D","#E6DB33","cyan")
mmcol[mmcol==1]<-cols[1]
mmcol[mmcol==2]<-cols[2]
mmcol[mmcol==3]<-cols[3]
mmcol[mmcol==4]<-cols[4]
plot(modelgraph,vertex.label=NA,vertex.size=1.5,vertex.col=mmcol,vertex.frame.color=mmcol,
     layout=layout.mds,edge.width=.1)
legend("topleft", legend=c("Trump", "Sanders","Clinton","Cruz"),
       fill=cols, cex=.8, box.lty=0)
trumploc<-sandersloc<-clintonloc<-cruzloc<-NULL
for (i in candidates){
  loc<-grepl(i,names(V(mmgraph)))
  path<-shortest_paths(mmgraph,loc,loc)
  graph<-subgraph(mmgraph,unique(unlist(path[[1]])))
  assign(paste0(i,"loc"),loc)
  assign(paste0(i,"path"),path)
  assign(paste0(i,"graph"),graph)
}

for (i in candidates){
  loc<-mmodel$membership==which(i==candidates)
  path<-shortest_paths(modelgraph,loc,loc)
  graph<-subgraph(modelgraph,unique(unlist(path[[1]])))
  assign(paste0(i,"locm"),loc)
  assign(paste0(i,"pathm"),path)
  assign(paste0(i,"graphm"),graph)
}
## Trump: What hurts the most
## Smoke a little smoke
which.max(eigen_centrality(trumpgraph)$vector)
which.max(eigen_centrality(trumpgraphm)$vector)
## "Sanders" cluster became Clinton cluster after learning!
## This implies some similarity between the two groups.
## Clinton: Wild world
## Baby one more time
which.max(eigen_centrality(sandersgraph)$vector)
which.max(eigen_centrality(sandersgraphm)$vector)
## Sanders: Take the money and run
## Bless the broken road
which.max(eigen_centrality(clintongraph)$vector)
which.max(eigen_centrality(clintongraphm)$vector)
## Cruz
which.max(eigen_centrality(cruzgraph)$vector)
which.max(eigen_centrality(cruzgraphm)$vector)



## Generating simpler plots

trumpnet=shortest_paths(musicnet,trump[,1],trump[,1])
sandersnet=shortest_paths(musicnet,sanders[,1],sanders[,1])
clintonnet=shortest_paths(musicnet,clinton[,1],clinton[,1])
cruznet=shortest_paths(musicnet,cruz[,1],cruz[,1])
polart<-c(trumpart,clintonart,sandersart,cruzart)
missing<-polart[!polart%in%nrelart[,1]]
polart<-polart[!polart%in%missing]
allnet=shortest_paths(musicnet,polart,polart)
seek<-c()
counter=1
for (i in allnet$vpath){
  if (length(i)==0)seek<-c(seek,counter)
  counter=counter+1
}

## The following codes extract a subset of the full network,
## that connects the six example artists with the minimum number of vertices.
## These artists have human names, of course.
## [Celine Dion, Don Williams, Keith Urban, Rick Pino, Jeniffer Lopez, and Ricky Martin]
example=c('ART4S9EykWXhStSc15wEx8QFK','ART4Ti0EKl2PVEms2NRMVGqNe','ART0u2FHSq3ln94y5Q57xazwf','ART1qTF7MRkOV8LNFgxejBPD5','ART2DlGxzQSjYe5N6G9nkYghR','ART7slfeZO9LsJbWgpkIoXBUJ')

cd=shortest_paths(musicnet,example[1],example)
dw=shortest_paths(musicnet,example[2],example)
ku=shortest_paths(musicnet,example[3],example)
rp=shortest_paths(musicnet,example[4],example)
jl=shortest_paths(musicnet,example[5],example)
RM=shortest_paths(musicnet,example[6],example)
vvec=c(names(unlist(cd[[1]])))
vvec=c(vvec,names(unlist(dw[[1]])))
vvec=c(vvec,names(unlist(ku[[1]])))
vvec=c(vvec,names(unlist(rp[[1]])))
vvec=c(vvec,names(unlist(jl[[1]])))
vvec=c(vvec,names(unlist(RM[[1]])))
exgraph=subgraph(musicnet,vvec)
simpleex=simplify(exgraph)
## In ****book, some users like these artists. Some of them also are opinionated in politics.
## TSA is a ****book user who supports a presidential candidate whose last name starts with T.
## Add this person as a vertex into the network.
simpleex1=simpleex+graph(matrix(c('TSA','ART4S9EykWXhStSc15wEx8QFK','TSA','ART4Ti0EKl2PVEms2NRMVGqNe'),ncol=2,byrow=F),directed = FALSE)
## NO is a new observation. She also gets in.
simpleex2=simpleex1+graph(matrix(c('NO','ART0u2FHSq3ln94y5Q57xazwf','NO','ART1qTF7MRkOV8LNFgxejBPD5'),ncol=2,byrow=F),directed=FALSE)
## CS supports a presidential candidate whose last name starts with C.
simpleex3=simpleex2+graph(matrix(c('CS','ART2DlGxzQSjYe5N6G9nkYghR','CS','ART7slfeZO9LsJbWgpkIoXBUJ'),ncol=2,byrow=F),directed=FALSE)
simpleex4=simplify(simpleex3)

## Get eigenvector centrality to visualize how "important" each vertex is.
eigensize=eigen_centrality(simpleex4)$vector
## Magnify vertices under focus (e.g. eigensize[63] is for Keith Urban).
eigensize[78:80]<-c(1,1,1)
eigensize[63:68]<-eigensize[63:68]*.66
## Add some graphics
shape=c(rep('circle',62),rep('square',6),rep('circle',9),rep('rectangle',3))
color=c(rep(rgb(.8,.8,.9),62),rep(rgb(.5,.9,.3),6),rep(rgb(.8,.8,.9),9),rep(rgb(.9,.86,.2),3))
## Plot it! (Note: the shape of the network is random but the represented network remains the same.)
pdf("lowtrump.pdf")
plot(simpleex4,vertex.label=c(rep(NA,62),"Keith Urban","Don Williams","Ricky Martin","Jennifer Lopez","Celine Dion","Rick Pino",rep(NA,9),"New Observation","Trump Supporter","Clinton Supporter"),vertex.shape=shape,
     vertex.label.dist=1.2,vertex.label.cex=.5,vertex.label.color="black",vertex.color=color, vertex.size=eigensize^.3*8)
dev.off()

## But lowtrump.pdf is still too big. For more intuitive visualization,
## I focus on the shortest distance between the three ****book users.
tinyex<-shortest_paths(simpleex4,"TSA",c("NO","CS"))
tinylist<-names(unlist(tinyex[[1]]))
tinygraph<-subgraph(musicnet,unique(c(tinylist[2:5],tinylist[8:10])))
simpletiny<-simplify(tinygraph)
tinyg<-tinygraph+graph(matrix(c("TSA",'ART4S9EykWXhStSc15wEx8QFK',
                                "NO",'ART0u2FHSq3ln94y5Q57xazwf',
                                "CS",'ART2DlGxzQSjYe5N6G9nkYghR'),
                              ncol=3),directed=FALSE)
simpletinyg<-simplify(tinyg)
tinylabel<-c("But For the Grace of God - Keith Urban","Up! - Shania Twain","Battlefield - Jordin Sparks","Keith Urban","Jennifer Lopez","Celine Dion","New Observation","Trump Supporter","Clinton Supporter")
tinyshape<-c(rep("circle",3),rep("square",3),rep("vrectangle",3))
tinycolor<-c(rep(rgb(.8,.8,.9),3),rep(rgb(.5,.9,.3),3),rep(rgb(.9,.86,.2),3))
## Plot it!
pdf("tinytmp.pdf")
par(mar=c(1,1,1,1))
plot(simpletinyg,vertex.label=tinylabel,vertex.shape=tinyshape,
     vertex.label.dist=2.5,vertex.label.cex=.8
     ,vertex.label.color="black",
     vertex.color=tinycolor, vertex.size=12,layout=layout.auto(simpletinyg))
dev.off()

## Shortest paths for every observable voters
allartists=c(trump[,1],sanders[,1],clinton[,1],cruz[,1])
allartists=allartists[allartists%in%nrelart[,1]]
allartists=unique(allartists)
allshortpaths<-shortest_paths(musicnet,from=allartists,to=allartists)
fullshortpaths<-shortest.paths(musicnet,v=allartists,to=allartists)
vvec=c(names(unlist(allshortpaths[[1]])))
allsubgraph=subgraph(musicnet,vvec)

plot(allsubgraph,vertex.label="",vertex.size=1)

## Testing data loss
length(grep("ART*",unique(vvec)))
length(grep("ART*",unique(names(V(allsubgraph)))))

## Write the graph to transfer to Python
write_graph(allsubgraph,"local/netlist/minmaxnetwork.csv","edgelist")

setwd("local/print")
par(mar=c(0,0,0,0))
pcolors=c(rgb(.8,.8,.9),rgb(.5,.9,.3),rgb(.8,.8,.9),rgb(.9,.86,.2))
## Print ttok1-3.jpeg
plot(c(),xlim=c(-1,1),ylim=c(-1,1),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",asp=1)
rect(-0.8,-0.2,-0.6,.2,col=rgb(.9,.86,.2))
text(-.7,-.45,"Trump Supporter")
rect(0.9,-0.15,0.6,.15,col=pcolors[2])
text(.75,-.45,"Keith Urban")
lines(c(-.6,.6),c(0,0))
text(0,.2,"👍",cex=3)


library(jpeg)
plot(c(),xlim=c(-1.5,3.5),ylim=c(-1,1),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",asp=1)
rect(-0.8,-0.2,-0.6,.2,col=rgb(.9,.86,.2))
text(-.7,-.45,"Trump Supporter")
lines(c(-.6,.6),c(0,0))
rect(0.9,-0.15,0.6,.15,col=pcolors[2])
text(.7,-.45,"Keith Urban")
lines(c(.9,2.1),c(0,0))
text(0,.3,"👍",cex=3)
library(plotrix)
draw.circle(2.3,0,.2,col=pcolors[3])
text(2.3,-.45,"But for the Grace of God")
spotify<-readJPEG("local/print/spotify.jpeg",T)
rasterImage(spotify,1.3,.1,1.7,.5)


plot(c(),xlim=c(-1,1),ylim=c(-1,1),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",asp=1)
text(0,0,"?",cex=10)
