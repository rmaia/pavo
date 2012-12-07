require(pavo)
eliz <- getspec('~/Documents/Projects/Starlings - RawData/Starling_irid/Lamprotornis_elisabeth', ext='ttt')
eliz <- aggspec(eliz, by=3)
chlo <- getspec('~/Documents/Projects/Starlings - RawData/Starling_irid/Lamprotornis_chloropterus', ext='ttt') 
chlo <- aggspec(chlo, 3)
acut <- getspec('~/Documents/Projects/Starlings - RawData/Starling_irid/Lamprotornis_acuticaudus', ext='ttt') 
acut <- aggspec(acut, 3)

bodypart<-c('throat','chest','belly','head','neck','mantle','rump','shoulder','wing','tail','cheek')
elizsex <- c('FEM','FEM','FEM','MAL','MAL','MAL','FEM','MAL')
acutsex<-c('MAL','MAL','MAL','FEM','MAL','FEM','FEM','MAL','MAL','FEM','MAL','FEM','FEM','MAL','FEM','MAL','MAL','FEM','FEM','FEM')
chlorsex<- c('MAL','MAL','MAL','MAL','FEM','FEM','MAL','FEM','MAL','FEM','FEM','FEM','MAL','MAL','FEM','FEM','FEM','MAL','FEM','MAL')

names(acut)[-1] <- paste(gsub('.....$','',names(acut))[-1],rep(acutsex,each=11),bodypart, sep='.')
names(eliz)[-1] <- paste(gsub('.....$','',names(eliz))[-1],rep(elizsex,each=11),bodypart, sep='.')
names(chlo)[-1] <- paste(gsub('.....$','',names(chlo))[-1],rep(chlorsex,each=11),bodypart, sep='.')









