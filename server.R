####======================================= Basic Exploratory Textual Analysis in R ===================================================####

###========================== Code Map

##> Section 0: Preliminary Code
#Contains code that will be run upon the start of the application

##> Section 1: Importing Corpus
#Contains code to either import corpus from the user, or to select a custom corpus coupled with the application

##> Section 2: Preprocessing
#Contains code used for performing preprocessing functions on data

##> Section 3: Feature Generation, Weighting, Selection
#Contains code used for generating, weighting, and performing (very) basic feature selection techniques

##> Section 4: Initial analysis
#Contains code for generating graphs for initial analyses, namely a Rank-Frequency graph and a Word Frequency graph

##> Section 5: Clustering Words
#Contains code used for performing basic word clustering, both hierarchical and partitional

##> Section 6: Clustering Documents
#Contains code used for performing very basic Information retrieval

##> Section 7: Word Networks
#Contains code for generating graphs that look a network of words


#========================================================================================================================#
##============================ Section 1: Load libraries ==========================================================================##

libraries<- c("tm","fastcluster","Snowball","ggplot2","RWeka","reshape2","RCurl","igraph")
loadLib<- sapply(libraries, function(x) require(x,character.only=TRUE))
if(all(loadLib)!=TRUE){
unLoaded<- which(loadLib==FALSE)
install.packages(libraries[unLoaded],repos="http://cran.us.r-project.org")
sapply(libraries[unLoaded],function(x) require(x,character.only=TRUE))
}

#the following was initially added to reduce computation
#however, it was recognised that reactive conducters can be used instead
#myCorpus<- list()


#============================ Load stopwords/thesauri

stopwordsLink<- getURL("https://raw2.github.com/noobuseR/Datasets/master/myStopwords.txt",ssl.verifypeer=FALSE)
thesaurusLink<- getURL("https://raw2.github.com/noobuseR/Datasets/master/thesaurus.txt",ssl.verifypeer=FALSE)
customStopwords<- read.table(textConnection(stopwordsLink),sep=" ",header=FALSE,as.is=TRUE)
customThesaurus<- read.table(textConnection(thesaurusLink),sep=" ",header=TRUE,as.is=TRUE)
myStopwords<- customStopwords[,2]
myStopwords<- gsub("$"," ",myStopwords)
myStopwords<- gsub("^"," ",myStopwords)
initialWords<- customThesaurus[,"InitialWords"]
initialWords<- gsub("^"," ",initialWords)
initialWords<- gsub("$"," ",initialWords)
finalWords<- customThesaurus[,"FinalWords"]
finalWords<- gsub("^"," ",finalWords)
finalWords<- gsub("$"," ",finalWords)


#============================= Build required functions

#======= Pre-processing Corpus

cleanSweepCorpus<- function(corpus, stopwords=FALSE, stem=FALSE,removePunct=FALSE,removeNum=FALSE,synonyms=FALSE,customStopwords=FALSE){
newCorpus<- corpus
newCorpus<- tolower(newCorpus)
newCorpus<- gsub("[\\)(\"]"," ",newCorpus)
if(stopwords != FALSE){
	englishStopwords<- c(stopwords("SMART"),stopwords("english"))
	englishStopwords<- gsub("^","\\\\b",englishStopwords)
	englishStopwords<- gsub("$","\\\\b",englishStopwords)
	x<- mapply(FUN=function(...){
	newCorpus<<- gsub(...,replacement=" ",x=newCorpus)},
	pattern=englishStopwords)
	}
if (customStopwords != FALSE){
	customStopwords<- gsub("^","\\\\b",customStopwords)
	customStopwords<- gsub("$","\\\\b",customStopwords)
	x<- mapply(FUN=function(...){
	newCorpus<<- gsub(...,replacement=" ",x=newCorpus)},
	pattern=customStopwords)
	}
if(synonyms != FALSE & (length(initialWords)==length(finalWords))){
	initialWords<- gsub("^","\\\\b",initialWords)
	initialWords<- gsub("$","\\\\b",initialWords)
	x<- mapply(FUN=function(...){
	newCorpus<<- gsub(...,x=newCorpus)},
	pattern=initialWords, replacement=finalWords)
	}
if(removePunct!=FALSE){
	newCorpus<- gsub("[[:punct:]]"," ", x=newCorpus)
	}
if(removeNum!=FALSE){
	newCorpus<- gsub("[[:digit:]]"," ",x=newCorpus)
	}
newCorpus<- stripWhitespace(newCorpus)
newCorpus<- gsub("^\\s","",newCorpus)
newCorpus<- gsub("\\s$","",newCorpus)
finalCorpus<- Corpus(VectorSource(newCorpus))
if(stem!=FALSE){
	finalCorpus<- tm_map(finalCorpus,stemDocument)
	}
return(finalCorpus)
}




#======= X-axis spacing for phrase clouds

mySpacing<- function(frequency){
seq(-min(frequency)/mean(frequency),max(frequency)/mean(frequency),length.out=length(frequency))
}
###code borrowed from: http://bridgewater.wordpress.com/2012/04/18/a-word-cloud-where-the-x-and-y-axes-mean-something/



#======= My GitHub url for datasets

ghubURL<- "https://raw2.github.com/noobuseR/Datasets/master/"





#======================================================================================================================#
#============================= The Shiny application =================================================================##




shinyServer(function(input,output,session){


#========================================================================================================================#
##======================= Section 2: Importing Corpus ==========================================================================##


initialCorpus<- reactive({
			if(input$confirm==0)
			return()
			isolate({
				if(input$corpusType=="dirFile"){
					myPath<- input$filePath
					myCorpus<- Corpus(DirSource(myPath),readerControl=list(language="english",reader=readPlain))
					myCorpus
					}
				else if(input$corpusType=="singleFile"){
					myPath<- input$filePath
					myFile<- scan(file=myPath,what=character(0),n=-1, sep="\n")
					myCorpus<- Corpus(VectorSource(myFile),readerControl=list(language="english",reader=readPlain))
					myCorpus
					} 
				else if(input$corpusType=="sample"){
					myFile<- getURL(paste0(ghubURL,input$sampleCorpus,".txt"),ssl.verifypeer=FALSE)
					txtFile<- scan(textConnection(myFile),sep="\n",what="character")
					myCorpus<- Corpus(VectorSource(txtFile),readerControl=list(language="english",reader=readPlain))
					myCorpus
					}
				})
			})

output$corpusStatus<- renderPrint({
				if(input$confirm==0)
				return("No Corpus selection made yet")
				isolate({
					initialCorpus() })
			})

	



#========================================================================================================================#
##============================= Section 3: Pre-processing ==================================================================##





preprocessedCorpus<- reactive({
				if(input$startPreprocess==0)
				return()
				isolate({
					newCorpus<- cleanSweepCorpus(corpus=initialCorpus(),stopwords=input$stopwords,stem=input$stemming,removePunct=input$punctuation,removeNum=input$numbers,synonyms=input$customThes,customStopwords=input$customStop)
					newCorpus })
			})


output$procCorpusStatus<- renderPrint({
					if(input$startPreprocess==0)
					return("No pre-processing applied on Corpus")
					isolate({
						preprocessedCorpus () })
			})







#========================================================================================================================#
#=========================== Section 4: Feature Generation, Weighting, and Selection ========================================##





initialUnigramMatrix<- reactive({
				if(input$generateMatrix==0)
				return()
				isolate({
					weightingScheme<- paste0(input$termWeight,input$docWeight,input$normalisation)
					initialMatrix<- TermDocumentMatrix(preprocessedCorpus(),
								control=list(weighting=function(x) weightSMART(x,spec=weightingScheme)))
					initialMatrix }) })

lowerFreqRange<- reactive({
			myMatrix<- initialUnigramMatrix()
			freq<- rowSums(as.matrix(myMatrix))
			range(freq) })

output$lowerFreqSlider<- renderUI({
				sliderInput("lowerFreqBound","Please set the Lower Bound for Frequency",
				min=round(lowerFreqRange()[1]),max=round(lowerFreqRange()[2]),value=round(lowerFreqRange()[1]),step=NULL,ticks=TRUE) })

finalUnigramMatrix<- reactive({
				if(input$selectFeatures==0)
				return()
				isolate({
					finalUnigramMatrix<- initialUnigramMatrix()
					if(input$lowerFreqBound!=lowerFreqRange()[1]){
						lowerBound<- findFreqTerms(finalUnigramMatrix,round(input$lowerFreqBound),Inf)
						finalUnigramMatrix<- finalUnigramMatrix[lowerBound]}
					if(input$sparsity!=100){
						finalUnigramMatrix<- removeSparseTerms(finalUnigramMatrix,sparse=(input$sparsity/100))}
					finalUnigramMatrix }) })

lowerFreqRangeDendro<- reactive({
					myMatrix<- finalUnigramMatrix()
					freq<- rowSums(as.matrix(myMatrix))
					range(freq) })

output$lowerFreqSliderDendro<- renderUI({
					sliderInput("dendroSize","Set Word Frequency lower bound",
					min=round(lowerFreqRangeDendro()[1]),max=round(lowerFreqRangeDendro()[2]),value=round(lowerFreqRangeDendro()[1]),step=NULL,ticks=TRUE) })



initialDf<- reactive({
			if(input$selectFeatures==0)
			return()
			isolate({
				myMat<- as.matrix(finalUnigramMatrix())
				words<- rownames(myMat)
				freq<- rowSums(myMat)
				myDf<- data.frame(words=words,freq=freq,stringsAsFactors=FALSE) 
				myDf }) })

output$initialuniMatrix<- renderPrint({ 
				if(input$generateMatrix==0)
				return("No Term-Document matrix constituted of single words available at the moment")
				isolate ({ initialUnigramMatrix() })
			})

output$finaluniMatrix<- renderPrint({
				if(input$selectFeatures==0)
				return("No Feature Selection procedure applied at the moment")
				isolate ({ finalUnigramMatrix() })
			})
					



#========================================================================================================================#
##===================================  Section 5: Initial Analysis ===============================================================##



rankFrequencyPlot<- reactive({
				if(input$generateRankFreq==0)
				return()
				isolate({
					myDf<- initialDf()
					myDf<- myDf[order(myDf$freq,decreasing=TRUE),]
					myDf<- transform(myDf,rank=seq_along(myDf$freq)) 
					ggplot(data=myDf,aes(x=log10(rank),y=log10(freq))) + geom_text(aes(label=words,size=3,angle=45)) +
					xlab("Words") + ylab("Frequency in Log scale") + scale_size(guide="none") + ggtitle("Rank-Frequency Plot")
					})
			})

output$rankFreqPlot<- renderPlot({
				if(input$generateRankFreq==0)
				return()
				print(rankFrequencyPlot())
			})

output$downloadRankFreqPlot<- downloadHandler(
				filename=function(){paste0("RankFrequencyPlot",Sys.time(),".pdf")},
				content=function(file){
					pdf(file, width=14,height=12)
					print(rankFrequencyPlot())
					dev.off()
			})
				






wordFrequencyPlot<- reactive({
				if(input$generateWordFreq==0)
				return()
				isolate({
					myDf<- initialDf()
					myDf<- myDf[order(myDf$freq,decreasing=TRUE),]
					myDf$words<- factor(myDf$words,levels=myDf[order(myDf$freq,decreasing=TRUE),"words"])
					if(length(myDf$freq)<=100){
					myDfNew<- myDf[seq_len(round(length(myDf$freq)/2)),]
					}
					else if(length(myDf$freq)>100 && length(myDf$freq)<=500){
					myDfNew<- myDf[seq_len(round(length(myDf$freq)/10)),]
					}
					else if(length(myDf$freq)>500 && length(myDf$freq)<=1000){
					myDfNew<- myDf[seq_len(round(length(myDf$freq)/20)),]
					}
					else if(length(myDf$freq)>1000 && length(myDf$freq)<=1500){
					myDfNew<- myDf[seq_len(round(length(myDf$freq)/40)),]
					}
					else if(length(myDf$freq)>1500 && length(myDf$freq)<=2000){
					myDfNew<- myDf[seq_len(round(length(myDf$freq)/70)),]
					}
					else if(length(myDf$freq)>2000 && length(myDf$freq)<=2500){
					myDfNew<- myDf[seq_len(round(length(myDf$freq)/80)),]
					}
					else if(length(myDf$freq)>2500){
					myDfNew<- myDf[seq_len(round(length(myDf$freq)/100)),]
					}
					ggplot(data=myDfNew,aes(x=words,y=freq)) + geom_bar(stat="Identity") + xlab("Words") + ylab("Frequency") + 
					ggtitle("Most Frequent Words") + theme(axis.text.x=element_text(angle=90,hjust=1))
					})
			})

output$wordFreqPlot<- renderPlot({
				if(input$generateWordFreq==0)
				return()
				print(wordFrequencyPlot())
			})

output$downloadWordFreqPlot<- downloadHandler(
					filename=function(){paste0("WordFrequencyPlot",Sys.time(),".pdf")},
					content=function(file){
					pdf(file,width=14,height=12)
					print(wordFrequencyPlot())
					dev.off()
			})







#========================================================================================================================#
##===================================  Section 6: Clustering words ===============================================================##





dendroGraphic<- reactive({
			if(input$generateDendro==0)
			return()
			isolate({
				mainMat<- finalUnigramMatrix()
				if(input$dendroSize!=lowerFreqRangeDendro()[1]){
					targetWords<- findFreqTerms(mainMat,input$dendroSize,Inf)
					mainMat<- mainMat[targetWords] }
				distMat<- dist(scale(mainMat))
				dendro<- hclust(distMat,method="ward")
				plot(dendro)
				})
			})
 
output$dendrogram<- renderPlot({ 
				if(input$generateDendro==0)
				return()
				print(dendroGraphic())
			})

output$downloadDendro<- downloadHandler(
					filename=function(){paste0("Dendrogram",Sys.time(),".pdf")},
					content=function(file){
					pdf(file,width=14,height=12)
					print(dendroGraphic())
					dev.off()
			})

			



associativeCloud<- reactive({
				if(input$generateAssoc==0)
				return()
				isolate({
					myMat<- as.matrix(finalUnigramMatrix())
					myAdjMat<- myMat %*% t(myMat)
					freq<- rowSums(myMat)
			
					adjGraph<- graph.adjacency(myAdjMat,weighted=TRUE,diag=FALSE)
					if(input$assocPresentation=="kk"){
						coordinates<- layout.kamada.kawai(adjGraph) }
					else{
						coordinates<- layout.fruchterman.reingold(adjGraph,weights=E(adjGraph)$weight) }
					wordsDf<- data.frame(term=V(adjGraph)$name,x.pos=coordinates[,1],y.pos=coordinates[,2],affiliations=diag(myAdjMat),stringsAsFactors=FALSE)
					set.seed(1011)
					wordsKmeans<- kmeans(wordsDf[,c(2,3)],centers=input$groupAssoc)
			
					wordsDf<- transform(wordsDf,freq=freq, percentOfDoc=100*(freq/length(preprocessedCorpus)),groups=as.factor(wordsKmeans$cluster))
					if(input$assocSize!=0){
						wordsDf<- subset(wordsDf,affiliations>quantile(wordsDf$affiliations)[input$assocSize]) }
					ggplot(data=wordsDf,aes(x=x.pos,y=y.pos)) + geom_text(aes(label=term,size=freq,alpha=percentOfDoc,colour=groups)) + xlab("") + ylab("") +
					theme_bw() + scale_colour_brewer(palette="Dark2",guide="none") + scale_size("Word Frequency") + scale_alpha("Overall Importance of Word",range=c(0.4,1),guide="none") +
					scale_x_continuous(breaks=c(min(wordsDf$x.pos),max(wordsDf$x.pos)),labels=c("","")) + scale_y_continuous(breaks=c(min(wordsDf$y.pos),max(wordsDf$y.pos)),labels=c("","")) +
					theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.ticks=element_blank())
				})
			})	

output$assocCloud<- renderPlot({
				if(input$generateAssoc==0)
				return()
				print(associativeCloud())
			})

output$downloadAssoc<- downloadHandler(
				filename=function(){paste0("AssocWordCloud",Sys.time(),".pdf")},
				content=function(file){
					pdf(file,width=14,height=12)
					print(associativeCloud())
					dev.off()
			})





#========================================================================================================================#
##===================================  Section 7: Clustering Documents =============================================================##


mdsDocClustering<- reactive({
				if(input$generateDocCluster==0)
				return()
				isolate({
					distMatrix<- dist(finalUnigramMatrix())
					mdsPoints<- cmdscale(d=distMatrix,eig=TRUE)
					clusterData<- data.frame(docNumber=rownames(finalUnigramMatrix()),x=mdsPoints$points[,1],y=mdsPoints$points[,2])
					ggplot(data=clusterData,aes(x=x,y=y)) + geom_point() + geom_text(aes(label=attr(distMatrix,"Labels"))) +
					ggtitle("Clustering Documents")
					})
			})
					

output$docClusters<- renderPlot({
				if(input$generateDocCluster==0)
				return()
				print(mdsDocClustering())
			})

output$downloadDocCluster<- downloadHandler(
					filename=function(){paste0("DocClusters",Sys.time(),".pdf")},
					content=function(file){
						pdf(file,width=14,height=12)
						print(mdsDocClustering())
						dev.off()
			})







#========================================================================================================================#
##===================================  Section 8: Word Networks ===============================================================##




wordNetworkGraph<- reactive({
				if(input$generateNetwork==0)
				return()
				isolate({
					#progress<- Progress$new(session,min=1,max=15)
					#on.exit(progress$close())
					#progress$set(message="Loading Graphic")
					initialMat<- finalUnigramMatrix()
					secondaryMat<- as.matrix(initialMat)
					adjacencyMat<- secondaryMat %*% t(secondaryMat)
					adjacencyDf<- data.frame(adjacencyMat)
					words<- rownames(secondaryMat)
					freq<- rowSums(secondaryMat)
					affiliations<- diag(adjacencyMat)
					if(input$networkSize==0){
						adjGraph<- graph.adjacency(adjacencyMat,diag=FALSE,weighted=TRUE)
						adjGraph<- delete.edges(adjGraph,E(adjGraph)[E(adjGraph)$weight<=1])	
						if(input$networkPresentation=="kk"){
							coordinates<- layout.kamada.kawai(adjGraph) }
						else {
							coordinates<- layout.fruchterman.reingold(adjGraph,weights=E(adjGraph)$weight) }					
						myEdges<- get.edgelist(adjGraph)
						mainDf<- data.frame(words=rownames(secondaryMat),freq=rowSums(secondaryMat),x.pos=coordinates[,1],y.pos=coordinates[,2],affiliations=affiliations,stringsAsFactors=FALSE)
						otherDf<- data.frame(edges1=myEdges[,1],edges2=myEdges[,2],weights=E(adjGraph)$weight,stringsAsFactors=FALSE)
						otherDf$id<- row.names(otherDf)
						auxiliaryDf<- melt(otherDf,id.vars=c(3,4))
						names(auxiliaryDf)[4]<- "words"
						finalDf<- merge(mainDf,auxiliaryDf,by="words",sort=FALSE)
						finalDf<- na.omit(finalDf)
						ggplot(data=finalDf,aes(x=x.pos,y=y.pos)) + geom_line(linetype=1,aes(colour=weights,group=id,alpha=0.2)) +
							geom_text(aes(x=x.pos,y=y.pos,label=words,size=freq)) + scale_size(guide="none") + scale_alpha(guide="none") + 
							scale_colour_continuous(low="#CCCC66",high="#006600","Weight Strength") + theme_bw() + xlab("") + ylab("") +
							scale_x_continuous(breaks=c(min(finalDf$x.pos),max(finalDf$x.pos)),labels=c("","")) + 
							scale_y_continuous(breaks=c(min(finalDf$y.pos),max(finalDf$y.pos)),labels=c("","")) +
							theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.ticks=element_blank()) }
					else {
						initialDf<- data.frame(words=words,freq=freq,affiliations=affiliations,stringsAsFactors=FALSE)
						impWords<- initialDf[initialDf$affiliations>quantile(initialDf$affiliations)[input$networkSize],"words"]
						reducedAdjDf<- adjacencyDf[impWords,]
						reducedAdjDf<- reducedAdjDf[,!names(reducedAdjDf) %in% impWords]
			
						incidGraph<- graph.incidence(reducedAdjDf,mode=c("out"),multiple=TRUE, weighted=TRUE)
						incidGraph<- delete.edges(incidGraph,E(incidGraph)[E(incidGraph)$weight <=1])
						Words<- V(incidGraph)$name
						Edges<- get.edgelist(incidGraph)
						Weights<- E(incidGraph)$weight
						if(input$networkPresentation=="kk"){
							coordinates<- layout.kamada.kawai(incidGraph) }
						else {
							coordinates<- layout.fruchterman.reingold(incidGraph,weights=E(incidGraph)$weight) }
						mainDf<- data.frame(words=Words,x.pos=coordinates[,1],y.pos=coordinates[,2],stringsAsFactors=FALSE)
						wordMatch<- match(mainDf$words,initialDf$words)
						mainDf$freq<- initialDf$freq[wordMatch]

						otherDf<- data.frame(edge1=Edges[,1],edge2=Edges[,2],weights=Weights,stringsAsFactors=FALSE)
						otherDf$id<- row.names(otherDf)
						auxiliaryDf<- melt(otherDf,id.vars=c(3,4))
						names(auxiliaryDf)[4]<- "words"
						finalDf<- merge(mainDf,auxiliaryDf,by="words",sort=FALSE)
						finalDf<- na.omit(finalDf)
						ggplot(data=finalDf,aes(x=x.pos,y=y.pos)) + geom_line(linetype=1,aes(colour=weights,group=id,alpha=0.2)) +
							geom_text(aes(x=x.pos,y=y.pos,label=words,size=freq)) + scale_size(guide="none") + scale_alpha(guide="none") +
							scale_colour_continuous(low="#CCCC66",high="#006600","Weight Strength") + theme_bw() + xlab("") + ylab("") +
							scale_x_continuous(breaks=c(min(finalDf$x.pos),max(finalDf$x.pos)),labels=c("","")) +
							scale_y_continuous(breaks=c(min(finalDf$y.pos),max(finalDf$y.pos)),labels=c("","")) +
							theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.ticks=element_blank()) } 
					})
			})

output$wordNetwork<- renderPlot({
				if(input$generateNetwork==0)
				return()
				print(wordNetworkGraph())
			})

output$downloadNetwork<- downloadHandler(
				filename=function(){paste0("wordsNetwork",Sys.time(),".pdf")},
				content=function(file){
					pdf(file,width=16,height=12)
					print(wordNetworkGraph())
					dev.off()
			})





observe({
	if(input$phase=="import" | input$phase=="preprocess" | input$phase=="featureGenerate" | input$phase=="featureSelect"){
		updateTabsetPanel(session,"tabset1","Corpus Generation")}
	else if(input$phase=="initialAnalysis"){
		updateTabsetPanel(session,"tabset1","Initial Analysis")}
	else if(input$phase=="dendroGenerate"){
		updateTabsetPanel(session,"tabset1","Dendrogram")}
	else if(input$phase=="clusterWords"){
		updateTabsetPanel(session,"tabset1","Clustering Words")}
	else if(input$phase=="clusterDocs"){
		updateTabsetPanel(session,"tabset1","Clustering Documents")}
	else if(input$phase=="wordNetworkGenerate"){
		updateTabsetPanel(session,"tabset1","Word Networks")}
	else if(input$phase=="about"){
		updateTabsetPanel(session,"tabset1","About")}
	else if(input$phase=="introduction"){
		updateTabsetPanel(session,"tabset1","Introduction")}
})






################################################### OLD CODE #######################################################################################
################################################### OLD CODE ######################################################################################
################################################### OLD CODE ######################################################################################




#===For bigrams
#finalBigramMatrix<- reactive({
#				weightingScheme<- paste0(input$termWeight,input$docWeight,input$normalisation)
##				initialMatrix<- TermDocumentMatrix(preprocessedCorpus(),
#							control=list(weighting=function(x) weightSMART(x,spec=weightingScheme),
#							tokenize=bigramTokenizer))
#				initialMatrix })




#phraseGraphic<- reactive({
#			if(input$generatePhrase==0)
#			return()
#			isolate({
#			myMat<- finalBigramMatrix()
#			words<- rownames(as.matrix(myMat))
#			distMat<- dist(as.matrix(myMat))
#			set.seed(1013)
#			groups<- kmeans(distMat,centers=input$groupPhrase)
#			freq<- rowSums(as.matrix(myMat))
#			bigramDf<- data.frame(words=words,freq=freq,cluster=as.factor(groups$cluster),stringsAsFactors=FALSE)
#			
#			bigramDf<- transform(bigramDf,y.pos=mySpacing(bigramDf$freq))
#			if(input$representation=="frequency"){
#				if(input$phraseSize!=0){
#					bigramDf<- subset(bigramDf,freq>quantile(bigramDf$freq)[input$phraseSize])
#				}
##				ggplot(data=bigramDf,aes(x=freq,y=y.pos)) + geom_text(aes(label=words,size=freq,colour=cluster)) + xlab("") + ylab("Term Frequency") +
#				theme_bw() + scale_colour_brewer(palette="Dark2",guide="none") + scale_size("Word Frequency")  +
#				scale_x_continuous(breaks=c((min(bigramDf$freq))-10,(max(bigramDf$freq))+10),labels=c("","")) + 
#				scale_y_continuous(breaks=c((min(bigramDf$y.pos))-10,(max(bigramDf$y.pos))+10),labels=c("","")) +
#				theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.ticks=element_blank()) 
#			}
#			else{
#				bigramDf<- transform(bigramDf,x.pos=4*(as.numeric(cluster)))
#				if(input$phraseSize!=0){
#					bigramDf<- subset(bigramDf,freq>quantile(bigramDf$freq)[input$phraseSize])
#				}
#				ggplot(data=bigramDf,aes(x=x.pos,y=y.pos)) + geom_text(aes(label=words,size=freq,colour=cluster)) + xlab("") + ylab("Term Frequency") +
#				theme_bw() + scale_colour_brewer(palette="Dark2",guide="none") + scale_size("Word Frequency",range=c(3,15))  +
#				scale_x_continuous(breaks=c((min(bigramDf$x.pos))-5,(max(bigramDf$x.pos))+5),labels=c("","")) + 
#				scale_y_continuous(breaks=c((min(bigramDf$y.pos))-5,(max(bigramDf$y.pos))+5),labels=c("","")) +
#				theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.ticks=element_blank()) 
#			} }) })
#


#output$dendrogram<- renderPlot({
#				if(input$generateDendro==0)
#				return()
#				print(dendroGraphic())
#			})

#output$downloadDendro<- downloadHandler(
#				filename=function(){paste("Dendrogram",Sys.time(),".pdf",sep="")},
#				content=function(file){
#					pdf(file, width=14,height=12)
#					print(dendroGraphic())
#					dev.off()
#			})
#

#output$assocCloud<- renderPlot({
#				if(input$generateAssoc==0)
#				return()
#				print(assocGraphic())
#			})
#


#output$phraseCloud<- renderPlot({
#				if(input$generatePhrase==0)
#				return()
#				print(phraseGraphic())
#			})
#
#output$downloadPhrase<- downloadHandler(
#				filename=function(){paste("Phrase Cloud",Sys.time(),".pdf",sep="")},
#				content=function(file){
#					pdf(file,width=14,height=12)
#					print(phraseGraphic())
#					dev.off()
#			})


#output$wordNetwork<- renderPlot({
#				if(input$generateNetwork==0)
#				return()
#				print(networkGraphic())
#			})




##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################

#unigramMatrix<- reactive({
#			weightingScheme<- paste0(input$termWeight,input$docWeight,input$normalisation)
#			initialMatrix<- TermDocumentMatrix(preprocessedCorpus(),
#					   control=list(weighting=function(x) weightSMART(x,spec=weightingScheme)))
#			finalUnigramMatrix<- initialMatrix
##			finalUnigramMatrix
#			if(input$lowerFreq!=1){
#			lowerBound<- findFreqTerms(finalUnigramMatrix,input$lowerFreq,Inf)
#			finalUnigramMatrix<- finalUnigramMatrix[lowerBound]}
#			if(input$sparsity!=100){
#			finalUnigramMatrix<- removeSparseTerms(finalUnigramMatrix,sparse=(input$sparsity/100))} 
##			finalUnigramMatrix 
#			})
#


#finalBigramMatrix<- reactive({
#			weightingScheme<- paste0(input$termWeight,input$docWeight,input$normalisation)
#			initialMatrix<- TermDocumentMatrix(preprocessedCorpus(),
#					   control=list(weighting=function(x) weightSMART(x,spec=weightingScheme),
#					   tokenize=bigramTokenizer))
#			finalBigramMatrix<- initialMatrix
#			if(input$lowerFreq!=1){
#			lowerBound<- findFreqTerms(finalBigramMatrix,input$lowerFreq,Inf)
#			finalBigramMatrix<- finalBigramMatrix[lowerBound]}
#			if(input$sparsity!=100){
#			finalBigramMatrix<- removeSparseTerms(finalBigramMatrix,sparse=(input$sparsity/100))} 
#			finalBigramMatrix })
			


#output$downloadDendro<- downloadHandler(
#				filename=function(){paste("dendrogram",Sys.time(),".png",sep="")},
#				content=function(file) {
#					png(file)
#					print(dendroGraphic())
#					dev.off()
#				})



#output$downloadAssoc<- downloadHandler(
#				filename=function(){paste("Associative WordCloud",Sys.time(),".png",sep="")},
#				content=function(file) {
#					png(file, width=1000,height=800,res=80,pointsize=4)
#					print(assocGraphic())
#					dev.off()
#				})

#output$downloadAssoc2<- downloadHandler(
#				filename=function(){paste("Associative WordCloud",Sys.time(),".png",sep="")},
#				content=function(file) {
#					
#					ggsave(file,assocGraphic(),dpi=1200)
#				})




#output$downloadPhrase<- downloadHandler(
#				filename=function(){paste("Phrase Cloud",Sys.time(),".png",sep="")},
#				content=function(file) {
#				png(file)
#				print(phraseGraphic())
#				dev.off()
#				})

#output$downloadNetwork<- downloadHandler(
#					filename=function(){paste("WordNetwork",Sys.time(),".png",sep="")},
#					content=function(file) {
#					png(file)
#					print(networkGraphic())
#					dev.off()
#				})
#



#unigramMatrix<- reactive({
#			if(input$termweight=="weightTf"){
#			initialUnigramMatrix<- TermDocumentMatrix(preprocessedCorpus(),control=list(weighting=weightTf)) }
#			if(input$weight=="weightTfIdf"){
#			initialUnigramMatrix<- TermDocumentMatrix(preprocessedCorpus(),control=list(weighting=weightTfIdf)) }
#			if(input$weight=="bin"){
#			initialUnigramMatrix<- TermDocumentMatrix(preprocessedCorpus(),control=list(weighting=weightBin)) }
#			finalUnigramMatrix<- initialUnigramMatrix
#			if(input$lowerFreq!=1){
#			lowerBound<- findFreqTerms(finalUnigramMatrix,input$lowerFreq,Inf)
#			finalUnigramMatrix<- initialUnigramMatrix[lowerBound]}
#			if(input$sparsity!=100){
#			finalUnigramMatrix<- removeSparseTerms(finalUnigramMatrix, sparse=(input$sparsity/100))}
#			finalUnigramMatrix })
#

#bigramMatrix<- reactive({
#			if(input$weight=="weightTf"){
#			initialBigramMatrix<- TermDocumentMatrix(preprocessedCorpus(),control=list(weighting=weightTf,tokenize=bigramTokenizer)) }
#			if(input$weight=="weightTfIdf"){
#			initialBigramMatrix<- TermDocumentMatrix(preprocessedCorpus(),control=list(weighting=weightTfIdf,tokenize=bigramTokenizer)) }
#			if(input$weight=="bin"){
#			initialBigramMatrix<- TermDocumentMatrix(preprocessedCorpus(),control=list(weighting=weightBin,tokenize=bigramTokenizer)) }
#			lowerBound<- findFreqTerms(initialBigramMatrix,input$lowerFreq,Inf)
#			finalBigramMatrix<- initialBigramMatrix[lowerBound]
#			if(input$sparsity!=100){
#			finalBigramMatrix<- removeSparseTerms(finalBigramMatrix, sparse=(input$sparsity/100))}
#			finalBigramMatrix })
#



#assocWordsDf1<- reactive({
#			myMat<- as.matrix(unigramMatrix())
#			myAdjMat<- myMat %*% t(myMat)
#			diag(myAdjMat)<- 0
#			freq<- rowSums(myMat)
#			
#			adjGraph<- graph.adjacency(myAdjMat,weighted=TRUE)
#			coordinates<- layout.kamada.kawai(adjGraph)
#			wordsDf<- data.frame(term=V(adjGraph)$name,x.pos=coordinates[,1],y.pos=coordinates[,2],stringsAsFactors=FALSE)
#			set.seed(1011)
#			wordsKmeans<- kmeans(wordsDf[,c(2,3)],5)
#			
#			wordsDf<- transform(wordsDf,freq=freq, percentOfDoc=100*(freq/length(preprocessedCorpus)),groups=as.factor(wordsKmeans$cluster))
#			ggplot(data=wordsDf,aes(x=x.pos,y=y.pos)) + geom_text(aes(label=term,size=freq,alpha=percentOfDoc,colour=groups)) + xlab("") + ylab("") +
#			theme_bw() + scale_colour_brewer(palette="Dark2",guide="none") + scale_size("Word Frequency",range=c(3,15)) + scale_alpha("Overall Importance of Word",range=c(0.4,1)) +
#			scale_x_continuous(breaks=c(min(wordsDf$x.pos),max(wordsDf$x.pos)),labels=c("","")) + scale_y_continuous(breaks=c(min(wordsDf$y.pos),max(wordsDf$y.pos)),labels=c("","")) +
#			theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.ticks=element_blank()) })
#



#networkDf1<- reactive({
#			myMat<- as.matrix(unigramMatrix())
#			adjMat<- myMat %*% t(myMat)
#			
#			adjGraph<- graph.adjacency(adjMat,diag=FALSE,weighted=TRUE)
#			adjGraph<- delete.edges(adjGraph,E(adjGraph)[E(adjGraph)$weight<=1])	
#			coordinates<- layout.kamada.kawai(adjGraph)					
#			myEdges<- get.edgelist(adjGraph)
#			mainDf<- data.frame(words=rownames(myMat),freq=rowSums(myMat),x.pos=coordinates[,1],y.pos=coordinates[,2],stringsAsFactors=FALSE)
#			otherDf<- data.frame(edges1=myEdges[,1],edges2=myEdges[,2],weights=E(adjGraph)$weight,stringsAsFactors=FALSE)
#			otherDf$id<- row.names(otherDf)
#			auxiliaryDf<- melt(otherDf,id.vars=c(3,4))
#			names(auxiliaryDf)[4]<- "words"
#			finalDf<- merge(mainDf,auxiliaryDf,by="words",sort=FALSE)
#			finalDf<- na.omit(finalDf)
#			ggplot(data=networkDf1,aes(x=x.pos,y=y.pos)) + geom_line(linetype=1,aes(colour=weights,group=id,alpha=0.2)) +
#				geom_text(aes(x=x.pos,y=y.pos,label=words,size=freq)) + scale_size(guide="none") + scale_alpha(guide="none") + 
#				scale_colour_continuous(low="#CCCC66",high="#006600","Weight Strength") + theme_bw() + xlab("") + ylab("") +
#				scale_x_continuous(breaks=c(min(networkDf1$x.pos),max(networkDf1$x.pos)),labels=c("","")) + 
#				scale_y_continuous(breaks=c(min(networkDf1$y.pos),max(networkDf1$y.pos)),labels=c("","")) +
#				theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.ticks=element_blank()) })





#output$assocCloud<- renderPlot({
#				if(!(input$fullCloud & input$assocCloud))
#				return() 
#				isolate({
#				print(ggplot(data=assocDf,aes(x=x.pos,y=y.pos)) + geom_text(aes(label=term,size=freq,alpha=percentOfDoc,colour=groups)) + xlab("") + ylab("") +
#			    	theme_bw() + scale_colour_brewer(palette="Dark2",guide="none") + scale_size("Word Frequency",range=c(3,15)) + scale_alpha("Overall Importance of Word",range=c(0.4,1)) +
#			    	scale_x_continuous(breaks=c(min(assocDf$x.pos),max(assocDf$x.pos)),labels=c("","")) + scale_y_continuous(breaks=c(min(assocDf$y.pos),max(assocDf$y.pos)),labels=c("","")) +
#			    	theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.ticks=element_blank())) })
# 			})

				


#output$assocCloud2<- renderPlot({
#				assocDf<- assocWordsDf()
#				assocDfSubset<- subset(assocDf,freq>quantile(assocDf$freq)[2])
#				if(!(input$impCloud & input$assocCloud))
#				return() 
#				isolate({				
#				print(ggplot(data=assocDfSubset,aes(x=x.pos,y=y.pos)) + geom_text(aes(label=term,size=freq,alpha=percentOfDoc,colour=groups)) + xlab("") + ylab("") +
#			    	theme_bw() + scale_colour_brewer(palette="Dark2",guide="none") + scale_size("Word Frequency",range=c(3,15)) + scale_alpha("Overall Importance of Word",range=c(0.4,1)) +
#			    	scale_x_continuous(breaks=c(min(assocDfSubset$x.pos),max(assocDfSubset$x.pos)),labels=c("","")) + scale_y_continuous(breaks=c(min(assocDfSubset$y.pos),max(assocDfSubset$y.pos)),labels=c("","")) +
#			    	theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.ticks=element_blank())) })
#			}) 

	

#output$downloadCloud2<- downloadHandler(
#				filename=function() {paste("assocWordCloud2",Sys.time(),".png",sep="")},
#				content=function(file){
#					png(file)
#    					assocCloud2()
#      				dev.off()
#    				})					
	


#output$phraseCloud1<- renderPlot({
#				phraseDf1<- phraseDf()
#				if(!(input$fullPhrase & input$phraseCloud))
#				return()
#				isolate({
#				print(ggplot(data=phraseDf1,aes(x=freq,y=y.pos)) + geom_text(aes(label=words,colour=cluster)) +
#				theme_bw() + scale_colour_brewer(palette="Dark2",guide="none") + xlab("") + ylab("") +
#				scale_x_continuous(breaks=c(min(phraseDf1$freq),max(phraseDf1$freq)),labels=c("","")) + scale_y_continuous(breaks=c(min(phraseDf1$y.pos),max(phraseDf1$y.pos)),labels=c("","")) +
#				theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.ticks=element_blank())) })
#			})


#output$phraseCloud2<- renderPlot({
#				phraseDf2<- phraseDf()
#				phraseDf2Subset<- subset(phraseDf2,freq>quantile(phraseDf2$freq)[3])
#				if(!(input$impPhrase & input$phraseCloud))
#				return()
#				isolate({
#				print(ggplot(data=phraseDf2Subset,aes(x=freq,y=y.pos)) + geom_text(aes(label=words,colour=cluster)) +
#				theme_bw() + scale_colour_brewer(palette="Dark2",guide="none") + xlab("") + ylab("") +
#				scale_x_continuous(breaks=c(min(phraseDf2Subset$freq),max(phraseDf2Subset$freq)),labels=c("","")) + scale_y_continuous(breaks=c(min(phraseDf2Subset$y.pos),max(phraseDf2Subset$y.pos)),labels=c("","")) +
#				theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.ticks=element_blank())) })
#			})
#				
		
#output$network1<- renderPlot({
#				networkDf1<- networkDf1()
#				if(!(input$fullNetwork & input$networkWords))
#				return()
#				isolate({
#				print(ggplot(data=networkDf1,aes(x=x.pos,y=y.pos)) + geom_line(linetype=1,aes(colour=weights,group=id,alpha=0.2)) +
#				geom_text(aes(x=x.pos,y=y.pos,label=words,size=freq)) + scale_size(guide="none") + scale_alpha(guide="none") + 
#				scale_colour_continuous(low="#CCCC66",high="#006600","Weight Strength") + theme_bw() + xlab("") + ylab("") +
#				scale_x_continuous(breaks=c(min(networkDf1$x.pos),max(networkDf1$x.pos)),labels=c("","")) + 
#				scale_y_continuous(breaks=c(min(networkDf1$y.pos),max(networkDf1$y.pos)),labels=c("","")) +
#				theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.ticks=element_blank())) })
#			})
						

#output$network2<- renderPlot({
#				networkDf2<- networkDf2()
#				if(!(input$impNetwork & input$networkWords))
#				return()
#				isolate({
#				print(ggplot(data=networkDf2,aes(x=x.pos,y=y.pos)) + geom_line(linetype=1,aes(colour=weights,group=id,alpha=0.2)) +
#				geom_text(aes(x=x.pos,y=y.pos,label=words,size=freq)) + scale_size(guide="none") + scale_alpha(guide="none") +
#				scale_colour_continuous(low="#CCCC66",high="#006600","Weight Strength") + theme_bw() + xlab("") + ylab("") +
#				scale_x_continuous(breaks=c(min(networkDf2$x.pos),max(networkDf2$x.pos)),labels=c("","")) +
#				scale_y_continuous(breaks=c(min(networkDf2$y.pos),max(networkDf2$y.pos)),labels=c("","")) +
#				theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.ticks=element_blank())) })
#			})


#output$testing<- renderPrint({
#			myDf<- assocWordsDf ()
#			myDf })
#




#assocCloudGraph1<- reactive({
#			myMat<- as.matrix(unigramMatrix())
#			myAdjMat<- myMat %*% t(myMat)
#			myAdjMatCopy<- myAdjMat
#			diag(myAdjMatCopy)<- 0
#
#			adjGraph<- graph.adjacency(myAdjMatCopy,weighted=TRUE)
#			coordinates<- layout.kamada.kawai(adjGraph)
#			wordsDf<- data.frame(term=V(adjGraph)$name,x.pos=coordinates[,1],y.pos=coordinates[,2],stringsAsFactors=FALSE)
#			set.seed(1011)
#			wordsKmeans<- kmeans(wordsDf[,c(2,3)],5)
#			wordsDf<- transform(wordsDf,freq=diag(myAdjMat), percentOfDoc=100*(diag(myAdjMat)/length(preprocessedCorpus)),groups=as.factor(wordsKmeans$cluster))
#			
#			print(ggplot(data=wordsDf,aes(x=x.pos,y=y.pos)) + geom_text(aes(label=term,size=freq,alpha=percentOfDoc,colour=groups)) + xlab("") + ylab("") +
#			    theme_bw() + scale_colour_brewer(palette="Dark2",guide="none") + scale_size("Word Frequency",range=c(3,15)) + scale_alpha("Overall Importance of Word",range=c(0.4,1)) +
#			    scale_x_continuous(breaks=c(min(wordsDf$x.pos),max(wordsDf$x.pos)),labels=c("","")) + scale_y_continuous(breaks=c(min(wordsDf$y.pos),max(wordsDf$y.pos)),labels=c("","")) +
#			    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.ticks=element_blank()))
#			})
# the above didn't work as plotting instructions are passed to reactive
# rather than to renderPlot



				



				

#output$assocCloud2<- renderPlot({
#				assocCloudGraph2() })
#				


#output$test1<- renderPrint({
#		if(input$focus=="words")
#		print("ok")
#		})	

#output$test2<- renderPrint({
#		stat<- input$sparsity
#		stat
#		})	


#output$sparseTest<- renderPrint({ input$sparsity })



})