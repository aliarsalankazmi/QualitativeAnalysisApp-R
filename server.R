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
unLoaded<- which(loadLib!=TRUE)
install.packages(libraries[unLoaded],repos="http://cran.us.r-project.org")
sapply(libraries[unLoaded],function(x) require(x,character.only=TRUE))
}



#============================= Build required functions

#======= Pre-processing Corpus

cleanSweepCorpus<- function(corpus, useStopwords=FALSE, stem=FALSE,removePunct=FALSE,removeNum=FALSE,useSynonyms=FALSE,
				initialWords,replacementWords,useCustomStopwords=FALSE,customStopwords){
newCorpus<- corpus
newCorpus<- tolower(newCorpus)
newCorpus<- gsub("[\\)(\"]"," ",newCorpus)
if(useStopwords != FALSE){
	englishStopwords<- c(stopwords("SMART"),stopwords("english"))
	englishStopwords<- gsub("^","\\\\b",englishStopwords)
	englishStopwords<- gsub("$","\\\\b",englishStopwords)
	x<- mapply(FUN=function(...){
	newCorpus<<- gsub(...,replacement=" ",x=newCorpus)},
	pattern=englishStopwords)
	}
if (useCustomStopwords != FALSE){
	stopwordsList<- unlist(strsplit(customStopwords,split=","))
	modifiedStopwordsList<- gsub("^\\s*","\\\\b",stopwordsList)
	modifiedStopwordsList<- gsub("\\s*$","\\\\b",modifiedStopwordsList)
	x<- mapply(FUN=function(...){
	newCorpus<<- gsub(...,replacement=" ",x=newCorpus)},
	pattern=modifiedStopwordsList)
	}
if(useSynonyms != FALSE){
	toChange<- unlist(strsplit(initialWords,split=","))
	changeInto<- unlist(strsplit(replacementWords,split=","))
	toChangeWords<- gsub("^\\s*","\\\\b",toChange)
	toChangeWords<- gsub("\\s*$","\\\\b",toChangeWords)
	changeIntoWords<- gsub("^\\s*|\\s*$","",changeInto)
	if(length(toChangeWords)==length(changeIntoWords)){
		x<- mapply(FUN=function(...){
		newCorpus<<- gsub(...,x=newCorpus)},
		pattern=toChangeWords, replacement=changeIntoWords)}
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
finalCorpus<- tm_map(finalCorpus,stripWhitespace)
return(finalCorpus)
}



#======= Access my GitHub url for loading datasets

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
					newCorpus<- cleanSweepCorpus(corpus=initialCorpus(),useStopwords=input$stopwords,
					stem=input$stemming,removePunct=input$punctuation,removeNum=input$numbers,useSynonyms=input$customThes,
					useCustomStopwords=input$customStopword,initialWords=input$customThesInitial,
					replacementWords=input$customThesReplacement,customStopwords=input$cusStopwords)
					newCorpus })
			})


output$procCorpusStatus<- renderPrint({
					if(input$startPreprocess==0)
					return("No pre-processing applied on Corpus")
					isolate({
						preprocessedCorpus () })
			})



#####output$stopper<- renderText({input$cusStopwords })
#####output$check<- renderText({input$customStop})


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
					xlab("Words' Rank") + ylab("Frequency in Log scale") + scale_size(guide="none") + ggtitle("Rank-Frequency Plot")
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
##===================================  Section 6: Clustering Documents =============================================================##


mdsDocClustering<- reactive({
				if(input$generateDocCluster==0)
				return()
				isolate({
					distMatrix<- dist(t(finalUnigramMatrix()))
					mdsPoints<- cmdscale(d=distMatrix,eig=TRUE)
					kClusters<- kmeans(t(finalUnigramMatrix()),centers=input$groupDocs,nstart=30)
					clusterData<- data.frame(docNumber=colnames(finalUnigramMatrix()),x=mdsPoints$points[,1],y=mdsPoints$points[,2],cluster=kClusters$cluster)
					ggplot(data=clusterData,aes(x=x,y=y)) + geom_point() + geom_text(aes(label=docNumber,colour=as.factor(cluster))) +
					ggtitle("Clustering Documents") + scale_colour_brewer(palette="Dark2",name="Document Groups") + theme(axis.ticks=element_blank()) +
					xlab("") + ylab("") + scale_x_continuous(breaks=c(min(clusterData$x),max(clusterData$x)),labels=c("","")) +
					scale_y_continuous(breaks=c(min(clusterData$y),max(clusterData$y)),labels=c("",""))
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
##===================================  Section 7: Clustering words ===============================================================##





dendroGraphic<- reactive({
			if(input$generateDendro==0)
			return()
			isolate({
				mainMat<- finalUnigramMatrix()
				if(input$dendroSize!=lowerFreqRangeDendro()[1]){
					targetWords<- findFreqTerms(mainMat,input$dendroSize,Inf)
					mainMat<- mainMat[targetWords] }
				distMat<- dist(scale(mainMat))
				dendro<- hclust(distMat,method="average")
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
	else if(input$phase=="userGuide"){
		updateTabsetPanel(session,"tabset1","Introduction")}
})




})