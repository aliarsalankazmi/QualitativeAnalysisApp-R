###load the necessary package###

require(shiny)

###select type of page to be created###

shinyUI(pageWithSidebar(
			headerPanel(
			#HTML('<div id="header"><img src="http://www.norfolk-pcc.gov.uk/wp-content/uploads/2013/10/OfficePCCforNorfolkLogo_Blue.png" alt="OPCC,Norfolk"/></div>'),
			"Exploratory Qualitative Analysis with R",
			windowTitle="Exploring Textual Data with R"),

###select configuration for sidebarPanel###

sidebarPanel(tags$h4("Phase Selection"),
	br(),
	selectInput("phase","Which phase would you like to choose?",
			c("Introduction"="introduction",
			"1. Importing Corpus"="import",
			"2. Pre-processing"="preprocess",
			"3. Feature Generation"="featureGenerate",
			"4. Feature Selection"="featureSelect",
			"5. Initial Analysis"="initialAnalysis",
			"6. Clustering Words"="clusterWords",
			"7. Cluster Documents"="clusterDocs",
			"8. Words Network Generation"="wordNetworkGenerate",
			"About"="about"),selected="Introduction"),
	br(),

		conditionalPanel(
			condition = "input.phase == 'import'",
			wellPanel(tags$strong("Importing a Corpus"),
			selectInput("corpusType","Select the type of Corpus?",
					c("A directory of Text files"="dir",
				  	  "A single Text file"="vector",
					  "Sample corpus"="sample")),
				conditionalPanel(
					condition = "input.corpusType != 'sample'",
					wellPanel(tags$strong("Upload your Corpus"),
					textInput("filePath","Please enter the Directory/File path:"),
					helpText(tags$strong("Note:"), "For Directories, use the following notation"),
					helpText(tags$em("'My Documents/My Project/My Directory/'")),
					helpText("For individual files, use the following notation"),
					helpText(tags$em("'My Documents/My Project/My TextFile.txt'"))
						)
					),
				conditionalPanel(
					condition = "input.corpusType == 'sample'",
					wellPanel(
					radioButtons("sampleCorpus", "Select custom Corpus:",
					c("UAE Expat Forum" = "UAEexpatForum",
					  "UAE Trip Advisor" = "UAEtripAdvisor",
					  "Middle East Politics" = "middleEastPolitics"))
						)
					),
			br(),
			actionButton("confirm","Upload Corpus")
				)
			),

	conditionalPanel(
		condition="input.phase=='preprocess'",
		wellPanel(tags$strong("Pre-processing on Corpus"),
		checkboxInput("punctuation","Punctuation Removal",FALSE),
		checkboxInput("numbers","Numbers Removal",FALSE),
		checkboxInput("stemming","Stem Words",FALSE),
		checkboxInput("stopwords","Stopwords Removal",FALSE),
		checkboxInput("customStop","Custom Stopwords",FALSE),
		checkboxInput("customThes","Custom Thesaurus",FALSE),
		br(),
		actionButton("startPreprocess","Apply Pre-processing")
			)
		),

	conditionalPanel(
		condition="input.phase=='featureGenerate'",
		wellPanel(tags$strong("Feature Generation"),
		radioButtons("termWeight","What is your Weighting Crietrion for Words?",
				c("Word Frequency"="n",
				  "Binary Frequency"="b",
				  "Logarithmic Scaling of Frequency"="l",
				  "Augmented Frequency"="a",
				  "Log-Average Frequency"="L")),
		radioButtons("docWeight","What is your Weighting Criterion for Documents?",
				c("Document Frequency"="n",
				  "Inverse Document Frequency"="t",
				  "Probabilistic Inverse Document Frequency Factor"="p")),
		radioButtons("normalisation","What is your Normalisation scheme?",
				c("None"="n",
				  "Cosine"="c")),
		br(),
		actionButton("generateMatrix","Generate Features")
			)
		),

	conditionalPanel(
		condition="input.phase=='featureSelect'",
		wellPanel(tags$strong("Feature Selection"),
		br(),
		uiOutput("lowerFreqSlider"),
		sliderInput("sparsity","Please set the Maximum Allowed Sparsity (in %)",
				min=20, max=100,value=100,step=1),
		br(),
		actionButton("selectFeatures", "Select Features")
			)
		),				
			
	conditionalPanel(
		condition="input.phase=='initialAnalysis'",
		wellPanel(tags$strong("Initial Analysis"),
		br(),
		actionButton("generateRankFreq","Generate Rank-Frequency Plot"),
		br(),
		downloadButton("downloadRankFreqPlot","Download Rank-Frequency Plot"),
		br(),
		actionButton("generateWordFreq","Generate Word-Frequency Plot"),
		br(),
		downloadButton("downloadWordFreqPlot","Download Word Frequency Plot")
			)
		),


	conditionalPanel(
		condition="input.phase=='clusterWords'",
		wellPanel(tags$strong("Associative Word Clouds Generation"),
		sliderInput("assocSize","Select Quantile for Word Frequency",
		min=0,max=4,value=0,step=1,ticks=TRUE),
		sliderInput("groupAssoc","Select number of groups to identify",
		min=1,max=8,value=5,step=1,ticks=TRUE),
		radioButtons("assocPresentation","Which Graph-drawing algorithm would you like to use?",
				c("Kamada & Kawai"="kk",
				  "Fruchterman & Reingold"="fr")),
		br(),
		actionButton("generateAssoc","Generate Associative Word Cloud"),
		br(),
		downloadButton("downloadAssoc","Download Associative Word Cloud")
			),
		wellPanel(tags$strong("Dendrogram Generation"),
		uiOutput("lowerFreqSliderDendro"),
		actionButton("generateDendro","Generate Dendrogram"),
		br(),
		downloadButton("downloadDendro","Download Dendrogram")
			)
		),



	conditionalPanel(
		condition="input.phase=='clusterDocs'",
		wellPanel(tags$strong("Please select type of Clustering to perform"),
		actionButton("generateDocCluster","Generate Document Clusters"),
		br(),
		downloadButton("downloadDocCluster","Download Document Clusters")
			)
		),



	conditionalPanel(
		condition="input.phase=='wordNetworkGenerate'",
		wellPanel(tags$strong("Words Network Generation"),
		sliderInput("networkSize","Select Quantile for Word Frequency",
		min=0,max=4,value=0,step=1,ticks=TRUE),
		radioButtons("networkPresentation","Which Graph-drawing algorithm would you like to use?",
				c("Kamada & Kawai"="kk",
				  "Fruchterman & Reingold"="fr")),
		br(),
		actionButton("generateNetwork","Generate Words Network"),
		br(),
		downloadButton("downloadNetwork","Download Words Network")
			)
		)				
	),


##================================= Select settings for mainPanel ================================================##

mainPanel(
	tabsetPanel(id="tabset1",
		tabPanel(title="Introduction",includeHTML("introduction.html")),
		tabPanel(title="Corpus Generation",verbatimTextOutput("corpusStatus"),verbatimTextOutput("procCorpusStatus"),verbatimTextOutput("initialuniMatrix"),verbatimTextOutput("finaluniMatrix")),
		tabPanel(title="Initial Analysis",plotOutput("rankFreqPlot",width="auto"), plotOutput("wordFreqPlot",width="auto")),
		tabPanel(title="Clustering Words", plotOutput("assocCloud",width="auto"), plotOutput("dendrogram", width="auto")), #plotOutput("phraseCloud",width="auto")),
		tabPanel(title="Clustering Documents", plotOutput("docClusters",width="auto")),
		tabPanel(title="Word Networks", plotOutput("wordNetwork",width="auto")),
		tabPanel(title="About",includeHTML("about.html"))
			)
		)
))