#Forest Plots for Ceara Cunningham
#Load Packace with Meta Analysis Functions (This package needs to be installed on machine with add.package(meta) prior to calling.
setwd('~/Documents/CRU/') #Note on all OS's but windows \ is an escape character.  You either need to use 2 or use a forward slash in paths.
require(meta) #Make the meta analysis functions load and become available.
	
#Load data to variables. (We can load data from a variety of sources in this case csv.

	plotData.mail <- read.csv(file="mail.csv")
	

#Look over datasets to see that they read in properly.

	plotData.mail
	

		
	
#PERFORM META ANALYSIS (LOGIT TRANSFORMED META ANALYSIS OF PROPORTIONS USING A RANDOM EFFECTS MODEL FOR POOLED ESTIMATE)
	
#Perfom Meta Analysis of Proportions using metaprop function. 
#By Modality
	
	ma.mail = metaprop( #results of meta analysis have been set to variable vml
	plotData.mail$events, #reference to column of event counts.
	plotData.mail$sampleSize, #reference to column of sample size counts.
	sm="PLOGIT", #tag to utilize Logit transformation of proportions in summary method.
	studlab=plotData.mail$study,#reference to column of study labels
	comb.random=TRUE, #tag to utilize random effects model in summary
	comb.fixed=FALSE, #tag to disable fixed effects model in summary
	warn=TRUE
	)
	

#Take a look at your meta analysis results prior to plotting them.
	summary(ma.mail)



#Plot your results
#By Modality	
	quartz(title="Mail")
	forest( #Notice that the plot isn't set to a variable.  
	ma.mail, #The dataset (which in the forest functions case is a metaprop object.)
	studlab=as.vector(t(plotData.mail[1])), #The labels for each study.
	ref=1, #location of reference line. (Doesn't seem to work properly in analysis of proportions plots.
	overall=TRUE, #Control the printing of an overall total (across groups)
	comb.random=TRUE, #Print the random effects totals.
	comb.fixed=FALSE, #Supress printing of the fixed effects totals.
	text.random="Pooled Estimate", #How do you want to label the random effects total.
	pooled.totals=TRUE, #Should we print the total number of observations?
	hetstat=FALSE, #Supress printing of heterogeinity statistics.
	xlim=c(-0.01,100), #Range for the x axis.
	col.diamond="#A10E38", #Color of the diamonds.
	col.diamond.lines="#A10E38", #color of the line surrounding the diamonds.
	col.square="#FFDF63", #color of the squares.
	col.i.inside.square="#000000", #color of the CI lines if they don't go beyond square boundaries.
	col.by="#000000", #Color of the by label.
	squaresize=0.5, #A scaling factor to mulitply the square size by (all squares)
	addspace=TRUE, #Would you like extra space added between the squares and diamonds
	leftcols=c("studlab","effect","ci"), #Names of columns for the left hand side of the plot.
	leftlabs=c("Study","Event\nRate","95% C.I."), #Labels for these columns
	rightcols=FALSE, #Supress right hand columns.
	fontsize=8, #Font size.
	colgap.forest=unit(0.2,"cm"), #You can control the space between the columns and the plot.
	plotwidth=unit(4,"cm"), #You can control the plot width.
	pscale=100 #Scale up proportions to percentages.
	)

