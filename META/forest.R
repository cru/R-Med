#Forest Plots for Ceara Cunningham
#Load Packace with Meta Analysis Functions (This package needs to be installed on machine with add.package(meta) prior to calling.
setwd('~/Documents/CRU/JetteCunningham2011/') #Note on all OS's but windows \ is an escape character.  You either need to use 2 or use a forward slash in paths.
require(meta) #Make the meta analysis functions load and become available.
	
#Load data to variables. (We can load data from a variety of sources in this case csv.

	plotData.mail <- read.csv(file="mail.csv")
	plotData.combined <- read.csv(file="combined.csv")
	plotData.online <- read.csv(file="online.csv")
	plotData.telephone <- read.csv(file="telephone.csv")
	
	plotData.emergPhys <- read.csv(file="emergPhys.csv")
	plotData.internalMed <- read.csv(file="internalMed.csv")
	plotData.neurologists <- read.csv(file="neurologists.csv")
	plotData.pediatricians <- read.csv(file="pediatricians.csv")
	plotData.surgery <- read.csv(file="surgery.csv")
	
	plotData.modality <- read.csv(file="modality.csv")
	plotData.specialty <- read.csv(file="specialty.csv")
	

#Look over datasets to see that they read in properly.

	plotData.mail
	plotData.combined
	plotData.online
	plotData.telephone

	plotData.emergPhys
	plotData.internalMed
	plotData.neurologists
	plotData.pediatricians
	plotData.surgery

	plotData.modality
	plotData.specialty
	

		
	
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
	
	ma.combined = metaprop( #results of meta analysis have been set to variable vml
	plotData.combined$events, #reference to column of event counts.
	plotData.combined$sampleSize, #reference to column of sample size counts.
	sm="PLOGIT", #tag to utilize Logit transformation of proportions in summary method.
	studlab=plotData.combined$study,#reference to column of study labels
	comb.random=TRUE, #tag to utilize random effects model in summary
	comb.fixed=FALSE, #tag to disable fixed effects model in summary
	warn=TRUE
	)

	ma.online = metaprop( #results of meta analysis have been set to variable vml
	plotData.online$events, #reference to column of event counts.
	plotData.online$sampleSize, #reference to column of sample size counts.
	sm="PLOGIT", #tag to utilize Logit transformation of proportions in summary method.
	studlab=plotData.online$study,#reference to column of study labels
	comb.random=TRUE, #tag to utilize random effects model in summary
	comb.fixed=FALSE, #tag to disable fixed effects model in summary
	warn=TRUE
	)

	ma.telephone = metaprop( #results of meta analysis have been set to variable vml
	plotData.telephone$events, #reference to column of event counts.
	plotData.telephone$sampleSize, #reference to column of sample size counts.
	sm="PLOGIT", #tag to utilize Logit transformation of proportions in summary method.
	studlab=plotData.telephone$study,#reference to column of study labels
	comb.random=TRUE, #tag to utilize random effects model in summary
	comb.fixed=FALSE, #tag to disable fixed effects model in summary
	warn=TRUE
	)
	
#By Specialty Group

	ma.emergPhys = metaprop( #results of meta analysis have been set to variable vml
	plotData.emergPhys$events, #reference to column of event counts.
	plotData.emergPhys$sampleSize, #reference to column of sample size counts.
	sm="PLOGIT", #tag to utilize Logit transformation of proportions in summary method.
	studlab=plotData.emergPhys$study,#reference to column of study labels
	comb.random=TRUE, #tag to utilize random effects model in summary
	comb.fixed=FALSE, #tag to disable fixed effects model in summary
	warn=TRUE
	)
	ma.internalMed = metaprop( #results of meta analysis have been set to variable vml
	plotData.internalMed$events, #reference to column of event counts.
	plotData.internalMed$sampleSize, #reference to column of sample size counts.
	sm="PLOGIT", #tag to utilize Logit transformation of proportions in summary method.
	studlab=plotData.internalMed$study,#reference to column of study labels
	comb.random=TRUE, #tag to utilize random effects model in summary
	comb.fixed=FALSE, #tag to disable fixed effects model in summary
	warn=TRUE
	)
	ma.neurologists = metaprop( #results of meta analysis have been set to variable vml
	plotData.neurologists$events, #reference to column of event counts.
	plotData.neurologists$sampleSize, #reference to column of sample size counts.
	sm="PLOGIT", #tag to utilize Logit transformation of proportions in summary method.
	studlab=plotData.neurologists$study,#reference to column of study labels
	comb.random=TRUE, #tag to utilize random effects model in summary
	comb.fixed=FALSE, #tag to disable fixed effects model in summary
	warn=TRUE
	)
	ma.pediatricians = metaprop( #results of meta analysis have been set to variable vml
	plotData.pediatricians$events, #reference to column of event counts.
	plotData.pediatricians$sampleSize, #reference to column of sample size counts.
	sm="PLOGIT", #tag to utilize Logit transformation of proportions in summary method.
	studlab=plotData.pediatricians$study,#reference to column of study labels
	comb.random=TRUE, #tag to utilize random effects model in summary
	comb.fixed=FALSE, #tag to disable fixed effects model in summary
	warn=TRUE
	)
	ma.surgery = metaprop( #results of meta analysis have been set to variable vml
	plotData.surgery$events, #reference to column of event counts.
	plotData.surgery$sampleSize, #reference to column of sample size counts.
	sm="PLOGIT", #tag to utilize Logit transformation of proportions in summary method.
	studlab=plotData.surgery$study,#reference to column of study labels
	comb.random=TRUE, #tag to utilize random effects model in summary
	comb.fixed=FALSE, #tag to disable fixed effects model in summary
	warn=TRUE
	)


#Overall Modality Groups then Specialty Groups.

	ma.modality = metaprop( #results of meta analysis have been set to variable vml
	plotData.modality$events, #reference to column of event counts.
	plotData.modality$sampleSize, #reference to column of sample size counts.
	sm="PLOGIT", #tag to utilize Logit transformation of proportions in summary method.
	studlab=plotData.modality$study,#reference to column of study labels
	comb.random=TRUE, #tag to utilize random effects model in summary
	comb.fixed=FALSE, #tag to disable fixed effects model in summary
	warn=TRUE,
	byvar=plotData.modality$outcome
	)

	ma.specialty = metaprop( #results of meta analysis have been set to variable vml
	plotData.specialty$events, #reference to column of event counts.
	plotData.specialty$sampleSize, #reference to column of sample size counts.
	sm="PLOGIT", #tag to utilize Logit transformation of proportions in summary method.
	studlab=plotData.specialty$study,#reference to column of study labels
	comb.random=TRUE, #tag to utilize random effects model in summary
	comb.fixed=FALSE, #tag to disable fixed effects model in summary
	warn=TRUE,
	byvar=plotData.specialty$outcome2
	)	


#Take a look at your meta analysis results prior to plotting them.
	summary(ma.mail)
	summary(ma.combined)
	summary(ma.online)
	summary(ma.telephone)

	summary(ma.emergPhys)
	summary(ma.internalMed)
	summary(ma.neurologists)
	summary(ma.pediatricians)
	summary(ma.surgery)

	summary(ma.modality)
	summary(ma.specialty)




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

	quartz(title="Combined")
	forest( #Notice that the plot isn't set to a variable.  
	ma.combined, #The dataset (which in the forest functions case is a metaprop object.)
	studlab=as.vector(t(plotData.combined[1])), #The labels for each study.
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

	quartz(title="Online")
	forest( #Notice that the plot isn't set to a variable.  
	ma.online, #The dataset (which in the forest functions case is a metaprop object.)
	studlab=as.vector(t(plotData.online[1])), #The labels for each study.
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

	quartz(title="Telephone")
	forest( #Notice that the plot isn't set to a variable.  
	ma.telephone, #The dataset (which in the forest functions case is a metaprop object.)
	studlab=as.vector(t(plotData.telephone[1])), #The labels for each study.
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
	
	
	
#By Specialty	
	quartz(title="Emergency Physicians")
	forest( #Notice that the plot isn't set to a variable.  
	ma.emergPhys, #The dataset (which in the forest functions case is a metaprop object.)
	studlab=as.vector(t(plotData.emergPhys[1])), #The labels for each study.
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

	quartz(title="Internal Medicine")
	forest( #Notice that the plot isn't set to a variable.  
	ma.internalMed, #The dataset (which in the forest functions case is a metaprop object.)
	studlab=as.vector(t(plotData.internalMed[1])), #The labels for each study.
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

	quartz(title="Neurologists")
	forest( #Notice that the plot isn't set to a variable.  
	ma.neurologists, #The dataset (which in the forest functions case is a metaprop object.)
	studlab=as.vector(t(plotData.neurologists[1])), #The labels for each study.
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

	quartz(title="Pediatricians")
	forest( #Notice that the plot isn't set to a variable.  
	ma.pediatricians, #The dataset (which in the forest functions case is a metaprop object.)
	studlab=as.vector(t(plotData.pediatricians[1])), #The labels for each study.
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
	
	quartz(title="Surgery")
	forest( #Notice that the plot isn't set to a variable.  
	ma.surgery, #The dataset (which in the forest functions case is a metaprop object.)
	studlab=as.vector(t(plotData.surgery[1])), #The labels for each study.
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
	
	
#Overall Modality and Specialty

	quartz(title="Modality")
	forest( #Notice that the plot isn't set to a variable.  
	ma.modality, #The dataset (which in the forest functions case is a metaprop object.)
	studlab=as.vector(t(plotData.modality[1])), #The labels for each study.
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
	pscale=100, #Scale up proportions to percentages.
	print.byvar=FALSE
	)
	
	quartz(title="Specialty")
	forest( #Notice that the plot isn't set to a variable.  
	ma.specialty, #The dataset (which in the forest functions case is a metaprop object.)
	studlab=as.vector(t(plotData.specialty[1])), #The labels for each study.
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
	pscale=100, #Scale up proportions to percentages.
	print.byvar=FALSE
	)
	
 