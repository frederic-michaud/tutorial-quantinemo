# R script for Tutorial 1

##
## Section 1.2

	# Select the simulation_stats.txt file from Demo1 (the fecundity simulation)
	demo1<-read.table(file.choose(), header=T)
	# Select the simulation_stats.txt file from Demo2 (the logistic growth simulation)
	demo2<-read.table(file.choose(), header=T)
	# plotting the data
	plot(demo1$generation,demo1$adlt.nbInd, xlim=c(0,20), xlab="generations",ylab="nb. of individuals", type='l', lty=2, lwd=2)
	lines(demo2$generation,demo2$adlt.nbInd, pch=2, lty=3, lwd=2)
	legend("topleft", c("Fecundity", "Logistic growth"), lty=c(2,3), lwd=2, cex=0.8, title="mating_nb_offspring_model")

##
## Section 1.3

	# Select the simulation_stats.txt file from the colonization simulation 
	colony<-read.table(file.choose(), header=T)
	# plotting the population sizes
	par(mar=c(5,4,1,4)+0.3)
	plot(colony$generation, colony$adlt.nbInd_p1, pch='1', cex=0.7, col="grey40", xlab="time in generations", ylab="nb. of individuals")
	for(i in 2:6) points(colony$generation, colony[,5+i], pch=as.character(i), cex=0.7, col="grey40")
	# plotting the number of migrants and the mean fecundity
	par(new = TRUE)
	plot(colony$generation, colony$emigrants, type = "l", lwd=1, ylim=c(0,10), axes = FALSE, bty = "n", xlab = "", ylab = "")
	axis(side=4)
	lines(colony$generation, colony$fem.meanFec, lty=2, lwd=1)
	mtext("nb. of offspring or migrants", side=4, line=3)
	# make a legend here!
	legend("topright", c("nb. of migrants/patch", "nb. of offspring/female", "nb individuals in patch 1"), bg="white", cex=0.8, lty=c(1,2,NA), pch=c(NA,NA,"1"))


##
## Section 1.4

	# Select the simulation_stats.txt file from the extinction simulation
	extinct<-read.table(file.choose(), header=T)
	# extract the data of replicate 1 and 2
	rep1<-extinct[extinct$replicate==1,]
	rep2<-extinct[extinct$replicate==2,]
	
		# define a function to plot the data (saves typing)
		plot.extinction<-function(mat, tit=""){
			par(mar=c(5,4,3,4)+0.1)
			total.migrants<-mat$emigrants * mat$adlt.nbPops
			plot(mat$generation, total.migrants, type="l", main=tit,lwd=1, col="grey", axes=FALSE, xlab="", ylab="")
			axis(4)
			par(new=TRUE)
			plot(mat$generation,mat$adlt.nbInd_p1,main=tit, type="l",lty=1, col="grey40", xlab="time in generations", ylab="nb. of individuals")
			for(i in 2:6) lines(mat$generation, mat[,6+i], lty=i, col="grey40")
		
			#plot(mat$generation,mat$colonisers, axes=FALSE, xlab="", ylab="", ylim=c(0,10))
			#points(mat$generation,mat$emigrants,pch=4)
			mtext("total nb. of emigrants",side=4, line=3)
			#for(i in 2:6) points(mat$generation, mat[,5+i], cex=0.7, pch=as.character(i), lty=1+i, col="grey40")
		}
	
	# plot the data of replicate 1 and 2
	par(mfrow=c(2,1))
	plot.extinction(rep1, tit="First replicate")
	legend("topright", paste("patch",1:6), lty=1:6)
	plot.extinction(rep2, tit="Second replicate")
	
	# select the simulation_mean.txt file
	mean.ext<-read.table(file.choose(), h=T)
	# plotting the number of inhabited patches and alive replicates across time 
	par(mar=c(5,4,1,4)+0.1, mfrow=c(1,1))
	plot(mean.ext$generation, mean.ext$alive.rpl,ylim=c(0,20), type="l",lty=1, lwd=2,col="grey", xlab="time in generations", ylab="", axes=FALSE)
	axis(4)
	mtext("alive replicates", side=4, line=3)
	par(new=TRUE)
	plot(mean.ext$generation, mean.ext$adlt.nbPops, type="l", lty=1, lwd=1, xlab="", ylab="inhabited patches")
	
	# select the simulation_var.txt file
	var.ext<-read.table(file.choose(), h=T)
	# plotting 
	plot(mean.ext$generation, mean.ext$adlt.nbInd, ylim=c(0,3000),type="l",lty=1, lwd=2, xlab="time in generations", ylab="population size")
	lines(mean.ext$generation, I(mean.ext$adlt.nbInd+sqrt(var.ext$adlt.nbInd)))
	lines(mean.ext$generation, I(mean.ext$adlt.nbInd-sqrt(var.ext$adlt.nbInd)))
