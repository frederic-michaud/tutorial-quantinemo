# Tutorial 3 R scripts

##
## Section 3.1
# Selection basics

	# First read the stats data of both simulations (Please select the xxx_stats.txt)
	# a) high selection intensity (low omega)
	omegaA<-5
	stat_A<-read.table(myfileA<-file.choose(),h=T)
	# b) low selection intenstiy (high omega)
	omegaB<-20
	stat_B<-read.table(myfileB<-file.choose(),h=T)
	
	# Plotting number of alleles and heterozygosity over time
	par(mfrow=c(2,3))
	plot(stat_A$generation, stat_A$q.adlt.nbAll, type='l', xlab="generation", ylab="nb. of alleles", ylim=c(150,255), main=paste("High selection intensity (",omegaA,")",sep=""))
	plot(stat_A$generation, stat_A$q.adlt.ho, type="l", xlab="generation", ylab="observed heterozygosity", ylim=c(0.95,1), main=paste("High selection intensity (",omegaA,")",sep=""))
	plot(stat_A$generation, stat_A$q.meanG, type="l", xlab="generation", ylab="mean genotypic value", ylim=c(-1,1), main=paste("High selection intensity (",omegaA,")",sep=""))
	
	plot(stat_B$generation, stat_B$q.adlt.nbAll, type='l', xlab="generation", ylab="nb. of alleles", ylim=c(150,255), main=paste("Low selection intensity (",omegaB,")",sep=""))
	plot(stat_B$generation, stat_B$q.adlt.ho, type="l", xlab="generation", ylab="observed heterozygosity", ylim=c(0.95,1), main=paste("Low selection intensity (",omegaB,")",sep=""))
	plot(stat_B$generation, stat_B$q.meanG, type="l", xlab="generation", ylab="mean genotypic value", ylim=c(-1,1), main=paste("Low selection intensity (",omegaB,")",sep=""))
		
	# Retrieving the file paths (so we can automatically read all genotype files) 
	prefileA<-substr(myfileA,1,nchar(myfileA)-10)
	prefileB<-substr(myfileB,1,nchar(myfileB)-10)
	
	nb.gen<-length(stat_A$generation)	# number of generations the simulation ran
	gen.dig<-nchar(nb.gen)
	nb.al<-stat_A$q.adlt.nbAll[1]		# number of alleles in the simulation 
	al.length<-nchar(255)				# number of digits of each allele
		
	# Creating matrices to store all the allele and trait data in per generation
	traitsA<-matrix(ncol=nb.gen, nrow=10000)
	traitsB<-matrix(ncol=nb.gen, nrow=10000)
	allelesA<-matrix(ncol=nb.gen, nrow=10000*2)
	allelesB<-matrix(ncol=nb.gen, nrow=10000*2)
	
	# Reading the genotype and genotypic value files (goes automatic, no need to select files by hand):
	for(gen in 1:nb.gen){
		# genotypic value files
		tabA<-read.table(paste(prefileA,"_g",formatC(gen,width=gen.dig,flag=0),".gen", sep=""),h=F, skip=2)
		traitsA[,gen]<-tabA[,2]
		tabB<-read.table(paste(prefileB,"_g",formatC(gen,width=gen.dig,flag=0),".gen", sep=""),h=F, skip=2)
		traitsB[,gen]<-tabB[,2]
		# genotype files
		tabA<-read.table(paste(prefileA,"_g",formatC(gen,width=gen.dig,flag=0),".dat", sep=""),h=F, skip=2, colClasses="character")
		allelesA[,gen]<-as.numeric(c(substr(tabA[,2],1,al.length),substr(tabA[,2],al.length+1,al.length*2)))
		tabB<-read.table(paste(prefileB,"_g",formatC(gen,width=gen.dig,flag=0),".dat", sep=""),h=F, skip=2, colClasses="character")
		allelesB[,gen]<-as.numeric(c(substr(tabB[,2],1,al.length),substr(tabB[,2],al.length+1,al.length*2)))
	}

	# Plotting the genotypic trait frequency data
	par(mfcol=c(2,nb.gen/2), cex=0.7, oma=c(0,3,0,0))
	odd.generations<-1:(nb.gen/2)*2-1
	for(gen in odd.generations) {
		hist(traitsA[,gen], xlim=c(-25,25), breaks=10, xlab="trait value", main=paste("generation",gen))
		hist(traitsB[,gen], xlim=c(-25,25), breaks=10, xlab="trait value", main=paste("generation",gen))
	}
	mtext(paste("Low selection intensity (",omegaB,")       |       High selection intensity (",omegaA,")",sep=""),side=2,line=1,outer=TRUE)

	
	# Plotting the genotype frequency data
	par(mfcol=c(2,nb.gen/2), cex=0.7, oma=c(0,3,0,0))
	odd.generations<-1:(nb.gen/2)*2-1
	for(gen in odd.generations) {
		barplot(table(allelesA[,gen]), main=paste("Allele frequencies at gen",gen), ylab="frequency", xlab="allele index")
		barplot(table(allelesB[,gen]), main=paste("Allele frequencies at gen",gen), ylab="frequency", xlab="allele index")
	}
	mtext(paste("Low selection intensity (",omegaB,")       |       High selection intensity (",omegaA,")",sep=""),side=2,line=1,outer=TRUE)


##
## Section 3.2
# The Selection-mutation-drift equilibrium
	
	# Read the files into R
	
	# Select the xxx_mean.txt file from the simulation with selection (the corresponding variance file will be automatically read as well)
	sel.mean<-read.table( spath<-file.choose(),h=T); sel.var<-read.table(paste(substr(spath,1,nchar(spath)-9),"_var.txt",sep=""),h=T)
	
	# Select the xxx_mean.txt file from the simulation withOUT selection (the corresponding variance file will be automatically read as well)
	drft.mean<-read.table( dpath<-file.choose(),h=T); drft.var<-read.table(paste(substr(dpath,1,nchar(dpath)-9),"_var.txt",sep=""),h=T)
	
	### Allelic variance
	
	par(mfrow=c(1,4))
	plot(sel.mean$q.adlt.nbAll ~ sel.mean$generation, type="l", ylim=c(0,250),lwd=2, main="Nb. of alleles", xlab="generations", ylab="nb. of alleles")
	lines(sel.mean$q.adlt.nbAll+sqrt(sel.var$q.adlt.nbAll) ~ sel.mean$generation, col="grey")
	lines(sel.mean$q.adlt.nbAll-sqrt(sel.var$q.adlt.nbAll)~ sel.mean$generation, col="grey")
	
	lines(drft.mean$q.adlt.nbAll ~ drft.mean$generation, type="l", ylim=c(0,250),col="firebrick", lwd=2)
	lines(drft.mean$q.adlt.nbAll+sqrt(drft.var$q.adlt.nbAll)~ drft.mean$generation, col="firebrick4")
	lines(drft.mean$q.adlt.nbAll-sqrt(drft.var$q.adlt.nbAll)~ drft.mean$generation, col="firebrick4")
	
	legend("topright", c("stabilising selection", "ss +- 1 stdev.", "no selection", "ns +- 1 stdev"),lty=c(1,1,1,1), lwd=c(2,1,2,1),col=c("black","grey","firebrick","firebrick4"))
	
	### Genetic diversity (expected heterozygosity)
	
	plot(sel.mean$q.adlt.hs ~ sel.mean$generation, type="l", ylim=c(0,1),lwd=2, main="Genetic diversity", xlab="generations", ylab="expected heterozygosity")
	lines(sel.mean$q.adlt.hs+sqrt(sel.var$q.adlt.hs) ~ sel.mean$generation, col="grey")
	lines(sel.mean$q.adlt.hs-sqrt(sel.var$q.adlt.hs)~ sel.mean$generation, col="grey")
	
	lines(drft.mean$q.adlt.hs ~ drft.mean$generation, type="l", ylim=c(0,1),col="firebrick", lwd=2)
	lines(drft.mean$q.adlt.hs+sqrt(drft.var$q.adlt.hs)~ drft.mean$generation, col="firebrick4")
	lines(drft.mean$q.adlt.hs-sqrt(drft.var$q.adlt.hs)~ drft.mean$generation, col="firebrick4")
	
	## Genotypic mean
	
	plot(sel.mean$q.meanG ~ sel.mean$generation, type="l", ylim=c(-5,5),lwd=2, col="black", main="Genotypic mean", xlab="generations", ylab="mean genotypic value")
	lines(sel.mean$q.meanG+sqrt(sel.var$q.meanG)~ sel.mean$generation, col="grey")
	lines(sel.mean$q.meanG-sqrt(sel.var$q.meanG)~ sel.mean$generation, col="grey")
	
	
	lines(drft.mean$q.meanG ~ drft.mean$generation, type="l", ylim=c(-5,5),lwd=2, col="firebrick")
	lines(drft.mean$q.meanG+sqrt(drft.var$q.meanG) ~ drft.mean$generation, col="firebrick4")
	lines(drft.mean$q.meanG-sqrt(drft.var$q.meanG)~ drft.mean$generation, col="firebrick4")
	
	### Genotypic variance
	
	plot(sel.mean$q.varG ~ sel.mean$generation, type="l", ylim=c(0,100),lwd=2, main="Genotypic variance", xlab="generations", ylab="genotypic variance")
	lines(sel.mean$q.varG+sqrt(sel.var$q.varG)~ sel.mean$generation, col="grey")
	lines(sel.mean$q.varG-sqrt(sel.var$q.varG)~ sel.mean$generation, col="grey")
	
	
	lines(drft.mean$q.varG ~ drft.mean$generation, type="l", ylim=c(0,100),lwd=2, col="firebrick")
	lines(drft.mean$q.varG+sqrt(drft.var$q.varG) ~ drft.mean$generation, col="firebrick4")
	lines(drft.mean$q.varG-sqrt(drft.var$q.varG)~ drft.mean$generation, col="firebrick4")
	

##
## Section 3.3
# 
	## PART A, the crossing table
	
	# Read the phenotypic file of generation 2 into R
	phenoMice<-read.table(file.choose(),skip=2, header=FALSE)
	# Plot the three possible phenotypes with their frequencies
	my.plot<-barplot(table(phenoMice[,2]),xlab="trait value", ylab="Frequency",col=c("white","brown","black"))
	text(my.plot, table(phenoMice[,2])-500, table(phenoMice[,2]), col=c("black","black","white"))
	
	## PART B, selection against the brown phenotype
	
	# Read the statistics means file
	mean.stat<-read.table(file.choose(), header=TRUE)
	# Read the statistics variance file
	var.stat<-read.table(file.choose(), header=TRUE)
	
	par(mfrow=c(1,2))
	# Plot the mean allele frequencies of locus 1: Bb
	plot(mean.stat$generation ,mean.stat$q.adlt.a.freq_l1_a1, type='l', main="locus Bb", xlab="Generation", ylab= "Allele frequency", ylim=c(-0.1,1.1))
	lines(mean.stat$generation ,mean.stat$q.adlt.a.freq_l1_a2, col="brown")
		# +- standard deviation
		lines(mean.stat$generation ,I(mean.stat$q.adlt.a.freq_l1_a1 - sqrt(var.stat$q.adlt.a.freq_l1_a1)) , col="black", lty=3)
		lines(mean.stat$generation ,I(mean.stat$q.adlt.a.freq_l1_a1 + sqrt(var.stat$q.adlt.a.freq_l1_a1)) , col="black", lty=3)
		lines(mean.stat$generation ,I(mean.stat$q.adlt.a.freq_l1_a2 - sqrt(var.stat$q.adlt.a.freq_l1_a2)) , col="brown", lty=3)
		lines(mean.stat$generation ,I(mean.stat$q.adlt.a.freq_l1_a2 + sqrt(var.stat$q.adlt.a.freq_l1_a2)) , col="brown", lty=3)
	
	# Plot the mean allele frequencies of locus 2: Cc
	plot(mean.stat$generation ,mean.stat$q.adlt.a.freq_l2_a1, , type='l', main="locus Cc", xlab="Generation", ylab= "Allele frequency", ylim=c(-0.1,1.1), col="blue")
	lines(mean.stat$generation ,mean.stat$q.adlt.a.freq_l2_a2, col="grey")
		# Plot the standard deviation 
		lines(mean.stat$generation ,I(mean.stat$q.adlt.a.freq_l2_a1 - sqrt(var.stat$q.adlt.a.freq_l2_a1)) , col="blue", lty=3)
		lines(mean.stat$generation ,I(mean.stat$q.adlt.a.freq_l2_a1 + sqrt(var.stat$q.adlt.a.freq_l2_a1)) , col="blue", lty=3)
		lines(mean.stat$generation ,I(mean.stat$q.adlt.a.freq_l2_a2 - sqrt(var.stat$q.adlt.a.freq_l2_a2)) , col="grey", lty=3)
		lines(mean.stat$generation ,I(mean.stat$q.adlt.a.freq_l2_a2 + sqrt(var.stat$q.adlt.a.freq_l2_a2)) , col="grey", lty=3)
