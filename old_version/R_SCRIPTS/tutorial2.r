# R scripts used to analyse data from Tutorial 2: Neutral genetics

##
## Section 2.1
# The mutation simulation. 

	# NB Run this script twice: once for the KAM (ntrl_mutation_model 0), and once for SSM (ntrl_mutation_model 1)
	
	# First read the stats data (Please select the simulation_stats.txt)
	sim_stat<-read.table(myfile<-file.choose(),h=T)
	
	# Plotting number of alleles and heterozygosity over time
	par(mfrow=c(1,2))
	plot(sim_stat$generation, sim_stat$n.adlt.nbAll, type='l', xlab="generation", ylab="nb. of alleles")
	plot(sim_stat$generation, sim_stat$n.adlt.ho, type="l", xlab="generation", ylab="observed heterozygosity", ylim=c(0,1))
	
	
	# Retrieving the genotypes of all individuals (goes automatic, no need to select files):
	setwd(substr(myfile,1,nchar(myfile)-21)); getwd()
	# - after 500 generations
	gen1000<-read.table("simulation_g1000.dat",h=F, skip=2, colClasses="character")
	all1000<-as.numeric(c(substr(gen1000[,2],1,2),substr(gen1000[,2],3,4)))
	# - after 1000 generations 
	gen2000<-read.table("simulation_g2000.dat",h=F, skip=2, colClasses="character")
	all2000<-as.numeric(c(substr(gen2000[,2],1,2),substr(gen2000[,2],3,4)))
	# - after 1500 generations
	gen3000<-read.table("simulation_g3000.dat",h=F, skip=2, colClasses="character")
	all3000<-as.numeric(c(substr(gen3000[,2],1,2),substr(gen3000[,2],3,4)))
	# - after 2000 generations
	gen4000<-read.table("simulation_g4000.dat",h=F, skip=2, colClasses="character")
	all4000<-as.numeric(c(substr(gen4000[,2],1,2),substr(gen4000[,2],3,4)))
	
	# Plotting the genotype frequency data
	par(mfrow=c(1,4), cex=0.7)
	barplot(table(all1000), main="Allele frequencies at gen. 1000")
	barplot(table(all2000), main="Allele frequencies at gen. 2000")
	barplot(table(all3000), main="Allele frequencies at gen. 3000")
	barplot(table(all4000), main="Allele frequencies at gen. 4000")

##
## Section 2.2
# Migration and the breakdown of differentiation. 

	# Section to select files and read tables
	# Each time select the xxx_mean.txt file with... 
	df.m.1<-read.table(file.choose(),header=TRUE)	# ...dispersal_rate 0.1 (index 1)
	df.m.05<-read.table(file.choose(),header=TRUE)	# ...dispersal_rate 0.05 (index 2)
	df.m.01<-read.table(file.choose(),header=TRUE)	# ...dispersal_rate 0.01 (index 3)
	df.m.005<-read.table(file.choose(),header=TRUE)	# ...dispersal_rate 0.005 (index 4)
	df.m.001<-read.table(file.choose(),header=TRUE)	# ...dispersal_rate 0.001 (index 5)
	
	# Combine these tables in a single list
	df.m<-list("m.1"= df.m.1, "m.05"=df.m.05, "m.01"=df.m.01, "m.005"=df.m.005, "m.001"=df.m.001 )
	#for(i in 1:length(df.m)) write.table(df.m[[i]],file=paste(names(df.m[i]),".txt", sep="")) # save these files elsewhere
	
	# Adjust the plotting area
	par(mfcol=c(2,5), mar=c(0,0,4,2.5)+0.2, oma=c(5,5,0,0)+0.2)
	# The dispersal rates (used for naming the plots)
	rates<-c(0.1,0.05,0.01,0.005,0.001)
	# Plotting number of alleles and the Fst-values for each simulation using a for-loop
	for(i in 1:5) {
		plot(df.m[[i]]$generation, df.m[[i]]$n.adlt.nbAll, ylim=c(0,10), main=paste("dispersal_rate",rates[i]), xlab="", ylab="")
		plot(df.m[[i]]$generation, df.m[[i]]$n.adlt.fst, ylim=c(0,1), type="l", lwd=2, xlab="", ylab="")
		lines(df.m[[i]]$generation, df.m[[i]]$n.adlt.fst_l1, lty=2)
		lines(df.m[[i]]$generation, df.m[[i]]$n.adlt.fst_l2, lty=3)
		}
	# Making the plotting area pretty
	legend("topright", c("Overal Fst","Fst locus 1", "Fst locus 2"), lty=c(1,2,3), lwd=c(2,1,1) )
	mtext("Time in generations", side=1, line=3,outer=TRUE, font=2)
	mtext("Fst",side=2,line=3,outer=TRUE, font=2, at=0.2)
	mtext("Nb. of alleles",side=2,line=3,outer=TRUE, font=2, at=0.7)
	


	
##
## Section 2.3  -> Use FSTAT 2.9.4
	

