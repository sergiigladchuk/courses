
# exercise written 14 August 2006 by Anders Brodin translated into R by John Waller 2016

GA = function() {
  
# variables:
POPSIZE = 50                   # popsize is number of chromosomes in the population
GENE_NUMBER = 8                # gene number is number of genes in each chromosome, should be one in these simple tasks
MAX_GENERATIONS = 50           # number of iterations
NUM_BREED = 20              # out of the 50 chromsomes (= possible solutions) the 20 best are allowed to reproduce

# genetic operators
mutation_rate = 0.04           # how large percentage of the genes experience mutations
mutation_free = 10             # number of topranked Individuals that should be saved from mutation


# Define Functions

Create_pop = function(POPSIZE,GENE_NUMBER) {                               # This function creates a random population of 50 chromosomes
# global POPSIZE GENE_NUMBER Ind;% Gene Fitness;

IndL = list() 										# create an empty list to put on Inds. A list of inidviduals = a population :)
for(Count1 in 1:POPSIZE) {                          # For counter for the chromosomes
    
	Gene = c()
	for(Count2 in 1:GENE_NUMBER) {                   # For counter for genes inside chromosomes
        
		g = runif(1,0,1)                                # g is a random number between 0 and 1
        
		if(g < 0.5) {                                  # fill genes with either 0 or 1
            Gene[Count2] = 0           # 0 or 1 is written into the array "Ind" which is an
        } else {                                         # array of a user defined variable (like struct or class)
            Gene[Count2] = 1           # the "Gene" is and integer inside the composite variable
        }

		}

IndL[[Count1]] = list(Gene = Gene, Fitness = -1)

    }

return(IndL) 
}

Evaluate_fitness = function(IndL,POPSIZE,GENE_NUMBER) {

  for(Count1 in 1:POPSIZE) {
     Added_Value = 0;
     for(Count2 in GENE_NUMBER:1) {
         X = 2^(GENE_NUMBER - Count2)
         if(IndL[[Count1]]$Gene[Count2] == 1) {
             Added_Value = Added_Value + X
         }
     }
     IndL[[Count1]]$Fitness = Added_Value
  }
  
  return(IndL)
}

Sort = function(IndL) {

IndL = IndL[rev(order(sapply(IndL,function(x) x$Fitness)))] # sort the list by fitness

return(IndL)
}

Reproduce = function(IndL,POPSIZE,NUM_BREED) {

  countback = POPSIZE
  for(Count1 in seq(1,POPSIZE/2,2)) {
      if(Count1 <= NUM_BREED) {
          Gene_No = 8*runif(1,0,1) + 1
          for(Count2 in 1:GENE_NUMBER) {
              if(Count2 <= Gene_No) {
                  IndL[[countback]]$Gene[Count2] = IndL[[Count1]]$Gene[Count2]
                  IndL[[countback - 1]]$Gene[Count2] = IndL[[Count1 + 1]]$Gene[Count2]
              } else {
                  IndL[[countback]]$Gene[Count2] = IndL[[Count1 + 1]]$Gene[Count2]
                  IndL[[countback - 1]]$Gene[Count2] = IndL[[Count1]]$Gene[Count2]
              }
          }
          countback = countback - 2
      }
  }
  
  return(IndL)
}

Mutate = function(IndL,POPSIZE,NUM_BREED,mutation_free,mutation_rate) {

  for(Count1 in mutation_free:POPSIZE) {
      for(Count2 in 1 : GENE_NUMBER) {
          Chance = runif(1,0,1)
          if(Chance <= mutation_rate) {
              if(IndL[[Count1]]$Gene[Count2] == 0) {
                  IndL[[Count1]]$Gene[Count2] = 1
              } else {
                  IndL[[Count1]]$Gene[Count2] = 0
              }
          }
      }
  }
  
  return(IndL)
}

IndL = Create_pop(POPSIZE,GENE_NUMBER)                                          # calls function that generates random chromosomes

# main loop over all of our functions

meantopfitness = c() # empty vectors to store results
meanfitness = c()

for(generation in 1:MAX_GENERATIONS) {  
  IndL = Evaluate_fitness(IndL,POPSIZE,GENE_NUMBER) 							# give back altered population with fitness
  IndL = Sort(IndL)
  IndL = Reproduce(IndL,POPSIZE,NUM_BREED)
  IndL = Mutate(IndL,POPSIZE,NUM_BREED,mutation_free,mutation_rate)
  
  # output
  addfitness = 0
  add_top = 0
  for(Count in 1:POPSIZE) {
      addfitness = addfitness + IndL[[Count]]$Fitness
      if(Count <= 10) {
         add_top = add_top + IndL[[Count]]$Fitness
      }
  }
  meantopfitness[generation] = add_top/10
  meanfitness[generation] = addfitness/POPSIZE
  
  cat("generation:",generation,"\n")
  cat("mean top fitness:",meantopfitness[generation],"\n")
  cat("mean fitness",meanfitness[generation],"\n")
}

# # plot results
cat("Change this directory to a suitable directory on your machine!")
jpeg('/home/sergii/Documents/courses/modelling/plot.jpg')
plot(1:MAX_GENERATIONS,meanfitness,ylim=c(0,255),pch=19,type="l")
points(1:MAX_GENERATIONS,meantopfitness,pch=19,col="red",type="l")
dev.off()

}

f <- function(x,y) x*sin(4*x)+1.1*y*sin(2*y)
x <- seq(0,10, length=101)
y <- seq(0,10, length=101)
z <- outer(x,y,f)

persp(x,y,z,
      theta=30, phi=30, expand=0.6,
      ticktype='detailed')

