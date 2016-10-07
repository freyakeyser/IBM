######################################
### Population dynamics parameters ###
######################################

Colors <- c("wild", "hybrid", "farm") # population phenotypes
tmax <- 100 # maximum time duration of simulation
n.initial <- 50*length(Colors) # inital population size
CC <- 500 # carrying capacity of offspring
M <- 0.2 # natural mortality


#########################
### Process functions ###
#########################

# Creation
make.inds <- function(id=NaN, color=NaN, age=NaN){
  inds <- data.frame(id = id, color = color, age = 0) # give then starting attributes (id, color, age)
  inds
}

# Growth
grow.inds <- function(inds){
  inds$age <- inds$age + 1 # advance age by one
  inds
}

# Reproduction (breeding, inheritance)
reproduce.inds <- function(inds){
  n.eggs <- rlnorm(nrow(inds), mean=0.01, sd=0.7) # how many offspring per adult
  n.eggs <- round(n.eggs * (1 - sum(n.eggs)/CC)) # mediate by carrying capacity
  if(sum(n.eggs) > 0){
    motherRow <- rep(seq(nrow(inds)), times=n.eggs) # mother's row
    offspring <- make.inds(id=seq(max(inds$id)+1, length.out=sum(n.eggs))) # make offspring
    offspring$color <- inds$color[motherRow] # inherit color
    inds <- rbind(inds, offspring) # add new offspring to adults
  }
  inds
}

# Mortality
kill.inds <- function(inds){
  pDeath <- 1 - exp(-M) # prob. of death
  kill <- which(runif(nrow(inds)) < pDeath) # kill these ones
  inds <- inds[-kill,]
  inds
}


###################
### Simulation ####
###################

# Initial population
inds <- make.inds(id=1:n.initial, color=as.factor(array(Colors, dim=n.initial)))
head(inds, 10)

# Object for storing results (population size in time)
N <- array(NaN, dim=c(tmax, length(Colors)))
colnames(N) <- Colors
N[1,] <- summary(inds$color)

# Run 
set.seed(1) # optional starting seed
for(t in 2:tmax){ 
  # population processes
  inds <- grow.inds(inds)
  inds <- reproduce.inds(inds)
  inds <- kill.inds(inds)
  
  # store results
  N[t,] <- summary(inds$color)
  print(paste("time =", t, "; n = ", sum(N[t,])))
  
  # break when one phenotype dominates
  if(sum(N[t,] > 0) == 1) {break}
}
N <- N[1:t,] # remove excess rows of results object


##############
#### Plots ###
##############
# Population sizes
png("pop_size.png", width=5, height=5, res=300, units="in", type="cairo")
op <- par(mar=c(5,5,1,1))
ylim <- 1.2 * range(N)
Time <- seq(nrow(N))
plot(Time, N[,1], t="n", lwd=2, col=4, ylim=ylim, ylab="Population size")
for(i in seq(Colors)){
  lines(Time, N[,i], col=c("red", "blue", "green")[i], lwd=2)
}
legend("topleft", legend=paste(Colors, "pop."), lwd=2, col=c("red", "blue", "green"), bty="n")
par(op)
dev.off()

# Age distribution of final population
png("age_dist.png", width=5, height=5, res=300, units="in", type="cairo")
op <- par(mar=c(5,5,2,1))
hist(inds$age, freq=FALSE, col=8, xlab="Age [years]", main="Age distribution")
par(op)
dev.off()