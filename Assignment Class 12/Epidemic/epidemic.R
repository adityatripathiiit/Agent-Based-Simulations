##
# -------------------------------------- EXPERIMENT 1 --------------------------------------------------
# In the 1st experiment we will try to adjust the infectioness parameter to the real
# data in the infected_portugal_01032020-17052020.csv file
# The conclusion is that for predetermined values of density (0.9), speed (0.5) and mortality (1.4)
# the value that best fits the real data for infectioness is 0.9
# ------------------------------------------------------------------------------------------------------
##


# load experiment 1 results from behaviorspace output
experiment_data_1 <- read.csv("epidemic experiment_1-table.csv",skip=6,stringsAsFactors = FALSE,sep=',')

# make clean column names
colnames(experiment_data_1) <- c("run","density","infectioness","mortality","speed","step","infected","cured","dead")

# aggregate data (calculate mean) keeping different steps and different infection rates
agg.data_1 <- aggregate(experiment_data_1, by=list(experiment_data_1$step,experiment_data_1$infectioness),FUN = mean)

#visualize data
with(agg.data_1[agg.data_1$infectioness==0.9,], plot(step,infected, t='l'))
with(agg.data_1[agg.data_1$infectioness==0.7,], lines(step,infected, t='l',lty=2))
with(agg.data_1[agg.data_1$infectioness==0.5,], lines(step,infected, t='l',lty=3))
with(agg.data_1[agg.data_1$infectioness==0.3,], lines(step,infected, t='l',lty=4))
with(agg.data_1[agg.data_1$infectioness==0.1,], lines(step,infected, t='l',lty=5))

with(agg.data_1[agg.data_1$infectioness==0.9,], plot(step,cured, t='l'))
with(agg.data_1[agg.data_1$infectioness==0.7,], lines(step,cured, t='l',lty=2))
with(agg.data_1[agg.data_1$infectioness==0.5,], lines(step,cured, t='l',lty=3))
with(agg.data_1[agg.data_1$infectioness==0.3,], lines(step,cured, t='l',lty=4))
with(agg.data_1[agg.data_1$infectioness==0.1,], lines(step,cured, t='l',lty=5))

with(agg.data_1[agg.data_1$infectioness==0.9,], plot(step,dead, t='l'))
with(agg.data_1[agg.data_1$infectioness==0.7,], lines(step,dead, t='l',lty=2))
with(agg.data_1[agg.data_1$infectioness==0.5,], lines(step,dead, t='l',lty=3))
with(agg.data_1[agg.data_1$infectioness==0.3,], lines(step,dead, t='l',lty=4))
with(agg.data_1[agg.data_1$infectioness==0.1,], lines(step,dead, t='l',lty=5))

# load real world infected data
infected_data <- read.csv("infected_portugal_01032020-17052020.csv",stringsAsFactors = FALSE,sep=';')

# compare curves
with(agg.data_1[agg.data_1$infectioness==0.9,], plot(step,infected, t='l'))
with(agg.data_1[agg.data_1$infectioness==0.7,], lines(step,infected, t='l',lty=2))
with(agg.data_1[agg.data_1$infectioness==0.5,], lines(step,infected, t='l',lty=3))
with(agg.data_1[agg.data_1$infectioness==0.3,], lines(step,infected, t='l',lty=4))
with(agg.data_1[agg.data_1$infectioness==0.1,], lines(step,infected, t='l',lty=5))
with(infected_data, lines(day,infected,t='h', col="red"))

##
# ----------------------- EXPERIMENT 2 -------------------------------------------------------------------------------
# Having found a value for infectioness that best fit the real data we now look to the parameter speed.
# Being very low (0.5) we hope the higher values with fit better the data.
# Perform another experiment now with the previous best value for infectioness and varying speed
# Aftercomparing the curves we found that the best fit is atained with speed = 2.5
# hoever we must scale down the simulated values in order to fite the real data
##

# load data result of BehaviorSpace experiment
experiment_data_2 <- read.csv("epidemic experiment_2-table.csv",skip=6,stringsAsFactors = FALSE,sep=',')

# make clean column names
colnames(experiment_data_2) <- c("run","density","infectioness","mortality","speed","step","infected","cured","dead")

# aggregate data (calculate mean) keeping different steps and different speed of movement of the population
agg.data_2 <- aggregate(experiment_data_2, by=list(experiment_data_2$step,experiment_data_2$speed),FUN = mean)

# visualize data but divide simulated data by five in order to have a better fit
with(infected_data, plot(day,infected,t='h', col="red", xlim=c(0,100)))
with(agg.data_2[agg.data_2$speed==4.0,], lines(step,infected/5, t='l',lty=1))
with(agg.data_2[agg.data_2$speed==3.5,], lines(step,infected/5, t='l',lty=2))
with(agg.data_2[agg.data_2$speed==3.0,], lines(step,infected/5, t='l',lty=3))
with(agg.data_2[agg.data_2$speed==2.5,], lines(step,infected/5, t='l',lty=4))
with(agg.data_2[agg.data_2$speed==2.0,], lines(step,infected/5, t='l',lty=5))
with(agg.data_2[agg.data_2$speed==1.5,], lines(step,infected/5, t='l',lty=6))
with(agg.data_2[agg.data_2$speed==1.0,], lines(step,infected/5, t='l',lty=3,lwd=2))

# Visualize identified solution till this point
with(infected_data, plot(day,infected,t='h', col="red"))
with(agg.data_2[agg.data_2$speed==2.5,],  lines(step,infected/5, t='l'))

# write the aggregated data to file
write.csv(agg.data_2[,c(4,5,6,7,8,9)],file="experiment_2_aggregated.csv",row.names=FALSE )

##
# ----------------------- EXPERIMENT 3 -------------------------------------------------------------------------------
# Having identified a reasonable fit for the real data with speed= 2.5 we nevertheless had to down scale the simulated values
# Lets try a new experiment with a downscale of the density (wich is intimately associated with population number)
# After testing with density equal to 0.2 we notice that there is not much improvement in our fit
# We conclude that there should be a dependence between density and speed parameter that should
# determine a better adjustment between simulated and real data
# it remains a task to be done, now, variying simultaneously density and speed
##

# load data result of BehaviorSpace experiment
experiment_data_3 <- read.csv("epidemic experiment_3-table.csv",skip=6,stringsAsFactors = FALSE,sep=',')

# make clean column names
colnames(experiment_data_3) <- c("run","density","infectioness","mortality","speed","step","infected","cured","dead")

# aggregate data (calculate mean) keeping different steps
agg.data_3 <- aggregate(experiment_data_3, by=list(experiment_data_3$step),FUN = mean)

# Visualize identified solution till this point
with(infected_data, plot(day,infected,t='h', col="red"))
with(agg.data_3,  lines(step,infected, t='l'))
