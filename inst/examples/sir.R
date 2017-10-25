library(gamar)
defpath("/Applications/Gama.app")
# Extract the model parameters from the definition of the model sir.gaml
experimentSIR <- getmodelparameter(paste0(system.file("examples",package="gamar"),"/sir.gaml"),"sir")
getparameternames(experimentSIR)
# Define a first type of experiments on SIR having the same initial value
# called experimentA with S0=950, I0=50 and R0=0
experimentA <- experimentSIR
getoutputnames(experimentA)
experimentA <- setparametervalue(experimentSIR,"S0",950)
experimentA <- setparametervalue(experimentSIR,"I0",50)
experimentA <- setparametervalue(experimentSIR,"R0",0)
# ... and with 100 steps and a frame rate of 1 for the images of the susceptibles
experimentA <- setfinalstep(experimentSIR,100)
experimentA <- setoutputframerate(experimentSIR,"susceptibles",1)


# Define a first experiment on this model
experimentA1 <- experimentA
experimentA1 <- setparametervalue(experimentA1,"beta",.3)
experimentA1 <- setparametervalue(experimentA1,"gamma",.1)
experimentplanA <- addtoexperimentplan(experimentA1)

# Define a secod experiment on this model
experimentA2 <- experimentA
experimentA2 <- setparametervalue(experimentA2,"beta",.5)
experimentA2 <- setparametervalue(experimentA2,"gamma",.10)
experimentplanA <- addtoexperimentplan(experimentplanA,experimentA2)

# Execute all the exeperiments in the plan
outputA <- runexpplan(experimentplanA,hpc = 2)
# Visualize the number of infected for the two experiments
par(mfrow=c(2,2))
with(outputA[[1]],plot(step,I,type="l",lwd=2,col="red"))
with(outputA[[2]],plot(step,I,type="l",lwd=2,col="blue"))
makemovie(outputA[[1]])
