
HR_Correction <- function(Area = NULL, Mass = NULL){

  #Area = 10
  #Mass = 10
  #Formulae for calculating a mass specific correction factor +/- 95% Prediction Intervals
  
  #Load in the fitted model from the manuscript
  load("Data/Bias_Model.Rda")
  

  if(Mass != 0){
  #Use the fitted model to calculate the mass specific correction factor
  ML_CF <- predict(Exp_Mod, newdata = data.frame(Mass = log10(Mass)))

  
  ## Use the delta method to get CIs on the fit
  fit <- predict(Exp_Mod)
  
  V <- vcov(Exp_Mod)
  X <- model.matrix(~log10(Mass),data=data.frame(Mass = log10(Mass)))
  se.fit <- sqrt(diag(X %*% V %*% t(X)))
  
  predframe <- data.frame(log10(Mass),
                          bias.95=ML_CF,
                          lwr=ML_CF-1.96*se.fit,
                          upr=ML_CF+1.96*se.fit)
  
  
  
  Max_CF <- predframe$upr

  Min_CF <- predframe$lwr
  
  } else {
    ML_CF <- 0
    Max_CF <- 0
    Min_CF <- 0
  }

  if(missing(Area)){
    
    Corrected_Area <- round(c(1/Min_CF, 1/ML_CF, 1/Max_CF), 3)
    
    
    names(Corrected_Area) <- c("Min", "ML", "Max")
    Corrected_Area <- t(as.data.frame(Corrected_Area))
    
    #row.names(Corrected_Area) <- "Mass specific correction factor"
    row.names(Corrected_Area) <- NULL
    
    Corrected_Area
  } else{
  #Apply the correction factor to the area estimate
  Corrected_Area <- c(Area, Mass, round(c(Area/Min_CF, Area/ML_CF, Area/Max_CF), 3))
  
  #Reorder
  Corrected_Area[3:5] <- Corrected_Area[3:5][order(Corrected_Area[3:5])]
  
  names(Corrected_Area) <- c("Original Area", "Mass (kg)", "Corrected Area - Min", "Corrected Area - ML", "Corrected Area - Max")
  Corrected_Area <- t(as.data.frame(Corrected_Area))
  
  #row.names(Corrected_Area) <- "Corrected area"
  row.names(Corrected_Area) <- NULL
  
  Corrected_Area
}
}


HR_Correction(Mass = 100)
