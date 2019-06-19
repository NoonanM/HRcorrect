
HR_Correction <- function(Area = NULL, Mass = NULL){

  #Area = 10
  #Mass = 10
  #Formulae for calculating a mass specific correction factor +/- 95% Confidence intervals
  ML_Correction_Factor <- function(x) {1/(1 + exp(0.3644301*(x - 3.693227)))}
  Min_Correction_Factor <- function(x) {1/(1 + exp(0.1888994*(x - 7.1498172)))}
  Max_Correction_Factor <- function(x) {1/(1 + exp(0.9177596*(x - 0.2366375)))}

  #Calculate the mass specific correction factor
  ML_CF <- ML_Correction_Factor(log10(Mass))

  Max_CF <- Max_Correction_Factor(log10(Mass))

  Min_CF <- Min_Correction_Factor(log10(Mass))

  if(missing(Area)){
    
    Corrected_Area <- round(c(1/Min_CF, 1/ML_CF, 1/Max_CF), 3)
    
    
    names(Corrected_Area) <- c("Min", "ML", "Max")
    Corrected_Area <- t(as.data.frame(Corrected_Area))
    
    row.names(Corrected_Area) <- "Mass specific correction factor"
    
    Corrected_Area
  } else{
  #Apply the correction factor to the area estimate
  Corrected_Area <- round(c(Area/Min_CF, Area/ML_CF, Area/Max_CF), 3)
  
  
  names(Corrected_Area) <- c("Min", "ML", "Max")
  Corrected_Area <- t(as.data.frame(Corrected_Area))
  
  row.names(Corrected_Area) <- "Corrected area"
  
  Corrected_Area
}
}


HR_Correction(Mass = 100)
