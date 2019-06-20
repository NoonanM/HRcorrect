# HRcorrect
A shiny app for correcting previously estimated 95\% home range areas
derived from conventional estimators for body-mass specific bias.

The correction is based on the regressional analyses in Noonan et al. (2019) Body size dependent underestimation of home range areas. In prep.

This correction is not a substitute for using a more rigorous home range estimator (e.g. autocorrelated-KDE), and should only be used for cases where the underlying tracking data are not accessible.

To install and run the app run:

```{r, echo=FALSE, message=FALSE, warning = FALSE}

# Get list of the currently installed packages
avail <- installed.packages()[, 1]

# Make a list of the required packages
needed <- c("shiny", "shinydeshboard", "devtools")

# Check which of these are missing
install <- needed[!needed %in% avail]

# And then install them if necessary
if (length(install) > 0) {
  install.packages(install)
}

# After installing the dependencies,
# install HRcorrect from the github repository
devtools::install_github(repo = "NoonanM/HRcorrect",
                         auth_token = )


# Finally, run the application
correct()

```

Notes:

The area correction is unit invariant, and will preserve whatever units the original area are provided in.

Mass must be provided in kilograms.

Upon installation, the HR_Correction() function is exported into the namespace and can be used directly in the command line interface.