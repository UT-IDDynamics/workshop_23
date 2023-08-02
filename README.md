# Welcome to the repository of materials for the

# University of Utah Infectious Disease Dynamics

# Modeling Workshop 2023!

## The included materials are intended as companions to our in-person workshop held in the Spring of 2023. They may, however, be useful to participants and non-participants as references.

# FILE DESCRIPTION

The install_packages_for_workshop.R file contains a few lines of code to install the dependencies for the rest of the materials. Required packages are: 'deSolve', 'R0', 'forecast', 'padr', and 'mgcv'. If you want to run shiny apps on your own computer, you will also need to install the 'shiny' package.

## The 'in_person_workshop_scripts' folder contains code for:

(1) a basic SIR simulation model (U_basic_sir.R)

(2) an example of a simple framework to estimate R0 from case data (U_estimate_r0.R)

(3) a simple SIRV simulation model (U_SIRV.R)

(4) a streamlined GAM model, including examples of fitting the model to case data and using the model to forecast (U_GAM_forecast.R)

(5) a rudimentary example of fitting an SIR model to case data (the same data as in #4) and using the fitted model to forecast (U_SIR_fit_forecast.R)

## The 'in_person_workshop_shiny_apps' folder contains scripts that...

create interactive shiny applications that allow users to explore and experience key lessons from the above scripts (except #2 - R0 estimation). Use is simple - just make sure that you have the 'shiny' package downloaded by running `install.packages('shiny')` before you try to run any of the programs in the folder.

## Each of these shiny apps are also hosted online. If you don't want to run the apps from your own computer, you can simply access them at the following URLs:

(1) <https://uouiddworkshop.shinyapps.io/sir_shiny/>

(2) N/A (nothing to see here)

(3) <https://uouiddworkshop.shinyapps.io/sirv_shiny/>

(4) <https://uouiddworkshop.shinyapps.io/gam_shiny/>

(5) <https://uouiddworkshop.shinyapps.io/sir_ssqfit_forecast/>

(6) A fancier SIRV model is also available. Relative to #3, this script adds the ability to specify the number of initial infections and outputs the total vaccine doses given. Useful for in-person workshop exercise where we explore the effects of different vaccine allocation strategies across different scenarios. <https://uouiddworkshop.shinyapps.io/scenario2_sirv_shiny/>

Like these shiny apps, but want to modify them or build your own? Shiny syntax can be a little confusing at first, but it goes quickly once you get the hang of it. A nice place to start is: <https://mastering-shiny.org/basic-app.html>

###### The other files in the directory are different cuts of a dataset on cases of a regional outbreak of a virus. You can safely ignore them :)

Questions? Comments? Concerns? Suggestions? Please contact us by creating an Issue or Pull Request using the tabs at the top of the page.

## This workshop and the companion materials were made possible by generous funding from the Centers for Disease Control and Prevention.
