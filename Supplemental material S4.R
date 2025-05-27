### Paper title: A novel method to estimate actual infrastructure-induced mortality by integrating sampling biases

### R script to simulate and analyse road survey census data for mammals g5

## R version 4.2.2

#In this script, we simulate census data for each mammals g5 group under various
#scenarios involving the number of road transects as described in Section 2.4 of
#the main text of the paper.
#Using the modeling framework outlined in Section 2.2, this script estimate 
#the simulated values for the total number of roadkills, as well as the carcass 
#location (pl) and carcass observation per per walking, cycling and driving survey
#method (pom) probabilities survey method (pom) probabilities. We use in the 
#analysis standard errors for the pl prior set at either 0.05 or 0.1.

#Here's an overview of the different sections of the code

#1. Global Simulation Parameters Setup: This part of the code let you customise 
#ech parameter in the simulation of data census and their posterior analysis

#2. Load and Visualize Monthly Trend: This part of the code load and shows in a 
#plot the annual tendency of roadkill abundance along a year

#3. Calculate alpha and beta Parameters of the Beta Distributed Prior for pl 
#Based on Roman et al. (2024): This section makes a beta distributed prior from
#a mean value and standar error of pl

#4. Census data simulation: This section simulates census data, since we assume 
#that mammals g5 carcasses are not affected by carcass persistence probability as
#they remain on the road all month and their roadkill numbers are low, 
#simulating daily number of roadkill (lambda_td) values along a D = 30 days 
#period led to an unrealistically high value of total number of roadkill.
#Therefore, here, we simulate a single lambda_td value for the entire month.
#Additionally, this section applies the model to estimate the total number of
#roadkills, pl and pom (po1 =walking, po2 = cycling and po3 = driving)


#load necessary packages
library(jagsUI)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(truncnorm)

setwd("...") # set path to location where all Supplementary material S5.1 files necessary to run the script are

# -----------------------------------------------------------------------------
#1. Global Simulation Parameters Setup
# -----------------------------------------------------------------------------

# Number of survey road_transects (modify for different simulation scenarios)
nroad_transects <- 10  # Scenarios: 10 or 100

# Number of months included in the simulation
nmonth <- 12  

# Number of survey methods considered in the simulation
nmethod <- 3  

# Number of simulations to run
nsimulations <- 20  

# Standard Error for Carcass Persistence Probability (pl) prior distribution (modify for different scenarios)
se_pl <- 0.05  # Scenarios: 0.05 or 0.10  

#Vertebrate group along with their charateristics
select_group <- function(group_name) {
  if (group_name == "mammals_g5") {
    anual_tendency <- c("anual_tendency_mammals_g5.csv")
    pl_group <- 0.5             # Carcass location probability
    pom_group <- c(1,0.9,0.8) # Carcass observation probabilities for each survey method (walking, cycling, driving)
  } else {
    stop("Invalid group. Please choose 'mammals_g5'.")
  }
  
  return(list(anual_tendency= anual_tendency, pl_group = pl_group, pom_group = pom_group))
}

#Select vertebrate group along with their charateristics
group <- "mammals_g5"  # write the group of interest
selected_group <- select_group(group)

# Directory to save simulation results (adjust to your specific path)
save_path <- "..."

# ------------------------------------------------------------------------------
#2. Load and Visualize Monthly Trend
# ------------------------------------------------------------------------------

# Load anual_tendency.csv
df <- read.csv(selected_group$anual_tendency, sep = ";")

# Specify the desired order of months
month_order <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", 
                 "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")

# Convert the "Month" column to an ordered factor based on the month order
df$Month <- factor(df$Month, levels = month_order)

# Create a line plot of roadkills by month
ggplot(df, aes(x = Month, y = Roadkill)) +
  geom_line(aes(group = 1), color = "blue", size = 1) +  # Add a connecting line
  geom_point(color = "red", size = 3) +                 # Highlight points
  labs(
    title = "Monthly Trend",
    x = "Month",
    y = "Number of Roadkill"
  )

# Extract annual trend data for further analysis
monthly_trend <- df$Roadkill

# -----------------------------------------------------------------------------
#3. Calculate alpha and beta Parameters of the Beta Distributed Prior for pl
# -----------------------------------------------------------------------------

mean_prior_pl <- selected_group$pl_group  # Prior Distribution Mean
se_prior_pl <- se_pl  # Prior Distribution Standard error

# Calculate alpha and beta parameters of the beta distribution
alpha_prior_pl <- ((1 - mean_prior_pl) / (se_prior_pl^2) - 1 / mean_prior_pl) * (mean_prior_pl^2)
beta_prior_pl <- alpha_prior_pl * (1 / mean_prior_pl - 1)

# Plot Beta Distribution
curve(dbeta(x, alpha_prior_pl, beta_prior_pl), 
      from = 0, to = 1, 
      main = expression("p"["P"]~" Prior"),
      ylab = "Density", 
      col = "blue", 
      lwd = 2)

# Print Results
cat("Prior Distribution Mean:", mean_prior_pl, "\n")
cat("Prior Distribution Standard error :", se_prior_pl, "\n")
cat("Alpha Parameter for Beta Distribution:", alpha_prior_pl, "\n")
cat("Beta Parameter for Beta Distribution:", beta_prior_pl, "\n")


# -----------------------------------------------------------------------------
#4. Census data simulation
# -----------------------------------------------------------------------------

# Initialize the vector to store results
census_data <- list()

# Loop through simulations
for (sim in 1:nsimulations) {
  
  # Generate and save simulated lambdas_td (lambdasim)
  
    lambdasim <- monthly_trend  
    
    # Save lambdasim for each time step i
    saveRDS(lambdasim, file = paste0(save_path, group, "SE_pl_", se_pl, "_nroad_transects_", nroad_transects, 
                                     
                                     "_lambdasim", i, "_sim_", sim, ".RData"))
  
  # Generate total number of roadkill (N_it) based on lambdasim

    
    # Initialize matrix for total number of roadkill
    N_it <- matrix(NA, nrow = nroad_transects, ncol = nmonth)
    
    for (t in 1:nmonth) {
      if (length(lambdasim) >= t) {
        N_it[, t] <- rpois(nroad_transects, lambda = lambdasim[t])  # equation 1
      }
    }
    
    # Save the matrix
    saveRDS(N_it, file = paste0(save_path, group, "SE_pl_", se_pl, "_nroad_transects_", nroad_transects, 
                                 
                                 "_N_it_sim_", sim, ".RData"))
  
  # Generate roadkill numbers located within the road (N2_it)
    N2_it <- matrix(NA, nrow = nroad_transects, ncol = nmonth)
    
    for (i in 1:nroad_transects) {
      for (t in 1:nmonth) {
        N2_it[i, t] <- rbinom(1, N_it[i, t], selected_group$pl_group)
      }
    }
    
    # Save the matrix
    saveRDS(N2_it, file=paste0(save_path, group, "SE_pl_", se_pl, "_nroad_transects_", nroad_transects, 
                                
                                "_N2_it_sim_", sim, ".RData"))
  
  # Census data simulation for each survey methods
  nsurveys <- 3  # Number of surveys
  
  
  # Initialize census data array for 3 methods
  C <- array(dim = c(nroad_transects, nmonth, nmethod, nsurveys))
  
  for (m in 1:3) {
    for (k in 1:nsurveys) {
      C[,,m,k] <- matrix(rbinom(nroad_transects * nmonth, N2_it, selected_group$pom_group[m]), nrow = nroad_transects, ncol = nmonth)
    }
  }
  
  # Save the generated census data for the current simulation
  saveRDS(C, file = paste0(save_path, group, "SE_pl_", se_pl, "_nroad_transects_", nroad_transects, 
                           
                           "_census_data_sim_", sim, ".RData"))
  
  # Store the generated census data in the list
  census_data[[sim]] <- C
}

# Loop over all census data sets
for (census_data_index in seq_along(census_data)) {
  
  # Access the current census data for processing
  current_census_data <- census_data[[census_data_index]]
  print(paste("Running simulation", census_data_index, "."))
  
  # Prepare the data for the model
  bdata <- list(
    C = current_census_data,                         # Observation data
    nroad_transects = dim(current_census_data)[1],   # Number of road transects
    nmonth = dim(current_census_data)[2],            # Number of months
    nmet = dim(current_census_data)[3],              # Number of survey methods
    nsurveys = dim(current_census_data)[4],          # Number of surveys
    alpha_prior_pl = alpha_prior_pl,                 # Prior parameter alpha for carcass location probability
    beta_prior_pl = beta_prior_pl                    # Prior parameter beta for carcass location probability
    )
  
  # Write the JAGS model to a file
  sink(paste("modelplpo(m)_census_data", census_data_index, ".txt", sep = ""))
  cat("
    model {
      # Priors
      pl ~ dbeta(alpha_prior_pl, beta_prior_pl)  # Carcass location probability
      
      for (m in 1:nmet) {
        po[m] ~ dunif(0, 1)  # Carcass observation probability
      }
   
      for (t in 1:nmonth) {  # Loop over months
        lambda[t] ~ dunif(0, 300)  # Expected abundance
      }
    
      # Likelihood
      for (i in 1:nroad_transects) {
        for (t in 1:nmonth) {
          N[i, t] ~ dpois(lambda[t])                      # Total number of roadkill
          N2[i,t] ~ dbin(pl, N[i,t])T(0,100000)          # Roadkills inside road. We use truncation to avoid negative values
          
          for (m in 1:nmet) {
            for (j in 1:nsurveys) {
              C[i, t, m, j] ~ dbin(po[m], N2[i, t])T(0, 100000)  # Census data
            }
          }
        }
      }
    
      # Derived quantities: Total number of roadkill across all surveyed road transects per month
      for (t in 1:nmonth) {
        totalN[t] <- sum(N[, t]) 
        totalN2[t] <- sum(N2[,t])
      }
    }
    ", fill = TRUE)
  sink()
  
  # Initialize state variables to avoid conflicts between data and model
  Nst <- apply(current_census_data, c(1, 2), max, na.rm = TRUE) + 500
  Nst[!is.finite(Nst)] <- 500  # Replace infinite/NA values with a default
  
  # Define initial values for the MCMC algorithm
  inits <- function() {
    list(
      lambda = runif(dim(current_census_data)[2], 5, 100),  # Initial lambda 
      pl = rbeta(1, alpha_prior_pl, beta_prior_pl),        # Initial carcass location probability
      po = runif(3),                                       # Initial carcass observation probability
      N = Nst                                              # Initial roadkill number
    )
  }
  
  # Specify parameters to monitor in the MCMC
  params <- c("lambda", "pl", "po", "totalN","totalN2")
  
  # MCMC settings for the JAGS model
  na <- 100000  # Number of adaptation steps
  ni <- 400000  # Number of iterations
  nt <- 1000    # Thinning interval
  nb <- 100000  # Burn-in period
  nc <- 3       # Number of chains
  
  # Run the JAGS model, check convergence, and summarize posterior distributions
  output <- jags(bdata, inits, params, paste("modelplpo(m)_census_data", census_data_index, ".txt", sep = ""),
                 n.adapt = na, n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE
  )
  
  # Save the output to a file for later analysis
  saveRDS(output, file = paste0(
    save_path, group, "SE_pl_", se_pl, 
    "_nroad_transects_", nroad_transects, 
    "_output_analysis_sim_", census_data_index, ".RData"
  ))
}
