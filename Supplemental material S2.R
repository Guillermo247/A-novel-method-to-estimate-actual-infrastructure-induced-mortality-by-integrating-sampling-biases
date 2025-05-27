### Paper title: A novel method to estimate actual infrastructure-induced mortality by integrating sampling biases

### R script to simulate and analyse road survey census data for vertebrate groups affected by carcass location bias

## R version 4.2.2

#In this script, we simulate census data for each vertebrate group under various
#scenarios involving the number of road transects, as well as the variability in
#both the daily number of roadkills (lambda_itd) and the daily carcass persistence 
#probability (ppd), as described in Section 2.4 of the main text of the paper.
#Using the modeling framework outlined in Section 2.2, this script estimate 
#the simulated values for the total number of roadkills, as well as the carcass 
#location (pl), carcass persistence (pp), and carcass observation per walking, 
#cycling and drivingsurvey method (pom) probabilities. We use in the analysis 
#standard errors for the pl and pp priors set at either 0.05 or 0.1.

#Here's an overview of the different sections of the code

#1. Global Simulation Parameters Setup: This part of the code let you customise 
#ech parameter in the simulation of data census and their posterior analysis

#2. Load and Visualize Monthly Trend: This part of the code load and shows in a 
#plot the annual tendency of roadkill abundance along a year

#3. Calculate alpha and beta Parameters of the Beta Distributed Prior for pl 
#Based on Roman et al. (2024): This section makes a beta distributed prior from
#a mean value and standar error of pl


#4. Generate Carcass Persistence Curve for pp Prior Based on Santos et al. (2011)
#Daily pp (ppd): This part of the code calculates the average probability of 
#carcass persistence probability (pp) over the days it remains on the road without
#decomposing (D-day period).

#5. Calculate alpha and beta Parameters of the Beta Distributed Prior for pp: This
#section makes a beta distributed prior from a mean value and standar error of pp

#6. Census data simulation on SD lambda_td 0.5 and SD ppd 0.05: This section 
#simulates census data considering the variability in the daily number of 
#roadkills (lambda_td) and daily carcass persistence (ppd), with standard 
#deviations of 0.5 and 0.05, respectively. Additionally, this section 
#applies the model to estimate the total number of roadkills, pl, pp and pom 
#(po1 =walking, po2 = cycling and po3 = driving)

#The following sections perform the same steps as Section 5, but with varying 
#levels of variability for lambda_td and ppd:
#7. Census data simulation on SD lambda_td 0.5 and SD ppd 0.15
#8. Census data simulation on SD lambda_td 0.5 and SD ppd 0
#9. Census data simulation on SD lambda_td 1.5 and SD ppd 0.05
#10. Census data simulation on SD lambda_td 1.5 and SD ppd 0.15
#11. Census data simulation on SD lambda_td 1.5 and SD ppd 0
#12. Census data simulation on SD lambda_td 0 and SD ppd 0.05
#13. Census data simulation on SD lambda_td 0 and SD ppd 0.15
#14. Census data simulation on SD lambda_td 0 and SD ppd 0


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

# Standard Error for Carcass Persistence Probability (pl) and Carcass Persistence Probability (pp) prior distribution (modify for different scenarios)
se_pl <- 0.05  # Scenarios: 0.05 or 0.10  
se_pp <- 0.05  # Scenarios: 0.05 or 0.10  

#Vertebrate group along with their charateristics
select_group <- function(group_name) {
  if (group_name == "reptiles_g2") {
    anual_tendency <- c("anual_tendency_reptiles_g2.csv")
    pl_group <- 0.42857            # Carcass location probability
    ppd_group <- 0.398477157       # Daily carcass persistence probability
    pom_group <- c(0.7,0.5,0.1) # Carcass observation probabilities for each survey method (walking, cycling, driving)
  } else if (group_name == "birds_bats_g1") {
    anual_tendency <- c("anual_tendency_birds_bats_g1.csv")
    pl_group <- 0.506329114
    ppd_group <- 0.358632169
    pom_group <- c(0.6,0.4,0.05)
  } else if (group_name == "birds_g2") {
    anual_tendency <- c("anual_tendency_birds_g2.csv")
    pl_group <- 0.692307692
    ppd_group <- 0.747433082
    pom_group <- c(0.8,0.6,0.2)
  } else if (group_name == "mammals_g4") {
    anual_tendency <- c("anual_tendency_mammals_g4.csv")
    pl_group <- 0.647058824
    ppd_group <- 0.805626598
    pom_group <- c(0.9,0.7,0.3)
  } else {
    stop("Invalid group. Please choose between 'reptiles_g2', 'birds_bats_g1', 'birds_g2' or 'mammals_g4'.")
  }
  
  return(list(anual_tendency= anual_tendency, pl_group = pl_group, ppd_group = ppd_group, pom_group = pom_group))
}

#Select vertebrate group along with their charateristics
group <- "mammals_g4"  # write the group of interest
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

# -------------------------------------------------------------------------------------------------------
#3. Calculate alpha and beta Parameters of the Beta Distributed Prior for pl Based on Roman et al. (2024)
# -------------------------------------------------------------------------------------------------------

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


# --------------------------------------------------------------------------------------------
#4. Generate Carcass Persistence Curve for pp Prior Based on Santos et al. (2011) Daily pp (ppd)
# --------------------------------------------------------------------------------------------

# Number of days to simulate a month (time period for persistence)
num_days <- 30  

# Initial values for time (x) and persistence probability (y)
x_values <- 0:(num_days - 1)  # Days from 0 to num_days-1
y_values <- 1  # Initial persistence probability of 1 for roadkill event day

# Generate Persistence Curve
for (day in 1:num_days) {
  # Update the persistence probability for each day
  if (day > 1) {
    y_values[day] <- y_values[day - 1] * selected_group$ppd_group
  }
}

# Plot Persistence Curve
plot(x_values, y_values, type = "l", xlim = c(0, 6), ylim = c(0, 1),
     xlab = "Days", ylab = "Persistence Probability",
     main = "Persistence Curve",
     col = "blue", lwd = 2)

# Calculate D = Maximum Days Carcass Remains Without Decomposing
target_probability <- 0.05  # pp = 0.05, carcass is nearly disappeared on the road
D <- log(target_probability) / log(selected_group$ppd_group)

# Round D to report as a discrete value in days 
D <- round(D, 0)

# Add reference lines to the plot to isolate Total and Under-Curve Area until D
abline(v = D, col = "red", lty = 2)  # Vertical line for time
abline(h = target_probability, col = "red", lty = 2)    # Horizontal line for probability

# Calculate Total and Under-Curve Area
total_area <- D * 0.95  # Rectangle width (max time) × height (0.95)

# Area under the curve using numerical integration
curve_function <- approxfun(x_values, y_values)
curve_integral <- integrate(curve_function, lower = 0, upper = D)$value

# Calculate pp by the percentage of area under the curve
pp <- curve_integral / total_area

# -----------------------------------------------------------------------------
#5. Calculate alpha and beta Parameters of the Beta Distributed Prior for pp
# -----------------------------------------------------------------------------
mean_prior_pp <- pp  # Prior Distribution Mean
se_prior_pp <- se_pp  # Prior Distribution Standard error

# Calculate alpha and beta parameters of the beta distribution
alpha_prior_pp <- ((1 - mean_prior_pp) / (se_prior_pp^2) - 1 / mean_prior_pp) * (mean_prior_pp^2)
beta_prior_pp <- alpha_prior_pp * (1 / mean_prior_pp - 1)

# Plot Beta Distribution
curve(dbeta(x, alpha_prior_pp, beta_prior_pp), 
      from = 0, to = 1, 
      main = expression("p"["P"]~" Prior"),
      ylab = "Density", 
      col = "blue", 
      lwd = 2)

# Print Results
cat("Prior Distribution Mean:", mean_prior_pp, "\n")
cat("Prior Distribution Standard error :", se_prior_pp, "\n")
cat("D:", D, "days\n")
cat("Alpha Parameter for Beta Distribution:", alpha_prior_pp, "\n")
cat("Beta Parameter for Beta Distribution:", beta_prior_pp, "\n")


# -----------------------------------------------------------------------------
#6. Census data simulation on SD lambda_td 0.5 and SD ppd 0.05
# -----------------------------------------------------------------------------

# Initialize the vector to store results
census_data <- list()

# Loop through simulations
for (sim in 1:nsimulations) {
  
  # Define standard deviations for lambda_td by random values that multiplies lambda_td and for ppd
  sd_random_values <- 0.5  # SD for random values affecting population growth
  sd_ppd <- 0.05  # SD for persistence probability
  
  # Generate random values with truncation ensuring values are greater than 0
  random_values <- rtruncnorm(n = D, a = 0, mean = 1, sd = sd_random_values)
  
  # Save the generated random values for this simulation
  saveRDS(random_values, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                       "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                       "_sd_random_values_sim_", sim, ".RData"))
  
  # Generate and save simulated lambdas_td (lambdasim)
  for (j in 1:D) {
    lambdasim <- monthly_trend * random_values[j]  # Adjusting growth rate with random values
    assign(paste0("lambdasim", j), lambdasim)  # Assign lambda value dynamically
    
    # Save lambdasim for each time step j
    saveRDS(lambdasim, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                     "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                     "_lambdasim", j, "_sim_", sim, ".RData"))
  }
  
  # Generate total number of roadkill (N_itd) based on lambdasim
  for (j in 1:D) {
    lambdasim_name <- paste0("lambdasim", j)
    lambdasim_current <- get(lambdasim_name)  # Retrieve current lambda_td value
    
    # Initialize matrix for total number of roadkill
    N_itd <- matrix(NA, nrow = nroad_transects, ncol = nmonth)
    
    for (t in 1:nmonth) {
      if (length(lambdasim_current) >= t) {
        N_itd[, t] <- rpois(nroad_transects, lambda = lambdasim_current[t])  # equation 1
      }
    }
    
    # Save the matrix
    assign(paste0("N_itd", j), N_itd)
    saveRDS(N_itd, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                 "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                 "_N_itd", j, "_sim_", sim, ".RData"))
  }
  
  # Generate roadkill numbers located within the road (N2_itd)
  for (j in 1:D) {
    N_itd_name <- paste0("N_itd", j)
    N_itd_current <- get(N_itd_name)
    
    N2_itd <- matrix(NA, nrow = nroad_transects, ncol = nmonth)
    
    for (i in 1:nroad_transects) {
      for (t in 1:nmonth) {
        N2_itd[i, t] <- rbinom(1, N_itd_current[i, t], selected_group$pl_group)
      }
    }
    
    assign(paste0("N2_itd", j), N2_itd)
    # Save the matrix
    saveRDS(N2_itd, file=paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                "_N2_itd", j, "_sim_", sim, ".RData"))
  }
  
  
  # Generate random daily carcass persistence probabilities along sd_ppd
  ppd <- rnorm(D, mean = selected_group$ppd_group, sd = sd_ppd)
  
  # Ensure that the product of all ppd values is greater than 0.05  
  # and no ppd value is greater than or equal to 1  
  while (prod(ppd) <= 0.05 || any(ppd >= 1)) { 
    ppd <- rnorm(D, mean = selected_group$ppd_group, sd = sd_ppd)
    ppd[ppd > 1] <- 1  
  }
  
  # Save the generated ppd values
  saveRDS(ppd, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                             "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                             "_ppd_sim_", sim, ".RData"))
  
  # Initialize N3_itD matrix
  N3_itD <- matrix(0, nrow = nroad_transects, ncol = nmonth)
  
  # Generate N3_itd matrix
  for (j in 1:D) {
    ppd_t <- if (j == 1) prod(ppd) else prod(ppd[j:length(ppd)])  # Adjust ppd according to the age in days of the carcasses
    
    # Initialize N3_itd for each day across D
    N3_itd <- matrix(NA, nrow = nroad_transects, ncol = nmonth)
    
    # Retrieve the N_itd for the current day
    N2_itd_name <- paste0("N2_itd", j)
    N2_itd_current <- get(N2_itd_name)
    
    # Generate the N3_itd matrix based on binomial distribution
    for (i in 1:nroad_transects) {
      for (t in 1:nmonth) {
        N3_itd[i, t] <- rbinom(1, N2_itd_current[i, t], ppd_t)  # equation 3
      }
    }
    
    # Save the N3_itd matrix for each day in D-day period
    assign(paste0("N3_itd", j), N3_itd)
    saveRDS(N3_itd, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                  "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                  "_N3_itd", j, "_sim_", sim, ".RData"))
    
    # Accumulate each N3_itd in N3_itD
    N3_itD <- N3_itD + N3_itd
    saveRDS(N3_itD, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                  "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                  "_N3_itD_sim_", sim, ".RData"))
  }
  
  
  # Census data simulation for each survey methods
  nsurveys <- 3  # Number of surveys
  
  
  # Initialize census data array for 3 methods
  C <- array(dim = c(nroad_transects, nmonth, nmethod, nsurveys))
  
  for (m in 1:3) {
    for (k in 1:nsurveys) {
      C[,,m,k] <- matrix(rbinom(nroad_transects * nmonth, N3_itD, selected_group$pom_group[m]), nrow = nroad_transects, ncol = nmonth)
    }
  }
  
  # Save the generated census data for the current simulation
  saveRDS(C, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                           "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                           "_census_data_sim_", sim, ".RData"))
  
  # Store the generated census data in the list
  census_data[[sim]] <- C
}



# Loop over all census data sets
for (census_data_index in seq_along(census_data)) {
  
  tryCatch({
    # Access the current census data for processing
    current_census_data <- census_data[[census_data_index]]
    print(paste("Running simulation", census_data_index, "for SD LAMBDAS", sd_random_values, "and SD ppd", sd_ppd, "."))
    
    # Prepare the data for the model
    bdata <- list(
      C = current_census_data,                         # Observation data
      nroad_transects = dim(current_census_data)[1],   # Number of road transects
      nmonth = dim(current_census_data)[2],            # Number of months
      nmet = dim(current_census_data)[3],              # Number of survey methods
      nsurveys = dim(current_census_data)[4],          # Number of surveys
      alpha_prior_pl = alpha_prior_pl,                 # Prior parameter alpha for carcass location probability
      beta_prior_pl = beta_prior_pl,                   # Prior parameter beta for carcass location probability
      alpha_prior_pp = alpha_prior_pp,                 # Prior parameter alpha for carcass persistence probability
      beta_prior_pp = beta_prior_pp                    # Prior parameter beta for carcass persistence probability
    )
    
    # Write the JAGS model to a file
    sink(paste0("modelplpppo(m)_census_data", census_data_index, ".txt"))
    cat("
      model {
        # Priors
        pl ~ dbeta(alpha_prior_pl, beta_prior_pl)  # Carcass location probability
        pp ~ dbeta(alpha_prior_pp, beta_prior_pp)  # Carcass persistence probability
        
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
            N2[i,t] ~ dbin(pl, N[i,t])T(0,100000)          # Roadkills inside road
            N3[i, t] ~ dbin(pp, N2[i, t])T(0, 100000)      # Roadkills preserved until road survey
            
            for (m in 1:nmet) {
              for (j in 1:nsurveys) {
                C[i, t, m, j] ~ dbin(po[m], N3[i, t])T(0, 100000)  # Census data
              }
            }
          }
        }
      
        # Derived quantities: Total number of roadkill across all surveyed road transects per month
        for (t in 1:nmonth) {
          totalN[t] <- sum(N[, t]) 
          totalN2[t] <- sum(N2[,t])
          totalN3[t] <- sum(N3[, t])   
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
        pp = rbeta(1, alpha_prior_pp, beta_prior_pp),        # Initial carcass persistence probability
        po = runif(3),                                       # Initial carcass observation probability
        N = Nst                                              # Initial roadkill number
      )
    }
    
    # Specify parameters to monitor in the MCMC
    params <- c("lambda", "pl", "pp", "po", "totalN", "totalN2", "totalN3")
    
    # MCMC settings for the JAGS model
    na <- 100000  # Number of adaptation steps
    ni <- 400000  # Number of iterations
    nt <- 1000    # Thinning interval
    nb <- 100000  # Burn-in period
    nc <- 3       # Number of chains
    
    # Run the JAGS model, check convergence, and summarize posterior distributions
    output <- jags(
      bdata, inits, params, 
      paste0("modelplpppo(m)_census_data", census_data_index, ".txt"),
      n.adapt = na, n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE
    )
    
    # Save the output to a file for later analysis
    saveRDS(output, file = paste0(
      save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, 
      "_nroad_transects_", nroad_transects, 
      "_SD_N_", sd_random_values, 
      "_SD_ppd_", sd_ppd, 
      "_output_analysis_sim_", census_data_index, ".RData"
    ))
    
  }, error = function(e) {
    warning(paste("Error in simulation", census_data_index, ":", conditionMessage(e)))
  })
}


# -----------------------------------------------------------------------------
#7. Census data simulation on SD lambda_td 0.5 and SD ppd 0.15
# -----------------------------------------------------------------------------

# Initialize the vector to store results
census_data <- list()

# Loop through simulations
for (sim in 1:nsimulations) {
  
  # Define standard deviations for lambda_td by random values that multiplies lambda_td and for ppd
  sd_random_values <- 0.5  # SD for random values affecting population growth
  sd_ppd <- 0.15  # SD for persistence probability
  
  # Generate random values with truncation ensuring values are greater than 0
  random_values <- rtruncnorm(n = D, a = 0, mean = 1, sd = sd_random_values)
  
  # Save the generated random values for this simulation
  saveRDS(random_values, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                       "_SD_N_", sd_random_values, "_sd_ppd_", sd_ppd, 
                                       "_sd_random_values_sim_", sim, ".RData"))
  
  # Generate and save simulated lambdas_td (lambdasim)
  for (j in 1:D) {
    lambdasim <- monthly_trend * random_values[j]  # Adjusting growth rate with random values
    assign(paste0("lambdasim", j), lambdasim)  # Assign lambda value dynamically
    
    # Save lambdasim for each time step j
    saveRDS(lambdasim, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                     "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                     "_lambdasim", j, "_sim_", sim, ".RData"))
  }
  
  # Generate total number of roadkill (N_itd) based on lambdasim
  for (j in 1:D) {
    lambdasim_name <- paste0("lambdasim", j)
    lambdasim_current <- get(lambdasim_name)  # Retrieve current lambda_td value
    
    # Initialize matrix for total number of roadkill
    N_itd <- matrix(NA, nrow = nroad_transects, ncol = nmonth)
    
    for (t in 1:nmonth) {
      if (length(lambdasim_current) >= t) {
        N_itd[, t] <- rpois(nroad_transects, lambda = lambdasim_current[t])  # equation 1
      }
    }
    
    # Save the matrix
    assign(paste0("N_itd", j), N_itd)
    saveRDS(N_itd, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                 "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                 "_N_itd", j, "_sim_", sim, ".RData"))
  }
  
  # Generate roadkill numbers located within the road (N2_itd)
  for (j in 1:D) {
    N_itd_name <- paste0("N_itd", j)
    N_itd_current <- get(N_itd_name)
    
    N2_itd <- matrix(NA, nrow = nroad_transects, ncol = nmonth)
    
    for (i in 1:nroad_transects) {
      for (t in 1:nmonth) {
        N2_itd[i, t] <- rbinom(1, N_itd_current[i, t], selected_group$pl_group)
      }
    }
    
    assign(paste0("N2_itd", j), N2_itd)
    # Save the matrix
    saveRDS(N2_itd, file=paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                "_N2_itd", j, "_sim_", sim, ".RData"))
  }
  
  
  # Generate random daily carcass persistence probabilities along sd_ppd
  ppd <- rnorm(D, mean = selected_group$ppd_group, sd = sd_ppd)
  
  # Ensure that the product of all ppd values is greater than 0.05  
  # and no ppd value is greater than or equal to 1  
  while (prod(ppd) <= 0.05 || any(ppd >= 1)) { 
    ppd <- rnorm(D, mean = selected_group$ppd_group, sd = sd_ppd)
    ppd[ppd > 1] <- 1  
  }
  
  # Save the generated ppd values
  saveRDS(ppd, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                             "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                             "_ppd_sim_", sim, ".RData"))
  
  # Initialize N3_itD matrix
  N3_itD <- matrix(0, nrow = nroad_transects, ncol = nmonth)
  
  # Generate N3_itd matrix
  for (j in 1:D) {
    ppd_t <- if (j == 1) prod(ppd) else prod(ppd[j:length(ppd)])  # Adjust ppd according to the age in days of the carcasses
    
    # Initialize N3_itd for each day across D
    N3_itd <- matrix(NA, nrow = nroad_transects, ncol = nmonth)
    
    # Retrieve the N_itd for the current day
    N2_itd_name <- paste0("N2_itd", j)
    N2_itd_current <- get(N2_itd_name)
    
    # Generate the N3_itd matrix based on binomial distribution
    for (i in 1:nroad_transects) {
      for (t in 1:nmonth) {
        N3_itd[i, t] <- rbinom(1, N2_itd_current[i, t], ppd_t)  # equation 3
      }
    }
    
    # Save the N3_itd matrix for each day in D-day period
    assign(paste0("N3_itd", j), N3_itd)
    saveRDS(N3_itd, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                  "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                  "_N3_itd", j, "_sim_", sim, ".RData"))
    
    # Accumulate each N3_itd in N3_itD
    N3_itD <- N3_itD + N3_itd
    saveRDS(N3_itD, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                  "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                  "_N3_itD_sim_", sim, ".RData"))
  }
  
  
  # Census data simulation for each survey methods
  nsurveys <- 3  # Number of surveys
  
  
  # Initialize census data array for 3 methods
  C <- array(dim = c(nroad_transects, nmonth, nmethod, nsurveys))
  
  for (m in 1:3) {
    for (k in 1:nsurveys) {
      C[,,m,k] <- matrix(rbinom(nroad_transects * nmonth, N3_itD, selected_group$pom_group[m]), nrow = nroad_transects, ncol = nmonth)
    }
  }
  
  # Save the generated census data for the current simulation
  saveRDS(C, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                           "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                           "_census_data_sim_", sim, ".RData"))
  
  # Store the generated census data in the list
  census_data[[sim]] <- C
}



# Loop over all census data sets
for (census_data_index in seq_along(census_data)) {
  
  tryCatch({
    # Access the current census data for processing
    current_census_data <- census_data[[census_data_index]]
    print(paste("Running simulation", census_data_index, "for SD LAMBDAS", sd_random_values, "and SD ppd", sd_ppd, "."))
    
    # Prepare the data for the model
    bdata <- list(
      C = current_census_data,                         # Observation data
      nroad_transects = dim(current_census_data)[1],   # Number of road transects
      nmonth = dim(current_census_data)[2],            # Number of months
      nmet = dim(current_census_data)[3],              # Number of survey methods
      nsurveys = dim(current_census_data)[4],          # Number of surveys
      alpha_prior_pl = alpha_prior_pl,                 # Prior parameter alpha for carcass location probability
      beta_prior_pl = beta_prior_pl,                   # Prior parameter beta for carcass location probability
      alpha_prior_pp = alpha_prior_pp,                 # Prior parameter alpha for carcass persistence probability
      beta_prior_pp = beta_prior_pp                    # Prior parameter beta for carcass persistence probability
    )
    
    # Write the JAGS model to a file
    sink(paste0("modelplpppo(m)_census_data", census_data_index, ".txt"))
    cat("
      model {
        # Priors
        pl ~ dbeta(alpha_prior_pl, beta_prior_pl)  # Carcass location probability
        pp ~ dbeta(alpha_prior_pp, beta_prior_pp)  # Carcass persistence probability
        
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
            N2[i,t] ~ dbin(pl, N[i,t])T(0,100000)          # Roadkills inside road
            N3[i, t] ~ dbin(pp, N2[i, t])T(0, 100000)      # Roadkills preserved until road survey
            
            for (m in 1:nmet) {
              for (j in 1:nsurveys) {
                C[i, t, m, j] ~ dbin(po[m], N3[i, t])T(0, 100000)  # Census data
              }
            }
          }
        }
      
        # Derived quantities: Total number of roadkill across all surveyed road transects per month
        for (t in 1:nmonth) {
          totalN[t] <- sum(N[, t]) 
          totalN2[t] <- sum(N2[,t])
          totalN3[t] <- sum(N3[, t])   
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
        pp = rbeta(1, alpha_prior_pp, beta_prior_pp),        # Initial carcass persistence probability
        po = runif(3),                                       # Initial carcass observation probability
        N = Nst                                              # Initial roadkill number
      )
    }
    
    # Specify parameters to monitor in the MCMC
    params <- c("lambda", "pl", "pp", "po", "totalN", "totalN2", "totalN3")
    
    # MCMC settings for the JAGS model
    na <- 100000  # Number of adaptation steps
    ni <- 400000  # Number of iterations
    nt <- 1000    # Thinning interval
    nb <- 100000  # Burn-in period
    nc <- 3       # Number of chains
    
    # Run the JAGS model, check convergence, and summarize posterior distributions
    output <- jags(
      bdata, inits, params, 
      paste0("modelplpppo(m)_census_data", census_data_index, ".txt"),
      n.adapt = na, n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE
    )
    
    # Save the output to a file for later analysis
    saveRDS(output, file = paste0(
      save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, 
      "_nroad_transects_", nroad_transects, 
      "_SD_N_", sd_random_values, 
      "_SD_ppd_", sd_ppd, 
      "_output_analysis_sim_", census_data_index, ".RData"
    ))
    
  }, error = function(e) {
    warning(paste("Error in simulation", census_data_index, ":", conditionMessage(e)))
  })
}

# -----------------------------------------------------------------------------
#8. Census data simulation on SD lambda_td 0.5 and SD ppd 0
# -----------------------------------------------------------------------------

# Initialize the vector to store results
census_data <- list()

# Loop through simulations
for (sim in 1:nsimulations) {
  
  # Define standard deviations for lambda_td by random values that multiplies lambda_td and for ppd
  sd_random_values <- 0.5  # SD for random values affecting population growth
  sd_ppd <- 0  # SD for persistence probability
  
  # Generate random values with truncation ensuring values are greater than 0
  random_values <- rtruncnorm(n = D, a = 0, mean = 1, sd = sd_random_values)
  
  # Save the generated random values for this simulation
  saveRDS(random_values, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                       "_SD_N_", sd_random_values, "_sd_ppd_", sd_ppd, 
                                       "_sd_random_values_sim_", sim, ".RData"))
  
  # Generate and save simulated lambdas_td (lambdasim)
  for (j in 1:D) {
    lambdasim <- monthly_trend * random_values[j]  # Adjusting growth rate with random values
    assign(paste0("lambdasim", j), lambdasim)  # Assign lambda value dynamically
    
    # Save lambdasim for each time step j
    saveRDS(lambdasim, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                     "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                     "_lambdasim", j, "_sim_", sim, ".RData"))
  }
  
  # Generate total number of roadkill (N_itd) based on lambdasim
  for (j in 1:D) {
    lambdasim_name <- paste0("lambdasim", j)
    lambdasim_current <- get(lambdasim_name)  # Retrieve current lambda_td value
    
    # Initialize matrix for total number of roadkill
    N_itd <- matrix(NA, nrow = nroad_transects, ncol = nmonth)
    
    for (t in 1:nmonth) {
      if (length(lambdasim_current) >= t) {
        N_itd[, t] <- rpois(nroad_transects, lambda = lambdasim_current[t])  # equation 1
      }
    }
    
    # Save the matrix
    assign(paste0("N_itd", j), N_itd)
    saveRDS(N_itd, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                 "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                 "_N_itd", j, "_sim_", sim, ".RData"))
  }
  
  # Generate roadkill numbers located within the road (N2_itd)
  for (j in 1:D) {
    N_itd_name <- paste0("N_itd", j)
    N_itd_current <- get(N_itd_name)
    
    N2_itd <- matrix(NA, nrow = nroad_transects, ncol = nmonth)
    
    for (i in 1:nroad_transects) {
      for (t in 1:nmonth) {
        N2_itd[i, t] <- rbinom(1, N_itd_current[i, t], selected_group$pl_group)
      }
    }
    
    assign(paste0("N2_itd", j), N2_itd)
    # Save the matrix
    saveRDS(N2_itd, file=paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                "_N2_itd", j, "_sim_", sim, ".RData"))
  }
  
  
  # Generate random daily carcass persistence probabilities along sd_ppd
  ppd <- rnorm(D, mean = selected_group$ppd_group, sd = sd_ppd)
  
  # Ensure that the product of all ppd values is greater than 0.05  
  # and no ppd value is greater than or equal to 1  
  #while (prod(ppd) <= 0.05 || any(ppd >= 1)) { 
  #  ppd <- rnorm(D, mean = selected_group$ppd_group, sd = sd_ppd)
  #  ppd[ppd > 1] <- 1  
  #}
  
  # Save the generated ppd values
  saveRDS(ppd, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                             "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                             "_ppd_sim_", sim, ".RData"))
  
  # Initialize N3_itD matrix
  N3_itD <- matrix(0, nrow = nroad_transects, ncol = nmonth)
  
  # Generate N3_itd matrix
  for (j in 1:D) {
    ppd_t <- if (j == 1) prod(ppd) else prod(ppd[j:length(ppd)])  # Adjust ppd according to the age in days of the carcasses
    
    # Initialize N3_itd for each day across D
    N3_itd <- matrix(NA, nrow = nroad_transects, ncol = nmonth)
    
    # Retrieve the N_itd for the current day
    N2_itd_name <- paste0("N2_itd", j)
    N2_itd_current <- get(N2_itd_name)
    
    # Generate the N3_itd matrix based on binomial distribution
    for (i in 1:nroad_transects) {
      for (t in 1:nmonth) {
        N3_itd[i, t] <- rbinom(1, N2_itd_current[i, t], ppd_t)  # equation 3
      }
    }
    
    # Save the N3_itd matrix for each day in D-day period
    assign(paste0("N3_itd", j), N3_itd)
    saveRDS(N3_itd, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                  "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                  "_N3_itd", j, "_sim_", sim, ".RData"))
    
    # Accumulate each N3_itd in N3_itD
    N3_itD <- N3_itD + N3_itd
    saveRDS(N3_itD, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                  "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                  "_N3_itD_sim_", sim, ".RData"))
  }
  
  
  # Census data simulation for each survey methods
  nsurveys <- 3  # Number of surveys
  
  
  # Initialize census data array for 3 methods
  C <- array(dim = c(nroad_transects, nmonth, nmethod, nsurveys))
  
  for (m in 1:3) {
    for (k in 1:nsurveys) {
      C[,,m,k] <- matrix(rbinom(nroad_transects * nmonth, N3_itD, selected_group$pom_group[m]), nrow = nroad_transects, ncol = nmonth)
    }
  }
  
  # Save the generated census data for the current simulation
  saveRDS(C, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                           "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                           "_census_data_sim_", sim, ".RData"))
  
  # Store the generated census data in the list
  census_data[[sim]] <- C
}



# Loop over all census data sets
for (census_data_index in seq_along(census_data)) {
  
  tryCatch({
    # Access the current census data for processing
    current_census_data <- census_data[[census_data_index]]
    print(paste("Running simulation", census_data_index, "for SD LAMBDAS", sd_random_values, "and SD ppd", sd_ppd, "."))
    
    # Prepare the data for the model
    bdata <- list(
      C = current_census_data,                         # Observation data
      nroad_transects = dim(current_census_data)[1],   # Number of road transects
      nmonth = dim(current_census_data)[2],            # Number of months
      nmet = dim(current_census_data)[3],              # Number of survey methods
      nsurveys = dim(current_census_data)[4],          # Number of surveys
      alpha_prior_pl = alpha_prior_pl,                 # Prior parameter alpha for carcass location probability
      beta_prior_pl = beta_prior_pl,                   # Prior parameter beta for carcass location probability
      alpha_prior_pp = alpha_prior_pp,                 # Prior parameter alpha for carcass persistence probability
      beta_prior_pp = beta_prior_pp                    # Prior parameter beta for carcass persistence probability
    )
    
    # Write the JAGS model to a file
    sink(paste0("modelplpppo(m)_census_data", census_data_index, ".txt"))
    cat("
      model {
        # Priors
        pl ~ dbeta(alpha_prior_pl, beta_prior_pl)  # Carcass location probability
        pp ~ dbeta(alpha_prior_pp, beta_prior_pp)  # Carcass persistence probability
        
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
            N2[i,t] ~ dbin(pl, N[i,t])T(0,100000)          # Roadkills inside road
            N3[i, t] ~ dbin(pp, N2[i, t])T(0, 100000)      # Roadkills preserved until road survey
            
            for (m in 1:nmet) {
              for (j in 1:nsurveys) {
                C[i, t, m, j] ~ dbin(po[m], N3[i, t])T(0, 100000)  # Census data
              }
            }
          }
        }
      
        # Derived quantities: Total number of roadkill across all surveyed road transects per month
        for (t in 1:nmonth) {
          totalN[t] <- sum(N[, t]) 
          totalN2[t] <- sum(N2[,t])
          totalN3[t] <- sum(N3[, t])   
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
        pp = rbeta(1, alpha_prior_pp, beta_prior_pp),        # Initial carcass persistence probability
        po = runif(3),                                       # Initial carcass observation probability
        N = Nst                                              # Initial roadkill number
      )
    }
    
    # Specify parameters to monitor in the MCMC
    params <- c("lambda", "pl", "pp", "po", "totalN", "totalN2", "totalN3")
    
    # MCMC settings for the JAGS model
    na <- 100000  # Number of adaptation steps
    ni <- 400000  # Number of iterations
    nt <- 1000    # Thinning interval
    nb <- 100000  # Burn-in period
    nc <- 3       # Number of chains
    
    # Run the JAGS model, check convergence, and summarize posterior distributions
    output <- jags(
      bdata, inits, params, 
      paste0("modelplpppo(m)_census_data", census_data_index, ".txt"),
      n.adapt = na, n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE
    )
    
    # Save the output to a file for later analysis
    saveRDS(output, file = paste0(
      save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, 
      "_nroad_transects_", nroad_transects, 
      "_SD_N_", sd_random_values, 
      "_SD_ppd_", sd_ppd, 
      "_output_analysis_sim_", census_data_index, ".RData"
    ))
    
  }, error = function(e) {
    warning(paste("Error in simulation", census_data_index, ":", conditionMessage(e)))
  })
}

# -----------------------------------------------------------------------------
#9. Census data simulation on SD lambda_td 1.5 and SD ppd 0.05
# -----------------------------------------------------------------------------

# Initialize the vector to store results
census_data <- list()

# Loop through simulations
for (sim in 1:nsimulations) {
  
  # Define standard deviations for lambda_td by random values that multiplies lambda_td and for ppd
  sd_random_values <- 1.5  # SD for random values affecting population growth
  sd_ppd <- 0.05  # SD for persistence probability
  
  # Generate random values with truncation ensuring values are greater than 0
  random_values <- rtruncnorm(n = D, a = 0, mean = 1, sd = sd_random_values)
  
  # Save the generated random values for this simulation
  saveRDS(random_values, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                       "_SD_N_", sd_random_values, "_sd_ppd_", sd_ppd, 
                                       "_sd_random_values_sim_", sim, ".RData"))
  
  # Generate and save simulated lambdas_td (lambdasim)
  for (j in 1:D) {
    lambdasim <- monthly_trend * random_values[j]  # Adjusting growth rate with random values
    assign(paste0("lambdasim", j), lambdasim)  # Assign lambda value dynamically
    
    # Save lambdasim for each time step j
    saveRDS(lambdasim, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                     "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                     "_lambdasim", j, "_sim_", sim, ".RData"))
  }
  
  # Generate total number of roadkill (N_itd) based on lambdasim
  for (j in 1:D) {
    lambdasim_name <- paste0("lambdasim", j)
    lambdasim_current <- get(lambdasim_name)  # Retrieve current lambda_td value
    
    # Initialize matrix for total number of roadkill
    N_itd <- matrix(NA, nrow = nroad_transects, ncol = nmonth)
    
    for (t in 1:nmonth) {
      if (length(lambdasim_current) >= t) {
        N_itd[, t] <- rpois(nroad_transects, lambda = lambdasim_current[t])  # equation 1
      }
    }
    
    # Save the matrix
    assign(paste0("N_itd", j), N_itd)
    saveRDS(N_itd, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                 "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                 "_N_itd", j, "_sim_", sim, ".RData"))
  }
  
  # Generate roadkill numbers located within the road (N2_itd)
  for (j in 1:D) {
    N_itd_name <- paste0("N_itd", j)
    N_itd_current <- get(N_itd_name)
    
    N2_itd <- matrix(NA, nrow = nroad_transects, ncol = nmonth)
    
    for (i in 1:nroad_transects) {
      for (t in 1:nmonth) {
        N2_itd[i, t] <- rbinom(1, N_itd_current[i, t], selected_group$pl_group)
      }
    }
    
    assign(paste0("N2_itd", j), N2_itd)
    # Save the matrix
    saveRDS(N2_itd, file=paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                "_N2_itd", j, "_sim_", sim, ".RData"))
  }
  
  
  # Generate random daily carcass persistence probabilities along sd_ppd
  ppd <- rnorm(D, mean = selected_group$ppd_group, sd = sd_ppd)
  
  # Ensure that the product of all ppd values is greater than 0.05  
  # and no ppd value is greater than or equal to 1  
  while (prod(ppd) <= 0.05 || any(ppd >= 1)) { 
    ppd <- rnorm(D, mean = selected_group$ppd_group, sd = sd_ppd)
    ppd[ppd > 1] <- 1  
  }
  
  
  # Save the generated ppd values
  saveRDS(ppd, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                             "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                             "_ppd_sim_", sim, ".RData"))
  
  # Initialize N3_itD matrix
  N3_itD <- matrix(0, nrow = nroad_transects, ncol = nmonth)
  
  # Generate N3_itd matrix
  for (j in 1:D) {
    ppd_t <- if (j == 1) prod(ppd) else prod(ppd[j:length(ppd)])  # Adjust ppd according to the age in days of the carcasses
    
    # Initialize N3_itd for each day across D
    N3_itd <- matrix(NA, nrow = nroad_transects, ncol = nmonth)
    
    # Retrieve the N_itd for the current day
    N2_itd_name <- paste0("N2_itd", j)
    N2_itd_current <- get(N2_itd_name)
    
    # Generate the N3_itd matrix based on binomial distribution
    for (i in 1:nroad_transects) {
      for (t in 1:nmonth) {
        N3_itd[i, t] <- rbinom(1, N2_itd_current[i, t], ppd_t)  # equation 3
      }
    }
    
    # Save the N3_itd matrix for each day in D-day period
    assign(paste0("N3_itd", j), N3_itd)
    saveRDS(N3_itd, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                  "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                  "_N3_itd", j, "_sim_", sim, ".RData"))
    
    # Accumulate each N3_itd in N3_itD
    N3_itD <- N3_itD + N3_itd
    saveRDS(N3_itD, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                  "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                  "_N3_itD_sim_", sim, ".RData"))
  }
  
  
  # Census data simulation for each survey methods
  nsurveys <- 3  # Number of surveys
  
  
  # Initialize census data array for 3 methods
  C <- array(dim = c(nroad_transects, nmonth, nmethod, nsurveys))
  
  for (m in 1:3) {
    for (k in 1:nsurveys) {
      C[,,m,k] <- matrix(rbinom(nroad_transects * nmonth, N3_itD, selected_group$pom_group[m]), nrow = nroad_transects, ncol = nmonth)
    }
  }
  
  # Save the generated census data for the current simulation
  saveRDS(C, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                           "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                           "_census_data_sim_", sim, ".RData"))
  
  # Store the generated census data in the list
  census_data[[sim]] <- C
}



# Loop over all census data sets
for (census_data_index in seq_along(census_data)) {
  
  tryCatch({
    # Access the current census data for processing
    current_census_data <- census_data[[census_data_index]]
    print(paste("Running simulation", census_data_index, "for SD LAMBDAS", sd_random_values, "and SD ppd", sd_ppd, "."))
    
    # Prepare the data for the model
    bdata <- list(
      C = current_census_data,                         # Observation data
      nroad_transects = dim(current_census_data)[1],   # Number of road transects
      nmonth = dim(current_census_data)[2],            # Number of months
      nmet = dim(current_census_data)[3],              # Number of survey methods
      nsurveys = dim(current_census_data)[4],          # Number of surveys
      alpha_prior_pl = alpha_prior_pl,                 # Prior parameter alpha for carcass location probability
      beta_prior_pl = beta_prior_pl,                   # Prior parameter beta for carcass location probability
      alpha_prior_pp = alpha_prior_pp,                 # Prior parameter alpha for carcass persistence probability
      beta_prior_pp = beta_prior_pp                    # Prior parameter beta for carcass persistence probability
    )
    
    # Write the JAGS model to a file
    sink(paste0("modelplpppo(m)_census_data", census_data_index, ".txt"))
    cat("
      model {
        # Priors
        pl ~ dbeta(alpha_prior_pl, beta_prior_pl)  # Carcass location probability
        pp ~ dbeta(alpha_prior_pp, beta_prior_pp)  # Carcass persistence probability
        
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
            N2[i,t] ~ dbin(pl, N[i,t])T(0,100000)          # Roadkills inside road
            N3[i, t] ~ dbin(pp, N2[i, t])T(0, 100000)      # Roadkills preserved until road survey
            
            for (m in 1:nmet) {
              for (j in 1:nsurveys) {
                C[i, t, m, j] ~ dbin(po[m], N3[i, t])T(0, 100000)  # Census data
              }
            }
          }
        }
      
        # Derived quantities: Total number of roadkill across all surveyed road transects per month
        for (t in 1:nmonth) {
          totalN[t] <- sum(N[, t]) 
          totalN2[t] <- sum(N2[,t])
          totalN3[t] <- sum(N3[, t])   
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
        pp = rbeta(1, alpha_prior_pp, beta_prior_pp),        # Initial carcass persistence probability
        po = runif(3),                                       # Initial carcass observation probability
        N = Nst                                              # Initial roadkill number
      )
    }
    
    # Specify parameters to monitor in the MCMC
    params <- c("lambda", "pl", "pp", "po", "totalN", "totalN2", "totalN3")
    
    # MCMC settings for the JAGS model
    na <- 100000  # Number of adaptation steps
    ni <- 400000  # Number of iterations
    nt <- 1000    # Thinning interval
    nb <- 100000  # Burn-in period
    nc <- 3       # Number of chains
    
    # Run the JAGS model, check convergence, and summarize posterior distributions
    output <- jags(
      bdata, inits, params, 
      paste0("modelplpppo(m)_census_data", census_data_index, ".txt"),
      n.adapt = na, n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE
    )
    
    # Save the output to a file for later analysis
    saveRDS(output, file = paste0(
      save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, 
      "_nroad_transects_", nroad_transects, 
      "_SD_N_", sd_random_values, 
      "_SD_ppd_", sd_ppd, 
      "_output_analysis_sim_", census_data_index, ".RData"
    ))
    
  }, error = function(e) {
    warning(paste("Error in simulation", census_data_index, ":", conditionMessage(e)))
  })
}


# -----------------------------------------------------------------------------
#10. Census data simulation on SD lambda_td 1.5 and SD ppd 0.15
# -----------------------------------------------------------------------------

# Initialize the vector to store results
census_data <- list()

# Loop through simulations
for (sim in 1:nsimulations) {
  
  # Define standard deviations for lambda_td by random values that multiplies lambda_td and for ppd
  sd_random_values <- 1.5  # SD for random values affecting population growth
  sd_ppd <- 0.15  # SD for persistence probability
  
  # Generate random values with truncation ensuring values are greater than 0
  random_values <- rtruncnorm(n = D, a = 0, mean = 1, sd = sd_random_values)
  
  # Save the generated random values for this simulation
  saveRDS(random_values, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                       "_SD_N_", sd_random_values, "_sd_ppd_", sd_ppd, 
                                       "_sd_random_values_sim_", sim, ".RData"))
  
  # Generate and save simulated lambdas_td (lambdasim)
  for (j in 1:D) {
    lambdasim <- monthly_trend * random_values[j]  # Adjusting growth rate with random values
    assign(paste0("lambdasim", j), lambdasim)  # Assign lambda value dynamically
    
    # Save lambdasim for each time step j
    saveRDS(lambdasim, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                     "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                     "_lambdasim", j, "_sim_", sim, ".RData"))
  }
  
  # Generate total number of roadkill (N_itd) based on lambdasim
  for (j in 1:D) {
    lambdasim_name <- paste0("lambdasim", j)
    lambdasim_current <- get(lambdasim_name)  # Retrieve current lambda_td value
    
    # Initialize matrix for total number of roadkill
    N_itd <- matrix(NA, nrow = nroad_transects, ncol = nmonth)
    
    for (t in 1:nmonth) {
      if (length(lambdasim_current) >= t) {
        N_itd[, t] <- rpois(nroad_transects, lambda = lambdasim_current[t])  # equation 1
      }
    }
    
    # Save the matrix
    assign(paste0("N_itd", j), N_itd)
    saveRDS(N_itd, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                 "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                 "_N_itd", j, "_sim_", sim, ".RData"))
  }
  
  # Generate roadkill numbers located within the road (N2_itd)
  for (j in 1:D) {
    N_itd_name <- paste0("N_itd", j)
    N_itd_current <- get(N_itd_name)
    
    N2_itd <- matrix(NA, nrow = nroad_transects, ncol = nmonth)
    
    for (i in 1:nroad_transects) {
      for (t in 1:nmonth) {
        N2_itd[i, t] <- rbinom(1, N_itd_current[i, t], selected_group$pl_group)
      }
    }
    
    assign(paste0("N2_itd", j), N2_itd)
    # Save the matrix
    saveRDS(N2_itd, file=paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                "_N2_itd", j, "_sim_", sim, ".RData"))
  }
  
  
  # Generate random daily carcass persistence probabilities along sd_ppd
  ppd <- rnorm(D, mean = selected_group$ppd_group, sd = sd_ppd)
  
  # Ensure that the product of all ppd values is greater than 0.05  
  # and no ppd value is greater than or equal to 1  
  while (prod(ppd) <= 0.05 || any(ppd >= 1)) { 
    ppd <- rnorm(D, mean = selected_group$ppd_group, sd = sd_ppd)
    ppd[ppd > 1] <- 1  
  }
  
  
  # Save the generated ppd values
  saveRDS(ppd, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                             "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                             "_ppd_sim_", sim, ".RData"))
  
  # Initialize N3_itD matrix
  N3_itD <- matrix(0, nrow = nroad_transects, ncol = nmonth)
  
  # Generate N3_itd matrix
  for (j in 1:D) {
    ppd_t <- if (j == 1) prod(ppd) else prod(ppd[j:length(ppd)])  # Adjust ppd according to the age in days of the carcasses
    
    # Initialize N3_itd for each day across D
    N3_itd <- matrix(NA, nrow = nroad_transects, ncol = nmonth)
    
    # Retrieve the N_itd for the current day
    N2_itd_name <- paste0("N2_itd", j)
    N2_itd_current <- get(N2_itd_name)
    
    # Generate the N3_itd matrix based on binomial distribution
    for (i in 1:nroad_transects) {
      for (t in 1:nmonth) {
        N3_itd[i, t] <- rbinom(1, N2_itd_current[i, t], ppd_t)  # equation 3
      }
    }
    
    # Save the N3_itd matrix for each day in D-day period
    assign(paste0("N3_itd", j), N3_itd)
    saveRDS(N3_itd, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                  "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                  "_N3_itd", j, "_sim_", sim, ".RData"))
    
    # Accumulate each N3_itd in N3_itD
    N3_itD <- N3_itD + N3_itd
    saveRDS(N3_itD, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                  "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                  "_N3_itD_sim_", sim, ".RData"))
  }
  
  
  # Census data simulation for each survey methods
  nsurveys <- 3  # Number of surveys
  
  
  # Initialize census data array for 3 methods
  C <- array(dim = c(nroad_transects, nmonth, nmethod, nsurveys))
  
  for (m in 1:3) {
    for (k in 1:nsurveys) {
      C[,,m,k] <- matrix(rbinom(nroad_transects * nmonth, N3_itD, selected_group$pom_group[m]), nrow = nroad_transects, ncol = nmonth)
    }
  }
  
  # Save the generated census data for the current simulation
  saveRDS(C, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                           "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                           "_census_data_sim_", sim, ".RData"))
  
  # Store the generated census data in the list
  census_data[[sim]] <- C
}



# Loop over all census data sets
for (census_data_index in seq_along(census_data)) {
  
  tryCatch({
    # Access the current census data for processing
    current_census_data <- census_data[[census_data_index]]
    print(paste("Running simulation", census_data_index, "for SD LAMBDAS", sd_random_values, "and SD ppd", sd_ppd, "."))
    
    # Prepare the data for the model
    bdata <- list(
      C = current_census_data,                         # Observation data
      nroad_transects = dim(current_census_data)[1],   # Number of road transects
      nmonth = dim(current_census_data)[2],            # Number of months
      nmet = dim(current_census_data)[3],              # Number of survey methods
      nsurveys = dim(current_census_data)[4],          # Number of surveys
      alpha_prior_pl = alpha_prior_pl,                 # Prior parameter alpha for carcass location probability
      beta_prior_pl = beta_prior_pl,                   # Prior parameter beta for carcass location probability
      alpha_prior_pp = alpha_prior_pp,                 # Prior parameter alpha for carcass persistence probability
      beta_prior_pp = beta_prior_pp                    # Prior parameter beta for carcass persistence probability
    )
    
    # Write the JAGS model to a file
    sink(paste0("modelplpppo(m)_census_data", census_data_index, ".txt"))
    cat("
      model {
        # Priors
        pl ~ dbeta(alpha_prior_pl, beta_prior_pl)  # Carcass location probability
        pp ~ dbeta(alpha_prior_pp, beta_prior_pp)  # Carcass persistence probability
        
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
            N2[i,t] ~ dbin(pl, N[i,t])T(0,100000)          # Roadkills inside road
            N3[i, t] ~ dbin(pp, N2[i, t])T(0, 100000)      # Roadkills preserved until road survey
            
            for (m in 1:nmet) {
              for (j in 1:nsurveys) {
                C[i, t, m, j] ~ dbin(po[m], N3[i, t])T(0, 100000)  # Census data
              }
            }
          }
        }
      
        # Derived quantities: Total number of roadkill across all surveyed road transects per month
        for (t in 1:nmonth) {
          totalN[t] <- sum(N[, t]) 
          totalN2[t] <- sum(N2[,t])
          totalN3[t] <- sum(N3[, t])   
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
        pp = rbeta(1, alpha_prior_pp, beta_prior_pp),        # Initial carcass persistence probability
        po = runif(3),                                       # Initial carcass observation probability
        N = Nst                                              # Initial roadkill number
      )
    }
    
    # Specify parameters to monitor in the MCMC
    params <- c("lambda", "pl", "pp", "po", "totalN", "totalN2", "totalN3")
    
    # MCMC settings for the JAGS model
    na <- 100000  # Number of adaptation steps
    ni <- 400000  # Number of iterations
    nt <- 1000    # Thinning interval
    nb <- 100000  # Burn-in period
    nc <- 3       # Number of chains
    
    # Run the JAGS model, check convergence, and summarize posterior distributions
    output <- jags(
      bdata, inits, params, 
      paste0("modelplpppo(m)_census_data", census_data_index, ".txt"),
      n.adapt = na, n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE
    )
    
    # Save the output to a file for later analysis
    saveRDS(output, file = paste0(
      save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, 
      "_nroad_transects_", nroad_transects, 
      "_SD_N_", sd_random_values, 
      "_SD_ppd_", sd_ppd, 
      "_output_analysis_sim_", census_data_index, ".RData"
    ))
    
  }, error = function(e) {
    warning(paste("Error in simulation", census_data_index, ":", conditionMessage(e)))
  })
}


# -----------------------------------------------------------------------------
#11. Census data simulation on SD lambda_td 1.5 and SD ppd 0
# -----------------------------------------------------------------------------

# Initialize the vector to store results
census_data <- list()

# Loop through simulations
for (sim in 1:nsimulations) {
  
  # Define standard deviations for lambda_td by random values that multiplies lambda_td and for ppd
  sd_random_values <- 1.5  # SD for random values affecting population growth
  sd_ppd <- 0  # SD for persistence probability
  
  # Generate random values with truncation ensuring values are greater than 0
  random_values <- rtruncnorm(n = D, a = 0, mean = 1, sd = sd_random_values)
  
  # Save the generated random values for this simulation
  saveRDS(random_values, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                       "_SD_N_", sd_random_values, "_sd_ppd_", sd_ppd, 
                                       "_sd_random_values_sim_", sim, ".RData"))
  
  # Generate and save simulated lambdas_td (lambdasim)
  for (j in 1:D) {
    lambdasim <- monthly_trend * random_values[j]  # Adjusting growth rate with random values
    assign(paste0("lambdasim", j), lambdasim)  # Assign lambda value dynamically
    
    # Save lambdasim for each time step j
    saveRDS(lambdasim, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                     "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                     "_lambdasim", j, "_sim_", sim, ".RData"))
  }
  
  # Generate total number of roadkill (N_itd) based on lambdasim
  for (j in 1:D) {
    lambdasim_name <- paste0("lambdasim", j)
    lambdasim_current <- get(lambdasim_name)  # Retrieve current lambda_td value
    
    # Initialize matrix for total number of roadkill
    N_itd <- matrix(NA, nrow = nroad_transects, ncol = nmonth)
    
    for (t in 1:nmonth) {
      if (length(lambdasim_current) >= t) {
        N_itd[, t] <- rpois(nroad_transects, lambda = lambdasim_current[t])  # equation 1
      }
    }
    
    # Save the matrix
    assign(paste0("N_itd", j), N_itd)
    saveRDS(N_itd, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                 "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                 "_N_itd", j, "_sim_", sim, ".RData"))
  }
  
  # Generate roadkill numbers located within the road (N2_itd)
  for (j in 1:D) {
    N_itd_name <- paste0("N_itd", j)
    N_itd_current <- get(N_itd_name)
    
    N2_itd <- matrix(NA, nrow = nroad_transects, ncol = nmonth)
    
    for (i in 1:nroad_transects) {
      for (t in 1:nmonth) {
        N2_itd[i, t] <- rbinom(1, N_itd_current[i, t], selected_group$pl_group)
      }
    }
    
    assign(paste0("N2_itd", j), N2_itd)
    # Save the matrix
    saveRDS(N2_itd, file=paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                "_N2_itd", j, "_sim_", sim, ".RData"))
  }
  
  
  # Generate random daily carcass persistence probabilities along sd_ppd
  ppd <- rnorm(D, mean = selected_group$ppd_group, sd = sd_ppd)
  
  # Ensure that the product of all ppd values is greater than 0.05  
  # and no ppd value is greater than or equal to 1  
  #while (prod(ppd) <= 0.05 || any(ppd >= 1)) { 
  #  ppd <- rnorm(D, mean = selected_group$ppd_group, sd = sd_ppd)
  #  ppd[ppd > 1] <- 1  
  #}
  
  # Save the generated ppd values
  saveRDS(ppd, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                             "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                             "_ppd_sim_", sim, ".RData"))
  
  # Initialize N3_itD matrix
  N3_itD <- matrix(0, nrow = nroad_transects, ncol = nmonth)
  
  # Generate N3_itd matrix
  for (j in 1:D) {
    ppd_t <- if (j == 1) prod(ppd) else prod(ppd[j:length(ppd)])  # Adjust ppd according to the age in days of the carcasses
    
    # Initialize N3_itd for each day across D
    N3_itd <- matrix(NA, nrow = nroad_transects, ncol = nmonth)
    
    # Retrieve the N_itd for the current day
    N2_itd_name <- paste0("N2_itd", j)
    N2_itd_current <- get(N2_itd_name)
    
    # Generate the N3_itd matrix based on binomial distribution
    for (i in 1:nroad_transects) {
      for (t in 1:nmonth) {
        N3_itd[i, t] <- rbinom(1, N2_itd_current[i, t], ppd_t)  # equation 3
      }
    }
    
    # Save the N3_itd matrix for each day in D-day period
    assign(paste0("N3_itd", j), N3_itd)
    saveRDS(N3_itd, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                  "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                  "_N3_itd", j, "_sim_", sim, ".RData"))
    
    # Accumulate each N3_itd in N3_itD
    N3_itD <- N3_itD + N3_itd
    saveRDS(N3_itD, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                  "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                  "_N3_itD_sim_", sim, ".RData"))
  }
  
  
  # Census data simulation for each survey methods
  nsurveys <- 3  # Number of surveys
  
  
  # Initialize census data array for 3 methods
  C <- array(dim = c(nroad_transects, nmonth, nmethod, nsurveys))
  
  for (m in 1:3) {
    for (k in 1:nsurveys) {
      C[,,m,k] <- matrix(rbinom(nroad_transects * nmonth, N3_itD, selected_group$pom_group[m]), nrow = nroad_transects, ncol = nmonth)
    }
  }
  
  # Save the generated census data for the current simulation
  saveRDS(C, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                           "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                           "_census_data_sim_", sim, ".RData"))
  
  # Store the generated census data in the list
  census_data[[sim]] <- C
}



# Loop over all census data sets
for (census_data_index in seq_along(census_data)) {
  
  tryCatch({
    # Access the current census data for processing
    current_census_data <- census_data[[census_data_index]]
    print(paste("Running simulation", census_data_index, "for SD LAMBDAS", sd_random_values, "and SD ppd", sd_ppd, "."))
    
    # Prepare the data for the model
    bdata <- list(
      C = current_census_data,                         # Observation data
      nroad_transects = dim(current_census_data)[1],   # Number of road transects
      nmonth = dim(current_census_data)[2],            # Number of months
      nmet = dim(current_census_data)[3],              # Number of survey methods
      nsurveys = dim(current_census_data)[4],          # Number of surveys
      alpha_prior_pl = alpha_prior_pl,                 # Prior parameter alpha for carcass location probability
      beta_prior_pl = beta_prior_pl,                   # Prior parameter beta for carcass location probability
      alpha_prior_pp = alpha_prior_pp,                 # Prior parameter alpha for carcass persistence probability
      beta_prior_pp = beta_prior_pp                    # Prior parameter beta for carcass persistence probability
    )
    
    # Write the JAGS model to a file
    sink(paste0("modelplpppo(m)_census_data", census_data_index, ".txt"))
    cat("
      model {
        # Priors
        pl ~ dbeta(alpha_prior_pl, beta_prior_pl)  # Carcass location probability
        pp ~ dbeta(alpha_prior_pp, beta_prior_pp)  # Carcass persistence probability
        
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
            N2[i,t] ~ dbin(pl, N[i,t])T(0,100000)          # Roadkills inside road
            N3[i, t] ~ dbin(pp, N2[i, t])T(0, 100000)      # Roadkills preserved until road survey
            
            for (m in 1:nmet) {
              for (j in 1:nsurveys) {
                C[i, t, m, j] ~ dbin(po[m], N3[i, t])T(0, 100000)  # Census data
              }
            }
          }
        }
      
        # Derived quantities: Total number of roadkill across all surveyed road transects per month
        for (t in 1:nmonth) {
          totalN[t] <- sum(N[, t]) 
          totalN2[t] <- sum(N2[,t])
          totalN3[t] <- sum(N3[, t])   
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
        pp = rbeta(1, alpha_prior_pp, beta_prior_pp),        # Initial carcass persistence probability
        po = runif(3),                                       # Initial carcass observation probability
        N = Nst                                              # Initial roadkill number
      )
    }
    
    # Specify parameters to monitor in the MCMC
    params <- c("lambda", "pl", "pp", "po", "totalN", "totalN2", "totalN3")
    
    # MCMC settings for the JAGS model
    na <- 100000  # Number of adaptation steps
    ni <- 400000  # Number of iterations
    nt <- 1000    # Thinning interval
    nb <- 100000  # Burn-in period
    nc <- 3       # Number of chains
    
    # Run the JAGS model, check convergence, and summarize posterior distributions
    output <- jags(
      bdata, inits, params, 
      paste0("modelplpppo(m)_census_data", census_data_index, ".txt"),
      n.adapt = na, n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE
    )
    
    # Save the output to a file for later analysis
    saveRDS(output, file = paste0(
      save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, 
      "_nroad_transects_", nroad_transects, 
      "_SD_N_", sd_random_values, 
      "_SD_ppd_", sd_ppd, 
      "_output_analysis_sim_", census_data_index, ".RData"
    ))
    
  }, error = function(e) {
    warning(paste("Error in simulation", census_data_index, ":", conditionMessage(e)))
  })
}


# -----------------------------------------------------------------------------
#12. Census data simulation on SD lambda_td 0 and SD ppd 0.05
# -----------------------------------------------------------------------------

# Initialize the vector to store results
census_data <- list()

# Loop through simulations
for (sim in 1:nsimulations) {
  
  # Define standard deviations for lambda_td by random values that multiplies lambda_td and for ppd
  sd_random_values <- 0  # SD for random values affecting population growth
  sd_ppd <- 0.05  # SD for persistence probability
  
  # Generate random values with truncation ensuring values are greater than 0
  random_values <- rtruncnorm(n = D, a = 0, mean = 1, sd = sd_random_values)
  
  # Save the generated random values for this simulation
  saveRDS(random_values, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                       "_SD_N_", sd_random_values, "_sd_ppd_", sd_ppd, 
                                       "_sd_random_values_sim_", sim, ".RData"))
  
  # Generate and save simulated lambdas_td (lambdasim)
  for (j in 1:D) {
    lambdasim <- monthly_trend * random_values[j]  # Adjusting growth rate with random values
    assign(paste0("lambdasim", j), lambdasim)  # Assign lambda value dynamically
    
    # Save lambdasim for each time step j
    saveRDS(lambdasim, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                     "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                     "_lambdasim", j, "_sim_", sim, ".RData"))
  }
  
  # Generate total number of roadkill (N_itd) based on lambdasim
  for (j in 1:D) {
    lambdasim_name <- paste0("lambdasim", j)
    lambdasim_current <- get(lambdasim_name)  # Retrieve current lambda_td value
    
    # Initialize matrix for total number of roadkill
    N_itd <- matrix(NA, nrow = nroad_transects, ncol = nmonth)
    
    for (t in 1:nmonth) {
      if (length(lambdasim_current) >= t) {
        N_itd[, t] <- rpois(nroad_transects, lambda = lambdasim_current[t])  # equation 1
      }
    }
    
    # Save the matrix
    assign(paste0("N_itd", j), N_itd)
    saveRDS(N_itd, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                 "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                 "_N_itd", j, "_sim_", sim, ".RData"))
  }
  
  # Generate roadkill numbers located within the road (N2_itd)
  for (j in 1:D) {
    N_itd_name <- paste0("N_itd", j)
    N_itd_current <- get(N_itd_name)
    
    N2_itd <- matrix(NA, nrow = nroad_transects, ncol = nmonth)
    
    for (i in 1:nroad_transects) {
      for (t in 1:nmonth) {
        N2_itd[i, t] <- rbinom(1, N_itd_current[i, t], selected_group$pl_group)
      }
    }
    
    assign(paste0("N2_itd", j), N2_itd)
    # Save the matrix
    saveRDS(N2_itd, file=paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                "_N2_itd", j, "_sim_", sim, ".RData"))
  }
  
  
  # Generate random daily carcass persistence probabilities along sd_ppd
  ppd <- rnorm(D, mean = selected_group$ppd_group, sd = sd_ppd)
  
  # Ensure that the product of all ppd values is greater than 0.05  
  # and no ppd value is greater than or equal to 1  
  while (prod(ppd) <= 0.05 || any(ppd >= 1)) { 
    ppd <- rnorm(D, mean = selected_group$ppd_group, sd = sd_ppd)
    ppd[ppd > 1] <- 1  
  }
  
  # Save the generated ppd values
  saveRDS(ppd, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                             "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                             "_ppd_sim_", sim, ".RData"))
  
  # Initialize N3_itD matrix
  N3_itD <- matrix(0, nrow = nroad_transects, ncol = nmonth)
  
  # Generate N3_itd matrix
  for (j in 1:D) {
    ppd_t <- if (j == 1) prod(ppd) else prod(ppd[j:length(ppd)])  # Adjust ppd according to the age in days of the carcasses
    
    # Initialize N3_itd for each day across D
    N3_itd <- matrix(NA, nrow = nroad_transects, ncol = nmonth)
    
    # Retrieve the N_itd for the current day
    N2_itd_name <- paste0("N2_itd", j)
    N2_itd_current <- get(N2_itd_name)
    
    # Generate the N3_itd matrix based on binomial distribution
    for (i in 1:nroad_transects) {
      for (t in 1:nmonth) {
        N3_itd[i, t] <- rbinom(1, N2_itd_current[i, t], ppd_t)  # equation 3
      }
    }
    
    # Save the N3_itd matrix for each day in D-day period
    assign(paste0("N3_itd", j), N3_itd)
    saveRDS(N3_itd, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                  "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                  "_N3_itd", j, "_sim_", sim, ".RData"))
    
    # Accumulate each N3_itd in N3_itD
    N3_itD <- N3_itD + N3_itd
    saveRDS(N3_itD, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                  "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                  "_N3_itD_sim_", sim, ".RData"))
  }
  
  
  # Census data simulation for each survey methods
  nsurveys <- 3  # Number of surveys
  
  
  # Initialize census data array for 3 methods
  C <- array(dim = c(nroad_transects, nmonth, nmethod, nsurveys))
  
  for (m in 1:3) {
    for (k in 1:nsurveys) {
      C[,,m,k] <- matrix(rbinom(nroad_transects * nmonth, N3_itD, selected_group$pom_group[m]), nrow = nroad_transects, ncol = nmonth)
    }
  }
  
  # Save the generated census data for the current simulation
  saveRDS(C, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                           "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                           "_census_data_sim_", sim, ".RData"))
  
  # Store the generated census data in the list
  census_data[[sim]] <- C
}



# Loop over all census data sets
for (census_data_index in seq_along(census_data)) {
  
  tryCatch({
    # Access the current census data for processing
    current_census_data <- census_data[[census_data_index]]
    print(paste("Running simulation", census_data_index, "for SD LAMBDAS", sd_random_values, "and SD ppd", sd_ppd, "."))
    
    # Prepare the data for the model
    bdata <- list(
      C = current_census_data,                         # Observation data
      nroad_transects = dim(current_census_data)[1],   # Number of road transects
      nmonth = dim(current_census_data)[2],            # Number of months
      nmet = dim(current_census_data)[3],              # Number of survey methods
      nsurveys = dim(current_census_data)[4],          # Number of surveys
      alpha_prior_pl = alpha_prior_pl,                 # Prior parameter alpha for carcass location probability
      beta_prior_pl = beta_prior_pl,                   # Prior parameter beta for carcass location probability
      alpha_prior_pp = alpha_prior_pp,                 # Prior parameter alpha for carcass persistence probability
      beta_prior_pp = beta_prior_pp                    # Prior parameter beta for carcass persistence probability
    )
    
    # Write the JAGS model to a file
    sink(paste0("modelplpppo(m)_census_data", census_data_index, ".txt"))
    cat("
      model {
        # Priors
        pl ~ dbeta(alpha_prior_pl, beta_prior_pl)  # Carcass location probability
        pp ~ dbeta(alpha_prior_pp, beta_prior_pp)  # Carcass persistence probability
        
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
            N2[i,t] ~ dbin(pl, N[i,t])T(0,100000)          # Roadkills inside road
            N3[i, t] ~ dbin(pp, N2[i, t])T(0, 100000)      # Roadkills preserved until road survey
            
            for (m in 1:nmet) {
              for (j in 1:nsurveys) {
                C[i, t, m, j] ~ dbin(po[m], N3[i, t])T(0, 100000)  # Census data
              }
            }
          }
        }
      
        # Derived quantities: Total number of roadkill across all surveyed road transects per month
        for (t in 1:nmonth) {
          totalN[t] <- sum(N[, t]) 
          totalN2[t] <- sum(N2[,t])
          totalN3[t] <- sum(N3[, t])   
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
        pp = rbeta(1, alpha_prior_pp, beta_prior_pp),        # Initial carcass persistence probability
        po = runif(3),                                       # Initial carcass observation probability
        N = Nst                                              # Initial roadkill number
      )
    }
    
    # Specify parameters to monitor in the MCMC
    params <- c("lambda", "pl", "pp", "po", "totalN", "totalN2", "totalN3")
    
    # MCMC settings for the JAGS model
    na <- 100000  # Number of adaptation steps
    ni <- 400000  # Number of iterations
    nt <- 1000    # Thinning interval
    nb <- 100000  # Burn-in period
    nc <- 3       # Number of chains
    
    # Run the JAGS model, check convergence, and summarize posterior distributions
    output <- jags(
      bdata, inits, params, 
      paste0("modelplpppo(m)_census_data", census_data_index, ".txt"),
      n.adapt = na, n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE
    )
    
    # Save the output to a file for later analysis
    saveRDS(output, file = paste0(
      save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, 
      "_nroad_transects_", nroad_transects, 
      "_SD_N_", sd_random_values, 
      "_SD_ppd_", sd_ppd, 
      "_output_analysis_sim_", census_data_index, ".RData"
    ))
    
  }, error = function(e) {
    warning(paste("Error in simulation", census_data_index, ":", conditionMessage(e)))
  })
}

# -----------------------------------------------------------------------------
#13. Census data simulation on SD lambda_td 0 and SD ppd 0.15
# -----------------------------------------------------------------------------

# Initialize the vector to store results
census_data <- list()

# Loop through simulations
for (sim in 1:nsimulations) {
  
  # Define standard deviations for lambda_td by random values that multiplies lambda_td and for ppd
  sd_random_values <- 0  # SD for random values affecting population growth
  sd_ppd <- 0.15  # SD for persistence probability
  
  # Generate random values with truncation ensuring values are greater than 0
  random_values <- rtruncnorm(n = D, a = 0, mean = 1, sd = sd_random_values)
  
  # Save the generated random values for this simulation
  saveRDS(random_values, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                       "_SD_N_", sd_random_values, "_sd_ppd_", sd_ppd, 
                                       "_sd_random_values_sim_", sim, ".RData"))
  
  # Generate and save simulated lambdas_td (lambdasim)
  for (j in 1:D) {
    lambdasim <- monthly_trend * random_values[j]  # Adjusting growth rate with random values
    assign(paste0("lambdasim", j), lambdasim)  # Assign lambda value dynamically
    
    # Save lambdasim for each time step j
    saveRDS(lambdasim, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                     "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                     "_lambdasim", j, "_sim_", sim, ".RData"))
  }
  
  # Generate total number of roadkill (N_itd) based on lambdasim
  for (j in 1:D) {
    lambdasim_name <- paste0("lambdasim", j)
    lambdasim_current <- get(lambdasim_name)  # Retrieve current lambda_td value
    
    # Initialize matrix for total number of roadkill
    N_itd <- matrix(NA, nrow = nroad_transects, ncol = nmonth)
    
    for (t in 1:nmonth) {
      if (length(lambdasim_current) >= t) {
        N_itd[, t] <- rpois(nroad_transects, lambda = lambdasim_current[t])  # equation 1
      }
    }
    
    # Save the matrix
    assign(paste0("N_itd", j), N_itd)
    saveRDS(N_itd, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                 "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                 "_N_itd", j, "_sim_", sim, ".RData"))
  }
  
  # Generate roadkill numbers located within the road (N2_itd)
  for (j in 1:D) {
    N_itd_name <- paste0("N_itd", j)
    N_itd_current <- get(N_itd_name)
    
    N2_itd <- matrix(NA, nrow = nroad_transects, ncol = nmonth)
    
    for (i in 1:nroad_transects) {
      for (t in 1:nmonth) {
        N2_itd[i, t] <- rbinom(1, N_itd_current[i, t], selected_group$pl_group)
      }
    }
    
    assign(paste0("N2_itd", j), N2_itd)
    # Save the matrix
    saveRDS(N2_itd, file=paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                "_N2_itd", j, "_sim_", sim, ".RData"))
  }
  
  
  # Generate random daily carcass persistence probabilities along sd_ppd
  ppd <- rnorm(D, mean = selected_group$ppd_group, sd = sd_ppd)
  
  # Ensure that the product of all ppd values is greater than 0.05  
  # and no ppd value is greater than or equal to 1  
  while (prod(ppd) <= 0.05 || any(ppd >= 1)) { 
    ppd <- rnorm(D, mean = selected_group$ppd_group, sd = sd_ppd)
    ppd[ppd > 1] <- 1  
  }
  
  # Save the generated ppd values
  saveRDS(ppd, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                             "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                             "_ppd_sim_", sim, ".RData"))
  
  # Initialize N3_itD matrix
  N3_itD <- matrix(0, nrow = nroad_transects, ncol = nmonth)
  
  # Generate N3_itd matrix
  for (j in 1:D) {
    ppd_t <- if (j == 1) prod(ppd) else prod(ppd[j:length(ppd)])  # Adjust ppd according to the age in days of the carcasses
    
    # Initialize N3_itd for each day across D
    N3_itd <- matrix(NA, nrow = nroad_transects, ncol = nmonth)
    
    # Retrieve the N_itd for the current day
    N2_itd_name <- paste0("N2_itd", j)
    N2_itd_current <- get(N2_itd_name)
    
    # Generate the N3_itd matrix based on binomial distribution
    for (i in 1:nroad_transects) {
      for (t in 1:nmonth) {
        N3_itd[i, t] <- rbinom(1, N2_itd_current[i, t], ppd_t)  # equation 3
      }
    }
    
    # Save the N3_itd matrix for each day in D-day period
    assign(paste0("N3_itd", j), N3_itd)
    saveRDS(N3_itd, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                  "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                  "_N3_itd", j, "_sim_", sim, ".RData"))
    
    # Accumulate each N3_itd in N3_itD
    N3_itD <- N3_itD + N3_itd
    saveRDS(N3_itD, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                  "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                  "_N3_itD_sim_", sim, ".RData"))
  }
  
  
  # Census data simulation for each survey methods
  nsurveys <- 3  # Number of surveys
  
  
  # Initialize census data array for 3 methods
  C <- array(dim = c(nroad_transects, nmonth, nmethod, nsurveys))
  
  for (m in 1:3) {
    for (k in 1:nsurveys) {
      C[,,m,k] <- matrix(rbinom(nroad_transects * nmonth, N3_itD, selected_group$pom_group[m]), nrow = nroad_transects, ncol = nmonth)
    }
  }
  
  # Save the generated census data for the current simulation
  saveRDS(C, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                           "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                           "_census_data_sim_", sim, ".RData"))
  
  # Store the generated census data in the list
  census_data[[sim]] <- C
}



# Loop over all census data sets
for (census_data_index in seq_along(census_data)) {
  
  tryCatch({
    # Access the current census data for processing
    current_census_data <- census_data[[census_data_index]]
    print(paste("Running simulation", census_data_index, "for SD LAMBDAS", sd_random_values, "and SD ppd", sd_ppd, "."))
    
    # Prepare the data for the model
    bdata <- list(
      C = current_census_data,                         # Observation data
      nroad_transects = dim(current_census_data)[1],   # Number of road transects
      nmonth = dim(current_census_data)[2],            # Number of months
      nmet = dim(current_census_data)[3],              # Number of survey methods
      nsurveys = dim(current_census_data)[4],          # Number of surveys
      alpha_prior_pl = alpha_prior_pl,                 # Prior parameter alpha for carcass location probability
      beta_prior_pl = beta_prior_pl,                   # Prior parameter beta for carcass location probability
      alpha_prior_pp = alpha_prior_pp,                 # Prior parameter alpha for carcass persistence probability
      beta_prior_pp = beta_prior_pp                    # Prior parameter beta for carcass persistence probability
    )
    
    # Write the JAGS model to a file
    sink(paste0("modelplpppo(m)_census_data", census_data_index, ".txt"))
    cat("
      model {
        # Priors
        pl ~ dbeta(alpha_prior_pl, beta_prior_pl)  # Carcass location probability
        pp ~ dbeta(alpha_prior_pp, beta_prior_pp)  # Carcass persistence probability
        
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
            N2[i,t] ~ dbin(pl, N[i,t])T(0,100000)          # Roadkills inside road
            N3[i, t] ~ dbin(pp, N2[i, t])T(0, 100000)      # Roadkills preserved until road survey
            
            for (m in 1:nmet) {
              for (j in 1:nsurveys) {
                C[i, t, m, j] ~ dbin(po[m], N3[i, t])T(0, 100000)  # Census data
              }
            }
          }
        }
      
        # Derived quantities: Total number of roadkill across all surveyed road transects per month
        for (t in 1:nmonth) {
          totalN[t] <- sum(N[, t]) 
          totalN2[t] <- sum(N2[,t])
          totalN3[t] <- sum(N3[, t])   
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
        pp = rbeta(1, alpha_prior_pp, beta_prior_pp),        # Initial carcass persistence probability
        po = runif(3),                                       # Initial carcass observation probability
        N = Nst                                              # Initial roadkill number
      )
    }
    
    # Specify parameters to monitor in the MCMC
    params <- c("lambda", "pl", "pp", "po", "totalN", "totalN2", "totalN3")
    
    # MCMC settings for the JAGS model
    na <- 100000  # Number of adaptation steps
    ni <- 400000  # Number of iterations
    nt <- 1000    # Thinning interval
    nb <- 100000  # Burn-in period
    nc <- 3       # Number of chains
    
    # Run the JAGS model, check convergence, and summarize posterior distributions
    output <- jags(
      bdata, inits, params, 
      paste0("modelplpppo(m)_census_data", census_data_index, ".txt"),
      n.adapt = na, n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE
    )
    
    # Save the output to a file for later analysis
    saveRDS(output, file = paste0(
      save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, 
      "_nroad_transects_", nroad_transects, 
      "_SD_N_", sd_random_values, 
      "_SD_ppd_", sd_ppd, 
      "_output_analysis_sim_", census_data_index, ".RData"
    ))
    
  }, error = function(e) {
    warning(paste("Error in simulation", census_data_index, ":", conditionMessage(e)))
  })
}

# -----------------------------------------------------------------------------
#14. Census data simulation on SD lambda_td 0 and SD ppd 0
# -----------------------------------------------------------------------------

# Initialize the vector to store results
census_data <- list()

# Loop through simulations
for (sim in 1:nsimulations) {
  
  # Define standard deviations for lambda_td by random values that multiplies lambda_td and for ppd
  sd_random_values <- 0  # SD for random values affecting population growth
  sd_ppd <- 0  # SD for persistence probability
  
  # Generate random values with truncation ensuring values are greater than 0
  random_values <- rtruncnorm(n = D, a = 0, mean = 1, sd = sd_random_values)
  
  # Save the generated random values for this simulation
  saveRDS(random_values, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                       "_SD_N_", sd_random_values, "_sd_ppd_", sd_ppd, 
                                       "_sd_random_values_sim_", sim, ".RData"))
  
  # Generate and save simulated lambdas_td (lambdasim)
  for (j in 1:D) {
    lambdasim <- monthly_trend * random_values[j]  # Adjusting growth rate with random values
    assign(paste0("lambdasim", j), lambdasim)  # Assign lambda value dynamically
    
    # Save lambdasim for each time step j
    saveRDS(lambdasim, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                     "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                     "_lambdasim", j, "_sim_", sim, ".RData"))
  }
  
  # Generate total number of roadkill (N_itd) based on lambdasim
  for (j in 1:D) {
    lambdasim_name <- paste0("lambdasim", j)
    lambdasim_current <- get(lambdasim_name)  # Retrieve current lambda_td value
    
    # Initialize matrix for total number of roadkill
    N_itd <- matrix(NA, nrow = nroad_transects, ncol = nmonth)
    
    for (t in 1:nmonth) {
      if (length(lambdasim_current) >= t) {
        N_itd[, t] <- rpois(nroad_transects, lambda = lambdasim_current[t])  # equation 1
      }
    }
    
    # Save the matrix
    assign(paste0("N_itd", j), N_itd)
    saveRDS(N_itd, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                 "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                 "_N_itd", j, "_sim_", sim, ".RData"))
  }
  
  # Generate roadkill numbers located within the road (N2_itd)
  for (j in 1:D) {
    N_itd_name <- paste0("N_itd", j)
    N_itd_current <- get(N_itd_name)
    
    N2_itd <- matrix(NA, nrow = nroad_transects, ncol = nmonth)
    
    for (i in 1:nroad_transects) {
      for (t in 1:nmonth) {
        N2_itd[i, t] <- rbinom(1, N_itd_current[i, t], selected_group$pl_group)
      }
    }
    
    assign(paste0("N2_itd", j), N2_itd)
    # Save the matrix
    saveRDS(N2_itd, file=paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                "_N2_itd", j, "_sim_", sim, ".RData"))
  }
  
  
  # Generate random daily carcass persistence probabilities along sd_ppd
  ppd <- rnorm(D, mean = selected_group$ppd_group, sd = sd_ppd)
  
  # Ensure that the product of all ppd values is greater than 0.05  
  # and no ppd value is greater than or equal to 1  
  #while (prod(ppd) <= 0.05 || any(ppd >= 1)) { 
  #  ppd <- rnorm(D, mean = selected_group$ppd_group, sd = sd_ppd)
  #  ppd[ppd > 1] <- 1  
  #}
  
  # Save the generated ppd values
  saveRDS(ppd, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                             "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                             "_ppd_sim_", sim, ".RData"))
  
  # Initialize N3_itD matrix
  N3_itD <- matrix(0, nrow = nroad_transects, ncol = nmonth)
  
  # Generate N3_itd matrix
  for (j in 1:D) {
    ppd_t <- if (j == 1) prod(ppd) else prod(ppd[j:length(ppd)])  # Adjust ppd according to the age in days of the carcasses
    
    # Initialize N3_itd for each day across D
    N3_itd <- matrix(NA, nrow = nroad_transects, ncol = nmonth)
    
    # Retrieve the N_itd for the current day
    N2_itd_name <- paste0("N2_itd", j)
    N2_itd_current <- get(N2_itd_name)
    
    # Generate the N3_itd matrix based on binomial distribution
    for (i in 1:nroad_transects) {
      for (t in 1:nmonth) {
        N3_itd[i, t] <- rbinom(1, N2_itd_current[i, t], ppd_t)  # equation 3
      }
    }
    
    # Save the N3_itd matrix for each day in D-day period
    assign(paste0("N3_itd", j), N3_itd)
    saveRDS(N3_itd, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                  "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                  "_N3_itd", j, "_sim_", sim, ".RData"))
    
    # Accumulate each N3_itd in N3_itD
    N3_itD <- N3_itD + N3_itd
    saveRDS(N3_itD, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                                  "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                                  "_N3_itD_sim_", sim, ".RData"))
  }
  
  
  # Census data simulation for each survey methods
  nsurveys <- 3  # Number of surveys
  
  
  # Initialize census data array for 3 methods
  C <- array(dim = c(nroad_transects, nmonth, nmethod, nsurveys))
  
  for (m in 1:3) {
    for (k in 1:nsurveys) {
      C[,,m,k] <- matrix(rbinom(nroad_transects * nmonth, N3_itD, selected_group$pom_group[m]), nrow = nroad_transects, ncol = nmonth)
    }
  }
  
  # Save the generated census data for the current simulation
  saveRDS(C, file = paste0(save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, "_nroad_transects_", nroad_transects, 
                           "_SD_N_", sd_random_values, "_SD_ppd_", sd_ppd, 
                           "_census_data_sim_", sim, ".RData"))
  
  # Store the generated census data in the list
  census_data[[sim]] <- C
}



# Loop over all census data sets
for (census_data_index in seq_along(census_data)) {
  
  tryCatch({
    # Access the current census data for processing
    current_census_data <- census_data[[census_data_index]]
    print(paste("Running simulation", census_data_index, "for SD LAMBDAS", sd_random_values, "and SD ppd", sd_ppd, "."))
    
    # Prepare the data for the model
    bdata <- list(
      C = current_census_data,                         # Observation data
      nroad_transects = dim(current_census_data)[1],   # Number of road transects
      nmonth = dim(current_census_data)[2],            # Number of months
      nmet = dim(current_census_data)[3],              # Number of survey methods
      nsurveys = dim(current_census_data)[4],          # Number of surveys
      alpha_prior_pl = alpha_prior_pl,                 # Prior parameter alpha for carcass location probability
      beta_prior_pl = beta_prior_pl,                   # Prior parameter beta for carcass location probability
      alpha_prior_pp = alpha_prior_pp,                 # Prior parameter alpha for carcass persistence probability
      beta_prior_pp = beta_prior_pp                    # Prior parameter beta for carcass persistence probability
    )
    
    # Write the JAGS model to a file
    sink(paste0("modelplpppo(m)_census_data", census_data_index, ".txt"))
    cat("
      model {
        # Priors
        pl ~ dbeta(alpha_prior_pl, beta_prior_pl)  # Carcass location probability
        pp ~ dbeta(alpha_prior_pp, beta_prior_pp)  # Carcass persistence probability
        
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
            N2[i,t] ~ dbin(pl, N[i,t])T(0,100000)          # Roadkills inside road
            N3[i, t] ~ dbin(pp, N2[i, t])T(0, 100000)      # Roadkills preserved until road survey
            
            for (m in 1:nmet) {
              for (j in 1:nsurveys) {
                C[i, t, m, j] ~ dbin(po[m], N3[i, t])T(0, 100000)  # Census data
              }
            }
          }
        }
      
        # Derived quantities: Total number of roadkill across all surveyed road transects per month
        for (t in 1:nmonth) {
          totalN[t] <- sum(N[, t]) 
          totalN2[t] <- sum(N2[,t])
          totalN3[t] <- sum(N3[, t])   
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
        pp = rbeta(1, alpha_prior_pp, beta_prior_pp),        # Initial carcass persistence probability
        po = runif(3),                                       # Initial carcass observation probability
        N = Nst                                              # Initial roadkill number
      )
    }
    
    # Specify parameters to monitor in the MCMC
    params <- c("lambda", "pl", "pp", "po", "totalN", "totalN2", "totalN3")
    
    # MCMC settings for the JAGS model
    na <- 100000  # Number of adaptation steps
    ni <- 400000  # Number of iterations
    nt <- 1000    # Thinning interval
    nb <- 100000  # Burn-in period
    nc <- 3       # Number of chains
    
    # Run the JAGS model, check convergence, and summarize posterior distributions
    output <- jags(
      bdata, inits, params, 
      paste0("modelplpppo(m)_census_data", census_data_index, ".txt"),
      n.adapt = na, n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE
    )
    
    # Save the output to a file for later analysis
    saveRDS(output, file = paste0(
      save_path, group, "SE_pl_", se_pl, "_SE_pp_", se_pp, 
      "_nroad_transects_", nroad_transects, 
      "_SD_N_", sd_random_values, 
      "_SD_ppd_", sd_ppd, 
      "_output_analysis_sim_", census_data_index, ".RData"
    ))
    
  }, error = function(e) {
    warning(paste("Error in simulation", census_data_index, ":", conditionMessage(e)))
  })
}
