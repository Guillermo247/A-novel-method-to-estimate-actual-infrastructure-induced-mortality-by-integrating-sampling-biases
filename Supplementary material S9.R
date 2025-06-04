### Paper title: A novel method to estimate actual infrastructure-induced mortality by integrating sampling biases

### Supplementary material S9
### R script to calculate Relative Root Mean Squared Error (RRMSE) values for total number of roadkills estimation (N_t)

## R version 4.2.2

# In this script, we load all the simulation outputs and the simulated real values  
# of the total number of roadkills. We then process them to calculate  
# the Relative Root Mean Square Error (RRMSE) for all vertebrate groups and  
# simulation scenarios. In the case of Mammals G5, there is a special section  
# at the end of the code dedicated to calculating its RRMSE.

# Due to the inability to rename the files, the differences between the concepts
#in the file names and the scientific article are as follows:
#p1 = pL (Carcass Location Probability)
#p2 = pP (Average Carcass Persistence Probability in D-day period)
#p3[1] = pOw (Carcass observation probability by walking survey method)
#p3[2] = pOc (Carcass observation probability by cycling survey method)
#p3[3] = pOd (Carcass observation probability by driving survey method)
#SE p1 = SE pL (Standar Error Carcass Location Probability)
#SE p2 = SE pP (Standar Error Carcass Persistence Probability)
#SD N = SD lambda (Variability in daily roadkill abundance)
#SD p2 = SD pPd (Variability in daily carcass persistence probability)
#nsites = number of transects
#lizards = Reptiles G1
#Snakes = Reptiles G2
#small_birds_and_bats = Birds/Bats G1
#medium_and_large_birds = Birds G2
#small_mammals = Mammals G1
#Lagomorphs = Mammals G2
#hedhehogs = Mammals G3
#medium_and_large_carnivores = Mammals G4
#ungulates = Mammals G5

#load necessary packages
library(ggplot2)
library(ggdist)
library(dplyr)
library(tidyr)
library(jagsUI)

setwd("C:/Users/USUARIO/Documents/ESTUDIOS/Master/Conservación Pablo Olavide/TFM/Analisis datos/Scripts Github/Resultados 100 simulaciones") # set path to location where all files necessary to run the script are
path_data <- "C:/Users/USUARIO/Documents/Appendix simulations"

##Vertebrate group along with their charateristics
select_group <- function(group_name) {
  if (group_name == "amphibians") {
    se005ntransect10 <- "amphibians/sites10_se05"
    se005ntransect100 <- "amphibians/sites100_se05"
    se01ntransect10 <- "amphibians/sites10_se10"
    se01ntransect100 <- "amphibians/sites100_se10"
    path_group_se_005<-"amphibians_SE_p2_0.05"
    path_group_se_01<-"amphibians_SE_p2_0.1"
    Obs_val_p1 <- NA
    Obs_val_p3_walking <- 0.5
    Obs_val_p3_cycling <- 0.3
    Obs_val_p3_driving <- 0.02
    ppd_group <- 0.392598565
    RRMSE <- "RRMSE_Amphibians"
    Group <- "Amphibians"
  } else if (group_name == "amphibians_abundance_peak") {
    se005ntransect10 <- "amphibians_abundance_peak/sites10_se05"
    se005ntransect100 <- "amphibians_abundance_peak/sites100_se05"
    se01ntransect10 <- "amphibians_abundance_peak/sites10_se10"
    se01ntransect100 <- "amphibians_abundance_peak/sites100_se10"
    path_group_se_005<-"amphibians_SE_p2_0.05"
    path_group_se_01<-"amphibians_SE_p2_0.1"
    Obs_val_p1 <- NA
    Obs_val_p3_walking <- 0.5
    Obs_val_p3_cycling <- 0.3
    Obs_val_p3_driving <- 0.02
    ppd_group <- 0.392598565
    RRMSE <- "RRMSE_Amphibians_abundace_peak"
    Group <- "Amphibians abundance peak"
  } else if (group_name == "reptiles_g1") {
    se005ntransect10 <- "reptiles_g1/sites10_se05"
    se005ntransect100 <- "reptiles_g1/sites100_se05"
    se01ntransect10 <- "reptiles_g1/sites10_se10"
    se01ntransect100 <- "reptiles_g1/sites100_se10"
    path_group_se_005<-"lizards_SE_p2_0.05"
    path_group_se_01<-"lizards_SE_p2_0.1"
    Obs_val_p1 <- NA
    Obs_val_p3_walking <- 0.5
    Obs_val_p3_cycling <- 0.3
    Obs_val_p3_driving <- 0.02
    ppd_group <- 0.05655527
    RRMSE <- "RRMSE_Reptiles_G1"
    Group <- "Reptiles G1"
  } else if (group_name == "reptiles_g1_abundance_peak") {
    se005ntransect10 <- "reptiles_g1_abundance_peak/sites10_se05"
    se005ntransect100 <- "reptiles_g1_abundance_peak/sites100_se05"
    se01ntransect10 <- "reptiles_g1_abundance_peak/sites10_se10"
    se01ntransect100 <- "reptiles_g1_abundance_peak/sites100_se10"
    path_group_se_005<-"lizards_SE_p2_0.05"
    path_group_se_01<-"lizards_SE_p2_0.1"
    Obs_val_p1 <- NA
    Obs_val_p3_walking <- 0.5
    Obs_val_p3_cycling <- 0.3
    Obs_val_p3_driving <- 0.02
    ppd_group <- 0.05655527
    RRMSE <- "RRMSE_Reptiles_G1_abundance_peak"
    Group <- "Reptiles G1 abundance peak"
  } else if (group_name == "reptiles_g2") {
    se005ntransect10 <- "reptiles_g2/sites10_se05"
    se005ntransect100 <- "reptiles_g2/sites100_se05"
    se01ntransect10 <- "reptiles_g2/sites10_se10"
    se01ntransect100 <- "reptiles_g2/sites100_se10"
    path_group_se_005<-"Snakes_SE_p1_0.05_SE_p2_0.05"
    path_group_se_01<-"Snakes_SE_p1_0.1_SE_p2_0.1"
    Obs_val_p1 <- 0.42857
    Obs_val_p3_walking <- 0.7
    Obs_val_p3_cycling <- 0.5
    Obs_val_p3_driving <- 0.1
    ppd_group <- 0.398477157
    RRMSE <- "RRMSE_Reptiles_G2"
    Group <- "Reptiles G2"
  } else if (group_name == "birds_bats_g1") {
    se005ntransect10 <- "birds_bats_g1/sites10_se05"
    se005ntransect100 <- "birds_bats_g1/sites100_se05"
    se01ntransect10 <- "birds_bats_g1/sites10_se10"
    se01ntransect100 <- "birds_bats_g1/sites100_se10"
    path_group_se_005<-"small_birds_and_bats_SE_p1_0.05_SE_p2_0.05"
    path_group_se_01<-"small_birds_and_bats_SE_p1_0.1_SE_p2_0.1"
    Obs_val_p1 <- 0.506329114
    Obs_val_p3_walking <- 0.6
    Obs_val_p3_cycling <- 0.4
    Obs_val_p3_driving <- 0.05
    ppd_group <- 0.358632169
    RRMSE <- "RRMSE_Birds_Bats_G1"
    Group <- "Birds/Bats G1"
  } else if (group_name == "birds_g2") {
    se005ntransect10 <- "birds_g2/sites10_se05"
    se005ntransect100 <- "birds_g2/sites100_se05"
    se01ntransect10 <- "birds_g2/sites10_se10"
    se01ntransect100 <- "birds_g2/sites100_se10"
    path_group_se_005<-"medium_and_large_birds_SE_p1_0.05_SE_p2_0.05"
    path_group_se_01<-"medium_and_large_birds_SE_p1_0.1_SE_p2_0.1"
    Obs_val_p1 <- 0.692307692
    Obs_val_p3_walking <- 0.8
    Obs_val_p3_cycling <- 0.6
    Obs_val_p3_driving <- 0.2
    ppd_group <- 0.747433082
    RRMSE <- "RRMSE_Birds_G2"
    Group <- "Birds G2"
  } else if (group_name == "mammals_g1") {
    se005ntransect10 <- "mammals_g1/sites10_se05"
    se005ntransect100 <- "mammals_g1/sites100_se05"
    se01ntransect10 <- "mammals_g1/sites10_se10"
    se01ntransect100 <- "mammals_g1/sites100_se10"
    path_group_se_005<-"small_mammals_SE_p2_0.05"
    path_group_se_01<-"small_mammals_SE_p2_0.1"
    Obs_val_p1 <- NA
    Obs_val_p3_walking <- 0.6
    Obs_val_p3_cycling <- 0.4
    Obs_val_p3_driving <- 0.05
    ppd_group <- 0.388601036
    RRMSE <- "RRMSE_Mammals_G1"
    Group <- "Mammals G1"
  } else if (group_name == "mammals_g2") {
    se005ntransect10 <- "mammals_g2/sites10_se05"
    se005ntransect100 <- "mammals_g2/sites100_se05"
    se01ntransect10 <- "mammals_g2/sites10_se10"
    se01ntransect100 <- "mammals_g2/sites100_se10"
    path_group_se_005<-"Lagomorphs_SE_p2_0.05"
    path_group_se_01<-"Lagomorphs_SE_p2_0.1"
    Obs_val_p1 <- NA
    Obs_val_p3_walking <- 0.8
    Obs_val_p3_cycling <- 0.6
    Obs_val_p3_driving <- 0.2
    ppd_group <- 0.506361323
    RRMSE <- "RRMSE_Mammals_G2"
    Group <- "Mammals G2"
  } else if (group_name == "mammals_g3") {
    se005ntransect10 <- "mammals_g3/sites10_se05"
    se005ntransect100 <- "mammals_g3/sites100_se05"
    se01ntransect10 <- "mammals_g3/sites10_se10"
    se01ntransect100 <- "mammals_g3/sites100_se10"
    path_group_se_005<-"hedgehogs_SE_p2_0.05"
    path_group_se_01<-"hedgehogs_SE_p2_0.1"
    Obs_val_p1 <- NA
    Obs_val_p3_walking <- 0.8
    Obs_val_p3_cycling <- 0.6
    Obs_val_p3_driving <- 0.2
    ppd_group <- 0.78250591
    RRMSE <- "RRMSE_Mammals_G3"
    Group <- "Mammals G3"
  } else if (group_name == "mammals_g4") {
    se005ntransect10 <- "mammals_g4/sites10_se05"
    se005ntransect100 <- "mammals_g4/sites100_se05"
    se01ntransect10 <- "mammals_g4/sites10_se10"
    se01ntransect100 <- "mammals_g4/sites100_se10"
    path_group_se_005<-"medium_and_large_carnivores_SE_p1_0.05_SE_p2_0.05"
    path_group_se_01<-"medium_and_large_carnivores_SE_p1_0.1_SE_p2_0.1"
    Obs_val_p1 <- 0.647058824
    Obs_val_p3_walking <- 0.9
    Obs_val_p3_cycling <- 0.7
    Obs_val_p3_driving <- 0.3
    ppd_group <- 0.805626598
    RRMSE <- "RRMSE_Mammals_G4"
    Group <- "Mammals G4"
  } else {
    stop("Invalid group. Please choose between 'amphibians', 'reptiles_g1','reptiles_g2', 'birds_bats_g1', 'birds_g2' 'mammals_g1', 'mammals_g2' 'mammals_g3' or 'mammals_g4'.")
  }
  
  return(list(se005ntransect10= se005ntransect10, se005ntransect100= se005ntransect100,se01ntransect10=se01ntransect10,
              se01ntransect100=se01ntransect100, path_group_se_005 = path_group_se_005, path_group_se_01 = path_group_se_01,
              Obs_val_p1 = Obs_val_p1, Obs_val_p3_walking =Obs_val_p3_walking, Obs_val_p3_cycling = Obs_val_p3_cycling,
              Obs_val_p3_driving = Obs_val_p3_driving, ppd_group = ppd_group, RRMSE = RRMSE, Group = Group))
}

#Select vertebrate group along with their charateristics
group <- "mammals_g4"  # write the group of interest
selected_group <- select_group(group)

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
Obs_val_p2 <- curve_integral / total_area

# ------------------------------------------------------------------------------
#1.Standar Error carcass location and persistence bias = 0.05 nº transects = 10
# ------------------------------------------------------------------------------


# Function to load files and store them in a list
load_outputs <- function(base_path, scenario, simulations) {
  files <- sapply(simulations, function(sim) {
    paste0(base_path, scenario, "sim_", sim, "_1.RData")
  })
  
  list_outputs <- lapply(files, function(file) {
    tryCatch({
      # Attempt to read the RData file
      readRDS(file)
    }, error = function(e) {
      # Show a warning if the file cannot be opened
      warning(paste("Could not open file:", file, "- Error:", e$message))
      return(NULL) # Return NULL if the file cannot be loaded
    })
  })
  
  # Filter out NULL values (unloaded files)
  list_outputs <- Filter(Negate(is.null), list_outputs)
  
  return(list_outputs)
}
# Create the new object with the combined path
path_se005ntransect10 <- paste0(file.path(path_data, selected_group$se005ntransect10), "/")

# Print the path
print(path_se005ntransect10)

# Generate output scenarios with integrated path_group_se_005
output_scenarios <- c(
  paste0(selected_group$path_group_se_005, "_nsites_10_SD_N_0.5_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group_se_005, "_nsites_10_SD_N_0.5_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group_se_005, "_nsites_10_SD_N_0.5_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group_se_005, "_nsites_10_SD_N_1.5_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group_se_005, "_nsites_10_SD_N_1.5_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group_se_005, "_nsites_10_SD_N_1.5_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group_se_005, "_nsites_10_SD_N_0_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group_se_005, "_nsites_10_SD_N_0_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group_se_005, "_nsites_10_SD_N_0_SD_p2_0_output_analysis_")
)

# Simulations, in this case, we have 20
simulations <- 1:20

# Load data for all scenarios and simulations
outputs <- list()
for (scenario in output_scenarios) {
  data <- load_outputs(path_se005ntransect10, scenario, simulations)
  outputs[[scenario]] <- data
}

# Verify loaded data
print(outputs[[output_scenarios[1]]][[1]])

# Create an object to store the Probability values of p1 from the output analysis for each scenario
p1_values <- list()

# Iterate over scenarios and simulations to extract p1, replacing NULL with NA 
#for the vertebrate groups not affected by carcass location bias
for (scenario in output_scenarios) {
  p1_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    if (is.null(output$sims.list$p1)) {
      NA  
    } else {
      output$sims.list$p1
    }
  })
}

# Create an object to store the Probability_values of p2 from the output analysis for each level
p2_values <- list()

# Iterate over scenarioes and simulations to extract totalN
for (scenario in output_scenarios) {
  p2_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$p2
  })
}

# Create an object to store the Probability_values of p3 walking from the output analysis for each level
p3_walking_values <- list()

# Iterate over scenarioes and simulations to extract totalN
for (scenario in output_scenarios) {
  p3_walking_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$p3[,1]
  })
}

# Create an object to store the Probability_values of p3 cycling from the output analysis for each level
p3_cycling_values <- list()

# Iterate over scenarioes and simulations to extract totalN
for (scenario in output_scenarios) {
  p3_cycling_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$p3[,2]
  })
}

# Create an object to store the Probability_values of p3 driving from the output analysis for each level
p3_driving_values <- list()

# Iterate over scenarioes and simulations to extract totalN
for (scenario in output_scenarios) {
  p3_driving_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$p3[,3]
  })
}

# Define the target length
target_length <- 18000

# Create a unified data frame
combined_data <- do.call(rbind, lapply(names(p1_values), function(scenario) {
  
  # Get probabilities for each variable
  p1_prob <- unlist(p1_values[[scenario]])
  p2_prob <- unlist(p2_values[[scenario]])
  p3_walking_prob <- unlist(p3_walking_values[[scenario]])
  p3_cycling_prob <- unlist(p3_cycling_values[[scenario]])
  p3_driving_prob <- unlist(p3_driving_values[[scenario]])
  
  # Fill with NA if the length is less than the target
  p1_prob <- c(p1_prob, rep(NA, target_length - length(p1_prob)))
  p2_prob <- c(p2_prob, rep(NA, target_length - length(p2_prob)))
  p3_walking_prob <- c(p3_walking_prob, rep(NA, target_length - length(p3_walking_prob)))
  p3_cycling_prob <- c(p3_cycling_prob, rep(NA, target_length - length(p3_cycling_prob)))
  p3_driving_prob <- c(p3_driving_prob, rep(NA, target_length - length(p3_driving_prob)))
  
  # Create the data frame for each variable
  rbind(
    data.frame(Variable = "p1", 
               Probability_value = p1_prob, 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario),
    
    data.frame(Variable = "p2", 
               Probability_value = p2_prob, 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario),
    
    data.frame(Variable = "p3 walking", 
               Probability_value = p3_walking_prob, 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario),
    
    data.frame(Variable = "p3 cycling", 
               Probability_value = p3_cycling_prob, 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario),
    
    data.frame(Variable = "p3 driving", 
               Probability_value = p3_driving_prob, 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario)
  )
}))

# Display the combined dataframe
print(combined_data)


# Group by 'Variable', 'Simulation', and 'scenario' and calculate the mean probability values
df_mean_grouped <- combined_data %>%
  group_by(Variable, Simulation, scenario) %>%
  summarise(mean_value = mean(Probability_value, na.rm = TRUE)) %>%
  ungroup()

# Display the new dataframe with calculated means
print(df_mean_grouped)

# Group by 'Variable', 'Simulation', and 'scenario' and calculate the variance of probability values
df_var_grouped <- combined_data %>%
  group_by(Variable, Simulation, scenario) %>%
  summarise(var_value = var(Probability_value, na.rm = TRUE)) %>%
  ungroup()

# Display the new dataframe with calculated variances
print(df_var_grouped)




# Merge the dataframes by 'Variable', 'Simulation', and 'scenario'
df_combined <- df_mean_grouped %>%
  left_join(df_var_grouped, by = c("Variable", "Simulation", "scenario"))

# Display the new combined dataframe
print(df_combined)

# Observed values
real_values <- data.frame(
  Variable = c("p1", "p2", "p3 walking", "p3 cycling", "p3 driving"),
  real_value = c(selected_group$Obs_val_p1, Obs_val_p2, selected_group$Obs_val_p3_walking, selected_group$Obs_val_p3_cycling, selected_group$Obs_val_p3_driving)
)

# Display the real values dataframe
print(real_values)

# Merge df_combined with real_values to add the real_value column
df_combined <- df_combined %>%
  left_join(real_values, by = "Variable")

# Display the combined dataframe with the new column
print(df_combined)

# Calculate RRMSE
calculate_rrmse <- function(estimated, real, variance) {
  return(sqrt((estimated - real)^2 + variance) / real)
}

# Initialize the list to store RRMSE results
rrmse_list_se005_nsites10 <- list()

# Loop over each variable
for (variable in unique(df_combined$Variable)) {
  
  # Filter data for the current variable
  df_variable <- df_combined %>% filter(Variable == variable)
  
  # Initialize sublist for the current variable
  rrmse_list_se005_nsites10[[variable]] <- list()
  
  # Loop over each scenario
  for (scenario_name in unique(df_variable$scenario)) {
    
    # Filter data for the current scenario
    df_scenario <- df_variable %>% filter(scenario == scenario_name)
    
    # Initialize sublist for the current scenario
    rrmse_list_se005_nsites10[[variable]][[scenario_name]] <- list()
    
    # Loop over each simulation
    for (sim in unique(df_scenario$Simulation)) {
      
      # Filter data for the current simulation
      df_simulation <- df_scenario %>% filter(Simulation == sim)
      
      # Extract estimated values, real value, and variance
      estimated <- df_simulation$mean_value
      real <- df_simulation$real_value[1]  # Assumes constant real_value per group
      variance <- df_simulation$var_value
      
      # Compute RRMSE if all necessary values are present
      if (!is.null(estimated) && length(estimated) > 0 && 
          !is.null(real) && length(real) > 0 && 
          !is.null(variance) && length(variance) > 0) {
        
        rrmse_list_se005_nsites10[[variable]][[scenario_name]][[sim]] <- 
          calculate_rrmse(estimated, real, variance)
        
      } else {
        rrmse_list_se005_nsites10[[variable]][[scenario_name]][[sim]] <- NA
        message(paste("Missing or invalid data for variable:", variable, 
                      "scenario:", scenario_name, "simulation:", sim))
      }
    }
  }
}

# Check the results for a specific variable, e.g., "p1"
print(rrmse_list_se005_nsites10[["p2"]])

# Create the RRMSE DataFrame from rrmse_list_se005_nsites10
rrmse_df_005_10 <- data.frame()

for (variable in names(rrmse_list_se005_nsites10)) {
  for (scenario in names(rrmse_list_se005_nsites10[[variable]])) {
    for (sim in 1:length(rrmse_list_se005_nsites10[[variable]][[scenario]])) {
      rrmse_value <- rrmse_list_se005_nsites10[[variable]][[scenario]][[sim]]
      
      # Add row to DataFrame
      rrmse_df_005_10 <- rbind(rrmse_df_005_10, data.frame(
        Variable = variable,
        RRMSE = rrmse_value,
        scenario = scenario,
        Simulation = sim
      ))
    }
  }
}

# Verify that all scenarioes have been correctly mapped
print(unique(rrmse_df_005_10$scenario))
print(rrmse_df_005_10)  # Display the complete DataFrame


# Define scenarios dynamically
scenarios <- c(
  paste0(selected_group$path_group_se_005, "_nsites_10_SD_N_0_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group_se_005, "_nsites_10_SD_N_0_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group_se_005, "_nsites_10_SD_N_0_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group_se_005, "_nsites_10_SD_N_0.5_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group_se_005, "_nsites_10_SD_N_0.5_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group_se_005, "_nsites_10_SD_N_0.5_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group_se_005, "_nsites_10_SD_N_1.5_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group_se_005, "_nsites_10_SD_N_1.5_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group_se_005, "_nsites_10_SD_N_1.5_SD_p2_0.15_output_analysis_")
)

# Define user-friendly labels
labels <- c(
  "SD N 0 p2 0", "SD N 0 p2 0.05", "SD N 0 p2 0.15",
  "SD N 0.5 p2 0", "SD N 0.5 p2 0.05", "SD N 0.5 p2 0.15",
  "SD N 1.5 p2 0", "SD N 1.5 p2 0.05", "SD N 1.5 p2 0.15"
)

# Create mapping using setNames()
scenario_map <- setNames(labels, scenarios)

# Map scenarios to user-friendly labels
rrmse_df_005_10$scenario <- factor(rrmse_df_005_10$scenario, levels = names(scenario_map), labels = scenario_map)

# Verify that all scenarios have been correctly mapped
print(unique(rrmse_df_005_10$scenario))


# Remove rows with NA in any column
# This is necessary to compute Generalized RRMSE properly
# and to avoid including the variable 'p1' in the analysis
# for vertebrate groups that are not affected by carcass location bias
rrmse_df_005_10 <- rrmse_df_005_10 %>%
  drop_na()

# Calculate generalized RRMSE using the product, ignoring any remaining NAs (if any)
rrmse_df_005_10 <- rrmse_df_005_10 %>%
  group_by(scenario, Simulation) %>%
  summarise(Generalized_RRMSE = prod(RRMSE, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Generalized_RRMSE = Generalized_RRMSE ^ (1 / length(unique(rrmse_df_005_10$Variable))))

# Display the dataframe with the new column
print(rrmse_df_005_10)

# ------------------------------------------------------------------------------
#2.Standar Error carcass location and persistence bias = 0.05 nº transects = 100
# ------------------------------------------------------------------------------

# Function to load files and store them in a list
load_outputs <- function(base_path, scenario, simulations) {
  files <- sapply(simulations, function(sim) {
    paste0(base_path, scenario, "sim_", sim, "_1.RData")
  })
  
  list_outputs <- lapply(files, function(file) {
    tryCatch({
      # Attempt to read the RData file
      readRDS(file)
    }, error = function(e) {
      # Show a warning if the file cannot be opened
      warning(paste("Could not open file:", file, "- Error:", e$message))
      return(NULL) # Return NULL if the file cannot be loaded
    })
  })
  
  # Filter out NULL values (unloaded files)
  list_outputs <- Filter(Negate(is.null), list_outputs)
  
  return(list_outputs)
}

# Create the new object with the combined path
path_se005ntransect100 <- paste0(file.path(path_data, selected_group$se005ntransect100), "/")

# Print the path
print(path_se005ntransect100)

# Generate output scenarios with integrated path_group_se_005
output_scenarios <- c(
  paste0(selected_group$path_group_se_005, "_nsites_100_SD_N_0.5_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group_se_005, "_nsites_100_SD_N_0.5_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group_se_005, "_nsites_100_SD_N_0.5_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group_se_005, "_nsites_100_SD_N_1.5_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group_se_005, "_nsites_100_SD_N_1.5_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group_se_005, "_nsites_100_SD_N_1.5_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group_se_005, "_nsites_100_SD_N_0_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group_se_005, "_nsites_100_SD_N_0_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group_se_005, "_nsites_100_SD_N_0_SD_p2_0_output_analysis_")
)

# Simulations, in this case, we have 20
simulations <- 1:20

# Load data for all scenarios and simulations
outputs <- list()
for (scenario in output_scenarios) {
  data <- load_outputs(path_se005ntransect100, scenario, simulations)
  outputs[[scenario]] <- data
}

# Verify loaded data
print(outputs[[output_scenarios[1]]][[1]])

# Create an object to store the Probability values of p1 from the output analysis for each scenario
p1_values <- list()

# Iterate over scenarios and simulations to extract p1, replacing NULL with NA 
#for the vertebrate groups not affected by carcass location bias
for (scenario in output_scenarios) {
  p1_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    if (is.null(output$sims.list$p1)) {
      NA  
    } else {
      output$sims.list$p1
    }
  })
}

# Create an object to store the Probability_values of p2 from the output analysis for each level
p2_values <- list()

# Iterate over scenarioes and simulations to extract totalN
for (scenario in output_scenarios) {
  p2_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$p2
  })
}

# Create an object to store the Probability_values of p3 walking from the output analysis for each level
p3_walking_values <- list()

# Iterate over scenarioes and simulations to extract totalN
for (scenario in output_scenarios) {
  p3_walking_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$p3[,1]
  })
}

# Create an object to store the Probability_values of p3 cycling from the output analysis for each level
p3_cycling_values <- list()

# Iterate over scenarioes and simulations to extract totalN
for (scenario in output_scenarios) {
  p3_cycling_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$p3[,2]
  })
}

# Create an object to store the Probability_values of p3 driving from the output analysis for each level
p3_driving_values <- list()

# Iterate over scenarioes and simulations to extract totalN
for (scenario in output_scenarios) {
  p3_driving_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$p3[,3]
  })
}

# Define the target length
target_length <- 18000

# Create a unified data frame
combined_data <- do.call(rbind, lapply(names(p1_values), function(scenario) {
  
  # Get probabilities for each variable
  p1_prob <- unlist(p1_values[[scenario]])
  p2_prob <- unlist(p2_values[[scenario]])
  p3_walking_prob <- unlist(p3_walking_values[[scenario]])
  p3_cycling_prob <- unlist(p3_cycling_values[[scenario]])
  p3_driving_prob <- unlist(p3_driving_values[[scenario]])
  
  # Fill with NA if the length is less than the target
  p1_prob <- c(p1_prob, rep(NA, target_length - length(p1_prob)))
  p2_prob <- c(p2_prob, rep(NA, target_length - length(p2_prob)))
  p3_walking_prob <- c(p3_walking_prob, rep(NA, target_length - length(p3_walking_prob)))
  p3_cycling_prob <- c(p3_cycling_prob, rep(NA, target_length - length(p3_cycling_prob)))
  p3_driving_prob <- c(p3_driving_prob, rep(NA, target_length - length(p3_driving_prob)))
  
  # Create the data frame for each variable
  rbind(
    data.frame(Variable = "p1", 
               Probability_value = p1_prob, 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario),
    
    data.frame(Variable = "p2", 
               Probability_value = p2_prob, 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario),
    
    data.frame(Variable = "p3 walking", 
               Probability_value = p3_walking_prob, 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario),
    
    data.frame(Variable = "p3 cycling", 
               Probability_value = p3_cycling_prob, 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario),
    
    data.frame(Variable = "p3 driving", 
               Probability_value = p3_driving_prob, 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario)
  )
}))

# Display the combined dataframe
print(combined_data)

# Group by 'Variable', 'Simulation', and 'scenario' and calculate the mean probability values
df_mean_grouped <- combined_data %>%
  group_by(Variable, Simulation, scenario) %>%
  summarise(mean_value = mean(Probability_value, na.rm = TRUE)) %>%
  ungroup()

# Display the new dataframe with calculated means
print(df_mean_grouped)

# Group by 'Variable', 'Simulation', and 'scenario' and calculate the variance of probability values
df_var_grouped <- combined_data %>%
  group_by(Variable, Simulation, scenario) %>%
  summarise(var_value = var(Probability_value, na.rm = TRUE)) %>%
  ungroup()

# Display the new dataframe with calculated variances
print(df_var_grouped)


# Merge the dataframes by 'Variable', 'Simulation', and 'scenario'
df_combined <- df_mean_grouped %>%
  left_join(df_var_grouped, by = c("Variable", "Simulation", "scenario"))

# Display the new combined dataframe
print(df_combined)

# Observed values
real_values <- data.frame(
  Variable = c("p1", "p2", "p3 walking", "p3 cycling", "p3 driving"),
  real_value = c(selected_group$Obs_val_p1, Obs_val_p2, selected_group$Obs_val_p3_walking, selected_group$Obs_val_p3_cycling, selected_group$Obs_val_p3_driving)
)

# Display the real values dataframe
print(real_values)

# Merge df_combined with real_values to add the real_value column
df_combined <- df_combined %>%
  left_join(real_values, by = "Variable")

# Display the combined dataframe with the new column
print(df_combined)

# Calculate RRMSE
calculate_rrmse <- function(estimated, real, variance) {
  return(sqrt((estimated - real)^2 + variance) / real)
}

# Initialize a list to store the results
rrmse_list_se005_nsites100 <- list()

# Loop over each variable
for (variable in unique(df_combined$Variable)) {
  
  # Filter data for the current variable
  df_variable <- df_combined %>% filter(Variable == variable)
  
  # Initialize sublist for the current variable
  rrmse_list_se005_nsites100[[variable]] <- list()
  
  # Loop over each scenario
  for (scenario_name in unique(df_variable$scenario)) {
    
    # Filter data for the current scenario
    df_scenario <- df_variable %>% filter(scenario == scenario_name)
    
    # Initialize sublist for the current scenario
    rrmse_list_se005_nsites100[[variable]][[scenario_name]] <- list()
    
    # Loop over each simulation
    for (sim in unique(df_scenario$Simulation)) {
      
      # Filter data for the current simulation
      df_simulation <- df_scenario %>% filter(Simulation == sim)
      
      # Extract estimated values, real value, and variance
      estimated <- df_simulation$mean_value
      real <- df_simulation$real_value[1]  # Assumes constant real_value per group
      variance <- df_simulation$var_value
      
      # Compute RRMSE if all necessary values are present
      if (!is.null(estimated) && length(estimated) > 0 && 
          !is.null(real) && length(real) > 0 && 
          !is.null(variance) && length(variance) > 0) {
        
        rrmse_list_se005_nsites100[[variable]][[scenario_name]][[sim]] <- 
          calculate_rrmse(estimated, real, variance)
        
      } else {
        rrmse_list_se005_nsites100[[variable]][[scenario_name]][[sim]] <- NA
        message(paste("Missing or invalid data for variable:", variable, 
                      "scenario:", scenario_name, "simulation:", sim))
      }
    }
  }
}

# Check the results for a specific variable, e.g., "p1"
print(rrmse_list_se005_nsites100[["p1"]])

# Create the RRMSE DataFrame from rrmse_list_se005_nsites100
rrmse_df_005_100 <- data.frame()

for (variable in names(rrmse_list_se005_nsites100)) {
  for (scenario in names(rrmse_list_se005_nsites100[[variable]])) {
    for (sim in 1:length(rrmse_list_se005_nsites100[[variable]][[scenario]])) {
      rrmse_value <- rrmse_list_se005_nsites100[[variable]][[scenario]][[sim]]
      
      # Add row to DataFrame
      rrmse_df_005_100 <- rbind(rrmse_df_005_100, data.frame(
        Variable = variable,
        RRMSE = rrmse_value,
        scenario = scenario,
        Simulation = sim
      ))
    }
  }
}

# Verify that all scenarioes have been correctly mapped
print(unique(rrmse_df_005_100$scenario))
print(rrmse_df_005_100)  # Display the complete DataFrame


# Define scenarios dynamically
scenarios <- c(
  paste0(selected_group$path_group_se_005, "_nsites_100_SD_N_0_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group_se_005, "_nsites_100_SD_N_0_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group_se_005, "_nsites_100_SD_N_0_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group_se_005, "_nsites_100_SD_N_0.5_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group_se_005, "_nsites_100_SD_N_0.5_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group_se_005, "_nsites_100_SD_N_0.5_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group_se_005, "_nsites_100_SD_N_1.5_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group_se_005, "_nsites_100_SD_N_1.5_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group_se_005, "_nsites_100_SD_N_1.5_SD_p2_0.15_output_analysis_")
)

# Define user-friendly labels
labels <- c(
  "SD N 0 p2 0", "SD N 0 p2 0.05", "SD N 0 p2 0.15",
  "SD N 0.5 p2 0", "SD N 0.5 p2 0.05", "SD N 0.5 p2 0.15",
  "SD N 1.5 p2 0", "SD N 1.5 p2 0.05", "SD N 1.5 p2 0.15"
)

# Create mapping using setNames()
scenario_map <- setNames(labels, scenarios)

# Map scenarios to user-friendly labels
rrmse_df_005_100$scenario <- factor(rrmse_df_005_100$scenario, levels = names(scenario_map), labels = scenario_map)

# Verify that all scenarios have been correctly mapped
print(unique(rrmse_df_005_100$scenario))

# Remove rows with NA in any column
# This is necessary to compute Generalized RRMSE properly
# and to avoid including the variable 'p1' in the analysis
# for vertebrate groups that are not affected by carcass location bias
rrmse_df_005_100 <- rrmse_df_005_100 %>%
  drop_na()

# Calculate generalized RRMSE using the product, ignoring any remaining NAs (if any)
rrmse_df_005_100 <- rrmse_df_005_100 %>%
  group_by(scenario, Simulation) %>%
  summarise(Generalized_RRMSE = prod(RRMSE, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Generalized_RRMSE = Generalized_RRMSE ^ (1 / length(unique(rrmse_df_005_100$Variable))))

# Display the dataframe with the new column
print(rrmse_df_005_100)

# ------------------------------------------------------------------------------
#3.Standar Error carcass location and persistence bias = 0.10 nº transects = 10
# ------------------------------------------------------------------------------

# Function to load files and store them in a list
load_outputs <- function(base_path, scenario, simulations) {
  files <- sapply(simulations, function(sim) {
    paste0(base_path, scenario, "sim_", sim, "_1.RData")
  })
  
  list_outputs <- lapply(files, function(file) {
    tryCatch({
      # Attempt to read the RData file
      readRDS(file)
    }, error = function(e) {
      # Show a warning if the file cannot be opened
      warning(paste("Could not open file:", file, "- Error:", e$message))
      return(NULL) # Return NULL if the file cannot be loaded
    })
  })
  
  # Filter out NULL values (unloaded files)
  list_outputs <- Filter(Negate(is.null), list_outputs)
  
  return(list_outputs)
}

# Create the new object with the combined path
path_se01ntransect10 <- paste0(file.path(path_data, selected_group$se01ntransect10), "/")

# Print the path
print(path_se01ntransect10)

# Generate output scenarios with integrated path_group_se_01
output_scenarios <- c(
  paste0(selected_group$path_group_se_01, "_nsites_10_SD_N_0.5_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group_se_01, "_nsites_10_SD_N_0.5_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group_se_01, "_nsites_10_SD_N_0.5_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group_se_01, "_nsites_10_SD_N_1.5_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group_se_01, "_nsites_10_SD_N_1.5_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group_se_01, "_nsites_10_SD_N_1.5_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group_se_01, "_nsites_10_SD_N_0_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group_se_01, "_nsites_10_SD_N_0_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group_se_01, "_nsites_10_SD_N_0_SD_p2_0_output_analysis_")
)

# Simulations, in this case, we have 20
simulations <- 1:20

# Load data for all scenarios and simulations
outputs <- list()
for (scenario in output_scenarios) {
  data <- load_outputs(path_se01ntransect10, scenario, simulations)
  outputs[[scenario]] <- data
}

# Verify loaded data
print(outputs[[output_scenarios[1]]][[1]])

# Create an object to store the Probability values of p1 from the output analysis for each scenario
p1_values <- list()

# Iterate over scenarios and simulations to extract p1, replacing NULL with NA 
#for the vertebrate groups not affected by carcass location bias
for (scenario in output_scenarios) {
  p1_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    if (is.null(output$sims.list$p1)) {
      NA  
    } else {
      output$sims.list$p1
    }
  })
}


# Create an object to store the Probability_values of p2 from the output analysis for each level
p2_values <- list()

# Iterate over scenarioes and simulations to extract totalN
for (scenario in output_scenarios) {
  p2_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$p2
  })
}

# Create an object to store the Probability_values of p3 walking from the output analysis for each level
p3_walking_values <- list()

# Iterate over scenarioes and simulations to extract totalN
for (scenario in output_scenarios) {
  p3_walking_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$p3[,1]
  })
}

# Create an object to store the Probability_values of p3 cycling from the output analysis for each level
p3_cycling_values <- list()

# Iterate over scenarioes and simulations to extract totalN
for (scenario in output_scenarios) {
  p3_cycling_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$p3[,2]
  })
}

# Create an object to store the Probability_values of p3 driving from the output analysis for each level
p3_driving_values <- list()

# Iterate over scenarioes and simulations to extract totalN
for (scenario in output_scenarios) {
  p3_driving_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$p3[,3]
  })
}

# Define the target length
target_length <- 18000

# Create a unified data frame
combined_data <- do.call(rbind, lapply(names(p1_values), function(scenario) {
  
  # Get probabilities for each variable
  p1_prob <- unlist(p1_values[[scenario]])
  p2_prob <- unlist(p2_values[[scenario]])
  p3_walking_prob <- unlist(p3_walking_values[[scenario]])
  p3_cycling_prob <- unlist(p3_cycling_values[[scenario]])
  p3_driving_prob <- unlist(p3_driving_values[[scenario]])
  
  # Fill with NA if the length is less than the target
  p1_prob <- c(p1_prob, rep(NA, target_length - length(p1_prob)))
  p2_prob <- c(p2_prob, rep(NA, target_length - length(p2_prob)))
  p3_walking_prob <- c(p3_walking_prob, rep(NA, target_length - length(p3_walking_prob)))
  p3_cycling_prob <- c(p3_cycling_prob, rep(NA, target_length - length(p3_cycling_prob)))
  p3_driving_prob <- c(p3_driving_prob, rep(NA, target_length - length(p3_driving_prob)))
  
  # Create the data frame for each variable
  rbind(
    data.frame(Variable = "p1", 
               Probability_value = p1_prob, 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario),
    
    data.frame(Variable = "p2", 
               Probability_value = p2_prob, 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario),
    
    data.frame(Variable = "p3 walking", 
               Probability_value = p3_walking_prob, 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario),
    
    data.frame(Variable = "p3 cycling", 
               Probability_value = p3_cycling_prob, 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario),
    
    data.frame(Variable = "p3 driving", 
               Probability_value = p3_driving_prob, 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario)
  )
}))

# Display the combined dataframe
print(combined_data)

# Group by 'Variable', 'Simulation', and 'scenario' and calculate the mean probability values
df_mean_grouped <- combined_data %>%
  group_by(Variable, Simulation, scenario) %>%
  summarise(mean_value = mean(Probability_value, na.rm = TRUE)) %>%
  ungroup()

# Display the new dataframe with calculated means
print(df_mean_grouped)

# Group by 'Variable', 'Simulation', and 'scenario' and calculate the variance of probability values
df_var_grouped <- combined_data %>%
  group_by(Variable, Simulation, scenario) %>%
  summarise(var_value = var(Probability_value, na.rm = TRUE)) %>%
  ungroup()

# Display the new dataframe with calculated variances
print(df_var_grouped)




# Merge the dataframes by 'Variable', 'Simulation', and 'scenario'
df_combined <- df_mean_grouped %>%
  left_join(df_var_grouped, by = c("Variable", "Simulation", "scenario"))

# Display the new combined dataframe
print(df_combined)

# Observed values
real_values <- data.frame(
  Variable = c("p1", "p2", "p3 walking", "p3 cycling", "p3 driving"),
  real_value = c(selected_group$Obs_val_p1, Obs_val_p2, selected_group$Obs_val_p3_walking, selected_group$Obs_val_p3_cycling, selected_group$Obs_val_p3_driving)
)

# Display the real values dataframe
print(real_values)

# Merge df_combined with real_values to add the real_value column
df_combined <- df_combined %>%
  left_join(real_values, by = "Variable")

# Display the combined dataframe with the new column
print(df_combined)

# Calculate RRMSE
calculate_rrmse <- function(estimated, real, variance) {
  return(sqrt((estimated - real)^2 + variance) / real)
}

# Initialize a list to store the results
rrmse_list_se01_nsites10 <- list()

# Loop over each variable
for (variable in unique(df_combined$Variable)) {
  
  # Filter data for the current variable
  df_variable <- df_combined %>% filter(Variable == variable)
  
  # Initialize sublist for the current variable
  rrmse_list_se01_nsites10[[variable]] <- list()
  
  # Loop over each scenario
  for (scenario_name in unique(df_variable$scenario)) {
    
    # Filter data for the current scenario
    df_scenario <- df_variable %>% filter(scenario == scenario_name)
    
    # Initialize sublist for the current scenario
    rrmse_list_se01_nsites10[[variable]][[scenario_name]] <- list()
    
    # Loop over each simulation
    for (sim in unique(df_scenario$Simulation)) {
      
      # Filter data for the current simulation
      df_simulation <- df_scenario %>% filter(Simulation == sim)
      
      # Extract estimated values, real value, and variance
      estimated <- df_simulation$mean_value
      real <- df_simulation$real_value[1]  # Assumes constant real_value per group
      variance <- df_simulation$var_value
      
      # Compute RRMSE if all necessary values are present
      if (!is.null(estimated) && length(estimated) > 0 && 
          !is.null(real) && length(real) > 0 && 
          !is.null(variance) && length(variance) > 0) {
        
        rrmse_list_se01_nsites10[[variable]][[scenario_name]][[sim]] <- 
          calculate_rrmse(estimated, real, variance)
        
      } else {
        rrmse_list_se01_nsites10[[variable]][[scenario_name]][[sim]] <- NA
        message(paste("Missing or invalid data for variable:", variable, 
                      "scenario:", scenario_name, "simulation:", sim))
      }
    }
  }
}

# Check the results for a specific variable, e.g., "p1"
print(rrmse_list_se01_nsites10[["p1"]])

# Create the RRMSE DataFrame from rrmse_list_se01_nsites10
rrmse_df_01_10 <- data.frame()

for (variable in names(rrmse_list_se01_nsites10)) {
  for (scenario in names(rrmse_list_se01_nsites10[[variable]])) {
    for (sim in 1:length(rrmse_list_se01_nsites10[[variable]][[scenario]])) {
      rrmse_value <- rrmse_list_se01_nsites10[[variable]][[scenario]][[sim]]
      
      # Add row to DataFrame
      rrmse_df_01_10 <- rbind(rrmse_df_01_10, data.frame(
        Variable = variable,
        RRMSE = rrmse_value,
        scenario = scenario,
        Simulation = sim
      ))
    }
  }
}

# Verify that all scenarioes have been correctly mapped
print(unique(rrmse_df_01_10$scenario))
print(rrmse_df_01_10)  # Display the complete DataFrame


# Define scenarios dynamically
scenarios <- c(
  paste0(selected_group$path_group_se_01, "_nsites_10_SD_N_0_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group_se_01, "_nsites_10_SD_N_0_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group_se_01, "_nsites_10_SD_N_0_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group_se_01, "_nsites_10_SD_N_0.5_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group_se_01, "_nsites_10_SD_N_0.5_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group_se_01, "_nsites_10_SD_N_0.5_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group_se_01, "_nsites_10_SD_N_1.5_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group_se_01, "_nsites_10_SD_N_1.5_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group_se_01, "_nsites_10_SD_N_1.5_SD_p2_0.15_output_analysis_")
)

# Define user-friendly labels
labels <- c(
  "SD N 0 p2 0", "SD N 0 p2 0.05", "SD N 0 p2 0.15",
  "SD N 0.5 p2 0", "SD N 0.5 p2 0.05", "SD N 0.5 p2 0.15",
  "SD N 1.5 p2 0", "SD N 1.5 p2 0.05", "SD N 1.5 p2 0.15"
)

# Create mapping using setNames()
scenario_map <- setNames(labels, scenarios)

# Map scenarios to user-friendly labels
rrmse_df_01_10$scenario <- factor(rrmse_df_01_10$scenario, levels = names(scenario_map), labels = scenario_map)

# Verify that all scenarios have been correctly mapped
print(unique(rrmse_df_01_10$scenario))

# Remove rows with NA in any column
# This is necessary to compute Generalized RRMSE properly
# and to avoid including the variable 'p1' in the analysis
# for vertebrate groups that are not affected by carcass location bias
rrmse_df_01_10 <- rrmse_df_01_10 %>%
  drop_na()

# Calculate generalized RRMSE using the product, ignoring any remaining NAs (if any)
rrmse_df_01_10 <- rrmse_df_01_10 %>%
  group_by(scenario, Simulation) %>%
  summarise(Generalized_RRMSE = prod(RRMSE, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Generalized_RRMSE = Generalized_RRMSE ^ (1 / length(unique(rrmse_df_01_10$Variable))))

# Display the dataframe with the new column
print(rrmse_df_01_10)

# ------------------------------------------------------------------------------
#4.Standar Error carcass location and persistence bias = 0.10 nº transects = 100
# ------------------------------------------------------------------------------

# Function to load files and store them in a list
load_outputs <- function(base_path, scenario, simulations) {
  files <- sapply(simulations, function(sim) {
    paste0(base_path, scenario, "sim_", sim, "_1.RData")
  })
  
  list_outputs <- lapply(files, function(file) {
    tryCatch({
      # Attempt to read the RData file
      readRDS(file)
    }, error = function(e) {
      # Show a warning if the file cannot be opened
      warning(paste("Could not open file:", file, "- Error:", e$message))
      return(NULL) # Return NULL if the file cannot be loaded
    })
  })
  
  # Filter out NULL values (unloaded files)
  list_outputs <- Filter(Negate(is.null), list_outputs)
  
  return(list_outputs)
}


# Create the new object with the combined path
path_se01ntransect100 <- paste0(file.path(path_data, selected_group$se01ntransect100), "/")

# Print the path
print(path_se01ntransect100)

# Generate output scenarios with integrated path_group_se_01
output_scenarios <- c(
  paste0(selected_group$path_group_se_01, "_nsites_100_SD_N_0.5_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group_se_01, "_nsites_100_SD_N_0.5_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group_se_01, "_nsites_100_SD_N_0.5_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group_se_01, "_nsites_100_SD_N_1.5_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group_se_01, "_nsites_100_SD_N_1.5_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group_se_01, "_nsites_100_SD_N_1.5_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group_se_01, "_nsites_100_SD_N_0_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group_se_01, "_nsites_100_SD_N_0_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group_se_01, "_nsites_100_SD_N_0_SD_p2_0_output_analysis_")
)

# Simulations, in this case, we have 20
simulations <- 1:20

# Load data for all scenarios and simulations
outputs <- list()
for (scenario in output_scenarios) {
  data <- load_outputs(path_se01ntransect100, scenario, simulations)
  outputs[[scenario]] <- data
}

# Verify loaded data
print(outputs[[output_scenarios[1]]][[1]])

# Create an object to store the Probability values of p1 from the output analysis for each scenario
p1_values <- list()

# Iterate over scenarios and simulations to extract p1, replacing NULL with NA 
#for the vertebrate groups not affected by carcass location bias
for (scenario in output_scenarios) {
  p1_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    if (is.null(output$sims.list$p1)) {
      NA  
    } else {
      output$sims.list$p1
    }
  })
}


# Create an object to store the Probability_values of p2 from the output analysis for each level
p2_values <- list()

# Iterate over scenarioes and simulations to extract totalN
for (scenario in output_scenarios) {
  p2_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$p2
  })
}

# Create an object to store the Probability_values of p3 walking from the output analysis for each level
p3_walking_values <- list()

# Iterate over scenarioes and simulations to extract totalN
for (scenario in output_scenarios) {
  p3_walking_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$p3[,1]
  })
}

# Create an object to store the Probability_values of p3 cycling from the output analysis for each level
p3_cycling_values <- list()

# Iterate over scenarioes and simulations to extract totalN
for (scenario in output_scenarios) {
  p3_cycling_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$p3[,2]
  })
}

# Create an object to store the Probability_values of p3 driving from the output analysis for each level
p3_driving_values <- list()

# Iterate over scenarioes and simulations to extract totalN
for (scenario in output_scenarios) {
  p3_driving_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$p3[,3]
  })
}

# Define the target length
target_length <- 18000

# Create a unified data frame
combined_data <- do.call(rbind, lapply(names(p1_values), function(scenario) {
  
  # Get probabilities for each variable
  p1_prob <- unlist(p1_values[[scenario]])
  p2_prob <- unlist(p2_values[[scenario]])
  p3_walking_prob <- unlist(p3_walking_values[[scenario]])
  p3_cycling_prob <- unlist(p3_cycling_values[[scenario]])
  p3_driving_prob <- unlist(p3_driving_values[[scenario]])
  
  # Fill with NA if the length is less than the target
  p1_prob <- c(p1_prob, rep(NA, target_length - length(p1_prob)))
  p2_prob <- c(p2_prob, rep(NA, target_length - length(p2_prob)))
  p3_walking_prob <- c(p3_walking_prob, rep(NA, target_length - length(p3_walking_prob)))
  p3_cycling_prob <- c(p3_cycling_prob, rep(NA, target_length - length(p3_cycling_prob)))
  p3_driving_prob <- c(p3_driving_prob, rep(NA, target_length - length(p3_driving_prob)))
  
  # Create the data frame for each variable
  rbind(
    data.frame(Variable = "p1", 
               Probability_value = p1_prob, 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario),
    
    data.frame(Variable = "p2", 
               Probability_value = p2_prob, 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario),
    
    data.frame(Variable = "p3 walking", 
               Probability_value = p3_walking_prob, 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario),
    
    data.frame(Variable = "p3 cycling", 
               Probability_value = p3_cycling_prob, 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario),
    
    data.frame(Variable = "p3 driving", 
               Probability_value = p3_driving_prob, 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario)
  )
}))

# Display the combined dataframe
print(combined_data)

# Group by 'Variable', 'Simulation', and 'scenario', and calculate the mean probability values
df_mean_grouped <- combined_data %>%
  group_by(Variable, Simulation, scenario) %>%
  summarise(mean_value = mean(Probability_value, na.rm = TRUE)) %>%
  ungroup()

# Display the new dataframe with calculated means
print(df_mean_grouped)

# Group by 'Variable', 'Simulation', and 'scenario', and calculate the variance of probability values
df_var_grouped <- combined_data %>%
  group_by(Variable, Simulation, scenario) %>%
  summarise(var_value = var(Probability_value, na.rm = TRUE)) %>%
  ungroup()

# Display the new dataframe with calculated variances
print(df_var_grouped)

# Merge the dataframes by 'Variable', 'Simulation', and 'scenario'
df_combined <- df_mean_grouped %>%
  left_join(df_var_grouped, by = c("Variable", "Simulation", "scenario"))

# Display the new combined dataframe
print(df_combined)

# Observed values
real_values <- data.frame(
  Variable = c("p1", "p2", "p3 walking", "p3 cycling", "p3 driving"),
  real_value = c(selected_group$Obs_val_p1, Obs_val_p2, selected_group$Obs_val_p3_walking, selected_group$Obs_val_p3_cycling, selected_group$Obs_val_p3_driving)
)

# Display the dataframe of real values
print(real_values)

# Merge df_combined with real_values to add the 'real_value' column
df_combined <- df_combined %>%
  left_join(real_values, by = "Variable")

# Display the combined dataframe with the new column
print(df_combined)

# Calculate RRMSE
calculate_rrmse <- function(estimated, real, variance) {
  return(sqrt((estimated - real)^2 + variance) / real)
}

# Initialize a list to store results
rrmse_list_se01_nsites100 <- list()

# Loop over each variable
for (variable in unique(df_combined$Variable)) {
  
  # Filter data for the current variable
  df_variable <- df_combined %>% filter(Variable == variable)
  
  # Initialize sublist for the current variable
  rrmse_list_se01_nsites100[[variable]] <- list()
  
  # Loop over each scenario
  for (scenario_name in unique(df_variable$scenario)) {
    
    # Filter data for the current scenario
    df_scenario <- df_variable %>% filter(scenario == scenario_name)
    
    # Initialize sublist for the current scenario
    rrmse_list_se01_nsites100[[variable]][[scenario_name]] <- list()
    
    # Loop over each simulation
    for (sim in unique(df_scenario$Simulation)) {
      
      # Filter data for the current simulation
      df_simulation <- df_scenario %>% filter(Simulation == sim)
      
      # Extract estimated values, real value, and variance
      estimated <- df_simulation$mean_value
      real <- df_simulation$real_value[1]  # Assumes constant real_value per group
      variance <- df_simulation$var_value
      
      # Compute RRMSE if all necessary values are present
      if (!is.null(estimated) && length(estimated) > 0 && 
          !is.null(real) && length(real) > 0 && 
          !is.null(variance) && length(variance) > 0) {
        
        rrmse_list_se01_nsites100[[variable]][[scenario_name]][[sim]] <- 
          calculate_rrmse(estimated, real, variance)
        
      } else {
        rrmse_list_se01_nsites100[[variable]][[scenario_name]][[sim]] <- NA
        message(paste("Missing or invalid data for variable:", variable, 
                      "scenario:", scenario_name, "simulation:", sim))
      }
    }
  }
}

# Check the results for a specific variable, e.g., "p1"
print(rrmse_list_se01_nsites100[["p1"]])

# Create the RRMSE DataFrame from rrmse_list_se01_nsites100
rrmse_df_01_100 <- data.frame()

for (variable in names(rrmse_list_se01_nsites100)) {
  for (scenario in names(rrmse_list_se01_nsites100[[variable]])) {
    for (sim in 1:length(rrmse_list_se01_nsites100[[variable]][[scenario]])) {
      rrmse_value <- rrmse_list_se01_nsites100[[variable]][[scenario]][[sim]]
      
      # Add row to DataFrame
      rrmse_df_01_100 <- rbind(rrmse_df_01_100, data.frame(
        Variable = variable,
        RRMSE = rrmse_value,
        scenario = scenario,
        Simulation = sim
      ))
    }
  }
}

# Verify that all scenarioes have been mapped correctly
print(unique(rrmse_df_01_100$scenario))
print(rrmse_df_01_100)  # Display the complete DataFrame


# Define scenarios dynamically
scenarios <- c(
  paste0(selected_group$path_group_se_01, "_nsites_100_SD_N_0_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group_se_01, "_nsites_100_SD_N_0_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group_se_01, "_nsites_100_SD_N_0_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group_se_01, "_nsites_100_SD_N_0.5_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group_se_01, "_nsites_100_SD_N_0.5_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group_se_01, "_nsites_100_SD_N_0.5_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group_se_01, "_nsites_100_SD_N_1.5_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group_se_01, "_nsites_100_SD_N_1.5_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group_se_01, "_nsites_100_SD_N_1.5_SD_p2_0.15_output_analysis_")
)

# Define user-friendly labels
labels <- c(
  "SD N 0 p2 0", "SD N 0 p2 0.05", "SD N 0 p2 0.15",
  "SD N 0.5 p2 0", "SD N 0.5 p2 0.05", "SD N 0.5 p2 0.15",
  "SD N 1.5 p2 0", "SD N 1.5 p2 0.05", "SD N 1.5 p2 0.15"
)

# Create mapping using setNames()
scenario_map <- setNames(labels, scenarios)

# Map scenarios to user-friendly labels
rrmse_df_01_100$scenario <- factor(rrmse_df_01_100$scenario, levels = names(scenario_map), labels = scenario_map)

# Verify that all scenarios have been correctly mapped
print(unique(rrmse_df_01_100$scenario))

# Remove rows with NA in any column
# This is necessary to compute Generalized RRMSE properly
# and to avoid including the variable 'p1' in the analysis
# for vertebrate groups that are not affected by carcass location bias
rrmse_df_01_100 <- rrmse_df_01_100 %>%
  drop_na()

# Calculate generalized RRMSE using the product, ignoring any remaining NAs (if any)
rrmse_df_01_100 <- rrmse_df_01_100 %>%
  group_by(scenario, Simulation) %>%
  summarise(Generalized_RRMSE = prod(RRMSE, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Generalized_RRMSE = Generalized_RRMSE ^ (1 / length(unique(rrmse_df_01_100$Variable))))

# Display the dataframe with the new column
print(rrmse_df_01_100)

# Add the 'SE nsites' column to each dataframe
rrmse_df_005_10$SE_nsites <- "SE 0.05 nsites 10"
rrmse_df_005_100$SE_nsites <- "SE 0.05 nsites 100"
rrmse_df_01_10$SE_nsites <- "SE 0.1 nsites 10"
rrmse_df_01_100$SE_nsites <- "SE 0.1 nsites 100"

# Combine all dataframes into a single one
combined_df <- rbind(rrmse_df_005_10, rrmse_df_005_100, rrmse_df_01_10, rrmse_df_01_100)

# Check the structure of the combined dataframe
head(combined_df)

# Create a copy of the combined dataframe and add the 'Group' column
selected_group$RRMSE <- combined_df
selected_group$RRMSE$Group <- selected_group$Group

# Check the structure of the new dataframe
head(selected_group$RRMSE)

# Create a global list to store all RRMSE results
if (!exists("all_RRMSE_list")) {
  all_RRMSE_list <- list()
}

# Create a copy of the combined dataframe and add the 'Group' column
selected_group$RRMSE <- combined_df
selected_group$RRMSE$Group <- selected_group$Group

# Store the result in the global list
all_RRMSE_list[[selected_group$Group]] <- selected_group$RRMSE

# Check the structure of the current RRMSE
head(selected_group$RRMSE)

# Combine all stored RRMSE dataframes into one final dataframe
final_RRMSE <- do.call(rbind, all_RRMSE_list)

# Check the structure of the final dataframe
head(final_RRMSE)


#####Run code for Mammals G5 after running the code for the rest of vertebrate groups#####

#Mammals G5
# ------------------------------------------------------------------------------
#1.Standar Error carcass location and persistence bias = 0.05 nº transects = 10
# ------------------------------------------------------------------------------


# Function to load files and store them in a list
load_outputs <- function(base_path, scenario, simulations) {
  files <- sapply(simulations, function(sim) {
    paste0(base_path, scenario, "sim_", sim, "_1.RData")
  })
  
  list_outputs <- lapply(files, function(file) {
    tryCatch({
      # Attempt to read the RData file
      readRDS(file)
    }, error = function(e) {
      # Show a warning if the file cannot be opened
      warning(paste("Could not open file:", file, "- Error:", e$message))
      return(NULL) # Return NULL if the file cannot be loaded
    })
  })
  
  # Filter out NULL values (unloaded files)
  list_outputs <- Filter(Negate(is.null), list_outputs)
  
  return(list_outputs)
}
# Create the new object with the combined path
path_se005ntransect10 <- paste0(file.path(path_data, "mammals_g5/sites10_se05"), "/")

# Print the path
print(path_se005ntransect10)

# Generate output scenarios with integrated path_group_se_005
output_scenarios <- c("ungulates_SE_p1_0.05_nsites_10_SD_N_0_SD_p1_0_output_analysis_")

# Simulations, in this case, we have 20
simulations <- 1:20

# Load data for all scenarios and simulations
outputs <- list()
for (scenario in output_scenarios) {
  data <- load_outputs(path_se005ntransect10, scenario, simulations)
  outputs[[scenario]] <- data
}

# Verify loaded data
print(outputs[[output_scenarios[1]]][[1]])


# Create an object to store the Probability_values of p1 from the output analysis for each level
p1_values <- list()

# Iterate over scenarioes and simulations to extract p1
for (scenario in output_scenarios) {
  p1_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$p1
  })
}


# Create an object to store the Probability_values of p3 walking from the output analysis for each level
p3_walking_values <- list()

# Iterate over scenarioes and simulations to extract totalN
for (scenario in output_scenarios) {
  p3_walking_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$p3[,1]
  })
}

# Create an object to store the Probability_values of p3 cycling from the output analysis for each level
p3_cycling_values <- list()

# Iterate over scenarioes and simulations to extract totalN
for (scenario in output_scenarios) {
  p3_cycling_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$p3[,2]
  })
}

# Create an object to store the Probability_values of p3 driving from the output analysis for each level
p3_driving_values <- list()

# Iterate over scenarioes and simulations to extract totalN
for (scenario in output_scenarios) {
  p3_driving_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$p3[,3]
  })
}

# Create a unified data frame for all plots, adding the 'Simulation' and 'scenario' columns
combined_data <- do.call(rbind, lapply(names(p1_values), function(scenario) {
  rbind(
    data.frame(Variable = "p1", 
               Probability_value = unlist(p1_values[[scenario]]), 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario),
    
    data.frame(Variable = "p3 walking", 
               Probability_value = unlist(p3_walking_values[[scenario]]), 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario),
    
    data.frame(Variable = "p3 cycling", 
               Probability_value = unlist(p3_cycling_values[[scenario]]), 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario),
    
    data.frame(Variable = "p3 driving", 
               Probability_value = unlist(p3_driving_values[[scenario]]), 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario)
  )
}))

# Display the combined dataframe
print(combined_data)

# Group by 'Variable', 'Simulation', and 'scenario' and calculate the mean probability values
df_mean_grouped <- combined_data %>%
  group_by(Variable, Simulation, scenario) %>%
  summarise(mean_value = mean(Probability_value, na.rm = TRUE)) %>%
  ungroup()

# Display the new dataframe with calculated means
print(df_mean_grouped)

# Group by 'Variable', 'Simulation', and 'scenario' and calculate the variance of probability values
df_var_grouped <- combined_data %>%
  group_by(Variable, Simulation, scenario) %>%
  summarise(var_value = var(Probability_value, na.rm = TRUE)) %>%
  ungroup()

# Display the new dataframe with calculated variances
print(df_var_grouped)




# Merge the dataframes by 'Variable', 'Simulation', and 'scenario'
df_combined <- df_mean_grouped %>%
  left_join(df_var_grouped, by = c("Variable", "Simulation", "scenario"))

# Display the new combined dataframe
print(df_combined)

# Observed values
real_values <- data.frame(
  Variable = c("p1", "p3 walking", "p3 cycling", "p3 driving"),
  real_value = c(0.5, 1, 0.9, 0.8)
)

# Display the real values dataframe
print(real_values)

# Merge df_combined with real_values to add the real_value column
df_combined <- df_combined %>%
  left_join(real_values, by = "Variable")

# Display the combined dataframe with the new column
print(df_combined)


# Calculate RRMSE
calculate_rrmse <- function(estimated, real, variance) {
  return(sqrt((estimated - real)^2 + variance) / real)
}

# Initialize a list to store the results
rrmse_list_se005_nsites10 <- list()

# Group by 'Variable', 'Simulation', and 'scenario' and calculate the RRMSE
for (variable in unique(df_combined$Variable)) {
  # Filter data by variable
  df_variable <- df_combined %>% filter(Variable == variable)
  
  rrmse_list_se005_nsites10[[variable]] <- list()
  
  for (scenario_name in unique(df_variable$scenario)) {
    df_scenario <- df_variable %>% filter(scenario == scenario_name)
    
    
    for (sim in unique(df_scenario$Simulation)) {
      # Filter by simulation
      df_simulation <- df_scenario %>% filter(Simulation == sim)
      
      # Extract estimated, real, and variance values
      estimated <- df_simulation$mean_value
      real <- df_simulation$real_value[1]  # As real_value is the same for all rows in this simulation and scenario
      variance <- df_simulation$var_value
      
      # Calculate RRMSE
      if (!is.null(estimated) && length(estimated) > 0 && !is.null(real) && length(real) > 0 && !is.null(variance) && length(variance) > 0) {
        rrmse_list_se005_nsites10[[variable]][[scenario]][[sim]] <- calculate_rrmse(estimated, real, variance)
      } else {
        rrmse_list_se005_nsites10[[variable]][[scenario]][[sim]] <- NA  # Or another value to indicate missing data
        message(paste("Missing or invalid data for variable:", variable, "scenario:", scenario, "simulation:", sim))
      }
    }
  }
}

# Check the results for a specific variable, e.g., "p1"
print(rrmse_list_se005_nsites10[["p1"]])



# Create the RRMSE DataFrame from rrmse_list_se005_nsites10
rrmse_df_005_10 <- data.frame()

for (variable in names(rrmse_list_se005_nsites10)) {
  for (scenario in names(rrmse_list_se005_nsites10[[variable]])) {
    for (sim in 1:length(rrmse_list_se005_nsites10[[variable]][[scenario]])) {
      rrmse_value <- rrmse_list_se005_nsites10[[variable]][[scenario]][[sim]]
      
      # Add row to DataFrame
      rrmse_df_005_10 <- rbind(rrmse_df_005_10, data.frame(
        Variable = variable,
        RRMSE = rrmse_value,
        scenario = scenario,
        Simulation = sim
      ))
    }
  }
}

# Verify that all scenarioes have been correctly mapped
print(unique(rrmse_df_005_10$scenario))
print(rrmse_df_005_10)  # Display the complete DataFrame


# Define scenarios dynamically
scenarios <- c(
  "ungulates_SE_p1_0.05_nsites_10_SD_N_0_SD_p1_0_output_analysis_"
)

# Define user-friendly labels
labels <- c(
  "SD N 0 p2 0")

# Create mapping using setNames()
scenario_map <- setNames(labels, scenarios)

# Map scenarios to user-friendly labels
rrmse_df_005_10$scenario <- factor(rrmse_df_005_10$scenario, levels = names(scenario_map), labels = scenario_map)

# Verify that all scenarios have been correctly mapped
print(unique(rrmse_df_005_10$scenario))

# Calculate generalized RRMSE using the product
rrmse_df_005_10 <- rrmse_df_005_10 %>%
  group_by(scenario, Simulation) %>%
  summarise(Generalized_RRMSE = prod(RRMSE, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Generalized_RRMSE = Generalized_RRMSE ^ (1 / length(unique(rrmse_df_005_10$Variable))))

# Display the dataframe with the new column
print(rrmse_df_005_10)

# ------------------------------------------------------------------------------
#2.Standar Error carcass location and persistence bias = 0.05 nº transects = 100
# ------------------------------------------------------------------------------

# Function to load files and store them in a list
load_outputs <- function(base_path, scenario, simulations) {
  files <- sapply(simulations, function(sim) {
    paste0(base_path, scenario, "sim_", sim, "_1.RData")
  })
  
  list_outputs <- lapply(files, function(file) {
    tryCatch({
      # Attempt to read the RData file
      readRDS(file)
    }, error = function(e) {
      # Show a warning if the file cannot be opened
      warning(paste("Could not open file:", file, "- Error:", e$message))
      return(NULL) # Return NULL if the file cannot be loaded
    })
  })
  
  # Filter out NULL values (unloaded files)
  list_outputs <- Filter(Negate(is.null), list_outputs)
  
  return(list_outputs)
}

# Create the new object with the combined path
path_se005ntransect100 <- paste0(file.path(path_data, "mammals_g5/sites100_se05"), "/")

# Print the path
print(path_se005ntransect100)

# Generate output scenarios with integrated path_group_se_005
output_scenarios <- c(
  "ungulates_SE_p1_0.05_nsites_100_SD_N_0_SD_p1_0_output_analysis_"
)

# Simulations, in this case, we have 20
simulations <- 1:20

# Load data for all scenarios and simulations
outputs <- list()
for (scenario in output_scenarios) {
  data <- load_outputs(path_se005ntransect100, scenario, simulations)
  outputs[[scenario]] <- data
}

# Verify loaded data
print(outputs[[output_scenarios[1]]][[1]])



# Create an object to store the Probability_values of p1 from the output analysis for each level
p1_values <- list()

# Iterate over scenarioes and simulations to extract p1
for (scenario in output_scenarios) {
  p1_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$p1
  })
}


# Create an object to store the Probability_values of p3 walking from the output analysis for each level
p3_walking_values <- list()

# Iterate over scenarioes and simulations to extract totalN
for (scenario in output_scenarios) {
  p3_walking_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$p3[,1]
  })
}

# Create an object to store the Probability_values of p3 cycling from the output analysis for each level
p3_cycling_values <- list()

# Iterate over scenarioes and simulations to extract totalN
for (scenario in output_scenarios) {
  p3_cycling_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$p3[,2]
  })
}

# Create an object to store the Probability_values of p3 driving from the output analysis for each level
p3_driving_values <- list()

# Iterate over scenarioes and simulations to extract totalN
for (scenario in output_scenarios) {
  p3_driving_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$p3[,3]
  })
}

# Create a unified data frame for all plots, adding the 'Simulation' and 'scenario' columns
combined_data <- do.call(rbind, lapply(names(p1_values), function(scenario) {
  rbind(
    data.frame(Variable = "p1", 
               Probability_value = unlist(p1_values[[scenario]]), 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario),
    
    data.frame(Variable = "p3 walking", 
               Probability_value = unlist(p3_walking_values[[scenario]]), 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario),
    
    data.frame(Variable = "p3 cycling", 
               Probability_value = unlist(p3_cycling_values[[scenario]]), 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario),
    
    data.frame(Variable = "p3 driving", 
               Probability_value = unlist(p3_driving_values[[scenario]]), 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario)
  )
}))

# Display the combined dataframe
print(combined_data)

# Group by 'Variable', 'Simulation', and 'scenario' and calculate the mean probability values
df_mean_grouped <- combined_data %>%
  group_by(Variable, Simulation, scenario) %>%
  summarise(mean_value = mean(Probability_value, na.rm = TRUE)) %>%
  ungroup()

# Display the new dataframe with calculated means
print(df_mean_grouped)

# Group by 'Variable', 'Simulation', and 'scenario' and calculate the variance of probability values
df_var_grouped <- combined_data %>%
  group_by(Variable, Simulation, scenario) %>%
  summarise(var_value = var(Probability_value, na.rm = TRUE)) %>%
  ungroup()

# Display the new dataframe with calculated variances
print(df_var_grouped)




# Merge the dataframes by 'Variable', 'Simulation', and 'scenario'
df_combined <- df_mean_grouped %>%
  left_join(df_var_grouped, by = c("Variable", "Simulation", "scenario"))

# Display the new combined dataframe
print(df_combined)

# Observed values
real_values <- data.frame(
  Variable = c("p1", "p3 walking", "p3 cycling", "p3 driving"),
  real_value = c(0.5, 1, 0.9, 0.8)
)

# Display the real values dataframe
print(real_values)

# Merge df_combined with real_values to add the real_value column
df_combined <- df_combined %>%
  left_join(real_values, by = "Variable")

# Display the combined dataframe with the new column
print(df_combined)


# Calculate RRMSE
calculate_rrmse <- function(estimated, real, variance) {
  return(sqrt((estimated - real)^2 + variance) / real)
}

# Initialize a list to store the results
rrmse_list_se005_nsites100 <- list()

# Group by 'Variable', 'Simulation', and 'scenario' and calculate the RRMSE
for (variable in unique(df_combined$Variable)) {
  # Filter data by variable
  df_variable <- df_combined %>% filter(Variable == variable)
  
  rrmse_list_se005_nsites100[[variable]] <- list()
  
  for (scenario_name in unique(df_variable$scenario)) {
    df_scenario <- df_variable %>% filter(scenario == scenario_name)
    
    
    for (sim in unique(df_scenario$Simulation)) {
      # Filter by simulation
      df_simulation <- df_scenario %>% filter(Simulation == sim)
      
      # Extract estimated, real, and variance values
      estimated <- df_simulation$mean_value
      real <- df_simulation$real_value[1]  # As real_value is the same for all rows in this simulation and scenario
      variance <- df_simulation$var_value
      
      # Calculate RRMSE
      if (!is.null(estimated) && length(estimated) > 0 && !is.null(real) && length(real) > 0 && !is.null(variance) && length(variance) > 0) {
        rrmse_list_se005_nsites100[[variable]][[scenario]][[sim]] <- calculate_rrmse(estimated, real, variance)
      } else {
        rrmse_list_se005_nsites100[[variable]][[scenario]][[sim]] <- NA  # Or another value to indicate missing data
        message(paste("Missing or invalid data for variable:", variable, "scenario:", scenario, "simulation:", sim))
      }
    }
  }
}

# Check the results for a specific variable, e.g., "p1"
print(rrmse_list_se005_nsites100[["p1"]])



# Create the RRMSE DataFrame from rrmse_list_se005_nsites100
rrmse_df_005_100 <- data.frame()

for (variable in names(rrmse_list_se005_nsites100)) {
  for (scenario in names(rrmse_list_se005_nsites100[[variable]])) {
    for (sim in 1:length(rrmse_list_se005_nsites100[[variable]][[scenario]])) {
      rrmse_value <- rrmse_list_se005_nsites100[[variable]][[scenario]][[sim]]
      
      # Add row to DataFrame
      rrmse_df_005_100 <- rbind(rrmse_df_005_100, data.frame(
        Variable = variable,
        RRMSE = rrmse_value,
        scenario = scenario,
        Simulation = sim
      ))
    }
  }
}

# Verify that all scenarioes have been correctly mapped
print(unique(rrmse_df_005_100$scenario))
print(rrmse_df_005_100)  # Display the complete DataFrame


# Define scenarios dynamically
scenarios <- c(
  "ungulates_SE_p1_0.05_nsites_100_SD_N_0_SD_p1_0_output_analysis_"
)

# Define user-friendly labels
labels <- c(
  "SD N 0 p2 0")

# Create mapping using setNames()
scenario_map <- setNames(labels, scenarios)

# Map scenarios to user-friendly labels
rrmse_df_005_100$scenario <- factor(rrmse_df_005_100$scenario, levels = names(scenario_map), labels = scenario_map)

# Verify that all scenarios have been correctly mapped
print(unique(rrmse_df_005_100$scenario))

# Calculate generalized RRMSE using the product
rrmse_df_005_100 <- rrmse_df_005_100 %>%
  group_by(scenario, Simulation) %>%
  summarise(Generalized_RRMSE = prod(RRMSE, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Generalized_RRMSE = Generalized_RRMSE ^ (1 / length(unique(rrmse_df_005_100$Variable))))

# Display the dataframe with the new column
print(rrmse_df_005_100)
# ------------------------------------------------------------------------------
#3.Standar Error carcass location and persistence bias = 0.1 nº transects = 10
# ------------------------------------------------------------------------------


# Function to load files and store them in a list
load_outputs <- function(base_path, scenario, simulations) {
  files <- sapply(simulations, function(sim) {
    paste0(base_path, scenario, "sim_", sim, "_1.RData")
  })
  
  list_outputs <- lapply(files, function(file) {
    tryCatch({
      # Attempt to read the RData file
      readRDS(file)
    }, error = function(e) {
      # Show a warning if the file cannot be opened
      warning(paste("Could not open file:", file, "- Error:", e$message))
      return(NULL) # Return NULL if the file cannot be loaded
    })
  })
  
  # Filter out NULL values (unloaded files)
  list_outputs <- Filter(Negate(is.null), list_outputs)
  
  return(list_outputs)
}
# Create the new object with the combined path
path_se005ntransect10 <- paste0(file.path(path_data, "mammals_g5/sites10_se10"), "/")

# Print the path
print(path_se005ntransect10)

# Generate output scenarios with integrated path_group_se_005
output_scenarios <- c("ungulates_SE_p1_0.1_nsites_10_SD_N_0_SD_p1_0_output_analysis_")

# Simulations, in this case, we have 20
simulations <- 1:20

# Load data for all scenarios and simulations
outputs <- list()
for (scenario in output_scenarios) {
  data <- load_outputs(path_se005ntransect10, scenario, simulations)
  outputs[[scenario]] <- data
}

# Verify loaded data
print(outputs[[output_scenarios[1]]][[1]])


# Create an object to store the Probability_values of p1 from the output analysis for each level
p1_values <- list()

# Iterate over scenarioes and simulations to extract p1
for (scenario in output_scenarios) {
  p1_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$p1
  })
}


# Create an object to store the Probability_values of p3 walking from the output analysis for each level
p3_walking_values <- list()

# Iterate over scenarioes and simulations to extract totalN
for (scenario in output_scenarios) {
  p3_walking_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$p3[,1]
  })
}

# Create an object to store the Probability_values of p3 cycling from the output analysis for each level
p3_cycling_values <- list()

# Iterate over scenarioes and simulations to extract totalN
for (scenario in output_scenarios) {
  p3_cycling_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$p3[,2]
  })
}

# Create an object to store the Probability_values of p3 driving from the output analysis for each level
p3_driving_values <- list()

# Iterate over scenarioes and simulations to extract totalN
for (scenario in output_scenarios) {
  p3_driving_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$p3[,3]
  })
}

# Create a unified data frame for all plots, adding the 'Simulation' and 'scenario' columns
combined_data <- do.call(rbind, lapply(names(p1_values), function(scenario) {
  rbind(
    data.frame(Variable = "p1", 
               Probability_value = unlist(p1_values[[scenario]]), 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario),
    
    data.frame(Variable = "p3 walking", 
               Probability_value = unlist(p3_walking_values[[scenario]]), 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario),
    
    data.frame(Variable = "p3 cycling", 
               Probability_value = unlist(p3_cycling_values[[scenario]]), 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario),
    
    data.frame(Variable = "p3 driving", 
               Probability_value = unlist(p3_driving_values[[scenario]]), 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario)
  )
}))

# Display the combined dataframe
print(combined_data)

# Group by 'Variable', 'Simulation', and 'scenario' and calculate the mean probability values
df_mean_grouped <- combined_data %>%
  group_by(Variable, Simulation, scenario) %>%
  summarise(mean_value = mean(Probability_value, na.rm = TRUE)) %>%
  ungroup()

# Display the new dataframe with calculated means
print(df_mean_grouped)

# Group by 'Variable', 'Simulation', and 'scenario' and calculate the variance of probability values
df_var_grouped <- combined_data %>%
  group_by(Variable, Simulation, scenario) %>%
  summarise(var_value = var(Probability_value, na.rm = TRUE)) %>%
  ungroup()

# Display the new dataframe with calculated variances
print(df_var_grouped)




# Merge the dataframes by 'Variable', 'Simulation', and 'scenario'
df_combined <- df_mean_grouped %>%
  left_join(df_var_grouped, by = c("Variable", "Simulation", "scenario"))

# Display the new combined dataframe
print(df_combined)

# Observed values
real_values <- data.frame(
  Variable = c("p1", "p3 walking", "p3 cycling", "p3 driving"),
  real_value = c(0.5, 1, 0.9, 0.8)
)

# Display the real values dataframe
print(real_values)

# Merge df_combined with real_values to add the real_value column
df_combined <- df_combined %>%
  left_join(real_values, by = "Variable")

# Display the combined dataframe with the new column
print(df_combined)


# Calculate RRMSE
calculate_rrmse <- function(estimated, real, variance) {
  return(sqrt((estimated - real)^2 + variance) / real)
}

# Initialize a list to store the results
rrmse_list_se01_nsites10 <- list()

# Group by 'Variable', 'Simulation', and 'scenario' and calculate the RRMSE
for (variable in unique(df_combined$Variable)) {
  # Filter data by variable
  df_variable <- df_combined %>% filter(Variable == variable)
  
  rrmse_list_se01_nsites10[[variable]] <- list()
  
  for (scenario_name in unique(df_variable$scenario)) {
    df_scenario <- df_variable %>% filter(scenario == scenario_name)
    
    
    for (sim in unique(df_scenario$Simulation)) {
      # Filter by simulation
      df_simulation <- df_scenario %>% filter(Simulation == sim)
      
      # Extract estimated, real, and variance values
      estimated <- df_simulation$mean_value
      real <- df_simulation$real_value[1]  # As real_value is the same for all rows in this simulation and scenario
      variance <- df_simulation$var_value
      
      # Calculate RRMSE
      if (!is.null(estimated) && length(estimated) > 0 && !is.null(real) && length(real) > 0 && !is.null(variance) && length(variance) > 0) {
        rrmse_list_se01_nsites10[[variable]][[scenario]][[sim]] <- calculate_rrmse(estimated, real, variance)
      } else {
        rrmse_list_se01_nsites10[[variable]][[scenario]][[sim]] <- NA  # Or another value to indicate missing data
        message(paste("Missing or invalid data for variable:", variable, "scenario:", scenario, "simulation:", sim))
      }
    }
  }
}

# Check the results for a specific variable, e.g., "p1"
print(rrmse_list_se01_nsites10[["p1"]])



# Create the RRMSE DataFrame from rrmse_list_se01_nsites10
rrmse_df_01_10 <- data.frame()

for (variable in names(rrmse_list_se01_nsites10)) {
  for (scenario in names(rrmse_list_se01_nsites10[[variable]])) {
    for (sim in 1:length(rrmse_list_se01_nsites10[[variable]][[scenario]])) {
      rrmse_value <- rrmse_list_se01_nsites10[[variable]][[scenario]][[sim]]
      
      # Add row to DataFrame
      rrmse_df_01_10 <- rbind(rrmse_df_01_10, data.frame(
        Variable = variable,
        RRMSE = rrmse_value,
        scenario = scenario,
        Simulation = sim
      ))
    }
  }
}

# Verify that all scenarioes have been correctly mapped
print(unique(rrmse_df_01_10$scenario))
print(rrmse_df_01_10)  # Display the complete DataFrame


# Define scenarios dynamically
scenarios <- c(
  "ungulates_SE_p1_0.1_nsites_10_SD_N_0_SD_p1_0_output_analysis_"
)

# Define user-friendly labels
labels <- c(
  "SD N 0 p2 0")

# Create mapping using setNames()
scenario_map <- setNames(labels, scenarios)

# Map scenarios to user-friendly labels
rrmse_df_01_10$scenario <- factor(rrmse_df_01_10$scenario, levels = names(scenario_map), labels = scenario_map)

# Verify that all scenarios have been correctly mapped
print(unique(rrmse_df_01_10$scenario))

# Calculate generalized RRMSE using the product
rrmse_df_01_10 <- rrmse_df_01_10 %>%
  group_by(scenario, Simulation) %>%
  summarise(Generalized_RRMSE = prod(RRMSE, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Generalized_RRMSE = Generalized_RRMSE ^ (1 / length(unique(rrmse_df_01_10$Variable))))

# Display the dataframe with the new column
print(rrmse_df_01_10)

# ------------------------------------------------------------------------------
#4.Standar Error carcass location and persistence bias = 0.1 nº transects = 100
# ------------------------------------------------------------------------------


# Function to load files and store them in a list
load_outputs <- function(base_path, scenario, simulations) {
  files <- sapply(simulations, function(sim) {
    paste0(base_path, scenario, "sim_", sim, "_1.RData")
  })
  
  list_outputs <- lapply(files, function(file) {
    tryCatch({
      # Attempt to read the RData file
      readRDS(file)
    }, error = function(e) {
      # Show a warning if the file cannot be opened
      warning(paste("Could not open file:", file, "- Error:", e$message))
      return(NULL) # Return NULL if the file cannot be loaded
    })
  })
  
  # Filter out NULL values (unloaded files)
  list_outputs <- Filter(Negate(is.null), list_outputs)
  
  return(list_outputs)
}


# Create the new object with the combined path
path_se01ntransect100 <- paste0(file.path(path_data, "mammals_g5/sites100_se10"), "/")

# Print the path
print(path_se01ntransect100)

# Generate output scenarios with integrated path_group_se_01
output_scenarios <- c(
  "ungulates_SE_p1_0.1_nsites_100_SD_N_0_SD_p1_0_output_analysis_"
)

# Simulations, in this case, we have 20
simulations <- 1:20

# Load data for all scenarios and simulations
outputs <- list()
for (scenario in output_scenarios) {
  data <- load_outputs(path_se01ntransect100, scenario, simulations)
  outputs[[scenario]] <- data
}

# Verify loaded data
print(outputs[[output_scenarios[1]]][[1]])


# Create an object to store the Probability_values of p1 from the output analysis for each level
p1_values <- list()

# Iterate over scenarioes and simulations to extract p1
for (scenario in output_scenarios) {
  p1_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$p1
  })
}


# Create an object to store the Probability_values of p3 walking from the output analysis for each level
p3_walking_values <- list()

# Iterate over scenarioes and simulations to extract totalN
for (scenario in output_scenarios) {
  p3_walking_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$p3[,1]
  })
}

# Create an object to store the Probability_values of p3 cycling from the output analysis for each level
p3_cycling_values <- list()

# Iterate over scenarioes and simulations to extract totalN
for (scenario in output_scenarios) {
  p3_cycling_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$p3[,2]
  })
}

# Create an object to store the Probability_values of p3 driving from the output analysis for each level
p3_driving_values <- list()

# Iterate over scenarioes and simulations to extract totalN
for (scenario in output_scenarios) {
  p3_driving_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$p3[,3]
  })
}

# Create a unified data frame for all plots, adding the 'Simulation' and 'scenario' columns
combined_data <- do.call(rbind, lapply(names(p1_values), function(scenario) {
  rbind(
    data.frame(Variable = "p1", 
               Probability_value = unlist(p1_values[[scenario]]), 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario),
    
    data.frame(Variable = "p3 walking", 
               Probability_value = unlist(p3_walking_values[[scenario]]), 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario),
    
    data.frame(Variable = "p3 cycling", 
               Probability_value = unlist(p3_cycling_values[[scenario]]), 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario),
    
    data.frame(Variable = "p3 driving", 
               Probability_value = unlist(p3_driving_values[[scenario]]), 
               Simulation = rep(simulations, each = target_length / length(simulations)),
               scenario = scenario)
  )
}))

# Display the combined dataframe
print(combined_data)

# Group by 'Variable', 'Simulation', and 'scenario' and calculate the mean probability values
df_mean_grouped <- combined_data %>%
  group_by(Variable, Simulation, scenario) %>%
  summarise(mean_value = mean(Probability_value, na.rm = TRUE)) %>%
  ungroup()

# Display the new dataframe with calculated means
print(df_mean_grouped)

# Group by 'Variable', 'Simulation', and 'scenario' and calculate the variance of probability values
df_var_grouped <- combined_data %>%
  group_by(Variable, Simulation, scenario) %>%
  summarise(var_value = var(Probability_value, na.rm = TRUE)) %>%
  ungroup()

# Display the new dataframe with calculated variances
print(df_var_grouped)




# Merge the dataframes by 'Variable', 'Simulation', and 'scenario'
df_combined <- df_mean_grouped %>%
  left_join(df_var_grouped, by = c("Variable", "Simulation", "scenario"))

# Display the new combined dataframe
print(df_combined)

# Observed values
real_values <- data.frame(
  Variable = c("p1", "p3 walking", "p3 cycling", "p3 driving"),
  real_value = c(0.5, 1, 0.9, 0.8)
)

# Display the real values dataframe
print(real_values)

# Merge df_combined with real_values to add the real_value column
df_combined <- df_combined %>%
  left_join(real_values, by = "Variable")

# Display the combined dataframe with the new column
print(df_combined)


# Calculate RRMSE
calculate_rrmse <- function(estimated, real, variance) {
  return(sqrt((estimated - real)^2 + variance) / real)
}

# Initialize a list to store the results
rrmse_list_se01_nsites100 <- list()

# Group by 'Variable', 'Simulation', and 'scenario' and calculate the RRMSE
for (variable in unique(df_combined$Variable)) {
  # Filter data by variable
  df_variable <- df_combined %>% filter(Variable == variable)
  
  rrmse_list_se01_nsites100[[variable]] <- list()
  
  for (scenario_name in unique(df_variable$scenario)) {
    df_scenario <- df_variable %>% filter(scenario == scenario_name)
    
    
    for (sim in unique(df_scenario$Simulation)) {
      # Filter by simulation
      df_simulation <- df_scenario %>% filter(Simulation == sim)
      
      # Extract estimated, real, and variance values
      estimated <- df_simulation$mean_value
      real <- df_simulation$real_value[1]  # As real_value is the same for all rows in this simulation and scenario
      variance <- df_simulation$var_value
      
      # Calculate RRMSE
      if (!is.null(estimated) && length(estimated) > 0 && !is.null(real) && length(real) > 0 && !is.null(variance) && length(variance) > 0) {
        rrmse_list_se01_nsites100[[variable]][[scenario]][[sim]] <- calculate_rrmse(estimated, real, variance)
      } else {
        rrmse_list_se01_nsites100[[variable]][[scenario]][[sim]] <- NA  # Or another value to indicate missing data
        message(paste("Missing or invalid data for variable:", variable, "scenario:", scenario, "simulation:", sim))
      }
    }
  }
}

# Check the results for a specific variable, e.g., "p1"
print(rrmse_list_se01_nsites100[["p1"]])



# Create the RRMSE DataFrame from rrmse_list_se01_nsites100
rrmse_df_01_100 <- data.frame()

for (variable in names(rrmse_list_se01_nsites100)) {
  for (scenario in names(rrmse_list_se01_nsites100[[variable]])) {
    for (sim in 1:length(rrmse_list_se01_nsites100[[variable]][[scenario]])) {
      rrmse_value <- rrmse_list_se01_nsites100[[variable]][[scenario]][[sim]]
      
      # Add row to DataFrame
      rrmse_df_01_100 <- rbind(rrmse_df_01_100, data.frame(
        Variable = variable,
        RRMSE = rrmse_value,
        scenario = scenario,
        Simulation = sim
      ))
    }
  }
}

# Verify that all scenarioes have been correctly mapped
print(unique(rrmse_df_01_100$scenario))
print(rrmse_df_01_100)  # Display the complete DataFrame


# Define scenarios dynamically
scenarios <- c(
  "ungulates_SE_p1_0.1_nsites_100_SD_N_0_SD_p1_0_output_analysis_"
)

# Define user-friendly labels
labels <- c(
  "SD N 0 p2 0")

# Create mapping using setNames()
scenario_map <- setNames(labels, scenarios)

# Map scenarios to user-friendly labels
rrmse_df_01_100$scenario <- factor(rrmse_df_01_100$scenario, levels = names(scenario_map), labels = scenario_map)

# Verify that all scenarios have been correctly mapped
print(unique(rrmse_df_01_100$scenario))

# Calculate generalized RRMSE using the product
rrmse_df_01_100 <- rrmse_df_01_100 %>%
  group_by(scenario, Simulation) %>%
  summarise(Generalized_RRMSE = prod(RRMSE, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Generalized_RRMSE = Generalized_RRMSE ^ (1 / length(unique(rrmse_df_01_100$Variable))))

# Display the dataframe with the new column
print(rrmse_df_01_100)

# Añadir la columna 'SE nsites' a cada dataframe
rrmse_df_005_10$SE_nsites <- "SE 0.05 nsites 10"
rrmse_df_005_100$SE_nsites <- "SE 0.05 nsites 100"
rrmse_df_01_10$SE_nsites <- "SE 0.1 nsites 10"
rrmse_df_01_100$SE_nsites <- "SE 0.1 nsites 100"

# Combinar todos los dataframes en uno solo
combined_df <- rbind(rrmse_df_005_10, rrmse_df_005_100, rrmse_df_01_10, rrmse_df_01_100)

# Verificar la estructura del dataframe combinado
head(combined_df)

# Crear una copia del dataframe combined_df y añadir la columna 'Group'
RRMSE_Mammals_G5 <- combined_df
RRMSE_Mammals_G5$Group <- "Mammals G5"

# Verificar la estructura del nuevo dataframe
head(RRMSE_Mammals_G5)

final_RRMSE <- rbind(final_RRMSE, RRMSE_Mammals_G5)


# Graph Distribution of RRMSE by vertebrate group and simulation scenario
# Convert RRMSE values to a logarithmic scale for plotting
final_RRMSE$log_RRMSE <- log(final_RRMSE$Generalized_RRMSE)
# Define the vertebrate groups order in the plot
final_RRMSE$Group <- factor(final_RRMSE$Group, 
                            levels = c("Amphibians", "Amphibians abundance peak",
                                       "Reptiles G1", "Reptiles G1 abundance peak",
                                       "Reptiles G2","Birds/Bats G1", "Birds G2", 
                                       "Mammals G1", "Mammals G2","Mammals G3", 
                                       "Mammals G4", "Mammals G5"))

# Define a custom color palette for the plot
custom_palette <- c(
  "#440154FF", 
  "#472D7BFF",
  "#3B528BFF", 
  "#2C728EFF", 
  "#21908CFF", 
  "#27AD81FF",
  "#5DC863FF", 
  "#AADC32FF", 
  "#FDE725FF"
)
# Create the plot
p <- ggplot(final_RRMSE, aes(x = log_RRMSE, y = scenario, color = scenario)) +
  stat_pointinterval(position = position_dodge(width = 0.5), .width = c(0.66, 0.95)) +
  facet_grid(SE_nsites ~ Group) +
  labs(x = "logRRMSE", y = NULL) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        axis.text.y = element_blank()) +
  scale_color_manual(values = custom_palette)+
  coord_cartesian(xlim = c(NA, 5))

# Display the plot
print(p)


