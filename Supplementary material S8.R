### Paper title: A novel method to estimate actual infrastructure-induced mortality by integrating sampling biases

### Supplementary material S8
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
library(jagsUI)

setwd("C:/Users/USUARIO/Documents/ESTUDIOS/Master/Conservación Pablo Olavide/TFM/Redaccion paper/redacciones finales/Supplementary matherial/Supplemetnal que no se donde va/Appendix simulations") # set path to location where all files necessary to run the script are
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
    ppd_group <- 0.392598565
    RRMSE <- "RRMSE_Amphibians_abundace_peak"
    Group <- "Amphibians abundance peak"
  } else if (group_name == "reptiles_g1") {
    se005ntransect10 <- "lizards/sites10_se05"
    se005ntransect100 <- "lizards/sites100_se05"
    se01ntransect10 <- "lizards/sites10_se10"
    se01ntransect100 <- "lizards/sites100_se10"
    path_group_se_005<-"lizards_SE_p2_0.05"
    path_group_se_01<-"lizards_SE_p2_0.1"
    ppd_group <- 0.05655527
    RRMSE <- "RRMSE_Reptiles_G1"
    Group <- "Reptiles G1"
  } else if (group_name == "reptiles_g1_abundance_peak") {
    se005ntransect10 <- "lizards_abundance_peak/sites10_se05"
    se005ntransect100 <- "lizards_abundance_peak/sites100_se05"
    se01ntransect10 <- "lizards_abundance_peak/sites10_se10"
    se01ntransect100 <- "lizards_abundance_peak/sites100_se10"
    path_group_se_005<-"lizards_SE_p2_0.05"
    path_group_se_01<-"lizards_SE_p2_0.1"
    ppd_group <- 0.05655527
    RRMSE <- "RRMSE_Reptiles_G1_abundance_peak"
    Group <- "Reptiles G1 abundance peak"
  } else if (group_name == "reptiles_g2") {
    se005ntransect10 <- "snakes/sites10_se05"
    se005ntransect100 <- "snakes/sites100_se05"
    se01ntransect10 <- "snakes/sites10_se10"
    se01ntransect100 <- "snakes/sites100_se10"
    path_group_se_005<-"Snakes_SE_p1_0.05_SE_p2_0.05"
    path_group_se_01<-"Snakes_SE_p1_0.1_SE_p2_0.1"
    ppd_group <- 0.398477157
    RRMSE <- "RRMSE_Reptiles_G2"
    Group <- "Reptiles G2"
  } else if (group_name == "birds_bats_g1") {
    se005ntransect10 <- "birds_bats/sites10_se05"
    se005ntransect100 <- "birds_bats/sites100_se05"
    se01ntransect10 <- "birds_bats/sites10_se10"
    se01ntransect100 <- "birds_bats/sites100_se10"
    path_group_se_005<-"small_birds_and_bats_SE_p1_0.05_SE_p2_0.05"
    path_group_se_01<-"small_birds_and_bats_SE_p1_0.1_SE_p2_0.1"
    ppd_group <- 0.358632169
    RRMSE <- "RRMSE_Birds_Bats_G1"
    Group <- "Birds/Bats G1"
  } else if (group_name == "birds_g2") {
    se005ntransect10 <- "large_birds/sites10_se05"
    se005ntransect100 <- "large_birds/sites100_se05"
    se01ntransect10 <- "large_birds/sites10_se10"
    se01ntransect100 <- "large_birds/sites100_se10"
    path_group_se_005<-"medium_and_large_birds_SE_p1_0.05_SE_p2_0.05"
    path_group_se_01<-"medium_and_large_birds_SE_p1_0.1_SE_p2_0.1"
    ppd_group <- 0.747433082
    RRMSE <- "RRMSE_Birds_G2"
    Group <- "Birds G2"
  } else if (group_name == "mammals_g1") {
    se005ntransect10 <- "small_mammals/sites10_se05"
    se005ntransect100 <- "small_mammals/sites100_se05"
    se01ntransect10 <- "small_mammals/sites10_se10"
    se01ntransect100 <- "small_mammals/sites100_se10"
    path_group_se_005<-"small_mammals_SE_p2_0.05"
    path_group_se_01<-"small_mammals_SE_p2_0.1"
    ppd_group <- 0.388601036
    RRMSE <- "RRMSE_Mammals_G1"
    Group <- "Mammals G1"
  } else if (group_name == "mammals_g2") {
    se005ntransect10 <- "lagomorph/sites10_se05"
    se005ntransect100 <- "lagomorph/sites100_se05"
    se01ntransect10 <- "lagomorph/sites10_se10"
    se01ntransect100 <- "lagomorph/sites100_se10"
    path_group_se_005<-"Lagomorphs_SE_p2_0.05"
    path_group_se_01<-"Lagomorphs_SE_p2_0.1"
    ppd_group <- 0.506361323
    RRMSE <- "RRMSE_Mammals_G2"
    Group <- "Mammals G2"
  } else if (group_name == "mammals_g3") {
    se005ntransect10 <- "hedhehog/sites10_se05"
    se005ntransect100 <- "hedhehog/sites100_se05"
    se01ntransect10 <- "hedhehog/sites10_se10"
    se01ntransect100 <- "hedhehog/sites100_se10"
    path_group_se_005<-"hedgehogs_SE_p2_0.05"
    path_group_se_01<-"hedgehogs_SE_p2_0.1"
    ppd_group <- 0.78250591
    RRMSE <- "RRMSE_Mammals_G3"
    Group <- "Mammals G3"
  } else if (group_name == "mammals_g4") {
    se005ntransect10 <- "carnivores/sites10_se05"
    se005ntransect100 <- "carnivores/sites100_se05"
    se01ntransect10 <- "carnivores/sites10_se10"
    se01ntransect100 <- "carnivores/sites100_se10"
    path_group_se_005<-"medium_and_large_carnivores_SE_p1_0.05_SE_p2_0.05"
    path_group_se_01<-"medium_and_large_carnivores_SE_p1_0.1_SE_p2_0.1"
    ppd_group <- 0.805626598
    RRMSE <- "RRMSE_Mammals_G4"
    Group <- "Mammals G4"
  } else {
    stop("Invalid group. Please choose between 'amphibians', 'reptiles_g1','reptiles_g2', 'birds_bats_g1', 'birds_g2' 'mammals_g1', 'mammals_g2' 'mammals_g3' or 'mammals_g4'.")
  }
  
  return(list(se005ntransect10= se005ntransect10, se005ntransect100= se005ntransect100,se01ntransect10=se01ntransect10,
              se01ntransect100=se01ntransect100, path_group_se_005 = path_group_se_005, path_group_se_01 = path_group_se_01,
              ppd_group = ppd_group, RRMSE = RRMSE, Group = Group))
}

#Select vertebrate group along with their charateristics
group <- "amphibians_abundance_peak"  # write the group of interest
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

# Define scenarios for N
N_scenarios <- c(
  paste0(selected_group$path_group_se_005, "_nsites_10_SD_N_0.5_SD_p2_0.05_N_t"),
  paste0(selected_group$path_group_se_005, "_nsites_10_SD_N_0.5_SD_p2_0.15_N_t"),
  paste0(selected_group$path_group_se_005, "_nsites_10_SD_N_0.5_SD_p2_0_N_t"),
  paste0(selected_group$path_group_se_005, "_nsites_10_SD_N_1.5_SD_p2_0.05_N_t"),
  paste0(selected_group$path_group_se_005, "_nsites_10_SD_N_1.5_SD_p2_0.15_N_t"),
  paste0(selected_group$path_group_se_005, "_nsites_10_SD_N_1.5_SD_p2_0_N_t"),
  paste0(selected_group$path_group_se_005, "_nsites_10_SD_N_0_SD_p2_0.05_N_t"),
  paste0(selected_group$path_group_se_005, "_nsites_10_SD_N_0_SD_p2_0.15_N_t"),
  paste0(selected_group$path_group_se_005, "_nsites_10_SD_N_0_SD_p2_0_N_t")
)

# To ensure we correctly load and manage the N matrices, we assign these names
object_names <- c(
  "SD_N_0.5_p2_0.05_output_analysis",
  "SD_N_0.5_p2_0.15_output_analysis",
  "SD_N_0.5_p2_0_output_analysis",
  "SD_N_1.5_p2_0.05_output_analysis",
  "SD_N_1.5_p2_0.15_output_analysis",
  "SD_N_1.5_p2_0_output_analysis",
  "SD_N_0_p2_0.05_output_analysis",
  "SD_N_0_p2_0.15_output_analysis",
  "SD_N_0_p2_0_output_analysis"
)

# Load N files
load_files <- function(scenario, object_name) {
  data_list <- list()
  for (i in 1:D) {  # Levels after N_t (N_t1, N_t2 ...  N_tD) 
    for (j in 1:20) {  # 20 simulations per level
      compressed_file <- paste0(path_se005ntransect10, scenario, i, "_sim_", j, "_1.RData")
      temp_file <- tempfile(fileext = ".RData")
      
      if (file.exists(compressed_file)) {
        tryCatch({
          # Decompress if necessary
          if (grepl("\\.gz$", compressed_file)) {
            gunzip(compressed_file, destname = temp_file, remove = FALSE)
            data <- readRDS(temp_file)
            data_list[[paste0("N_t", i, "_sim_", j)]] <- data
            unlink(temp_file) # Delete temp file
          } else {
            data <- readRDS(compressed_file)
            data_list[[paste0("N_t", i, "_sim_", j)]] <- data
          }
        }, error = function(e) {
          message(paste("Could not load file:", compressed_file, "Error:", e))
        })
      } else {
        message(paste("File does not exist:", compressed_file))
      }
    }
  }
  assign(object_name, data_list, envir = .GlobalEnv)
}

# Load all files into corresponding objects
for (i in seq_along(N_scenarios)) {
  load_files(N_scenarios[i], object_names[i])
}

# Sum N_t1, N_t2, etc., to calculate the total roadkill
sum_matrices <- function(object) {
  result_list <- list()
  for (j in 1:20) {  # 20 simulations per level
    sum_matrix <- NULL
    for (i in 1:3) {  # 3 levels after N_t
      element_name <- paste0("N_t", i, "_sim_", j)
      if (!is.null(object[[element_name]])) {
        if (is.null(sum_matrix)) {
          sum_matrix <- object[[element_name]]
        } else {
          sum_matrix <- sum_matrix + object[[element_name]]
        }
      }
    }
    result_list[[paste0("sim_", j)]] <- sum_matrix
  }
  return(result_list)
}

# Sum matrices for each object and store the result
for (object_name in object_names) {
  object <- get(object_name)
  sum_result <- sum_matrices(object)
  assign(paste0(object_name, "_summed"), sum_result, envir = .GlobalEnv)
}

# List of summed object names
summed_object_names <- c(
  "SD_N_0.5_p2_0.05_output_analysis_summed",
  "SD_N_0.5_p2_0.15_output_analysis_summed",
  "SD_N_0.5_p2_0_output_analysis_summed",
  "SD_N_1.5_p2_0.05_output_analysis_summed",
  "SD_N_1.5_p2_0.15_output_analysis_summed",
  "SD_N_1.5_p2_0_output_analysis_summed",
  "SD_N_0_p2_0.05_output_analysis_summed",
  "SD_N_0_p2_0.15_output_analysis_summed",
  "SD_N_0_p2_0_output_analysis_summed"
)


# Function to sum columns of each matrix in an object for comparison with analysis results
sum_columns <- function(object) {
  result_list <- list()
  for (element_name in names(object)) {
    matrix <- object[[element_name]]
    if (!is.null(matrix)) {
      col_sum <- colSums(matrix)
      result_list[[element_name]] <- col_sum
    }
  }
  return(result_list)
}

# Sum columns for each summed object and store the result
for (summed_object_name in summed_object_names) {
  object <- get(summed_object_name)
  col_sum_result <- sum_columns(object)
  assign(gsub("_summed$", "_col_sum", summed_object_name), col_sum_result, envir = .GlobalEnv)
}

# List of names of col_sum objects
col_sum_object_names <- c(
  "SD_N_0.5_p2_0.05_output_analysis_col_sum",
  "SD_N_0.5_p2_0.15_output_analysis_col_sum",
  "SD_N_0.5_p2_0_output_analysis_col_sum",
  "SD_N_1.5_p2_0.05_output_analysis_col_sum",
  "SD_N_1.5_p2_0.15_output_analysis_col_sum",
  "SD_N_1.5_p2_0_output_analysis_col_sum",
  "SD_N_0_p2_0.05_output_analysis_col_sum",
  "SD_N_0_p2_0.15_output_analysis_col_sum",
  "SD_N_0_p2_0_output_analysis_col_sum"
)

# Initialize the results object
results <- list()

# Populate the results object with the desired structure
for (i in seq_along(N_scenarios)) {
  col_sum_object_name <- col_sum_object_names[i]
  object <- get(col_sum_object_name)
  col_sum_by_level <- list()
  for (j in 1:20) {  # 20 simulations
    sim_name <- paste0("sim_", j)
    col_sum_by_level[[j]] <- object[[sim_name]]
  }
  results[[N_scenarios[i]]] <- list(col_sum_by_level = col_sum_by_level)
}

# Example of how to access the data
print(results[[N_scenarios[1]]]$col_sum_by_level[[1]])

# Create an object to store totalN values from output analysis for each level
totalN_values <- list()

# Iterate over scenarios and simulations to extract totalN
for (scenario in output_scenarios) {
  totalN_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$totalN
  })
}

# Create a list to store the means of each column at each level
means_totalNs_sim_list <- list()

# Calculate the means of each column at each level of totalN_values
for (scenario in output_scenarios) {
  means_totalNs_sim_list[[scenario]] <- lapply(totalN_values[[scenario]], function(totalN_matrix) {
    apply(totalN_matrix, 2, mean)
  })
}

# Verify the results
print(means_totalNs_sim_list[[output_scenarios[1]]][[1]])


##########################################################################################
############################ Relative Root Mean Squared Error ############################
##########################################################################################

# Create a list to store the variances of each column at each level
vars_totalNs_sim_list <- list()

# Calculate the variances of each column at each level of totalN_values
for (scenario in output_scenarios) {
  vars_totalNs_sim_list[[scenario]] <- lapply(totalN_values[[scenario]], function(totalN_matrix) {
    apply(totalN_matrix, 2, var)
  })
}

# Add +1 to simulation results to avoid division by zero
for (scenario in N_scenarios) {
  for (level in seq_along(results[[scenario]]$col_sum_by_level)) {
    results[[scenario]]$col_sum_by_level[[level]] <- results[[scenario]]$col_sum_by_level[[level]] + 1
  }
}

# Verify the results before calculating RRMSE
print(means_totalNs_sim_list[[output_scenarios[1]]][[1]])
print(results[[N_scenarios[1]]]$col_sum_by_level[[1]])
print(vars_totalNs_sim_list[[output_scenarios[1]]][[1]])

# Calculate RRMSE
calculate_rrmse <- function(estimated, actual, variance) {
  return(sqrt((estimated - actual)^2 + variance) / actual)
}

# Construct the dynamic object name
object_name <- paste0(selected_group$path_group_se_005, "_rrmse_list_se_005_nsites_10")

# Create the list dynamically
assign(object_name, list())

for (scenario in output_scenarios) {
  # Get the current object
  temp_list <- get(object_name, envir = .GlobalEnv)
  
  # Initialize scenario entry
  temp_list[[scenario]] <- list()
  
  for (j in 1:20) {  # 20 simulations
    if (!is.null(results[[N_scenarios[which(output_scenarios == scenario)]]]) &&
        length(results[[N_scenarios[which(output_scenarios == scenario)]]]$col_sum_by_level) >= j &&
        !is.null(means_totalNs_sim_list[[scenario]]) &&
        length(means_totalNs_sim_list[[scenario]]) >= j &&
        !is.null(vars_totalNs_sim_list[[scenario]]) &&
        length(vars_totalNs_sim_list[[scenario]]) >= j) {
      
      actual <- results[[N_scenarios[which(output_scenarios == scenario)]]]$col_sum_by_level[[j]]
      estimated <- means_totalNs_sim_list[[scenario]][[j]]
      variance <- vars_totalNs_sim_list[[scenario]][[j]]
      
      if (!is.null(estimated) && !is.null(actual) && !is.null(variance) && length(actual) > 0) {
        temp_list[[scenario]][[j]] <- calculate_rrmse(estimated, actual, variance)
      } else {
        temp_list[[scenario]][[j]] <- NA
        message(paste("Missing or invalid data for scenario:", scenario, "simulation:", j))
      }
    } else {
      temp_list[[scenario]][[j]] <- NA
      message(paste("Data missing for scenario:", scenario, "simulation:", j))
    }
  }
  
  # Update the object in the global environment
  assign(object_name, temp_list, envir = .GlobalEnv)
}

eval(parse(text = object_name))

#Initialize an empty dataframe
rrmse_df_005_10 <- data.frame(RRMSE = numeric(), scenario = character(), Simulation = integer(), stringsAsFactors = FALSE)

for (scenario in output_scenarios) {
  for (j in 1:length(means_totalNs_sim_list[[scenario]])) {  # Iterate over the 20 simulations
    actual <- results[[N_scenarios[which(output_scenarios == scenario)]]]$col_sum_by_level[[j]]
    estimated <- means_totalNs_sim_list[[scenario]][[j]]
    variance <- vars_totalNs_sim_list[[scenario]][[j]]
    
    if (!is.null(estimated) && !is.null(actual) && !is.null(variance) && length(actual) > 0) {
      rrmse_value <- calculate_rrmse(estimated, actual, variance)
    } else {
      rrmse_value <- NA  # Assign NA if data is missing
      message(paste("Missing or invalid data for scenario:", scenario, "simulation:", j))
    }
    
    # Add row to the dataframe
    rrmse_df_005_10 <- rbind(rrmse_df_005_10, data.frame(
      RRMSE = rrmse_value,
      scenario = scenario,
      Simulation = j
    ))
  }
}


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

# Define scenarios for N
N_scenarios <- c(
  paste0(selected_group$path_group_se_005, "_nsites_100_SD_N_0.5_SD_p2_0.05_N_t"),
  paste0(selected_group$path_group_se_005, "_nsites_100_SD_N_0.5_SD_p2_0.15_N_t"),
  paste0(selected_group$path_group_se_005, "_nsites_100_SD_N_0.5_SD_p2_0_N_t"),
  paste0(selected_group$path_group_se_005, "_nsites_100_SD_N_1.5_SD_p2_0.05_N_t"),
  paste0(selected_group$path_group_se_005, "_nsites_100_SD_N_1.5_SD_p2_0.15_N_t"),
  paste0(selected_group$path_group_se_005, "_nsites_100_SD_N_1.5_SD_p2_0_N_t"),
  paste0(selected_group$path_group_se_005, "_nsites_100_SD_N_0_SD_p2_0.05_N_t"),
  paste0(selected_group$path_group_se_005, "_nsites_100_SD_N_0_SD_p2_0.15_N_t"),
  paste0(selected_group$path_group_se_005, "_nsites_100_SD_N_0_SD_p2_0_N_t")
)

# To ensure we correctly load and manage the N matrices, we assign these names
object_names <- c(
  "SD_N_0.5_p2_0.05_output_analysis",
  "SD_N_0.5_p2_0.15_output_analysis",
  "SD_N_0.5_p2_0_output_analysis",
  "SD_N_1.5_p2_0.05_output_analysis",
  "SD_N_1.5_p2_0.15_output_analysis",
  "SD_N_1.5_p2_0_output_analysis",
  "SD_N_0_p2_0.05_output_analysis",
  "SD_N_0_p2_0.15_output_analysis",
  "SD_N_0_p2_0_output_analysis"
)

# Load N files
load_files <- function(scenario, object_name) {
  data_list <- list()
  for (i in 1:D) {  # Levels after N_t (N_t1, N_t2 ...  N_tD) 
    for (j in 1:20) {  # 20 simulations per level
      compressed_file <- paste0(path_se005ntransect100, scenario, i, "_sim_", j, "_1.RData")
      temp_file <- tempfile(fileext = ".RData")
      
      if (file.exists(compressed_file)) {
        tryCatch({
          # Decompress if necessary
          if (grepl("\\.gz$", compressed_file)) {
            gunzip(compressed_file, destname = temp_file, remove = FALSE)
            data <- readRDS(temp_file)
            data_list[[paste0("N_t", i, "_sim_", j)]] <- data
            unlink(temp_file) # Delete temp file
          } else {
            data <- readRDS(compressed_file)
            data_list[[paste0("N_t", i, "_sim_", j)]] <- data
          }
        }, error = function(e) {
          message(paste("Could not load file:", compressed_file, "Error:", e))
        })
      } else {
        message(paste("File does not exist:", compressed_file))
      }
    }
  }
  assign(object_name, data_list, envir = .GlobalEnv)
}

# Load all files into corresponding objects
for (i in seq_along(N_scenarios)) {
  load_files(N_scenarios[i], object_names[i])
}

# Sum N_t1, N_t2, etc., to calculate the total roadkill
sum_matrices <- function(object) {
  result_list <- list()
  for (j in 1:20) {  # 20 simulations per level
    sum_matrix <- NULL
    for (i in 1:3) {  # 3 levels after N_t
      element_name <- paste0("N_t", i, "_sim_", j)
      if (!is.null(object[[element_name]])) {
        if (is.null(sum_matrix)) {
          sum_matrix <- object[[element_name]]
        } else {
          sum_matrix <- sum_matrix + object[[element_name]]
        }
      }
    }
    result_list[[paste0("sim_", j)]] <- sum_matrix
  }
  return(result_list)
}

# Sum matrices for each object and store the result
for (object_name in object_names) {
  object <- get(object_name)
  sum_result <- sum_matrices(object)
  assign(paste0(object_name, "_summed"), sum_result, envir = .GlobalEnv)
}

# List of summed object names
summed_object_names <- c(
  "SD_N_0.5_p2_0.05_output_analysis_summed",
  "SD_N_0.5_p2_0.15_output_analysis_summed",
  "SD_N_0.5_p2_0_output_analysis_summed",
  "SD_N_1.5_p2_0.05_output_analysis_summed",
  "SD_N_1.5_p2_0.15_output_analysis_summed",
  "SD_N_1.5_p2_0_output_analysis_summed",
  "SD_N_0_p2_0.05_output_analysis_summed",
  "SD_N_0_p2_0.15_output_analysis_summed",
  "SD_N_0_p2_0_output_analysis_summed"
)


# Function to sum columns of each matrix in an object for comparison with analysis results
sum_columns <- function(object) {
  result_list <- list()
  for (element_name in names(object)) {
    matrix <- object[[element_name]]
    if (!is.null(matrix)) {
      col_sum <- colSums(matrix)
      result_list[[element_name]] <- col_sum
    }
  }
  return(result_list)
}

# Sum columns for each summed object and store the result
for (summed_object_name in summed_object_names) {
  object <- get(summed_object_name)
  col_sum_result <- sum_columns(object)
  assign(gsub("_summed$", "_col_sum", summed_object_name), col_sum_result, envir = .GlobalEnv)
}

# List of names of col_sum objects
col_sum_object_names <- c(
  "SD_N_0.5_p2_0.05_output_analysis_col_sum",
  "SD_N_0.5_p2_0.15_output_analysis_col_sum",
  "SD_N_0.5_p2_0_output_analysis_col_sum",
  "SD_N_1.5_p2_0.05_output_analysis_col_sum",
  "SD_N_1.5_p2_0.15_output_analysis_col_sum",
  "SD_N_1.5_p2_0_output_analysis_col_sum",
  "SD_N_0_p2_0.05_output_analysis_col_sum",
  "SD_N_0_p2_0.15_output_analysis_col_sum",
  "SD_N_0_p2_0_output_analysis_col_sum"
)

# Initialize the results object
results <- list()

# Populate the results object with the desired structure
for (i in seq_along(N_scenarios)) {
  col_sum_object_name <- col_sum_object_names[i]
  object <- get(col_sum_object_name)
  col_sum_by_level <- list()
  for (j in 1:20) {  # 20 simulations
    sim_name <- paste0("sim_", j)
    col_sum_by_level[[j]] <- object[[sim_name]]
  }
  results[[N_scenarios[i]]] <- list(col_sum_by_level = col_sum_by_level)
}

# Example of how to access the data
print(results[[N_scenarios[1]]]$col_sum_by_level[[1]])

# Create an object to store totalN values from output analysis for each level
totalN_values <- list()

# Iterate over scenarios and simulations to extract totalN
for (scenario in output_scenarios) {
  totalN_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$totalN
  })
}

# Create a list to store the means of each column at each level
means_totalNs_sim_list <- list()

# Calculate the means of each column at each level of totalN_values
for (scenario in output_scenarios) {
  means_totalNs_sim_list[[scenario]] <- lapply(totalN_values[[scenario]], function(totalN_matrix) {
    apply(totalN_matrix, 2, mean)
  })
}

# Verify the results
print(means_totalNs_sim_list[[output_scenarios[1]]][[1]])


##########################################################################################
############################ Relative Root Mean Squared Error ############################
##########################################################################################

# Create a list to store the variances of each column at each level
vars_totalNs_sim_list <- list()

# Calculate the variances of each column at each level of totalN_values
for (scenario in output_scenarios) {
  vars_totalNs_sim_list[[scenario]] <- lapply(totalN_values[[scenario]], function(totalN_matrix) {
    apply(totalN_matrix, 2, var)
  })
}

# Add +1 to simulation results to avoid division by zero
for (scenario in N_scenarios) {
  for (level in seq_along(results[[scenario]]$col_sum_by_level)) {
    results[[scenario]]$col_sum_by_level[[level]] <- results[[scenario]]$col_sum_by_level[[level]] + 1
  }
}

# Verify the results before calculating RRMSE
print(means_totalNs_sim_list[[output_scenarios[1]]][[1]])
print(results[[N_scenarios[1]]]$col_sum_by_level[[1]])
print(vars_totalNs_sim_list[[output_scenarios[1]]][[1]])

# Calculate RRMSE
calculate_rrmse <- function(estimated, actual, variance) {
  return(sqrt((estimated - actual)^2 + variance) / actual)
}

# Construct the dynamic object name
object_name <- paste0(selected_group$path_group_se_005, "_rrmse_list_se_005_nsites_100")

# Create the list dynamically
assign(object_name, list())

for (scenario in output_scenarios) {
  # Get the current object
  temp_list <- get(object_name, envir = .GlobalEnv)
  
  # Initialize scenario entry
  temp_list[[scenario]] <- list()
  
  for (j in 1:20) {  # 20 simulations
    if (!is.null(results[[N_scenarios[which(output_scenarios == scenario)]]]) &&
        length(results[[N_scenarios[which(output_scenarios == scenario)]]]$col_sum_by_level) >= j &&
        !is.null(means_totalNs_sim_list[[scenario]]) &&
        length(means_totalNs_sim_list[[scenario]]) >= j &&
        !is.null(vars_totalNs_sim_list[[scenario]]) &&
        length(vars_totalNs_sim_list[[scenario]]) >= j) {
      
      actual <- results[[N_scenarios[which(output_scenarios == scenario)]]]$col_sum_by_level[[j]]
      estimated <- means_totalNs_sim_list[[scenario]][[j]]
      variance <- vars_totalNs_sim_list[[scenario]][[j]]
      
      if (!is.null(estimated) && !is.null(actual) && !is.null(variance) && length(actual) > 0) {
        temp_list[[scenario]][[j]] <- calculate_rrmse(estimated, actual, variance)
      } else {
        temp_list[[scenario]][[j]] <- NA
        message(paste("Missing or invalid data for scenario:", scenario, "simulation:", j))
      }
    } else {
      temp_list[[scenario]][[j]] <- NA
      message(paste("Data missing for scenario:", scenario, "simulation:", j))
    }
  }
  
  # Update the object in the global environment
  assign(object_name, temp_list, envir = .GlobalEnv)
}

eval(parse(text = object_name))


#Initialize an empty dataframe
rrmse_df_005_100 <- data.frame(RRMSE = numeric(), scenario = character(), Simulation = integer(), stringsAsFactors = FALSE)

for (scenario in output_scenarios) {
  for (j in 1:length(means_totalNs_sim_list[[scenario]])) {  # Iterate over the 20 simulations
    actual <- results[[N_scenarios[which(output_scenarios == scenario)]]]$col_sum_by_level[[j]]
    estimated <- means_totalNs_sim_list[[scenario]][[j]]
    variance <- vars_totalNs_sim_list[[scenario]][[j]]
    
    if (!is.null(estimated) && !is.null(actual) && !is.null(variance) && length(actual) > 0) {
      rrmse_value <- calculate_rrmse(estimated, actual, variance)
    } else {
      rrmse_value <- NA  # Assign NA if data is missing
      message(paste("Missing or invalid data for scenario:", scenario, "simulation:", j))
    }
    
    # Add row to the dataframe
    rrmse_df_005_100 <- rbind(rrmse_df_005_100, data.frame(
      RRMSE = rrmse_value,
      scenario = scenario,
      Simulation = j
    ))
  }
}


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

# Define scenarios for N
N_scenarios <- c(
  paste0(selected_group$path_group_se_01, "_nsites_10_SD_N_0.5_SD_p2_0.05_N_t"),
  paste0(selected_group$path_group_se_01, "_nsites_10_SD_N_0.5_SD_p2_0.15_N_t"),
  paste0(selected_group$path_group_se_01, "_nsites_10_SD_N_0.5_SD_p2_0_N_t"),
  paste0(selected_group$path_group_se_01, "_nsites_10_SD_N_1.5_SD_p2_0.05_N_t"),
  paste0(selected_group$path_group_se_01, "_nsites_10_SD_N_1.5_SD_p2_0.15_N_t"),
  paste0(selected_group$path_group_se_01, "_nsites_10_SD_N_1.5_SD_p2_0_N_t"),
  paste0(selected_group$path_group_se_01, "_nsites_10_SD_N_0_SD_p2_0.05_N_t"),
  paste0(selected_group$path_group_se_01, "_nsites_10_SD_N_0_SD_p2_0.15_N_t"),
  paste0(selected_group$path_group_se_01, "_nsites_10_SD_N_0_SD_p2_0_N_t")
)

# To ensure we correctly load and manage the N matrices, we assign these names
object_names <- c(
  "SD_N_0.5_p2_0.05_output_analysis",
  "SD_N_0.5_p2_0.15_output_analysis",
  "SD_N_0.5_p2_0_output_analysis",
  "SD_N_1.5_p2_0.05_output_analysis",
  "SD_N_1.5_p2_0.15_output_analysis",
  "SD_N_1.5_p2_0_output_analysis",
  "SD_N_0_p2_0.05_output_analysis",
  "SD_N_0_p2_0.15_output_analysis",
  "SD_N_0_p2_0_output_analysis"
)

# Load N files
load_files <- function(scenario, object_name) {
  data_list <- list()
  for (i in 1:D) {  # Levels after N_t (N_t1, N_t2 ...  N_tD) 
    for (j in 1:20) {  # 20 simulations per level
      compressed_file <- paste0(path_se01ntransect10, scenario, i, "_sim_", j, "_1.RData")
      temp_file <- tempfile(fileext = ".RData")
      
      if (file.exists(compressed_file)) {
        tryCatch({
          # Decompress if necessary
          if (grepl("\\.gz$", compressed_file)) {
            gunzip(compressed_file, destname = temp_file, remove = FALSE)
            data <- readRDS(temp_file)
            data_list[[paste0("N_t", i, "_sim_", j)]] <- data
            unlink(temp_file) # Delete temp file
          } else {
            data <- readRDS(compressed_file)
            data_list[[paste0("N_t", i, "_sim_", j)]] <- data
          }
        }, error = function(e) {
          message(paste("Could not load file:", compressed_file, "Error:", e))
        })
      } else {
        message(paste("File does not exist:", compressed_file))
      }
    }
  }
  assign(object_name, data_list, envir = .GlobalEnv)
}

# Load all files into corresponding objects
for (i in seq_along(N_scenarios)) {
  load_files(N_scenarios[i], object_names[i])
}

# Sum N_t1, N_t2, etc., to calculate the total roadkill
sum_matrices <- function(object) {
  result_list <- list()
  for (j in 1:20) {  # 20 simulations per level
    sum_matrix <- NULL
    for (i in 1:3) {  # 3 levels after N_t
      element_name <- paste0("N_t", i, "_sim_", j)
      if (!is.null(object[[element_name]])) {
        if (is.null(sum_matrix)) {
          sum_matrix <- object[[element_name]]
        } else {
          sum_matrix <- sum_matrix + object[[element_name]]
        }
      }
    }
    result_list[[paste0("sim_", j)]] <- sum_matrix
  }
  return(result_list)
}

# Sum matrices for each object and store the result
for (object_name in object_names) {
  object <- get(object_name)
  sum_result <- sum_matrices(object)
  assign(paste0(object_name, "_summed"), sum_result, envir = .GlobalEnv)
}

# List of summed object names
summed_object_names <- c(
  "SD_N_0.5_p2_0.05_output_analysis_summed",
  "SD_N_0.5_p2_0.15_output_analysis_summed",
  "SD_N_0.5_p2_0_output_analysis_summed",
  "SD_N_1.5_p2_0.05_output_analysis_summed",
  "SD_N_1.5_p2_0.15_output_analysis_summed",
  "SD_N_1.5_p2_0_output_analysis_summed",
  "SD_N_0_p2_0.05_output_analysis_summed",
  "SD_N_0_p2_0.15_output_analysis_summed",
  "SD_N_0_p2_0_output_analysis_summed"
)


# Function to sum columns of each matrix in an object for comparison with analysis results
sum_columns <- function(object) {
  result_list <- list()
  for (element_name in names(object)) {
    matrix <- object[[element_name]]
    if (!is.null(matrix)) {
      col_sum <- colSums(matrix)
      result_list[[element_name]] <- col_sum
    }
  }
  return(result_list)
}

# Sum columns for each summed object and store the result
for (summed_object_name in summed_object_names) {
  object <- get(summed_object_name)
  col_sum_result <- sum_columns(object)
  assign(gsub("_summed$", "_col_sum", summed_object_name), col_sum_result, envir = .GlobalEnv)
}

# List of names of col_sum objects
col_sum_object_names <- c(
  "SD_N_0.5_p2_0.05_output_analysis_col_sum",
  "SD_N_0.5_p2_0.15_output_analysis_col_sum",
  "SD_N_0.5_p2_0_output_analysis_col_sum",
  "SD_N_1.5_p2_0.05_output_analysis_col_sum",
  "SD_N_1.5_p2_0.15_output_analysis_col_sum",
  "SD_N_1.5_p2_0_output_analysis_col_sum",
  "SD_N_0_p2_0.05_output_analysis_col_sum",
  "SD_N_0_p2_0.15_output_analysis_col_sum",
  "SD_N_0_p2_0_output_analysis_col_sum"
)

# Initialize the results object
results <- list()

# Populate the results object with the desired structure
for (i in seq_along(N_scenarios)) {
  col_sum_object_name <- col_sum_object_names[i]
  object <- get(col_sum_object_name)
  col_sum_by_level <- list()
  for (j in 1:20) {  # 20 simulations
    sim_name <- paste0("sim_", j)
    col_sum_by_level[[j]] <- object[[sim_name]]
  }
  results[[N_scenarios[i]]] <- list(col_sum_by_level = col_sum_by_level)
}

# Example of how to access the data
print(results[[N_scenarios[1]]]$col_sum_by_level[[1]])

# Create an object to store totalN values from output analysis for each level
totalN_values <- list()

# Iterate over scenarios and simulations to extract totalN
for (scenario in output_scenarios) {
  totalN_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$totalN
  })
}

# Create a list to store the means of each column at each level
means_totalNs_sim_list <- list()

# Calculate the means of each column at each level of totalN_values
for (scenario in output_scenarios) {
  means_totalNs_sim_list[[scenario]] <- lapply(totalN_values[[scenario]], function(totalN_matrix) {
    apply(totalN_matrix, 2, mean)
  })
}

# Verify the results
print(means_totalNs_sim_list[[output_scenarios[1]]][[1]])


##########################################################################################
############################ Relative Root Mean Squared Error ############################
##########################################################################################

# Create a list to store the variances of each column at each level
vars_totalNs_sim_list <- list()

# Calculate the variances of each column at each level of totalN_values
for (scenario in output_scenarios) {
  vars_totalNs_sim_list[[scenario]] <- lapply(totalN_values[[scenario]], function(totalN_matrix) {
    apply(totalN_matrix, 2, var)
  })
}

# Add +1 to simulation results to avoid division by zero
for (scenario in N_scenarios) {
  for (level in seq_along(results[[scenario]]$col_sum_by_level)) {
    results[[scenario]]$col_sum_by_level[[level]] <- results[[scenario]]$col_sum_by_level[[level]] + 1
  }
}

# Verify the results before calculating RRMSE
print(means_totalNs_sim_list[[output_scenarios[1]]][[1]])
print(results[[N_scenarios[1]]]$col_sum_by_level[[1]])
print(vars_totalNs_sim_list[[output_scenarios[1]]][[1]])

# Calculate RRMSE
calculate_rrmse <- function(estimated, actual, variance) {
  return(sqrt((estimated - actual)^2 + variance) / actual)
}

for (scenario in output_scenarios) {
  # Get the current object
  temp_list <- get(object_name, envir = .GlobalEnv)
  
  # Initialize scenario entry
  temp_list[[scenario]] <- list()
  
  for (j in 1:20) {  # 20 simulations
    if (!is.null(results[[N_scenarios[which(output_scenarios == scenario)]]]) &&
        length(results[[N_scenarios[which(output_scenarios == scenario)]]]$col_sum_by_level) >= j &&
        !is.null(means_totalNs_sim_list[[scenario]]) &&
        length(means_totalNs_sim_list[[scenario]]) >= j &&
        !is.null(vars_totalNs_sim_list[[scenario]]) &&
        length(vars_totalNs_sim_list[[scenario]]) >= j) {
      
      actual <- results[[N_scenarios[which(output_scenarios == scenario)]]]$col_sum_by_level[[j]]
      estimated <- means_totalNs_sim_list[[scenario]][[j]]
      variance <- vars_totalNs_sim_list[[scenario]][[j]]
      
      if (!is.null(estimated) && !is.null(actual) && !is.null(variance) && length(actual) > 0) {
        temp_list[[scenario]][[j]] <- calculate_rrmse(estimated, actual, variance)
      } else {
        temp_list[[scenario]][[j]] <- NA
        message(paste("Missing or invalid data for scenario:", scenario, "simulation:", j))
      }
    } else {
      temp_list[[scenario]][[j]] <- NA
      message(paste("Data missing for scenario:", scenario, "simulation:", j))
    }
  }
  
  # Update the object in the global environment
  assign(object_name, temp_list, envir = .GlobalEnv)
}

eval(parse(text = object_name))

# Initialize an empty dataframe
rrmse_df_01_10 <- data.frame(RRMSE = numeric(), scenario = character(), Simulation = integer(), stringsAsFactors = FALSE)

for (scenario in output_scenarios) {
  for (j in 1:length(means_totalNs_sim_list[[scenario]])) {  # Iterate over the 20 simulations
    actual <- results[[N_scenarios[which(output_scenarios == scenario)]]]$col_sum_by_level[[j]]
    estimated <- means_totalNs_sim_list[[scenario]][[j]]
    variance <- vars_totalNs_sim_list[[scenario]][[j]]
    
    if (!is.null(estimated) && !is.null(actual) && !is.null(variance) && length(actual) > 0) {
      rrmse_value <- calculate_rrmse(estimated, actual, variance)
    } else {
      rrmse_value <- NA  # Assign NA if data is missing
      message(paste("Missing or invalid data for scenario:", scenario, "simulation:", j))
    }
    
    # Add row to the dataframe
    rrmse_df_01_10 <- rbind(rrmse_df_01_10, data.frame(
      RRMSE = rrmse_value,
      scenario = scenario,
      Simulation = j
    ))
  }
}


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


# ------------------------------------------------------------------------------
#3.Standar Error carcass location and persistence bias = 0.10 nº transects = 100
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

# Define scenarios for N
N_scenarios <- c(
  paste0(selected_group$path_group_se_01, "_nsites_100_SD_N_0.5_SD_p2_0.05_N_t"),
  paste0(selected_group$path_group_se_01, "_nsites_100_SD_N_0.5_SD_p2_0.15_N_t"),
  paste0(selected_group$path_group_se_01, "_nsites_100_SD_N_0.5_SD_p2_0_N_t"),
  paste0(selected_group$path_group_se_01, "_nsites_100_SD_N_1.5_SD_p2_0.05_N_t"),
  paste0(selected_group$path_group_se_01, "_nsites_100_SD_N_1.5_SD_p2_0.15_N_t"),
  paste0(selected_group$path_group_se_01, "_nsites_100_SD_N_1.5_SD_p2_0_N_t"),
  paste0(selected_group$path_group_se_01, "_nsites_100_SD_N_0_SD_p2_0.05_N_t"),
  paste0(selected_group$path_group_se_01, "_nsites_100_SD_N_0_SD_p2_0.15_N_t"),
  paste0(selected_group$path_group_se_01, "_nsites_100_SD_N_0_SD_p2_0_N_t")
)

# To ensure we correctly load and manage the N matrices, we assign these names
object_names <- c(
  "SD_N_0.5_p2_0.05_output_analysis",
  "SD_N_0.5_p2_0.15_output_analysis",
  "SD_N_0.5_p2_0_output_analysis",
  "SD_N_1.5_p2_0.05_output_analysis",
  "SD_N_1.5_p2_0.15_output_analysis",
  "SD_N_1.5_p2_0_output_analysis",
  "SD_N_0_p2_0.05_output_analysis",
  "SD_N_0_p2_0.15_output_analysis",
  "SD_N_0_p2_0_output_analysis"
)

# Load N files
load_files <- function(scenario, object_name) {
  data_list <- list()
  for (i in 1:D) {  # Levels after N_t (N_t1, N_t2 ...  N_tD) 
    for (j in 1:20) {  # 20 simulations per level
      compressed_file <- paste0(path_se01ntransect100, scenario, i, "_sim_", j, "_1.RData")
      temp_file <- tempfile(fileext = ".RData")
      
      if (file.exists(compressed_file)) {
        tryCatch({
          # Decompress if necessary
          if (grepl("\\.gz$", compressed_file)) {
            gunzip(compressed_file, destname = temp_file, remove = FALSE)
            data <- readRDS(temp_file)
            data_list[[paste0("N_t", i, "_sim_", j)]] <- data
            unlink(temp_file) # Delete temp file
          } else {
            data <- readRDS(compressed_file)
            data_list[[paste0("N_t", i, "_sim_", j)]] <- data
          }
        }, error = function(e) {
          message(paste("Could not load file:", compressed_file, "Error:", e))
        })
      } else {
        message(paste("File does not exist:", compressed_file))
      }
    }
  }
  assign(object_name, data_list, envir = .GlobalEnv)
}

# Load all files into corresponding objects
for (i in seq_along(N_scenarios)) {
  load_files(N_scenarios[i], object_names[i])
}

# Sum N_t1, N_t2, etc., to calculate the total roadkill
sum_matrices <- function(object) {
  result_list <- list()
  for (j in 1:20) {  # 20 simulations per level
    sum_matrix <- NULL
    for (i in 1:3) {  # 3 levels after N_t
      element_name <- paste0("N_t", i, "_sim_", j)
      if (!is.null(object[[element_name]])) {
        if (is.null(sum_matrix)) {
          sum_matrix <- object[[element_name]]
        } else {
          sum_matrix <- sum_matrix + object[[element_name]]
        }
      }
    }
    result_list[[paste0("sim_", j)]] <- sum_matrix
  }
  return(result_list)
}

# Sum matrices for each object and store the result
for (object_name in object_names) {
  object <- get(object_name)
  sum_result <- sum_matrices(object)
  assign(paste0(object_name, "_summed"), sum_result, envir = .GlobalEnv)
}

# List of summed object names
summed_object_names <- c(
  "SD_N_0.5_p2_0.05_output_analysis_summed",
  "SD_N_0.5_p2_0.15_output_analysis_summed",
  "SD_N_0.5_p2_0_output_analysis_summed",
  "SD_N_1.5_p2_0.05_output_analysis_summed",
  "SD_N_1.5_p2_0.15_output_analysis_summed",
  "SD_N_1.5_p2_0_output_analysis_summed",
  "SD_N_0_p2_0.05_output_analysis_summed",
  "SD_N_0_p2_0.15_output_analysis_summed",
  "SD_N_0_p2_0_output_analysis_summed"
)


# Function to sum columns of each matrix in an object for comparison with analysis results
sum_columns <- function(object) {
  result_list <- list()
  for (element_name in names(object)) {
    matrix <- object[[element_name]]
    if (!is.null(matrix)) {
      col_sum <- colSums(matrix)
      result_list[[element_name]] <- col_sum
    }
  }
  return(result_list)
}

# Sum columns for each summed object and store the result
for (summed_object_name in summed_object_names) {
  object <- get(summed_object_name)
  col_sum_result <- sum_columns(object)
  assign(gsub("_summed$", "_col_sum", summed_object_name), col_sum_result, envir = .GlobalEnv)
}

# List of names of col_sum objects
col_sum_object_names <- c(
  "SD_N_0.5_p2_0.05_output_analysis_col_sum",
  "SD_N_0.5_p2_0.15_output_analysis_col_sum",
  "SD_N_0.5_p2_0_output_analysis_col_sum",
  "SD_N_1.5_p2_0.05_output_analysis_col_sum",
  "SD_N_1.5_p2_0.15_output_analysis_col_sum",
  "SD_N_1.5_p2_0_output_analysis_col_sum",
  "SD_N_0_p2_0.05_output_analysis_col_sum",
  "SD_N_0_p2_0.15_output_analysis_col_sum",
  "SD_N_0_p2_0_output_analysis_col_sum"
)

# Initialize the results object
results <- list()

# Populate the results object with the desired structure
for (i in seq_along(N_scenarios)) {
  col_sum_object_name <- col_sum_object_names[i]
  object <- get(col_sum_object_name)
  col_sum_by_level <- list()
  for (j in 1:20) {  # 20 simulations
    sim_name <- paste0("sim_", j)
    col_sum_by_level[[j]] <- object[[sim_name]]
  }
  results[[N_scenarios[i]]] <- list(col_sum_by_level = col_sum_by_level)
}

# Example of how to access the data
print(results[[N_scenarios[1]]]$col_sum_by_level[[1]])

# Create an object to store totalN values from output analysis for each level
totalN_values <- list()

# Iterate over scenarios and simulations to extract totalN
for (scenario in output_scenarios) {
  totalN_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$totalN
  })
}

# Create a list to store the means of each column at each level
means_totalNs_sim_list <- list()

# Calculate the means of each column at each level of totalN_values
for (scenario in output_scenarios) {
  means_totalNs_sim_list[[scenario]] <- lapply(totalN_values[[scenario]], function(totalN_matrix) {
    apply(totalN_matrix, 2, mean)
  })
}

# Verify the results
print(means_totalNs_sim_list[[output_scenarios[1]]][[1]])


##########################################################################################
############################ Relative Root Mean Squared Error ############################
##########################################################################################

# Create a list to store the variances of each column at each level
vars_totalNs_sim_list <- list()

# Calculate the variances of each column at each level of totalN_values
for (scenario in output_scenarios) {
  vars_totalNs_sim_list[[scenario]] <- lapply(totalN_values[[scenario]], function(totalN_matrix) {
    apply(totalN_matrix, 2, var)
  })
}

# Add +1 to simulation results to avoid division by zero
for (scenario in N_scenarios) {
  for (level in seq_along(results[[scenario]]$col_sum_by_level)) {
    results[[scenario]]$col_sum_by_level[[level]] <- results[[scenario]]$col_sum_by_level[[level]] + 1
  }
}

# Verify the results before calculating RRMSE
print(means_totalNs_sim_list[[output_scenarios[1]]][[1]])
print(results[[N_scenarios[1]]]$col_sum_by_level[[1]])
print(vars_totalNs_sim_list[[output_scenarios[1]]][[1]])

# Calculate the RRMSE
calculate_rrmse <- function(estimated, actual, variance) {
  return(sqrt((estimated - actual)^2 + variance) / actual)
}
# Construct the dynamic object name
object_name <- paste0(selected_group$path_group_se_01, "_rrmse_list_se_01_nsites_10")

# Create the list dynamically
assign(object_name, list())

for (scenario in output_scenarios) {
  # Get the current object
  temp_list <- get(object_name, envir = .GlobalEnv)
  
  # Initialize scenario entry
  temp_list[[scenario]] <- list()
  
  for (j in 1:20) {  # 20 simulations
    if (!is.null(results[[N_scenarios[which(output_scenarios == scenario)]]]) &&
        length(results[[N_scenarios[which(output_scenarios == scenario)]]]$col_sum_by_level) >= j &&
        !is.null(means_totalNs_sim_list[[scenario]]) &&
        length(means_totalNs_sim_list[[scenario]]) >= j &&
        !is.null(vars_totalNs_sim_list[[scenario]]) &&
        length(vars_totalNs_sim_list[[scenario]]) >= j) {
      
      actual <- results[[N_scenarios[which(output_scenarios == scenario)]]]$col_sum_by_level[[j]]
      estimated <- means_totalNs_sim_list[[scenario]][[j]]
      variance <- vars_totalNs_sim_list[[scenario]][[j]]
      
      if (!is.null(estimated) && !is.null(actual) && !is.null(variance) && length(actual) > 0) {
        temp_list[[scenario]][[j]] <- calculate_rrmse(estimated, actual, variance)
      } else {
        temp_list[[scenario]][[j]] <- NA
        message(paste("Missing or invalid data for scenario:", scenario, "simulation:", j))
      }
    } else {
      temp_list[[scenario]][[j]] <- NA
      message(paste("Data missing for scenario:", scenario, "simulation:", j))
    }
  }
  
  # Update the object in the global environment
  assign(object_name, temp_list, envir = .GlobalEnv)
}


eval(parse(text = object_name))

# # Initialize an empty dataframe
rrmse_df_01_100 <- data.frame(RRMSE = numeric(), scenario = character(), Simulation = integer(), stringsAsFactors = FALSE)

for (scenario in output_scenarios) {
  for (j in 1:length(means_totalNs_sim_list[[scenario]])) {  # Iterate over the 20 simulations
    actual <- results[[N_scenarios[which(output_scenarios == scenario)]]]$col_sum_by_level[[j]]
    estimated <- means_totalNs_sim_list[[scenario]][[j]]
    variance <- vars_totalNs_sim_list[[scenario]][[j]]
    
    if (!is.null(estimated) && !is.null(actual) && !is.null(variance) && length(actual) > 0) {
      rrmse_value <- calculate_rrmse(estimated, actual, variance)
    } else {
      rrmse_value <- NA  # Assign NA if data is missing
      message(paste("Missing or invalid data for scenario:", scenario, "simulation:", j))
    }
    
    # Add row to the dataframe
    rrmse_df_01_100 <- rbind(rrmse_df_01_100, data.frame(
      RRMSE = rrmse_value,
      scenario = scenario,
      Simulation = j
    ))
  }
}


# Verify that the scenarios have been correctly mapped
print(unique(rrmse_df_01_100$scenario))


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
path_se005ntransect10 <- paste0(file.path(path_data, "ungulates/sites10_se05"), "/")

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

# Define scenarios for N
N_scenarios <- c(
  "ungulates_SE_p1_0.05_nsites_10_SD_N_0_SD_p1_0_N_t")

# To ensure we correctly load and manage the N matrices, we assign these names
object_names <- c(
  "SD_N_0_p2_0_output_analysis"
)

# Load N files
load_files <- function(scenario, object_name) {
  data_list <- list()
  for (i in 1:1) {  # We assume that ungulates persist throughout the month, so we simulate monthly roadkills at a single Nt level
    for (j in 1:20) {  # 20 simulations per level
      compressed_file <- paste0(path_se005ntransect10, scenario, i, "_sim_", j, "_1.RData")
      temp_file <- tempfile(fileext = ".RData")
      
      if (file.exists(compressed_file)) {
        tryCatch({
          # Decompress if necessary
          if (grepl("\\.gz$", compressed_file)) {
            gunzip(compressed_file, destname = temp_file, remove = FALSE)
            data <- readRDS(temp_file)
            data_list[[paste0("N_t", i, "_sim_", j)]] <- data
            unlink(temp_file) # Delete temp file
          } else {
            data <- readRDS(compressed_file)
            data_list[[paste0("N_t", i, "_sim_", j)]] <- data
          }
        }, error = function(e) {
          message(paste("Could not load file:", compressed_file, "Error:", e))
        })
      } else {
        message(paste("File does not exist:", compressed_file))
      }
    }
  }
  assign(object_name, data_list, envir = .GlobalEnv)
}

# Load all files into corresponding objects
for (i in seq_along(N_scenarios)) {
  load_files(N_scenarios[i], object_names[i])
}

# Sum N_t1, N_t2, etc., to calculate the total roadkill
sum_matrices <- function(object) {
  result_list <- list()
  for (j in 1:20) {  # 20 simulations per level
    sum_matrix <- NULL
    for (i in 1:3) {  # 3 levels after N_t
      element_name <- paste0("N_t", i, "_sim_", j)
      if (!is.null(object[[element_name]])) {
        if (is.null(sum_matrix)) {
          sum_matrix <- object[[element_name]]
        } else {
          sum_matrix <- sum_matrix + object[[element_name]]
        }
      }
    }
    result_list[[paste0("sim_", j)]] <- sum_matrix
  }
  return(result_list)
}

# Sum matrices for each object and store the result
for (object_name in object_names) {
  object <- get(object_name)
  sum_result <- sum_matrices(object)
  assign(paste0(object_name, "_summed"), sum_result, envir = .GlobalEnv)
}

# List of summed object names
summed_object_names <- c(
  "SD_N_0_p2_0_output_analysis_summed"
)


# Function to sum columns of each matrix in an object for comparison with analysis results
sum_columns <- function(object) {
  result_list <- list()
  for (element_name in names(object)) {
    matrix <- object[[element_name]]
    if (!is.null(matrix)) {
      col_sum <- colSums(matrix)
      result_list[[element_name]] <- col_sum
    }
  }
  return(result_list)
}

# Sum columns for each summed object and store the result
for (summed_object_name in summed_object_names) {
  object <- get(summed_object_name)
  col_sum_result <- sum_columns(object)
  assign(gsub("_summed$", "_col_sum", summed_object_name), col_sum_result, envir = .GlobalEnv)
}

# List of names of col_sum objects
col_sum_object_names <- c(
  "SD_N_0_p2_0_output_analysis_col_sum"
)

# Initialize the results object
results <- list()

# Populate the results object with the desired structure
for (i in seq_along(N_scenarios)) {
  col_sum_object_name <- col_sum_object_names[i]
  object <- get(col_sum_object_name)
  col_sum_by_level <- list()
  for (j in 1:20) {  # 20 simulations
    sim_name <- paste0("sim_", j)
    col_sum_by_level[[j]] <- object[[sim_name]]
  }
  results[[N_scenarios[i]]] <- list(col_sum_by_level = col_sum_by_level)
}

# Example of how to access the data
print(results[[N_scenarios[1]]]$col_sum_by_level[[1]])

# Create an object to store totalN values from output analysis for each level
totalN_values <- list()

# Iterate over scenarios and simulations to extract totalN
for (scenario in output_scenarios) {
  totalN_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$totalN
  })
}

# Create a list to store the means of each column at each level
means_totalNs_sim_list <- list()

# Calculate the means of each column at each level of totalN_values
for (scenario in output_scenarios) {
  means_totalNs_sim_list[[scenario]] <- lapply(totalN_values[[scenario]], function(totalN_matrix) {
    apply(totalN_matrix, 2, mean)
  })
}

# Verify the results
print(means_totalNs_sim_list[[output_scenarios[1]]][[1]])


##########################################################################################
############################ Relative Root Mean Squared Error ############################
##########################################################################################

# Create a list to store the variances of each column at each level
vars_totalNs_sim_list <- list()

# Calculate the variances of each column at each level of totalN_values
for (scenario in output_scenarios) {
  vars_totalNs_sim_list[[scenario]] <- lapply(totalN_values[[scenario]], function(totalN_matrix) {
    apply(totalN_matrix, 2, var)
  })
}

# Add +1 to simulation results to avoid division by zero
for (scenario in N_scenarios) {
  for (level in seq_along(results[[scenario]]$col_sum_by_level)) {
    results[[scenario]]$col_sum_by_level[[level]] <- results[[scenario]]$col_sum_by_level[[level]] + 1
  }
}

# Verify the results before calculating RRMSE
print(means_totalNs_sim_list[[output_scenarios[1]]][[1]])
print(results[[N_scenarios[1]]]$col_sum_by_level[[1]])
print(vars_totalNs_sim_list[[output_scenarios[1]]][[1]])

# Calculate RRMSE
calculate_rrmse <- function(estimated, actual, variance) {
  return(sqrt((estimated - actual)^2 + variance) / actual)
}

# Construct the dynamic object name
object_name <- paste0(selected_group$path_group_se_005, "_rrmse_list_se_005_nsites_10")

# Create the list dynamically
assign(object_name, list())

for (scenario in output_scenarios) {
  # Get the current object
  temp_list <- get(object_name, envir = .GlobalEnv)
  
  # Initialize scenario entry
  temp_list[[scenario]] <- list()
  
  for (j in 1:20) {  # 20 simulations
    if (!is.null(results[[N_scenarios[which(output_scenarios == scenario)]]]) &&
        length(results[[N_scenarios[which(output_scenarios == scenario)]]]$col_sum_by_level) >= j &&
        !is.null(means_totalNs_sim_list[[scenario]]) &&
        length(means_totalNs_sim_list[[scenario]]) >= j &&
        !is.null(vars_totalNs_sim_list[[scenario]]) &&
        length(vars_totalNs_sim_list[[scenario]]) >= j) {
      
      actual <- results[[N_scenarios[which(output_scenarios == scenario)]]]$col_sum_by_level[[j]]
      estimated <- means_totalNs_sim_list[[scenario]][[j]]
      variance <- vars_totalNs_sim_list[[scenario]][[j]]
      
      if (!is.null(estimated) && !is.null(actual) && !is.null(variance) && length(actual) > 0) {
        temp_list[[scenario]][[j]] <- calculate_rrmse(estimated, actual, variance)
      } else {
        temp_list[[scenario]][[j]] <- NA
        message(paste("Missing or invalid data for scenario:", scenario, "simulation:", j))
      }
    } else {
      temp_list[[scenario]][[j]] <- NA
      message(paste("Data missing for scenario:", scenario, "simulation:", j))
    }
  }
  
  # Update the object in the global environment
  assign(object_name, temp_list, envir = .GlobalEnv)
}

eval(parse(text = object_name))

#Initialize an empty dataframe
rrmse_df_005_10 <- data.frame(RRMSE = numeric(), scenario = character(), Simulation = integer(), stringsAsFactors = FALSE)

for (scenario in output_scenarios) {
  for (j in 1:length(means_totalNs_sim_list[[scenario]])) {  # Iterate over the 20 simulations
    actual <- results[[N_scenarios[which(output_scenarios == scenario)]]]$col_sum_by_level[[j]]
    estimated <- means_totalNs_sim_list[[scenario]][[j]]
    variance <- vars_totalNs_sim_list[[scenario]][[j]]
    
    if (!is.null(estimated) && !is.null(actual) && !is.null(variance) && length(actual) > 0) {
      rrmse_value <- calculate_rrmse(estimated, actual, variance)
    } else {
      rrmse_value <- NA  # Assign NA if data is missing
      message(paste("Missing or invalid data for scenario:", scenario, "simulation:", j))
    }
    
    # Add row to the dataframe
    rrmse_df_005_10 <- rbind(rrmse_df_005_10, data.frame(
      RRMSE = rrmse_value,
      scenario = scenario,
      Simulation = j
    ))
  }
}


# Define scenarios dynamically
scenarios <- c(
  "ungulates_SE_p1_0.05_nsites_10_SD_N_0_SD_p1_0_output_analysis_")

# Define user-friendly labels
labels <- c(
  "SD N 0 p2 0")

# Create mapping using setNames()
scenario_map <- setNames(labels, scenarios)

# Map scenarios to user-friendly labels
rrmse_df_005_10$scenario <- factor(rrmse_df_005_10$scenario, levels = names(scenario_map), labels = scenario_map)

# Verify that all scenarios have been correctly mapped
print(unique(rrmse_df_005_10$scenario))

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
path_se005ntransect100 <- paste0(file.path(path_data, "ungulates/sites100_se05"), "/")

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

# Define scenarios for N
N_scenarios <- c(
  "ungulates_SE_p1_0.05_nsites_100_SD_N_0_SD_p1_0_N_t")


# To ensure we correctly load and manage the N matrices, we assign these names
object_names <- c(
  "SD_N_0_p2_0_output_analysis"
)

# Load N files
load_files <- function(scenario, object_name) {
  data_list <- list()
  for (i in 1:1) {  # We assume that ungulates persist throughout the month, so we simulate monthly roadkills at a single Nt level 
    for (j in 1:20) {  # 20 simulations per level
      compressed_file <- paste0(path_se005ntransect100, scenario, i, "_sim_", j, "_1.RData")
      temp_file <- tempfile(fileext = ".RData")
      
      if (file.exists(compressed_file)) {
        tryCatch({
          # Decompress if necessary
          if (grepl("\\.gz$", compressed_file)) {
            gunzip(compressed_file, destname = temp_file, remove = FALSE)
            data <- readRDS(temp_file)
            data_list[[paste0("N_t", i, "_sim_", j)]] <- data
            unlink(temp_file) # Delete temp file
          } else {
            data <- readRDS(compressed_file)
            data_list[[paste0("N_t", i, "_sim_", j)]] <- data
          }
        }, error = function(e) {
          message(paste("Could not load file:", compressed_file, "Error:", e))
        })
      } else {
        message(paste("File does not exist:", compressed_file))
      }
    }
  }
  assign(object_name, data_list, envir = .GlobalEnv)
}

# Load all files into corresponding objects
for (i in seq_along(N_scenarios)) {
  load_files(N_scenarios[i], object_names[i])
}

# Sum N_t1, N_t2, etc., to calculate the total roadkill
sum_matrices <- function(object) {
  result_list <- list()
  for (j in 1:20) {  # 20 simulations per level
    sum_matrix <- NULL
    for (i in 1:3) {  # 3 levels after N_t
      element_name <- paste0("N_t", i, "_sim_", j)
      if (!is.null(object[[element_name]])) {
        if (is.null(sum_matrix)) {
          sum_matrix <- object[[element_name]]
        } else {
          sum_matrix <- sum_matrix + object[[element_name]]
        }
      }
    }
    result_list[[paste0("sim_", j)]] <- sum_matrix
  }
  return(result_list)
}

# Sum matrices for each object and store the result
for (object_name in object_names) {
  object <- get(object_name)
  sum_result <- sum_matrices(object)
  assign(paste0(object_name, "_summed"), sum_result, envir = .GlobalEnv)
}

# List of summed object names
summed_object_names <- c(
  "SD_N_0_p2_0_output_analysis_summed"
)


# Function to sum columns of each matrix in an object for comparison with analysis results
sum_columns <- function(object) {
  result_list <- list()
  for (element_name in names(object)) {
    matrix <- object[[element_name]]
    if (!is.null(matrix)) {
      col_sum <- colSums(matrix)
      result_list[[element_name]] <- col_sum
    }
  }
  return(result_list)
}

# Sum columns for each summed object and store the result
for (summed_object_name in summed_object_names) {
  object <- get(summed_object_name)
  col_sum_result <- sum_columns(object)
  assign(gsub("_summed$", "_col_sum", summed_object_name), col_sum_result, envir = .GlobalEnv)
}

# List of names of col_sum objects
col_sum_object_names <- c(
  "SD_N_0_p2_0_output_analysis_col_sum"
)

# Initialize the results object
results <- list()

# Populate the results object with the desired structure
for (i in seq_along(N_scenarios)) {
  col_sum_object_name <- col_sum_object_names[i]
  object <- get(col_sum_object_name)
  col_sum_by_level <- list()
  for (j in 1:20) {  # 20 simulations
    sim_name <- paste0("sim_", j)
    col_sum_by_level[[j]] <- object[[sim_name]]
  }
  results[[N_scenarios[i]]] <- list(col_sum_by_level = col_sum_by_level)
}

# Example of how to access the data
print(results[[N_scenarios[1]]]$col_sum_by_level[[1]])

# Create an object to store totalN values from output analysis for each level
totalN_values <- list()

# Iterate over scenarios and simulations to extract totalN
for (scenario in output_scenarios) {
  totalN_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$totalN
  })
}

# Create a list to store the means of each column at each level
means_totalNs_sim_list <- list()

# Calculate the means of each column at each level of totalN_values
for (scenario in output_scenarios) {
  means_totalNs_sim_list[[scenario]] <- lapply(totalN_values[[scenario]], function(totalN_matrix) {
    apply(totalN_matrix, 2, mean)
  })
}

# Verify the results
print(means_totalNs_sim_list[[output_scenarios[1]]][[1]])


##########################################################################################
############################ Relative Root Mean Squared Error ############################
##########################################################################################

# Create a list to store the variances of each column at each level
vars_totalNs_sim_list <- list()

# Calculate the variances of each column at each level of totalN_values
for (scenario in output_scenarios) {
  vars_totalNs_sim_list[[scenario]] <- lapply(totalN_values[[scenario]], function(totalN_matrix) {
    apply(totalN_matrix, 2, var)
  })
}

# Add +1 to simulation results to avoid division by zero
for (scenario in N_scenarios) {
  for (level in seq_along(results[[scenario]]$col_sum_by_level)) {
    results[[scenario]]$col_sum_by_level[[level]] <- results[[scenario]]$col_sum_by_level[[level]] + 1
  }
}

# Verify the results before calculating RRMSE
print(means_totalNs_sim_list[[output_scenarios[1]]][[1]])
print(results[[N_scenarios[1]]]$col_sum_by_level[[1]])
print(vars_totalNs_sim_list[[output_scenarios[1]]][[1]])

# Calculate RRMSE
calculate_rrmse <- function(estimated, actual, variance) {
  return(sqrt((estimated - actual)^2 + variance) / actual)
}

# Construct the dynamic object name
object_name <- paste0(selected_group$path_group_se_005, "_rrmse_list_se_005_nsites_100")

# Create the list dynamically
assign(object_name, list())

for (scenario in output_scenarios) {
  # Get the current object
  temp_list <- get(object_name, envir = .GlobalEnv)
  
  # Initialize scenario entry
  temp_list[[scenario]] <- list()
  
  for (j in 1:20) {  # 20 simulations
    if (!is.null(results[[N_scenarios[which(output_scenarios == scenario)]]]) &&
        length(results[[N_scenarios[which(output_scenarios == scenario)]]]$col_sum_by_level) >= j &&
        !is.null(means_totalNs_sim_list[[scenario]]) &&
        length(means_totalNs_sim_list[[scenario]]) >= j &&
        !is.null(vars_totalNs_sim_list[[scenario]]) &&
        length(vars_totalNs_sim_list[[scenario]]) >= j) {
      
      actual <- results[[N_scenarios[which(output_scenarios == scenario)]]]$col_sum_by_level[[j]]
      estimated <- means_totalNs_sim_list[[scenario]][[j]]
      variance <- vars_totalNs_sim_list[[scenario]][[j]]
      
      if (!is.null(estimated) && !is.null(actual) && !is.null(variance) && length(actual) > 0) {
        temp_list[[scenario]][[j]] <- calculate_rrmse(estimated, actual, variance)
      } else {
        temp_list[[scenario]][[j]] <- NA
        message(paste("Missing or invalid data for scenario:", scenario, "simulation:", j))
      }
    } else {
      temp_list[[scenario]][[j]] <- NA
      message(paste("Data missing for scenario:", scenario, "simulation:", j))
    }
  }
  
  # Update the object in the global environment
  assign(object_name, temp_list, envir = .GlobalEnv)
}

eval(parse(text = object_name))


#Initialize an empty dataframe
rrmse_df_005_100 <- data.frame(RRMSE = numeric(), scenario = character(), Simulation = integer(), stringsAsFactors = FALSE)

for (scenario in output_scenarios) {
  for (j in 1:length(means_totalNs_sim_list[[scenario]])) {  # Iterate over the 20 simulations
    actual <- results[[N_scenarios[which(output_scenarios == scenario)]]]$col_sum_by_level[[j]]
    estimated <- means_totalNs_sim_list[[scenario]][[j]]
    variance <- vars_totalNs_sim_list[[scenario]][[j]]
    
    if (!is.null(estimated) && !is.null(actual) && !is.null(variance) && length(actual) > 0) {
      rrmse_value <- calculate_rrmse(estimated, actual, variance)
    } else {
      rrmse_value <- NA  # Assign NA if data is missing
      message(paste("Missing or invalid data for scenario:", scenario, "simulation:", j))
    }
    
    # Add row to the dataframe
    rrmse_df_005_100 <- rbind(rrmse_df_005_100, data.frame(
      RRMSE = rrmse_value,
      scenario = scenario,
      Simulation = j
    ))
  }
}


# Define scenarios dynamically
scenarios <- c(
  "ungulates_SE_p1_0.05_nsites_100_SD_N_0_SD_p1_0_output_analysis_")

# Define user-friendly labels
labels <- c(
  "SD N 0 p2 0")


# Create mapping using setNames()
scenario_map <- setNames(labels, scenarios)

# Map scenarios to user-friendly labels
rrmse_df_005_100$scenario <- factor(rrmse_df_005_100$scenario, levels = names(scenario_map), labels = scenario_map)

# Verify that all scenarios have been correctly mapped
print(unique(rrmse_df_005_100$scenario))

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
path_se01ntransect100 <- paste0(file.path(path_data, "ungulates/sites10_se10"), "/")

# Print the path
print(path_se01ntransect10)

# Generate output scenarios with integrated path_group_se_01
output_scenarios <- c(
  "ungulates_SE_p1_0.1_nsites_10_SD_N_0_SD_p1_0_output_analysis_"
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

# Define scenarios for N
N_scenarios <- c(
  "ungulates_SE_p1_0.1_nsites_10_SD_N_0_SD_p1_0_N_t")

# To ensure we correctly load and manage the N matrices, we assign these names
object_names <- c(
  "SD_N_0_p2_0_output_analysis"
)

# Load N files
load_files <- function(scenario, object_name) {
  data_list <- list()
  for (i in 1:1) {  # We assume that ungulates persist throughout the month, so we simulate monthly roadkills at a single Nt level 
    for (j in 1:20) {  # 20 simulations per level
      compressed_file <- paste0(path_se01ntransect100, scenario, i, "_sim_", j, "_1.RData")
      temp_file <- tempfile(fileext = ".RData")
      
      if (file.exists(compressed_file)) {
        tryCatch({
          # Decompress if necessary
          if (grepl("\\.gz$", compressed_file)) {
            gunzip(compressed_file, destname = temp_file, remove = FALSE)
            data <- readRDS(temp_file)
            data_list[[paste0("N_t", i, "_sim_", j)]] <- data
            unlink(temp_file) # Delete temp file
          } else {
            data <- readRDS(compressed_file)
            data_list[[paste0("N_t", i, "_sim_", j)]] <- data
          }
        }, error = function(e) {
          message(paste("Could not load file:", compressed_file, "Error:", e))
        })
      } else {
        message(paste("File does not exist:", compressed_file))
      }
    }
  }
  assign(object_name, data_list, envir = .GlobalEnv)
}

# Load all files into corresponding objects
for (i in seq_along(N_scenarios)) {
  load_files(N_scenarios[i], object_names[i])
}

# Sum N_t1, N_t2, etc., to calculate the total roadkill
sum_matrices <- function(object) {
  result_list <- list()
  for (j in 1:20) {  # 20 simulations per level
    sum_matrix <- NULL
    for (i in 1:3) {  # 3 levels after N_t
      element_name <- paste0("N_t", i, "_sim_", j)
      if (!is.null(object[[element_name]])) {
        if (is.null(sum_matrix)) {
          sum_matrix <- object[[element_name]]
        } else {
          sum_matrix <- sum_matrix + object[[element_name]]
        }
      }
    }
    result_list[[paste0("sim_", j)]] <- sum_matrix
  }
  return(result_list)
}

# Sum matrices for each object and store the result
for (object_name in object_names) {
  object <- get(object_name)
  sum_result <- sum_matrices(object)
  assign(paste0(object_name, "_summed"), sum_result, envir = .GlobalEnv)
}

# List of summed object names
summed_object_names <- c(
  "SD_N_0_p2_0_output_analysis_summed"
)


# Function to sum columns of each matrix in an object for comparison with analysis results
sum_columns <- function(object) {
  result_list <- list()
  for (element_name in names(object)) {
    matrix <- object[[element_name]]
    if (!is.null(matrix)) {
      col_sum <- colSums(matrix)
      result_list[[element_name]] <- col_sum
    }
  }
  return(result_list)
}

# Sum columns for each summed object and store the result
for (summed_object_name in summed_object_names) {
  object <- get(summed_object_name)
  col_sum_result <- sum_columns(object)
  assign(gsub("_summed$", "_col_sum", summed_object_name), col_sum_result, envir = .GlobalEnv)
}

# List of names of col_sum objects
col_sum_object_names <- c(
  "SD_N_0_p2_0_output_analysis_col_sum"
)

# Initialize the results object
results <- list()

# Populate the results object with the desired structure
for (i in seq_along(N_scenarios)) {
  col_sum_object_name <- col_sum_object_names[i]
  object <- get(col_sum_object_name)
  col_sum_by_level <- list()
  for (j in 1:20) {  # 20 simulations
    sim_name <- paste0("sim_", j)
    col_sum_by_level[[j]] <- object[[sim_name]]
  }
  results[[N_scenarios[i]]] <- list(col_sum_by_level = col_sum_by_level)
}

# Example of how to access the data
print(results[[N_scenarios[1]]]$col_sum_by_level[[1]])

# Create an object to store totalN values from output analysis for each level
totalN_values <- list()

# Iterate over scenarios and simulations to extract totalN
for (scenario in output_scenarios) {
  totalN_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$totalN
  })
}

# Create a list to store the means of each column at each level
means_totalNs_sim_list <- list()

# Calculate the means of each column at each level of totalN_values
for (scenario in output_scenarios) {
  means_totalNs_sim_list[[scenario]] <- lapply(totalN_values[[scenario]], function(totalN_matrix) {
    apply(totalN_matrix, 2, mean)
  })
}

# Verify the results
print(means_totalNs_sim_list[[output_scenarios[1]]][[1]])


##########################################################################################
############################ Relative Root Mean Squared Error ############################
##########################################################################################

# Create a list to store the variances of each column at each level
vars_totalNs_sim_list <- list()

# Calculate the variances of each column at each level of totalN_values
for (scenario in output_scenarios) {
  vars_totalNs_sim_list[[scenario]] <- lapply(totalN_values[[scenario]], function(totalN_matrix) {
    apply(totalN_matrix, 2, var)
  })
}

# Add +1 to simulation results to avoid division by zero
for (scenario in N_scenarios) {
  for (level in seq_along(results[[scenario]]$col_sum_by_level)) {
    results[[scenario]]$col_sum_by_level[[level]] <- results[[scenario]]$col_sum_by_level[[level]] + 1
  }
}

# Verify the results before calculating RRMSE
print(means_totalNs_sim_list[[output_scenarios[1]]][[1]])
print(results[[N_scenarios[1]]]$col_sum_by_level[[1]])
print(vars_totalNs_sim_list[[output_scenarios[1]]][[1]])

# Calculate the RRMSE
calculate_rrmse <- function(estimated, actual, variance) {
  return(sqrt((estimated - actual)^2 + variance) / actual)
}
# Construct the dynamic object name
object_name <- paste0(selected_group$path_group_se_01, "_rrmse_list_se_01_nsites_10")

# Create the list dynamically
assign(object_name, list())

for (scenario in output_scenarios) {
  # Get the current object
  temp_list <- get(object_name, envir = .GlobalEnv)
  
  # Initialize scenario entry
  temp_list[[scenario]] <- list()
  
  for (j in 1:20) {  # 20 simulations
    if (!is.null(results[[N_scenarios[which(output_scenarios == scenario)]]]) &&
        length(results[[N_scenarios[which(output_scenarios == scenario)]]]$col_sum_by_level) >= j &&
        !is.null(means_totalNs_sim_list[[scenario]]) &&
        length(means_totalNs_sim_list[[scenario]]) >= j &&
        !is.null(vars_totalNs_sim_list[[scenario]]) &&
        length(vars_totalNs_sim_list[[scenario]]) >= j) {
      
      actual <- results[[N_scenarios[which(output_scenarios == scenario)]]]$col_sum_by_level[[j]]
      estimated <- means_totalNs_sim_list[[scenario]][[j]]
      variance <- vars_totalNs_sim_list[[scenario]][[j]]
      
      if (!is.null(estimated) && !is.null(actual) && !is.null(variance) && length(actual) > 0) {
        temp_list[[scenario]][[j]] <- calculate_rrmse(estimated, actual, variance)
      } else {
        temp_list[[scenario]][[j]] <- NA
        message(paste("Missing or invalid data for scenario:", scenario, "simulation:", j))
      }
    } else {
      temp_list[[scenario]][[j]] <- NA
      message(paste("Data missing for scenario:", scenario, "simulation:", j))
    }
  }
  
  # Update the object in the global environment
  assign(object_name, temp_list, envir = .GlobalEnv)
}


eval(parse(text = object_name))

# # Initialize an empty dataframe
rrmse_df_01_10 <- data.frame(RRMSE = numeric(), scenario = character(), Simulation = integer(), stringsAsFactors = FALSE)

for (scenario in output_scenarios) {
  for (j in 1:length(means_totalNs_sim_list[[scenario]])) {  # Iterate over the 20 simulations
    actual <- results[[N_scenarios[which(output_scenarios == scenario)]]]$col_sum_by_level[[j]]
    estimated <- means_totalNs_sim_list[[scenario]][[j]]
    variance <- vars_totalNs_sim_list[[scenario]][[j]]
    
    if (!is.null(estimated) && !is.null(actual) && !is.null(variance) && length(actual) > 0) {
      rrmse_value <- calculate_rrmse(estimated, actual, variance)
    } else {
      rrmse_value <- NA  # Assign NA if data is missing
      message(paste("Missing or invalid data for scenario:", scenario, "simulation:", j))
    }
    
    # Add row to the dataframe
    rrmse_df_01_10 <- rbind(rrmse_df_01_10, data.frame(
      RRMSE = rrmse_value,
      scenario = scenario,
      Simulation = j
    ))
  }
}


# Verify that the scenarios have been correctly mapped
print(unique(rrmse_df_01_10$scenario))


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
path_se01ntransect100 <- paste0(file.path(path_data, "ungulates/sites100_se10"), "/")

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

# Define scenarios for N
N_scenarios <- c(
  "ungulates_SE_p1_0.1_nsites_100_SD_N_0_SD_p1_0_N_t")

# To ensure we correctly load and manage the N matrices, we assign these names
object_names <- c(
  "SD_N_0_p2_0_output_analysis"
)

# Load N files
load_files <- function(scenario, object_name) {
  data_list <- list()
  for (i in 1:1) {  # We assume that ungulates persist throughout the month, so we simulate monthly roadkills at a single Nt level 
    for (j in 1:20) {  # 20 simulations per level
      compressed_file <- paste0(path_se01ntransect100, scenario, i, "_sim_", j, "_1.RData")
      temp_file <- tempfile(fileext = ".RData")
      
      if (file.exists(compressed_file)) {
        tryCatch({
          # Decompress if necessary
          if (grepl("\\.gz$", compressed_file)) {
            gunzip(compressed_file, destname = temp_file, remove = FALSE)
            data <- readRDS(temp_file)
            data_list[[paste0("N_t", i, "_sim_", j)]] <- data
            unlink(temp_file) # Delete temp file
          } else {
            data <- readRDS(compressed_file)
            data_list[[paste0("N_t", i, "_sim_", j)]] <- data
          }
        }, error = function(e) {
          message(paste("Could not load file:", compressed_file, "Error:", e))
        })
      } else {
        message(paste("File does not exist:", compressed_file))
      }
    }
  }
  assign(object_name, data_list, envir = .GlobalEnv)
}

# Load all files into corresponding objects
for (i in seq_along(N_scenarios)) {
  load_files(N_scenarios[i], object_names[i])
}

# Sum N_t1, N_t2, etc., to calculate the total roadkill
sum_matrices <- function(object) {
  result_list <- list()
  for (j in 1:20) {  # 20 simulations per level
    sum_matrix <- NULL
    for (i in 1:3) {  # 3 levels after N_t
      element_name <- paste0("N_t", i, "_sim_", j)
      if (!is.null(object[[element_name]])) {
        if (is.null(sum_matrix)) {
          sum_matrix <- object[[element_name]]
        } else {
          sum_matrix <- sum_matrix + object[[element_name]]
        }
      }
    }
    result_list[[paste0("sim_", j)]] <- sum_matrix
  }
  return(result_list)
}

# Sum matrices for each object and store the result
for (object_name in object_names) {
  object <- get(object_name)
  sum_result <- sum_matrices(object)
  assign(paste0(object_name, "_summed"), sum_result, envir = .GlobalEnv)
}

# List of summed object names
summed_object_names <- c(
  "SD_N_0_p2_0_output_analysis_summed"
)


# Function to sum columns of each matrix in an object for comparison with analysis results
sum_columns <- function(object) {
  result_list <- list()
  for (element_name in names(object)) {
    matrix <- object[[element_name]]
    if (!is.null(matrix)) {
      col_sum <- colSums(matrix)
      result_list[[element_name]] <- col_sum
    }
  }
  return(result_list)
}

# Sum columns for each summed object and store the result
for (summed_object_name in summed_object_names) {
  object <- get(summed_object_name)
  col_sum_result <- sum_columns(object)
  assign(gsub("_summed$", "_col_sum", summed_object_name), col_sum_result, envir = .GlobalEnv)
}

# List of names of col_sum objects
col_sum_object_names <- c(
  "SD_N_0_p2_0_output_analysis_col_sum"
)

# Initialize the results object
results <- list()

# Populate the results object with the desired structure
for (i in seq_along(N_scenarios)) {
  col_sum_object_name <- col_sum_object_names[i]
  object <- get(col_sum_object_name)
  col_sum_by_level <- list()
  for (j in 1:20) {  # 20 simulations
    sim_name <- paste0("sim_", j)
    col_sum_by_level[[j]] <- object[[sim_name]]
  }
  results[[N_scenarios[i]]] <- list(col_sum_by_level = col_sum_by_level)
}

# Example of how to access the data
print(results[[N_scenarios[1]]]$col_sum_by_level[[1]])

# Create an object to store totalN values from output analysis for each level
totalN_values <- list()

# Iterate over scenarios and simulations to extract totalN
for (scenario in output_scenarios) {
  totalN_values[[scenario]] <- lapply(outputs[[scenario]], function(output) {
    output$sims.list$totalN
  })
}

# Create a list to store the means of each column at each level
means_totalNs_sim_list <- list()

# Calculate the means of each column at each level of totalN_values
for (scenario in output_scenarios) {
  means_totalNs_sim_list[[scenario]] <- lapply(totalN_values[[scenario]], function(totalN_matrix) {
    apply(totalN_matrix, 2, mean)
  })
}

# Verify the results
print(means_totalNs_sim_list[[output_scenarios[1]]][[1]])


##########################################################################################
############################ Relative Root Mean Squared Error ############################
##########################################################################################

# Create a list to store the variances of each column at each level
vars_totalNs_sim_list <- list()

# Calculate the variances of each column at each level of totalN_values
for (scenario in output_scenarios) {
  vars_totalNs_sim_list[[scenario]] <- lapply(totalN_values[[scenario]], function(totalN_matrix) {
    apply(totalN_matrix, 2, var)
  })
}

# Add +1 to simulation results to avoid division by zero
for (scenario in N_scenarios) {
  for (level in seq_along(results[[scenario]]$col_sum_by_level)) {
    results[[scenario]]$col_sum_by_level[[level]] <- results[[scenario]]$col_sum_by_level[[level]] + 1
  }
}

# Verify the results before calculating RRMSE
print(means_totalNs_sim_list[[output_scenarios[1]]][[1]])
print(results[[N_scenarios[1]]]$col_sum_by_level[[1]])
print(vars_totalNs_sim_list[[output_scenarios[1]]][[1]])

# Calculate the RRMSE
calculate_rrmse <- function(estimated, actual, variance) {
  return(sqrt((estimated - actual)^2 + variance) / actual)
}
# Construct the dynamic object name
object_name <- paste0(selected_group$path_group_se_01, "_rrmse_list_se_01_nsites_10")

# Create the list dynamically
assign(object_name, list())

for (scenario in output_scenarios) {
  # Get the current object
  temp_list <- get(object_name, envir = .GlobalEnv)
  
  # Initialize scenario entry
  temp_list[[scenario]] <- list()
  
  for (j in 1:20) {  # 20 simulations
    if (!is.null(results[[N_scenarios[which(output_scenarios == scenario)]]]) &&
        length(results[[N_scenarios[which(output_scenarios == scenario)]]]$col_sum_by_level) >= j &&
        !is.null(means_totalNs_sim_list[[scenario]]) &&
        length(means_totalNs_sim_list[[scenario]]) >= j &&
        !is.null(vars_totalNs_sim_list[[scenario]]) &&
        length(vars_totalNs_sim_list[[scenario]]) >= j) {
      
      actual <- results[[N_scenarios[which(output_scenarios == scenario)]]]$col_sum_by_level[[j]]
      estimated <- means_totalNs_sim_list[[scenario]][[j]]
      variance <- vars_totalNs_sim_list[[scenario]][[j]]
      
      if (!is.null(estimated) && !is.null(actual) && !is.null(variance) && length(actual) > 0) {
        temp_list[[scenario]][[j]] <- calculate_rrmse(estimated, actual, variance)
      } else {
        temp_list[[scenario]][[j]] <- NA
        message(paste("Missing or invalid data for scenario:", scenario, "simulation:", j))
      }
    } else {
      temp_list[[scenario]][[j]] <- NA
      message(paste("Data missing for scenario:", scenario, "simulation:", j))
    }
  }
  
  # Update the object in the global environment
  assign(object_name, temp_list, envir = .GlobalEnv)
}


eval(parse(text = object_name))

# # Initialize an empty dataframe
rrmse_df_01_100 <- data.frame(RRMSE = numeric(), scenario = character(), Simulation = integer(), stringsAsFactors = FALSE)

for (scenario in output_scenarios) {
  for (j in 1:length(means_totalNs_sim_list[[scenario]])) {  # Iterate over the 20 simulations
    actual <- results[[N_scenarios[which(output_scenarios == scenario)]]]$col_sum_by_level[[j]]
    estimated <- means_totalNs_sim_list[[scenario]][[j]]
    variance <- vars_totalNs_sim_list[[scenario]][[j]]
    
    if (!is.null(estimated) && !is.null(actual) && !is.null(variance) && length(actual) > 0) {
      rrmse_value <- calculate_rrmse(estimated, actual, variance)
    } else {
      rrmse_value <- NA  # Assign NA if data is missing
      message(paste("Missing or invalid data for scenario:", scenario, "simulation:", j))
    }
    
    # Add row to the dataframe
    rrmse_df_01_100 <- rbind(rrmse_df_01_100, data.frame(
      RRMSE = rrmse_value,
      scenario = scenario,
      Simulation = j
    ))
  }
}


# Verify that the scenarios have been correctly mapped
print(unique(rrmse_df_01_100$scenario))


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
final_RRMSE$log_RRMSE <- log(final_RRMSE$RRMSE)
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





