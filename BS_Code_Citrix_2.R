# Uncomment the install rows below each time using Uni lab machines

options("install.lock"=FALSE)
install.packages("sde")
install.packages("RQuantLib")
install.packages("dplyr") #, dependencies = TRUE, INSTALL_opts = '--no-lock')
install.packages("tidyverse")
install.packages("rsample")
# install.packages("devtools")
# library(devtools) # Needed for install_github if using
install.packages("keras")
# install_github("rstudio/keras")

library(sde)
library(RQuantLib)
install.packages("utf8")
library(dplyr)
library(tidyverse)
library(rsample)
library(keras)

install_keras() # (tensorflow = 'gpu') # Answer Y to installing miniconda if not already installed Anaconda

library(tensorflow) # Only for set_random_seed() function

# filepath="C:/Users/xtc_e/OneDrive - University of Sussex/Documents/2-Proposal/"
filepath="C:/Users/eh555/OneDrive - University of Sussex/Documents/2-Proposal/"
source(paste0(filepath,"RFunctions.R"))

##############
# Train models

# Create training data
target_fn   = BSM_EU_call
target_fn_d = BSM_EU_call_delta
S0          = 100
N           = 480
Tend        = 2
K_range     = c(0.5, 2)
b_range     = c(0, 0) # Set to c(0, 0) if not modelling a barrier option (0.6, 0.99)
eta_range   = c(0, 0) # Set to c(0, 0) if not modelling a barrier option (0, 0.3)
r_range     = c(0.01, 0.12) # Set to c(0.05, 0.05) if not modelling (0.01, 0.12)
sigma_range = c(0.05, 0.5) # Set to c(0.2, 0.2) if not modelling (0.05, 0.5)
dummy_range = c(0, 0) # Set to c(0, 0) if not adding a dummy variable (1, 5)
param_num   = 80 # Number of random parameter settings to be used for each date and maturity combination
T           = seq(0, Tend+0.5, 1/12)
excl        = 0 # Not needed now after moving to using S / K for training rather than S and K separately
noise_sigma = 0
num_paths   = 4
training    = sim_paths(target_fn, S0, N, Tend, K_range, b_range, eta_range, r_range, sigma_range, dummy_range, param_num, T, excl, num_paths, noise_sigma)
training2   = sim_paths(target_fn, S0, N, Tend, K_range, b_range, eta_range, r_range, sigma_range, dummy_range, as.integer(param_num * 0.3), T, excl, num_paths = 1, noise_sigma = 0)

# Fit multiple models to multiple training paths
training_cols           = c("S_K", "T_t", "r", "sigma") # Vector of columns to be used in training the model(s) (set up for "S_K", "T_t", "b", "eta", "r", "sigma", "dummy")
hidden_layers           = c(2) # Vector - 1, 2, 3 and/or 4 allowed according to number of hidden layers required. Use "1b"/"2b"/"1l"/"2l" to use batch or layer normalisation or "2sgdm" to test sgd optimiser with momentum = 0.9.
# params must be a list of lists. Each list must give values for epochs, batch_size, units, L2_factor, activation, optimizer and momentum (used only for 'sgdm' optimizer)
# activation options are "relu" or "sigmoid" or "tanh" or "softmax"
# optimizer options are "sgd", "rmsprop", "adagrad", "adadelta", "adam", "adamax", "nadam"
# units options are a vector to allow each hidden layer (max 4) to have a different number of units
# initializer options are "glorot_uniform", "glorot_normal", "he_normal", "he_uniform"
params                  = list(list(epochs = 10, batch_size = 64, units = c(200, 200), L2_factor = 0,
                                    activation = "relu", initializer = "glorot_uniform", dropout = 0,
                                    optimizer = "sgdm", momentum = 0.995))
trained_models          = build_models(hidden_layers, training, training2, params, training_cols)
models                  = trained_models[[1]]
learns                  = trained_models[[2]]
means                   = trained_models[[3]]
stds                    = trained_models[[4]]
results_train           = trained_models[[5]]
results_per_model_train = trained_models[[6]]
results_train_detail    = trained_models[[7]]

# Save training results as csv
write_excel_csv(results_train, paste0(filepath,"results_train.csv"))
write_excel_csv(results_per_model_train, paste0(filepath,"results_per_model_train.csv"))

##############
# Test models

# Create testing data
S0          = 100
N           = 240
Tend        = 1
K_range     = c(0.5,2)
b_range     = c(0, 0)
eta_range   = c(0, 0)
r_range     = c(0.01, 0.12)
sigma_range = c(0.05, 0.5)
dummy_range = c(0, 0)
param_num   = 20
T           = seq(0,Tend+0.5,1/12)
excl        = 0
num_paths   = 10
testing     = sim_paths(target_fn, S0, N, Tend, K_range, b_range, eta_range, r_range, sigma_range, dummy_range, param_num, T, excl, num_paths)

# Test multiple models on multiple training paths against multiple testing paths
# First define call option contract terms (list of vectors format c(K, Tend, b, eta) with b and eta = 0 if dealing with vanilla option)
opt_terms = list(list(K=80, T=1, b=0.95, eta=0.1),
                 list(K=100, T=1, b=0.85, eta=0.1),
                 list(K=125, T=1, b=0.7, eta=0.1))
tests_mean  = test_results(target_fn, target_fn_d, models, hidden_layers, params, results_per_model_train, testing, means, stds, opt_terms, training_cols, "mean")
results_mean_per_model = tests_mean[[1]]
results_mean_per_K     = tests_mean[[2]]
results_mean           = tests_mean[[3]]

# Save testing results as csv
write_excel_csv(results_mean, paste0(filepath,"results_mean.csv"))
write_excel_csv(results_mean_per_K, paste0(filepath,"results_mean_per_K.csv"))
write_excel_csv(results_mean_per_model, paste0(filepath,"results_mean_per_model.csv"))

tests_best  = test_results(target_fn, target_fn_d, models, hidden_layers, params, results_per_model_train, testing, means, stds, opt_terms, training_cols, "best")
results_best_per_model = tests_best[[1]]
results_best_per_K     = tests_best[[2]]
results_best           = tests_best[[3]]

# Save testing results as csv
write_excel_csv(results_best, paste0(filepath,"results_best.csv"))
write_excel_csv(results_best_per_K, paste0(filepath,"results_best_per_K.csv"))
write_excel_csv(results_best_per_model, paste0(filepath,"results_best_per_model.csv"))

###############################################
# Saving and loading models and testing results

exp_no = 2
# Save trained models and test results to disk
saveRDS(trained_models,paste0(filepath, "TrainedModels_", exp_no, ".RDS"))
save_models(models, filepath, exp_no)
saveRDS(tests_mean,paste0(filepath, "Tests_mean_", exp_no, ".RDS"))
saveRDS(tests_best,paste0(filepath, "Tests_best_", exp_no, ".RDS"))

# Load trained_models from disk
trained_models = readRDS(paste0(filepath, "TrainedModels_", exp_no, ".RDS"))
models                  = trained_models[[1]]
learns                  = trained_models[[2]]
means                   = trained_models[[3]]
stds                    = trained_models[[4]]
results_train           = trained_models[[5]]
results_per_model_train = trained_models[[6]]
results_train_detail    = trained_models[[7]]
models = load_models(filepath, 1)
# Load tests_mean from disk
tests_mean = readRDS(paste0(filepath, "Tests_mean_", exp_no, ".RDS"))
results_mean_per_model = tests_mean[[1]]
results_mean_per_K     = tests_mean[[2]]
results_mean           = tests_mean[[3]]
testing_paths_mean     = tests_mean[[4]]
results_mean_detail    = tests_mean[[5]]
# Load tests_best from disk
tests_best = readRDS(paste0(filepath, "Tests_best_", exp_no, ".RDS"))
results_best_per_model = tests_best[[1]]
results_best_per_K     = tests_best[[2]]
results_best           = tests_best[[3]]
testing_paths_best     = tests_best[[4]]
results_best_detail    = tests_best[[5]]



# Plotting not connected to any results as these are not saved yet (apart from for last model/training path/testing path)
results_plot=results_train_detail[[1]][[1]][[1]]
plot(x=results_plot$true, y=results_plot$pred , cex= 0.001, xlab='Actual', ylab='Predicted')
lines(results_plot$true, results_plot$true, col="green")
plot(x=results_plot$S_K, y=results_plot$err, xlab='S / K', ylab='|BS Price - Model Prediction|',cex=0.01)
plot(x=results_plot$T_t, y=results_plot$err, xlab='Years to maturity', ylab='|BS Price - Model Prediction|',cex=0.01)
plot(x=results_plot$T_t, y=results_plot$err_abs, xlab='Years to maturity', ylab='BS Price - Model Prediction',cex=0.01)
plot(x=results_plot$T_t, y=results_plot$err_mod, xlab='Years to maturity', ylab='BS Price / K - Model Prediction / K',cex=0.01)



# Play area
tests_2  = test_results_2(models, params, training, results_per_model_train, testing, means, stds, K_test, T_test, "mean")
results_per_model_2 = tests_2[[1]]
results_per_K_2     = tests_2[[2]]
results_2           = tests_2[[3]]
testing_paths_2     = tests_2[[4]]

tests_3  = test_results_2(models, params, training, results_per_model_train, testing, means, stds, K_test, T_test, "best")
results_per_model_3 = tests_3[[1]]
results_per_K_3     = tests_3[[2]]
results_3           = tests_3[[3]]
testing_paths_3     = tests_3[[4]]

x1=testing_paths[[1]][[1]][[1]][[3]]
x2=testing_paths[[1]][[1]][[2]][[3]]

testing_path          = testing[[1]] %>% dplyr::group_by(t) %>% dplyr::summarise(S = max(S), r = 0.5, sigma = 0.2, K = 100, T = 1)
testing_path$T_t      = testing_path$T-testing_path$t
testing_path$S_K      = testing_path$S/testing_path$K 
testing_path$ML       = ML_EU_call(models[[1]][[1]], testing_path %>% dplyr::select(all_of(training_cols)), means, stds, testing_path$K)
ML_delta(ML_EU_call, models[[1]][[1]], testing_path %>% dplyr::select(all_of(training_cols)), means, stds, testing_path$K, testing_path$ML)

BSM_EU_call_barrier(S=100, K=100, B=90, r=0.05, sigma=0.2, t=0, T=1)
BarrierOption("downout", "call", 80, 100, 0, 0.05, 1, 0.2, 90, 0)
ML_price(models[[1]][[1]], model_inputs=tibble(S_K=1,B_K=0.9,T_t=0.01), list(means[[1]]), list(stds[[1]]), 100)

K = 100
T = 1
b = 0.9
eta = 0.05
B = b * K * exp(-eta * T)
r = 0.05
v = 0.2
S = seq(60, 150, 0.1)

BS_C = BSM_EU_call(S, K, b, eta, r, v, t=0, T)
BS_C_DO = BSM_EU_call_barrier(S, K, b, eta, r, v, t=0, T, "down", "out")
BS_C_DO_delta = BSM_EU_call_barrier_delta(S, K, b, eta, r, v, t=0, T, "down", "out", prices = BS_C_DO)
BS_C_DI = BSM_EU_call_barrier(S, K, b, eta, r, v, t=0, T, "down", "in")
BS_C_DI_delta = BSM_EU_call_barrier_delta(S, K, b, eta, r, v, t=0, T, "down", "in", prices = BS_C_DI)
BS_C_ML = ML_price(list(models[[1]][[1]][[1]]), model_inputs=tibble(S_K=S/K,b=b,eta=eta,T_t=T), list(means[[1]]), list(stds[[1]]), 100)

plot(x=S, y=BS_C_ML, cex= 0.001, xlab='Underlying Price', ylab='Option Price', ylim=c(-5, 60))
lines(S, BS_C_DO, col="green")
lines(S, BS_C, col="red")
lines(S, BS_C_DI, col="blue")
lines(S, BS_C_DI_delta, col="orange")

BS_C_DO_RQLib = c()
for (s in S) {
  if (s<=B) {BS_C_DO_RQLib = c(BS_C_DO_RQLib, 0)}
  else {BS_C_DO_RQLib = c(BS_C_DO_RQLib, BarrierOption("downout", "call", s, K, 0, r, 1, v, B)$value)}
}
lines(S, BS_C_DO_RQLib, col="blue")




