library(RQuantLib)
library(sde)
library(readxl)

filepath="C:/Users/xtc_e/OneDrive - University of Sussex/Documents/2-Proposal/"
# filepath="C:/Users/eh555/OneDrive - University of Sussex/Documents/2-Proposal/"
source(paste0(filepath,"RFunctions.R"))

######################################
# Load trained models and test results

exp_no = 2
# Load trained_models from disk
trained_models = readRDS(paste0(filepath,"TrainedModels_",exp_no,".RDS"))
models                  = trained_models[[1]]
learns                  = trained_models[[2]]
means                   = trained_models[[3]]
stds                    = trained_models[[4]]
results_train           = trained_models[[5]]
results_per_model_train = trained_models[[6]]
results_train_detail    = trained_models[[7]]
# Load tests_mean from disk
tests_mean = readRDS(paste0(filepath,"Tests_mean_",exp_no,".RDS"))
results_mean_per_model = tests_mean[[1]]
results_mean_per_K     = tests_mean[[2]]
results_mean           = tests_mean[[3]]
testing_paths_mean     = tests_mean[[4]]
results_mean_detail    = tests_mean[[5]]
# Load tests_best from disk
tests_best = readRDS(paste0(filepath,"Tests_best_",exp_no,".RDS"))
results_best_per_model = tests_best[[1]]
results_best_per_K     = tests_best[[2]]
results_best           = tests_best[[3]]
testing_paths_best     = tests_best[[4]]
results_best_detail    = tests_best[[5]]

########################
# Plotting BS Call value

K = 100; r = 0.05; v = 0.2
Ss = seq(70, 150, 0.05)
Ts = c(0.25, 0.5, 1)
EuroCallApply = function(Ss, K, r, T, v) {unlist(sapply(Ss, EuropeanOption, type="call", strike=K,
                                    dividendYield=0, riskFreeRate=r, maturity=T, volatility=v)[1,])}
EuroCallApplyLine = function(Ss, K, r, T, v, C) {lines(Ss, EuroCallApply(Ss, K, r, T, v),
                                    type="l", lwd=2, col=C)}
plot(Ss, sapply(Ss, call_payoff, K = K), type="l", lwd=2, xlab="Price of underlying asset",
     ylab="Value of option")
for (T in 1:length(Ts)) {
    EuroCallApplyLine(Ss, K, r, Ts[T], v, T+1)
}
legend(x="topleft", legend=c("0", "3", "6", "12"), title = "Months to Maturity", lwd = 2, col = 1:4)

################################
# Plotting BS Barrier Call value

K = 100; r = 0.05; v = 0.2; T = 1; b = 0.9; eta = 0.1
Ss = seq(75, 120, 0.05)
plot(Ss, sapply(Ss, call_payoff, K = K), type="l", lwd=2, xlab="Price of underlying asset",
     ylab="Value of option")
EuroCallApplyLine(Ss, K, r, T, v, "red")
lines(Ss, BSM_EU_call_barrier(Ss, K, b, eta, r, v, 0, T), col="blue")
legend(x="topleft", legend=c("Payoff", "Standard Call", "Down-and-out"), title = "Types", lwd = 2, col = c("black", "red", "blue"))

#########################
# Plotting Long Fwd value

K = 100; Ss = seq(50, 150, 0.05)
plot(Ss, Ss - K, type="l", lwd=2, xlab="Price of underlying asset",
     ylab="Value of forward")

#############################
# Plotting multiple GBM paths

ts = seq(0, 25, 0.01); S0 = 100; r = 0.05; sigma = 0.05
plot(ts, sde::GBM(x=S0, N=length(ts) - 1, r=r, sigma=sigma, T=ts[length(ts)]), type="l",
     lwd=2, xlab="Time in years", ylab="Price", ylim=c(50, 500))
for (path in 2:5) {
    lines(ts, sde::GBM(x=S0, N=length(ts) - 1, r=r, sigma=sigma, T=ts[length(ts)]), type="l",
          lwd=2, col=path)
}

#########################################
# Importing and plotting Ensemble results

tb = read_excel(paste0(filepath,"Results_reorgs.xlsx"),"Ensembles")
Ensemble_list = list(c("Mean", "solid"), c("Best", "dotted"))
tb = tb %>% mutate(logLoss=log10(Loss))
plot(x=0, y=0, xlab="Experiment number", ylab="Log(MSE Losses)", ylim=c(min(tb$logLoss), max(tb$logLoss)), xlim=c(0.5, 6.5))
for (c in 1:4) {
    for (t in Ensemble_list) {
        data = tb %>% filter(Training==c, Ensemble_type==t[1])
        lines(unlist(data %>% select(Experiment)), unlist(data %>% select(logLoss)), col=c, lty=t[2], lwd=2)
    }
}
legend(x="topleft", legend=c("Training 1", "Training 2", "Training 3", "Training 4"), title = "Training", lwd = 2, col = 1:4)
legend(x="bottomright", legend=c("Mean", "Best"), title = "Ensemble", lwd=2, lty=c("solid", "dotted"))

################################
# Plotting example tracking path

data = testing_paths_best[[1]][[1]][[1]][[3]]
plot(data$t, data$S, ylim=c(-160, 160), type="l", xlab="Time", ylab="Value", col="blue")
lines(data$t, data$K, col="black")
lines(data$t, data$ML, col="green")
lines(data$t, data$V_b_ML, col="orange")
lines(data$t, data$V_s_ML, col="purple")
lines(data$t, data$V_ML, col="red")
legend("bottomleft", legend=c("Underlying", "Strike", "Option", "Underlying hedge", "Cash", "Portfolio"),
       col=c("blue", "black", "green", "orange", "purple", "red"), lty="solid", cex=0.75, pt.cex = 1)

#########################
# Plotting results errors

data = results_best_detail[[1]][[1]][[1]]
# Plot Predicted vs Actual
plot(x=data$true, y=data$pred, xlab='BSM Price', ylab='ML Price', col=alpha("black", 0.2), cex=0.001)
lines(data$true, data$true, col="green")
# Plot Error/K against S/K
plot(x=data$S_K, y=data$mod_err, ylim=c(-0.06, 0.06), xlab='Underlying / Strike', ylab='(ML Price - BSM Price) / Strike', col=alpha("black", 0.2), cex=0.001)
abline(h=0, col="green")
# Plot Error/K against T-t
plot(x=data$T_t, y=data$mod_err, ylim=c(-0.06, 0.06), xlab='Years to maturity', ylab='(ML Price - BSM Price) / Strike', col=alpha("black", 0.2), cex=0.001)
abline(h=0, col="green")

###############
# Other testing

# Plotting barrier options
K = 100
T = 1
b = 0.9
eta = 0.1
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