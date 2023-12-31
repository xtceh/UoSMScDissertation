library(RQuantLib)
library(sde)

library(readxl)
library(scales)
library(dplyr)

filepath="C:/Users/xtc_e/OneDrive - University of Sussex/Documents/2-Proposal/"
# filepath="C:/Users/eh555/OneDrive - University of Sussex/Documents/2-Proposal/"
source(paste0(filepath,"RFunctions.R"))

######################################
# Load trained models and test results

exp_no = "8"
# Load trained_models from disk
trained_models          = readRDS(paste0(filepath,"TrainedModels_",exp_no,".RDS"))
models                  = trained_models[[1]]
learns                  = trained_models[[2]]
means                   = trained_models[[3]]
stds                    = trained_models[[4]]
results_train           = trained_models[[5]]
results_per_model_train = trained_models[[6]]
results_train_detail    = trained_models[[7]]
models                  = load_models(filepath, exp_no)
# Load tests_mean from disk
tests_mean              = readRDS(paste0(filepath,"Tests_mean_",exp_no,".RDS"))
results_mean_per_model  = tests_mean[[1]]
results_mean_per_K      = tests_mean[[2]]
results_mean            = tests_mean[[3]]
testing_paths_mean      = tests_mean[[4]]
results_mean_detail     = tests_mean[[5]]
# Load tests_best from disk
tests_best              = readRDS(paste0(filepath,"Tests_best_",exp_no,".RDS"))
results_best_per_model  = tests_best[[1]]
results_best_per_K      = tests_best[[2]]
results_best            = tests_best[[3]]
testing_paths_best      = tests_best[[4]]
results_best_detail     = tests_best[[5]]

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

# Values against S relative to payoff and standard call option
K = 100; r = 0.05; v = 0.2; T = 1; b = 0.9; eta = 0.1
Ss = seq(75, 120, 0.05)
plot(Ss, sapply(Ss, call_payoff, K = K), type="l", lwd=2, xlab="Price of underlying asset",
     ylab="Value of option")
EuroCallApplyLine(Ss, K, r, T, v, "red")
lines(Ss, BSM_EU_call_barrier(Ss, K, b, eta, r, v, 0, T), col="blue")
legend(x="topleft", legend=c("Payoff", "Standard Call", "Down-and-out"), title = "Types",
       lwd = 2, col = c("black", "red", "blue"))

# Values against each input
S = 1; K = 1; r = 0.05; v = 0.2; T = 1; b = 0.9; eta = 0.1
S_Ks = seq(0.25, 1.25, 0.005); Ts = seq(0, 2, 0.01); rs = seq(0.01, 0.12, 0.001)
vs = seq(0.05, 0.5, 0.001); bs = seq(0.1, 0.999, 0.001); etas = seq(0, 0.5, 0.01)
plot(S_Ks/max(S_Ks), BSM_EU_call_barrier(S_Ks, K, b, eta, r, v, 0, T), type="l", lwd=2,
     xlab="Increasing value of input", ylab="Value of option", xlim=c(0,1))
lines(Ts/max(Ts), BSM_EU_call_barrier(S, K, b, eta, r, v, 0, Ts), col="blue", lwd=2)
lines(rs/max(rs), BSM_EU_call_barrier(S, K, b, eta, rs, v, 0, T), col="red", lwd=2)
lines(vs/max(vs), BSM_EU_call_barrier(S, K, b, eta, r, vs, 0, T), col="green", lwd=2)
lines(bs/max(bs), BSM_EU_call_barrier(S, K, bs, eta, r, v, 0, T), col="orange", lwd=2)
lines(etas/max(etas), BSM_EU_call_barrier(S, K, b, etas, r, v, 0, T), col="purple", lwd=2)
legend(x="topleft", legend=c("Underlying/Strike", "Time to maturity", "Risk-free interest rate", "Underlying volatility", "Barrier level", "Barrier decay"), title = "BSM model inputs",
       lwd = 2, col = c("black", "blue", "red", "green", "orange", "purple"), cex=0.9)

# Values with noise
S = 1; K = 1; r = 0.05; v = 0.2; T = 1; b = 0.9; eta = 0.1
S_Ks = seq(0.5, 1.5, 0.005)
prices=BSM_EU_call_barrier(S_Ks, K, b, eta, r, v, 0, T)
plot(S_Ks, prices + rnorm(length(prices), mean=0, sd=0.4) * prices, pch=20,
     xlab="Strike / Underlying price", ylab="Value of option", col=alpha("black",0.75))
lines(S_Ks, prices, cex=0.01, col="green")
legend(x="topleft", legend=c("Noisy BSM prices", "Exact BSM prices"), lty=c(NA, 1), pch=c(20, NA), col=c("black", "green"))

#########################################################
# Charts developed as examples for ones used in Shiny App

# BSM and ML values against one input
# Against S_K
S = 100; K = 100; r = 0.05; v = 0.2; T = 1; b = 0.9; eta = 0.1; dummy = 3
Ks = seq(S*2, S/2, length.out=1000)
S_Ks = S/Ks
input_data = tibble(S_K=S_Ks, T_t=T, r=r, sigma=v, b=b, eta=eta, dummy=dummy)
input_data = scale_data(input_data, means[[1]], stds[[1]])
MLPrices = predict(models[[1]][[1]][[1]], input_data) * Ks
BSMPrices = BSM_EU_call_barrier(S, Ks, b, eta, r, v, 0, T)
plot(S_Ks, MLPrices, type="l", col="red", xlab="Underlying/Strike Price", ylab="Value of option",
     ylim=c(0,max(max(MLPrices),max(BSMPrices))),
     main="ML/BSM Model Price of Option with Increasing Value of Underlying/Strike")
lines(S_Ks, BSMPrices, col="blue")
if (BSMPrices[1]>0.5*max(BSMPrices)) {x="bottomleft"} else {x="topleft"}
legend(x=x, legend=c("ML Model Price", "BSM Model Price"), col=c("red","blue"), lwd=c(1,1))
# Against T
Ts = seq(2.5, 1/240, length.out=1000)
input_data = tibble(S_K=S/K, T_t=Ts, r=r, sigma=v, b=b, eta=eta, dummy=dummy)
input_data = scale_data(input_data, means[[1]], stds[[1]])
MLPrices = predict(models[[1]][[1]][[1]], input_data) * K
BSMPrices = BSM_EU_call_barrier(S, K, b, eta, r, v, 0, Ts)
plot(Ts, MLPrices, type="l", col="red", xlab="Time to Maturity", ylab="Value of option",
     ylim=c(0,max(max(MLPrices),max(BSMPrices))),
     main="Price of Option vs Time to Maturity")
lines(Ts, BSMPrices, col="blue")
if (BSMPrices[1]>0.5*max(BSMPrices)) {x="bottomleft"} else {x="topleft"}
legend(x=x, legend=c("ML Model Price", "BSM Model Price"), col=c("red","blue"), lwd=c(1,1))

# Charting created tracking paths
target_fn = BSM_EU_call_barrier
target_fn_d = BSM_EU_call_barrier_delta
training_cols = c("S_K", "T_t", "r", "sigma", "b", "eta", "dummy")
S = 100; K = 100; r = 0.05; v = 0.2; T = 1; b = 0.9; eta = 0.1; dummy = 3
ltd_path = generate_data_ltd(target_fn, S, K, b, eta, r, v, dummy, T, 1)
track_path = tracking_path_ltd(target_fn, target_fn_d, ltd_path, list(models[[1]][[1]][[1]]), list(means[[1]]), list(stds[[1]]), K, T, b, eta, r, v, dummy, c("S_K", "T_t", "r", "sigma", "b", "eta", "dummy"))
plot(track_path$t, track_path$S, ylim=c(-150, 150), type="l", xlab="Time", ylab="Value", col="blue")
lines(track_path$t, track_path$K, col="black")
lines(track_path$t, -track_path$ML, col="green")
lines(track_path$t, track_path$V_b_ML, col="orange")
lines(track_path$t, track_path$V_s_ML, col="purple")
lines(track_path$t, track_path$V_ML, col="red")
legend("bottomright", legend=c("Option", "Underlying hedge", "Cash"),
       col=c("green", "orange", "purple"), lty="solid", cex=0.8, pt.cex = 1)
legend("bottomleft", legend=c("Underlying", "Strike", "Portfolio (net)"),
       col=c("blue", "black", "red"), lty="solid", cex=0.8, pt.cex = 1)

plot(track_path$t, track_path$V_ML, ylim=c(-5, 5), type="l", xlab="Time", ylab="Value", col="blue")
lines(track_path$t, track_path$V_BS, col="red")
legend("bottomleft", legend=c("ML Portfolio (net)", "BSM Portfolio (net)"),
       col=c("blue", "red"), lty="solid", cex=0.8, pt.cex = 1)

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
Ensemble_list = list(c("Mean", "dotted"), c("Best", "solid"))
tb = tb %>% mutate(logLoss=log10(Loss))
plot(x=0, y=0, xlab="Experiment number", ylab="Log(MSE Losses)", ylim=c(min(tb$logLoss), max(tb$logLoss)), xlim=c(0.8, 8.2))
for (c in 1:4) {
    for (t in Ensemble_list) {
        data = tb %>% filter(Training==c, Ensemble_type==t[1])
        lines(unlist(data %>% select(Experiment)), unlist(data %>% select(logLoss)), col=c, lty=t[2], lwd=2)
    }
}
legend(x="topleft", legend=c("Training 1", "Training 2", "Training 3", "Training 4"), title = "Training", lwd = 2, col = 1:4, cex=0.8)
legend(x="bottomright", legend=c("Mean", "Best"), title = "Ensemble", lwd=2, lty=c("dotted", "solid"), cex=0.8)

################################
# Plotting example tracking path

data = testing_paths_best[[1]][[1]][[1]][[3]]
plot(data$t, data$S, ylim=c(-125, 140), type="l", xlab="Time", ylab="Value", col="blue")
lines(data$t, data$K, col="black")
lines(data$t, -data$ML, col="green")
lines(data$t, data$V_b_ML, col="orange")
lines(data$t, data$V_s_ML, col="purple")
lines(data$t, data$V_ML, col="red")
legend("bottomright", legend=c("Option", "Underlying hedge", "Cash"),
       col=c("green", "orange", "purple"), lty="solid", cex=0.8, pt.cex = 1)
legend("bottomleft", legend=c("Underlying", "Strike", "Portfolio (net)"),
       col=c("blue", "black", "red"), lty="solid", cex=0.8, pt.cex = 1)

#########################
# Plotting results errors

data = results_best_detail[[1]][[1]][[1]]
# Plot Predicted vs Actual
plot(x=data$true, y=data$pred, xlab='BSM Price', ylab='ML Price', col=alpha("black", 0.2), cex=0.001)
lines(data$true, data$true, col="green")
legend(x="topleft", legend=c("ML prices", "Zero error"), lty=c(NA, 1), pch=c(20, NA), col=c("black", "green"))
# Plot Error/K against S/K
plot(x=data$S_K, y=data$mod_err, ylim=c(-0.06, 0.06), xlab='Underlying / Strike', ylab='(ML Price - BSM Price) / Strike', col=alpha("black", 0.2), cex=0.001)
abline(h=0, col="green")
legend(x="bottomleft", legend=c("Errors", "Zero error"), lty=c(NA, 1), pch=c(20, NA), col=c("black", "green"))
# Plot Error/K against T-t
plot(x=data$T_t, y=data$mod_err, ylim=c(-0.06, 0.06), xlab='Years to maturity', ylab='(ML Price - BSM Price) / Strike', col=alpha("black", 0.2), cex=0.001)
abline(h=0, col="green")
legend(x="bottomright", legend=c("Errors", "Zero error"), lty=c(NA, 1), pch=c(20, NA), col=c("black", "green"))

# Try using smoothScatter (not used as not as clear as the basic dots)
# Plot Error/BSM Price against S/K
smoothScatter(x=data$S_K, y=data$mod_err, ylim=c(-0.06, 0.06), xlab='Underlying / Strike', ylab='100*(ML-BSM)/BSM Prices', colramp = colorRampPalette(c("white", "black")), cex=0.001)
abline(h=0, col="green")
legend(x="bottomright", legend=c("Errors", "Zero error"), lty=c(NA, 1), pch=c(20, NA), col=c("black", "green"))
# Plot Error/BSM Price against S/K
smoothScatter(x=data$T_t, y=data$mod_err, ylim=c(-0.06, 0.06), xlab='Years to maturity', ylab='100*(ML-BSM)/BSM Prices', colramp = colorRampPalette(c("white", "black")), cex=0.001)
abline(h=0, col="green")
legend(x="bottomright", legend=c("Errors", "Zero error"), lty=c(NA, 1), pch=c(20, NA), col=c("black", "green"))

# Try percentage error (not used as too much weight to percentage errors in very small prices)
# Plot Error/BSM Price against S/K
plot(x=data$S_K, y=data$err/data$true*100, ylim=c(-30, 30), xlab='Underlying / Strike', ylab='100*(ML-BSM)/BSM Prices', col=alpha("black", 0.2), cex=0.001)
abline(h=0, col="green")
legend(x="bottomright", legend=c("Errors", "Zero error"), lty=c(NA, 1), pch=c(20, NA), col=c("black", "green"))
# Plot Error/BSM Price against T-t
plot(x=data$T_t, y=data$err/data$true*100, ylim=c(-30, 30), xlab='Years to maturity', ylab='100*(ML-BSM)/BSM Prices', col=alpha("black", 0.2), cex=0.001)
abline(h=0, col="green")
legend(x="bottomright", legend=c("Errors", "Zero error"), lty=c(NA, 1), pch=c(20, NA), col=c("black", "green"))

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

# Sample data
x <- 1:10
y <- c(3, 5, 8, 6, 4, 7, 9, 4, 6, 8)

# Create a scatterplot with points and a line
plot(x, y, type = "p", pch = 16, col = "blue", main = "Scatterplot with Line", xlab = "X", ylab = "Y")
lines(x, y, col = "red")

# Add a legend
legend("topright", legend = c("Scatterplot", "Line"), col = c("blue", "red"), pch = c(16, NA), lty = c(NA, 1))




# Functions to get ML and BSM prices dependent on Exp No
MLPricePerExp = function(Exp, models, means, stds, S, K, T, r, v, b, eta, dummy) {
    B = barrier(K, b, eta, T)
    if (Exp == 1) {input_data = tibble(S_K=S/K, T_t=T)}
    else if (Exp == 2) {input_data = tibble(S_K=S/K, T_t=T, r=r, sigma=v)}
    else if (Exp == 3 | Exp == 4 | Exp == 5) {input_data = tibble(S_K=S/K, T_t=T, r=r, sigma=v, b=b, eta=eta)}
    else {input_data = tibble(S_K=S/K, T_t=T, r=r, sigma=v, b=b, eta=eta, dummy=dummy)}
    input_data = scale_data(input_data, means, stds)
    return(ind(S > B) * predict(models, input_data) * K)
}
# Function to price a down-and-out/in barrier option using the BSM model
barrier = function(K, b, eta, T) {return(b * K * exp(-eta * T))}

Exp="8"; Ss=seq(50,200); T=1; K=100; r=0.05; v=0.2; b=0.90; eta=0.1; dummy=3
MLPricePerExp(Exp, models[[1]][[1]][[1]], means[[1]], stds[[1]], Ss, K, T, r, v, b, eta, dummy)
