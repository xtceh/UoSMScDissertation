#######################################################################
# Functions copied from RFunctions.R as used for experiments in project

# Function to scale data: Take x as data.table & means/stds as vectors as inputs and return a matrix suitable for data input to a Keras model
scale_data = function(x, means, stds) {
    for (c in seq_along(x)) {
        x[[c]] = x[[c]] - means[c]
        x[[c]] = x[[c]] / stds[c]
    }
    return(as.matrix(x))
}

# Function to price/get delta for an option using the ML model
ML_price = function(models, model_inputs, means, stds, K) { # models/means/stds are lists - one element for each training run to be included in mean pricing
    len   = length(models)
    price = rep(0, dim(model_inputs)[1])
    for (i in 1:len) {
        model_inputs_tmp = scale_data(model_inputs, means[[i]], stds[[i]])
        price = price + as.vector(models[[i]] %>% predict(model_inputs_tmp) * K)
    }
    return(price/len)
}
ML_delta = function(models, model_inputs, means, stds, K, prices = NULL) { # models/means/stds are lists - one element for each training run to be included in mean pricing
    if (is.null(prices)) {
        prices = ML_price(models, model_inputs, means, stds, K)
    }
    model_inputs$S_K = model_inputs$S_K + 0.000001
    prices_adj = ML_price(models, model_inputs, means, stds, K)
    return((prices_adj - prices) * 1000000 / K) # Returns price change for an increase of 1 in S
}

# Call payoff and other helper functions
call_payoff = function(S, K) {
    payoff = S - K
    payoff[payoff<=0] = 0
    return(payoff)
}
ind  = function(cond) {ifelse(cond, 1, 0)}
d1   = function(s, r, sigma, T) {(log(s) + (r + sigma^2 / 2) * T) / sigma / sqrt(T)}
d2   = function(s, r, sigma, T) {(d1(s, r, sigma, T) - sigma * sqrt(T))}

# Function to price/get a delta for a standard option using the BSM model
BSM_EU_call = function(S, K, b, eta, r, sigma, t=0, T) { # S and T must be same length vectors if require return vector
    return(S * pnorm(d1(S / K, r, sigma, T - t)) - (K * exp(-r * (T - t)) * pnorm(d2(S / K, r, sigma, T - t))))
}
BSM_EU_call_delta = function(S, K, b, eta, r, sigma, t=0, T){ # S and T must be same length vector if require return vector
    return(pnorm(d1(S / K, r, sigma, T - t)))
}

# Function to price a down-and-out/in barrier option using the BSM model
barrier = function(K, b, eta, T) {return(b * K * exp(-eta * T))}
# As for BS<M_EU_call but b=Barrier relative to exercise at maturity, eta=decay rate of barrier relative to exercise as time to maturity increases
# (type1="up"/"down", type2="out"/"in") but "up" formula not currently working
BSM_EU_call_barrier = function(S, K, b, eta, r, sigma, t=0, T, type1 = "down", type2 = "out") {
    B = barrier(K, b, eta, T-t)
    if (type1 == "down") {
        out = (S / B)^(1 - 2 * (r - eta) / sigma**2) * BSM_EU_call(B^2 / S, K, 0, 0, r, sigma, t, T)
        if (type2 == "out") {return(ind(S > B) * (BSM_EU_call(S, K, 0, 0, r, sigma, t, T) - out))}
        else {return(ind(S > B) * out)}
    } else if (type1 == "up") {
        out = (S / B)^(1 + 2 * (r - eta) / sigma**2) * BSM_EU_call(S * K / B, K, 0, 0, r, sigma, t, T)
        if (type2 == "out") {return(ind(S < B) * (BSM_EU_call(S, K, 0, 0, r, sigma, t, T) - out))}
        else {return(ind(S < B) * out)}
    }
}
BSM_EU_call_barrier_delta = function(S, K, b, eta, r, sigma, t=0, T, type1 = "down", type2 = "out", prices = NULL) { # models/means/stds are lists - one element for each training run to be included in mean pricing
    if (is.null(prices)) {
        prices = BSM_EU_call_barrier(S, K, b, eta, r, sigma, t=t, T, type1 = type1, type2 = type2)
    }
    S = S + 0.000001 * K
    prices_adj = BSM_EU_call_barrier(S, K, b, eta, r, sigma, t=t, T, type1 = type1, type2 = type2)
    return((prices_adj - prices) * 1000000 / K) # Returns price change for an increase of 1 in S
}

########################################################################
# Functions adapted from RFunctions.R as used for experiments in project

# Function to generate a simple path of values for S with other values constant
generate_data_ltd = function(target_fn, S0, K, b, eta, r, sigma, dummy, T, defaults, seed) {
    set.seed(seed = seed)
    S_all     = sde::GBM(x=S0, N=T*240, r = r, sigma = sigma, T = T)
    t         = seq(0, T, 1/240)
    tb        = tibble(S = S_all, t = t)
    tb$T      = rep(T, length(S_all))
    tb$K      = rep(K, length(S_all))
    tb$b      = rep(b, length(S_all))
    tb$eta    = rep(eta, length(S_all))
    tb$r      = rep(r, length(S_all))
    tb$sigma  = rep(sigma, length(S_all))
    tb$dummy  = rep(dummy, length(S_all))
    tb$T_t    = tb$T - tb$t
    tb$S_K    = tb$S / tb$K
    tb$B      = tb$b * tb$K * exp(-tb$eta * tb$T_t)
    return(tb)
}

# Function to return, for 1 simulation of underlying prices, the tracking error over time for both BS and ML models
tracking_path_ltd = function(target_fn, target_fn_d, testing, models, means, stds, K_test, T_test, b_test, eta_test, r_test, sigma_test, dummy_test, training_cols) { # models, means and stds are each lists with a number of elements equal to the number of training paths the model was trained with
    N                     = nrow(testing)
    testing_path          = testing %>% dplyr::group_by(t) %>% dplyr::summarise(S = max(S), K = K_test, T = T_test, b = b_test,
                                                                                eta=eta_test, r = r_test, sigma = sigma_test, dummy = dummy_test) # Get one entry for each date in chosen testing run
    testing_path$T_t      = testing_path$T-testing_path$t # Add time to maturity to match new T
    testing_path$S_K      = testing_path$S/testing_path$K # Add S/K to match new K
    testing_path$B        = barrier(testing_path$K, testing_path$b, testing_path$eta, testing_path$T_t)  
    testing_path$BS       = target_fn(testing_path$S, testing_path$K, testing_path$b, testing_path$eta, testing_path$r,
                                      testing_path$sigma, testing_path$t, testing_path$T) # Get BS option price for each day
    testing_path$BS_d     = target_fn_d(testing_path$S, testing_path$K, testing_path$b, testing_path$eta,
                                        testing_path$r, testing_path$sigma, testing_path$t, testing_path$T) # Get BS option delta for each day
    testing_path$ML       = ML_price(models, testing_path %>% dplyr::select(all_of(training_cols)), means, stds, testing_path$K)
    testing_path$ML_d     = ML_delta(models, testing_path %>% dplyr::select(all_of(training_cols)), means, stds, testing_path$K, testing_path$ML)
    for (row in 1:nrow(testing_path)) { # After down-and-out barrier is crossed, zero all subsequent BSM and ML values and deltas
        if (as.numeric(testing_path[row, which(colnames(testing_path) == 'S')]) <= as.numeric(testing_path[row, which(colnames(testing_path) == 'B')])) {
            testing_path[row:nrow(testing_path), which(colnames(testing_path) == 'BS')] = 0
            testing_path[row:nrow(testing_path), which(colnames(testing_path) == 'BS_d')] = 0            
            testing_path[row:nrow(testing_path), which(colnames(testing_path) == 'ML')] = 0
            testing_path[row:nrow(testing_path), which(colnames(testing_path) == 'ML_d')] = 0
        }
    }
    testing_path$V_c      = -testing_path$BS # Get Value of the call option in portfolio for both BS and ML model
    testing_path$V_s_BS   = testing_path$S * testing_path$BS_d # Get value of underlying in portfolio for BS
    testing_path$V_s_ML   = testing_path$S * testing_path$ML_d # Get value of underlying in portfolio for ML model
    # Loop through each date to update the BS and ML cash values, adding each new value to a vector
    for (row in 1:nrow(testing_path)) {
        if (row == 1) {
            V_c    = as.numeric(testing_path[row, which(colnames(testing_path) == 'V_c')])
            V_s_BS = as.numeric(testing_path[row, which(colnames(testing_path) == 'V_s_BS')])
            V_s_ML = as.numeric(testing_path[row, which(colnames(testing_path) == 'V_s_ML')])
            V_b_BS = - (V_c + V_s_BS) # At t=0, simply the cash paid for the option less the cash paid for the hedging in underlying
            V_b_ML = - (V_c + V_s_ML)
        }
        else {
            r_1    = as.numeric(testing_path[row - 1, which(colnames(testing_path) == 'r')])
            S      = as.numeric(testing_path[row, which(colnames(testing_path) == 'S')])
            BS_d   = as.numeric(testing_path[row, which(colnames(testing_path) == 'BS_d')])
            BS_d_1 = as.numeric(testing_path[row - 1, which(colnames(testing_path) == 'BS_d')])
            ML_d   = as.numeric(testing_path[row, which(colnames(testing_path) == 'ML_d')])
            ML_d_1 = as.numeric(testing_path[row - 1, which(colnames(testing_path) == 'ML_d')])
            V_b_BS = c(V_b_BS, exp(r_1 * (T_test / N)) * V_b_BS[row - 1] - S * (BS_d - BS_d_1)) # At t>0, previous day's cash + interest + change of hedge in underlying
            V_b_ML = c(V_b_ML, exp(r_1 * (T_test / N)) * V_b_ML[row - 1] - S * (ML_d - ML_d_1)) 
        }
    }
    testing_path$V_b_BS   = V_b_BS # Get value of hedging shares in portfolio for BS
    testing_path$V_b_ML   = V_b_ML# Get value of hedging shares in portfolio for ML model
    testing_path$V_BS     = testing_path$V_c + testing_path$V_s_BS + testing_path$V_b_BS # Get value of BS hedging portfolio (=> tracking error)
    testing_path$V_ML     = testing_path$V_c + testing_path$V_s_ML + testing_path$V_b_ML # Get value of ML model hedging portfolio (=> tracking error)
    return(testing_path)
}

###########################################
# Functions added for ShinyApp specifically

# Functions to get ML and BSM prices dependent on Exp No
MLPricePerExp = function(Exp, models, means, stds, S, K, T, r, v, b, eta, dummy) {
    B = barrier(K, b, eta, T)
    if (Exp == 1) {input_data = tibble(S_K=S/K, T_t=T)}
    else if (Exp == 2) {input_data = tibble(S_K=S/K, T_t=T, r=r, sigma=v)}
    else if (Exp == 3 | Exp == 4 | Exp == 5) {input_data = tibble(S_K=S/K, T_t=T, r=r, sigma=v, b=b, eta=eta)}
    else {input_data = tibble(S_K=S/K, T_t=T, r=r, sigma=v, b=b, eta=eta, dummy=dummy)}
    input_data = scale_data(input_data, means[[Exp]], stds[[Exp]])
    return(ind(S > B) * predict(models[[Exp]], input_data) * K)
}
BSMPricePerExp = function(Exp, S, K, T, r, v, b, eta, defaults) {
    if (Exp == 1) {return(BSM_EU_call(S, K, 0, 0, defaults$r, defaults$v, 0, T))}
    else if (Exp == 2) {return(BSM_EU_call(S, K, 0, 0, r, v, 0, T))}
    else {return(BSM_EU_call_barrier(S, K, b, eta, r, v, 0, T))}
}

# Function to plot the ML model results against the BSM model for a range of one of the inputs
PlotComps = function(Exp, type, models, means, stds, S, K, T, r, v, b, eta, dummy, defaults, isPercent=FALSE) {
    if (type=="S_K") {S = seq(K/2, K*2, length.out=1000); S_K=S/K; xs=S; xlab="Underlying Price"; main="Option vs Underlying"}
    else if (type=="T") {T = seq(1/240, 2.5, length.out=1000); S_K=S/K; xs=T; xlab="Time to Maturity"; main="Option vs Time to Maturity"}
    else if (type=="r") {r = seq(0.01, 0.12, length.out=1000); S_K=S/K; xs=r; xlab="Interest Rate (annual %)"; main="Option vs Interest Rate"}
    else if (type=="v") {v = seq(0.05, 0.5, length.out=1000); S_K=S/K; xs=v; xlab="Volatility (annual %)"; main="Option vs Volatility"}
    else if (type=="b") {b = seq(0.6, 0.99, length.out=1000); S_K=S/K; xs=b; xlab="Barrier (% of strike price at maturity)"; main="Option vs Barrier Level"}
    else if (type=="eta") {eta = seq(0, 0.3, length.out=1000); S_K=S/K; xs=eta; xlab="Barrier decay rate (annual %)"; main="Option vs Barrier Decay Rate"}
    else if (type=="dummy") {dummy = seq(1, 5, length.out=1000); S_K=S/K; xs=dummy; xlab="Dummy value in training inputs"; main="Option vs Dummy Value"}
    MLPrices = MLPricePerExp(Exp, models, means, stds, S, K, T, r, v, b, eta, dummy)
    BSMPrices = BSMPricePerExp(Exp, S, K, T, r, v, b, eta, defaults)
    if (type=="dummy") {BSMPrices = rep(BSMPrices, 1000)}
    plot(xs, MLPrices, type="l", col="red", xlab=xlab, ylab="Value of option",
         ylim=c(0,max(max(MLPrices),max(BSMPrices))), main=main, xaxt="n")
    ticks2 = axTicks(1); if (isPercent==TRUE) {ticks2 = ticks2*100}
    axis(1, at=axTicks(1), labels=ticks2)
    lines(xs, BSMPrices, col="blue")
    if (MLPrices[1000] > 0.5 * max(MLPrices)) {x="bottomright"} else {x="topright"}
    legend(x=x, legend=c("ML Model", "BSM Model"), col=c("red","blue"), lwd=c(1,1))
}

# Function to add a row with a numerical input on the left and a text on the right
GetNumericalInputRow = function(width1, width2, input, inputText, val, min, max, style, otherText) {
    return(fluidRow(
        column(width=width1, numericInput(input, inputText, val, min, max)),
        column(width=width2, p(style = style, otherText)
        )
    ))
}

# Function to create a tracking path for the ML or BSM model
GetTrackingPath = function(Exp, models, means, stds, S, K, b, eta, r, v, dummy, T, defaults, seed=1) {
    if (Exp == 1) {training_cols = c("S_K", "T_t"); r=defaults$r; v=defaults$v}
    else if (Exp == 2) {training_cols = c("S_K", "T_t", "r", "sigma")}
    else if (Exp == 3 | Exp == 4 | Exp == 5) {training_cols = c("S_K", "T_t", "r", "sigma", "b", "eta")}
    else {training_cols = c("S_K", "T_t", "r", "sigma", "b", "eta", "dummy")}
    if (Exp == 1 | Exp == 2) {target_fn = BSM_EU_call; target_fn_d = BSM_EU_call_delta}
    else {target_fn = BSM_EU_call_barrier; target_fn_d = BSM_EU_call_barrier_delta}
    ltd_path = generate_data_ltd(target_fn, S, K, b, eta, r, v, dummy, T, defaults, seed)
    return(tracking_path_ltd(target_fn, target_fn_d, ltd_path, list(models[[Exp]]), list(means[[Exp]]), list(stds[[Exp]]), K, T, b, eta, r, v, dummy, training_cols))
}

# Function to plot elements of a tracking path for the ML or BSM model
PlotTrackingElements = function(track_path, ML_BSM="ML") {
    if (ML_BSM=="ML") {data = track_path %>% select(S, t, K, Opt=ML, Cash=V_b_ML, Hedge=V_s_ML, TrackErr=V_ML)}
    else {data = track_path %>% select(S, t, K, Opt=BS, Cash=V_b_BS, Hedge=V_s_BS, TrackErr=V_BS)}
    par(mar = c(4, 4, 1, 2))
    plot(data$t, data$S, ylim=c(-180, 140), type="l", xlab="Time", ylab="Value", col="blue")
    lines(data$t, data$K, col="black")
    lines(data$t, -data$Opt, col="green")
    lines(data$t, data$Cash, col="orange")
    lines(data$t, data$Hedge, col="purple")
    lines(data$t, data$TrackErr, col="red")
    legend("bottomright", legend=c("Option", "Underlying", "Cash"),
           col=c("green", "orange", "purple"), lty="solid", cex=0.8, pt.cex = 1)
    legend("bottomleft", legend=c("Underlying", "Strike", "Portfolio (net)"),
           col=c("blue", "black", "red"), lty="solid", cex=0.8, pt.cex = 1)
    par(mar = c(5, 4, 4, 2))
}

# Function to plot the net hedged portfolio values resulting from using the ML or BSM model
PlotHedgeComps = function(track_path) {    
    par(mar = c(5, 4, 1, 2))
    plot(track_path$t, track_path$V_ML, ylim=c(-4, 3), type="l", xlab="Time", ylab="Value", col="blue")
    lines(track_path$t, track_path$V_BS, col="red")
    legend("bottomleft", legend=c("ML Portfolio (net)", "BSM Portfolio (net)"),
           col=c("blue", "red"), lty="solid", cex=0.8, pt.cex = 1)
    par(mar = c(5, 4, 4, 2))
}