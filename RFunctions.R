# Convert a list to a string including the names of the list elements
list_to_string = function(l) {
    n = names(l)
    s = paste0(n[1], "=", l[1])
    for (e in 2:length(l)) {
        s = paste0(s, ", ", n[e], "=", l[e])
    }
    return(s)
}

# Functions to help pricing and greek functions later
# Standard call payoff on exercise
call_payoff = function(S, K) {
    payoff = S - K
    payoff[payoff<=0] = 0
    return(payoff)
}
ind  = function(cond) {ifelse(cond, 1, 0)}
d1   = function(s, r, sigma, T) {(log(s) + (r + sigma^2 / 2) * T) / sigma / sqrt(T)}
d2   = function(s, r, sigma, T) {(d1(s, r, sigma, T) - sigma * sqrt(T))}

# Formulae based on BSM dynamics for the underlying: Take vectors or single numerics as inputs and return a vector (or numeric if all inputs are single numerics)
# European call: (S=Underlying asset prices, K=Exercise price, r=Risk-free interest rate, sigma=Volatility of underlying asset price, t=Current time, T=Maturity time)
BSM_EU_call = function(S, K, b, eta, r, sigma, t=0, T) { # S and T must be same length vectors if require return vector
    return(S * pnorm(d1(S / K, r, sigma, T - t)) - (K * exp(-r * (T - t)) * pnorm(d2(S / K, r, sigma, T - t))))
}
BSM_EU_call_delta = function(S, K, b, eta, r, sigma, t=0, T){ # S and T must be same length vector if require return vector
    return(pnorm(d1(S / K, r, sigma, T - t)))
}

# European down-and-out barrier call:
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

# Function to generate N days of data, simple world with no weekends or holidays
# - Covering Tend years, each with N / Tend days
# - Constant risk-free interest rate, constant sigma
# - 1 element in each vector for each combination of N+1 days (t), Strikes (K), Barriers (B) and option maturities (T)
# - Removing all elements where T<(t+years_excl) as too close to maturity date causes exploding gradients (but not now when training with S/K)
# - Adding a target (BS/K column) from the target_fn acting on the other generated data plus a noise element relative to the exact BS price using noise_sigma
generate_data = function(target_fn, S0, N, Tend, K_range, b_range, eta_range, r_range, sigma_range, dummy_range, param_num, T, years_excl = 0, noise_sigma = 0) {
    S_all = sde::GBM(x=S0, N=N, r = mean(r_range), sigma = mean(sigma_range), T = Tend)
    t         = rep(seq(0,Tend,Tend/N),each = length(T) * param_num)
    tb        = tibble(t = t)
    tb$S      = rep(S_all, each = length(T) * param_num)
    tb$T      = rep(rep(T, length(S_all)), each = param_num)
    tb$K      = runif(length(t), K_range[1], K_range[2])
    tb$K      = tb$K * tb$S
    tb$b      = runif(length(t), b_range[1], b_range[2])
    tb$eta    = runif(length(t), eta_range[1], eta_range[2])
    tb$r      = runif(length(t), r_range[1], r_range[2])
    tb$sigma  = runif(length(t), sigma_range[1], sigma_range[2])
    tb$dummy  = runif(length(t), dummy_range[1], dummy_range[2])
    tb$T_t    = tb$T - tb$t
    tb$S_K    = tb$S / tb$K
    tb$B      = tb$b * tb$K * exp(-tb$eta * tb$T_t)
    tb        = tb[!tb$T_t <= years_excl,] # Remove where maturity date reached or in the past
    tb        = tb[!tb$S <= tb$B,] # Remove where underlying price less than or equal to down-and-out barrier    
    tb$BS_c   = target_fn(S = tb$S, K = tb$K, b = tb$b, eta = tb$eta, r = tb$r, sigma = tb$sigma, t = tb$t, T = tb$T)
    tb$BS_K_c = tb$BS_c / tb$K
    tb$noise  = rnorm(length(tb$S), mean = 0, sd = noise_sigma)
    tb$BS     = tb$BS_c + tb$noise * tb$BS_c
    tb$BS_K   = tb$BS / tb$K
    return(tb)
}

# Function to scale data: Take x as data.table & means/stds as vectors as inputs and return a matrix suitable for data input to a Keras model
scale_data = function(x, means, stds) {
    for (c in seq_along(x)) {
        x[[c]] = x[[c]] - means[c]
        x[[c]] = x[[c]] / stds[c]
    }
    return(as.matrix(x))
}

# Functions to build models
build_model_1 = function(ncols, units, L2_factor, activation, initializer, dropout, optimizer, momentum = 0.9) {
    mod = keras_model_sequential() %>%
               layer_dense(units = units[1], activation = activation, input_shape = ncols, kernel_regularizer = regularizer_l2(L2_factor),
                           kernel_initializer = initializer) %>%
               layer_dropout(rate = dropout) %>%
               layer_dense(units = 1)
    if (optimizer == "sgdm") {return(mod %>% compile(optimizer = optimizer_sgd(momentum = momentum), loss = "mse", metrics = c("mae")))}
    else {return(mod %>% compile(optimizer = optimizer, loss = "mse", metrics = c("mae")))}
}
build_model_2 = function(ncols, units, L2_factor, activation, initializer, dropout, optimizer, momentum = 0.9) {
    mod = keras_model_sequential() %>%
               layer_dense(units = units[1], activation = activation, input_shape = ncols, kernel_regularizer = regularizer_l2(L2_factor),
                           kernel_initializer = initializer) %>%
               layer_dropout(rate = dropout) %>%
               layer_dense(units = units[2], activation = activation, kernel_regularizer = regularizer_l2(L2_factor),
                           kernel_initializer = initializer) %>%
               layer_dropout(rate = dropout) %>%
               layer_dense(units = 1)
    if (optimizer == "sgdm") {return(mod %>% compile(optimizer = optimizer_sgd(momentum = momentum), loss = "mse", metrics = c("mae")))}
    else {return(mod %>% compile(optimizer = optimizer, loss = "mse", metrics = c("mae")))}
}
build_model_3 = function(ncols, units, L2_factor, activation, initializer, dropout, optimizer, momentum = 0.9) {
    mod = keras_model_sequential() %>%
               layer_dense(units = units[1], activation = activation, input_shape = ncols, kernel_regularizer = regularizer_l2(L2_factor),
                           kernel_initializer = initializer) %>%
               layer_dropout(rate = dropout) %>%
               layer_dense(units = units[2], activation = activation, kernel_regularizer = regularizer_l2(L2_factor),
                           kernel_initializer = initializer) %>%
               layer_dropout(rate = dropout) %>%
               layer_dense(units = units[3], activation = activation, kernel_regularizer = regularizer_l2(L2_factor),
                           kernel_initializer = initializer) %>%
               layer_dropout(rate = dropout) %>%
               layer_dense(units = 1)
    if (optimizer == "sgdm") {return(mod %>% compile(optimizer = optimizer_sgd(momentum = momentum), loss = "mse", metrics = c("mae")))}
    else {return(mod %>% compile(optimizer = optimizer, loss = "mse", metrics = c("mae")))}
}
build_model_4 = function(ncols, units, L2_factor, activation, initializer, dropout, optimizer, momentum = 0.9) {
    mod = keras_model_sequential() %>%
               layer_dense(units = units[1], activation = activation, input_shape = ncols, kernel_regularizer = regularizer_l2(L2_factor),
                           kernel_initializer = initializer) %>%
               layer_dropout(rate = dropout) %>%
               layer_dense(units = units[2], activation = activation, kernel_regularizer = regularizer_l2(L2_factor),
                           kernel_initializer = initializer) %>%
               layer_dropout(rate = dropout) %>%
               layer_dense(units = units[3], activation = activation, kernel_regularizer = regularizer_l2(L2_factor),
                           kernel_initializer = initializer) %>%
               layer_dropout(rate = dropout) %>%
               layer_dense(units = units[4], activation = activation, kernel_regularizer = regularizer_l2(L2_factor),
                           kernel_initializer = initializer) %>%
               layer_dropout(rate = dropout) %>%
               layer_dense(units = 1)
    if (optimizer == "sgdm") {return(mod %>% compile(optimizer = optimizer_sgd(momentum = momentum), loss = "mse", metrics = c("mae")))}
    else {return(mod %>% compile(optimizer = optimizer, loss = "mse", metrics = c("mae")))}
}
build_model_1b = function(ncols, units, L2_factor, activation, initializer, dropout, optimizer, momentum) {
    mod = keras_model_sequential() %>%
               layer_dense(units = units[1], activation = activation, input_shape = ncols, kernel_regularizer = regularizer_l2(L2_factor),
                           kernel_initializer = initializer) %>%
               layer_batch_normalization() %>%
               layer_dropout(rate = dropout) %>%
               layer_dense(units = 1)
    if (optimizer == "sgdm") {return(mod %>% compile(optimizer = optimizer_sgd(momentum = momentum), loss = "mse", metrics = c("mae")))}
    else {return(mod %>% compile(optimizer = optimizer, loss = "mse", metrics = c("mae")))}
}
build_model_1l = function(ncols, units, L2_factor, activation, initializer, dropout, optimizer, momentum) {
    mod = keras_model_sequential() %>%
               layer_dense(units = units[1], activation = activation, input_shape = ncols, kernel_regularizer = regularizer_l2(L2_factor),
                           kernel_initializer = initializer) %>%
               layer_dropout(rate = dropout) %>%
               layer_layer_normalization() %>%
               layer_dense(units = 1)
    if (optimizer == "sgdm") {return(mod %>% compile(optimizer = optimizer_sgd(momentum = momentum), loss = "mse", metrics = c("mae")))}
    else {return(mod %>% compile(optimizer = optimizer, loss = "mse", metrics = c("mae")))}
}
build_model_2b = function(ncols, units, L2_factor, activation, initializer, dropout, optimizer, momentum) {
    mod = keras_model_sequential() %>%
               layer_dense(units = units[1], activation = activation, input_shape = ncols, kernel_regularizer = regularizer_l2(L2_factor),
                           kernel_initializer = initializer) %>%
               layer_batch_normalization() %>%
               layer_dropout(rate = dropout) %>%
               layer_dense(units = units[2], activation = activation, kernel_regularizer = regularizer_l2(L2_factor),
                           kernel_initializer = initializer) %>%
               layer_batch_normalization() %>%
               layer_dropout(rate = dropout) %>%
               layer_dense(units = 1)
    if (optimizer == "sgdm") {return(mod %>% compile(optimizer = optimizer_sgd(momentum = momentum), loss = "mse", metrics = c("mae")))}
    else {return(mod %>% compile(optimizer = optimizer, loss = "mse", metrics = c("mae")))}
}
build_model_2l = function(ncols, units, L2_factor, activation, initializer, dropout, optimizer, momentum) {
    mod = keras_model_sequential() %>%
               layer_dense(units = units[1], activation = activation, input_shape = ncols, kernel_regularizer = regularizer_l2(L2_factor),
                           kernel_initializer = initializer) %>%
               layer_layer_normalization() %>%
               layer_dropout(rate = dropout) %>%
               layer_dense(units = units[2], activation = activation, kernel_regularizer = regularizer_l2(L2_factor),
                           kernel_initializer = initializer) %>%
               layer_layer_normalization() %>%
               layer_dropout(rate = dropout) %>%
               layer_dense(units = 1)
    if (optimizer == "sgdm") {return(mod %>% compile(optimizer = optimizer_sgd(momentum = momentum), loss = "mse", metrics = c("mae")))}
    else {return(mod %>% compile(optimizer = optimizer, loss = "mse", metrics = c("mae")))}
}

# Function to return list of multiple simulated paths from generate_data() together with lists of means and sds of the S/K and T-t values
sim_paths = function(target_fn, S0, N, Tend, K_range, b_range, eta_range, r_range, sigma_range, dummy_range, param_num, T, excl, num_paths, noise_sigma = 0) {
    training = list()
    for (i in 1:num_paths) {
        set.seed(i)
        training[[i]] = generate_data(target_fn, S0, N, Tend, K_range, b_range, eta_range, r_range, sigma_range, dummy_range, param_num, T, excl, noise_sigma)
    }
    return(training)
}

# Functions to return a vector of prices or greeks using the average of the results of a list of ML models, model_inputs is a data.table with only the required inputs to the model and K is a single numeric strike price
# Assumes that the model has been trained on a price / exercise price basis so the result needs to be multiplied by the exercise price
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
ML_rho = function(models, model_inputs, means, stds, K, prices = NULL) { # models/means/stds are lists - one element for each training run to be included in mean pricing
    if (is.null(prices)) {
        prices = ML_price(models, model_inputs, means, stds, K)
    }
    model_inputs$r = model_inputs$r + 0.000001
    prices_adj = ML_price(models, model_inputs, means, stds, K)
    return((prices_adj - prices) * 10000) # Returns price change for an increase of 0.01 (or 1%) in r
}
ML_vega = function(models, model_inputs, means, stds, K, prices = NULL) { # models/means/stds are lists - one element for each training run to be included in mean pricing
    if (is.null(prices)) {
        prices = ML_price(models, model_inputs, means, stds, K)
    }
    model_inputs$sigma = model_inputs$sigma + 0.0001
    prices_adj = ML_price(models, model_inputs, means, stds, K)
    return((prices_adj - prices) * 10000) # Returns price change for an increase of 0.01 (or 1%) in sigma
}

# Function to test results
test_acc = function(models, test, means, stds, training_cols) {
    results           = NULL
    t0                = proc.time()
    results$pred      = ML_price(models, test %>% dplyr::select(all_of(training_cols)), means, stds, test$K)
    t1                = as.numeric((proc.time()-t0)[1])
    results$r         = test$r
    results$sigma     = test$sigma
    results$K         = test$K
    results$true      = test$BS
    results$T_t       = test$T_t
    results$S_K       = test$S_K
    results$b         = test$b
    results$eta       = test$eta    
    results$BS_K      = test$BS_K
    results$err       = results$pred - results$true
    results$abs_err   = abs(results$err)
    results$mod_err   = results$err / results$K
    return(list(data.frame(results), t1))
}

# Function to build multiple models on multiple training paths
#build_models = function(hidden_layers, training, training_x, means, stds, params) { # Remove training_x once working. means and stds added
build_models = function(hidden_layers, training, training2, params, training_cols) {
    models                = list()
    learns                = list()
    results_layers        = rep(hidden_layers, each=length(training) * length(params))
    results_trains        = rep(rep(1:length(training)), length(hidden_layers) * length(params))
    results_n             = c()
    results_split         = c()
    results_mean_err      = c()
    results_sd_err        = c()
    results_R_2           = c()
    results_t_train       = c()
    results_t_test        = c()
    results_val_loss      = c()
    results_all_loss      = c()
    results_test_loss     = c()
    results_param_set     = c()
    results_params        = c()
    training_x            = list()
    means                 = list()
    stds                  = list()
    results_detail        = list()
    for (i in 1:length(training)) {
        training_x[[i]] = training[[i]] %>% dplyr::select(all_of(training_cols))
        means[[i]]      = colMeans(training_x[[i]])
        stds[[i]]       = apply(training_x[[i]], 2, sd)
        training_x[[i]] = scale_data(training_x[[i]], means[[i]], stds[[i]])
    }
    for (m in 1:length(hidden_layers)) {
        models[[m]] = list()
        learns[[m]] = list()
        results_detail[[m]] = list()
        for (p in 1:length(params)) {
            models[[m]][[p]] = list()
            learns[[m]][[p]] = list()
            results_detail[[m]][[p]] = list()
            epochs           = params[[p]]$epochs
            batch_size       = params[[p]]$batch_size
            units            = params[[p]]$units
            L2_factor        = params[[p]]$L2_factor
            optimizer        = params[[p]]$optimizer
            momentum         = params[[p]]$momentum
            activation       = params[[p]]$activation
            initializer      = params[[p]]$initializer
            dropout          = params[[p]]$dropout
            for (i in 1:length(training)) {
                print(paste0("m=",m,", p=",p,", i=",i,", params: ",list_to_string(params[[p]])))
                set_random_seed(i)
                models[[m]][[p]][[i]] = get(paste0("build_model_", hidden_layers[m]))(ncol(training_x[[i]]), units, L2_factor, activation, initializer, dropout, optimizer, momentum) # Build model
                t_b               = proc.time() #t0=Sys.time()
                learns[[m]][[p]][[i]] = models[[m]][[p]][[i]] %>% # Fit model to training data
                    fit(x = training_x[[i]], y = training[[i]]$BS_K, epochs = epochs, batch_size = batch_size, validation_split = .3, verbose = TRUE,)
                t_e               = proc.time()
                t1                = as.numeric((t_e-t_b)[1]) #print(difftime(Sys.time(), t0, units = "secs"))
                temp              = test_acc(list(models[[m]][[p]][[i]]), training[[i]], list(means[[i]]), list(stds[[i]]), training_cols)
                results           = temp[[1]] # Get test measures of model against training data
                results_detail[[m]][[p]][[i]] = results
                results_n         = c(results_n, dim(training_x[[i]])[1])
                results_split     = c(results_split, .3)
                results_mean_err  = c(results_mean_err, mean(results$err))
                results_sd_err    = c(results_sd_err, sd(results$err))
                results_R_2       = c(results_R_2, cor(results$true, results$pred)^2)
                results_t_train   = c(results_t_train, t1)
                results_t_test    = c(results_t_test, temp[[2]])
                results_val_loss  = c(results_val_loss, learns[[m]][[p]][[i]]$metrics$val_loss[epochs])
                results_all_loss  = c(results_all_loss, mean(results$mod_err^2))
                results_param_set = c(results_param_set, p)
                results_params    = c(results_params, list_to_string(params[[p]]))
            }
            for (i in 1:length(training)) { # Loop through each trained model and check losses on testing sets of input data (training2 to differentiate from the later testing processes)
                loss = 0
                for (j in 1: length(training2)) {
                    temp              = test_acc(list(models[[m]][[p]][[i]]), training2[[j]], list(means[[i]]), list(stds[[i]]), training_cols)
                    results           = temp[[1]]
                    loss = loss + mean(results$mod_err^2)
                }
                results_test_loss  = c(results_test_loss, loss / length(training2))
            }
        }
    }
    results_tbl       = tibble(layers = results_layers, param_set = results_param_set, train_run = results_trains,
                         num_trains = results_n, split = results_split, mean_err = results_mean_err,
                         mean_sd = results_sd_err, R_squared = results_R_2, train_time = results_t_train,
                         test_time = results_t_test, val_loss = results_val_loss, all_loss = results_all_loss,
                         test_loss = results_test_loss, params = results_params)
    results_per_model = results_tbl %>% dplyr::group_by(layers, param_set) %>%
        dplyr::summarise(num_i = length(training), best_i = train_run[which.min(test_loss)], num_trains = mean(num_trains),
                         split = mean(split), mean_err = mean(mean_err), mean_sd = mean(mean_sd), R_squared = mean(R_squared),
                         sum_train_time = sum(train_time), mean_test_time = mean(test_time), mean_val_loss = mean(val_loss),
                         mean_all_loss = mean(all_loss), mean_test_loss = mean(test_loss), min_test_loss = min(test_loss),
                         params = max(params)) 
    return(list(models, learns, means, stds, results_tbl, results_per_model, results_detail))
}

# Function to choose values from a tibble given 2 unique row identifiers and the column name
choose_results_train_2 = function(results_train, m, p, col_name) {
    result = results_train %>% dplyr::filter(layers == m, param_set == p)
    return(result %>% dplyr::pull(col_name))
}

# Function to choose values from a tibble given 3 unique row identifiers and the column name
choose_results_train_3 = function(results_train, m, p, i, col_name) {
    result = results_train %>% dplyr::filter(layers == m, param_set == p, train_run == i)
    return(result %>% dplyr::pull(col_name))
}

# Function to return, for 1 simulation of underlying prices, the tracking error over time for both BS and ML models
tracking_path = function(target_fn, target_fn_d, testing, models, means, stds, K_test, T_test, b_test, eta_test, r_test, sigma_test, dummy_test, training_cols) { # models, means and stds are each lists with a number of elements equal to the number of training paths the model was trained with
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

# Function to run test_acc for each model for each testing simulation and tracking_path for each set of option terms in addition and return the results
test_results = function(target_fn, target_fn_d, models, layers, params, results_per_model_train, testing, means, stds, opt_terms, training_cols, ensemble_type = "mean") {
    num_models         = length(models)
    num_params         = length(params)
    num_trains         = length(means)
    num_tests          = length(testing)
    num_terms          = length(opt_terms)
    testing_paths      = list()
    results_detail     = list()
    results_layers     = rep(layers, each = num_params * num_tests * num_terms)
    results_num_trains = rep(num_trains, num_models * num_params * num_tests * num_terms)
    results_param_sets = rep(rep(1:num_params, each = num_tests * num_terms), num_models)
    results_tests      = rep(rep(1:num_tests, each = num_terms), num_models * num_params)
    results_terms      = c()
    results_num_i      = c()
    results_num_j      = c()    
    results_num_tests  = c()
    results_val_loss   = c()
    results_train_time = c()
    results_mean_err   = c()
    results_sd_err     = c()
    results_R_2        = c()
    results_test_loss  = c()
    results_t          = c()
    results_BS_err     = c()
    results_ML_err     = c()
    results_params     = c()
    for (m in 1:num_models) {
        testing_paths[[m]]  = list()
        results_detail[[m]] = list()
        for (p in 1:num_params) {
            testing_paths[[m]][[p]]  = list()
            results_detail[[m]][[p]] = list()
            for (j in 1:num_tests) {
                i       = choose_results_train_2(results_per_model_train, layers[m], p, "best_i")
                r       = mean(testing[[j]]$r)
                sigma   = mean(testing[[j]]$sigma)
                dummy   = mean(testing[[j]]$dummy)
                testing_paths[[m]][[p]][[j]] = list()
                if (ensemble_type == "mean") {
                    results_tmp = test_acc(models[[m]][[p]], testing[[j]], means, stds, training_cols)
                } else {
                    results_tmp = test_acc(list(models[[m]][[p]][[i]]), testing[[j]], list(means[[i]]), list(stds[[i]]), training_cols)
                }
                results = results_tmp[[1]]
                results_detail[[m]][[p]][[j]] = results
                for (k in 1:num_terms) {
                    print(paste0("m=",m,", p=",p,", j=",j,", k=",k))
                    K                  = opt_terms[[k]]$K
                    T                  = opt_terms[[k]]$T
                    b                  = opt_terms[[k]]$b
                    eta                = opt_terms[[k]]$eta
                    results_terms = c(results_terms, list_to_string(opt_terms[[k]]))
                    results_num_i      = c(results_num_i, num_trains)
                    results_num_j      = c(results_num_j, num_tests)
                    results_num_tests  = c(results_num_tests, length(testing[[1]]$t))
                    if (ensemble_type == "mean") {
                        testing_path = tracking_path(target_fn, target_fn_d, testing[[j]], models[[m]][[p]], means, stds, K, T, b, eta, r, sigma, dummy, training_cols)
                    } else {
                        testing_path = tracking_path(target_fn, target_fn_d, testing[[j]], list(models[[m]][[p]][[i]]), list(means[[i]]), list(stds[[i]]), K, T, b, eta, r, sigma, dummy, training_cols)
                    }
                    testing_paths[[m]][[p]][[j]][[k]] = testing_path
                    len = length(testing_paths[[m]][[p]][[j]][[k]]$t)
                    results_mean_err   = c(results_mean_err, mean(results$err))
                    results_sd_err     = c(results_sd_err, sd(results$err))
                    results_R_2        = c(results_R_2, cor(results$true, results$pred)^2)
                    results_test_loss  = c(results_test_loss, mean(results$mod_err^2))
                    if (ensemble_type == "mean") {
                        results_val_loss   = c(results_val_loss, choose_results_train_2(results_per_model_train, layers[m], p, "mean_test_loss"))
                        results_train_time = c(results_train_time, choose_results_train_2(results_per_model_train, layers[m], p, "sum_train_time"))
                    } else {
                        results_val_loss   = c(results_val_loss, choose_results_train_2(results_per_model_train, layers[m], p, "min_test_loss"))
                        results_train_time = c(results_train_time, choose_results_train_2(results_per_model_train, layers[m], p, "sum_train_time"))                        
                    }
                    results_t          = c(results_t, results_tmp[[2]])
                    results_BS_err     = c(results_BS_err, testing_paths[[m]][[p]][[j]][[k]]$V_BS[len])
                    results_ML_err     = c(results_ML_err, testing_paths[[m]][[p]][[j]][[k]]$V_ML[len])
                    results_params     = c(results_params, list_to_string(params[[p]]))
                }
            }
        }
    }
    results_tbl = tibble(layer = results_layers, param_set = results_param_sets, test_run = results_tests, terms = results_terms,
                         num_i = results_num_i, num_j = results_num_j, num_tests = results_num_tests, val_loss = results_val_loss,
                         train_time = results_train_time, err = results_mean_err, sd = results_sd_err, R_squared = results_R_2,
                         test_loss = results_test_loss, test_time = results_t, BS_track_err = abs(results_BS_err),
                         ML_track_err = abs(results_ML_err), ML_BS_track_err = ML_track_err - BS_track_err, params = results_params)
    results_per_K     = results_tbl %>% dplyr::group_by(layer, param_set, terms) %>%
        dplyr::summarise(num_i = mean(num_i), num_tests = mean(num_tests), num_j = mean(num_j), val_loss = mean(val_loss),
                         mean_train_time = mean(train_time), mean_err = mean(err), mean_sd = mean(sd), mean_R_squared = mean(R_squared),
                         mean_test_loss = mean(test_loss), mean_test_time = mean(test_time), BS_track_err = mean(BS_track_err),
                         ML_track_err = mean(ML_track_err), ML_BS_track_err = mean(ML_BS_track_err), params = max(params))
    results_per_model = results_tbl %>% dplyr::group_by(layer, param_set) %>%
        dplyr::summarise(num_i = mean(num_i), num_j = mean(num_j), num_tests = mean(num_tests), val_loss = mean(val_loss), 
                         mean_train_time = mean(train_time), mean_err = mean(err), mean_sd = mean(sd), mean_R_squared = mean(R_squared),
                         mean_test_loss = mean(test_loss), mean_test_time = mean(test_time), BS_track_err = mean(BS_track_err),
                         ML_track_err = mean(ML_track_err), ML_BS_track_err = mean(ML_BS_track_err), params = max(params))
    return(list(results_per_model, results_per_K, results_tbl, testing_paths, results_detail))
}

# Function to save all of the models in the nested list of models
save_models = function(models, filepath, exp_no) {
    for (m in 1:length(models)) {
        for (p in 1:length(models[[m]])) {
            for (i in 1:length(models[[m]][[p]])) {
                save_model_tf(models[[m]][[p]][[i]], paste0(filepath,"models/", exp_no, "/", m, "/", p, "/", i))
            }
        }
    }
}

# Function to reload all of the models in the nested list of models
load_models = function(filepath, exp_no) {
    result = list()
    dirs_m = list.dirs(paste0(filepath,"models/", exp_no), recursive = FALSE)
    for (m in dirs_m) {
        m = substr(m, regexpr("/[^/]*$", m) + 1, nchar(m))
        result[[m]] = list()
        dirs_p = list.dirs(paste0(filepath,"models/", exp_no, "/", m), recursive = FALSE)   
        for (p in dirs_p) {
            p = substr(p, regexpr("/[^/]*$", p) + 1, nchar(p))
            result[[m]][[p]] = list()
            dirs_i = list.dirs(paste0(filepath,"models/", exp_no, "/", m, "/", p), recursive = FALSE)
            for (i in dirs_i) {
                i = substr(i, regexpr("/[^/]*$", i) + 1, nchar(i))
                result[[m]][[p]][[i]] = load_model_tf(paste0(filepath,"models/", exp_no, "/", m, "/", p, "/", i))
            }
        }
    }
    return(result)
}