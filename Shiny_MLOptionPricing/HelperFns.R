# Function to scale data: Take x as data.table & means/stds as vectors as inputs and return a matrix suitable for data input to a Keras model
scale_data = function(x, means, stds) {
    for (c in seq_along(x)) {
        x[[c]] = x[[c]] - means[c]
        x[[c]] = x[[c]] / stds[c]
    }
    return(as.matrix(x))
}

ML_price = function(models, model_inputs, means, stds, K) { # models/means/stds are lists - one element for each training run to be included in mean pricing
    len   = length(models)
    price = rep(0, dim(model_inputs)[1])
    for (i in 1:len) {
        model_inputs_tmp = scale_data(model_inputs, means[[i]], stds[[i]])
        price = price + as.vector(models[[i]] %>% predict(model_inputs_tmp) * K)
    }
    return(price/len)
}

call_payoff = function(S, K) {
    payoff = S - K
    payoff[payoff<=0] = 0
    return(payoff)
}
ind  = function(cond) {ifelse(cond, 1, 0)}
d1   = function(s, r, sigma, T) {(log(s) + (r + sigma^2 / 2) * T) / sigma / sqrt(T)}
d2   = function(s, r, sigma, T) {(d1(s, r, sigma, T) - sigma * sqrt(T))}

BSM_EU_call = function(S, K, b, eta, r, sigma, t=0, T) { # S and T must be same length vectors if require return vector
    return(S * pnorm(d1(S / K, r, sigma, T - t)) - (K * exp(-r * (T - t)) * pnorm(d2(S / K, r, sigma, T - t))))
}

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