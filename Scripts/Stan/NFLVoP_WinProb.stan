//
// This Stan program defines a simple win probability model, with a
// vector of values 'win_prob' modeled on a beta distribution with shape 'alpha'
// and rate 'beta'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector of length 'N'.
data {
  int<lower=0> N;
  vector[N] win_prob;
  vector[N] win_margin;
}

// The parameters accepted by the model.
parameters {
  real b0;
  real b1;
  real<lower=0> sigma;
}


transformed parameters {
  real<lower=0> mu [N];
  real<lower=0> alpha [N];
  real<lower=0> beta_param [N];
  for (i in 1:N){
    mu[i] = b0 + b1*win_margin[i];
    alpha[i] = (mu[i]^2 - mu[i]^3 - (mu[i]*sigma^2)) / sigma^2;
    beta_param[i] = (mu[i] - (2*mu[i]^2) + mu[i]^3 - sigma^2 + (mu[i] * sigma^2)) / sigma^2;
  }
}

// The model to be estimated. We model the output
// 'y' to be distributed on the beta distribution with parameters 'alpha' and 'beta'.
model {
  // linear predictor
  // vector[N] eta;
  // eta = b0 + b1 * win_margin;
  // likelihood
  // win_prob ~ normal(b0 + b1 * win_margin, sigma) T[0,1];
  win_prob ~ beta(alpha, beta_param);
}

