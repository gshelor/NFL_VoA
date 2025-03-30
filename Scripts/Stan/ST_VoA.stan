//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N; // Number of teams
  vector[N] net_st_ppg; // special teams ppg, used as basis for special teams VoA rating
  vector[N] net_st_epa; // special teams EPA
  vector[N] net_kick_return_avg; // net yards per kick return
  vector[N] net_punt_return_avg; // net yards per punt return
  // vector[N] net_kick_return_TDs; // net kick return TDs per game
  // vector[N] net_punt_return_TDs; // net punt return TDs per game
  vector[N] net_fg_rate; // net field goal conversion rate
  // vector[N] net_fg_made_pg; // number of field goals made per game
  vector[N] net_xp_rate; // net extra point conversion rate
  // vector[N] net_xpts_pg; // number of extra points made per game
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real b0; // intercept
  real beta_net_st_epa; // coefficient for special teams EPA
  real beta_net_kick_return_avg; // coefficient for yards per kick return
  real beta_net_punt_return_avg; // coefficient for yards per punt return
  // real beta_net_kick_return_TDs; // coefficient for net kick return TDs per game
  // real beta_net_punt_return_TDs; // coefficient for net punt return TDs per game
  real beta_net_fg_rate; // coefficient for field goal conversion rate
  // real beta_net_fg_made_pg; // coefficient for number of field goals made per game
  real beta_net_xp_rate; // coefficient for extra point conversion rate
  // real beta_net_xpts_pg; // coefficient for number of extra points made per game
  real sigma; // Standard deviation of the normal distribution
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  // Define linear predictor directly in the model block
  net_st_ppg ~ normal(b0 + beta_net_st_epa * net_st_epa + beta_net_kick_return_avg * net_kick_return_avg + beta_net_punt_return_avg * net_punt_return_avg + beta_net_fg_rate * net_fg_rate + beta_net_xp_rate * net_xp_rate, sigma);
}

