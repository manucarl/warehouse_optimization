// STAN CODE
data {
  int N; // number of observations
  int K; // number of predictors + intercept
  matrix[N, K] x; // matrix of predictors
  vector[N] y; // y vector
}

parameters {
  vector[K] beta; // intercept and slope parameters
  real<lower=0> sigma; // constrained to be positive
}

model {
  vector[N] mu; // declaring a mu vector
  
  // priors
  beta ~ normal(0, 1); // declares the same prior for intercept and slope
  sigma ~ exponential(1); // using an exponential prior on sigma
  
  // likelihood
  for(i in 1:N) {
    mu[i] = x[i] * beta; // * is matrix multiplication in this context
  }
  
  y ~ normal(mu, sigma);
}