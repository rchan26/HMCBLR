data {
  int<lower=0> nsamples;
  int<lower=0> p;
  int<lower=0, upper=1> y[nsamples];
  matrix[nsamples, (p+1)] X; // X includes the intercept term in first column
  vector[p+1] prior_means;
  vector[p+1] prior_variances;
  int C;
  real power;
}
parameters {
  vector[p+1] beta; // beta_{0} is intercept term
}
model {
  target += power * normal_lpdf(beta | prior_means, sqrt(C*prior_variances));
  target += power * bernoulli_logit_lpmf(y | X*beta);
}
