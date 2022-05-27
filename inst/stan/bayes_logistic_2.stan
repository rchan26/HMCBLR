data {
  int<lower=0> nsamples;
  int<lower=0> p;
  int<lower=0, upper=1> y[nsamples];
  matrix[nsamples, p] X; // X does not include the intercept term in first column
  vector[p+1] prior_means;
  vector[p+1] prior_variances;
  int C;
}
parameters {
  vector[p+1] beta; // beta_{0} is intercept term
}
model {
  y ~ bernoulli_logit_glm(X, beta[1], beta[2:]);
  beta ~ normal(prior_means, sqrt(C*prior_variances));
}
