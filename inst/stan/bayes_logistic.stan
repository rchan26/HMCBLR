data {
  int<lower=0> nsamples;
  int<lower=0> p;
  vector<lower=0, upper=1>[nsamples] y;
  // int<lower=0, upper=1> y[nsamples];
  matrix[nsamples, (p+1)] X; // X includes the intercept term in first column
  vector<lower=0>[nsamples] count;
  // int<lower=0> count[nsamples];
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
  target += power * sum((count).*(((X*beta).*(y))-log(1+exp(X*beta))));
  // for (i in 1:nsamples)
  //   target += count[i] * bernoulli_logit_lpmf(y[i] | row(X,i)*beta);
}
