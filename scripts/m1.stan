// The input data is a vector 'y' of length 'N'.
data {
  // data size
  int N; //number of observations
  int B; //number of behaviors
  
  // individual ID and physical characteristics
  int num_observations[N];
  int band[N];
  real tarsus[N];
  real weight[N];
  real wing[N];
  int sex[N];
  int experience[N];
  
  // effects of the observation process
  real exttime[N];
  
  // behavioral response
  int passive[N];
  int bite[N];
  int run_hide[N];
  int regurgitate[N];
  int vocalize[N];
  int kick[N];
  
}

// The parameters accepted by the model: 
// beta_wt, beta_TL, beta_WL, beta_0
parameters {
  
 real coeff_wt[B];
 real coeff_tl[B];
 real coeff_wl[B];
 real beta0[B];
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  coeff_wt ~ normal(0, 10);
  coeff_tl ~ normal(0, 10); 
  coeff_wl ~ normal(0, 10);
  beta0 ~ normal(0, 10);
  

  // for each individual, for each behavior, generate a p.
  for(i in 1:N){
    bite[i] ~ bernoulli_logit(coeff_wt[1]*weight[i]+coeff_tl[1]*tarsus[i]+coeff_wl[1]*wing[i]+beta0[1])
    run_hide[i] ~ bernoulli_logit(coeff_wt[2]*weight[i]+coeff_tl[2]*tarsus[i]+coeff_wl[2]*wing[i]+beta0[2])
    regurgitate[i] ~ bernoulli_logit(coeff_wt[3]*weight[i]+coeff_tl[3]*tarsus[i]+coeff_wl[3]*wing[i]+beta0[3])
    vocalize[i] ~ bernoulli_logit(coeff_wt[4]*weight[i]+coeff_tl[4]*tarsus[i]+coeff_wl[4]*wing[i]+beta0[4])
    kick[i] ~ bernoulli_logit(coeff_wt[5]*weight[i]+coeff_tl[5]*tarsus[i]+coeff_wl[5]*wing[i]+beta0[5])
  }
}


  



