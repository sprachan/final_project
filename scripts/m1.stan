// The input data is a vector 'y' of length 'N'.
data {
  // data size
  int N;
  
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
  
}

// The parameters accepted by the model: 
// beta_wt, beta_TL, beta_WL, beta_0
parameters {
  
 real coeff_wt;
 real coeff_tl;
 real coeff_wl;
 real intercept;
 vector p[6];
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  coeff_wt ~ normal(0, 10);
  coeff_tl ~ normal(0, 10);
  coeff_wl ~ normal(0, 10);
  intercept ~ normal(0, 10);
  
  // number of behaviors = 6

  // for each individual, for each behavior, generate a p.
  for(i in 1:N){
    
      for(j in 1:6){
        logit_arg = exp(coeff_wt*weight[i]+coeff_tl*tarsus[i]+coeff_wl*wing[i]+intercept);
        p[j] = logit_arg/(1+logit_arg);
    
  }
    passive[i] ~ binomial(num_observations[i], p[1]);
    bite[i] ~ binomial(num_observations[i], p[2]);
    run_hide[i] ~ binomial(num_observations[i], p[3]);
    regurgitate[i] ~ binomial(num_observations[i], p[4]);
    vocalize[i] ~ binomial(num_observations[i], p[5]);
    kick[i] ~ binomial(num_observations[i], p[6]);
  }
  



