// The input data is a vector 'y' of length 'N'.
data {
  // data size
  int N; //number of observations
  int B; //number of behaviors we are modeling (NB: DIFFERENT FROM M1)
  
  // individual ID and physical characteristics
  // int num_observations[N];
  // int band[N];
  real tarsus[N];
  real weight[N];
  real wing[N];
  int sex[N];
  int experience[N];
  
  // effects of the observation process
  // real exttime[N];
  
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
 // bite and run
 real coeff_wt_br[B];
 real coeff_tl_br[B];
 real coeff_wl_br[B];
 real beta0_br[B];
 // only bite
 real coeff_wt_b[B];
 real coeff_tl_b[B];
 real coeff_wl_b[B];
 real beta0_b[B];
 // only run
 real coeff_wt_r[B];
 real coeff_tl_r[B];
 real coeff_wl_r[B];
 real beta0_r[B];
 // neither bite nor run
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
    if (bite[i] == 1)
      if (run_hide[i] == 1) // bite and run
        regurgitate[i] ~ bernoulli_logit(coeff_wt_br[1]*weight[i]+coeff_tl_br[1]*tarsus[i]+coeff_wl_br[1]*wing[i]+beta0_br[1]);
        vocalize[i] ~ bernoulli_logit(coeff_wt_br[2]*weight[i]+coeff_tl_br[2]*tarsus[i]+coeff_wl_br[2]*wing[i]+beta0_br[2]);
        kick[i] ~ bernoulli_logit(coeff_wt_br[3]*weight[i]+coeff_tl_br[3]*tarsus[i]+coeff_wl_br[3]*wing[i]+beta0_br[3]);
      else // bite and NOT run 
        regurgitate[i] ~ bernoulli_logit(coeff_wt_b[1]*weight[i]+coeff_tl_b[1]*tarsus[i]+coeff_wl_b[1]*wing[i]+beta0_b[1]);
        vocalize[i] ~ bernoulli_logit(coeff_wt_b[2]*weight[i]+coeff_tl_b[2]*tarsus[i]+coeff_wl_b[2]*wing[i]+beta0_b[2]);
        kick[i] ~ bernoulli_logit(coeff_wt_b[3]*weight[i]+coeff_tl_b[3]*tarsus[i]+coeff_wl_b[3]*wing[i]+beta0_b[3]);
    else
      if (run_hide[i] == 1) // NOT bite and run
        regurgitate[i] ~ bernoulli_logit(coeff_wt_r[1]*weight[i]+coeff_tl_r[1]*tarsus[i]+coeff_wl_r[1]*wing[i]+beta0_r[1]);
        vocalize[i] ~ bernoulli_logit(coeff_wt_r[2]*weight[i]+coeff_tl_r[2]*tarsus[i]+coeff_wl_r[2]*wing[i]+beta0_r[2]);
        kick[i] ~ bernoulli_logit(coeff_wt_r[3]*weight[i]+coeff_tl_r[3]*tarsus[i]+coeff_wl_r[3]*wing[i]+beta0_r[3]);
      else // NOT bite and NOT run 
        regurgitate[i] ~ bernoulli_logit(coeff_wt[1]*weight[i]+coeff_tl[1]*tarsus[i]+coeff_wl[1]*wing[i]+beta0[1]);
        vocalize[i] ~ bernoulli_logit(coeff_wt[2]*weight[i]+coeff_tl[2]*tarsus[i]+coeff_wl[2]*wing[i]+beta0[2]);
        kick[i] ~ bernoulli_logit(coeff_wt[3]*weight[i]+coeff_tl[3]*tarsus[i]+coeff_wl[3]*wing[i]+beta0[3]);
  }
}


  



