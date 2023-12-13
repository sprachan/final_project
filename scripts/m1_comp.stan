// The input data is a vector 'y' of length 'N'.
data {
  // data size
  int N; // number of rows in the data (number of observations of all the data)
  int B; //number of behaviors
  
  // individual ID and physical characteristics
  //int num_observations[N];
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
 real coeff_tl[B];
 real coeff_experience[B];
 real beta0[B];
}

// The model to be estimated. 
model {
  coeff_tl ~ normal(0, 10); 
  beta0 ~ normal(0, 10);
  // for each individual, for each behavior, generate a p.
  for(i in 1:N){
    bite[i] ~ bernoulli_logit(coeff_tl[1]*tarsus[i]+coeff_experience[1]*experience[i]+beta0[1]);
    run_hide[i] ~ bernoulli_logit(coeff_tl[2]*tarsus[i]+coeff_experience[2]*experience[i]+beta0[2]);
    regurgitate[i] ~ bernoulli_logit(coeff_tl[3]*tarsus[i]+coeff_experience[3]*experience[i]+beta0[3]);
    vocalize[i] ~ bernoulli_logit(coeff_tl[4]*tarsus[i]+coeff_experience[4]*experience[i]+beta0[4]);
    kick[i] ~ bernoulli_logit(coeff_tl[5]*tarsus[i]+coeff_experience[5]*experience[i]+beta0[5]);
  }
}

generated quantities{
  // quantity we want to generate
  matrix[N,B] p;
  for(n in 1:N){
    for(b in 1:B){
      p[n,b] = exp(coeff_tl[b]*tarsus[n]+coeff_experience[b]*experience[n]+beta0[b])/(1+exp(coeff_tl[b]*tarsus[n]+coeff_experience[b]*experience[n]+beta0[b]));
    }
  }
}
  



