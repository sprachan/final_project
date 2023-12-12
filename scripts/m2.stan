// The input data is a vector 'y' of length 'N'.
data {
  // data size
  int N; //number of observations
  int B; //number of behaviors we are modeling (NB: DIFFERENT FROM M1)
  
  // individual ID and physical characteristics
  // int num_observations[N];
  // int band[N];
  // real tarsus[N];
  // real weight[N];
  // real wing[N];
  // int sex[N];
  // int experience[N];
  
  // effects of the observation process
  // real exttime[N];
  
  // behavioral response
  // int passive[N];
  int bite[N];
  int run_hide[N];
  int regurgitate[N];
  int vocalize[N];
  int kick[N];
  
}

// The parameters accepted by the model: 
// beta_wt, beta_TL, beta_WL, beta_0
parameters {
 real <lower=0, upper=1> p_br[B];
 real <lower=0, upper=1> p_b[B];
 real <lower=0, upper=1> p_r[B];
 real <lower=0, upper=1> p[B];
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  p_br ~ beta(1, 1);
  p_b ~ beta(1, 1);
  p_r ~ beta(1, 1);
  p ~ beta(1, 1);
  
  // for each individual, for each behavior.
  for(i in 1:N){
    if (bite[i] == 1) {
      if (run_hide[i] == 1) { // bite and run
        regurgitate[i] ~ bernoulli(p_br[1]);
        vocalize[i] ~ bernoulli(p_br[2]);
        kick[i] ~ bernoulli(p_br[3]);
      } 
      else { // bite and NOT run 
        regurgitate[i] ~ bernoulli(p_b[1]);
        vocalize[i] ~ bernoulli(p_b[2]);
        kick[i] ~ bernoulli(p_b[3]);
      }
    } else {
      if (run_hide[i] == 1) { // NOT bite and run
        regurgitate[i] ~ bernoulli(p_r[1]);
        vocalize[i] ~ bernoulli(p_r[2]);
        kick[i] ~ bernoulli(p_r[3]);
      } else { // NOT bite and NOT run 
        regurgitate[i] ~ bernoulli(p[1]);
        vocalize[i] ~ bernoulli(p[2]);
        kick[i] ~ bernoulli(p[3]);
      }
    }
  }
}




  



