// The input data is a vector 'y' of length 'N'.
data {
  // data size
  int N; //number of observations
  int B; //number of behaviors we are modeling (NB: DIFFERENT FROM M1)
  
  // individual ID and physical characteristics
  // int num_observations[N];
  // int band[N];
  vector[N] tarsus;
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
 real coeff_tl_br [B];
 real beta0_br [B];
 // only bite
 real coeff_tl_b [B];
 real beta0_b [B];
 // only run
 real coeff_tl_r [B];
 real beta0_r [B];
 // neither bite nor run
 real coeff_tl [B];
 real beta0 [B];
}

model {
  coeff_tl ~ normal(0, 10); 
  beta0 ~ normal(0, 10);
  
  // partition via bite and run/hide behaviors
  for(i in 1:N){
    if (bite[i] == 1) {
      if (run_hide[i] == 1) { // bite and run
        regurgitate[i] ~ bernoulli_logit(coeff_tl_br[1]*tarsus[i]+beta0_br[1]);
        vocalize[i] ~ bernoulli_logit(coeff_tl_br[2]*tarsus[i]+beta0_br[2]);
        kick[i] ~ bernoulli_logit(coeff_tl_br[3]*tarsus[i]+beta0_br[3]);
      } 
      else { // bite and NOT run 
        regurgitate[i] ~ bernoulli_logit(coeff_tl_b[1]*tarsus[i]+beta0_b[1]);
        vocalize[i] ~ bernoulli_logit(coeff_tl_b[2]*tarsus[i]+beta0_b[2]);
        kick[i] ~ bernoulli_logit(coeff_tl_b[3]*tarsus[i]+beta0_b[3]);
      }
    } else {
      if (run_hide[i] == 1) { // NOT bite and run
        regurgitate[i] ~ bernoulli_logit(coeff_tl_r[1]*tarsus[i]+beta0_r[1]);
        vocalize[i] ~ bernoulli_logit(coeff_tl_r[2]*tarsus[i]+beta0_r[2]);
        kick[i] ~ bernoulli_logit(coeff_tl_r[3]*tarsus[i]+beta0_r[3]);
      } else { // NOT bite and NOT run 
        regurgitate[i] ~ bernoulli_logit(coeff_tl[1]*tarsus[i]+beta0[1]);
        vocalize[i] ~ bernoulli_logit(coeff_tl[2]*tarsus[i]+beta0[2]);
        kick[i] ~ bernoulli_logit(coeff_tl[3]*tarsus[i]+beta0[3]);
      }
    }
  }
}

generated quantities {
  real p_r [N];
  real p_v [N];
  real p_k [N];
  
  for(i in 1:N){
    if (bite[i] == 1) {
      if (run_hide[i] == 1) { // bite and run
        p_r[i] = exp(coeff_tl_br[1]*tarsus[i]+beta0_br[1])/(1+exp(coeff_tl_br[1]*tarsus[i]+beta0_br[1]));
        p_v[i] = exp(coeff_tl_br[2]*tarsus[i]+beta0_br[2])/(1+exp(coeff_tl_br[2]*tarsus[i]+beta0_br[2]));
        p_k[i] = exp(coeff_tl_br[3]*tarsus[i]+beta0_br[3])/(1+exp(coeff_tl_br[3]*tarsus[i]+beta0_br[3]));
      } 
      else { // bite and NOT run 
        p_r[i] = exp(coeff_tl_b[1]*tarsus[i]+beta0_b[1])/(1+exp(coeff_tl_b[1]*tarsus[i]+beta0_b[1]));
        p_v[i] = exp(coeff_tl_b[2]*tarsus[i]+beta0_b[2])/(1+exp(coeff_tl_b[2]*tarsus[i]+beta0_b[2]));
        p_k[i] = exp(coeff_tl_b[3]*tarsus[i]+beta0_b[3])/(1+exp(coeff_tl_b[3]*tarsus[i]+beta0_b[3]));
      }
    } else {
      if (run_hide[i] == 1) { // NOT bite and run
        p_r[i] = exp(coeff_tl_r[1]*tarsus[i]+beta0_r[1])/(1+exp(coeff_tl_r[1]*tarsus[i]+beta0_r[1]));
        p_v[i] = exp(coeff_tl_r[2]*tarsus[i]+beta0_r[2])/(1+exp(coeff_tl_r[2]*tarsus[i]+beta0_r[2]));
        p_k[i] = exp(coeff_tl_r[3]*tarsus[i]+beta0_r[3])/(1+exp(coeff_tl_r[3]*tarsus[i]+beta0_r[3]));
      } else { // NOT bite and NOT run 
        p_r[i] = exp(coeff_tl[1]*tarsus[i]+beta0[1])/(1+exp(coeff_tl[1]*tarsus[i]+beta0[1]));
        p_v[i] = exp(coeff_tl[2]*tarsus[i]+beta0[2])/(1+exp(coeff_tl[2]*tarsus[i]+beta0[2]));
        p_k[i] = exp(coeff_tl[3]*tarsus[i]+beta0[3])/(1+exp(coeff_tl[3]*tarsus[i]+beta0[3]));
      }
    }
  }
  // for(n in 1:N){
  //     if (bite[n] == 1){
  //       if (run_hide[n] == 1){
  //         // bite and run
  //         for(b in 1:B){
  //           p[n, b] = exp(coeff_tl_br[b]*tarsus[n] + beta0_br[b])/(1+exp(coeff_tl_br[b]*tarsus[n] + beta0_br[b]));
  //         }
  //       }else if (run_hide[n] == 0){
  //         // bite, don't run
  //         for(b in 1:B){
  //           p[n, b] = exp(coeff_tl_b[b]*tarsus[n] + beta0_b[b])/(1+exp(coeff_tl_b[b]*tarsus[n] + beta0_b[b]));
  //         }
  //       }
  //     }else {
  //       if(run_hide[n] == 1){
  //         // don't bite, do run
  //         for(b in 1:B){
  //           p[n, b] = exp(coeff_tl_r[b]*tarsus[n]+beta0_r[b])/(1+exp(coeff_tl_r[b]*tarsus[n]+beta0_r[b]));
  //         }
  //       }else{
  //       // don't bite, don't run
  //       for(b in 1:B{
  //         p[n, b] = exp(coeff_tl[b]*tarsus[n] + beta0[b])/(1+exp(coeff_tl[b]*tarsus[n] + beta0[b]));
  //       }
  //     }
  //     }
  // }
  }



  



