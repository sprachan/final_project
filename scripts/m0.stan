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

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real <lower=0, upper=1> p[5];
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  p ~ beta(1, 1);
  
  for(i in 1:N){
      passive[i] ~ binomial(num_observations[i], p[1]);
      bite[i] ~ binomial(num_observations[i], p[2]);
      run_hide[i] ~ binomial(num_observations[i], p[3]);
      regurgitate[i] ~ binomial(num_observations[i], p[4]);
      vocalize[i] ~ binomial(num_observations[i], p[5]);
    
  }
}

