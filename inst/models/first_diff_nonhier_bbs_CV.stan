// This is a Stan implementation of the first difference model that shares information among strata on the annual differences
// Non-Hierarchical
//
// This is an elaboration of the model used in Link and Sauer
//




data {
  int<lower=1> n_sites;
  int<lower=1> n_strata;
  int<lower=1> n_counts;
  int<lower=1> n_years;
  int<lower=1> fixed_year; //middle year of the time-series scaled to ~(n_years/2)


  array[n_counts] int<lower=0> count;              // count observations
  array[n_counts] int<lower=1> strat;               // strata indicators
  array[n_counts] int<lower=1> year; // year index
  array[n_counts] int<lower=1> site; // site index
  array[n_counts] int<lower=0> first_year; // first year index
  array[n_counts] int<lower=1> observer;              // observer indicators

  int<lower=1> n_observers;// number of observers


  // extra data to support the first-difference time-series implementation, which is centered at the mid-year of the available time
  // data to center the abundance estimate
  int n_Iy1; //indexing vector dimension - number of years before fixed_year
  int n_Iy2; //indexing vector dimension - number of years after fixed_year
  array[n_Iy1] int Iy1;//indexing vector - indices of years before fixed_year
  array[n_Iy2] int Iy2;//indexing vector - indices of years after fixed_year
  // a vector of zeros to fill fixed beta values for fixed_year
  vector[n_strata] zero_betas;


  // array data to estimate annual indices using only observer-site combinations that are in each stratum
  array[n_strata] int<lower=0> n_obs_sites_strata; // number of observer-site combinations in each stratum
  int<lower=0> max_n_obs_sites_strata; //largest value of n_obs_sites_strata
  array[n_strata,max_n_obs_sites_strata] int ste_mat; //matrix identifying which sites are in each stratum
  array[n_strata,max_n_obs_sites_strata] int obs_mat; //matrix identifying which sites are in each stratum
  // above are effectively ragged arrays, but filled with 0 values so that Stan will accept it as data
  // but throws an error if an incorrect strata-site combination is called

  array[n_strata] real non_zero_weight; //proportion of the sites included - scaling factor

  // Extra Poisson variance options
  int<lower=0,upper=1> heavy_tailed; //indicator if extra poisson variance should be t-distributed or normal (yes = 1, no = 0 and therefore normal)
  int<lower=0,upper=1> calc_nu; //indicator if nu should be calculated (yes = 1, no = 0)
  int<lower=0,upper=1> use_pois; //indicator if count variation should be based on over-dispersed Poisson (if ==1) or Negative binomial (if == 0)

  // loo or CV calculations
  int<lower=0,upper=1> calc_log_lik; //indicator if log_lik should be calculated (log_lik for all obs to support loo = 1, no log-lik = 0)
  int<lower=0,upper=1> calc_CV; //indicator if CV should be calculated (CrossValidation = 1, no CV = 0)
  // CV folds - if calc_CV == 1 then the following values define the training and test sets
  int<lower=1, upper=n_counts> n_train; //
  int<lower=1, upper=n_counts> n_test; //
  array[n_train] int<lower=1, upper=n_counts> train; // indices of counts to include in train data
  array[n_test] int<lower=1, upper=n_counts> test; // indices of counts to include in test data

  // This approach to CV only works if all observers and routes are included in each training-fold
  // So, CV folds must be nested within observers and routes
  // Could implement leave future observation style CV within observers and routes if indexing was done carefully

}

transformed data {
   //These statements split the data into training and testing sets for cross-validation
   // if calc_CV == 0 (and so no CV is required), then n_train = n_count and all data are included in the training set
   // in that case, n_test = 1, but all values of xxx_te are ignored for the remainder of the model
     array[n_train] int count_tr = count[train];
     array[n_train] int strat_tr = strat[train];
     array[n_train] int year_tr = year[train];
     array[n_train] int site_tr = site[train];
     array[n_train] int first_year_tr = first_year[train];
     array[n_train] int observer_tr = observer[train];

     array[n_test] int count_te = count[test];
     array[n_test] int strat_te = strat[test];
     array[n_test] int year_te = year[test];
     array[n_test] int site_te = site[test];
     array[n_test] int first_year_te = first_year[test];
     array[n_test] int observer_te = observer[test];

     int<lower=1> n_years_m1 = n_years-1;



}


parameters {
  vector[n_train*use_pois] noise_raw;             // over-dispersion if use_pois == 1

 vector[n_strata] strata_raw; // strata intercepts
   real STRATA; // hyperparameter intercepts

  real eta; //first-year effect

  vector[n_observers] obs_raw;    // observer effects
  vector[n_sites] ste_raw;   // site (route) effects
  real<lower=0> sdnoise;    // sd of over-dispersion, if use_pois == 1
  real<lower=0> sdobs;    // sd of observer effects
  real<lower=0> sdste;    // sd of site (route) effects
  real<lower=0> sdstrata;    // sd of intercepts among strata
  real<lower=3> nu; // df of t-distribution, if calc_nu == 1, > 3 so that it has a finite mean, variance, kurtosis

  vector<lower=0>[n_strata] sdbeta;    // sd of annual changes among strata
  // real<lower=0> sdBETA;    // sd of overall annual changes
  //
  // vector[n_years_m1] BETA_raw;//_hyperparameter of overall annual change values - "differences" between years
   matrix[n_strata,n_years_m1] beta_raw;         // strata level parameters
  //
}

transformed parameters {
  vector[n_train] E;           // log_scale additive likelihood
  matrix[n_strata,n_years] beta;         // strata-level mean differences (0-centered deviation from continental mean BETA)
  matrix[n_strata,n_years] yeareffect;  // matrix of estimated annual values of trajectory
  //vector[n_years_m1] BETA; // annual estimates of continental mean differences (n_years - 1, because middle year is fixed at 0)
  vector[n_years] YearEffect;
  vector[n_strata] strata;
  real<lower=0> phi; //transformed sdnoise if use_pois == 0 (and therefore Negative Binomial)


  if(use_pois){
    phi = 0;
  }else{
    phi = 1/sqrt(sdnoise); //as recommended to avoid prior that places most prior mass at very high overdispersion by https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
  }

  //BETA = sdBETA * BETA_raw;

  beta[,fixed_year] = zero_betas; //fixed at zero
  yeareffect[,fixed_year] = zero_betas; //fixed at zero
 // YearEffect[fixed_year] = 0; //fixed at zero

for(s in 1:n_strata){
// first half of time-series - runs backwards from fixed_year
  for(t in Iy1){
    beta[s,t] = (sdbeta[s] * beta_raw[s,t]);// + BETA[t];
    yeareffect[s,t] = yeareffect[s,t+1] + beta[s,t];
  //  YearEffect[t] = YearEffect[t+1] + BETA[t]; // hyperparameter trajectory interesting to monitor but no direct inference
  }
// second half of time-series - runs forwards from fixed_year
   for(t in Iy2){
    beta[s,t] = (sdbeta[s] * beta_raw[s,t-1]);// + BETA[t-1];//t-1 indicators to match dimensionality
    yeareffect[s,t] = yeareffect[s,t-1] + beta[s,t];
   // YearEffect[t] = YearEffect[t-1] + BETA[t-1];
  }
}
   strata = (sdstrata*strata_raw) + STRATA;


  for(i in 1:n_train){
    real noise;
    real obs = sdobs*obs_raw[observer_tr[i]];
    real ste = sdste*ste_raw[site_tr[i]]; // site intercepts
    if(use_pois){
    noise = sdnoise*noise_raw[i];
    }else{
    noise = 0;
    }

    E[i] =  strata[strat_tr[i]] + yeareffect[strat_tr[i],year_tr[i]] + eta*first_year_tr[i] + ste + obs + noise;
  }



  }



model {
  nu ~ gamma(2,0.1); // prior on df for t-distribution of heavy tailed site-effects from https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations#prior-for-degrees-of-freedom-in-students-t-distribution
  if(use_pois){
    if(heavy_tailed){
    if(calc_nu){
      noise_raw ~ student_t(nu,0,1);//student_t(nu,0,1); //normal tailed extra Poisson log-normal variance
   }else{
      noise_raw ~ student_t(3,0,1);//student_t(nu,0,1); //normal tailed extra Poisson log-normal variance
    }
   }else{
    noise_raw ~ std_normal();//student_t(nu,0,1); //normal tailed extra Poisson log-normal variance
    }
  }
  if(use_pois){
  sdnoise ~ student_t(3,0,1); //prior on scale of extra Poisson log-normal variance or inverse sqrt(phi) for negative binomial
  }else{
  sdnoise ~ student_t(3,0,1); //prior on scale of extra Poisson log-normal variance or inverse sqrt(phi) for negative binomial
  }
  sdobs ~ normal(0,0.3); // informative prior on scale of observer effects - suggests observer variation larger than 3-4-fold differences is unlikely
  sdste ~ student_t(3,0,1); //prior on sd of site effects
  sdbeta ~ student_t(3,0,0.1); // prior on sd of differences among strata
  //sdBETA ~ student_t(3,0,0.1); // prior on sd of mean hyperparameter differences


  obs_raw ~ std_normal();//observer effects
  //sum(obs_raw) ~ normal(0,0.001*n_observers); // constraint isn't useful here

  ste_raw ~ std_normal();//site effects
  //sum(ste_raw) ~ normal(0,0.001*n_sites); //constraint isn't useful here


  //BETA_raw ~ std_normal();// prior on fixed effect mean intercept

  STRATA ~ std_normal();// prior on fixed effect mean intercept
  eta ~ std_normal();// prior on first-year observer effect


  sdstrata ~ student_t(3,0,1); //prior on sd of intercept variation


for(s in 1:(n_strata)){
    beta_raw[s,] ~ std_normal();
  sum(beta_raw[s,]) ~ normal(0,0.001*n_years_m1);
}

   strata_raw ~ std_normal();
  //soft sum to zero constraint on strata intercepts
  sum(strata_raw) ~ normal(0,0.001*n_strata);


if(use_pois){
  count_tr ~ poisson_log(E); //vectorized count likelihood with log-transformation
}else{
   count_tr ~ neg_binomial_2_log(E,phi); //vectorized count likelihood with log-transformation

}

}

 generated quantities {

   array[n_strata,n_years] real<lower=0> n; //full annual indices
//   array[n_strata*calc_n2,n_years*calc_n2] real<lower=0> n2; //full annual indices calculated assuming site-effects are log-normal and the same among strata
//   array[n_strata*calc_n2,n_years*calc_n2] real<lower=0> n_slope2; //smooth component of annual indices calculated assuming site-effects are log-normal and the same among strata
   real<lower=0> retrans_noise;
   real<lower=0> retrans_obs;
   real<lower=0> retrans_ste;
   vector[n_counts*calc_log_lik] log_lik; // alternative value to track the observervation level log-likelihood
   vector[n_test*calc_CV] log_lik_cv; // alternative value to track the log-likelihood of the coutns in the test dataset
   real adj;

  if(calc_log_lik){
  // potentially useful for estimating loo-diagnostics, such as looic
  if(use_pois){
  for(i in 1:n_counts){
   log_lik[i] = poisson_log_lpmf(count_tr[i] | E[i]);
   }
  }else{
   for(i in 1:n_counts){
   log_lik[i] = neg_binomial_2_log_lpmf(count_tr[i] | E[i] , phi);
   }
  }
  }

  if(calc_CV){
    for(i in 1:n_test){

    real noise;
    real obs = sdobs*obs_raw[observer_te[i]];
    real ste = sdste*ste_raw[site_te[i]]; // site intercepts

   if(use_pois){
      if(heavy_tailed){
        if(calc_nu){
    noise = student_t_rng(nu,0,sdnoise);
        }else{
    noise = student_t_rng(3,0,sdnoise);
        }
      }else{
    noise = normal_rng(0,sdnoise);
      }


   log_lik_cv[i] = poisson_log_lpmf(count_te[i] | strata[strat_te[i]] + yeareffect[strat_te[i],year_te[i]] + eta*first_year_te[i] + ste + obs + noise);

   }else{
     noise = 0;
  log_lik_cv[i] = neg_binomial_2_log_lpmf(count_te[i] | strata + yeareffect[strat_te[i],year_te[i]] + eta*first_year_te[i] + ste + obs + noise, phi);

   }

  }

  }

  if(use_pois){
  if(heavy_tailed){
      if(calc_nu){
         adj = (1.422*(nu^0.906))/(1+(1.422*(nu^0.906)));
        }else{
         adj = (1.422*(3^0.906))/(1+(1.422*(3^0.906)));
        }
    }else{
      adj = 1;
    }
     retrans_noise = 0.5*(sdnoise/adj)^2;
}else{
  adj = 1;
  retrans_noise = 0;
}

retrans_obs = 0.5*(sdobs^2);
retrans_ste = 0.5*(sdste^2);

// Annual indices of abundance - strata-level annual predicted counts


for(y in 1:n_years){

      for(s in 1:n_strata){

  array[n_obs_sites_strata[s]] real n_t;

        for(t in 1:n_obs_sites_strata[s]){

  real ste = sdste*ste_raw[ste_mat[s,t]]; // site intercepts
  real obs = sdobs*obs_raw[obs_mat[s,t]]; // observer intercepts



      n_t[t] = exp(strata[s] + yeareffect[s,y] + retrans_noise + ste + obs);
        }
        n[s,y] = non_zero_weight[s] * mean(n_t);//mean of exponentiated predictions across sites in a stratum
        //using the mean of hte exponentiated values, instead of including the log-normal
        // retransformation factor (0.5*sdste^2), because this retransformation makes 2 questionable assumptions:
          // 1 - assumes that sdste is equal among all strata
          // 2 - assumes that the distribution of site-effects is normal
        // As a result, these annual indices reflect predictions of mean annual abundance within strata of the routes that are included in the stratum
        // if(calc_n2){
        // n2[s,y] = non_zero_weight[s] * exp(strata + beta[s]*(y-fixed_year) + retrans_ste + yeareffect[s,y] + retrans_noise + retrans_obs);//mean of exponentiated predictions across sites in a stratum
        // }

    }
  }



 }

