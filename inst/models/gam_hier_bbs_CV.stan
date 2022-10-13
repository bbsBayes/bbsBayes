// This is a Stan implementation of the gamye model
// Hierarchical

// Consider moving annual index calculations outside of Stan to
// facilitate the ragged array issues and to reduce the model output size (greatly)
// althought nice to have them here where Rhat and ess_ are calculated

// iCAR function, from Morris et al. 2019
// Morris, M., K. Wheeler-Martin, D. Simpson, S. J. Mooney, A. Gelman, and C. DiMaggio (2019).
// Bayesian hierarchical spatial models: Implementing the Besag York Mollié model in stan.
// Spatial and Spatio-temporal Epidemiology 31:100301.

 functions {
   real icar_normal_lpdf(vector bb, int ns, array[] int n1, array[] int n2) {
     return -0.5 * dot_self(bb[n1] - bb[n2])
       + normal_lpdf(sum(bb) | 0, 0.001 * ns); //soft sum to zero constraint on bb
  }
 }



data {
  int<lower=1> n_sites;
  int<lower=1> n_strata;
  int<lower=1> n_counts;
  int<lower=1> n_years;

  array[n_counts] int<lower=0> count;              // count observations
  array[n_counts] int<lower=1> strat;               // strata indicators
  array[n_counts] int<lower=1> year; // year index
  array[n_counts] int<lower=1> site; // site index
  array[n_counts] int<lower=0> first_year; // first year index
  array[n_counts] int<lower=1> observer;              // observer indicators

  int<lower=1> n_observers;// number of observers

  // array data to estimate annual indices using only observer-site combinations that are in each stratum
  array[n_strata] int<lower=0> n_obs_sites_strata; // number of observer-site combinations in each stratum
  int<lower=0> max_n_obs_sites_strata; //largest value of n_obs_sites_strata
  array[n_strata,max_n_obs_sites_strata] int ste_mat; //matrix identifying which sites are in each stratum
  array[n_strata,max_n_obs_sites_strata] int obs_mat; //matrix identifying which sites are in each stratum
  // above are effectively ragged arrays, but filled with 0 values so that Stan will accept it as data
  // but throws an error if an incorrect strata-site combination is called

  array[n_strata] real non_zero_weight; //proportion of the sites in the stratum included - scaling factor


  // data for spline s(year)
  int<lower=1> n_knots_year;  // number of knots in the basis function for year
  matrix[n_years, n_knots_year] year_basis; // basis function matrix

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




}


parameters {
  vector[n_train*use_pois] noise_raw;             // over-dispersion if use_pois == 1

 vector[n_strata] strata_raw;
   real STRATA;

  real eta; //first-year intercept

  vector[n_observers] obs_raw;    // sd of year effects
  vector[n_sites] ste_raw;   //
  real<lower=0> sdnoise;    // sd of over-dispersion
  real<lower=0> sdobs;    // sd of observer effects
  real<lower=0> sdste;    // sd of site effects
  //array[n_knots_year] real<lower=0> sdbeta;    // sd of GAM coefficients among strata
  array[n_strata] real<lower=0> sdbeta;    // sd of GAM coefficients among strata
  real<lower=0> sdstrata;    // sd of intercepts
  real<lower=0> sdBETA;    // sd of GAM coefficients
  real<lower=3> nu; // df of t-distribution > 3 so that it has a finite mean, variance, kurtosis

  vector[n_knots_year] BETA_raw;//_raw;
  matrix[n_strata,n_knots_year] beta_raw;         // GAM strata level parameters

}

transformed parameters {
  vector[n_train] E;           // log_scale additive likelihood
  matrix[n_strata,n_knots_year] beta;         // spatial effect slopes (0-centered deviation from continental mean slope B)
  matrix[n_years,n_strata] smooth_pred;
  vector[n_years] SMOOTH_pred;
  vector[n_knots_year] BETA;
  real<lower=0> phi; //transformed sdnoise


  if(use_pois){
    phi = 0;
  }else{
    phi = 1/sqrt(sdnoise); //as recommended to avoid prior that places most prior mass at very high overdispersion by https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
  }


  BETA = sdBETA*BETA_raw;
  //structure for spatial version where beta values are centered on mean for each knot
  // for(k in 1:n_knots_year){
  //   beta[,k] = (sdbeta * beta_raw[,k]) + BETA[k];
  // }
  SMOOTH_pred = year_basis * BETA;
  //structure for non-spatial version where beta values are estimated then centered on mean smooth
  for(s in 1:n_strata){
    beta[s,] = (sdbeta[s] * beta_raw[s,]) + transpose(BETA);
  }

  for(s in 1:n_strata){
     smooth_pred[,s] = year_basis * transpose(beta[s,]);
}


// intercepts and slopes




  for(i in 1:n_train){
    real noise;
    real obs = sdobs*obs_raw[observer_tr[i]];
    real strata = (sdstrata*strata_raw[strat_tr[i]]) + STRATA;
    real ste = sdste*ste_raw[site_tr[i]]; // site intercepts
    if(use_pois){
    noise = sdnoise*noise_raw[i];
    }else{
    noise = 0;
    }

    E[i] =  smooth_pred[year_tr[i],strat_tr[i]] + strata + eta*first_year_tr[i] + ste + obs + noise;
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

  sdnoise ~ student_t(3,0,1); //prior on scale of extra Poisson log-normal variance or inverse sqrt(phi) for negative binomial

  sdobs ~ normal(0,0.3); // informative prior on scale of observer effects - suggests observer variation larger than 3-4-fold differences is unlikely
  sdste ~ student_t(3,0,1); //prior on sd of site effects
  sdBETA ~ student_t(3,0,1); // prior on sd of GAM parameters
  sdbeta ~ student_t(3,0,1); // prior on sd of GAM parameters
  sdstrata ~ student_t(3,0,1); //prior on sd of intercept variation


  obs_raw ~ std_normal(); // ~ student_t(3,0,1);//observer effects
  //sum(obs_raw) ~ normal(0,0.001*n_observers); // constraint isn't useful here

  ste_raw ~ std_normal();//site effects
  //sum(ste_raw) ~ normal(0,0.001*n_sites); //constraint isn't useful here

  BETA_raw ~ std_normal();// prior on fixed effect mean GAM parameters
  //sum to zero constraint built into the basis function


  STRATA ~ normal(0,1);// prior on fixed effect mean intercept
  eta ~ normal(0,1);// prior on first-year observer effect



for(s in 1:n_strata){
    beta_raw[s,] ~ normal(0,1);
}
   strata_raw ~ normal(0,1);
    sum(strata_raw) ~ normal(0,0.001*n_strata);

if(use_pois){
  count_tr ~ poisson_log(E); //vectorized count likelihood with log-transformation
}else{
   count_tr ~ neg_binomial_2_log(E,phi); //vectorized count likelihood with log-transformation

}

}

 generated quantities {

   array[n_strata,n_years] real<lower=0> n;
   real<lower=0> retrans_noise;
   real<lower=0> retrans_obs;
   real<lower=0> retrans_ste;
   vector[n_counts*calc_log_lik] log_lik; // alternative value to track the observervation level log-likelihood
   vector[n_test*calc_CV] log_lik_cv; // alternative value to track the log-likelihood of the coutns in the test dataset
   real adj;

 // if the user wishes to calculate log_lik values to support package loo.
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

  // if cross-validation is being run
  if(calc_CV){
    for(i in 1:n_test){

    real noise;
    real obs = sdobs*obs_raw[observer_te[i]];
    real strata = (sdstrata*strata_raw[strat_te[i]]) + STRATA;
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


   log_lik_cv[i] = poisson_log_lpmf(count_te[i] | smooth_pred[year_te[i],strat_te[i]] + strata + eta*first_year_te[i] + ste + obs + noise);

   }else{
     noise = 0;
  log_lik_cv[i] = neg_binomial_2_log_lpmf(count_te[i] | smooth_pred[year_te[i],strat_te[i]] + strata + eta*first_year_te[i] + ste + obs + noise, phi);

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
  real strata = (sdstrata*strata_raw[s]) + STRATA;

        for(t in 1:n_obs_sites_strata[s]){  //for each observer-route combination in the realised dtaset

  real ste = sdste*ste_raw[ste_mat[s,t]]; // site intercepts
  real obs = sdobs*obs_raw[obs_mat[s,t]]; // observer intercepts


      n_t[t] = exp(strata+ smooth_pred[y,s] + retrans_noise + obs + ste);// + retrans_obs);
        }
        n[s,y] = non_zero_weight[s] * mean(n_t);//mean of exponentiated predictions across sites in a stratum
        //using the mean of hte exponentiated values, instead of including the log-normal
        // retransformation factor (0.5*sdste^2), because this retransformation makes 2 questionable assumptions:
          // 1 - assumes that sdste is equal among all strata
          // 2 - assumes that the distribution of site-effects is normal
        // As a result, these annual indices reflect predictions of mean annual abundance within strata of the routes that are included in the stratum


    }
  }



 }

