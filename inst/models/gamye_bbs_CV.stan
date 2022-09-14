// This is a Stan implementation of the gamye model

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
  int<lower=1> nsites;
  int<lower=1> nstrata;
  int<lower=1> ncounts;
  int<lower=1> nyears;
  
  array[ncounts] int<lower=0> count;              // count observations
  array[ncounts] int<lower=1> strat;               // strata indicators
  array[ncounts] int<lower=1> year; // year index
  array[ncounts] int<lower=1> site; // site index
  array[ncounts] int<lower=0> firstyr; // first year index
  array[ncounts] int<lower=1> observer;              // observer indicators
  
  int<lower=1> nobservers;// number of observers

  // array data to estimate annual indices using only observer-site combinations that are in each stratum
  array[nstrata] int<lower=0> nobs_sites_strata; // number of observer-site combinations in each stratum
  int<lower=0> maxnobs_sites_strata; //largest value of nobs_sites_strata 
  array[nstrata,maxnobs_sites_strata] int ste_mat; //matrix identifying which sites are in each stratum
  array[nstrata,maxnobs_sites_strata] int obs_mat; //matrix identifying which sites are in each stratum
  // above are effectively ragged arrays, but filled with 0 values so that Stan will accept it as data
  // but throws an error if an incorrect strata-site combination is called
  
  array[nstrata] real nonzeroweight; //proportion of the sites in the stratum included - scaling factor
 

  // data for spline s(year)
  int<lower=1> nknots_year;  // number of knots in the basis function for year
  matrix[nyears, nknots_year] year_basis; // basis function matrix

  // Extra Poisson variance options
  int<lower=0,upper=1> heavy_tailed; //indicator if extra poisson variance should be t-distributed or normal (yes = 1, no = 0 and therefore normal)
  int<lower=0,upper=1> calc_nu; //indicator if nu should be calculated (yes = 1, no = 0)
  int<lower=0,upper=1> use_pois; //indicator if count variation should be based on over-dispersed Poisson (if ==1) or Negative binomial (if == 0)
  
  // loo or CV calculations
  int<lower=0,upper=1> calc_log_lik; //indicator if log_lik should be calculated (log_lik for all obs to support loo = 1, no log-lik = 0)
  int<lower=0,upper=1> calc_CV; //indicator if CV should be calculated (CrossValidation = 1, no CV = 0)
  // CV folds - if calc_CV == 1 then the following values define the training and test sets
  int<lower=1, upper=ncounts> ntrain; //
  int<lower=1, upper=ncounts> ntest; //
  array[ntrain] int<lower=1, upper=ncounts> train; // indices of counts to include in train data
  array[ntest] int<lower=1, upper=ncounts> test; // indices of counts to include in test data
  
  // This approach to CV only works if all observers and routes are included in each training-fold
  // So, CV folds must be nested within observers and routes
  // Could implement leave future observation style CV within observers and routes if indexing was done carefully

}

transformed data {
   
     array[ntrain] int count_tr = count[train];
     array[ntrain] int strat_tr = strat[train];
     array[ntrain] int year_tr = year[train];
     array[ntrain] int site_tr = site[train];
     array[ntrain] int firstyr_tr = firstyr[train];
     array[ntrain] int observer_tr = observer[train];
     
     array[ntest] int count_te = count[test];
     array[ntest] int strat_te = strat[test];
     array[ntest] int year_te = year[test];
     array[ntest] int site_te = site[test];
     array[ntest] int firstyr_te = firstyr[test];
     array[ntest] int observer_te = observer[test];
     
  
  
  
}


parameters {
  vector[ntrain*use_pois] noise_raw;             // over-dispersion if use_pois == 1
 
 vector[nstrata] strata_raw;
   real STRATA; 

  real eta; //first-year intercept
  
  matrix[nstrata,nyears] yeareffect_raw;

  vector[nobservers] obs_raw;    // sd of year effects
  vector[nsites] ste_raw;   // 
  real<lower=0> sdnoise;    // sd of over-dispersion
  real<lower=0> sdobs;    // sd of observer effects
  real<lower=0> sdste;    // sd of site effects
  //array[nknots_year] real<lower=0> sdbeta;    // sd of GAM coefficients among strata 
  array[nstrata] real<lower=0> sdbeta;    // sd of GAM coefficients among strata 
  real<lower=0> sdstrata;    // sd of intercepts
  real<lower=0> sdBETA;    // sd of GAM coefficients
  array[nstrata] real<lower=0> sdyear;    // sd of year effects
  real<lower=3> nu; // df of t-distribution > 3 so that it has a finite mean, variance, kurtosis
  
  vector[nknots_year] BETA_raw;//_raw; 
  matrix[nstrata,nknots_year] beta_raw;         // GAM strata level parameters

}

transformed parameters { 
  vector[ntrain] E;           // log_scale additive likelihood
  matrix[nstrata,nknots_year] beta;         // spatial effect slopes (0-centered deviation from continental mean slope B)
  matrix[nyears,nstrata] smooth_pred;
  vector[nyears] SMOOTH_pred; 
  matrix[nstrata,nyears] yeareffect;
  vector[nknots_year] BETA;
  real<lower=0> phi; //transformed sdnoise
  
  
  if(use_pois){
    phi = 0;
  }else{
    phi = 1/sqrt(sdnoise); //as recommended to avoid prior that places most prior mass at very high overdispersion by https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
  }
  
  
  BETA = sdBETA*BETA_raw;
  //structure for spatial version where beta values are centered on mean for each knot  
  // for(k in 1:nknots_year){
  //   beta[,k] = (sdbeta * beta_raw[,k]) + BETA[k];
  // }
  SMOOTH_pred = year_basis * BETA; 
  //structure for non-spatial version where beta values are estimated then centered on mean smooth
  for(s in 1:nstrata){
    beta[s,] = (sdbeta[s] * beta_raw[s,]) + transpose(BETA);
  }
  
  for(s in 1:nstrata){
     smooth_pred[,s] = year_basis * transpose(beta[s,]);
}

for(s in 1:nstrata){
    yeareffect[s,] = sdyear[s]*yeareffect_raw[s,];

}

// intercepts and slopes

  


  for(i in 1:ntrain){
    real noise;
    real obs = sdobs*obs_raw[observer_tr[i]];
    real strata = (sdstrata*strata_raw[strat_tr[i]]) + STRATA;
    real ste = sdste*ste_raw[site_tr[i]]; // site intercepts
    if(use_pois){
    noise = sdnoise*noise_raw[i];
    }else{
    noise = 0;
    }
    
    E[i] =  smooth_pred[year_tr[i],strat_tr[i]] + strata + yeareffect[strat_tr[i],year_tr[i]] + eta*firstyr_tr[i] + ste + obs + noise;
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
  sdyear ~ gamma(2,2); // prior on sd of yeareffects - stratum specific, and boundary-avoiding with a prior mode at 0.5 (1/2) - recommended by https://doi.org/10.1007/s11336-013-9328-2 
  sdBETA ~ student_t(3,0,1); // prior on sd of GAM parameters
  sdbeta ~ student_t(3,0,1); // prior on sd of GAM parameters
  sdstrata ~ student_t(3,0,1); //prior on sd of intercept variation
 
 
  obs_raw ~ std_normal(); // ~ student_t(3,0,1);//observer effects
  //sum(obs_raw) ~ normal(0,0.001*nobservers); // constraint isn't useful here

  ste_raw ~ std_normal();//site effects
  //sum(ste_raw) ~ normal(0,0.001*nsites); //constraint isn't useful here
 
 for(s in 1:nstrata){

  yeareffect_raw[s,] ~ std_normal();
  //soft sum to zero constraint on year effects within a stratum
  sum(yeareffect_raw[s,]) ~ normal(0,0.001*nyears);
  
 }
  
  BETA_raw ~ std_normal();// prior on fixed effect mean GAM parameters
  //sum to zero constraint built into the basis function

  
  STRATA ~ normal(0,1);// prior on fixed effect mean intercept
  eta ~ normal(0,1);// prior on first-year observer effect
  
  
 
for(s in 1:nstrata){
    beta_raw[s,] ~ normal(0,1);
}
   strata_raw ~ normal(0,1);
    sum(strata_raw) ~ normal(0,0.001*nstrata);
 
if(use_pois){
  count_tr ~ poisson_log(E); //vectorized count likelihood with log-transformation
}else{
   count_tr ~ neg_binomial_2_log(E,phi); //vectorized count likelihood with log-transformation
 
}

}

 generated quantities {

   array[nstrata,nyears] real<lower=0> n;
   array[nstrata,nyears] real<lower=0> nsmooth;
   real<lower=0> retrans_noise;
   real<lower=0> retrans_obs;
   real<lower=0> retrans_ste;
   vector[ncounts*calc_log_lik] log_lik; // alternative value to track the observervation level log-likelihood
   vector[ntest*calc_CV] log_lik_cv; // alternative value to track the log-likelihood of the coutns in the test dataset
   real adj;
 
 // if the user wishes to calculate log_lik values to support package loo.
  if(calc_log_lik){
  // potentially useful for estimating loo-diagnostics, such as looic
  if(use_pois){
  for(i in 1:ncounts){
   log_lik[i] = poisson_log_lpmf(count_tr[i] | E[i]);
   }
  }else{
   for(i in 1:ncounts){
   log_lik[i] = neg_binomial_2_log_lpmf(count_tr[i] | E[i] , phi);
   } 
  }
  }
  
  // if cross-validation is being run
  if(calc_CV){
    for(i in 1:ntest){
      
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
   
      
   log_lik_cv[i] = poisson_log_lpmf(count_te[i] | smooth_pred[year_te[i],strat_te[i]] + strata + yeareffect[strat_te[i],year_te[i]] + eta*firstyr_te[i] + ste + obs + noise);
  
   }else{
     noise = 0;
  log_lik_cv[i] = neg_binomial_2_log_lpmf(count_te[i] | smooth_pred[year_te[i],strat_te[i]] + strata + yeareffect[strat_te[i],year_te[i]] + eta*firstyr_te[i] + ste + obs + noise, phi);
  
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


for(y in 1:nyears){

      for(s in 1:nstrata){

  array[nobs_sites_strata[s]] real n_t;
  array[nobs_sites_strata[s]] real nsmooth_t;
  real retrans_yr = 0.5*(sdyear[s]^2);
  real strata = (sdstrata*strata_raw[s]) + STRATA;
  
        for(t in 1:nobs_sites_strata[s]){  //for each observer-route combination in the realised dtaset

  real ste = sdste*ste_raw[ste_mat[s,t]]; // site intercepts
  real obs = sdobs*obs_raw[obs_mat[s,t]]; // observer intercepts


      n_t[t] = exp(strata+ smooth_pred[y,s] + yeareffect[s,y] + retrans_noise + obs + ste);// + retrans_obs);
      nsmooth_t[t] = exp(strata + smooth_pred[y,s] + retrans_yr + retrans_noise + obs + ste);// + retrans_obs);
        }
        n[s,y] = nonzeroweight[s] * mean(n_t);//mean of exponentiated predictions across sites in a stratum
        nsmooth[s,y] = nonzeroweight[s] * mean(nsmooth_t);//mean of exponentiated predictions across sites in a stratum
        //using the mean of hte exponentiated values, instead of including the log-normal
        // retransformation factor (0.5*sdste^2), because this retransformation makes 2 questionable assumptions:
          // 1 - assumes that sdste is equal among all strata
          // 2 - assumes that the distribution of site-effects is normal
        // As a result, these annual indices reflect predictions of mean annual abundance within strata of the routes that are included in the stratum


    }
  }



 }

