// Model to predict trawl width as a function of time.

#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator()(){
   // Data declarations:
   DATA_IVECTOR(t);           // Time index (n_obs).
   DATA_VECTOR(width);        // Trawl width observations (n_obs).
   DATA_IVECTOR(tow);         // Tow index (n_obs).
   DATA_MATRIX(speed);        // Vessel speed matrix (n_t x n_tow).
   DATA_MATRIX(active);       // Active trawling phase identifier matrix (n_t x n_tow).
   DATA_MATRIX(passive);      // Passive trawling phase identifier matrix (n_t x n_tow).
   DATA_VECTOR(x);            // Sampling station x coordinate (n_station).
   DATA_VECTOR(y);            // Sampling station y coordinate (n_station).
   DATA_IVECTOR(station);     // Sampling station identifier (n_tow).   
   
   // Parameter and random effect declarations:
   PARAMETER(alpha);                         // Global mean parameter.

   // Spatial and tow-level processes:
   PARAMETER_VECTOR(station_effect);         // Station random effect (n_station).
   PARAMETER(log_sigma_process_station);     // Log-scale station effect process error.
   PARAMETER(log_sigma_obs_station);         // Log-scale station effect observation error.
   PARAMETER(log_a_station);                 // Spatial Gaussian process smoothness parameter.
   PARAMETER(log_rho_station);               // Spatial Gaussian process range correlation parameter.
   PARAMETER_VECTOR(comparative_effect);     // Comparative tow random intercept effect (n_tow - n_station).
   PARAMETER(mu_comparative);                // Comparative effect global mean.
   PARAMETER(log_sigma_comparative);         // Log-scale comparative tow effect.
   
   // Time processes:   
   PARAMETER_VECTOR(lambda_global);          // Time Gaussian process random effect (n_t).
   PARAMETER(log_sigma_process_global);      // Log-scale global process standard error. 
   PARAMETER_MATRIX(lambda);                 // Autoregressive random effect (n_t x n_station).
   PARAMETER(log_sigma_process);             // Log-scale process standard error.
   PARAMETER(log_a_t);                       // Time Gaussian process smoothness parameter.
   PARAMETER(log_rho_t);                     // Time Gaussian process range correlation parameter.
      
   // Observation error processes:
   PARAMETER(log_sigma_obs);                 // Log-scale observation standard error.
   PARAMETER(log_sigma_outlier);             // Log-scale outlier standard error.
   PARAMETER(logit_p_outlier);               // Logit-scale proportion of outliers.
        
   // Transformed parameters:  
   Type sigma_process_station = exp(log_sigma_process_station);  // Station effect process error.
   Type sigma_obs_station     = exp(log_sigma_obs_station);      // Station effect observation error.   
   Type a_station             = exp(log_a_station);              // Spatial Gaussian process smoothness parameter. 
   Type rho_station           = exp(log_rho_station);            // Spatial Gaussian process range correlation parameter.
   Type sigma_comparative     = exp(log_sigma_comparative);      // Comparative tow effect error.
   Type sigma_process_global  = exp(log_sigma_process_global);   // Global process standard error.  
   Type sigma_process         = exp(log_sigma_process);          // Process standard error.
   Type a_t                   = exp(log_a_t);                    // Time Gaussian process smoothness parameter.
   Type rho_t                 = exp(log_rho_t);                  // Time Gaussian process range correlation parameter.
   Type sigma_obs             = exp(log_sigma_obs);              // Observation standard error.
   Type sigma_outlier         = exp(log_sigma_outlier);          // Outlier standard error.  
   Type p_outlier = Type(1) / (Type(1) + exp(-logit_p_outlier)); // Proportion of outliers:

   // Observation and effect dimensions:
   int n_obs = width.size();
   int n_t = lambda.rows();
   int n_station = station_effect.size();
   int n_comparative = comparative_effect.size();
   int n_tow = n_station + n_comparative;

   // Initialize negative log-likelihood accumulator:
   Type res = 0;
  
   // Rational quadratic spatial prior over station effects:
   matrix<Type> Sigma_station(n_station, n_station); 
   for (int i = 0; i < n_station; i++){
      Sigma_station(i,i) = sigma_process_station + sigma_obs_station;
      for (int j = 0; j < i; j++){
         Type d = sqrt((x[i]-x[j]) * (x[i]-x[j]) + (y[i]-y[j]) * (y[i]-y[j])) / rho_station;     
         Sigma_station(i,j) = sigma_process_station * exp(-a_station * log(1 + (d * d) / (2 * a_station)));  
         Sigma_station(j,i) = Sigma_station(i,j);
      }
   }
   using namespace density;
   MVNORM_t<Type> nll_station(Sigma_station);
   res += nll_station(station_effect);
   
   // Comparative tow effects:
   for (int j = 0; j < n_comparative; j++){
      res -= dnorm(comparative_effect[j], mu_comparative, sigma_comparative, true);
   }
   
   // Rational quadratic covariance matrix for wing spread times series:
   matrix<Type> Sigma_t(n_t, n_t); 
   for (int i = 0; i < n_t; i++){
      Sigma_t(i,i) = Type(1);
      for (int j = 0; j < i; j++){
         Type d = abs(Type(i-j)) / rho_t;               
         Sigma_t(i,j) = exp(-a_t * log(1 + (d * d) / (2 * a_t)));  
         Sigma_t(j,i) = Sigma_t(i,j);
      }
   }
   using namespace density;
   MVNORM_t<Type> nll_t(Sigma_t);
   
   // Global Gaussian process:
   res += nll_t(lambda_global);
   
   // Gaussian process by station:
   for (int j = 0; j < n_station; j++){
      res += nll_t(vector<Type>(lambda.col(j)));
   }
      
   // Process model for reference vessel:
   matrix<Type> mu(n_t,n_tow);
   for (int i = 0; i < n_t; i++){
      for (int j = 0; j < n_station; j++){
         mu(i,j) = alpha + station_effect[j] + sigma_process_global * lambda_global[i] + sigma_process * lambda(i,station[j]);   
      }
      for (int j = 0; j < n_comparative; j++){
         mu(i,j+n_station) = alpha + station_effect[station[j+n_station]] + sigma_process_global * lambda_global[i] + sigma_process * lambda(i,station[j+n_station]) + comparative_effect[j];   
      }
   }
       
   // Observation model:
   for (int i = 0; i < n_obs; i++){
      res -= log((Type(1)-p_outlier) * dnorm(width[i], mu(t[i], tow[i]), sigma_obs, false) +
                          p_outlier  * dnorm(width[i], mu(t[i], tow[i]), sigma_obs + sigma_outlier, false)); 
   }

   // Calculate summary statistics:
   vector<Type> swept_area(n_tow);
   vector<Type> swept_area_passive(n_tow);
   vector<Type> mean_width(n_tow);
   vector<Type> mean_width_passive(n_tow);
   swept_area.fill(0);
   swept_area_passive.fill(0);
   mean_width.fill(0);
   mean_width_passive.fill(0);
   for (int i = 0; i < n_t; i++){
      for (int j = 0; j < n_tow; j++){
         swept_area[j]         += active(i,j) * speed(i,j) * mu(i,j);
         swept_area_passive[j] += passive(i,j) * speed(i,j) * mu(i,j);
         mean_width[j]         += (active(i,j) * mu(i,j)) / active.col(j).sum();
         mean_width_passive[j] += (passive(i,j) * mu(i,j)) / passive.col(j).sum();
      }   
   }
      
   // Export results:
   REPORT(swept_area);
   REPORT(swept_area_passive);
   REPORT(mean_width);
   REPORT(mean_width_passive);
      
   return res;
}
