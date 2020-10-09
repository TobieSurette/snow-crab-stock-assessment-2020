#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator()(){
   // Data declarations:
   DATA_IVECTOR(t);           // Time index (n_obs).
   DATA_VECTOR(width);        // Trawl width observations (n_obs).
   DATA_IVECTOR(tow);         // Tow index (n_obs).
   DATA_MATRIX(speed);        // Vessel speed matrix (n x n_tow).
   DATA_MATRIX(active);       // Active trawling phase identifier matrix (n x n_tow).
   DATA_MATRIX(passive);      // Passive trawling phase identifier matrix (n x n_tow).
   DATA_VECTOR(x);            // Sampling station x coordinate.
   DATA_VECTOR(y);            // Sampling station y coordinate.
   
   // Parameter and random effect declarations:
   PARAMETER(alpha);                    // Global mean parameter.
   PARAMETER_VECTOR(tow_effect);        // Tow random intercept effect (n_tow).
   PARAMETER(log_sigma_process_tow);    // Log-scale tow effect process error.
   PARAMETER(log_sigma_obs_tow);        // Log-scale tow effect observation error.
   PARAMETER(log_a_tow);                // Tow effect correlation smoothness parameter.
   PARAMETER(log_rho_tow);              // Tow effect correlation range parameter.
   PARAMETER(log_sigma_process_global); // Log-scale global process standard error.
   PARAMETER(log_sigma_process);        // Log-scale process standard error.
   PARAMETER(log_sigma_obs);            // Log-scale observation standard error.
   PARAMETER(log_sigma_outlier);        // Log-scale outlier standard error.
   PARAMETER(log_rho);                  // Log-scale range correlation parameter.
   PARAMETER(logit_p_outlier);          // Logit-scale proportion of outliers (n_tow).
   PARAMETER_VECTOR(lambda_global);     // Global Autoregressive random effect (n).
   PARAMETER_MATRIX(lambda);            // Autoregressive random effect (n x n_tow).
   PARAMETER(log_a);                    // Gaussian process rational quadratic smoothness parameter.
        
   // Transformed parameters:
   Type sigma_process_tow    = exp(log_sigma_process_tow);    // Tow effect process error.
   Type sigma_obs_tow        = exp(log_sigma_obs_tow);        // Tow effect observtaion error.   
   Type a_tow                = exp(log_a_tow);                // Tow effect shape correlation parameter. 
   Type rho_tow              = exp(log_rho_tow);              // Tow effect range correlation parameter.
   Type sigma_process_global = exp(log_sigma_process_global); // Global process standard error .  
   Type sigma_process        = exp(log_sigma_process);        // Process standard error.
   Type sigma_obs            = exp(log_sigma_obs);            // Observation standard error.
   Type sigma_outlier        = exp(log_sigma_outlier);        // Outlier standard error.
   Type rho                  = exp(log_rho);                  // Range correlation parameter.
     
   // Number of observations:
   int n_obs = width.size();
   int n = lambda.rows();
   int n_tow = tow_effect.size();
   
   // Initialize negative log-likelihood accumulator:
   Type res = 0;
  
   // Proportion of outliers:
   Type p_outlier = Type(1) / (Type(1) + exp(-logit_p_outlier));

   // Rational quadratic spatial prior over tow effects:
   matrix<Type> Sigma_tow(n_tow,n_tow); 
   for (int i = 0; i < n_tow; i++){
      Sigma_tow(i,i) = sigma_process_tow + sigma_obs_tow;
      for (int j = 0; j < i; j++){
         Type d = sqrt((x[i]-x[j]) * (x[i]-x[j]) + (y[i]-y[j]) * (y[i]-y[j])) / rho_tow;     
         Sigma_tow(i,j) = sigma_process_tow * exp(-a_tow * log(1 + (d * d) / (2 * a_tow)));  
         Sigma_tow(j,i) = Sigma_tow(i,j);
      }
   }
   using namespace density;
   MVNORM_t<Type> nll_tow(Sigma_tow);
   res += nll_tow(tow_effect);
   
   // Rational quadratic covariance matrix for wing spread times series:
   matrix<Type> Sigma(n,n); 
   Type a = exp(log_a);
   Type d;
   for (int i = 0; i < n; i++){
      Sigma(i,i) = Type(1);
      for (int j = 0; j < i; j++){
         d = abs(Type(i-j)) / rho;               
         Sigma(i,j) = exp(-a * log(1 + (d * d) / (2 * a)));  
         Sigma(j,i) = Sigma(i,j);
      }
   }
   using namespace density;
   MVNORM_t<Type> nll_exp(Sigma);
   
   // Global Gaussian process:
   res += nll_exp(lambda_global);
   
   // Gaussian process by tow:
   for (int j = 0; j < n_tow; j++){
      res += nll_exp(vector<Type>(lambda.col(j)));
   }
      
   // Process model for reference vessel:
   matrix<Type> mu(n,n_tow);
   for (int i = 0; i < n; i++){
      for (int j = 0; j < n_tow; j++){
         mu(i,j) = alpha + tow_effect[j] + sigma_process_global * lambda_global[i] + sigma_process * lambda(i,j);   
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
   for (int i = 0; i < n; i++){
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
