functions {
  // Winter peak in influenza negative ILI
  vector fmin_vec(vector a, vector b) {
    return 0.5 * (a + b - fabs(a - b));
  }
  vector fmax_vec(vector a, vector b) {
    return 0.5 * (a + b + fabs(a - b));
  }
  
  vector TransRate1_vectorized(real npi, vector NPI1) {
    return exp(-npi * NPI1);
  }
  
  
  vector build_b_spline(real[] t, real[] ext_knots, int ind, int order);
  vector build_b_spline(real[] t, real[] ext_knots, int ind, int order) {
    // INPUTS:
    //    t:          the points at which the b_spline is calculated
    //    ext_knots:  the set of extended knots
    //    ind:        the index of the b_spline
    //    order:      the order of the b-spline
    vector[size(t)] b_spline;
    vector[size(t)] w1 = rep_vector(0, size(t));
    vector[size(t)] w2 = rep_vector(0, size(t));
    if (order==1)
      for (i in 1:size(t)) // B-splines of order 1 are piece-wise constant
        b_spline[i] = (ext_knots[ind] <= t[i]) && (t[i] < ext_knots[ind+1]);
    else {
      if (ext_knots[ind] != ext_knots[ind+order-1])
        w1 = (to_vector(t) - rep_vector(ext_knots[ind], size(t))) /
          (ext_knots[ind+order-1] - ext_knots[ind]);
      if (ext_knots[ind+1] != ext_knots[ind+order])
        w2 = 1 - (to_vector(t) - rep_vector(ext_knots[ind+1], size(t))) /
          (ext_knots[ind+order] - ext_knots[ind+1]);
      // Calculating the B-spline recursively as linear interpolation of two lower-order splines
      b_spline = w1 .* build_b_spline(t, ext_knots, ind, order-1) +
        w2 .* build_b_spline(t, ext_knots, ind+1, order-1);
    }
    return b_spline;
  }
}
data {
  int<lower=0> K; // number of PROVIENCES
  int<lower=0> T; // number of days
  int<lower=0> W; // number of weeks
  vector<lower=0>[K] N; // population size
  int<lower=0> cases[W,K];    // ILI+ PROXY    //
  real NPI1[K,1155];    // NPI
  real C[961,1155];
  int nl[961];
  int part[K+1];
  real phi0;
  real error;
  int num_data;             // number of data points
  int num_knots;            // num of knots
  vector[num_knots] knots;  // the sequence of knots
  int spline_degree;        // the degree of spline (is equal to order - 1)
  real X[num_data];
  row_vector[num_knots+spline_degree-1] a_raw1[K];
  real diversity[W];
}
transformed data {
  matrix[K,K] Mobility[T];
  int num_basis = num_knots + spline_degree - 1; // total number of B-splines
  matrix[num_basis, num_data] B;  // matrix of B-splines
  vector[spline_degree + num_knots] ext_knots_temp;
  vector[2*spline_degree + num_knots] ext_knots; // set of extended knots
  matrix[K,1155] NPI1_matrix = to_matrix(NPI1);
  vector[K] zerovector = rep_vector(0,K);
  real gamma = 1.0/10.0;
  real q = 0.0/1000;
  real sigma = 1.0/5;
  for (t in 1:T) {
    for (k in 1:K) {
      for (j in part[k]:part[k+1]-1){
        Mobility[t,k,nl[j]] = 3.09*C[j,t]/N[k];
      }
    }
  }
  ext_knots_temp = append_row(rep_vector(knots[1], spline_degree), knots);
  ext_knots = append_row(ext_knots_temp, rep_vector(knots[num_knots], spline_degree));
  for (ind in 1:num_basis){
    B[ind,:] = to_row_vector(build_b_spline(X, to_array_1d(ext_knots), ind, spline_degree + 1));
  }
  B[num_knots + spline_degree - 1, num_data] = 1;
}
parameters {
  real<lower=0.00, upper=0.1> thetap;  
  real<lower=0.95,upper=1> S0[2];
  real<lower=0,upper=0.1> E0[2];
  real<lower=0,upper=0.1> I0[2];
  real<lower=0,upper=1> npi;
  vector<lower=0>[K] phi;
  vector<lower=0,upper=3>[3] v;
  row_vector<upper=3.5>[num_basis] a_raw[K];
  vector<lower=0,upper=0.6>[K] p;
  vector<lower=1>[K] sp;
  real<lower=0,upper=1> rate;
  real<lower=0> div_rate;
}
transformed parameters {
  matrix<lower=0>[K,T] S;
  matrix<lower=0>[K,T] E;
  matrix<lower=0>[K,T] I;
  matrix<lower=0>[K,T] R;
  matrix[K,W] PILI;  // weekly simulated data
  matrix<lower=0,upper=200>[K,T] Rt1;
  vector[K] newS;
  vector[K] newE;
  vector[K] newI;
  vector[K] newR;
  matrix[K,T] pILI; // daily simulated data
  vector[K] total;
  vector[K] move_vec;
  vector[K] moveE_vec;
  row_vector[num_basis] a[K];
  matrix[K,num_data] exp_Beta_KT;
  real v_temp;
  vector[K] thetapP;
  vector [K]beta_col;
  
  for (k in 1:K) {
    a[k,] = a_raw[k,];
    exp_Beta_KT[k,] = exp(to_row_vector(a[k,]*B));
  }
  
  // Initial
  for (k in 1:K) {
    Rt1[k,1] = 3;
    if (k==17) {
      S[k,1] = S0[1]; // print("newS:",S[k,1])
      E[k,1] = E0[1];
      I[k,1] = I0[1];
      R[k,1] = 0;
    } else {
      S[k,1] = S0[2]; // print("newS:",S[k,1])
      E[k,1] = E0[2];
      I[k,1] = I0[2];
      R[k,1] = 0;
    }
    total[k] = S[k,1] + E[k,1] + I[k,1] + R[k,1];
    S[k,1] = S[k,1] / total[k];
    E[k,1] = E[k,1] / total[k];
    I[k,1] = I[k,1] / total[k];
    R[k,1] = R[k,1] / total[k];
    pILI[k,1] = sigma * E[k,1];
  }
  PILI = rep_matrix(0, K, W);
  
  // SIR
  for (w in 1:W) {
    for (t in max(2,(w-1)*7+1):(w*7)) {
      thetapP = thetap ./ (1+exp(-rate*NPI1_matrix[,t]));
      v_temp = (v[1] + (t >= 430) * (v[2] - v[1]) + (t >= 730) * (v[3] - v[2]))*exp(div_rate*diversity[w]);
      
      // 批量计算beta (向量化)
      beta_col = exp_Beta_KT[,t] .* TransRate1_vectorized(npi, NPI1_matrix[,t]);
      move_vec = (1-thetapP) .*(Mobility[t-1] * I[:,t-1]) - (1-thetapP) .*(rep_row_vector(1.0, K)*Mobility[t-1])' .* I[:,t-1];
      moveE_vec = Mobility[t-1] * E[:,t-1] - (rep_row_vector(1.0, K)*Mobility[t-1])' .* E[:,t-1];

      // Vectorized updates for all regions k at time t-1
      newS = -(q * R[:, t-1] - (1 - p) .* S[:, t-1] .* beta_col .* (I[:, t-1] + move_vec) - sp .* p .* S[:, t-1] .* beta_col .* (I[:, t-1] + move_vec + v_temp * square(I[:, t-1] + move_vec)));
      S[:, t] = fmax_vec(zerovector, S[:, t-1] - newS);

      newE = -((1 - p) .* S[:, t-1] .* beta_col .* (I[:, t-1] + move_vec) + moveE_vec + sp .* p .* S[:, t-1] .* beta_col .* (I[:, t-1] + move_vec + v_temp * square(I[:, t-1] + move_vec)) - sigma * E[:, t-1]);
      E[:, t] = fmax_vec(zerovector, E[:, t-1] - newE);

      newI = -(sigma * E[:, t-1] - (gamma) * I[:, t-1]);
      I[:, t] = fmax_vec(zerovector, I[:, t-1] - newI);

      newR = -(gamma * I[:, t-1] - (q) * R[:, t-1]);
      R[:, t] = fmax_vec(zerovector, R[:, t-1] - newR);

      // Vectorized normalization
      total = S[:, t] + E[:, t] + I[:, t] + R[:, t];
      S[:, t] = S[:, t] ./ total;
      E[:, t] = E[:, t] ./ total;
      R[:, t] = R[:, t] ./ total;
      I[:, t] = I[:, t] ./ total;

      Rt1[:, t] = S[:, t] .* (1 - p) .* beta_col / gamma + sp .* S[:, t] .* p .* beta_col / gamma;
      pILI[:, t] = sigma * E[:, t];
      PILI[:, w] = PILI[:, w] + pILI[:, t]  .* thetapP;
    }
  }
}
model {
  thetap ~  normal(0.0065, 0.001);
  rate ~ normal(0.03165, 0.003);
  
  v ~ normal(0, 1);
  p ~ normal(0, 0.5);
  sp ~ normal(1, 1);
  div_rate ~ normal(0, 0.5);

  S0[1] ~ lognormal(log(0.985), 0.1);
  E0[1] ~ lognormal(log(0.0055), 0.1);
  I0[1] ~ lognormal(log(0.0095), 0.1);
  S0[2] ~ lognormal(log(0.9999), 0.01);
  E0[2] ~ lognormal(log(0.00005), 0.01);
  I0[2] ~ lognormal(log(0.00005), 0.01);
  
  npi ~ normal(0.00886, 0.005);
  phi ~ exponential(phi0); 
  
  for (k in 1:K) {
    a_raw[k,] ~ normal(a_raw1[k,], 0.05);
    cases[, k] ~ neg_binomial_2((PILI[k, ] * N[k] + error), 1.0/phi[k]);     
  }
}

generated quantities {
  real pred_cases[K,W];
  real log_lik[K,W];
  for (k in 1:K){ 
    for (w in 1:W) { 
      pred_cases[k,w] = neg_binomial_2_rng(PILI[k,w] * N[k] + error, 1.0/phi[k]);
      log_lik[k,w] = neg_binomial_2_lpmf(cases[w,k] | (PILI[k,w] * N[k] + error), 1.0/phi[k]);
    }
  }
}
