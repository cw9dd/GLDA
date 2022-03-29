data {
    int<lower=2> K;     // Number of components
    int<lower=2> V;     // Number of features
    int<lower=2> M;     // Number of participants
    int<lower=2> N;     // Number of all observations
    vector[V] w[N];     // Observation matrix (N observations by V features)
    int<lower=1,upper=M> pid[N];    // Participant ID for Observation n
    vector<lower=0>[K] alpha;  // Hyperparameter for participant-component distribution theta ~ Dir(alpha)
    real nu; // Hyperparameter for component covariance matices: degrees of freedom for inverse wishart distribution
    cov_matrix[V] psi; // Hyperparameter for component covariance matrices: scale matrix for inverse wishart distribution
    vector[V] mu0; // Hyperparameter for component means 
    real kappa;
}

parameters{
    simplex[K] theta[M];
    vector[V] mu[K];
    cov_matrix[V] sigma[K];
}
    
model{
    for (m in 1:M)
        theta[m] ~ dirichlet(alpha);
    for (k in 1:K){
        sigma[k] ~ inv_wishart(nu, psi); // inverse wishart distribution
        mu[k] ~ multi_normal(mu0, kappa * sigma[k]);
    }
    for (n in 1:N){
        real gamma[K];
        for (k in 1:K)
            gamma[k] = log(theta[pid[n],k]) + multi_normal_lpdf(w[n] | mu[k], sigma[k]);  
        target += log_sum_exp(gamma);
    }
}
