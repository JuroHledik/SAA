################################################################################
# INSTALL DEPENDENCIES
################################################################################
# install.packages("cli")
# install.packages("cramer")
# install.packages("data.table")
# install.packages("devtools")
# install.packages("gclus")
# install.packages("MASS")
# install.packages("moments")
# install.packages("PearsonDS")
# install.packages("quadprog")
# install.packages("readxl")
# install.packages("Rcpp")
# install.packages("Rfast")
# install.packages('stringr')
# install.packages('TSP')
# install.packages("VineCopula")
# install.packages("DT")
# install.packages("randomcoloR")
# install.packages("tidyverse")
# install.packages('rsconnect')
# install.packages('AsioHeaders')
# install.packages('shinyloadtest')
# install.packages('websocket')
# install.packages("GGally")
# install.packages("memuse")
# devtools::install_github("krlmlr/ulimit")

# ################################################################################
# # DEPENDENCIES
# ################################################################################
library(cli)
library(cramer)
library(data.table)
library(gclus)
library(moments)
library(PearsonDS)
library(quadprog)
library(readxl)
library(Rfast)
library(stringr)
library(TSP)
library(VineCopula)

source(paste0(path_root, "code/R/config.R"))

portfolio_optimization <- function(optimization_inputs){
  constraints_df = optimization_inputs$constraints_df
  theta1 = optimization_inputs$theta1
  theta2 = optimization_inputs$theta2
  theta3 = optimization_inputs$theta3
  theta4 = optimization_inputs$theta4
  N = optimization_inputs$N
  N_visual = optimization_inputs$N_visual
  maturity = optimization_inputs$maturity
  alpha = optimization_inputs$alpha
  Omega = optimization_inputs$Omega
  min_return = optimization_inputs$min_return
  min_cvar_return = optimization_inputs$min_cvar_return
  Lambda = optimization_inputs$Lambda
  alpha_cvar_total = optimization_inputs$alpha_cvar_total
  frequency = optimization_inputs$frequency
  return_model = optimization_inputs$return_model
  
  theta = c(theta1, theta2, theta3, theta4)
  
  print(Lambda)
  #If we don't care about variance at all, we should add an epsilon amount to theta2, otherwise it's a linear program and quadprog gets bitchy about it.
  if (theta[2]==0) {
    theta[2] = theta[2] + eps
  }
  ################################################################################
  # Set the maturity
  ################################################################################

  maturity = determine_maturity(maturity,frequency)

  ################################################################################
  # LOAD simulated returns
  ################################################################################
  if (return_model=="Gaussian") {
    load(file = paste0(path_model_output_simulated_returns,frequency,"/","gaussian/df_maturity", maturity, ".Rdata")) 
    df_sim = df_sim_MG
  } else {
    if (return_model=="Pearson") {
      load(file = paste0(path_model_output_simulated_returns,frequency,"/","vine_copula/df_maturity", maturity, ".Rdata"))
    }
  }
  N_max = nrow(df_sim)
  df_sim = df_sim[sample(N_max, N), ]
  N = nrow(df_sim)
  K = ncol(df_sim)
  asset_names = names(df_sim)
  asset_names_no_escape = str_replace(asset_names,"/", " ")

  ################################################################################
  # Initiate investor preferences' options
  ################################################################################

  # Manually set investor constraints:

  # lower_limit_amounts = numeric(K)
  # upper_limit_amounts = numeric(K) + Inf
  # 
  # lower_limit_percentages = numeric(K)
  # upper_limit_percentages = numeric(K) + 1
  # 
  # lower_limit_amounts[1] = 1500
  # # lower_limit_amounts[3] = 8500
  # 
  # upper_limit_amounts[1] = 1500
  # # upper_limit_amounts[3] = 8500
  # 
  # lower_limit_percentages[2] =  0.05
  # 
  # upper_limit_percentages[2] = 0.05
  # 
  # lower_limit_percentages[4] =  0.03
  # lower_limit_percentages[5] =  0.026

  # Set current manual constraints:
  # Gold, US Gov, ASW, Equity, China, Inflation bonds, MBS, EM:

  # lower_limit_amounts = c(1500, 570, 8500, 350, 300, 0, 0, 0)
  # upper_limit_amounts = c(1500, 570, 8500, Inf, Inf, Inf, Inf, Inf)
  #
  # lower_limit_percentages = c(0.13, 0.05, 0.739, 0.03, 0.026, 0, 0, 0)
  # upper_limit_percentages = c(0.13, 0.05, 0.739, 1, 1, 1, 1, 1)
  
  lower_limit_percentages = as.vector(constraints_df$perc_min)
  upper_limit_percentages = as.vector(constraints_df$perc_max)
  lower_limit_amounts = as.vector(constraints_df$vol_min)
  upper_limit_amounts = as.vector(constraints_df$vol_max)
  v = as.vector(constraints_df$vol_prev)
  
  Omega_prev = sum(v)
  v = v/Omega_prev
  
  
  utility_functions = c("MaxExpReturn","MinVariance","MinExpShortfallAtLevelAMaturityB")
  investor_constraints = c("ExpShortfallAtLevelXMaturityYLowerThanZ","MinExpReturnX","MaxVarianceX")

  hard_constraints_percentage_df = df_sim[0,]
  hard_constraints_percentage_df <- rbind(NA * t(data.frame(numeric(K))),hard_constraints_percentage_df)
  hard_constraints_percentage_df <- rbind(NA * t(data.frame(numeric(K))),hard_constraints_percentage_df)
  hard_constraints_percentage_df <- rbind(NA * t(data.frame(numeric(K))),hard_constraints_percentage_df)
  colnames(hard_constraints_percentage_df) = asset_names
  rownames(hard_constraints_percentage_df) = c("equal_to", "lower_limit", "upper_limit")

  hard_constraints_amount_df = df_sim[0,]
  hard_constraints_amount_df <- rbind(t(data.frame(numeric(K)*NA)),hard_constraints_amount_df)
  hard_constraints_amount_df <- rbind(t(data.frame(numeric(K)*NA)),hard_constraints_amount_df)
  hard_constraints_amount_df <- rbind(NA * t(data.frame(numeric(K))),hard_constraints_amount_df)
  colnames(hard_constraints_amount_df) = asset_names
  rownames(hard_constraints_amount_df) = c("equal_to", "lower_limit", "upper_limit")

  # Assign data to constraint dataframes:
  for (k in 1:K) {
    if (lower_limit_amounts[k] == upper_limit_amounts[k]) {
      hard_constraints_amount_df["equal_to",k] = lower_limit_amounts[k]
      hard_constraints_amount_df["lower_limit",k] = 0
      hard_constraints_amount_df["upper_limit",k] = Inf
    } else {
      hard_constraints_amount_df["lower_limit",k] = 0
      hard_constraints_amount_df["upper_limit",k] = Inf
      if (lower_limit_amounts[k] > 0) {
        hard_constraints_amount_df["lower_limit",k] = lower_limit_amounts[k]
      }
      if (upper_limit_amounts[k]!=Inf) {
        hard_constraints_amount_df["upper_limit",k] = upper_limit_amounts[k]
      }
    }
    if (lower_limit_percentages[k] == upper_limit_percentages[k]) {
      hard_constraints_percentage_df["equal_to",k] = lower_limit_percentages[k]
      hard_constraints_percentage_df["lower_limit",k] = 0
      hard_constraints_percentage_df["upper_limit",k] = 1
    } else {
      hard_constraints_percentage_df["lower_limit",k] = 0
      hard_constraints_percentage_df["upper_limit",k] = 1
      if (lower_limit_percentages[k] > 0) {
        hard_constraints_percentage_df["lower_limit",k] = lower_limit_percentages[k]
      }
      if (upper_limit_percentages[k] != 1) {
        hard_constraints_percentage_df["upper_limit",k] = upper_limit_percentages[k]
      }
    }

  }

  constraints = list()
  
  
  if (min_cvar_return>-1) {
    constraint = list(name = "ExpShortfallReturnGreaterThanX", params = c(alpha, min_cvar_return))
    constraints = append(constraints, list(constraint))
  }

  if (!is.na(Lambda) & !is.na(Omega)) {
    if (Lambda<Omega) {
      constraint = list(name = "ExpShortfallAmountLowerThanX", params = c(alpha, Lambda))
      constraints = append(constraints, list(constraint))
    }  
  }

  if (min_return>-1) {
    constraint = list(name = "MinExpReturnX", params = c(min_return))
    constraints = append(constraints, list(constraint))
  }
  ################################################################################
  # Formulate the problem as quadratic programming
  ################################################################################
  # Determine whether we need to use the shortfall variables t, z_i:
  shortfall_variables = FALSE
  if (!is.na(Lambda) & !is.na(Omega)) {
    if (Lambda<Omega) {
      shortfall_variables = TRUE 
    }
  }
  if ((theta[3]>0) | (min_cvar_return>-1)) {
   shortfall_variables = TRUE 
  }
  print(constraints)
  solvable = TRUE
  tryCatch(
    {
      #Get the expected returns and var-covar matrix:
      df = df_sim
      x_bar = as.numeric(colMeans(df))
      Sigma = as.matrix(cov(df))
      
      #Investor weights:
      # theta = c(1,1+eps,100000)
      
      #Transformation to standard quadprog notation:
      d = theta[1] * x_bar + theta[4] * v
      D = theta[2] * Sigma + theta[4] * diag(K)
      if (shortfall_variables==TRUE) {
        d = append(d, (numeric(N)+theta[3])/(N*alpha))
        d = append(d, theta[3])
        D = cbind(D, matrix(0,K,N+1))
        D = rbind(D, cbind(matrix(0,N+1,K), eps * diag(N+1))) # The eps here is to make the matrix positive semi-definite. Otherwise quadprog is unhappy. It's ok because by definition, it is positive semi-definite so no problem.
      }
      # CONSTRAINTS
      #############
      
      #Initiate matrix A of linear constraints:
      #Weights sum up to 1, one equality constraint:
      
      A = matrix(1,1,K)
      b = c(1)
      
      # Weights equal to constraint:
      number_of_equality_constraints = 1
      for (k in 1:K) {
        if (!is.na(hard_constraints_percentage_df["equal_to",k])) {
          temp = matrix(0,1,K)
          temp[1,k] = 1
          A = rbind(A,temp)
          b = append(b,hard_constraints_percentage_df["equal_to",k])
          number_of_equality_constraints = number_of_equality_constraints + 1
        }
      }
      
      # Amounts equal to constraint:
      if (!is.na(Omega)) { # If portfolio size is set, assign weights proportionally:
        for (k in 1:K) {
          if (!is.na(hard_constraints_amount_df["equal_to",k])) {
            temp = matrix(0,1,K)
            temp[1,k] = 1
            A = rbind(A,temp)
            b = append(b,hard_constraints_amount_df["equal_to",k]/Omega)
            number_of_equality_constraints = number_of_equality_constraints + 1
          }
        }
      } else { #Otherwise, set the dependencies between assets:
        for (k1 in 1:K) {
          if (!is.na(hard_constraints_amount_df["equal_to",k1])) {
            break # Finds the first asset with amount equal to constraint
          }
        }
        for (k2 in (k1+1):K) {
          if (!is.na(hard_constraints_amount_df["equal_to",k2])) {
            temp = matrix(0,1,K)
            temp[1,k1] = hard_constraints_amount_df["equal_to",k2]
            temp[1,k2] = - hard_constraints_amount_df["equal_to",k1]
            A = rbind(A,temp)
            b = append(b,0)
            number_of_equality_constraints = number_of_equality_constraints + 1
          }
        }
      }
      
      # Amounts bigger than constraint, ONLY IF SIGMA IS SET MANUALLY, OTHERWISE NOT POSSIBLE:
      if (!is.na(Omega)) {
        for (k in 1:K) {
          if (hard_constraints_amount_df["lower_limit",k] > 0) {
            temp = matrix(0,1,K)
            temp[1,k] = -1
            A = rbind(A,temp)
            b = append(b,-hard_constraints_amount_df["lower_limit",k]/Omega)
          }
        }
      }
      
      # Amounts lower than constraint, ONLY IF SIGMA IS SET MANUALLY, OTHERWISE NOT POSSIBLE:
      if (!is.na(Omega)) {
        for (k in 1:K) {
          if (hard_constraints_amount_df["upper_limit",k] != Inf) {
            temp = matrix(0,1,K)
            temp[1,k] = 1
            A = rbind(A,temp)
            b = append(b,hard_constraints_amount_df["upper_limit",k]/Omega)
          }
        }
      }
      
      #Weights non-negative:
      A = rbind(A,-diag(K))
      b = append(b, 0*numeric(K))
      
      #Weights bigger than lower bound:
      A = rbind(A,-diag(K))
      b = append(b,-as.numeric(hard_constraints_percentage_df["lower_limit",]))
      
      #Weights lower than upper bound:
      A = rbind(A,diag(K))
      b = append(b,as.numeric(hard_constraints_percentage_df["upper_limit",]))
      
      #Expected return greater than:
      for (constraint in constraints) {
        if (constraint$name == "MinExpReturnX") {
          min_return = constraint$params[1]
          A = rbind(A, -x_bar)
          b = append(b, -min_return)
        }
      }
      
      L = length(b)
      
      # If also minimizing shortfall, Introduce variables z,t that are relevant for shortfall minimization:
      if (shortfall_variables==TRUE) {
        # Add z,t variables:
        A = cbind(A, matrix(0,L,N+1))
        
        # z_i <= 0
        A = rbind(A, cbind(matrix(0,N,K), diag(N), matrix(0,N,1)))
        b = append(b, numeric(N))
        
        # t + z_i <= wTx_i
        for (n in 1:N) {
          x = df[n,]
          temp = matrix(0,1,N)
          temp[1,n] = 1
          A = rbind(A,as.matrix(cbind(-x, temp,1)))
          b = append(b,0)
        }
      }
      
      #Shortfall return greater than:
      for (constraint in constraints) {
        if (constraint$name == "ExpShortfallReturnGreaterThanX") {
          alpha = constraint$params[1]
          min_cvar_return = constraint$params[2]
          A = rbind(A, append(matrix(0,1,K), append(-(numeric(N)+1)/(N*alpha), -1)))
          b = append(b, -min_cvar_return)
        }
      }
      
      #Shortfall amount lower than:
      for (constraint in constraints) {
        if (constraint$name == "ExpShortfallAmountLowerThanX") {
          alpha = constraint$params[1]
          Lambda = constraint$params[2]
          A = rbind(A, append(matrix(0,1,K), append(-(numeric(N)+1)/(N*alpha), -1)))
          b = append(b, Lambda/Omega)
        }
      }
    },
    error = function(e) {
      solvable <<- FALSE
    }
  )
  


  ################################################################################
  # Solve it
  ################################################################################

  if (solvable==TRUE) {
    tryCatch( { test <- solve.QP(Dmat = D, dvec = d, Amat = -t(A), bvec = -b, meq=number_of_equality_constraints, factorized = FALSE) }
              , error = function(e) {solvable <<- FALSE})
    # test = solve.QP(Dmat = D, dvec = d, Amat = -t(A), bvec = -b, meq=number_of_equality_constraints, factorized = FALSE)
    # test = ipop(-d, D, A, -1/eps*(numeric(L)+1), -1/eps*(numeric(K)+1), 1/eps*(numeric(K)+1), 1/eps*(numeric(L)+1) + b)
    
    ################################################################################
    # Evaluate the results
    ################################################################################
    
    #f the problem is feasible, compute all relevant characteristics:
    if (solvable==TRUE) {
      w = test$solution[1:K]
      cutoff = 3
      
      temp = sample(N_max, N_visual)
      if (return_model=="Gaussian") {
        load(file = paste0(path_model_output_simulated_returns,frequency,"/","gaussian/df_maturity", maturity, ".Rdata")) 
        df_sim = df_sim_MG
        df_sim_pa = df_sim_MG_pa
        df_sim = df_sim[temp, ]
        df_sim_pa = df_sim_pa[temp, ]
      } else {
        if (return_model=="Pearson") {
          load(file = paste0(path_model_output_simulated_returns,frequency,"/","vine_copula/df_maturity", maturity, ".Rdata"))
          df_sim = df_sim[temp, ]
          df_sim_pa = df_sim_pa[temp, ]
        }
      }
      
      x_bar = as.numeric(colMeans(df_sim))
      Sigma = as.matrix(cov(df_sim))
      
      expected_return = w %*% x_bar
      variance = w %*% Sigma %*% w
      
      Omega = NA
      for (k in 1:K) {
        if (!is.na(hard_constraints_amount_df["equal_to", k])) {
          Omega = hard_constraints_amount_df["equal_to", k] / w[k]
          break
        }
        if (k==K) {
          Omega = Omega_prev
        }
      }
      
      # If the Omega is extremely large, it means that the algorithm is just trying to compensate for fixed amount constraint that it DOES NOT want there.
      if (!is.na(Omega)) {
        if (Omega>1/eps | Omega< -1/eps) {
          Omega = NA
        }
      }
      
      x = as.matrix(df_sim) %*% w
      c_var_return = mean(sort(x)[1:floor(alpha_cvar_total*N_visual)])
      c_var_amount = - c_var_return * Omega
      P_neg_return = sum((x)<0)/N_visual
      
      density_x = density(x, from = expected_return - cutoff * sqrt(variance), to = expected_return + cutoff * sqrt(variance))
      
      # per annum:
      x_bar_pa = as.numeric(colMeans(df_sim_pa))
      Sigma_pa = as.matrix(cov(df_sim_pa))
      expected_return_pa = w %*% x_bar_pa
      variance_pa = w %*% Sigma_pa %*% w
      
      x_pa = as.matrix(df_sim_pa) %*% w
      density_x_pa = density(x_pa, from = expected_return_pa - cutoff * sqrt(variance_pa), to = expected_return_pa + cutoff * sqrt(variance_pa))
      
      for (k in 1:K) {
        print(paste0(Sys.time(), ": OPTIMAL PORTFOLIO: w",k , " = ", round(w[k]*100,2), "%"))
        w[k]=round(w[k],4)
      }
      print(paste0(Sys.time(), ": OPTIMAL PORTFOLIO: expected_return = ", expected_return))
      print(paste0(Sys.time(), ": OPTIMAL PORTFOLIO: expected_return p.a.= ", expected_return_pa))
      print(paste0(Sys.time(), ": OPTIMAL PORTFOLIO: variance = ", variance))
      print(paste0(Sys.time(), ": OPTIMAL PORTFOLIO: cvar_return = ", c_var_return))
      print(paste0(Sys.time(), ": OPTIMAL PORTFOLIO: cvar_vol = ", round(c_var_amount,2)))
      print(paste0(Sys.time(), ": OPTIMAL PORTFOLIO: Omega = ", round(Omega)))
      print(paste0(Sys.time(), ": OPTIMAL PORTFOLIO: P_neg_return = ", round(100 * P_neg_return,2), "%"))
      print(paste0(Sys.time(), ": OPTIMAL PORTFOLIO: expected_profit = ", expected_return * Omega))
      
      
      results = list()
      results$density_x = density_x
      results$w = w
      results$Omega = Omega
      results$variance = variance
      results$expected_return = expected_return
      results$expected_profit = expected_return * Omega
      results$cvar_return = c_var_return
      results$cvar_vol = c_var_amount
      results$P_neg_return = P_neg_return
      results$expected_return_pa = expected_return_pa
      results$density_x_pa = density_x_pa
      results$solvable = TRUE
      
    } else {
      results = list()
      results$solvable = FALSE
      results$error_type = "unfeasible"
    }
  } else {
    results = list()
    results$solvable = FALSE
    results$error_type = "not_enough_RAM"
  }
  return(results)
}
# results = portfolio_optimization(c(1,2,3), 100, 100, 1, 0.05, 11520, 0)