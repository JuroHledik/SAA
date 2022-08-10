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
# install.packages("lpSolve")
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
library(lpSolve)
library(pracma)

source(paste0(path_root, "code/R/config.R"))

portfolio_optimization <- function(optimization_inputs){
  
  options(max.print=999999)
  
  constraints_df = optimization_inputs$constraints_df
  theta1 = optimization_inputs$theta1
  theta2 = optimization_inputs$theta2
  theta3 = optimization_inputs$theta3
  theta4 = optimization_inputs$theta4
  N = optimization_inputs$N
  N_visual = optimization_inputs$N_visual
  
  alpha = optimization_inputs$alpha
  Omega = optimization_inputs$Omega
  min_return = optimization_inputs$min_return
  min_cvar_return = optimization_inputs$min_cvar_return
  Lambda = optimization_inputs$Lambda
  alpha_cvar_total = optimization_inputs$alpha_cvar_total
  
  return_model = optimization_inputs$return_model
  include_in_shortfall_optimization = optimization_inputs$include_in_shortfall_optimization
  secondary_horizon = optimization_inputs$secondary_horizon
  shortfall_variables = optimization_inputs$shortfall_variables
  shortfall_variables_secondary_horizon = optimization_inputs$shortfall_variables_secondary_horizon

  if (return_model!="Custom") {
    frequency = optimization_inputs$frequency
    maturity = optimization_inputs$maturity
  } else {
    frequency = "custom"
    maturity = "custom"
  }
  
  if (secondary_horizon=="yes") {
    return_model_secondary_horizon = optimization_inputs$return_model_secondary_horizon
    min_return_secondary_horizon = optimization_inputs$min_return_secondary_horizon
    min_cvar_return_secondary_horizon = optimization_inputs$min_cvar_return_secondary_horizon
    Lambda_secondary_horizon = optimization_inputs$Lambda_secondary_horizon
    alpha_secondary_horizon = optimization_inputs$alpha_secondary_horizon
    shortfall_variables_secondary_horizon = optimization_inputs$shortfall_variables_secondary_horizon
    
    if (return_model_secondary_horizon!="Custom") {
      maturity_secondary_horizon = optimization_inputs$maturity_secondary_horizon
    } else {
      maturity_secondary_horizon = "custom"
    }
    
    #Determine the maturity for the secondary horizon:
    maturity_secondary_horizon = determine_maturity(maturity_secondary_horizon, frequency)
  } 

  theta = c(theta1, theta2, theta3, theta4)

  problem_is_linear = TRUE
  if (theta[2]!=0 || theta[4]!=0) {
    problem_is_linear = FALSE
  }
  
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
    } else {
      if (return_model=="Custom") {
        load(file = paste0(path_model_output_user_imported_returns,"/user_imported_returns.Rdata"))
      }
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
  
  lower_limit_percentages = as.numeric(as.vector(constraints_df$perc_min))
  upper_limit_percentages = as.numeric(as.vector(constraints_df$perc_max))
  lower_limit_amounts = as.numeric(as.vector(constraints_df$vol_min))
  upper_limit_amounts = as.numeric(as.vector(constraints_df$vol_max))
  v = as.numeric(as.vector(constraints_df$vol_prev))
  
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
  hard_constraints_amount_df <- rbind(t(data.frame(numeric(K)*NA)),hard_constraints_amount_df)
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
  
  # Secondary horizon constraints:
  if (secondary_horizon=="yes") {
    if (min_cvar_return_secondary_horizon>-1) {
      constraint = list(name = "ExpShortfallReturnGreaterThanX_secondary_horizon", params = c(alpha_secondary_horizon, min_cvar_return_secondary_horizon))
      constraints = append(constraints, list(constraint))
    }
    
    if (!is.na(Lambda_secondary_horizon) & !is.na(Omega)) {
      if (Lambda_secondary_horizon<Omega) {
        constraint = list(name = "ExpShortfallAmountLowerThanX_secondary_horizon", params = c(alpha_secondary_horizon, Lambda_secondary_horizon))
        constraints = append(constraints, list(constraint))
      }  
    }
    
    if (min_return_secondary_horizon>-1) {
      constraint = list(name = "MinExpReturnX_secondary_horizon", params = c(min_return_secondary_horizon))
      constraints = append(constraints, list(constraint))
    }    
  }
  ################################################################################
  # Formulate the problem as quadratic programming
  ################################################################################
  solvable = TRUE
  tryCatch(
    {
      #Get the expected returns and var-covar matrix:
      df = df_sim
      x_bar = as.numeric(colMeans(df))
      Sigma = as.matrix(cov(df))

      #Transformation to standard quadprog notation:
      d = theta[1] * x_bar + theta[4] * v
      D = theta[2] * Sigma + theta[4] * diag(K)
      if (shortfall_variables==TRUE) {
        d = append(d, (numeric(N)+theta[3])/(N*alpha))
        d = append(d, theta[3])
        D = cbind(D, matrix(0,K,N+1))
        D = rbind(D, cbind(matrix(0,N+1,K), eps * diag(N+1))) # The eps here is to make the matrix positive semi-definite. Otherwise quadprog is unhappy. It's ok because by definition, it is positive semi-definite so no problem.
      }
      
      if (secondary_horizon=="yes") {
        if (shortfall_variables_secondary_horizon==TRUE) {
          d = append(d, numeric(N+1))
          if (shortfall_variables==FALSE) {
            D = cbind(D, matrix(0,K,N+1))
            D = rbind(D, cbind(matrix(0,N+1,K), eps * diag(N+1))) # The eps here is to make the matrix positive semi-definite. Otherwise quadprog is unhappy. It's ok because by definition, it is positive semi-definite so no problem.
          } else {
            D = cbind(D, matrix(0, K + N + 1, N + 1))
            D = rbind(D, cbind(matrix(0, N + 1, K + N + 1), eps * diag(N +1)))
          }
        }
      }
      
      #If the secondary horizon constraint is used, get the expected returns:
      if (secondary_horizon=="yes") {
        if (return_model_secondary_horizon=="Gaussian") {
          load(file = paste0(path_model_output_simulated_returns,frequency,"/","gaussian/df_maturity", maturity_secondary_horizon, ".Rdata")) 
          df_sim = df_sim_MG
        } else {
          if (return_model_secondary_horizon=="Pearson") {
            load(file = paste0(path_model_output_simulated_returns,frequency,"/","vine_copula/df_maturity", maturity_secondary_horizon, ".Rdata"))
          } else {
            if (return_model_secondary_horizon=="Custom") {
              load(file = paste0(path_model_output_user_imported_returns,"/user_imported_returns_secondary.Rdata"))
            }
          }
        }
        N_max = nrow(df_sim)
        df_sim = df_sim[sample(N_max, N), ]
        df_secondary_horizon = df_sim
        x_bar_secondary_horizon = as.numeric(colMeans(df_secondary_horizon))
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
      
      #Expected return greater than (secondary horizon):
      if (secondary_horizon=="yes") {
        for (constraint in constraints) {
          if (constraint$name == "MinExpReturnX_secondary_horizon") {
            min_return = constraint$params[1]
            A = rbind(A, -x_bar_secondary_horizon)
            b = append(b, -min_return)
          }
        }
      }

      # If also minimizing shortfall, Introduce variables z,t that are relevant for shortfall minimization:
      L = length(b)
      if (shortfall_variables==TRUE) {
        # Add z,t variables:
        A = cbind(A, matrix(0,L,N+1))
        
        # z_i <= 0
        A = rbind(A, cbind(matrix(0,N,K), diag(N), matrix(0,N,1)))
        b = append(b, numeric(N))
        
        # t + z_i <= wTx_i
        for (n in 1:N) {
          # If 
          x = df[n,] * include_in_shortfall_optimization
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

     
      
      if (secondary_horizon=="yes") {
        # If shortfall constraint is included in the secondary horizon, introduce variables z,t also for the secondary horizon:
        L = length(b)
        if (shortfall_variables_secondary_horizon==TRUE) {
          print('A')
          # Add z,t variables:
          A = cbind(A, matrix(0,L,N+1))
          
          if (shortfall_variables==FALSE) {
            # z_i <= 0
            A = rbind(A, cbind(matrix(0, N, K), diag(N), matrix(0,N,1)))
            b = append(b, numeric(N))
            
            # t + z_i <= wTx_i
            for (n in 1:N) {
              x = df_secondary_horizon[n,] * include_in_shortfall_optimization
              temp = matrix(0,1,N)
              temp[1,n] = 1
              A = rbind(A,as.matrix(cbind(-x, temp,1)))
              b = append(b,0)
            }
          } else {
            print('B')
            # z_i <= 0
            A = rbind(A, cbind(matrix(0,N,K + N + 1), diag(N), matrix(0,N,1)))
            b = append(b, numeric(N))
            
            # t + z_i <= wTx_i
            for (n in 1:N) {
              x = df_secondary_horizon[n,] * include_in_shortfall_optimization
              temp = matrix(0,1,N)
              temp[1,n] = 1
              A = rbind(A,as.matrix(cbind(-x, matrix(0,1,N+1), temp,1)))
              b = append(b,0)
            }
          }
        }
        
        #Shortfall return greater than (secondary horizon):
        for (constraint in constraints) {
          if (constraint$name == "ExpShortfallReturnGreaterThanX_secondary_horizon") {
            alpha = constraint$params[1]
            min_cvar_return = constraint$params[2]
            if (shortfall_variables==FALSE) {
              A = rbind(A, append(matrix(0,1,K), append(-(numeric(N)+1)/(N*alpha), -1)))
              b = append(b, -min_cvar_return)
            } else {
              A = rbind(A, append(matrix(0,1,K + N + 1), append(-(numeric(N)+1)/(N*alpha), -1)))
              b = append(b, -min_cvar_return)
            }
          }
        }
        
        #Shortfall amount lower than (secondary horizon):
        for (constraint in constraints) {
          if (constraint$name == "ExpShortfallAmountLowerThanX_secondary_horizon") {
            alpha = constraint$params[1]
            Lambda = constraint$params[2]
            if (shortfall_variables==FALSE) {
              A = rbind(A, append(matrix(0,1,K), append(-(numeric(N)+1)/(N*alpha), -1)))
              b = append(b, Lambda/Omega)
            } else {
              A = rbind(A, append(matrix(0,1,K + N + 1), append(-(numeric(N)+1)/(N*alpha), -1)))
              b = append(b, Lambda/Omega)
            }
          }
        }  
      }
      
      
    },
    error = function(e) {
      print(Omega)
      print(hard_constraints_amount_df)
      print(e)
      solvable <<- FALSE
    }
  )

  ################################################################################
  # Solve it
  ################################################################################
  if (solvable==TRUE) {
    #Determine whether we are solving a quadratic or a linear program:
    # if (problem_is_linear==FALSE) {
      #QUADRATIC
      #Scaling to avoid overflow errors:
      sc <- norm(D,"2")
      
      print('matrix D')
      print(D)
      print('vector d')
      print(d)
      print('matrix A')
      print(-t(A))
      print('vector b')
      print(-b)
      print('meq')
      print(number_of_equality_constraints)
      
      
      tryCatch( { test <- solve.QP(Dmat = D/sc, dvec = d/sc, Amat = -t(A), bvec = -b, meq=number_of_equality_constraints, factorized = FALSE) }
                , error = function(e) {
                  print(e)
                  solvable <<- FALSE
                }
      )  
    # } else {
    #   #LINEAR
    #   #Set constraints:
    #   const.dir <- vector( "character" , nrow(A))
    #   for (i in 1:nrow(A)) {
    #     if (i<=number_of_equality_constraints) {
    #       const.dir[i] = "="  
    #     } else {
    #       const.dir[i] = "<="  
    #     }
    #   }
    #   print(const.dir)
    #   #Optimize
    #   print('bleble')
    #   print(solvable)
    #   print(A)
    #   print(b)
    #   tryCatch( { test = lpSolve::lp(direction="min", objective.in=-d, const.mat = A, const.dir = const.dir, const.rhs = b) }
    #             , error = function(e) {
    #               # print(e)
    #               solvable <<- FALSE
    #             }
    #   )  
    #   print(test)
    # }
    
    # test = solve.QP(Dmat = D, dvec = d, Amat = -t(A), bvec = -b, meq=number_of_equality_constraints, factorized = FALSE)
    # test = ipop(-d, D, A, -1/eps*(numeric(L)+1), -1/eps*(numeric(K)+1), 1/eps*(numeric(K)+1), 1/eps*(numeric(L)+1) + b)

    print(solvable)    
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
        } else {
          if (return_model=="Custom") {
            load(file = paste0(path_model_output_user_imported_returns,"user_imported_returns.Rdata"))
            df_sim = df_sim[temp, ]
            df_sim_pa = df_sim_pa[temp, ]
          }
        }
      }
      
      x_bar = as.numeric(colMeans(df_sim))
      Sigma = as.matrix(cov(df_sim))
      
      expected_return = w %*% x_bar
      variance = w %*% Sigma %*% w
      
      if (is.na(Omega)) {
        for (k in 1:K) {
          if (!is.na(hard_constraints_amount_df["equal_to", k])) {
            Omega = hard_constraints_amount_df["equal_to", k] / w[k]
            break
          }
          if (k==K) {
            Omega = Omega_prev
          }
        }  
      }

      # If the Omega is extremely large, it means that the algorithm is just trying to compensate for fixed amount constraint that it DOES NOT want there.
      if (!is.na(Omega)) {
        if (Omega>1/eps | Omega< -1/eps) {
          Omega = NA
        }
      }
      
      df_sim_matrix = as.matrix(df_sim)
      
      #Create a portfolio without asset classes that are not included in shortfall optimization:
      w_2 = (w * include_in_shortfall_optimization) / sum(w)
      
      # x is the vector of historical returns if we had portfolio w
      x = df_sim_matrix %*% w
      
      # x_2 is the vector of historical returns if we had portfolio w_2
      # x_2 = (df_sim_matrix * pracma::repmat(include_in_shortfall_optimization,N,1)) %*% w
      x_2 = df_sim_matrix %*% w_2
      
      # Market - based expected shortfall
      c_var_return = mean(sort(x)[1:floor(alpha_cvar_total*N_visual)]) 
      c_var_amount = - c_var_return * Omega
      
      # Accounting - based expected shortfall (when we do not take the contribution of gold into account)
      c_var_return_A = mean(sort(x_2)[1:floor(alpha_cvar_total*N_visual)]) 
      c_var_amount_A = - c_var_return_A * Omega
      
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
        # print(paste0(Sys.time(), ": OPTIMAL PORTFOLIO: w",k , " = ", round(w[k]*100,2), "%"))
        w[k]=round(w[k],8)
      }
      # print(paste0(Sys.time(), ": OPTIMAL PORTFOLIO: expected_return = ", expected_return))
      # print(paste0(Sys.time(), ": OPTIMAL PORTFOLIO: expected_return p.a.= ", expected_return_pa))
      # print(paste0(Sys.time(), ": OPTIMAL PORTFOLIO: variance = ", variance))
      # print(paste0(Sys.time(), ": OPTIMAL PORTFOLIO: cvar_return = ", c_var_return))
      # print(paste0(Sys.time(), ": OPTIMAL PORTFOLIO: cvar_vol = ", round(c_var_amount,2)))
      # print(paste0(Sys.time(), ": OPTIMAL PORTFOLIO: Omega = ", round(Omega)))
      # print(paste0(Sys.time(), ": OPTIMAL PORTFOLIO: P_neg_return = ", round(100 * P_neg_return,2), "%"))
      # print(paste0(Sys.time(), ": OPTIMAL PORTFOLIO: expected_profit = ", expected_return * Omega))
      
      results = list()
      results$density_x = density_x
      results$w = w
      results$Omega = Omega
      results$variance = variance
      results$expected_return = expected_return
      results$expected_profit = expected_return * Omega
      results$cvar_return = c_var_return
      results$cvar_vol = c_var_amount
      results$cvar_return_A = c_var_return_A
      results$cvar_vol_A = c_var_amount_A
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