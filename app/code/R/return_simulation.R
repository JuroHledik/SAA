################################################################################
# INSTALL DEPENDENCIES
# ################################################################################
# install.packages("cli")
# install.packages("cramer")
# install.packages("data.table")
# install.packages("devtools")
# install.packages("gclus")
# install.packages("MASS")
# install.packages("moments")
# install.packages("PearsonDS")
# install.packages("readxl")
# install.packages("Rcpp")
# install.packages("Rfast")
# install.packages("sqldf")
# install.packages('stringr')
# install.packages('TSP')
# install.packages("VineCopula")
# install.packages("lubridate")

################################################################################
# DEPENDENCIES
################################################################################
library(cli)
library(cramer)
library(data.table)
library(gclus)
library(lubridate)
library(MASS)
library(moments)
library(PearsonDS)
library(readxl)
library(Rfast)
library(sqldf)
library(stringr)
library(TSP)
library(VineCopula)


source(paste0(path_root, "code/R/config.R"))
source(paste0(path_root, "code/R/functions.R"))


return_simulation <- function(return_simulation_inputs){
  
  
  # N_simulation = 1000000
  # frequency = "quarterly"
  # selected_copula_types = allowed_copula_types
  # selected_copula_codes = allowed_copula_codes

  N_simulation = 10 * max(sample_sizes)
  frequency = return_simulation_inputs$frequency
  selected_copula_types = return_simulation_inputs$selected_copula_types
  subtract_EURIBOR = return_simulation_inputs$subtract_EURIBOR
  
  selected_copula_codes = allowed_copula_codes[allowed_copula_types %in% selected_copula_types]


  print_message_with_time("IMPORTING DATA...")

  ################################################################################
  # DATA IMPORT
  ################################################################################
  # df <- read_excel(paste0(path_data,"indexy1.xls"))
  df = read.csv(paste0(path_data,"historical_daily_returns.csv"), header=TRUE, sep=";", na.strings=c("","NA"))
  df = df[complete.cases(df),]
  
  df_euribor = read.csv(paste0(path_data,"euribor.csv"), header=TRUE, sep=";", na.strings=c("","NA"))
  df_euribor = df_euribor[complete.cases(df_euribor),]

  for (k in 2:ncol(df)) {
    df[,k] <- gsub(',', '.', df[,k])
  }
  
  for (k in 2:ncol(df_euribor)) {
    df_euribor[,k] <- gsub(',', '.', df_euribor[,k])
  }
  
  df[,2:ncol(df)] <- sapply(df[,2:ncol(df)], as.character)
  df[,2:ncol(df)] <- sapply(df[,2:ncol(df)], as.numeric)
  df_euribor[,2] <- sapply(df_euribor[,2], as.character)
  df_euribor[,2] <- sapply(df_euribor[,2], as.numeric)
  
  #Get daily euribor rate:
  df_euribor$Rate = df_euribor$Rate/100/365

  #Create a dataset with the relevant frequency:  
  df$day =  sub("^0+", "", substr(df$Date,1,2))
  df$month =  sub("^0+", "", substr(df$Date,4,5))
  df$year = substr(df$Date,7,10)
  df$Date = format(as.Date(ymd(paste0(df$year,"-",df$month, "-", df$day))))
  df$week = lubridate::week(ymd(df$Date))
  df$quarter = lubridate::quarter(ymd(df$Date))  
  
  df_euribor$day =  sub("^0+", "", substr(df_euribor$Date,1,2))
  df_euribor$month =  sub("^0+", "", substr(df_euribor$Date,4,5))
  df_euribor$year = substr(df_euribor$Date,7,10)
  df_euribor$Date = format(as.Date(ymd(paste0(df_euribor$year,"-",df_euribor$month, "-", df_euribor$day))))
  df_euribor$week = lubridate::week(ymd(df_euribor$Date))
  df_euribor$quarter = lubridate::quarter(ymd(df_euribor$Date))

  #Subtract the euribor from all asset classes
  print_message_with_time(paste0("Subtracting EURIBOR from all asset classes..."))
  df = sqldf("select a.*, b.Rate from df a left join df_euribor b on a.Date=b.Date")
  df[,2:(ncol(df)-6)] = df[,2:(ncol(df)-6)] - df[,"Rate"]
  
  #ASW portfolio is already currency hedged, no need to subtract the euribor, add it back:
  for (i in 1:length(subtract_EURIBOR)) {
    if (subtract_EURIBOR[i]==0) {
      print_message_with_time(paste0("Adding back the EURIBOR rate to asset number ", as.character(i), "..."))
      df[,i] = df[,i] + df[,"Rate"]    
    }
  }
  
  df = df[complete.cases(df),]
  
  df_hist = df[,1:(ncol(df)-6)]
  
  temp = df
  temp[,2:(ncol(temp)-6)] = temp[,2:(ncol(temp)-6)] + 1
  if (frequency=="daily") {
    temp = temp[,2:(ncol(temp)-6)]
    maturities = maturities_daily
  } else {
    if (frequency=="weekly") {
      temp = aggregate(temp[,2:(ncol(df)-6)],list(temp$year, temp$week), prod)
      temp = temp[,3:ncol(temp)]
      maturities = maturities_weekly
    } else {
      if (frequency=="monthly") {
        temp = aggregate(temp[,2:(ncol(df)-6)],list(temp$year, temp$month), prod)
        temp = temp[,3:ncol(temp)]
        maturities = maturities_monthly
      } else {
        if (frequency=="quarterly") {
          temp = aggregate(temp[,2:(ncol(df)-6)],list(temp$year, temp$quarter), prod)
          temp = temp[,3:ncol(temp)]
          maturities = maturities_quarterly
        }
      }
    }
  }
  df = temp - 1
  
  N = nrow(df)
  K = ncol(df)
  
  # Custom asset names:
  constraints_df = read.csv(paste0(path_data,"portfolio_constraints.csv"), header=TRUE, sep=";", colClasses = c(vol_max = "character") )
  names(df) = constraints_df$asset
  
  asset_names = names(df)
  asset_names_no_escape = str_replace(asset_names,"/", " ")
  
  ################################################################################
  # DIRECTORY CREATION
  ################################################################################
  dir.create(file.path(path_model_output), showWarnings = FALSE)
  dir.create(file.path(path_model_output_figures), showWarnings = FALSE)
  dir.create(file.path(path_model_output_historical_returns), showWarnings = FALSE)
  dir.create(file.path(paste0(path_model_output,"KS_test/")), showWarnings = FALSE)  
  dir.create(file.path(paste0(path_model_output,"simulated_returns/")), showWarnings = FALSE)
  dir.create(file.path(paste0(path_model_output,"copula_structure/")), showWarnings = FALSE)
  
  
  dir.create(file.path(paste0(path_model_output,"historical_returns/",frequency)), showWarnings = FALSE)
  
  dir.create(file.path(paste0(path_model_output_figures,"/correlations/")), showWarnings = FALSE)
  dir.create(file.path(paste0(path_model_output_figures,"/correlations/",frequency)), showWarnings = FALSE)
  
  dir.create(file.path(paste0(path_model_output_figures,"marginal_distributions/")), showWarnings = FALSE)
  dir.create(file.path(paste0(path_model_output_figures,"marginal_distributions/",frequency)), showWarnings = FALSE)
  
  dir.create(file.path(paste0(path_model_output_figures,"marginal_distributions_uniform/")), showWarnings = FALSE)
  dir.create(file.path(paste0(path_model_output_figures,"marginal_distributions_uniform/",frequency)), showWarnings = FALSE)
  
  dir.create(file.path(paste0(path_model_output_simulated_returns,frequency, "/")), showWarnings = FALSE)
  dir.create(file.path(paste0(path_model_output_simulated_returns,frequency, "/", "gaussian/")), showWarnings = FALSE)
  dir.create(file.path(paste0(path_model_output_simulated_returns,frequency, "/", "vine_copula/")), showWarnings = FALSE)
  
  dir.create(file.path(paste0(path_model_output_copula_structure,frequency, "/")), showWarnings = FALSE)
  

  ################################################################################
  # SAVING HISTORICAL RETURNS
  ################################################################################

  save(df, file = paste0(path_model_output_historical_returns, frequency, "/", "df.Rdata"))
  save(df_hist, file = paste0(path_model_output_historical_returns, "/", "df_hist.Rdata"))
  
  ################################################################################
  # PLOT CORRELATIONS
  ################################################################################
  
  print_message_with_time("PLOTTING PAIRWISE CORRELATIONS OF HISTORICAL DATA...")
  #STANDARD LINEAR:
  
  df.r <- abs(cor(df)) # get correlations
  df.col <- dmat.color(df.r) # get colors
  # reorder variables so those with highest correlation
  # are closest to the diagonal
  df.o <- order.single(df.r)
  
  #Plot
  png(paste0(path_model_output_figures,"/","correlations/",frequency,"/","Historical_Returns",".png"), width = 3000, height = 3000)
  cpairs(df, gap=.5, main="Historical returns" ) 
  dev.off()
  
  ################################################################################
  # EXPORT MARGINAL DISTRIBUTIONS' FIGURES AND CONVERT THE DATA TO [0,1]^N
  ################################################################################
  df_U = df
  param_marginals = data.frame(index = numeric(),
                               nu = numeric(),
                               mu = numeric(),
                               sigma = numeric(),
                               stringsAsFactors = FALSE) 
  
  #Go through the individual variables:
  k=8
  #For each asset class, fit the pearson VII distribution:
  for (k in 1:K) {
    print_message_with_time(paste0("UNIFORM TRANSFORMATION: Processing asset ", k, " of ", K, "..."))
    #Load the asset class observations:
    x =  as.vector(df[[asset_names[k]]])
    
    #Find its mean, variance, rate, shape, skewness and kurtosis:
    x_mean <- mean(x)
    x_var <- var(x)
    x_rate <- x_mean / x_var
    x_shape <- ( (x_mean)^2 ) / x_var
    x_skew = skewness(x)
    x_kurt = kurtosis(x)
    
    print(paste0("Historical data: mean = ", x_mean, ", var =", x_var, ", skew =", x_skew, ", kurt =", x_kurt))
    
    # Compute the Pearson type VII parameters:
    mu = x_mean
    nu = (6 - 4*x_kurt) / (3 - x_kurt)
    if (nu<0) { #If nu is negative, the fourth moment does not exist. The closest thing is nu=Inf, but that wouldn't work so we settle with nu=1/eps
      nu = 1/eps
    }
    sigma = sqrt(x_var) * sqrt((nu-2)/nu)
    
    #Generate simulated observations from the estimated distribution and also from
    #the normal distribution.
    temp1 = rpearsonVII(N_simulation, nu, mu, sigma)
    temp2 = rnorm(N_simulation, mu, sqrt(x_var))

    print(paste0("Simulated data: mean = ", mean(temp1), ", var =", var(temp1), ", skew =", skewness(temp1), ", kurt =", kurtosis(temp1)))    
    
    #Set x axis plot limits:
    xrange = range(c(-3*sqrt(x_var), 3*sqrt(x_var)))
    
    #Get the empirical densities:
    hg1 = density(temp1, from=min(xrange), to=max(xrange))
    hg2 = density(temp2, from=min(xrange), to=max(xrange))
    
    #Save the resulting figure within the specified folder
    png(paste0(path_model_output_figures,"marginal_distributions/",frequency,"/",asset_names_no_escape[k],".png"), width = 800, height = 800)
    hist(x, # histogram
         col = "peachpuff", # column color
         border = "black",
         prob = TRUE, # show densities instead of frequencies
         xlim = xrange,
         breaks=ceiling(N/4),
         main = asset_names[k])
    lines(hg1, xlim=xrange, col = "red", lwd=2)
    lines(hg2, xlim=xrange, col = "blue", lwd=2, lty="dashed")
    legend("topright",
           legend=c(paste0("Pearson type VII fit \n","mu=",round(mean(temp1),2),", sigma=",round(sqrt(var(temp1)),2),", kurt=",round(kurtosis(temp1),2)),
                    paste0("Gaussian fit \n","mu=",round(mean(temp2),2),", sigma=",round(sqrt(var(temp2)),2),", kurt=",round(kurtosis(temp2),2))),
           col=c("red", "blue"), lty=1:2, cex=1, lwd = 2, y.intersp=2)
    dev.off()
    
    # Transform the returns into a [0,1] uniformly distributed variable and update
    # the relevant dataframe.
    U = ppearsonVII(x, nu, mu, sigma)
    df_U[[asset_names[k]]] = U
    
    #Save the histogram to see how uniform the data is:
    
    png(paste0(path_model_output_figures,"marginal_distributions_uniform/",frequency,"/", asset_names_no_escape[k],".png"), width = 800, height = 800)
    hist(U, # histogram
         col = "peachpuff", # column color
         border = "black",
         prob = TRUE, # show densities instead of frequencies
         xlim = range(0,1),
         breaks=10,
         main = paste0("Uniform transformation: \n", asset_names[k]))
    dev.off()
    
    #Append the marginal distribution parameter values:
    df_temp<-data.frame(k,nu,mu,sigma)
    names(df_temp)<-c("index","nu","mu","sigma")
    param_marginals <- rbind(param_marginals, df_temp)
  }
  #Append the asset classes' names to the parameter dataframe:
  param_marginals$asset_names = asset_names
  param_marginals <- param_marginals[, c(1, 5, 2, 3, 4)]
  
  ################################################################################
  # FIT THE MODEL
  ################################################################################
  print_message_with_time(paste0("COPULA: Fitting the model..."))

  # RVM <- RVineStructureSelect(df_U, familyset = c(1:8), progress = FALSE)
  RVM <- RVineStructureSelect(df_U, familyset = selected_copula_codes, progress = FALSE)
  
  ## see the object's content or a summary
  str(RVM)
  summary(RVM)
  contour(RVM)
  RVM$Matrix
  
  df_sim_U = as.data.frame(RVineSim(N_simulation,RVM))
  df_sim_1M = df_sim_U
  for (k in 1:K) {
    #Import the relevant simulated uniform asset:
    u =  as.vector(df_sim_U[[asset_names[k]]])
    
    #Import nu,mu,sigma:
    nu = as.vector(param_marginals[["nu"]])[k]
    mu = as.vector(param_marginals[["mu"]])[k]
    sigma = as.vector(param_marginals[["sigma"]])[k]
    
    #Transform the asset:
    df_sim_1M[[asset_names[k]]] = qpearsonVII(u, nu, mu, sigma)
  }
  df_sim = df_sim_1M[1:N,]
  
  #Plot
 
  png(paste0(path_model_output_figures,"correlations/",frequency,"/","Simulated_returns",".png"), width = 3000, height = 3000)
  cpairs(df_sim, gap=.5, main="Simulated returns" ) 
  dev.off()
  
  #Plot simulated and historical data together:
  group = NA
  group[df[[asset_names[1]]]<Inf] <- 1
  group2 = append(group, group+1)
  group3 = append(group2, group+2)
  

  png(paste0(path_model_output_figures,"correlations/",frequency,"/","Historical_vs_Pearson_simulated_returns",".png"), width = 6000, height = 6000)
  pairs(rbind(df,df_sim),
        col = c("black", "red")[group2],   # Change color by group
        pch = c(18, 1)[group2],                            # Change points by group
        main = "Simulated (red) vs historical (black) returns")
  dev.off()
  
  ################################################################################
  # VISUALLY Check the fit against multivariate normal distribution
  ################################################################################
  print_message_with_time(paste0("PLOT: Plotting the scatter plot..."))
  # Estimate the parameters:
  temp = mvnorm.mle(data.matrix(df))
  MG_mu = temp$mu
  MG_sigma = temp$sigma
  
  # Simulate multivariate Gaussian observations:
  df_sim_MG_1M = mvrnorm(n = N_simulation, MG_mu, MG_sigma, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
  df_sim_MG = mvrnorm(n = N,MG_mu, MG_sigma, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
  colnames(df_sim_MG) = asset_names
  df_sim_MG = as.data.frame(df_sim_MG)
  df_sim_MG_1M = as.data.frame(df_sim_MG_1M)
  #Plot the figure:

  png(paste0(path_model_output_figures,"correlations/",frequency,"/","Historical_vs_Gaussian_and_Pearson_returns",".png"), width = 6000, height = 6000)
  pairs(rbind(df, df_sim_MG, df_sim),
        col = c("black", "blue", "red")[group3],   # Change color by group
        pch = c(18, 1,6)[group3],            # Change points by group
        main = "Copula-simulated (red) vs Gaussian-simulated (blue) vs historical (black) returns")
  dev.off()
  
  ################################################################################
  # NUMERICALLY Check the fit against multivariate normal distribution
  ################################################################################
  print_message_with_time(paste0("TESTS: Running the distributional equality testing..."))
  df_equality_test <- data.frame(copula_p_value = numeric(),
                                 gaussian_p_value = numeric(),
                                 stringsAsFactors = FALSE)
  
  # Do a Cramer test (K dimensional nonparametric on equality of distributions)
  temp1 = cramer.test(data.matrix(df), data.matrix(df_sim))$p.value
  temp2 = cramer.test(data.matrix(df), data.matrix(df_sim_MG))$p.value
  
  df_equality_test = rbind(df_equality_test,c(temp1,temp2))
  
  # Do K iterations of the Kolmogorov-Smirnov test:
  
  for (k in 1:K) {
    print(asset_names[k])
    df_equality_test = rbind(df_equality_test,c(0,0))
    print_message_with_time(paste0("COPULA: Kolmogorov - Smirnov test: Processing asset ", k, " of ", K, ": ", asset_names[k], "."))
    #Import the relevant simulated uniform asset:
    dist1 =  as.vector(df_sim_1M[[asset_names[k]]])
    dist2 =  as.vector(df[[asset_names[k]]])
    df_equality_test[k,1] = ks.test(dist1, dist2)$p.value
  
    print_message_with_time(paste0("GAUSSIAN: Kolmogorov - Smirnov test: Processing asset ", k, " of ", K, ": ", asset_names[k], "."))
    #Import the relevant simulated uniform asset:
    dist1 =  as.vector(df_sim_MG_1M[[asset_names[k]]])
    dist2 =  as.vector(df[[asset_names[k]]])
    df_equality_test[k,2] = ks.test(dist1, dist2)$p.value
  }
  
  for (k in 1:K) {
    print(asset_names[k])
    df_equality_test = rbind(df_equality_test,c(0,0))
    print_message_with_time(paste0("COPULA: Kolmogorov - Smirnov test: Processing asset ", k, " of ", K, ": ", asset_names[k], "."))
    #Import the relevant simulated uniform asset:
    dist1 =  as.vector(df_sim[[asset_names[k]]])
    dist2 =  as.vector(df[[asset_names[k]]])
    df_equality_test[K+k,1] = ks.test(dist1, dist2)$p.value
    
    print_message_with_time(paste0("GAUSSIAN: Kolmogorov - Smirnov test: Processing asset ", k, " of ", K, ": ", asset_names[k], "."))
    #Import the relevant simulated uniform asset:
    dist1 =  as.vector(df_sim_MG[[asset_names[k]]])
    dist2 =  as.vector(df[[asset_names[k]]])
    df_equality_test[K+k,2] = ks.test(dist1, dist2)$p.value
  }
  names(df_equality_test)<-c("copula","Gaussian")
  
  write.csv(df_equality_test,paste0(path_model_output,"KS_test/KS_", frequency,".csv"), row.names = FALSE)
  
  ################################################################################
  # Create directories
  ################################################################################
  save(df_sim_MG, file = paste0(path_model_output_simulated_returns,frequency, "/", "gaussian/df_sim_MG.Rdata"))
  save(df_sim, file = paste0(path_model_output_simulated_returns,frequency, "/", "vine_copula/df_sim.Rdata"))
  

  ################################################################################
  # Create different maturities
  ################################################################################
  max_sample_size = max(sample_sizes)
  for (maturity in maturities) {
    print_message_with_time(paste0("CREATING DATASET: Processing maturity ", maturity, "."))
    df_temp = df_sim_1M[sample(nrow(df_sim_1M), max_sample_size), ]*0 + 1
    df_temp_MG = df_sim_MG_1M[sample(nrow(df_sim_1M), max_sample_size), ]*0 + 1
    for (period in 1:maturity) {
      df_temp_temp = df_sim_1M[sample(nrow(df_sim_1M), max_sample_size), ] + 1
      df_temp_temp_MG = df_sim_MG_1M[sample(nrow(df_sim_MG_1M), max_sample_size), ] + 1
      df_temp = as.matrix(df_temp) * as.matrix(df_temp_temp)
      df_temp_MG = as.matrix(df_temp_MG) * as.matrix(df_temp_temp_MG)
    }
    
    if (frequency=="daily") {
      df_sim_pa = as.data.frame(df_temp ^ (253/maturity)) - 1
      df_sim_MG_pa = as.data.frame(df_temp_MG ^ (253/maturity)) - 1  
    } else {
      if (frequency=="weekly") {
        df_sim_pa = as.data.frame(df_temp ^ (52/maturity)) - 1
        df_sim_MG_pa = as.data.frame(df_temp_MG ^ (52/maturity)) - 1  
      } else {
        if (frequency=="monthly") {
          df_sim_pa = as.data.frame(df_temp ^ (12/maturity)) - 1
          df_sim_MG_pa = as.data.frame(df_temp_MG ^ (12/maturity)) - 1  
        } else {
          if (frequency=="quarterly") {
            df_sim_pa = as.data.frame(df_temp ^ (4/maturity)) - 1
            df_sim_MG_pa = as.data.frame(df_temp_MG ^ (4/maturity)) - 1  
          }
        }
      }
    }
    
    df_temp = as.data.frame(df_temp) - 1
    df_temp_MG = as.data.frame(df_temp_MG) - 1
    row.names(df_temp) <- NULL
    row.names(df_temp_MG) <- NULL
    row.names(df_sim_pa) <- NULL
    row.names(df_sim_MG_pa) <- NULL
    df_sim = df_temp
    df_sim_MG = df_temp_MG
    save(df_sim, df_sim_pa, file = paste0(path_model_output_simulated_returns,frequency,"/vine_copula/df_maturity", maturity, ".Rdata"))
    save(df_sim_MG, df_sim_MG_pa, file = paste0(path_model_output_simulated_returns,frequency,"/gaussian/df_maturity", maturity, ".Rdata"))
  }

  ################################################################################
  # Export asset names
  ################################################################################
  save(asset_names_no_escape, file = paste0(path_model_output,"asset_names_no_escape.Rdata"))
  print_message_with_time("DONE.")
  
  ################################################################################
  # Export copula structure
  ################################################################################
  save(RVM, file = paste0(path_model_output_copula_structure,frequency, "/", "RVM.RData"))
  sink(paste0(path_model_output_copula_structure,frequency, "/", "summary.txt"))
  summary(RVM)
  sink()
}