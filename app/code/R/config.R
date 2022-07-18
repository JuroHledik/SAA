path_data = paste0(path_root, "data/")
path_model_output = paste0(path_root, "model_output/")
path_model_output_figures = paste0(path_root, "model_output/figures/")
path_model_output_historical_returns = paste0(path_root, "model_output/historical_returns/")
path_model_output_simulated_returns = paste0(path_root, "model_output/simulated_returns/")
path_model_output_user_imported_returns = paste0(path_root, "model_output/user_imported_returns/")
path_model_output_copula_structure = paste0(path_root, "model_output/copula_structure/")

eps = 0.0000001

# sample_sizes = c(100,500)
sample_sizes = c(100,500,1000,2000,5000,10000,50000,100000)

maturities = c(1,3,6,12,60)

maturities_daily = c(1, 5, 21, 63, 253, 2*253, 3*253, 4*253, 5*253, 6*253, 7*253, 8*253, 9*253, 10*253)
maturities_weekly = c(1, 4, 13, 52, 2*52, 3*52, 4*52, 5*52, 6*52, 7*52, 8*52, 9*52, 10*52)
maturities_monthly = c(1, 3, 12, 2*12, 3*12, 4*12, 5*12, 6*12, 7*12, 8*12, 9*12, 10*12)
maturities_quarterly = c(1, 4, 2*4, 3*4, 4*4, 5*4, 6*4, 7*4, 8*4, 9*4, 10*4)

maturities_choices_daily = c("1 day", "1 week", "1 month", "1 quarter", "1 year", "2 years", "3 years", "4 years", "5 years", "6 years", "7 years", "8 years", "9 years", "10 years")
maturities_choices_weekly = c("1 week", "1 month", "1 quarter", "1 year", "2 years", "3 years", "4 years", "5 years", "6 years", "7 years", "8 years", "9 years", "10 years")
maturities_choices_monthly = c("1 month", "1 quarter", "1 year", "2 years", "3 years", "4 years", "5 years", "6 years", "7 years", "8 years", "9 years", "10 years")
maturities_choices_quarterly = c("1 quarter", "1 year", "2 years", "3 years", "4 years", "5 years", "6 years", "7 years", "8 years", "9 years", "10 years")

frequencies = c("daily", "weekly", "monthly", "quarterly")

#Used for user-imported returns:
frequencies_conversion_daily = c(253, 52, 12, 1, 1/2, 1/3, 1/4, 1/5, 1/6, 1/7, 1/8, 1/9, 1/10)

return_models = c("Gaussian", "Pearson", "Custom")

allowed_copula_types = c("Independence",
                         "Gaussian",
                         "Student",
                         "Clayton",
                         "Gumbel",
                         "Frank",
                         "Joe",
                         "BB1",
                         "BB6",
                         "BB7",
                         "BB8",
                         "rot. Clayton (180 deg)",
                         "rot. Gumbel (180 deg)",
                         "rot. Joe (180 deg)",
                         "rot. BB1 (180 deg)",
                         "rot. BB6 (180 deg)",
                         "rot. BB7 (180 deg)",
                         "rot. BB8 (180 deg)",
                         "rot. Clayton (90 deg)",
                         "rot. Gumbel (90 deg)",
                         "rot. Joe (90 deg)",
                         "rot. BB1 (90 deg)",
                         "rot. BB6 (90 deg)",
                         "rot. BB7 (90 deg)",
                         "rot. BB8 (90 deg)",
                         "rot. Clayton (270 deg)",
                         "rot. Gumbel (270 deg)",
                         "rot. Joe (270 deg)",
                         "rot. BB1 (270 deg)",
                         "rot. BB6 (270 deg)",
                         "rot. BB7 (270 deg)",
                         "rot. BB8 (270 deg)",
                         "Tawn type I",
                         "Tawn type I (180 deg)",
                         "Tawn type I (90 deg)",
                         "Tawn type I (270 deg)",
                         "Tawn type II",
                         "Tawn type II (180 deg)",
                         "Tawn type II (90 deg)",
                         "Tawn type II (270 deg)")

allowed_copula_codes = c(0,
                         1,
                         2,
                         3,
                         4,
                         5,
                         6,
                         7,
                         8,
                         9,
                         10,
                         13,
                         14,
                         16,
                         17,
                         18,
                         19,
                         20,
                         23,
                         24,
                         26,
                         27,
                         28,
                         29,
                         30,
                         33,
                         34,
                         36,
                         37,
                         38,
                         39,
                         40,
                         104,
                         114,
                         124,
                         134,
                         204,
                         214,
                         224,
                         234)


