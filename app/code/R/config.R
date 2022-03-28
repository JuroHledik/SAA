path_data = paste0(path_root, "data/")
path_model_output = paste0(path_root, "model_output/")
path_model_output_figures = paste0(path_root, "model_output/figures/")
path_model_output_historical_returns = paste0(path_root, "model_output/historical_returns/")
path_model_output_simulated_returns = paste0(path_root, "model_output/simulated_returns/")
path_model_output_copula_structure = paste0(path_root, "model_output/copula_structure/")

eps = 0.000000000001

# sample_sizes = c(100,500)
sample_sizes = c(100,500,1000,2000,5000,10000,50000,100000)

maturities = c(1,3,6,12,60)

maturities_daily = c(1,5,21,63,253,506,759,1012,1265)
maturities_weekly = c(1,4,13,52,104,156,208,260)
maturities_monthly = c(1,3,12,24,36,48,60)
maturities_quarterly = c(1,4,8,12,16,20)

maturities_choices_daily = c("1 day", "1 week", "1 month", "1 quarter", "1 year", "2 years", "3 years", "4 years", "5 years")
maturities_choices_weekly = c("1 week", "1 month", "1 quarter", "1 year", "2 years", "3 years", "4 years", "5 years")
maturities_choices_monthly = c("1 month", "1 quarter", "1 year", "2 years", "3 years", "4 years", "5 years")
maturities_choices_quarterly = c("1 quarter", "1 year", "2 years", "3 years", "4 years", "5 years")

frequencies = c("daily", "weekly", "monthly", "quarterly")

return_models = c("Gaussian", "Pearson")

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


