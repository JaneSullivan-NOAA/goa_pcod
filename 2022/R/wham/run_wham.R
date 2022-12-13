rm(list = ls())
require(dplyr)
require(ggplot2)
require(r4ss)
library(wham)
source('2022/R/wham/aux_fun.R')
source('2022/R/wham/get_aging_error.R')
# Base model (base WHAM) -------------------------------------------------

# If needed:
# r4ss::run_SS_models(dirvec = '2022/Stock_Synthesis_files/Model19.1a (22) - wADFG')

# Read SS model:

data_file = r4ss::SS_readdat_3.30(file = '2022/Stock_Synthesis_files/Model19.1a (22) - wADFG/GOAPcod2022Oct25_wADFG.dat')
SS_report = r4ss::SS_output(dir = '2022/Stock_Synthesis_files/Model19.1a (22) - wADFG', covar = FALSE) # from OM

# Some colors to plot
mycols = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# Some basic information for WHAM model:
n_ages = length(unique(SS_report$agebins))
# min(SS_report$lendbase$Bin); max(SS_report$lendbase$Bin)
length_vector = seq(from = 1, to = 117, by = 2)
min_year = SS_report$startyr
max_year = SS_report$endyr
n_years = length(min_year:max_year)
# NAA info from SS:
NAA_SS = SS_report$natage[SS_report$natage$`Beg/Mid` == 'B' & SS_report$natage$Yr >= min_year & SS_report$natage$Yr <= max_year, 14:(14+n_ages-1)]
# Biological parameters (taken from SS model): 
LWpars = c(SS_report$Growth_Parameters$WtLen1, SS_report$Growth_Parameters$WtLen2)
# LVB:  growth rate (K), asymptotic length (Linf), length-at-age 1 (L1), coefficient
# of variation of length-at-age 1 (CV1), and coefficient of variation of
# length-at- the maximum age (CVA).}
GWpars = c(SS_report$Growth_Parameters$K, SS_report$Growth_Parameters$Linf, 
           SS_report$Growth_Parameters$L_a_A1,
           SS_report$Growth_Parameters$CVmin, SS_report$Growth_Parameters$CVmax)
selpars1 = SS_report$parameters$Value[grepl('FshTrawl', SS_report$parameters$Label) & !grepl('BLK|DEV|dev', SS_report$parameters$Label)] #c(57.4654 ,-4.10776,5.08886,-0.164564,-999,10)
selppars2 = SS_report$parameters$Value[grepl('FshLL', SS_report$parameters$Label) & !grepl('BLK|DEV|dev', SS_report$parameters$Label)] #c(65.9927,-5.062,5.11804,10,-999,10)
selppars3 = SS_report$parameters$Value[grepl('FshPot', SS_report$parameters$Label) & !grepl('BLK|DEV|dev', SS_report$parameters$Label)]  #c(70.5935,-12.1134,5.01767,4.07254,-999,0.203773)
selppars4 = SS_report$parameters$Value[grepl('Srv', SS_report$parameters$Label) & !grepl('Q|LL|BLK|DEV|dev', SS_report$parameters$Label)]  #c(60.2422,-11.1713,5.35012,4.64514,-2.84308,-1.01928)
selppars5 = SS_report$parameters$Value[grepl('LLSrv', SS_report$parameters$Label) & !grepl('Q|BLK|DEV|dev', SS_report$parameters$Label)]  #c(65.5972,-12.5095,4.69815,4.57853,-999,-0.481806)

# Prepare input data: ------------------------------------------------
wham_data = list()
wham_data$ages = 1:n_ages 
wham_data$lengths = length_vector
wham_data$years = as.integer(min_year:max_year)
#Catch information:
wham_data$n_fleets = data_file$Nfleet
wham_data$agg_catch = matrix(data_file$catch$catch[data_file$catch$year > 0], nrow = n_years, ncol = data_file$Nfleet)
wham_data$use_agg_catch = matrix(1L, nrow = n_years, ncol = data_file$Nfleet)
wham_data$catch_cv = matrix(data_file$catch$catch_se[data_file$catch$year > 0], nrow = n_years, ncol = data_file$Nfleet)
# Survey information:
tmp_data <- data_file$CPUE[data_file$CPUE$index > 0,]
tmp_data2 <- full_join(expand.grid(year = wham_data$years, index = unique(tmp_data$index)),
                       tmp_data, by = c('year', 'index'))
wham_data$n_indices = length(unique(tmp_data2$index))
wham_data$agg_indices = matrix(tmp_data2$obs, nrow = n_years, ncol = wham_data$n_indices)
wham_data$agg_indices[is.na(wham_data$agg_indices)] = 0
wham_data$index_cv = matrix(tmp_data2$se_log, nrow = n_years, ncol = wham_data$n_indices)
wham_data$index_cv[is.na(wham_data$index_cv)] = 0
wham_data$units_indices = matrix(0L, nrow = n_years, ncol = wham_data$n_indices)
wham_data$use_indices = matrix(1L, nrow = n_years, ncol = wham_data$n_indices)
wham_data$use_indices[is.na(tmp_data2$obs)] = -1
# Len comps catch:
wham_lencomps = array(NA, dim = c(wham_data$n_fleets, n_years, length(length_vector)))
wham_lenNeff = matrix(0, ncol = wham_data$n_fleets, nrow = n_years)
wham_lenuse = matrix(-1, ncol = wham_data$n_fleets, nrow = n_years)
for(j in 1:wham_data$n_fleets) {
  
  lencomp_fleet = data_file$lencomp[data_file$lencomp$FltSvy == j, ]
  lencomp_fleet2 = as.matrix(lencomp_fleet[,7:ncol(lencomp_fleet)])
  lencomp_temp = matrix(NA, ncol = length(length_vector), nrow = n_years)
  for(i in 1:length(length_vector)) {
    if(i == length(length_vector)) lencomp_temp[match(lencomp_fleet$Yr, wham_data$years),i] = lencomp_fleet2[,ncol(lencomp_fleet2)]
    else lencomp_temp[match(lencomp_fleet$Yr, wham_data$years),i] = rowSums(lencomp_fleet2[,(2*i-1):(2*i)])
  }
  wham_lencomps[j,,] = lencomp_temp/rowSums(lencomp_temp)
  wham_lenNeff[match(lencomp_fleet$Yr, wham_data$years),j] = lencomp_fleet$Nsamp
  wham_lenuse[match(lencomp_fleet$Yr, wham_data$years),j] = 1
  
}
wham_data$catch_pal = wham_lencomps
wham_data$catch_NeffL = wham_lenNeff
wham_data$use_catch_pal = wham_lenuse
# Len comps index:
wham_lencomps = array(NA, dim = c(wham_data$n_indices, n_years, length(length_vector)))
wham_lenNeff = matrix(0, ncol = wham_data$n_indices, nrow = n_years)
wham_lenuse = matrix(-1, ncol = wham_data$n_indices, nrow = n_years)
for(j in 1:wham_data$n_indices) {
  
  lencomp_fleet = data_file$lencomp[data_file$lencomp$FltSvy == j + wham_data$n_fleets, ]
  lencomp_fleet2 = as.matrix(lencomp_fleet[,7:ncol(lencomp_fleet)])
  lencomp_temp = matrix(NA, ncol = length(length_vector), nrow = n_years)
  for(i in 1:length(length_vector)) {
    if(i == length(length_vector)) lencomp_temp[match(lencomp_fleet$Yr, wham_data$years),i] = lencomp_fleet2[,ncol(lencomp_fleet2)]
    else lencomp_temp[match(lencomp_fleet$Yr, wham_data$years),i] = rowSums(lencomp_fleet2[,(2*i-1):(2*i)])
  }
  wham_lencomps[j,,] = lencomp_temp/rowSums(lencomp_temp)
  wham_lenNeff[match(lencomp_fleet$Yr, wham_data$years),j] = lencomp_fleet$Nsamp
  wham_lenuse[match(lencomp_fleet$Yr, wham_data$years),j] = 1
  
}
wham_data$index_pal = wham_lencomps
wham_data$index_NeffL = wham_lenNeff
wham_data$use_index_pal = wham_lenuse
# CAAL fishery and index TODO:
# agecomp_index = data_file$agecomp
# tmp_data = data.frame(Yr = wham_data$years)
# tmp_data2 = merge(tmp_data, agecomp_index, by = 'Yr', all.x = TRUE)
# tmp_data3 = t(apply(tmp_data2[,11:ncol(tmp_data2)], 1, function(x) { x/sum(x) }))
# tmp_data3[is.na(tmp_data3)] = 0
# tmp_data4 = cbind(tmp_data3, matrix(0, ncol = 8, nrow = n_years))
# wham_data$index_paa = as.matrix(tmp_data4)
# wham_data$index_Neff = matrix(ifelse(test = is.na(tmp_data2$Nsamp), yes = 0, no = tmp_data2$Nsamp),
#                               nrow = n_years, ncol = 1)
# wham_data$use_index_paa = matrix(ifelse(test = is.na(tmp_data2$Nsamp), yes = -1, no = 1),
#                                  nrow = n_years, ncol = 1)
# Add aging error (ADD YEAR DIMENSION):
base_aging_error = get_aging_error_matrix(obs_age = SS_report$age_error_mean$type1[2:(n_ages+1)],
                                          sd = SS_report$age_error_sd$type1[2:(n_ages+1)])
wham_data$index_aging_error = array(NA, dim = c(wham_data$n_indices,n_ages, n_ages))
wham_data$index_aging_error[1,,] = base_aging_error
wham_data$index_aging_error[2,,] = base_aging_error
wham_data$use_index_aging_error = c(1,1)
wham_data$catch_aging_error = array(NA, dim = c(wham_data$n_fleets,n_ages, n_ages))
wham_data$catch_aging_error[1,,] = base_aging_error
wham_data$catch_aging_error[2,,] = base_aging_error
wham_data$catch_aging_error[3,,] = base_aging_error
wham_data$use_catch_aging_error = c(1,1,1)
# selectivity and F options
wham_data$selblock_pointer_fleets = matrix(rep(c(1,2,3), each = n_years), ncol = wham_data$n_fleets, nrow = n_years)
wham_data$F = matrix(0.2, ncol = wham_data$n_fleets, nrow = n_years)
wham_data$selblock_pointer_indices = matrix(rep(c(4,5), each = n_years), ncol = wham_data$n_indices, nrow = n_years)
wham_data$fracyr_indices = matrix(0.5, ncol = wham_data$n_indices, nrow = n_years)
wham_data$fracyr_SSB = matrix(0, ncol = 1, nrow = n_years)
# WAA information 
wham_data$waa_pointer_indices = rep(1, times = wham_data$n_indices)
wham_data$waa_pointer_fleets = rep(2, times = wham_data$n_fleets)
wham_data$waa_pointer_totcatch = 2
wham_data$waa_pointer_ssb = 3
wham_data$waa_pointer_jan1 = 3
wham_data$maturity = matrix(rep(SS_report$endgrowth[2:(n_ages+1),18], times = max_year - min_year + 1),
                            ncol = n_ages, nrow = max_year - min_year + 1, byrow = TRUE) 

wham_data$Fbar_ages = 3L:10L
wham_data$percentSPR = 40
wham_data$percentFXSPR = 100
wham_data$percentFMSY = 100
wham_data$XSPR_R_avg_yrs = 1:n_years
wham_data$XSPR_R_opt = 2
wham_data$simulate_period = c(1,0)
wham_data$bias_correct_process = 1
wham_data$bias_correct_observation = 1

# Ecov information - temperature on LLSrv (FltSrv 5) catchability:
# env1 = data_file$envdat$Value[data_file$envdat$Variable == 1]
env1 = merge(data.frame(Yr = wham_data$years, Variable = 1),
             data_file$envdat[data_file$envdat$Variable == 1,],
             by = c('Yr', 'Variable'), all.x = TRUE) %>% 
  mutate(logsigma = log(0.0001),
         use = ifelse(is.na(Value), -1, 1))

# ecov <- list(
#   label = c("env1"),
#   mean = as.matrix(env1$Value),
#   logsigma = as.matrix(env1$logsigma), # no obs error
#   year = wham_data$years,
#   use_obs = as.matrix(env1$use),
#   lag = list(rep(0, 1)), # ? no lag
#   ages = list(1:n_ages,1:n_ages), # ? not age-specific
#   process_model = c('ar1'), # ? 
#   where = list('q'), # ?
#   where_subindex = c(0, 1), # ?
#   how = c(1)) # ?

# Prepare input object:
input = prepare_wham_input(model_name="cod_1",
                               selectivity=list(model = c('len-double-normal', 'len-double-normal'),
                                                re = c('iid', 'iid'),
                                                initial_pars=list(selpars1,
                                                                  selpars2),
                                                fix_pars = list(c(1,2,4,5), c(2,4:6)),
                                                n_selblocks = 2),
                               M = list(model = 'constant', re = 'none',
                                        initial_means = 0.325,
                                        est_ages = 1),
                               NAA_re = list(sigma="rec", cor = 'iid', N1_model = 0,
                                             recruit_model = 2,
                                             N1_pars = as.vector(as.matrix(NAA_SS[1,])),
                                             recruit_pars = 344300),
                               growth = list(model = 'Richards',
                                             re = c('none', 'none', 'iid_y', 'none', 'none', 'none'),
                                             init_vals = GWpars,
                                             est_pars = c(1:4)),
                               LW = list(re = c('none', 'none'),
                                     init_vals = LWpars),
                               catchability = list(re = c('none'), 
                                                   initial_q = 0.97, q_lower = 0,
                                                   q_upper = 1000, prior_sd = NA),
                               age_comp = 'dirichlet-pool0',
                               len_comp = 'dir-mult',
                               basic_info = wham_data)

# Fix some parameters and add random effects if required:
input$par$log_NAA_sigma = log(0.6681) # sigma as in SS
input$map$log_NAA_sigma = factor(NA) # fix sigma
input$map$log_N1_pars = factor(rep(NA, times = n_ages)) # fix NAA at y=1
# log_NAA:
input$par$log_NAA = as.matrix(log(NAA_SS)[-1,])
input$par$log_F1 = log(0.097) # as in SS
# F devs:
Fts = SS_report$derived_quants[grep(pattern = 'F_', x = SS_report$derived_quants$Label),]
Fts = Fts[1:n_years, 'Value']
F_devs = log(Fts)[-1] - log(Fts)[-n_years]
input$par$F_devs[,1] = F_devs # set F_devs
# Fix q:
# input$map$logit_q = factor(NA) # fix q
# temporal variability for omega1 and omega2: Did it in this way because SS does not estimate any link parameter
input$par$LW_re[,,1] = rep(log(1 + env1/LWpars[1]), times = n_ages)
input$par$LW_re[,,2] = rep(log(1 + env2/LWpars[2]), times = n_ages)
# deviations in selectivity parameters (this is the tricky part):
DblN_ascend_1 = 0.1817 # from SS_report, par 3
DblN_end_1 = 0.6754 # from SS_report, par 6
DblN_peak_2 = 0.2065 # from SS_report, par 1
DblN_ascend_2 = 0.7573 # from SS_report, par 3
DblN_ascend_1_devs = SS_report$parameters[184:228, 'Value']
DblN_end_1_devs = SS_report$parameters[229:273, 'Value']
DblN_peak_2_devs = SS_report$parameters[274:313, 'Value']
DblN_ascend_2_devs = SS_report$parameters[314:353, 'Value']
ascend_1 = selpars1[3] + DblN_ascend_1*DblN_ascend_1_devs # 1977-2021
end_1 = selpars1[6] + DblN_end_1*DblN_end_1_devs # 1977-2021
peak_2 = selpars2[1]*exp(DblN_peak_2*DblN_peak_2_devs) # 1982-2021, this is EXP !!!!!
ascend_2 = selpars2[3] + DblN_ascend_2*DblN_ascend_2_devs # 1982-2021
#these are the RE for WHAM:
fish1_par3_re = -log((input$data$selpars_upper[1,37]-ascend_1)/(ascend_1-input$data$selpars_lower[1,37]))-input$par$logit_selpars[1,37]
fish1_par6_re = -log((input$data$selpars_upper[1,40]-end_1)/(end_1-input$data$selpars_lower[1,40]))-input$par$logit_selpars[1,40]
surv1_par1_re = -log((input$data$selpars_upper[2,35]-peak_2)/(peak_2-input$data$selpars_lower[2,35]))-input$par$logit_selpars[2,35]
surv1_par3_re = -log((input$data$selpars_upper[2,37]-ascend_2)/(ascend_2-input$data$selpars_lower[2,37]))-input$par$logit_selpars[2,37]
# put RE values in INPUT:
input$par$selpars_re[1:input$data$n_years_model] = fish1_par3_re
input$par$selpars_re[(input$data$n_years_model+1):(input$data$n_years_model*2)] = fish1_par6_re
input$par$selpars_re[(input$data$n_years_model*2+6):(input$data$n_years_model*3)] = surv1_par1_re
input$par$selpars_re[(input$data$n_years_model*3+6):(input$data$n_years_model*4)] = surv1_par3_re
input$map$selpars_re = rep(factor(NA), times = length(input$par$selpars_re)) # fix random effects
input$map$sel_repars = rep(factor(NA), times = 6) # fix selex parameters
# Fix selex params:
input$map$logit_selpars = rep(factor(NA), times = 80)
# Only random effects for growth component (save some time):
input$random = 'growth_re'

#Run model:
fit = fit_wham(input, do.osa = FALSE, do.fit = TRUE, do.retro = FALSE)
save(fit, file = 'fit.RData')
#load('fit.RData')

# Make plots
dir.create(path = 'fit')
plot_wham_output(mod = fit, dir.main = 'fit', out.type = 'pdf')

# Plot SSB:
this_model = fit
model_name = 'WHAM'
tmp = data.frame(name = names(this_model$sdrep$value),
                 est = this_model$sdrep$value, sd = this_model$sdrep$sd)
data2 = cbind(model = model_name, year=1977:2021,
              filter(tmp, name=='log_SSB') %>% dplyr::select(-name))

plot_data = data2
ggplot(plot_data, aes(year, exp(est), ymin=exp(est-1.96*sd), ymax=exp(est+1.96*sd),
                      fill=model, color=model)) +
  ylim(0,NA) + labs(y='SSB') +
  geom_ribbon(alpha=.3, color = NA) + geom_line(lwd=1) +
  labs( color=NULL, fill=NULL) +
  scale_fill_manual(values = mycols[2:3]) +
  scale_color_manual(values = mycols[2:3]) +
  theme_bw() +
  theme(legend.position='top') 
ggsave(filename = 'compare_SSB_1.png', width = 190, height = 130, units = 'mm', dpi = 500)

# Explore results:
fit$sdrep
fit$rep[grep('nll',names(fit$rep))] %>% lapply(sum) %>% unlist
fit$rep$SDAA
fit$rep$selpars[[1]]
fit$rep$LW_par[,,1]
fit$rep$LW_par[,,2]
plot(fit$rep$SSB)
plot(fit$rep$LAA[,1], type = 'l')
fit$rep$F
plot(fit$rep$selAL[[1]][1,])
plot(fit$rep$selAL[[1]][4,])
plot(fit$rep$selAL[[2]][1,])
plot(fit$rep$selAA[[1]][1,])
plot(fit$rep$selAA[[1]][2,])
plot(fit$rep$selAA[[2]][1,])
matplot(fit$rep$phi_mat[1,,], type = 'l')
plot(fit$rep$pred_catch_pal[1,1,])


plot(SS_report$timeseries$Yr[3:47], SS_report$timeseries$SpawnBio[3:47], type = 'l', ylim = c(0, 1.3E+06))
lines(SS_report$timeseries$Yr[3:47], fit$rep$SSB, col = 2)

plot(SS_report$growthseries$`1`, type = 'l', ylim = c(7, 18))
lines(fit$rep$LAA[,1], col = 2)

plot(fit$rep$pred_index_paa[1,1,], type = 'b')
lines(fit$rep$pred_IAA[1,1,]/sum(fit$rep$pred_IAA[1,1,]), col = 2, type = 'b')


# -------------------------------------------------------------------------
# Model 2: estimate selex deviates:
# Prepare input:
input2 = prepare_wham_input(model_name="cod_2",
                           selectivity=list(model = c('len-double-normal', 'len-double-normal'),
                                            re = c('iid', 'iid'),
                                            initial_pars=list(selpars1,
                                                              selpars2),
                                            fix_pars = list(c(1,2,4,5), c(2,4:6)),
                                            n_selblocks = 2),
                           M = list(model = 'constant', re = 'none',
                                    initial_means = 0.325,
                                    est_ages = 1),
                           NAA_re = list(sigma="rec", cor = 'iid', N1_model = 0,
                                         recruit_model = 2,
                                         N1_pars = as.vector(as.matrix(NAA_SS[1,])),
                                         recruit_pars = 344300),
                           growth = list(model = 'Richards',
                                         re = c('none', 'none', 'iid_y', 'none', 'none', 'none'),
                                         init_vals = GWpars,
                                         est_pars = c(1:4)),
                           LW = list(re = c('none', 'none'),
                                     init_vals = LWpars),
                           catchability = list(re = c('none'), 
                                               initial_q = 0.97, q_lower = 0,
                                               q_upper = 1000, prior_sd = NA),
                           age_comp = 'dirichlet-pool0',
                           len_comp = 'dir-mult',
                           basic_info = wham_data)

# NAA parameters
input2$par$log_NAA_sigma = log(0.6681) # sigma as in SS
input2$map$log_NAA_sigma = factor(NA) # fix sigma
input2$map$log_N1_pars = factor(rep(NA, times = n_ages)) # fix NAA at y=0
# log_NAA:
input2$par$log_NAA = as.matrix(log(NAA_SS)[-1,])
input2$par$log_F1 = log(0.097) # as in SS
# F devs:
Fts = SS_report$derived_quants[grep(pattern = 'F_', x = SS_report$derived_quants$Label),]
Fts = Fts[1:n_years, 'Value']
F_devs = log(Fts)[-1] - log(Fts)[-n_years]
input2$par$F_devs[,1] = F_devs # set F_devs
# temporal variability for omega1 and omega2
input2$par$LW_re[,,1] = rep(log(1 + env1/LWpars[1]), times = n_ages)
input2$par$LW_re[,,2] = rep(log(1 + env2/LWpars[2]), times = n_ages)
# Turn off random effects:
input2$random = 'growth_re'

#Run model:
fit2 = fit_wham(input2, do.osa = FALSE, do.fit = TRUE, do.retro = FALSE)
save(fit2, file = 'fit2.RData')

# Explore results:
matplot(fit2$rep$phi_mat[1,,], type = 'l')
plot(fit2$rep$LAA[,1], type = 'l')
