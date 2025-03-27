
raking_loss_p<-function(w,dat,target, se = NULL, bbw=rep(1,length(w))){
  xtilde = apply(dat,2,"weighted.mean", w = w*bbw)
  delta = xtilde-target
  if(is.null(se)){
    objective  = sum(delta^2)
  } else {
    objective = sum( (delta/se)^2 )
  }
  penalty = 10*(mean(w)-1)^2
  return(objective+penalty)
}


make_design_weights<-function(implist, M = 8, poolfileloc = "C:/Users/marcu/Dropbox/UNMC/phase-2/KidsightsDashboard/DashboardNE22/KidsightDashboardNE22/data/implist_pool_of_NE_microdata.rds"){
  
  library(tidyverse)
  library(srvyr)
  library(svrep)
  library(MCMCpack)
  
  pool_list = readr::read_rds(file = poolfileloc)
  
  #print(Bootstraps)
  #print(M)
  cl = parallel::makeCluster(M)
  parallel::clusterExport(cl, c("implist", "Bootstraps","raking_loss_p", "to_acs","M", "pool_list"))
  wgts_list<-pbapply::pblapply(1:M, function(m){
    
    library(tidyverse)
    library(srvyr)
    library(svrep)
    
    unmc_m = implist[[m]] %>% 
      dplyr::mutate( 
        sc_age_years = floor(years), 
        fpl_i1 = round(100*inc99/(cpi99*povertyline),0), 
        fpl_i1 = ifelse(fpl_i1>=400, 400, fpl_i1),
        fpl_i1 = ifelse(fpl_i1<50, 50, fpl_i1),
        native = as.integer(raceG=="American Indian or Alaskan Native, non-Hisp." ),
        api = as.integer(raceG=="Asian or Pacific Islander, non-Hisp."), 
        black = as.integer(raceG=="Black or African-American, non-Hisp." ), 
        other = as.integer(raceG=="Some Other Race, non-Hisp."), 
        twoplus = as.integer(raceG=="Two or More, non-Hisp."), 
        white = as.integer(raceG=="White, non-Hisp."), 
        nohisp = 1-hisp, 
        fpl0 = as.integer(fpl_i1<100), 
        fpl1 = as.integer(fpl_i1>=100 & fpl_i1<200), 
        fpl2 = as.integer(fpl_i1>=200 & fpl_i1<300), 
        fpl3 = as.integer(fpl_i1>=300 & fpl_i1<400), 
        fpl4 = as.integer(fpl_i1>=400), 
        ba = as.integer(educ_mom=="College Degree"), 
        hs = as.integer(educ_mom=="High School Graduate (including Equivalency)"), 
        lths = as.integer(educ_mom=="Less than High School Graduate"), 
        aa = as.integer(educ_mom=="Some College or Associate's Degree"), 
        source = "UNMC", 
        wgt = 1)
    
    acs_m = pool_list[[m]] %>% 
      dplyr::filter(source == "ACS") %>% 
      to_acs() 
    
    outlist_m <- lapply(0:5, function(yr_x){
      
      # Get imputation-specific weights (no bootstrap)
      acs_mx = acs_m %>% 
        dplyr::filter(sc_age_years == yr_x) %>% 
        dplyr::select(stratum,serial, perwt, metro, sc_age_years, hisp:fpl4)    
      
      targets_acs_mx = acs_mx %>% 
        pivot_longer(col = hisp:fpl4, names_to = "variable", values_to = "value") %>% 
        srvyr::as_survey_design(strata = stratum, weights = perwt, id = serial) %>%  
        dplyr::group_by(sc_age_years, variable) %>% 
        dplyr::summarise(pcts = srvyr::survey_mean(value, proportion = T, prop_method = "beta"))
      
      unmc_mx = unmc_m %>% 
        dplyr::filter(sc_age_years == yr_x) %>% 
        dplyr::select(ResponseId, dplyr::all_of(targets_acs_mx$variable))
      n_mx = nrow(unmc_mx)
      
      
      optim_mx = optim(
        par = rep(1,nrow(unmc_mx)), 
        fn = raking_loss_p, 
        dat = unmc_mx %>% dplyr::select(-ResponseId), 
        target=targets_acs_mx %>%  purrr::pluck("pcts"),
        se=targets_acs_mx %>% purrr::pluck("pcts_se"),
        lower = .1,
        upper = 10,
        method = "L-BFGS-B", 
        control = list(maxit = 1E4) 
      )
      
      wgt_mx = optim_mx$par
      
      out_mx = unmc_mx %>% 
        dplyr::mutate(.imp = m, sc_age_years = yr_x)  %>% 
        dplyr::select(.imp, ResponseId, sc_age_years) %>% 
        dplyr::mutate(wgt = wgt_mx/mean(wgt_mx))
      
      acs_bperwt_mx = acs_mx %>%
        dplyr::filter(sc_age_years == yr_x) %>% 
        srvyr::as_survey_design(strata = stratum, weights = perwt, id = serial) %>%  
        svrep::as_bootstrap_design(replicates = Bootstraps) %>% 
        weights(type = "analysis")
      
      tmplist<-lapply(1:Bootstraps, function(b){
        
        targets_acs_mxb = acs_mx %>% 
          dplyr::mutate(perwt = acs_bperwt_mx[,b]) %>% 
          dplyr::select(stratum,serial, perwt, sc_age_years, hisp:fpl4) %>% 
          pivot_longer(col = hisp:fpl4, names_to = "variable", values_to = "value") %>% 
          srvyr::as_survey_design(strata = stratum, weights = perwt, id = serial) %>%  
          dplyr::group_by(sc_age_years, variable) %>% 
          dplyr::summarise(pcts = weighted.mean(value, w = perwt)) %>% 
          dplyr::left_join(targets_acs_mx %>% dplyr::select(sc_age_years, variable, pcts_se), by = c("sc_age_years","variable"))
        
        
        bbw_b = MCMCpack::rdirichlet(1, rep(1,n_mx)) %>% as.vector()
        optim_mxb = optim(
          par =wgt_mx, 
          fn = raking_loss_p, 
          dat = unmc_mx %>% dplyr::select(-ResponseId), 
          target=targets_acs_mxb %>% purrr::pluck("pcts"),
          se=targets_acs_mxb %>% purrr::pluck("pcts_se"),
          bbw = bbw_b,
          lower = .1,
          upper = 10,
          method = "L-BFGS-B", 
          control = list(maxit = 1E4) 
        )
        
        
        wgts_mxb = optim_mxb$par*bbw_b
      })
      
      for(b in 1:Bootstraps){
        wgts_mxb = tmplist[[b]]
        out_mx = out_mx %>% dplyr::mutate(wgtb = wgts_mxb/mean(wgts_mxb))
        names(out_mx)[ncol(out_mx)] = paste0("wgt", b)
      }
      
      
      return(out_mx)
    })
    
    ret_m = outlist_m %>% dplyr::bind_rows()
    
    return(ret_m)
    
  }, cl=cl)
  parallel::stopCluster(cl)
  
  
  readr::write_rds(wgts_list, file = "data/design_weights.rds")
  return(wgts_list)
  
  
}



