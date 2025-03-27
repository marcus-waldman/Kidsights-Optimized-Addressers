rm(list = ls())



library(tidyverse)
setwd("C:/Users/waldmanm/Downloads")
dat = readr::read_rds("C:/Users/waldmanm/Dropbox/UNMC/phase-2/KidsightsDashboard/KidsightsExplorer/data/demo_list/zcta.rds")
zip2county = readr::read_csv("C:/Users/waldmanm/Dropbox/UNMC/phase-2/data-files/source-files/GEOCORR/zip2county.csv") %>% 
  dplyr::filter(stab == "NE") %>% 
  dplyr::mutate(zip = zcta, county = stringr::str_remove_all(CountyName, " NE")) %>% 
  dplyr::select(zip,county, afact)

poolfileloc = "C:/Users/waldmanm/Dropbox/UNMC/phase-2/KidsightsDashboard/DashboardNE22/KidsightDashboardNE22/data/implist_pool_of_NE_microdata.rds"




dat = dat %>% dplyr::select(-geometry)

head(dat)

hisp = dat %>% dplyr::filter(category_collapse == "Hispanic or Latino") %>% dplyr::arrange(-pct) %>% 
  dplyr::mutate(N = pct*N_under6) %>% 
  dplyr::arrange(-N) 

N_hisp = sum(hisp$N)
hisp = hisp %>% dplyr::mutate(n1 = round(N*500/N_hisp,0), n2 = round(.1*N,0)) %>% 
  dplyr::filter(n1>0, n2>0) %>% 
  dplyr::mutate(n = ifelse(n1>n2,n2,n1))

hisp = hisp %>% dplyr::select(category_collapse, zip,n) %>% 
  dplyr::left_join(zip2county) %>% 
  dplyr::group_by(county) %>% 
  dplyr::summarise(cateogry_collapse = category_collapse[1], n = round(sum(afact*n),0))

  


black = dat %>% 
  dplyr::filter(category_collapse == "Black or African American Alone") %>%  
  dplyr::mutate(N = pct*N_under6) %>% 
  dplyr::arrange(-N) 

N_black = sum(black$N)

black = black %>% dplyr::mutate(n1 = round(N*500/N_black,0), n2 = round(.1*N,0)) %>% 
  dplyr::filter(n1>0, n2>0) %>% 
  dplyr::mutate(n = ifelse(n1>n2,n2,n1))

black = black %>% dplyr::select(category_collapse, zip,n)




native = dat %>% 
  dplyr::filter(category_collapse == "American Indian and Alaska Native Alone") %>% 
  dplyr::arrange(-pct) %>% 
  dplyr::mutate(N = pct*N_under6) %>% 
  dplyr::arrange(-N) 


N_native = sum(native$N)

native = native %>% 
  dplyr::mutate(n1 = round(N*500/N_native,0), n2 = round(.2*N,0)) %>% 
  dplyr::filter(n1>0, n2>0) %>% 
  dplyr::mutate(n = ifelse(n1>n2,n2,n1))



sum(native$n)

native = native %>% dplyr::filter(n>0) %>% dplyr::select(category_collapse, zip,n)


low_edu = dat %>% dplyr::filter(category_collapse == "High School or Below") %>% 
  dplyr::filter(!(zip %in% hisp$zip)) %>% 
  dplyr::filter(!(zip %in% black$zip)) %>% 
  dplyr::filter(!(zip %in% native$zip)) %>% 
  dplyr::mutate(N = pct*N_under6) %>% 
  dplyr::arrange(-N)

N_lowedu =  sum(low_edu$N)

low_edu = low_edu %>% 
  dplyr::mutate(n1 = round(N*500/N_lowedu,0), n2 = round(.1*N,0)) %>% 
  dplyr::filter(n1>0, n2>0) %>% 
  dplyr::mutate(n = ifelse(n1>n2,n2,n1))

low_edu = low_edu %>% dplyr::filter(n>0) %>% dplyr::select(category_collapse, zip,n)


sum(low_edu$n)


nn = sum(hisp$n) + sum(native$n) + sum(black$n) + sum(low_edu$n)
  
hi = dat %>% 
  dplyr::filter(variable == "Income Ratio to FPL") %>% 
  dplyr::group_by(zip) %>% 
  dplyr::summarise(N = sum(N_under6), land_area = ALAND10[1]) %>% 
  dplyr::arrange(N)  %>% 
  dplyr::mutate(pop_density = log(N/land_area))

hist(hi$pop_density)


hi = hi %>% dplyr::arrange(pop_density) %>% 
  dplyr::filter(!(zip %in% hisp$zip)) %>% 
  dplyr::filter(!(zip %in% native$zip)) %>% 
  dplyr::filter(!(zip %in% black$zip)) %>% 
  dplyr::filter(!(zip %in% low_edu$zip)) %>% 
  dplyr::mutate(N_cum = cumsum(N)) %>%
  dplyr::mutate(n1 = .1*N_cum) %>% 
  dplyr::filter(n1<2000-nn)
  

low_popd = hi  %>% dplyr::mutate(category_collapse = "Low population density", n = round(.1*N,0))


low_popd = low_popd %>% dplyr::select(zip,category_collapse,n)



out = hisp %>% dplyr::bind_rows(black) %>% dplyr::bind_rows(native) %>% dplyr::bind_rows(low_edu) %>% dplyr::bind_rows(low_popd) %>% 
  dplyr::group_by(category_collapse) %>% 
  dplyr::reframe(zipcodes = paste0(sort(as.numeric(zip)),collapse = ", "), N = sum(n)) %>% 
  dplyr::mutate(`Age less than 1 Year` = N/6, `Age 1 Year` = N/6, `Age 2 Years` = N/6, `Age 3 Years` = N/6, `Age 4 Years` = N/6, `Age 5 Years` = N/6)


readr::write_csv(out, file = "to Justin Moon.csv")
