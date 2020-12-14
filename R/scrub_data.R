
#install.packages(tidyxl)
#install.packages(lubridate)
#install.packages(plotly)
library(tidyxl)
library(lubridate)
library(plotly)
# a few helper functions
address_fn = function(alpha, start){
  out = list()
  for(i in start:(start + 30)){
    out[[i]] = paste0(alpha,i)  
  }
  out = do.call("rbind", out)
  return(out)
}

date_fn = function(year, mon, start){
  out = list()
  for(i in start:(start + 30)){
    out[[i]] = paste0(year,"-", mon,"-",i)  
  }
  out = do.call("rbind", out)
  return(out)
}

#load the data 
dat = tidyxl::xlsx_cells("./data/Ajmer_Rainfall data_1973-2008.xlsx")

years = dat %>% group_by(sheet) %>% summarise(n=n())
#start rows of stations (assuming consistent, looks like every 50th row)
stations_i = seq(1,1100,50)


stations_dat = list()
run_nbr = 0; for(year in 1:NROW(years)){
  year_dat = dat %>% filter(sheet == years$sheet[year]) 
  for (station in 1:NROW(stations)){
    
    station_name = year_dat[year_dat$address==paste0("C", stations_i[station]),"character"]
    if(NROW(station_name)>0){
      run_nbr = run_nbr + 1
      
      mons = tolower(month.abb)
      cols = LETTERS[2:13]
      mon_out = list()
      for(mon in 1:12){
        mon_dat = year_dat %>% filter(address %in% c(address_fn(cols[mon],stations_i[station] + 5))) %>% select(numeric)
        mon_dt = date_fn(years$sheet[year],mon,1)
        mon_out[[mon]] = data.frame(mon_dt, mon_dat)
      }
      
      mon_out = do.call("rbind", mon_out)
      mon_out$dt = lubridate::ymd(mon_out$mon_dt)
      mon_out = mon_out %>% filter(!is.na(dt))
      
      stations_dat[[run_nbr]] = data.frame(year = years$sheet[year], station_name = station_name$character, 
                                           dt = mon_out$dt, numeric = mon_out$numeric)
    }
  }
}
stations_dat = do.call("rbind", stations_dat)


#check the output
station_names = stations_dat %>% group_by(station_name) %>% summarise(cnt = n(), avg = mean(numeric))

plot_ly(stations_dat %>% filter(station_name == station_names$station_name[1]), y = ~numeric, x=~dt, type="scatter", mode= "line")

write.csv(stations_dat, "./data/Ajmer_Rainfall data_1973-2008.csv")


