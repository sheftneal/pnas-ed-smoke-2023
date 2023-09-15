source("scripts/00_init.R")

#load CA shapefile
ca <- tigris::states() %>% filter(STATEFP=="06")


##################################################################                                      
##############   Run Regressions   ###############################
##################################################################                      


        ######### panel 1: Google Searches ##############
          
          
          # load google data and subset to CA
          goog <- read_rds("data/google_trends_smoke_DMA_normalized_with_covariates.RDS")


          goog <- goog %>% mutate(month=month(date), year=year(date)) %>% mutate(dmamonth = paste(dma,month,sep="_")) %>% 
            drop_na(hits,smokePM) %>% 
            mutate(state=substr(geo,4,5)) %>% 
            filter(state=="CA") %>% 
            filter(keyword=="air quality")
          
          goog = goog %>% mutate(smokePM_pred = smokePM,
                                 bin0 = as.numeric(smokePM_pred <= 0),
                                 bin0_5 = as.numeric(smokePM_pred > 0 & smokePM_pred <= 5),
                                 bin5_10 = as.numeric(smokePM_pred > 5 & smokePM_pred <= 10),
                                 bin10_15 = as.numeric(smokePM_pred > 10 & smokePM_pred <= 15),
                                 bin15_20 = as.numeric(smokePM_pred > 15 & smokePM_pred <= 20),
                                 bin20_25 = as.numeric(smokePM_pred > 20 & smokePM_pred <= 25),
                                 bin25_50 = as.numeric(smokePM_pred > 25 & smokePM_pred <= 50),
                                 bin50_100 = as.numeric(smokePM_pred > 50 & smokePM_pred <= 100),
                                 bin100 = as.numeric(smokePM_pred > 100),
                                 regbin1 = bin0_5, #0-50 percentile
                                 regbin2 = bin5_10, #50-75 percentile
                                 regbin3 = bin10_15 + bin15_20 + bin20_25,#75-90 percentile
                                 regbin4 = bin25_50 ,  #90+ percentile
                                 regbin5 =  bin50_100 + bin100 #95+ percentile
          )
          
          
          # linear model
          feols(hits~ poly(smokePM,4,raw = TRUE) + temperature+ precipitation | dmamonth+date, goog) #linear model
          
          model = feols(hits~ regbin1 + regbin2 + regbin3 + regbin4 + regbin5 + temperature+ precipitation | dmamonth+date, goog) #linear model
          
          
          df <- data.frame(bin = 1:5, coef = coef(model)[1:5], se = se(model)[1:5]) %>% mutate(lb = coef + qnorm(0.05)*se, ub = coef + qnorm(0.95)*se)
          df$baserate = mean(goog$hits, na.rm = T)
          write_rds(df, file = "data/clean/beh-reg-binned-response-google.rds")
          
        
          
          
        #########  panel 2: TRAFFIC ###############################
          
          
          
          datawd <- "data/raw/caltran"
          
          
          ca <- tigris::states() %>% filter(STATEFP=="06")
          county <- tigris::counties() %>% filter(STATEFP=="06")
          sdf <- list.files(paste(datawd,"station-day-files",sep="/"), pattern = ".txt" )
          smd <- list.files(paste(datawd, "station-metadata-files", sep = "/"), pattern =".txt")
          
          
          #read in flow volume
          sdf_list <- list(); length(sdf_list)<-length(sdf)
          for(i in 1:length(sdf_list)){
            
            sdf_list[[i]] <- read.delim(paste(datawd,"station-day-files",sdf[i], sep="/"), sep = ",", header = F)
            
          }
          sdf_data <- data.frame(rbindlist(sdf_list))
          names(sdf_data)<-c("time","stationid","district","route","direction","lanetype","stationlength","samples","perobs","totflow",paste("delay",seq(35,60,5),sep=""))
          
          #drop missing traffic flow
          sdf_data <- sdf_data %>% drop_na(totflow)
          sdf_data <- sdf_data %>% arrange(stationid, time)
          
          
          #read in flow volume metadata
          smd_list <- list(); length(smd_list)<-length(smd)  
          for(i in 1:length(smd_list)){
            
            try( #some meta data files are empty so skip those
              smd_list[[i]] <- read.delim(paste(datawd,"station-metadata-files",smd[i], sep="/"), header = T)
            )
            print(i)
          }
          smd_data <- data.frame(rbindlist(smd_list))
          names(smd_data)<-c("stationid","fwy","direction","district","county","city","statepost","abspost","lat","lon","length","type","lanes","name",paste("userid",1:4,sep="_"))
          
          smd_data <- smd_data %>% drop_na(lat, lon) %>% dplyr::select(stationid, district,county, city, lat, lon, lanes, name) %>% distinct() %>% 
            mutate(incity = !is.na(city)) %>% 
            group_by(stationid, district) %>% summarise(county = max(county, na.rm = T),lat = mean(lat, na.rm = T), lon = mean(lon, na.rm = T), lanes = mean(lanes, na.rm = T), incity = mean(incity, na.rm = T), 
                                                        lat_min = min(lat, na.rm = T), lat_max = max(lat, na.rm = T), lon_min = min(lon, na.rm = T), lon_max = max(lon, na.rm = T)) %>% 
            mutate(lat_move = lat_max - lat_min, lon_move = lon_max - lon_min)
          
          #check max - min lat + lon to make sure no big moves
          
          data <- left_join(sdf_data, smd_data)
          write_rds(data, file = "data/clean/caltran_station_daily_traffic_volume_2006_2017.rds", compress = "gz")
          
          
          
          #now read in all data and subset by obs
          data <- read_rds("data/clean/caltran_station_daily_traffic_volume_2006_2017.rds")
          
          
          
          
          
          # obs_count = data %>% filter(incity != 0)  %>% group_by(stationid) %>% summarise(nobs = n())
          obs_count = data %>% group_by(stationid) %>% summarise(nobs = n())
          data <- left_join(data, obs_count)
          data <- filter(data, nobs > .9*4383) #filter to stations with at least 90% obs non-missing over the study period
          
          data = data %>% drop_na(lon, lat) #drop stations with missing lat-lon
          
          #now bring in the smoke data and join with traffic volume
          
          #create sf object with station locs
          caltran_locs <- data %>% dplyr::select(stationid, lon, lat) %>% distinct() %>% st_as_sf(coords = c("lon","lat"), crs = crs(ca))
          
         
          #read in gridded smoke PM data available online
          spm <- read_rds("~/BurkeLab Dropbox/Projects/daily-10km-smokePM/final/10km_grid/smokePM2pt5_predictions_daily_10km_20060101-20201231.rds")
          #read in smoke PM grid
          spm_grid <- read_sf("~/BurkeLab Dropbox/Projects/daily-10km-smokePM/final/10km_grid/10km_grid_wgs84/10km_grid_wgs84.shp")
          caltran_locs <- caltran_locs %>% st_transform(crs(spm_grid))
          
          
          #identify which grid cells caltran stations fall into
          
          station_sm_overlap <- st_within(caltran_locs, spm_grid) %>% unlist()
          station_sm_overlap <- data.frame(stationid = caltran_locs$stationid, cellrow = station_sm_overlap, cell = spm_grid$ID[station_sm_overlap])
          
          #check it worked by plotting some random smoke grid cells and stations that fall within them
          #(can re-run these 3 lines to check for however many obs that it worked correctly)
          random_row = round(runif(n =1, min = 1, max = nrow(station_sm_overlap)))
          plot(st_geometry(filter(spm_grid, ID == station_sm_overlap$cell[random_row]) )) #grid boundaries
          plot(st_geometry(filter(caltran_locs, stationid == station_sm_overlap$stationid[random_row])), add = T, col = 'red') #a station we think is in that grid
          
          
          #identify unique set of smoke PM grid cells for which we have caltrans station data
          caltran_smoke_cells <- sort(unique(station_sm_overlap$cell))
          
          #subset smoke pm data to those cells and our sample period
          spm <-  spm %>% 
            filter(grid_id_10km %in% caltran_smoke_cells & (date >= "2006-01-01" & date <= "2017-12-31") ) 
          
          
          #now bring smoke PM grid cell into the caltrans station data
          
          data <- data %>% left_join(station_sm_overlap[,c("stationid","cell")])  
          data$date <- as.Date(substr(data$time,1 ,10 ), format = "%m/%d/%Y") #convert char string to date format
          data <- data %>% rename(grid_id_10km = cell)
          
          #now join. becasue smokePM data sparse any days without smoke will not be matched and so NA needs to be replaed with 0s
          data <- data %>% left_join(spm) %>% mutate(smokePM_pred = replace(smokePM_pred, is.na(smokePM_pred), 0))
          
          data$doy <- yday(data$date) #day of yr
          data$dow <- wday(data$date) #day of wk (1 = Sun)
          data$year <- year(data$date)
          data$month = month(data$date)
          data$season <- as.numeric(data$month %in% 6:10)
          data$season_yr <- paste(data$season, data$year,sep="-")
          
          data$smokePM1 = data$smokePM_pred
          data$smokePM2 = data$smokePM_pred^2
          data$smokePM3 = data$smokePM_pred^3
          data$smokePM4 = data$smokePM_pred^4
          
          #bring in temperature
          tmp <-  read_rds('data/clean/zip_day_prism.rds') 
          grid <- raster("/Volumes/TERAnodon5/PRISM/daily/tmax/PRISM_tmax_stable_4kmD2_20071214_bil/PRISM_tmax_stable_4kmD2_20071214_bil.bil"); grid[] <- 1:ncell(grid);
          tmp$date <- as.Date(paste(tmp$year, tmp$month, tmp$day, sep = "-"), format = "%Y-%m-%d")
          
          data$cell <- cellFromXY(grid, as.matrix(data[,c("lon","lat")]))
          
          data <- left_join(data, tmp[,c("date","cell","tmx","ppt")])
          
          data <- data %>% arrange(stationid, time) 
          write_rds(data, file = "data/clean/caltran_analysis_data.rds", compress ="gz")
          
          
          
          
          data <- read_rds("data/clean/caltran_analysis_data.rds")
          
          
          
          data <- data  %>% mutate(
            bin0 = as.numeric(smokePM_pred <= 0),
            bin0_5 = as.numeric(smokePM_pred > 0 & smokePM_pred <= 5),
            bin5_10 = as.numeric(smokePM_pred > 5 & smokePM_pred <= 10),
            bin10_15 = as.numeric(smokePM_pred > 10 & smokePM_pred <= 15),
            bin15_20 = as.numeric(smokePM_pred > 15 & smokePM_pred <= 20),
            bin20_25 = as.numeric(smokePM_pred > 20 & smokePM_pred <= 25),
            bin25_50 = as.numeric(smokePM_pred > 25 & smokePM_pred <= 50),
            bin50_100 = as.numeric(smokePM_pred > 50 & smokePM_pred <= 100),
            bin100 = as.numeric(smokePM_pred > 100 )
          )
          
          data <- data %>%
            mutate(
              regbin1 = bin0_5, #0-50 percentile
              regbin2 = bin5_10, #50-75 percentile
              regbin3 = bin10_15 + bin15_20 + bin20_25,#75-90 percentile
              regbin4 = bin25_50 ,  #90+ percentile
              regbin5 =  bin50_100 + bin100 #95+ percentile
            )
          
          
          
          data$county_mth <- paste(data$county, data$month, sep = "-")
          
           
          data2 = filter(data, incity == 1) 
          
          vstat_locs <- data2 %>% dplyr::select(stationid, lon, lat) %>% distinct()
          
          
          model2 <- feols(totflow ~ regbin1 + regbin2 + regbin3 + regbin4 + regbin5 + tmx + ppt |  stationid  + county_mth + dow + year, data = data, cluster = "stationid")
          model2_lag <- feols(totflow ~ l(regbin1,0:3) + l(regbin2,0:3) + l(regbin3,0:3) + l(regbin4,0:3) + l(regbin5,0:3)+ tmx + ppt | stationid  + county_mth + doy + dow + year, data = data, cluster = "stationid", panel.id = c("stationid","date"))
          model2_lag_city <- feols(totflow ~ l(regbin1,0:7) + l(regbin2,0:7) + l(regbin3,0:7) + l(regbin4,0:7) + l(regbin5,0:7)+ tmx + ppt| stationid + county_mth + dow + year, data = data2, cluster = "stationid", panel.id = c("stationid","date"))
          
          
          b1 = get_bin_response_cumulative(model2_lag, "regbin1",3,"stationid")
          b2 = get_bin_response_cumulative(model2_lag, "regbin2",3,"stationid")
          b3 = get_bin_response_cumulative(model2_lag, "regbin3",3,"stationid")
          b4 = get_bin_response_cumulative(model2_lag, "regbin4",3,"stationid")
          b5 = get_bin_response_cumulative(model2_lag, "regbin5",3,"stationid")
          
          
          b11 = get_bin_response_cumulative(model2_lag_city, "regbin1",7,"stationid")
          b22 = get_bin_response_cumulative(model2_lag_city, "regbin2",7,"stationid")
          b33 = get_bin_response_cumulative(model2_lag_city, "regbin3",7,"stationid")
          b44 = get_bin_response_cumulative(model2_lag_city, "regbin4",7,"stationid")
          b55 = get_bin_response_cumulative(model2_lag_city, "regbin5",7,"stationid")

          #df = data.frame(bin = 1:5, coef = coef(model2)[1:5], se =  se(model2)[1:5]) %>% mutate(lb = coef + qnorm(0.025)*se, ub = coef + qnorm(0.975)*se)
          
          df = data.frame(bin = 1:5, coef = c(b1[1],b2[1],b3[1],b4[1],b5[1]), se = c(b1[2],b2[2],b3[2],b4[2],b5[2])) %>% mutate(lb = coef + qnorm(0.025)*se, ub = coef + qnorm(0.975)*se)
          df$baserate = mean(data$totflow, na.rm = T)
          write_rds(df, file = "data/clean/beh-reg-binned-response-traffic.rds")
          
          
          
        ######### panel 3:  National Park Visitations ##############
                
                #read in the NP data
                          np <- list()
                          npf <- list.files("data/raw/NPvisits/", pattern = "Visitation by Month")
                          for (i in 1:length(npf)){np[[i]] = read_csv(paste("data/raw/NPvisits/",npf[i],sep=""))}
                          np = data.frame(rbindlist(np, fill = T))
                          np = np[,1:18]
                          np <- np %>% dplyr::select(-AnnualTotal, -Textbox4)
                          
                          np2 <- np %>% pivot_longer(names_to = "month",cols = 2:13 )
                          np2$mo <- NA
                          np2$mo[np2$month=="JAN"]<-1
                          np2$mo[np2$month=="FEB"]<-2
                          np2$mo[np2$month=="MAR"]<-3
                          np2$mo[np2$month=="APR"]<-4
                          np2$mo[np2$month=="MAY"]<-5
                          np2$mo[np2$month=="JUN"]<-6
                          np2$mo[np2$month=="JUL"]<-7
                          np2$mo[np2$month=="AUG"]<-8
                          np2$mo[np2$month=="SEP"]<-9
                          np2$mo[np2$month=="OCT"]<-10
                          np2$mo[np2$month=="NOV"]<-11
                          np2$mo[np2$month=="DEC"]<-12
                          
                          np2$date <- as.Date(paste(np2$mo, "01",np2$Year, sep = "-"), format ="%m-%d-%Y" )
                          np2 <- np2 %>% rename(year = Year, park = Park)
                          
                          
                          np <- np2; rm(np2)
                          np <- np %>% drop_na(lon, lat, park)
                          np <- np %>% filter(year >=2006 & year<=2017)
                          
                          
                          
                          #create sf object with station locs
                          np_locs <- np %>% dplyr::select(park, lon, lat) %>% distinct() %>% st_as_sf(coords = c("lon","lat"), crs = crs(ca))
                
                                  
                        #show locations of data
                        plot(st_geometry(ca))
                         plot(st_geometry(np_locs), pch = 21, col = 'red3', bg = 'gray90', cex = 0.4, lwd = 0.5, add = T)
                
                         
                  #read in the NP notes
                         
                         np_notes <- list()
                         npf_notes <- list.files("data/raw/NPvisits/", pattern = "Comments")
                         for (i in 1:length(npf_notes)){
                           np_notes[[i]] = read_csv(paste("data/raw/NPvisits/",npf_notes[i],sep=""),skip = 3)
                           np_notes[[i]]$park <- as.character(read_csv(paste("data/raw/NPvisits/",npf_notes[i],sep=""),skip = 0)[1,1])
                         
                           }
                         np_notes = data.frame(rbindlist(np_notes, fill = T)) %>% 
                                    mutate(date = as.Date(CollectedDate, format = "%m/%d/%Y"),
                                           year = year(date),
                                           month = month(date)
                                        )
                         
                         
                         
                         
                         np_notes$firemention  <- np_notes$closuremention <- np_notes$smokemention <- 0
                         
                         np_notes$firemention[grep(x = np_notes$Comments, pattern = "fire")]<-1
                         np_notes$closuremention[grep(x = np_notes$Comments, pattern ="close")]<-1
                         np_notes$closuremention[grep(x = np_notes$Comments, pattern ="closure")]<-1
                         np_notes$smokemention[grep(x = np_notes$Comments, pattern = "smoke")]<-1
                         
                         np_notes <- np_notes %>% filter(date>="2006-01-01" & date <="2017-12-31")
                         np_notes <- np_notes %>% dplyr::select(park, year, month, firemention, smokemention, closuremention)         
                
                #process smoke PM data for NPs         
                                       
                              #read in gridded smoke PM data available online
                              spm <- read_rds("~/BurkeLab Dropbox/Projects/daily-10km-smokePM/final/10km_grid/smokePM2pt5_predictions_daily_10km_20060101-20201231.rds")
                              #read in smoke PM grid
                              spm_grid <- read_sf("~/BurkeLab Dropbox/Projects/daily-10km-smokePM/final/10km_grid/10km_grid_wgs84/10km_grid_wgs84.shp")
                              np_locs <- np_locs %>% st_transform(crs(spm_grid))
                              
                              
                              #identify which grid cells np stations fall into
                              
                              station_sm_overlap <- st_within(np_locs, spm_grid) %>% unlist()
                              station_sm_overlap <- data.frame(stationid = np_locs$park, cellrow = station_sm_overlap, cell = spm_grid$ID[station_sm_overlap])
                              
                              #check it worked by plotting some random smoke grid cells and stations that fall within them
                              #(can re-run these 3 lines to check for however many obs that it worked correctly)
                              random_row = round(runif(n =1, min = 1, max = nrow(station_sm_overlap)))
                              plot(st_geometry(filter(spm_grid, ID == station_sm_overlap$cell[random_row]) )) #grid boundaries
                              plot(st_geometry(filter(np_locs, park == station_sm_overlap$stationid[random_row])), add = T, col = 'red') #a station we think is in that grid
                              
                              
                              #identify unique set of smoke PM grid cells for which we have nps station data
                              np_smoke_cells <- sort(unique(station_sm_overlap$cell))
                              
                              #subset smoke pm data to those cells and our sample period
                              spm <-  spm %>% 
                                filter(grid_id_10km %in% np_smoke_cells & (date >= "2006-01-01" & date <= "2017-12-31") ) 
                              
                              
                              #now bring smoke PM grid cell into the nps station data
                              names(station_sm_overlap)[1]<-"park"
                              
                              data <- np %>% left_join(station_sm_overlap[,c("park","cell")])  
                              #data$date <- as.Date(substr(data$time,1 ,10 ), format = "%m/%d/%Y") #convert char string to date format
                              data <- data %>% rename(grid_id_10km = cell)
                              
                              
                              #need to take spm from daily to monthly bin counts
                              spm_m <- spm %>% 
                                    mutate(
                                    bin0 = as.numeric(smokePM_pred <= 0),
                                    bin0_5 = as.numeric(smokePM_pred > 0 & smokePM_pred <= 5),
                                    bin5_10 = as.numeric(smokePM_pred > 5 & smokePM_pred <= 10),
                                    bin10_15 = as.numeric(smokePM_pred > 10 & smokePM_pred <= 15),
                                    bin15_20 = as.numeric(smokePM_pred > 15 & smokePM_pred <= 20),
                                    bin20_25 = as.numeric(smokePM_pred > 20 & smokePM_pred <= 25),
                                    bin25_50 = as.numeric(smokePM_pred > 25 & smokePM_pred <= 50),
                                    bin50_100 = as.numeric(smokePM_pred > 50 & smokePM_pred <= 100),
                                    bin100 = as.numeric(smokePM_pred > 100),
                                        regbin1 = bin0_5, #0-50 percentile
                                        regbin2 = bin5_10, #50-75 percentile
                                        regbin3 = bin10_15 + bin15_20 + bin20_25,#75-90 percentile
                                        regbin4 = bin25_50 ,  #90+ percentile
                                        regbin5 =  bin50_100 + bin100, #95+ percentile
                                      year = year(date), month = month(date)) %>% 
                                    group_by(grid_id_10km, year, month) %>% 
                                    summarise(spm = mean(smokePM_pred, na.rm = T), spm_max = max(smokePM_pred, na.rm = T), regbin1 = sum(regbin1, na.m  = T), regbin2 = sum(regbin2, na.rm = T), regbin3 = sum(regbin3, na.rm = T), regbin4 = sum(regbin4, na.rm = T), regbin5 = sum(regbin5, na.rm = T))
                              
                
                  #now join. becasue smokePM data sparse any days without smoke will not be matched and so NA needs to be replaed with 0s
                      
                      data <- data %>% dplyr::select(-month) %>% rename(month = mo) %>% dplyr::select(-date)
                      data <- data %>% left_join(spm_m, relationship = "many-to-many") %>% 
                              mutate(regbin1 = replace(regbin1, is.na(regbin1), 0)) %>% 
                              mutate(regbin2 = replace(regbin2, is.na(regbin2), 0))%>% 
                              mutate(regbin3 = replace(regbin3, is.na(regbin3), 0))%>% 
                              mutate(regbin4 = replace(regbin4, is.na(regbin4), 0))%>% 
                              mutate(regbin5 = replace(regbin5, is.na(regbin5), 0),
                                     spm = replace(spm, is.na(spm),0),
                                     spm_max = replace(spm_max, is.na(spm_max),0))
                      
                      
                  #define relevant variables prior to analysis    
                      data$season <- as.numeric(data$month %in% 6:10)
                      data$season_yr <- paste(data$season, data$year,sep="-")
                      
                      data$park_yr <- paste(data$park, data$year, sep = "-")
                      data$park_moy <- paste(data$park, data$month, sep = "-")
                      data$spm1 = data$spm
                      data$spm2 = data$spm^2
                      data$spm3 = data$spm^3
                      data$spm4 = data$spm^4
                      
                      data$spm_max1 = data$spm_max
                      data$spm_max2 = data$spm_max^2
                      data$spm_max3 = data$spm_max^3
                      data$spm_max4 = data$spm_max^4
                      
                      
                      data$logval <- log(data$value)
                      
                      data <- data %>% left_join(np_notes) %>% 
                              mutate(firemention = replace(firemention, is.na(firemention), 0),
                                     closuremention = replace(closuremention, is.na(closuremention), 0),
                                     smokemention = replace(smokemention, is.na(smokemention), 0))
                
                          
                    wtdf <- data %>% group_by(park) %>% summarise(aveVisit = mean(value, na.rm = T))
                    data= data %>% left_join(wtdf)
                
                    
                    #add temperature
                    
                    #read in cell-day tmp,  aggregate to cell-month, assign cell, and join
                    tmp <-  read_rds('data/clean/zip_day_prism.rds') 
                    grid <- raster("/Volumes/TERAnodon5/PRISM/daily/tmax/PRISM_tmax_stable_4kmD2_20071214_bil/PRISM_tmax_stable_4kmD2_20071214_bil.bil"); grid[] <- 1:ncell(grid);
                
                    #go daily to monthly
                    tmp <- tmp %>% group_by(cell,year, month ) %>% 
                            summarise(tmx = mean(tmx, na.rm = T), ppt = sum(ppt, na.rm = T))
                    
                    tmp$lon <- xyFromCell(grid, tmp$cell)[,1]
                    tmp$lat <- xyFromCell(grid, tmp$cell)[,2]
                    
                    #we only have data for zipcodes and some nat parks not in zips 
                    nearest = FNN::get.knnx(query = data[, c("lon", "lat")],data = tmp[,c("lon","lat")], k=1)
                    data$cell <- tmp$cell[nearest$nn.index]
                    
                    
                    data <- left_join(data, tmp[,c("cell","year","month","tmx","ppt")])
                    
                
                
              #estimate models  
                    
                model <- feols(logval ~ regbin1 + regbin2 + regbin3 + regbin4 + regbin5 + tmx + ppt + firemention + closuremention | park +  year + park_moy, data = data, cluster = "park")
                
                
                df <- data.frame(bin = 1:5, coef = coef(model)[1:5], se = se(model)[1:5]) %>% mutate(lb = coef + qnorm(0.05)*se, ub = coef + qnorm(0.95)*se)
                
                write_rds(df, file = "data/clean/beh-reg-binned-response-np.rds")
                
                

  ##################################################################                                      
  ##############   PLOT   ##########################################
  ##################################################################                      
      
                
                
                
      pdf("figures/raw/behavior-responses-3panel-raw.pdf", width = 12, height = 4)
              
              par(mfrow = c(1,3))
              
              #panel a
              df = read_rds("data/clean/beh-reg-binned-response-google.rds")
              plot(1:5, 1:5, axes = F, col = NA, xlab = "",ylab = "", xlim = c(0.5,5.5),ylim =c(-50, 50))
              abline(h = 0, col = 'red', lwd = 0.5)
              segments(x0 = 1:5, y0 = df$lb, y1 = df$ub, col = 'navy', lwd = 3)
              points(df$coef, pch = 16, cex=3, col = 'navy')
              
              axis(2, las = 2, at = seq(-50, 50, 10), cex.axis = 1.5)
              mtext(side = 3, text  = "Air Quality Salience", adj = 0, cex=2)
              mtext(side = 1, text  = "Smoke PM2.5",cex = 1.5)
              mtext(side = 2, text  = "Search Index",cex = 1.5,line=2)
              
              
              
              
              
              #panel b
              df = read_rds("data/clean/beh-reg-binned-response-traffic.rds") %>% mutate(coef = coef/baserate, lb = lb/baserate, ub = ub/baserate)
              plot(1:5, 1:5, axes = F, col = NA, xlab = "",ylab = "", xlim = c(0.5,5.5),ylim =c(-.1, .1))
              abline(h = 0, col = 'red', lwd = 0.5)
              segments(x0 = 1:5, y0 = df$lb, y1 = df$ub, col = 'navy', lwd = 3)
              points(df$coef, pch = 16, cex=3, col = 'navy')
              
              axis(2, las = 2, at = seq(-0.1, .1, .02), labels = seq(-0.1, .1, .02)*100, cex.axis = 1.5)
              mtext(side = 3, text  = "Traffic Volume", adj = 0, cex=2)
              mtext(side = 1, text  = "Smoke PM2.5",cex = 1.5)
              mtext(side = 2, text  = "% change",cex = 1.5,line=2)
              
              
              
              
              #panel c
              df = read_rds("data/clean/beh-reg-binned-response-np.rds")
              plot(1:5, 1:5, axes = F, col = NA, xlab = "",ylab = "", xlim = c(0.5,5.5),ylim =c(-.05, .05))
              abline(h = 0, col = 'red', lwd = 0.5)
              segments(x0 = 1:5, y0 = df$lb, y1 = df$ub, col = 'navy', lwd = 3)
              points(df$coef, pch = 16, cex=3, col = 'navy')
              
              axis(2, las = 2, at = seq(-0.05, .05, .01), labels = seq(-0.05, .05, .01)*100, cex.axis = 1.5)
              mtext(side = 3, text  = "Recreation Visits", adj = 0, cex=2)
              mtext(side = 1, text  = "Smoke PM2.5",cex = 1.5)
              mtext(side = 2, text  = "% change",cex = 1.5,line=2)
      
      dev.off()
      
      
