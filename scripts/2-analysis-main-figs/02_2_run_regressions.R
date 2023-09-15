source("scripts/00_init.R")

        data <- read_rds('data/analysis-data-private.rds') #data frame
        pdata <- panel(data, panel.id = c("zip","date"), duplicate.method = "first") #put into panel data format for feols
        rm(data)
        gc()  
        
    

               #calculate base rates for figures
                brates <-  pdata %>% as.data.frame() %>% 
                                summarise(across(ed_visit_rate:ed_other_infection_rate, ~weighted.mean(.x,pdata$avePop, na.rm = T )))
                
                brates <- data.frame(outcome = names(brates), brate = as.numeric(brates)) %>%
                                      mutate(brate = round(brate, 2)) %>%  arrange(-brate)
          
                write_rds(brates, file = "data/clean/fig3-outcome-base-rates.rds")
             
                
          ##############################################################################################################              
          # Run regressions      
          ##############################################################################################################                            
            
                
          FE <- "zip + county_month + season_yr + dow"
            
            exp_bin <- "l(regbin1, 0:7) + l(regbin2, 0:7) + l(regbin3, 0:7) + l(regbin4, 0:7)+ l(regbin5, 0:7) + tmx + ppt + dist2fire"
            
          #Binned Models
            
                outcomes <- names(pdata)[grep(x = names(pdata), pattern = "_rate")]
                
                #pull out primary diagnoses top-level groups
                outcomes = outcomes[outcomes %in% outcomes[grep(x = outcomes, pattern ="_.*_.*_rate")]==F]
                drop_col <- names(pdata)[grep(x = names(pdata), pattern = "_rate")][names(pdata)[grep(x = names(pdata), pattern = "_rate")] %in% names(pdata)[grep(x = names(pdata), pattern = "_rate")][grep(x = names(pdata)[grep(x = names(pdata), pattern = "_rate")], pattern ="_.*_.*_rate")]==T]
                
                #to save memory limit data frame to outcomes we want to use for this exercise
                pdata <- pdata[,names(pdata) %in% drop_col == F]
                pdata <- panel(pdata, panel.id = c("zip","date"), duplicate.method = "first") 
                
                
                #initialize list for storing results
                model_output <- list(); length(model_output)<- length(outcomes)
                
                #loop over outcomes and run regressions
                for(i in 1:length(outcomes)){
                  
                  eqn <-get_eqn(outcomes[i],exp_bin,FE) #function defined in script 00_init.R
                  mod <- feols(eqn, data = pdata, weights = pdata$avePop, cluster = "zip" ) #run reg
                  model_output[[i]] <- get_bin_response(mod) #function defined in script 00_init.R
                  model_output[[i]]$outcome <- outcomes[i] #add outcome label
                  print(paste("done with ",outcomes[i],sep=""))
                                                
                                    }                
                
                output <- data.frame(data.table::rbindlist(model_output)) #store all rsults in single data frame
                write_rds(output, file = "data/fig3-binned-model-output-primaryGroupings.rds")
                
                
                
                ##############################################################################################################                            
                ## SECONDARY OUTCOMES
                
                rm(list = ls())
                gc()
                source("scripts/00_init.R")
                
                #load data
                data <- read_rds('data/clean/analysis-zip-day-outcomes-exposures-rates-data-with-covar-final.rds')
                pdata <- panel(data, panel.id = c("zip","date"), duplicate.method = "first") 
                rm(data)
                gc()  
                
                
                
                FE <- "zip + county_month + season_yr + dow"
                
                exp_bin <- "l(regbin1, 0:7) + l(regbin2, 0:7) + l(regbin3, 0:7) + l(regbin4, 0:7)+ l(regbin5, 0:7) + tmx + ppt + dist2fire"
                
                
                #Binned Models
                
                outcomes <- c("ed_resp_asthma_rate","ed_resp_copd_rate","ed_circ_hypertension_rate","ed_circ_ihd_rate","ed_injury_fracture_rate","ed_injury_superftl_rate","ed_symp_respiratory_sob_rate","ed_symp_respiratory_cough_rate","ed_symp_abdomen_rate")
                
                
                #drop outcomes we're not using right now
                drop_col <-   names(pdata)[grep(x = names(pdata), pattern = "_rate")][names(pdata)[grep(x = names(pdata), pattern = "_rate")] %in% outcomes == F]
                pdata <- pdata[,names(pdata) %in% drop_col == F]
                pdata <- panel(pdata, panel.id = c("zip","date"), duplicate.method = "first") 
                
                
                #initialize list for storing output
                model_output <- list(); length(model_output)<- length(outcomes)
                
                
                #loop over outcomes and run regressions
                for(i in 1:length(outcomes)){
                  
                  eqn <-get_eqn(outcomes[i],exp_bin,FE) #function defined in script 00_init.R
                  mod <- feols(eqn, data = pdata, weights = pdata$avePop, cluster = "zip" ) #regression
                  model_output[[i]] <- get_bin_response(mod) #function defined in script 00_init.R
                  model_output[[i]]$outcome <- outcomes[i] #label outcome
                  print(paste("done with ",outcomes[i],sep=""))
                  
                }                
                
                output <- data.frame(data.table::rbindlist(model_output)) #store results in data frame
                write_rds(output, file = "data/fig3-binned-model-output-secondaryGroupings.rds")
                
                
     
                
                
