source("scripts/00_init.R")




##############################################################################################################              
# Run Regressions
##############################################################################################################                            
        
        
      # PRIMARY GROUPINGS #

              #load data
              data <- read_rds('data/analysis-data-private.rds')
              pdata <- panel(data, panel.id = c("zip","date"), duplicate.method = "first") 
              rm(data)
              gc()  
      
      
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
              write_rds(output, file = "data/binned-model-output-primaryGroupings.rds")

        # SECONDARY GROUPINGS #
        
                  #load data
                  data <- read_rds('data/analysis-data-private.rds')
                  pdata <- panel(data, panel.id = c("zip","date"), duplicate.method = "first") 
                  rm(data)
                  gc()  
                
                      FE <- "zip + county_month + season_yr + dow"
                      
                      exp_bin <- "l(regbin1, 0:7) + l(regbin2, 0:7) + l(regbin3, 0:7) + l(regbin4, 0:7)+ l(regbin5, 0:7) + tmx + ppt + dist2fire"
                
                      outcomes <- c("ed_resp_asthma_rate","ed_resp_copd_rate","ed_circ_hypertension_rate","ed_circ_ihd_rate","ed_injury_fracture_rate","ed_injury_superftl_rate","ed_symp_respiratory_cough_rate","ed_symp_abdomen_rate")
                
          
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
                write_rds(output, file = "data/binned-model-output-secondaryGroupings.rds")
                


##############################################################################################################              
# Plot
##############################################################################################################                            

        ####################            
        ### Large Panels ###            
        ####################            
        
              
          results <- read_rds("binned-model-output-primaryGroupings.rds") 
          brates <- read_rds("data/baserates.rds")
          
          alpha = 0.05
          results <- left_join(results, brates) %>% dplyr::select(-lb, -ub) %>% 
            mutate(
              ub = est + qnorm(1-alpha/14)*se,
              lb = est + qnorm(alpha/14)*se,
              est = est/brate,
              lb = lb/brate,
              ub = ub/brate
            )


          xbin <- c(0, 7, 15, 34, 50)
          


        
        
        pdf(file = "figures/raw/fig3-big-panels-raw.pdf", width = 16, height = 16)
        
        par(mfrow = c(2,2))
        par(mar = c(4,5,3,4))
        par(oma =c(2,2,2,0))
      
                  
                  
                  #Respiratory - all
                  reponse_curve <- results %>% filter(outcome == "ed_resp_rate")
                  
                  plot(xbin, reponse_curve$est,
                       xlim = c(0,50),ylim= c(-.3, .3), type = "l", col= NA, axes = F, 
                       xlab = "",ylab = "Percent Change", cex.lab=2)
                  
                  abline(h =0, col ='red', lwd = .5,lty=1)
                  
                  polygon(x = c(xbin[1], xbin, rev(xbin)), y = c(reponse_curve$lb[1],reponse_curve$ub, rev(reponse_curve$lb)), border = NA, 
                          col =add.alpha(MetBrewer::met.brewer("Signac",14)[12]   , .75))
                  lines(xbin, reponse_curve$est, lwd=3)
                  
                  axis(1, tick = T, at = seq(0,50,10), cex.axis = 1.5)
                  axis(2, las = 2, tick = T, at = seq(-.3, .3, .1),labels = seq(-30, 30, 10), cex.axis = 1.5)
                  
                  mtext(side = 3, text = "Respiratory",adj = 0, cex = 1.75,line=1)
                  
                  
                  
                  
                  #Circulatory
                  reponse_curve <- results %>% filter(outcome == "ed_circ_rate")
                  
                  plot(xbin, reponse_curve$est,
                       xlim = c(0,50),ylim= c(-.3, .3), type = "l", col= NA, axes = F, 
                       xlab = "",ylab = "", cex.lab = 2)
                  
                  abline(h =0, col ='red', lwd = .5,lty=1)
                  
                  polygon(x = c(xbin[1], xbin, rev(xbin)), y = c(reponse_curve$lb[1],reponse_curve$ub, rev(reponse_curve$lb)), border = NA, 
                          col =add.alpha(MetBrewer::met.brewer("Signac",14)[2]   , .75))# 
                  lines(xbin, reponse_curve$est, lwd=3)
                  
                  axis(1, tick = T, at = seq(0,50,10), cex.axis = 1.5)
                  axis(2, las = 2, tick = T, at = seq(-.3, .3, .1),labels = seq(-30, 30, 10), cex.axis = 1.5)
                  
                  mtext(side = 3, text = "Circulatory System",adj = 0, cex = 1.75,line=1)
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  #Symptoms
                  reponse_curve <- results %>% filter(outcome == "ed_symp_rate")
                  
                  plot(xbin, reponse_curve$est,
                       xlim = c(0,50),ylim= c(-.3, .3), type = "l", col= NA, axes = F, 
                       xlab = "",ylab = "Percent Change", cex.lab = 2)
                  
                  abline(h =0, col ='red', lwd = .5,lty=1)
                  
                  polygon(x = c(xbin[1], xbin, rev(xbin)), y = c(reponse_curve$lb[1],reponse_curve$ub, rev(reponse_curve$lb)), border = NA, 
                          col =add.alpha(MetBrewer::met.brewer("Signac",14)[14]   , .75))
                  lines(xbin, reponse_curve$est, lwd=3)
                  
                  axis(1, tick = T, at = seq(0,50,10), cex.axis = 1.5)
                  axis(2, las = 2, tick = T, at = seq(-.3, .3, .1),labels = seq(-30, 30, 10), cex.axis = 1.5)
                  
                  mtext(side = 3, text = "Symptoms",adj = 0, cex = 1.75,line=1)
                  
                  
                
                  
                  
                  #All injuries
                  reponse_curve <- results %>% filter(outcome == "ed_injury_rate")
                  
                  plot(xbin, reponse_curve$est,
                       xlim = c(0,50),ylim= c(-.3, .3), type = "l", col= NA, axes = F, 
                       xlab = "",ylab = "", cex.lab = 2)
                  
                  abline(h =0, col ='red', lwd = .5,lty=1)
                  
                  polygon(x = c(xbin[1], xbin, rev(xbin)), y = c(reponse_curve$lb[1],reponse_curve$ub, rev(reponse_curve$lb)), border = NA, 
                          col =add.alpha(MetBrewer::met.brewer("Signac",14)[13]   , .75))
                  lines(xbin, reponse_curve$est, lwd=3)
                  
                  axis(1, tick = T, at = seq(0,50,10), cex.axis = 1.5)
                  axis(2, las = 2, tick = T, at = seq(-.3, .3, .1),labels = seq(-30, 30, 10), cex.axis = 1.5)
                  
                  mtext(side = 3, text = "Accidental Injuries",adj = 0, cex = 1.75,line=1)
                  
                  
                  
        
        
        
                dev.off()



                
                
                
                
                
                

                
    ####################            
    ### Small Panels ###            
    ####################            
                
                
                results <- read_rds("data/clean/data/binned-model-output-secondaryGroupings.rds") 
                brates <- read_rds("data/clean/baserates.rds")
                alpha = 0.05
                results <- left_join(results, brates) %>% dplyr::select(-lb, -ub) %>% 
                  mutate(
                    ub = est + qnorm(1-alpha/8)*se,
                    lb = est + qnorm(alpha/8)*se,
                    est = est/brate,
                    lb = lb/brate,
                    ub = ub/brate
                  )
                
                
                xbin <- c(0, 7, 15, 34, 50)
                
                
                
                pdf(file = "figures/raw/fig3-small-panels-raw.pdf", width = 3, height = 16)
                
                par(mfrow = c(8,1))
                par(mar = c(4,5,3,4))
                
                
                
                #Respiratory 
                         
                          #asthma
                          reponse_curve <- results %>% filter(outcome == "ed_resp_asthma_rate")
                          
                          plot(xbin, reponse_curve$est,
                               xlim = c(0,50),ylim= c(-.5, .5), type = "l", col= NA, axes = F, 
                               xlab = "",ylab = "Percent change", cex.lab = 2)
                          
                          abline(h =0, col ='red', lwd = .5,lty=1)
                          
                          polygon(x = c(xbin[1], xbin, rev(xbin)), y = c(reponse_curve$lb[1],reponse_curve$ub, rev(reponse_curve$lb)), border = NA, 
                                  col =add.alpha(MetBrewer::met.brewer("Signac",14)[12]   , .75))# 
                          lines(xbin, reponse_curve$est, lwd=3)
                          
                          axis(1, tick = T, at = seq(0,50,10), cex.axis = 1.5)
                          axis(2, las = 2, tick = T, at = seq(-.5, .5, .25),labels = seq(-50, 50, 25), cex.axis = 1.5)
                          
                          mtext(side = 3, text = "Asthma",adj = 0, cex = 1.75,line=1)
                          
                          
                          
                          #copd
                          reponse_curve <- results %>% filter(outcome == "ed_resp_copd_rate")
                          
                          plot(xbin, reponse_curve$est,
                               xlim = c(0,50),ylim= c(-.5, .5), type = "l", col= NA, axes = F, 
                               xlab = "",ylab = "Percent change", cex.lab = 2)
                          
                          abline(h =0, col ='red', lwd = .5,lty=1)
                          
                          polygon(x = c(xbin[1], xbin, rev(xbin)), y = c(reponse_curve$lb[1],reponse_curve$ub, rev(reponse_curve$lb)), border = NA, 
                                  col =add.alpha(MetBrewer::met.brewer("Signac",14)[12]   , .75))
                          lines(xbin, reponse_curve$est, lwd=3)
                          
                          axis(1, tick = T, at = seq(0,50,10), cex.axis = 1.5)
                          axis(2, las = 2, tick = T, at = seq(-.5, .5, .25),labels = seq(-50, 50, 25), cex.axis = 1.5)
                          
                          mtext(side = 3, text = "COPD",adj = 0, cex = 1.75,line=1)
                          
                          
                          
                          
                          
                
                #Circulatory
                             
                          #Hypertension       
                          reponse_curve <- results %>% filter(outcome == "ed_circ_hypertension_rate")
                          
                          plot(xbin, reponse_curve$est,
                               xlim = c(0,50),ylim= c(-.5, .5), type = "l", col= NA, axes = F, 
                               xlab = "",ylab = "Percent Change", cex.lab = 2)
                          
                          abline(h =0, col ='red', lwd = .5,lty=1)
                          
                          polygon(x = c(xbin[1], xbin, rev(xbin)), y = c(reponse_curve$lb[1],reponse_curve$ub, rev(reponse_curve$lb)), border = NA, 
                                  col =add.alpha(MetBrewer::met.brewer("Signac",14)[2]   , .75))
                          lines(xbin, reponse_curve$est, lwd=3)
                          
                          axis(1, tick = T, at = seq(0,50,10), cex.axis = 1.5)
                          axis(2, las = 2, tick = T, at = seq(-.5, .5, .25),labels = seq(-50, 50, 25), cex.axis = 1.5)
                          
                          mtext(side = 3, text = "Hypertension",adj = 0, cex = 1.75,line=1)
                          
                          
                          #IHD
                          reponse_curve <- results %>% filter(outcome == "ed_circ_ihd_rate")
                          
                          plot(xbin, reponse_curve$est,
                               xlim = c(0,50),ylim= c(-.5, .5), type = "l", col= NA, axes = F, 
                               xlab = "",ylab = "Percent Change", cex.lab = 2)
                          
                          abline(h =0, col ='red', lwd = .5,lty=1)
                          
                          polygon(x = c(xbin[1], xbin, rev(xbin)), y = c(reponse_curve$lb[1],reponse_curve$ub, rev(reponse_curve$lb)), border = NA, 
                                  col =add.alpha(MetBrewer::met.brewer("Signac",14)[2]   , .75))
                          lines(xbin, reponse_curve$est, lwd=3)
                          
                          axis(1, tick = T, at = seq(0,50,10), cex.axis = 1.5)
                          axis(2, las = 2, tick = T, at = seq(-.5, .5, .25),labels = seq(-50, 50, 25), cex.axis = 1.5)
                          
                          mtext(side = 3, text = "IHD",adj = 0, cex = 1.75,line=1)
                          
                          
                          
                
                
                
                
                
                #All injuries

                          
                      reponse_curve <- results %>% filter(outcome == "ed_injury_fracture_rate")
      
                      plot(xbin, reponse_curve$est,
                           xlim = c(0,50),ylim= c(-.5, .5), type = "l", col= NA, axes = F,
                           xlab = "",ylab = "Percent Change", cex.lab = 2)
      
                      abline(h =0, col ='red', lwd = .5,lty=1)
      
                      polygon(x = c(xbin[1], xbin, rev(xbin)), y = c(reponse_curve$lb[1],reponse_curve$ub, rev(reponse_curve$lb)), border = NA,
                              col =add.alpha(MetBrewer::met.brewer("Signac",14)[13]   , .75))
                      lines(xbin, reponse_curve$est, lwd=3)
      
                      axis(1, tick = T, at = seq(0,50,10), cex.axis = 1.5)
                      axis(2, las = 2, tick = T, at = seq(-.5, .5, .25),labels = seq(-50, 50, 25), cex.axis = 1.5)
      
                      mtext(side = 3, text = "Fractures",adj = 0, cex = 1.75,line=1)
      
      
                      
                      
                      
                      
                      reponse_curve <- results %>% filter(outcome == "ed_injury_superftl_rate")
                      
                      plot(xbin, reponse_curve$est,
                           xlim = c(0,50),ylim= c(-.5, .5), type = "l", col= NA, axes = F,
                           xlab = "",ylab = "Percent Change", cex.lab = 2)
                      
                      abline(h =0, col ='red', lwd = .5,lty=1)
                      
                      polygon(x = c(xbin[1], xbin, rev(xbin)), y = c(reponse_curve$lb[1],reponse_curve$ub, rev(reponse_curve$lb)), border = NA,
                              col =add.alpha(MetBrewer::met.brewer("Signac",14)[13]   , .75))
                      lines(xbin, reponse_curve$est, lwd=3)
                      
                      axis(1, tick = T, at = seq(0,50,10), cex.axis = 1.5)
                      axis(2, las = 2, tick = T, at = seq(-.5, .5, .25),labels = seq(-50, 50, 25), cex.axis = 1.5)
                      
                      mtext(side = 3, text = "Superficial Injuries",adj = 0, cex = 1.75,line=1)
                      
                      
                      
                
                
                #Symptoms
                          #Respiratory
                      
                          reponse_curve <- results %>% filter(outcome == "ed_symp_respiratory_cough_rate")
                          
                          plot(xbin, reponse_curve$est,
                               xlim = c(0,50),ylim= c(-.5, .5), type = "l", col= NA, axes = F, 
                               xlab = "",ylab = "", cex.lab = 2)
                          
                          abline(h =0, col ='red', lwd = .5,lty=1)
                          
                          polygon(x = c(xbin[1], xbin, rev(xbin)), y = c(reponse_curve$lb[1],reponse_curve$ub, rev(reponse_curve$lb)), border = NA, 
                                  col =add.alpha(MetBrewer::met.brewer("Signac",14)[14]   , .75))
                          lines(xbin, reponse_curve$est, lwd=3)
                          
                          axis(1, tick = T, at = seq(0,50,10), cex.axis = 1.5)
                          axis(2, las = 2, tick = T, at = seq(-.5, .5, .25),labels = seq(-50, 50, 25), cex.axis = 1.5)
                          
                          mtext(side = 3, text = "Cough",adj = 0, cex = 1.75,line=1)
                          
                          
                          
                          reponse_curve <- results %>% filter(outcome == "ed_symp_abdomen_rate")
                          
                          plot(xbin, reponse_curve$est,
                               xlim = c(0,50),ylim= c(-.5, .5), type = "l", col= NA, axes = F, 
                               xlab = "",ylab = "", cex.lab = 2)
                          
                          abline(h =0, col ='red', lwd = .5,lty=1)
                          
                          polygon(x = c(xbin[1], xbin, rev(xbin)), y = c(reponse_curve$lb[1],reponse_curve$ub, rev(reponse_curve$lb)), border = NA, 
                                  col =add.alpha(MetBrewer::met.brewer("Signac",14)[14]   , .75))
                          lines(xbin, reponse_curve$est, lwd=3)
                          
                          axis(1, tick = T, at = seq(0,50,10), cex.axis = 1.5)
                          axis(2, las = 2, tick = T, at = seq(-.5, .5, .25),labels = seq(-50, 50, 25), cex.axis = 1.5)
                          
                          mtext(side = 3, text = "Abdominal Pain",adj = 0, cex = 1.75,line=1)
                          
                          
                
                
                
                dev.off()
                
                
                
                
                
                
                
                
                
                
