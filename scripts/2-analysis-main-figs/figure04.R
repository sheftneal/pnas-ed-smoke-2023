source("scripts/00_init.R")

pdata <- read_rds('data/analysis-data-private.rds')
pdata <- panel(pdata, panel.id = c("zip","date"), duplicate.method = "first") 

       
    #define equation 
        bins <- "l(regbin1, 0:7) + l(regbin2, 0:7) + l(regbin3, 0:7) + l(regbin4, 0:7)+ l(regbin5, 0:7) + tmx + ppt+ dist2fire "
        
        bin_int <- paste(bins, "+ (",bins,"):low_group + (", bins,"):high_group",sep="")
        
        FE <- "zip + county_month + season_yr + dow"
        
        
        
    #Asthma    
        eqn1 <-get_eqn("ed_resp_asthma_rate",bins,FE)
        
        
        var = "health_insurance"; i = 1
        pdata[,"low_group"] <- as.numeric(pdata[,var[i]] < quantile(pdata[,var[i]], .33, na.rm = T)) #greater than 50th percnetile
        pdata[,"high_group"] <- as.numeric(pdata[,var[i]] > quantile(pdata[,var[i]], .66, na.rm = T)) #greater than 50th percnetile

      
      mod1a <- feols(eqn1, data = pdata[pdata$low_group == 1,], weights = pdata$avePop[pdata$low_group == 1], cluster = "zip" )
      mod1b <- feols(eqn1, data = pdata[pdata$low_group == 0 & pdata$high_group == 0,], weights = pdata$avePop[pdata$low_group == 0 & pdata$high_group == 0], cluster = "zip" )
      mod1c <- feols(eqn1, data = pdata[pdata$high_group == 1,], weights = pdata$avePop[pdata$high_group == 1], cluster = "zip" )
      

        
        low <- data.frame(type = "low", 
                          rbind(
                            get_bin_response_cumulative(mod1a, "regbin1",6,"zip"),
                            get_bin_response_cumulative(mod1a, "regbin2",6,"zip"),
                            get_bin_response_cumulative(mod1a, "regbin3",6,"zip"),
                            get_bin_response_cumulative(mod1a, "regbin4",6,"zip"),
                            get_bin_response_cumulative(mod1a, "regbin5",6,"zip")))
        
        med <- data.frame(type = "med", 
                          rbind(
                            get_bin_response_cumulative(mod1b, "regbin1",6,"zip"),
                            get_bin_response_cumulative(mod1b, "regbin2",6,"zip"),
                            get_bin_response_cumulative(mod1b, "regbin3",6,"zip"),
                            get_bin_response_cumulative(mod1b, "regbin4",6,"zip"),
                            get_bin_response_cumulative(mod1b, "regbin5",6,"zip")))
        
        high <- data.frame(type = "high", 
                           rbind(
                             get_bin_response_cumulative(mod1c, "regbin1",6,"zip"),
                             get_bin_response_cumulative(mod1c, "regbin2",6,"zip"),
                             get_bin_response_cumulative(mod1c, "regbin3",6,"zip"),
                             get_bin_response_cumulative(mod1c, "regbin4",6,"zip"),
                             get_bin_response_cumulative(mod1c, "regbin5",6,"zip")))


      res1 <- rbind(low, med, high)

      rm(mod1a, mod1b, mod1c); gc()


      # Symptoms
          eqn2 <-get_eqn("ed_symp_rate",bins,FE)
                
          mod2a <- feols(eqn2, data = pdata[pdata$low_group == 1,], weights = pdata$avePop[pdata$low_group == 1], cluster = "zip" )
          mod2b <- feols(eqn2, data = pdata[pdata$low_group == 0 & pdata$high_group == 0,], weights = pdata$avePop[pdata$low_group == 0 & pdata$high_group == 0], cluster = "zip" )
          dat2c <- pdata[pdata$high_group == 1,]
          mod2c <- feols(eqn2, data = dat2c, weights = dat2c$avePop, cluster = "zip" )


        
        low <- data.frame(type = "low", 
                          rbind(
                            get_bin_response_cumulative(mod2a, "regbin1",6,"zip"),
                            get_bin_response_cumulative(mod2a, "regbin2",6,"zip"),
                            get_bin_response_cumulative(mod2a, "regbin3",6,"zip"),
                            get_bin_response_cumulative(mod2a, "regbin4",6,"zip"),
                            get_bin_response_cumulative(mod2a, "regbin5",6,"zip")))
        
        med <- data.frame(type = "med", 
                          rbind(
                            get_bin_response_cumulative(mod2b, "regbin1",6,"zip"),
                            get_bin_response_cumulative(mod2b, "regbin2",6,"zip"),
                            get_bin_response_cumulative(mod2b, "regbin3",6,"zip"),
                            get_bin_response_cumulative(mod2b, "regbin4",6,"zip"),
                            get_bin_response_cumulative(mod2b, "regbin5",6,"zip")))
        
        high <- data.frame(type = "high", 
                           rbind(
                             get_bin_response_cumulative(mod2c, "regbin1",6,"zip"),
                             get_bin_response_cumulative(mod2c, "regbin2",6,"zip"),
                             get_bin_response_cumulative(mod2c, "regbin3",6,"zip"),
                             get_bin_response_cumulative(mod2c, "regbin4",6,"zip"),
                             get_bin_response_cumulative(mod2c, "regbin5",6,"zip")))


        res2 <- rbind(low, med, high)
        
        rm(mod2a, mod2b, mod2c); gc()













####### PLOT


col = wes_palette("BottleRocket2")[c(2,1,3)]



        
        results = res1
        
        results$lb <- results$est -2*results$se
        results$ub <- results$est +2*results$se
        
        #divide through by group specific base rates (ie by base rate for given level of insurance)
        results[1:5,c("est","lb","ub")] <- results[1:5,c("est","lb","ub")]/brate$ed_symp_rate[3]
        results[6:10,c("est","lb","ub")] <- results[6:10,c("est","lb","ub")]/brate$ed_symp_rate[2]
        results[11:15,c("est","lb","ub")] <- results[11:15,c("est","lb","ub")]/brate$ed_symp_rate[1]
        



            
            pdf(file = "figures/raw/fig4-symptoms-smallpanels-raw.pdf", width = 5, height = 10)
                    
                    
                    par(mfrow = c(3,1))
                    
                    
                    par(mar = c(4,5,6,4))
                    par(oma =c(0,0,0,0))
                    
                    xlim <- c(0, 50)
                    ylim <- c(-.3,.3)#c(-10,10)
                    yaxis_at <- seq(-.5, .5, .25)
                    yaxis_labels <- yaxis_at*100
                    xaxis_at <- seq(0, 50, 10)
                    alpha_ci <- 0.7
                    
                    
                    
                    
                    ##Health Insurance
                    
                    reponse_curve <- results %>% filter(type == "low")
                    
                    plot(xbin, reponse_curve$est,
                         xlim = xlim,ylim= ylim, type = "l", col= NA, axes = F, 
                         xlab = "",ylab = "% change visits", cex.lab = 2)
                    
                    abline(h =0, col ='red', lwd = .5,lty=1)
                    
                    polygon(x = c(xbin[1], xbin, rev(xbin)), y = c(reponse_curve$lb[1],reponse_curve$ub, rev(reponse_curve$lb)), border = NA, 
                            col =add.alpha(col[1], alpha_ci))# 
                    lines(xbin, reponse_curve$est, lwd=3)
                    axis(2, las =2, cex.axis = 1.5, at = seq(-.2, .2, .1), labels = seq(-20,20,10))          
                    
                    
                    reponse_curve <- results %>% filter( type == "med")
                    
                    plot(xbin, reponse_curve$est,
                         xlim = xlim,ylim= ylim, type = "l", col= NA, axes = F, 
                         xlab = "",ylab = "% change visits", cex.lab = 2)
                    
                    abline(h =0, col ='red', lwd = .5,lty=1)
                    
                    polygon(x = c(xbin[1], xbin, rev(xbin)), y = c(reponse_curve$lb[1],reponse_curve$ub, rev(reponse_curve$lb)), border = NA, 
                            col =add.alpha(col[2], alpha_ci))# 
                    lines(xbin, reponse_curve$est, lwd=3)
                    axis(2, las =2, cex.axis = 1.5, at = seq(-.3, .3, .1), labels = seq(-20,20,10))          
                    
                    
                    reponse_curve <- results %>% filter( type == "high")
                    
                    plot(xbin, reponse_curve$est,
                         xlim = xlim,ylim= ylim, type = "l", col= NA, axes = F, 
                         xlab = "",ylab = "% change visits", cex.lab = 2)
                    
                    abline(h =0, col ='red', lwd = .5,lty=1)
                    
                    polygon(x = c(xbin[1], xbin, rev(xbin)), y = c(reponse_curve$lb[1],reponse_curve$ub, rev(reponse_curve$lb)), border = NA, 
                            col =add.alpha(col[3], alpha_ci))# 
                    lines(xbin, reponse_curve$est, lwd=3)
                    
                    
                    axis(2, las =2, cex.axis = 1.5, at = seq(-.3, .3, .1), labels = seq(-20,20,10))          
                    axis(1, tick = T, cex.axis=1.5)
                    
            dev.off()
            
            
            
            results = res2
            
            results$lb <- results$est -2*results$se
            results$ub <- results$est +2*results$se
            
            #divide through by group specific base rates (ie by base rate for given level of insurance)
            results[1:5,c("est","lb","ub")] <- results[1:5,c("est","lb","ub")]/brate$ed_asthma_rate[3]
            results[6:10,c("est","lb","ub")] <- results[6:10,c("est","lb","ub")]/brate$ed_asthma_rate[2]
            results[11:15,c("est","lb","ub")] <- results[11:15,c("est","lb","ub")]/brate$ed_asthma_rate[1]
            
            

        #asthma
        pdf(file = "figures/raw/fig4-asthma-smallpanels-raw.pdf", width = 5, height = 10)
                
                
                par(mfrow = c(3,1))
                
                
                par(mar = c(4,5,6,4))
                par(oma =c(0,0,0,0))
                
                xlim <- c(0, 50)
                ylim <- c(-2,2)#c(-10,10)
                yaxis_at <- seq(-2, 2, .5)
                yaxis_labels <- yaxis_at*100
                xaxis_at <- seq(0, 50, 10)
                alpha_ci <- 0.7
                
                
                
                ##Health Insurance
                
                reponse_curve <- results %>% filter(form == 1 & type == "low")
                
                plot(xbin, reponse_curve$est,
                     xlim = xlim,ylim= ylim, type = "l", col= NA, axes = F, 
                     xlab = "",ylab = "% change visits", cex.lab = 2)
                
                abline(h =0, col ='red', lwd = .5,lty=1)
                
                polygon(x = c(xbin[1], xbin, rev(xbin)), y = c(reponse_curve$lb[1],reponse_curve$ub, rev(reponse_curve$lb)), border = NA, 
                        col =add.alpha(col[1], alpha_ci))# 
                lines(xbin, reponse_curve$est, lwd=3)
                axis(2, las =2, cex.axis = 1.5, at = seq(-2, 2, .5), labels = seq(-200,200,50))          
                
                
                reponse_curve <- results %>% filter(form == 1 & type == "med")
                
                plot(xbin, reponse_curve$est,
                     xlim = xlim,ylim= ylim, type = "l", col= NA, axes = F, 
                     xlab = "",ylab = "% change visits", cex.lab = 2)
                
                abline(h =0, col ='red', lwd = .5,lty=1)
                
                polygon(x = c(xbin[1], xbin, rev(xbin)), y = c(reponse_curve$lb[1],reponse_curve$ub, rev(reponse_curve$lb)), border = NA, 
                        col =add.alpha(col[2], alpha_ci))# 
                lines(xbin, reponse_curve$est, lwd=3)
                axis(2, las =2, cex.axis = 1.5, at = seq(-2, 2, .5), labels = seq(-200,200,50))          
                
                reponse_curve <- results %>% filter(form == 1 & type == "high")
                
                plot(xbin, reponse_curve$est,
                     xlim = xlim,ylim= ylim, type = "l", col= NA, axes = F, 
                     xlab = "",ylab = "% change visits", cex.lab = 2)
                
                abline(h =0, col ='red', lwd = .5,lty=1)
                
                polygon(x = c(xbin[1], xbin, rev(xbin)), y = c(reponse_curve$lb[1],reponse_curve$ub, rev(reponse_curve$lb)), border = NA, 
                        col =add.alpha(col[3], alpha_ci))# 
                lines(xbin, reponse_curve$est, lwd=3)
                
                
                
                
                axis(2, las =2, cex.axis = 1.5, at = seq(-2, 2, .5), labels = seq(-200,200,50))          
                axis(1, tick = T, cex.axis=1.5)
                
        
        
        dev.off()
        
        
        
        
        
        
        
        
        
        
