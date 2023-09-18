source("scripts/00_init.R")

##############################################################################################################              
# Run Regressions
##############################################################################################################                            


          ################
          # Panel A     
          ################
          
          ## Run regressions ##

          #read in data, keep only cols we need, and define polynomial
          pdata <- read_rds('data/analysis-data-private.rds') %>% as.data.frame() %>% 
                dplyr::select(zip, date, season_yr, county_month, dow, tmx, ppt, dist2fire, avePop, date, ed_visit_rate, starts_with("smokePM"))
          pdata <- as.data.table(pdata, ~zip + date)
          
          lgs=0:7
          
          anscols = paste0("smokePM_l",lgs)
          pdata[, (anscols) := data.table::shift(.SD, lgs, fill=NA, "lag"), .SDcols="smokePM", by=zip]
          
          anscols = paste0("smokePM2_l",lgs)
          pdata[, (anscols) := data.table::shift(.SD, lgs, fill=NA, "lag"), .SDcols="smokePM2", by=zip]
          
          anscols = paste0("smokePM3_l",lgs)
          pdata[, (anscols) := data.table::shift(.SD, lgs, fill=NA, "lag"), .SDcols="smokePM3", by=zip]
          
          anscols = paste0("smokePM4_l",lgs)
          pdata[, (anscols) := data.table::shift(.SD, lgs, fill=NA, "lag"), .SDcols="smokePM4", by=zip]
          
          pdata <- pdata %>% drop_na(avePop)
          
      
          
          
          #bootstrap  (SLOW! - just run once)
          
          Nboot = 2000
          lagcoef = matrix(nrow = Nboot, ncol = 4)
          zip = pdata %>% dplyr::select(zip, avePop) %>% distinct()
          options(warn = -1)
          set.seed(94305)
          
          
          for (i in 1:Nboot) {
            
            cl  <- data.frame(zip=sample(zip$zip, size = length(zip$zip),prob =zip$avePop,replace=T))
            dtboot <- inner_join(pdata,cl,by="zip")
            
            fmla <- as.formula(paste("ed_visit_rate ~", paste0("smokePM_l",0:7, collapse = "+"),"+",paste0("smokePM2_l",0:7, collapse = "+"),"+",paste0("smokePM3_l",0:7, collapse = "+"),"+",paste0("smokePM4_l",0:6, collapse = "+") ,"+smokePM4_l7 + tmx + ppt + dist2fire | zip + season_yr + county_month + dow",sep=""))
            mod <- feols(fmla,weights = dtboot$avePop, data=dtboot )
            
            
            
            b <- coef(mod)
            
            lagcoef[i,1] <- sum(b[grep(x = names(b), pattern = "smokePM_")])
            lagcoef[i,2] <- sum(b[grep(x = names(b), pattern = "smokePM2_")])
            lagcoef[i,3] <- sum(b[grep(x = names(b), pattern = "smokePM3_")])
            lagcoef[i,4] <- sum(b[grep(x = names(b), pattern = "smokePM4_")])
            
            print(i)
          }
          write_rds(lagcoef, file = "data/fig2a-polynomial-bootstrap-output_popweighted.rds")
          
          
      ## Plotting Prep ##
          
          xbin = 0:40
          response_coef <- read_rds("data/fig2a-polynomial-bootstrap-output_popweighted.rds")[1:1000,] #take first 1,000 runs
          response_curve_b <- matrix(nrow = nrow(response_coef), ncol = length(xbin))
          response_curve_b <- response_curve_b[!is.na(response_coef[,1]),]
          
          #evaluate at x for all integers 0 through 40 for all bootstrap runs
          for(i in 1:nrow(response_curve_b)){response_curve_b[i,] <- xbin*response_coef[i,1] + xbin^2*response_coef[i,2] + xbin^3*response_coef[i,3] + xbin^4*response_coef[i,4]}
          
          response_curve <- data.frame(x = xbin, est = apply(response_curve_b,2,median), lb = apply(response_curve_b, 2, function(x){quantile(x, .025)}), ub = apply(response_curve_b, 2, function(x){quantile(x, .975)})) #pull out median + 95%CI
          
          response_curve_perchange <- response_curve
          response_curve_perchange[,2:4] <- response_curve_perchange[,2:4]/base_rates$brate[1] #convert rates to % change
          write_rds(response_curve_perchange, file = "data/response_main_poly_percent_change.rds")
          
          
          
          
              
      #########################            
      # Panel B     
      #########################
      
        #Run regression 
          
          #reload analysis data
          pdata <- read_rds('data/analysis-data-private.rds')
          
          
          
          outcomes <- names(pdata)[grep(x = names(pdata), pattern = "_rate")]
          
          #pull out primary diagnoses top-level groups
          outcomes = outcomes[outcomes %in% outcomes[grep(x = outcomes, pattern ="_.*_.*_rate")]==F]
          
          #drop non primary outcomes to save memory
          drop_col <- names(pdata)[grep(x = names(pdata), pattern = "_rate")][names(pdata)[grep(x = names(pdata), pattern = "_rate")] %in% names(pdata)[grep(x = names(pdata), pattern = "_rate")][grep(x = names(pdata)[grep(x = names(pdata), pattern = "_rate")], pattern ="_.*_.*_rate")]==T]
          pdata <- pdata[,names(pdata) %in% drop_col == F]
          
          pdata <- pdata %>% rename(smokePM1 = smokePM) #to help make names distinct before we pull them out
          pdata <- panel(pdata, panel.id = c("zip","date"), duplicate.method = "first") 
          
          
          
          #define model
          FE <- "zip + county_month  + dow + season_yr "
          exp_poly <- "l(smokePM1,0:7) + l(smokePM2,0:7) + l(smokePM3,0:7)  + l(smokePM4,0:7) + tmx + ppt + dist2fire"  
          xbin <- 0:40
          
        
          #run model for each outcome      
          model_output <- matrix(nrow = length(outcomes), ncol = 4)
          rownames(model_output) <- outcomes
          colnames(model_output)<-paste("deg",1:4, sep = "")
          
          for(i in 1:length(outcomes)){
            
            eqn <-get_eqn(outcomes[i],exp_poly,FE)
            mod <- feols(eqn, data = pdata, weights = pdata$avePop, cluster = "zip" )
            model_output[i,] <- get_poly_coef_cumulative(mod, 4)
            print(paste("done with ",outcomes[i],sep=""))
            
          }                
          
          write_rds(model_output, file = "data/fig2b-polynomial-model-output-by-diagnosis.rds")
          
        
      #prep data  
          resp_coef <- read_rds("data/fig2b-polynomial-model-output-by-diagnosis.rds") %>% as.data.frame()
          brates <- read_rds("data/baserates.rds")
          
          resp_coef <- data.frame(outcome = rownames(resp_coef), resp_coef)
          rownames(resp_coef) <- 1:nrow(resp_coef)
          resp_coef <- left_join(resp_coef, brates) %>% arrange(-brate)
          resp_coef <- resp_coef[c(1:8,10:9,11,13:12,14),] #rounding issues with sorting
          
          resp_coef$color <- c(MetBrewer::met.brewer("Signac",14)[14:1])    
          
          resp <- data.frame(outcome = resp_coef$outcome)
          
          delta <- 1/80
          x <- seq(0, 40, delta)
          
          
          resp[,2:(length(x)+1)]<-NA
          for(i in 1:nrow(resp)){
            resp[i,2:ncol(resp)] <- x*resp_coef$deg1[i] + x^2*resp_coef$deg2[i] + x^3*resp_coef$deg3[i] + x^4*resp_coef$deg4[i]
          }
          
          
          


##############################################################################################################              
# Plot
##############################################################################################################                            

          
    #read in data on smoke PM distribution to be plotted as histogram below response curve
          
          
          smoke <- read_rds("data/clean/smokePMdistribution.rds")
          smoke <- smoke[smoke>0] #want to plot smoke distribution | smoke present
          smoke[smoke>40]<-40 #topcode at 40
          hist(smoke, breaks = 40)
          hst_obj <- get_hst_obj(smoke, 0, 40, 1)
          hst_obj$count[hst_obj$count<2000]<-2000
          
          xbin <- 0:40
          
      
    

     
        
        
    
    pdf(file = "figures/1-raw/fig2-raw.pdf", width = 12, height = 5)
    
    par(mfrow = c(1,2))
    par(mar = c(4,5,1,1))
    
    
        ### Panel A ###
          
          plot(xbin, response_curve$est,
               xlim = c(0,40),ylim= c(-6.15, 2), type = "l", col= NA, axes = F, 
               xlab = "Smoke PM2.5",ylab = "Change in ED visits per 100K", cex.lab = 1.5)
          
          abline(h =0, col ='red', lwd = .25,lty=1)
          
          polygon(x = c(xbin[1], xbin, rev(xbin)), y = c(response_curve$lb[1],response_curve$ub, rev(response_curve$lb)), border = NA, 
                  col =add.alpha('royalblue', .5))# 
          lines(xbin, response_curve$est, lwd=3, col = 'navy')
          
          
          
          axis(1, tick = T, at = seq(0,40,5), cex.axis = 1.5)
          axis(2, las = 2, tick = T, at = seq(-6, 2, 1), cex.axis = 1.5)
          
          plotHist(hst_obj, col = 'gray70',0.5, bottom = -6.25, height = 2.5, norm  = max(hst_obj), border.col = 'white')
    
    
          
          
          
         ### Panel B ###
          
          
          
          plot(0:40, resp[1,2:42],  xlim = c(0, 40), ylim = c(-6.15,2),xlab = "",ylab = "", axes = F, col =NA,type = "l",lwd=3)
          
          for(i in 1:length(x)){
            #positive
            
            subdat <- resp[,c(1, i + 1)]   
            subdat$color <- resp_coef$color[1:nrow(resp_coef)]
            
            pos <- subdat[subdat[,2]>=0,]
            neg <- subdat[subdat[,2]<0,]
            
            
            if(nrow(pos)>0){rect(xleft = x[i] - delta/2 , xright =x[i] + delta/2, ybottom = c(0, cumsum((unlist(pos[1:(nrow(pos)-1),2]) ))), ytop =   c(cumsum((unlist(pos[,2]) ))), col = (pos$color), border = NA)}
            if(nrow(neg)>0){rect(xleft = x[i] - delta/2, xright = x[i] + delta/2, ybottom = c(0, cumsum((unlist(neg[1:(nrow(neg)-1),2]) ))), ytop =   c(cumsum((unlist(neg[,2]) ))), col = (neg$color), border = NA)}
            
            
          }  
          
          
          
          
          abline(h = 0, col ='red', lwd =2)
          lines(xbin, response_curve$est, lwd=3, col = 'black')
          
          
          axis(2, tick = T, las = 2, at = seq(-6, 2, 1), cex.axis = 1.5)
          axis(1, tick = T, at = seq(0, 40, 5), cex.axis = 1.5)
          
          mtext(side = 1, text = "Smoke PM2.5",line=3, cex = 2)
          
          
          plotHist(hst_obj, col = 'gray70',0.5, bottom = -6.25, height = 2.5, norm  = max(hst_obj), border.col = 'white')
          
          
          
    dev.off()            
    

