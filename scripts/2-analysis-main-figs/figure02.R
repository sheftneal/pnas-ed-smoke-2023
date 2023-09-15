source("scripts/00_init.R")


##############################################################################################################              
# Run Regressions
##############################################################################################################                            


          ################
          # Panel A     
          ################
          
          
          #read in data, keep only cols we need, and define polynomial
          pdata <- read_rds('data/clean/analysis-zip-day-outcomes-exposures-rates-data-with-covar-final.rds')
          
          #make sure weird zips get dropped
          check = pdata %>% as.data.frame() %>% group_by(zip) %>% summarise(sc = sum(smokePM>0 | !is.na(tmx), na.rm = T) )
          dropzip = filter(check, sc==0) %>% dplyr::select(zip) %>% unlist()
          
          
          
          pdata <- as.data.frame(pdata)
          pdata <- pdata %>% filter(zip %in% dropzip == F)
          
          pdata$season <- as.numeric(pdata$month %in% 6:10)
          pdata$season_yr <- paste(pdata$season, pdata$year,sep="-")
          pdata <-  pdata %>% as.data.frame() %>% 
            dplyr::select(zip, date, season_yr, county_month, dow, tmx, ppt, dist2fire, avePop, date, ed_visit_rate, starts_with("smokePM"))
          #define lags
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
          
          
          
          tmxppt_replace = pdata %>%as.data.frame() %>%  filter(zip %in% c(93013, 93452,94937,95567,95558)) %>% dplyr::select(zip, date, tmx, ppt) %>% distinct()
          tmxppt_replace <- tmxppt_replace %>% 
            mutate(
              zip = replace(zip, zip == 93013, 93001),
              zip = replace(zip, zip == 93452, 93920),
              zip = replace(zip, zip == 94937, 94956),
              zip = replace(zip, zip == 95567, 95531),
              zip = replace(zip, zip == 95558, 95589)
            ) %>% 
            rename(tmx_replace = tmx, ppt_replace = ppt)
          
          
          pdata = left_join(pdata, tmxppt_replace)
          pdata$tmx[!is.na(pdata$tmx_replace)] <-pdata$tmx_replace[!is.na(pdata$tmx_replace)]
          pdata$ppt[!is.na(pdata$ppt_replace)] <-pdata$ppt_replace[!is.na(pdata$ppt_replace)]
          pdata = pdata %>% dplyr::select(-ends_with("replace"))
          
          
          
          
          
          
          
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
          write_rds(lagcoef, file = "data/clean/fig2-polynomial-bootstrap-output_popweighted-032323.rds")
          
          
          
              
              #########################            
              # Panel B     
              #########################
          
          
          rm(list =ls())
          gc()
          source("scripts/00_init.R")
          pdata <- read_rds('data/clean/analysis-zip-day-outcomes-exposures-rates-data-with-covar-final.rds')
          
          
          
          outcomes <- names(pdata)[grep(x = names(pdata), pattern = "_rate")]
          
          #pull out primary diagnoses top-level groups
          outcomes = outcomes[outcomes %in% outcomes[grep(x = outcomes, pattern ="_.*_.*_rate")]==F]
          
          drop_col <- names(pdata)[grep(x = names(pdata), pattern = "_rate")][names(pdata)[grep(x = names(pdata), pattern = "_rate")] %in% names(pdata)[grep(x = names(pdata), pattern = "_rate")][grep(x = names(pdata)[grep(x = names(pdata), pattern = "_rate")], pattern ="_.*_.*_rate")]==T]
          pdata <- pdata[,names(pdata) %in% drop_col == F]
          
          pdata$season <- as.numeric(pdata$month %in% 6:10)
          pdata$season_yr <- paste(pdata$season, pdata$year,sep="-")
          pdata <- pdata %>% rename(smokePM1 = smokePM) #to help make names distinct before we pull them out
          pdata <- panel(pdata, panel.id = c("zip","date"), duplicate.method = "first") 
          
          
          
          # pdata <- panel(pdata, panel.id = c("zip","date"), duplicate.method = "first") 
          #     
          #         
          
          
          
          FE <- "zip + county_month  + dow + season_yr "
          
          exp_poly <- "l(smokePM1,0:7) + l(smokePM2,0:7) + l(smokePM3,0:7)  + l(smokePM4,0:7) + tmx + ppt + dist2fire"  
          
          xbin <- 0:50
          
          #Polynomial Models
          
          outcomes <- outcomes[outcomes %in% c( "ed_visit_rate","ed_alcohol_rate" ,"ed_endocrine_rate") ==F]
          
          
          model_output <- matrix(nrow = length(outcomes), ncol = 4)
          rownames(model_output) <- outcomes
          colnames(model_output)<-paste("deg",1:4, sep = "")
          
          for(i in 1:length(outcomes)){
            
            eqn <-get_eqn(outcomes[i],exp_poly,FE)
            mod <- feols(eqn, data = pdata, weights = pdata$avePop, cluster = "zip" )
            model_output[i,] <- get_poly_coef_cumulative(mod, 4)
            print(paste("done with ",outcomes[i],sep=""))
            
          }                
          
          write_rds(model_output, file = "data/clean/fig2b-polynomial-model-output-by-diagnosis_sy.rds")
          
          
          
          


##############################################################################################################              
# Plot
##############################################################################################################                            


      
      #Panel A
          xbin = 0:40
          response_coef <- read_rds("data/clean/fig2-polynomial-bootstrap-output_popweighted-032323.rds")[1:1000,]
          response_curve_b <- matrix(nrow = nrow(response_coef), ncol = length(xbin))
          response_curve_b <- response_curve_b[!is.na(response_coef[,1]),]
          for(i in 1:nrow(response_curve_b)){response_curve_b[i,] <- xbin*response_coef[i,1] + xbin^2*response_coef[i,2] + xbin^3*response_coef[i,3] + xbin^4*response_coef[i,4]}
          
          response_curve <- data.frame(x = xbin, est = apply(response_curve_b,2,median), lb = apply(response_curve_b, 2, function(x){quantile(x, .025)}), ub = apply(response_curve_b, 2, function(x){quantile(x, .975)}))
          
          response_curve_perchange <- response_curve
          response_curve_perchange[,2:4] <- response_curve_perchange[,2:4]/read_rds("data/clean/fig3-outcome-base-rates.rds")$brate[1]
          write_rds(response_curve_perchange, file = "data/clean/response_main_poly_percent_change.rds")
          
          
          # response_coef_singlereg <- read_rds("data/clean/fig4-polynomial-model-output.rds")[1,]
          # y <- xbin*response_coef_singlereg["deg1"] + xbin^2*response_coef_singlereg["deg2"] + xbin^3*response_coef_singlereg["deg3"] + xbin^4*response_coef_singlereg["deg4"] 
          
          

    ### Prep Panel B ###        
        
        resp_coef <- read_rds("data/clean/fig2b-polynomial-model-output-by-diagnosis_sy.rds") %>% as.data.frame()
        brates <- read_rds("data/clean/fig3-outcome-base-rates.rds")
        
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
        
        

        smoke <- read_rds("data/clean/smokePMdistribution.rds")
        smoke <- smoke[smoke>0]
        smoke[smoke>40]<-40
        hist(smoke, breaks = 40)
        
        hst_obj <- get_hst_obj(smoke, 0, 40, 1)
        #hst_obj$count[nrow(hst_obj)] <- sum(smoke==30)
        hst_obj$count[hst_obj$count<2000]<-2000
        
        #brates <- read_rds("data/clean/fig3-outcome-base-rates.rds")
        xbin <- 0:40
        
        
        
    
    pdf(file = "figures/manuscript/revised/raw/fig2-raw.pdf", width = 12, height = 5)
    
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
          
        #  lines(x, x*resp_coef$deg1[1]+ x^2*resp_coef$deg2[1] + x^3*resp_coef$deg3[1] + x^4*resp_coef$deg4[1], lwd = 3, col = 'black')
          
          
          axis(1, tick = T, at = seq(0,40,5), cex.axis = 1.5)
          axis(2, las = 2, tick = T, at = seq(-6, 2, 1), cex.axis = 1.5)
          
          plotHist(hst_obj, col = 'gray70',0.5, bottom = -6.25, height = 2.5, norm  = max(hst_obj), border.col = 'white')
    
    
          #segments(x0 = quantile(smoke, c(0.25,0.5, .75, .9, .95, .975, .99 )), y0 = -12, y1 =-8 )
          
          
          
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
          #lines(x, x*resp_coef$deg1[1]+ x^2*resp_coef$deg2[1] + x^3*resp_coef$deg3[1] + x^4*resp_coef$deg4[1], lwd = 8, col = 'black')
          lines(xbin, response_curve$est, lwd=3, col = 'black')
          
          
          axis(2, tick = T, las = 2, at = seq(-6, 2, 1), cex.axis = 1.5)
          axis(1, tick = T, at = seq(0, 40, 5), cex.axis = 1.5)
          
         # mtext(side = 2, text = "Change in ED Visits per 100K",line=3, cex = 2)
          mtext(side = 1, text = "Smoke PM2.5",line=3, cex = 2)
          
          
          plotHist(hst_obj, col = 'gray70',0.5, bottom = -6.25, height = 2.5, norm  = max(hst_obj), border.col = 'white')
          
          
          
    dev.off()            
    


    ####################################################################
    #Calculations with the response curve:
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

    
    plot(50:50, resp[1,52],  xlim = c(49.85, 50.05), ylim = c(-8,4),xlab = "",ylab = "", axes = F, col =NA,type = "l",lwd=3)
    
    
       
    i = 401
    
    subdat <- resp[,c(1, i + 1)]   
    subdat$color <- resp_coef$color[1:nrow(resp_coef)]
    
    pos <- subdat[subdat[,2]>=0,]
    neg <- subdat[subdat[,2]<0,]
    
    
    # if(nrow(pos)>0){rect(xleft = 49.9 - delta/2 , xright =49.9 + delta/2, ybottom = c(0, cumsum((unlist(pos[1:(nrow(pos)-1),2]) ))), ytop =   c(cumsum((unlist(pos[,2]) ))), col = (pos$color), border = NA)}
    # if(nrow(neg)>0){rect(xleft = 49.9 - delta/2, xright = 49.9+ delta/2, ybottom = c(0, cumsum((unlist(neg[1:(nrow(neg)-1),2]) ))), ytop =   c(cumsum((unlist(neg[,2]) ))), col = (neg$color), border = NA)}
    # 
    # 
    # text(x = 49.9, y = (cumsum((unlist(neg[,2]) ))  + c(0, cumsum((unlist(neg[1:(nrow(neg)-1),2]) ))))/2 , labels = paste(round(abs(100*neg[,2]/sum(abs(round(neg[,2],1)))),0), "%",sep=""), col ='white',cex=.75)
    # 
    # text(x = 49.9, y = (cumsum((unlist(pos[,2]) ))  + c(0, cumsum((unlist(pos[1:(nrow(pos)-1),2]) ))))/2 , labels = paste(round(abs(100*pos[,2]/sum(abs(round(pos[,2],1)))),0), "%",sep=""), col ='white',cex=.75)
    # 
    pos1 = data.frame(label = pos$outcome, val = pos[,2],  share = round(abs(100*pos[,2]/sum(abs(round(pos[,2],1)))),0), col = pos$color)
                     
    neg1 =      data.frame(label = neg$outcome, val = neg[,2],  share = round(abs(100*neg[,2]/sum(abs(round(neg[,2],1)))),0), col = neg$color)             
    
    i = 1001
    subdat <- resp[,c(1, i + 1)]   
    subdat$color <- resp_coef$color[1:nrow(resp_coef)]
    
    pos <- subdat[subdat[,2]>=0,]
    neg <- subdat[subdat[,2]<0,]
    
    
    # if(nrow(pos)>0){rect(xleft = 49.95 - delta/2 , xright =49.95 + delta/2, ybottom = c(0, cumsum((unlist(pos[1:(nrow(pos)-1),2]) ))), ytop =   c(cumsum((unlist(pos[,2]) ))), col = (pos$color), border = NA)}
    # if(nrow(neg)>0){rect(xleft = 49.95 - delta/2, xright = 49.95+ delta/2, ybottom = c(0, cumsum((unlist(neg[1:(nrow(neg)-1),2]) ))), ytop =   c(cumsum((unlist(neg[,2]) ))), col = (neg$color), border = NA)}
    # 
    # text(x = 49.95, y = (cumsum((unlist(neg[,2]) ))  + c(0, cumsum((unlist(neg[1:(nrow(neg)-1),2]) ))))/2 , labels = paste(round(abs(100*neg[,2]/sum(abs(round(neg[,2],1)))),0), "%",sep=""), col ='white',cex=.75)
    # 
    # text(x = 49.95, y = (cumsum((unlist(pos[,2]) ))  + c(0, cumsum((unlist(pos[1:(nrow(pos)-1),2]) ))))/2 , labels = paste(round(abs(100*pos[,2]/sum(abs(round(pos[,2],1)))),0), "%",sep=""), col ='white',cex=.75)
    # 
    
    pos2 = data.frame(label = pos$outcome, val = pos[,2],  share = round(abs(100*pos[,2]/sum(abs(round(pos[,2],1)))),0), col = pos$color)
    
    neg2 =      data.frame(label = neg$outcome, val = neg[,2],  share = round(abs(100*neg[,2]/sum(abs(round(neg[,2],1)))),0), col = neg$color)             
    
    
      i = length(x)
      subdat <- resp[,c(1, i + 1)]   
      subdat$color <- resp_coef$color[1:nrow(resp_coef)]
      
      pos <- subdat[subdat[,2]>=0,]
      neg <- subdat[subdat[,2]<0,]
      
      
      # if(nrow(pos)>0){rect(xleft = 50 - delta/2 , xright =50 + delta/2, ybottom = c(0, cumsum((unlist(pos[1:(nrow(pos)-1),2]) ))), ytop =   c(cumsum((unlist(pos[,2]) ))), col = (pos$color), border = NA)}
      # if(nrow(neg)>0){rect(xleft = 50 - delta/2, xright = 50+ delta/2, ybottom = c(0, cumsum((unlist(neg[1:(nrow(neg)-1),2]) ))), ytop =   c(cumsum((unlist(neg[,2]) ))), col = (neg$color), border = NA)}
      # 
      # text(x = 50, y = (cumsum((unlist(neg[,2]) ))  + c(0, cumsum((unlist(neg[1:(nrow(neg)-1),2]) ))))/2 , labels = paste(round(abs(100*neg[,2]/sum(abs(round(neg[,2],1)))),0), "%",sep=""), col ='white',cex=.75)
      # 
      # text(x = 50, y = (cumsum((unlist(pos[,2]) ))  + c(0, cumsum((unlist(pos[1:(nrow(pos)-1),2]) ))))/2 , labels = paste(round(abs(100*pos[,2]/sum(abs(round(pos[,2],1)))),0), "%",sep=""), col ='white',cex=.75)
      # 
      # 
      # abline(h = 0, col = 'red',lwd=2)
      # 
      
      
      
      
      
      pos3 = data.frame(label = pos$outcome, val = pos[,2],  share = round(abs(100*pos[,2]/sum(abs(round(pos[,2],1)))),0), col = pos$color)
      
      neg3 =      data.frame(label = neg$outcome, val = neg[,2],  share = round(abs(100*neg[,2]/sum(abs(round(neg[,2],1)))),0), col = neg$color)             
      
      template <- data.frame(label = rev(resp[,c(1, 1)]$outcome)  )

      pos1 = template %>% left_join(pos1) %>%  mutate(val = replace(val, is.na(val),0))
      pos2 = template %>% left_join(pos2) %>%  mutate(val = replace(val, is.na(val),0))
      pos3 = template %>% left_join(pos3) %>%   mutate(val = replace(val, is.na(val),0))
      neg1 = template %>% left_join(neg1) %>%  mutate(val = replace(val, is.na(val),0))
      neg2 = template %>% left_join(neg2) %>%  mutate(val = replace(val, is.na(val),0))
      neg3 = template %>% left_join(neg3) %>%  mutate(val = replace(val, is.na(val),0))
      
      pos1$share <- 100*pos1$val/sum(pos1$val, na.rm = T)
      pos2$share <- 100*pos2$val/sum(pos2$val, na.rm = T)
      pos3$share <- 100*pos3$val/sum(pos3$val, na.rm = T)
      neg1$share <- 100*neg1$val/sum(neg1$val, na.rm = T)
      neg2$share <- 100*neg2$val/sum(neg2$val, na.rm = T)
      neg3$share <- 100*neg3$val/sum(neg3$val, na.rm = T)
      
      
      pdf(file = "figures/raw/figSX-fig2b-contribution-barplot-raw.pdf", width =4, height = 12)   
      par(mfrow = c(3,1))
      
      barplot(pos1$share, beside = T, col = pos1$col, horiz = T, xlim =c(-100,100), axes=F, names.arg = gsub(pos1$label, pattern = "ed_", replace = ""), las=2)
      barplot(-neg1$share, beside = T, col = neg1$col, horiz = T, add = T, axes=F)
      #axis(1, at = seq(-100,100,25), labels = abs(seq(-100,100,25)))
      mtext(side = 3, adj = 0, text = "Low Smoke",cex=1.5)
      abline(v = 0, col = 'red',lwd=2)
      
      barplot(pos2$share, beside = T, col = pos2$col, horiz = T, xlim =c(-100,100), axes=F, names.arg = gsub(pos1$label, pattern = "ed_", replace = ""), las=2)
      barplot(-neg2$share, beside = T, col = neg2$col, horiz = T, add = T, axes=F)
      #axis(1, at = seq(-100,100,25), labels = abs(seq(-100,100,25)))
      mtext(side = 3, adj = 0, text = "Moderate Smoke",cex=1.5)
      abline(v = 0, col = 'red',lwd=2)
      
      barplot(pos3$share, beside = T, col = pos3$col, horiz = T, xlim =c(-100,100), axes=F,  names.arg = gsub(pos1$label, pattern = "ed_", replace = ""), las=2)
      barplot(-neg3$share, beside = T, col = neg3$col, horiz = T, add = T, axes=F)
      axis(1, at = seq(-100,100,25), labels = abs(seq(-100,100,25)))
      mtext(side = 3, adj = 0, text = "High Smoke",cex=1.5)
      abline(v = 0, col = 'red',lwd=2)
      mtext(side = 1, text = "estimated contribution (%)",cex=1.5,line=3)
      
      dev.off()
   # }  


      
      
      
      
      
      
      
      
      
      
      ####
      
      
      
      plot(0:50, resp[1,2:52],  xlim = c(0, 50), ylim = c(-8,4),xlab = "",ylab = "", axes = F, col =NA,type = "l",lwd=3)
      
      
      cumsum((unlist(pos[1:(nrow(pos)-1),2]) ))
      
      
      yn <- yp <- rep(NA, 2000)
      
      for(i in 1:length(x)){
        #positive
        
        subdat <- resp[,c(1, i + 1)]   
        subdat$color <- resp_coef$color[1:nrow(resp_coef)]
        
        pos <- subdat[subdat[,2]>=0,]
        neg <- subdat[subdat[,2]<0,]
        
        
        yp[i] <- max(cumsum(unlist(pos[,2]) ))
        yn[i] <- min(cumsum(unlist(neg[,2]) ))
        
     
        
      }  
      
      
      pdf(file = "figures/raw/fig-attribution-subpanel.pdf", width = 6, height = 5)
      
      
      plot(0:50, resp[1,2:52],  xlim = c(0, 50), ylim = c(-8,4),xlab = "",ylab = "", axes = F, col =NA,type = "l",lwd=3)
      
      for(i in 1:length(x)){
        #positive
        
        subdat <- resp[,c(1, i + 1)]   
        subdat$color <- resp_coef$color[1:nrow(resp_coef)]
        
        pos <- subdat[subdat[,2]>=0,]
        neg <- subdat[subdat[,2]<0,]
        
        
        if(nrow(pos)>0){rect(xleft = x[i] - delta/2 , xright =x[i] + delta/2, ybottom = c(0, cumsum((unlist(pos[1:(nrow(pos)-1),2]) ))), ytop =   c(cumsum((unlist(pos[,2]) ))), col = add.alpha('navy', 0.2), border = NA)}
        if(nrow(neg)>0){rect(xleft = x[i] - delta/2, xright = x[i] + delta/2, ybottom = c(0, cumsum((unlist(neg[1:(nrow(neg)-1),2]) ))), ytop =   c(cumsum((unlist(neg[,2]) ))), col = add.alpha('red3', 0.2), border = NA)}
        
        
      }  
      
      
      
      
     # abline(h = 0, col ='gray', lwd =2)
      #lines(x, x*resp_coef$deg1[1]+ x^2*resp_coef$deg2[1] + x^3*resp_coef$deg3[1] + x^4*resp_coef$deg4[1], lwd = 8, col = 'black')
     # lines(xbin, response_curve$est, lwd=3, col = 'black')
      
      
     
      
     # plotHist(hst_obj, col = 'gray70',0.5, bottom = -8, height = 4, norm  = max(hst_obj), border.col = 'white')
      
      
      
      lines(x, yn, col ='red3', lwd=4)
      lines(x, yp, col ='navy', lwd=4)
      
      abline(h = 0, col ='gray', lwd =2)
      #lines(x, x*resp_coef$deg1[1]+ x^2*resp_coef$deg2[1] + x^3*resp_coef$deg3[1] + x^4*resp_coef$deg4[1], lwd = 8, col = 'black')
      lines(xbin, response_curve$est, lwd=3, col = 'black')
      
      
      axis(2, tick = T, las = 2, at = seq(-8, 4, 2), cex.axis = 1.5)
      axis(1, tick = T, at = seq(0, 50, 10), cex.axis = 1.5)
      
      # mtext(side = 2, text = "Change in ED Visits per 100K",line=3, cex = 2)
      mtext(side = 1, text = "Smoke PM2.5",line=3, cex = 2)
      
      
     # plotHist(hst_obj, col = 'gray70',0.5, bottom = -8, height = 4, norm  = max(hst_obj), border.col = 'white')
      dev.off()
      

