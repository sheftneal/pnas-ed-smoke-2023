source("scripts/00_init.R")

              
        ########################################################################
          # [1]   Prep Data
        ########################################################################
              

            #PANEL A

                  # dx <- readRDS( "data/fig1-dx-icd-by-year.rds") #[not included  - is a data frame with annual statewide counts of total ED visits by ICD code]
                  # 
                  #             #aggregate counts by grouping
                  #               ave <- dx %>% group_by(year,cat) %>% summarise(count = sum(count))
                  # 
                  #             #get total visits by year to be used for calculating shares
                  #               tot <- dx %>% group_by(year) %>% summarise(count = sum(count)) %>% rename(count_tot = count)
                  # 
                  #             #calculate share of total visits by ICD grouping
                  #               ave <- left_join(ave, tot) %>% mutate(share = count/count_tot)
                  # 
                  # write_rds(ave, file = "data/fig1-data.rds")            
                  
          
            #PANEL B
                    
                  #   symp <-   dx %>% 
                  #             filter(cat=="symp") %>% 
                  #             group_by(cat_sub, cat_sub2) %>% 
                  #             summarise(count = sum(count, na.rm = T))# %>% 
                  # 
                  #   
                  #   acc <-    dx %>% 
                  #             filter(cat=="injury") %>% 
                  #             mutate(
                  #               cat_sub2 = replace(cat_sub2, cat_sub2 == "skull non-frac","other")) %>% 
                  #             mutate(cat_sub2 = replace(cat_sub2, is.na(cat_sub2) & cat_sub=="other","other")) %>% 
                  #             group_by(cat_sub, cat_sub2) %>% 
                  #             summarise(count = sum(count, na.rm = T)) %>% 
                  #             arrange(cat_sub,-count)
                  #   
                  #   
                  #   resp <-   dx %>% 
                  #             filter(cat=="resp")  %>% 
                  #             mutate(
                  #               cat_sub2 = replace(cat_sub2, is.na(cat_sub2),"other resp"),
                  #             ) %>%
                  #             group_by(cat_sub, cat_sub2) %>% 
                  #             summarise(count = sum(count, na.rm = T)) %>% 
                  #             arrange(cat_sub, -count)
                  #           
                  #   
                  #   cvd <-    dx %>% 
                  #             filter(cat=="circ") %>% 
                  #             group_by(cat_sub) %>% 
                  #             summarise(count = sum(count, na.rm = T))
                  # 
                  # save(symp, acc, resp, cvd, file = "data/fig1-b-data.Rdata")

            #PANEL C + D    
                
                # #smoke PM from Childs et al 2023 downloadable here: https://www.dropbox.com/work/projects/daily-10km-smokePM/final/10km_grid
                # spm <- read_rds("data/smokePM2pt5_predictions_daily_10km_20060101-20201231.rds")
                # 
                # #mapping of smoke PM grid cells to California state boundaries
                # load("data/smoke-pm-grid-CA-zip-mapping.RData")
                # 
                # 
                # #subset full dataset to cells that overlap California zipcodes
                # spm <- spm[spm$grid_id_10km %in% zip_grid_ids,]
                # 
                # 
                # #define bins at cells level, then left-join with zip code to cell id mapping, then take mean up to zip code day, save as list item. eventually rbindlist.
                # 
                # # define bins
                # 
                # spm <- spm %>% mutate(
                #   bin0_5 = as.numeric(smokePM_pred > 0 & smokePM_pred <= 5),
                #   bin5_10 = as.numeric(smokePM_pred > 5 & smokePM_pred <= 10),
                #   bin10_25 = as.numeric(smokePM_pred > 10 & smokePM_pred <= 25),
                #   bin25_50 = as.numeric(smokePM_pred > 25 & smokePM_pred <= 50),
                #   bin50 = as.numeric(smokePM_pred > 50 )
                # )
                # 
                # ncell <- length(unique(spm$grid_id_10km))
                # 
                # 
                # 
                # spm_yr <- spm %>% 
                #   mutate(year = lubridate::year(date)) %>% 
                #   group_by(year) %>% 
                #   summarise(across(c("bin0_5","bin5_10","bin10_25","bin25_50","bin50"), ~sum(.x, na.rm = T)) )
                # 
                # spm_share <-  spm %>%
                #   summarise(across(c("bin0_5","bin5_10","bin10_25","bin25_50","bin50"), ~sum(.x, na.rm = T)) )
                # 
                # spm_share <- data.frame(cat = c("0-5","5-10","10-25", "25-50",">50"), count = as.numeric(spm_share))
                # 
                # 
                # spm_hist <- as.numeric(spm$smokePM_pred)
                # spm_hist[spm_hist>100]<-100
                # spm_hist <- spm_hist[spm_hist>0]
                # spm_hist <- spm_hist


                #save()
                  
          ########################################################################
          # [2]   Plot
          ########################################################################
              
          #################################################
          # Panel A - ED Visits over time by ICD grouping
          #################################################           

            ave <- read_rds("data/fig1-a-data.rds")                       

                  #some reformatting for plot prep
                        ave_wide <- ave %>% dplyr::select(year, cat, count) %>% pivot_wider(names_from = cat, values_from = count) #long to wide for plotting
                        

                        aveV <- apply(ave_wide[,2:ncol(ave_wide)], 2, function(x){mean(x, na.rm = T)}) #calc average per year by grouping
                        ave_wide[,2:ncol(ave_wide)] <- ave_wide[,2:ncol(ave_wide)][order(-aveV)] #order min to max for plotting

                            
                        tot_yr <- ave %>% group_by(year) %>% summarise(count  = sum(count)) #calc total by year
                            
                  # Make Plot
                            
                            pdf("figures/1-raw/fig1a-raw.pdf")
                            
                            areaplot(ave_wide$year, ave_wide[,2:ncol(ave_wide)]/1e6, 
                                     col = MetBrewer::met.brewer(name = "Signac",n = ncol(ave_wide)-1, type = "continuous")[(ncol(ave_wide)-1):1], 
                                     xlim = c(2006, 2017),ylim = c(0,14),border = 'white',xlab = "",ylab = "", axes = F, legend = F,   args.legend = list(x = "topleft", cex = 0.65))
                            
                            axis(1, at = seq(2007,2017,2), tick = F)
                            axis(2, las = 2, tick = F)
                            
                            mtext(side = 2, text = "Visits (millions)",line=3, cex = 1.5)
                            mtext(side = 3, text = "Total ED visits by primary diagnosis", cex=2, adj=0)

                            
                          dev.off()
              
              
                      
                          
            #################################################
            # Panel B - tree plots by sub category
            #################################################           
                            
            load("data/fig1-b-data.Rdata")
                
              #General Symptoms
                pdf("figures/raw/fig1b-symp-rawr.pdf")
                      par(mar = c(4,4,4,4))
                      treemap(symp,
                          index=c("cat_sub","cat_sub2"),
                          vSize="count",
                          type="index",
                          palette = "Greens"
                              ) 
                    dev.off()
                          
                          
                          
                #Injuries
                pdf("figures/raw/fig1b-inj-raw.pdf")
                par(mar = c(4,4,4,4))
                  treemap(acc,
                          index=c("cat_sub","cat_sub2"),
                          vSize="count",
                          type="index", 
                          palette = "Blues",
                          fontsize.labels =25
                            ) 
                dev.off()
                          
                 
                #Respiratory      
                pdf("figures/raw/fig1b-resp-raw.pdf")
                par(mar = c(4,4,4,4))
                treemap(resp,
                        type = "index",
                        index=c("cat_sub","cat_sub2"),
                        vSize="count",
                        palette  = "Purples"
                ) 
                dev.off()
                
              
                #Circ System
                pdf("figures/raw/fig1b-cvd-raw.pdf")
                par(mar = c(4,4,4,4))
                treemap(cvd,
                        index=c("cat_sub"),
                        vSize="count",
                        type="index",
                        palette = "YlOrBr"
                ) 
                dev.off()
                
               
                
          ######################################################################      
          ###### Calculate total counts cited in title of each sub panel ####
                
                dx %>% filter(cat=="resp") %>% group_by(year) %>% summarise(count= sum(count)) %>% ungroup() %>% summarise(count = mean(count)) %>% mutate(count = round(count/1e6,1))
                
                dx %>% filter(cat=="circ") %>% group_by(year) %>% summarise(count= sum(count)) %>% ungroup() %>% summarise(count = mean(count))%>% mutate(count = round(count/1e6,1))
                
                dx %>% filter(cat=="injury") %>% group_by(year) %>% summarise(count= sum(count)) %>% ungroup() %>% summarise(count = mean(count))%>% mutate(count = round(count/1e6,1))
                
                dx %>% filter(cat=="symp") %>% group_by(year) %>% summarise(count= sum(count)) %>% ungroup() %>% summarise(count = mean(count))%>% mutate(count = round(count/1e6,1))
                
                
                  
           
                
              #################################################
              # Panel C - Smoke time series area plot
              #################################################           
                
                

                 pdf("figures/raw/fig1-c-raw.pdf")

                      areaplot(spm_yr$year, spm_yr[,2:ncol(spm_yr)]/ncell, col = MetBrewer::met.brewer("Greek",5)[5:1], border = 'white',xlab = "",ylab = "", axes = F, legend = F,   args.legend = list(x = "topleft", cex = 0.65))
                          axis(1, at = seq(2007,2017,2), tick = F)
                          axis(2, las = 2, tick = F)
                      mtext(side = 2, text = "Days",line=3, cex = 1.5)
                      mtext(side = 3, text = "Average Annual Smoke Frequency by Intensity", cex=2, adj=0)

                dev.off()
                
                
                
              #################################################
              # Panel D - Histogram of smoke distribution
              #################################################           
                
                     
                pdf("figures/raw/fig1-d-raw.pdf")
               
                hist(spm_hist, col = c(rep(MetBrewer::met.brewer("Greek",5)[5],1 ), 
                                       rep(MetBrewer::met.brewer("Greek",5)[4],1 ),
                                       rep(MetBrewer::met.brewer("Greek",5)[3],3 ),
                                       rep(MetBrewer::met.brewer("Greek",5)[2],5 ),
                                       rep(MetBrewer::met.brewer("Greek",5)[1],10 )
                                      ),
                border = 'white', axes = F, xlab = "", ylab = "", main = "",freq =F
                    )
                
                axis(1, tick = F,at = seq(0,100,10))
                axis(2, tick = F, las=2, at = seq(0, .1, .02), labels = 5*seq(0,10,2))
                
                mtext(side = 1, text = "Smoke PM2.5",cex=2,line=3)
                mtext(side = 2, text = "Share of days (%)",cex=2,line=3)
          
                dev.off()
        
                
                        
                #count smoke days
                spm %>% filter(smokePM_pred>0) %>% mutate(year = lubridate::year(date)) %>% group_by(year,grid_id_10km) %>% summarise(count = n()) %>% ungroup() %>% summarise(ave = mean(count))
                
                
                
                
                
                
                 
                
                
