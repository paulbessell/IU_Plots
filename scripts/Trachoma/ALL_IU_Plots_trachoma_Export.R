
library(tidyverse)
library(readr)
library(ggplot2)
library(Hmisc)

setwd("../RCode/IU_Plots") # Update
plotOutputPath <- paste0("Trachoma/Plots/", Sys.Date(), "/Individual")
outpath <- if(!dir.exists(plotOutputPath)) dir.create(plotOutputPath, recursive = T)
plotOutputPath <- paste0("Trachoma/Plots/", Sys.Date())

ius <- read_csv("Data/Oncho/IU_List.csv") # Necessary for curating IU names later. Just a global list

prevPath <- "Data/Trachoma/historic_simulations"
prevalenceFiles <- list.files(prevPath) # Requires updating with folder names. Also below
iusList <- gsub(".csv|PrevDataset_Trachoma_", "", prevalenceFiles)

geostat <- get(load("Data/Trachoma/trachoma_maps.rds"))
geostat_years <- get(load("Data/Trachoma/trachoma_map_years.rds"))
for(i in 1:length(geostat_years)){
  if(i == 1){
    geostatDF <- geostat[[i]]$data
    geostatDF$Year <- geostat_years[i]
  }
  if(i > 1){
    cDF <- geostat[[i]]$data
    cDF$Year <- geostat_years[i]
    geostatDF <- geostatDF %>% bind_rows(cDF)
  }
}

geostatDF <- geostatDF %>%
  filter(!is.na(max_tf_prev_upr)) %>%
  mutate(tf_prev_lwr = case_when(
    max_tf_prev_upr == 0.05 ~ 0,
    max_tf_prev_upr == 0.099 ~ 0.05,
    max_tf_prev_upr == 0.299 ~ 0.1,
    max_tf_prev_upr == 0.499 ~ 0.3,
    max_tf_prev_upr == 1 ~ 0.5)) %>%
  left_join(ius)

mda <- read_csv("Data/Trachoma/mda_history_trachoma.csv") %>% 
  left_join(ius) %>%
  filter(PC_in_group > 0)

    
helperDF <- line_df <- data.frame( # Needed for plotting
  x = seq(2000, 2020, 5),
  y = rep(-0.02, 5),
  xend = seq(2000, 2020, 5),
  yend = rep(0,5)
)

pdf(file = paste0(plotOutputPath, "/Trachoma_IU_Plots.pdf"),onefile = TRUE)
for(i in 1:length(iusList)){
# for(i in 1:100){
    cIU <- iusList[i]

    exprev <- read_csv(paste0(prevPath, "/PrevDataset_Trachoma_", cIU, ".csv")) %>%
      filter(measure == "prevalence",
              Time >= 2000) %>%
      dplyr::select(-c(age_start, age_end, measure)) %>%
      gather(key = "Sim_ID", value = "Prevalence", -c("Time"))
  
    exgeostat <- geostatDF %>%
      filter(IU_ID_Full == cIU)
    
    #exoncho <- onchohistory %>%
    #  filter(IU_CODE == cIU)
    
    exmda <- mda %>%
      filter(IU_ID_Full == cIU) %>%
      filter(Year >= 2000)
    
    # sitePresent <- cIU %in% onchoSites$IU_CODE # Check whether there is site-level data
    # if(sitePresent){
    #   exsites <- onchoSites %>%
    #     filter(IU_CODE == cIU,
    #            Examined >= Positive) #%>%
    #     mutate(mprev = binconf(Positive, Examined)[,1],
    #            lci = binconf(Positive, Examined)[,2],
    #            uci = binconf(Positive, Examined)[,3])
    #   }

    max1 <- max(c(exprev$Prevalence)) # Setr the limits for the axes
    # if(sitePresent) max1 <- max(c(max1, exsites$uci))
    
    plottop <- ceiling(max1 / 0.1) * 0.1
    plotbottom <- -0.1 * plottop
    helperDF$y2 <- helperDF$y * plottop
    
    # tester <- sum(!is.na(exoncho$Endemicity_P)) != 0
    # 
    #   if(tester){
    #     lastyear <- max(exoncho$Year[!is.na(exoncho$Parent1_IU_ID)])
    #     parentIU <- exoncho$Parent1_IU_ID[exoncho$Year == lastyear]
    #     lastIU <- ifelse(parentIU < 10000, paste0(exoncho$ADMIN0ISO3[exoncho$Year == 2021], "0", parentIU), paste0(exoncho$ADMIN0ISO3[exoncho$Year == 2021], parentIU))
    #     allIUs <- unique(onchohistory$IU_ID_Full[onchohistory$Parent1_IU_ID == parentIU])
    #     allIUs <- paste(allIUs[!is.na(allIUs)], collapse = ", ")
    #   }
    # 
    
    testPlot <- #ggplot(data = exgeostat, aes(x = Year, y = Prev, group = Year)) + 
      ggplot(data = exprev, aes(x = Time, y = Prevalence, group = Sim_ID)) +
      geom_line(col = "grey85") +
      theme(axis.title.x = element_blank()) +
      # geom_violin(fill = "#057AC1", col = "#057AC1", alpha = 0.8, width = 1) +
      geom_segment(x = 2000, xend = 2023, y = -0.1 * plottop, yend = -0.1 * plottop, size = 10, col = NA)
    
    if(nrow(exgeostat) > 0){
      testPlot <- testPlot +
        geom_segment(data = exgeostat, aes(x = Year, y = tf_prev_lwr, yend = max_tf_prev_upr), inherit.aes = F, col =  "#057AC1")
    }
    
    if(nrow(exmda) > 0) {
      testPlot <- testPlot +  
      geom_point(data = exmda, aes(x = Year, y = -.05 * plottop), inherit.aes = F, shape = 2, size = 2)
    }
    
    # if(sitePresent){
    #   testPlot <- testPlot + 
    #     geom_point(data = exsites, aes(x = xPos, y = mprev), col = "#DE4800", inherit.aes = F) +
    #     geom_linerange(data = exsites, aes(x = xPos, ymin = lci, ymax = uci), col = "#DE4800", inherit.aes = F)
    # }
    
    
    testPlot <- testPlot +
      scale_x_continuous(limits = c(1999.5, 2024)) +
      scale_y_continuous(limits = c(plotbottom, plottop)) +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.ticks.x = element_blank(),
            panel.grid = element_blank())  +
      theme(axis.line.x = element_blank(), axis.text.x = element_blank()) +
      geom_hline(yintercept = 0) +
      geom_segment(data = helperDF, aes(x = x, xend = xend, y = y2, yend = yend), inherit.aes = F) +
      geom_text(data = helperDF, aes(x = x, y = (y2- (0.01 * plottop)), label = x), inherit.aes = F) +
      ylab("Prevalence")
    
    # if(tester){
    #   testPlot <- testPlot +
    #   geom_text(aes(x = mean(c(2000, lastyear)), y = -0.15 * plottop), label = lastIU) +
    #   geom_text(aes(x = mean(c(2000, lastyear)), y = -0.2 * plottop), label = paste0("(", allIUs, ")"), size = 3) +
    #   geom_text(aes(x = mean(c(2023, lastyear)), y = -0.175 * plottop), label = cIU)
    # }
    tester <- FALSE
    if(!tester) testPlot <- testPlot +  geom_text(aes(x = mean(c(2000, 2024)), y = -0.1 * plottop), label = cIU)
      testPlot <- testPlot +  geom_hline(yintercept = 0.01, lty = "dashed")
    
    
    print(testPlot)
    ggsave(testPlot, file = paste0(plotOutputPath, "/Individual/", cIU, ".jpg"))

}
dev.off()
