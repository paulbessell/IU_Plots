
library(tidyverse)
library(readr)
library(ggplot2)
library(Hmisc)

setwd(".../IU_Plots") # Update
plotOutputPath <- paste0("LF/Plots/", Sys.Date(), "/Individual")
outpath <- if(!dir.exists(plotOutputPath)) dir.create(plotOutputPath, recursive = T)
plotOutputPath <- paste0("LF/Plots/", Sys.Date())

ius <- read_csv("Data/Schisto/IU_List.csv") # Necessary for curating IU names later

prevalenceFiles <- list.files("Data/LF/LF_prevalence_data_081124") # Requires updating with folder names. Also below
iusList <- gsub(".csv", "", prevalenceFiles)

geostat <- read_csv("Data/LF/LF_map_data_141124/GeostatisticalMaps.csv") %>% # This is the mapping data
  gather(key = "Sample", value = "Value", -c("IUID", "Year"))

lfhistory <- read_csv("Data/LF/final_df_lf_Parent_Child.csv") %>% # IU history data
  mutate(IU_ID_Full = ifelse(IU_ID_MAPPING < 10000, paste0(ADMIN0ISO3, "0", IU_ID_MAPPING), paste0(ADMIN0ISO3, IU_ID_MAPPING)))

lfSites <- read_csv("Data/LF/LF_Sites.csv") %>% # Site level data from ESPEN
  mutate(IU_ID_Full = ifelse(IU_2021 < 10000, paste0(ISO3, "0", IU_2021), paste0(ISO3, IU_2021))) %>%
  mutate(monthN = match(MonthSurvey, month.name),
         xPos = SurveyYear + ((monthN - 6) / 13) +
           runif(nrow(.), -0.3, 0.3),
         sampleType = NA,
         sampleType = ifelse(Method_2 %in% c("Blood smear", "Filtration",  "Chamber"), "mf", sampleType),
         sampleType = ifelse(Method_2 %in% c("FTS (Ag)", "ICT (Ag)"), "Ag", sampleType)
  ) %>%
  filter(!is.na(sampleType)) %>%
  filter(!is.na(Examined), !is.na(Positive))

lfRegions <- read_csv("Data/LF/LF_Regions/IU_regions.csv")

mda <- read_csv("Data/LF/LF_MDA_data_141124/MDAIU.csv") %>%
  gather(key = "Year", value = "Scenario", -IUID) %>%
  mutate("Year2" = as.numeric(gsub("MDACov_", "", Year)),
         "MDA" = ifelse(Scenario == "00x.xxx", NA, 1),
         "Coverage" = as.numeric(substr(Scenario, 0, 2)) / 100,
         "Coverage" = ifelse(Coverage == 0, NA, Coverage)) 

    
helperDF <- line_df <- data.frame( # Needed for plotting
  x = seq(2000, 2020, 5),
  y = rep(-0.02, 5),
  xend = seq(2000, 2020, 5),
  yend = rep(0,5)
)

pdf(file = paste0(plotOutputPath, "/LF_IU_Plots.pdf"),onefile = TRUE)
for(i in 1:length(iusList)){
# for(i in 1:100){
    cIU <- iusList[i]

    exprev <- read_csv(paste0("Data/LF/LF_prevalence_data_081124/", cIU, ".csv")) %>%
      gather(key = "Sim_ID", value = "Prevalence", -c("IUID", "year"))
  
    exgeostat <- geostat %>%
      filter(IUID == cIU)
    exlf <- lfhistory %>%
      filter(IU_ID_Full == cIU)
    exmda <- mda %>%
      filter(IUID == cIU)
    
    sitePresent <- cIU %in% lfSites$IU_ID_Full # Check whether there is site-level data
    if(sitePresent){
      exsites <- lfSites %>%
        filter(IU_ID_Full == cIU) %>%
        mutate(mprev = binconf(Positive, Examined)[,1],
               lci = binconf(Positive, Examined)[,2],
               uci = binconf(Positive, Examined)[,3])
      }

    max1 <- max(c(exprev$Prevalence)) # Setr the limits for the axes
    if(sitePresent) max1 <- max(c(max1, exsites$uci))
    
    plottop <- ceiling(max1 / 0.1) * 0.1
    plotbottom <- -0.2 * plottop
    helperDF$y2 <- helperDF$y * plottop
    
    tester <- sum(!is.na(exlf$Endemicity_P)) != 0
    
      if(tester){
        lastyear <- max(exlf$Year[!is.na(exlf$Parent1_IU_ID)])
        parentIU <- exlf$Parent1_IU_ID[exlf$Year == lastyear]
        lastIU <- ifelse(parentIU < 10000, paste0(exlf$ADMIN0ISO3[exlf$Year == 2021], "0", parentIU), paste0(exlf$ADMIN0ISO3[exlf$Year == 2021], parentIU))
        allIUs <- unique(lfhistory$IU_ID_Full[lfhistory$Parent1_IU_ID == parentIU])
        allIUs <- paste(allIUs[!is.na(allIUs)], collapse = ", ")
      }
    
    
    testPlot <- ggplot(data = exgeostat, aes(x = Year, y = Value, group = Year)) + 
      geom_line(data = exprev, aes(x = year, y = Prevalence, group = Sim_ID), col = "grey85") + theme(axis.title.x = element_blank()) +
      geom_violin(fill = "#057AC1", col = "#057AC1", alpha = 0.8, width = 1) +
      geom_segment(x = 2000, xend = 2023, y = -0.1 * plottop, yend = -0.1 * plottop, size = 10, col = NA)
    
    if(nrow(exmda) > 0) {
      testPlot <- testPlot +  
      geom_point(data = exmda, aes(x = Year2, y = -.1 * plottop), inherit.aes = F, shape = 2, size = 2)
    }
    
    if(sitePresent){
      testPlot <- testPlot + 
        geom_point(data = exsites, aes(x = xPos, y = mprev), col = "#DE4800", inherit.aes = F) +
        geom_linerange(data = exsites, aes(x = xPos, ymin = lci, ymax = uci), col = "#DE4800", inherit.aes = F)
    }
    
    
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
      geom_text(data = helperDF, aes(x = x, y = (y2- (0.02 * plottop)), label = x), inherit.aes = F) +
      ylab("Prevalence")
    
    if(tester){
      testPlot <- testPlot +
      geom_text(aes(x = mean(c(2000, lastyear)), y = -0.15 * plottop), label = lastIU) +
      geom_text(aes(x = mean(c(2000, lastyear)), y = -0.2 * plottop), label = paste0("(", allIUs, ")"), size = 3) +
      geom_text(aes(x = mean(c(2023, lastyear)), y = -0.175 * plottop), label = cIU)
    }
    if(!tester) testPlot <- testPlot +  geom_text(aes(x = mean(c(2000, 2024)), y = -0.2 * plottop), label = cIU)
      testPlot <- testPlot +  geom_hline(yintercept = 0.01, lty = "dashed")
    
    
    print(testPlot)
    ggsave(testPlot, file = paste0(plotOutputPath, "/Individual/", cIU, ".jpg"))

}
dev.off()
