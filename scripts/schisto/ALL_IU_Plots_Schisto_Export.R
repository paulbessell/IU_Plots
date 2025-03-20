
library(tidyverse)
library(readr)
library(ggplot2)
library(Hmisc)

setwd("..../IU_Plots") # Update
plotOutputPath <- paste0("Schisto/Plots/", Sys.Date(), "/Individual")
outpath <- if(!dir.exists(plotOutputPath)) dir.create(plotOutputPath, recursive = T)
plotOutputPath <- paste0("Schisto/Plots/", Sys.Date())

ius <- read_csv("Data/Schisto/IU_List.csv") # Necessary for curating IU names later

prevalenceFiles <- list.files("Data/Schisto/SCH_prevalence_data_021224") # Requires updating with folder names. Also below
iusList <- gsub("PrevDataset_Haema_|.csv", "", prevalenceFiles) # Currently only for haematobium. Not seen data for other species

geostat <- get(load("Data/Schisto/SCH_map_data_021224/SCH_map_data_021224/haematobium_maps.rds")) # This is the mapping data

geostatcomb <- as.data.frame(geostat[[1]]) %>%
  mutate(Year = 2002) %>%
  bind_rows(as.data.frame(geostat[[2]]) %>%
              mutate(Year = 2013))  %>%
  bind_rows(as.data.frame(geostat[[3]]) %>%
              mutate(Year = 2022))
names(geostatcomb) <- gsub("data.", "", names(geostatcomb))


geostatcombGather <- geostatcomb %>%
  dplyr::select(-TaskID) %>%
  gather(key = "SimID", value = "Prev", -c("Year", "IU_ID")) %>%
  left_join(ius %>%
              dplyr::select(IU_ID, IU_ID_Full))


schhistory <- read_csv("Data/Schisto/Schisto_IU_Cleaned_1.csv") %>% # IU history data
  mutate(IU_ID_Full = ifelse(IU_ID_MAPPING < 10000, paste0(ADMIN0ISO3, "0", IU_ID_MAPPING), paste0(ADMIN0ISO3, IU_ID_MAPPING))) %>%
  filter(TargetPop == "Total")

schSites <- read_csv("Data/Schisto/Schisto_Sites.csv") %>% # Site level data from ESPEN
  mutate(IU_ID_Full = ifelse(IU_2021 < 10000, paste0(ISO3, "0", IU_2021), paste0(ISO3, IU_2021))) %>%
  filter(!is.na(Examined), !is.na(Positive))  %>%
  filter(SCH_spp %in% c("s.haematobium",  "S.haematobium")) %>%
  group_by(IU_ID_Full, SurveyYear) %>%
  summarise(Examined = sum(Examined),
            Positive = sum(Positive)) %>%
  mutate(xPos = SurveyYear)



mda <- read_csv("Data/Schisto/SCH_MDA_data_021224/SCH_MDA_data_021224/mda_history_haematobium.csv") %>%
  filter(MDA_scheme == "Old Product B (SOC)") %>%
  left_join(schhistory %>%
              dplyr::select(IU_ID_MAPPING, IU_ID_Full) %>%
              distinct())

    
helperDF <- line_df <- data.frame( # Needed for plotting
  x = seq(2000, 2020, 5),
  y = rep(-0.02, 5),
  xend = seq(2000, 2020, 5),
  yend = rep(0,5)
)

pdf(file = paste0(plotOutputPath, "/Haematobium_IU_Plots_SAC.pdf"),onefile = TRUE)
for(i in 1:length(iusList)){
# for(i in 1:100){
    cIU <- iusList[i]

    exprev <- read_csv(paste0("Data/Schisto/SCH_prevalence_data_021224/PrevDataset_Haema_", cIU, ".csv")) %>%
      dplyr::select(!c("age_start", "age_end",   "intensity", "species")) %>%
      gather(key = "Sim_ID", value = "Prevalence", -c("measure", "year_id")) %>%
      filter(measure == "Prevalence SAC")
  
    exgeostat <- geostatcombGather %>%
      filter(IU_ID_Full == cIU)
    exlf <- schhistory %>%
      filter(IU_ID_Full == cIU)
    exmda <- mda %>%
      filter(IU_ID_Full == cIU)
    
    sitePresent <- cIU %in% schSites$IU_ID_Full # Check whether there is site-level data
    if(sitePresent){
      exsites <- schSites %>%
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
        allIUs <- unique(schhistory$IU_ID_Full[schhistory$Parent1_IU_ID == parentIU])
        allIUs <- paste(allIUs[!is.na(allIUs)], collapse = ", ")
      }
    
    
    testPlot <- ggplot(data = exgeostat, aes(x = Year, y = Prev, group = Year)) + 
      geom_line(data = exprev, aes(x = year_id, y = Prevalence, group = Sim_ID), col = "grey85") + theme(axis.title.x = element_blank()) +
      geom_violin(fill = "#057AC1", col = "#057AC1", alpha = 0.8, width = 1) +
      geom_segment(x = 2000, xend = 2023, y = -0.1 * plottop, yend = -0.1 * plottop, size = 10, col = NA)
    
    if(nrow(exmda) > 0) {
      testPlot <- testPlot +  
      geom_point(data = exmda, aes(x = Year, y = -.1 * plottop), inherit.aes = F, shape = 2, size = 2)
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
