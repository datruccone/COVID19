library(BatchGetSymbols)
library("ggplot2")
library(Rmisc)
library(reshape2)
library(lubridate)
library(scales)
#library(ggthemes) #Themes for ggplot2
library(hrbrthemes) #Themes for ggplot2

load("Crashes Comparison.RData")

#Indexes Import, this piece of code is based on: https://cran.r-project.org/web/packages/BatchGetSymbols/vignettes/BatchGetSymbols-vignette.html
l.out <- BatchGetSymbols(tickers = c('^GSPC'), 
                         first.date = '1973-1-1',   
                         last.date = Sys.Date(), 
                         freq.data = 'weekly',
#                         freq.data = 'daily',
                         cache.folder = file.path(tempdir(), 
                                                  'BGS_Cache') ) # cache in tempdir()

#SP500_Daily <- l.out$df.tickers
SP500 <- l.out$df.tickers
rm(l.out)

#Function to extract data between given dates
date_range <- function(x,y){round(SP500[SP500$ref.date >= x & SP500$ref.date <= y,c(3,10)]*100,2)}

#Crises and Outbreaks Dates Inputs
Crises <- na.omit(readxl::read_excel("Crashes_List.xlsx",sheet = "Crises"))
Outbreaks <- na.omit(readxl::read_excel("Crashes_List.xlsx",sheet = "Outbreaks"))

crises_list <-list()
outbreaks_list <- list()

for (i in 1:nrow(Crises)){
  start_date = as.Date(Crises[[i,2]])  #https://stackoverflow.com/questions/18508633/get-only-the-value-of-an-element-in-an-r-data-frame-without-the-index
  end_date = as.Date(Crises[[i,3]])
  weeks_leght = as.integer(round(difftime(end_date,start_date,units = c("weeks"))))
  Temp <- data.frame(Week = seq.int(1,weeks_leght,1), date_range(start_date,end_date)) #It may give an error here if last week COVID-19 data is not yet available
  Temp$volume <- Temp$volume/1000000000 # Volumen en 1x10^9 = 1 Billion (American)
  Temp$Cumulative <- cumsum(Temp$ret.closing.prices)
  assign(paste0(year(as.Date(Crises[[i,2]])),"_", as.character(Crises[i,1])),Temp)
  crises_list[i] <- list(Temp)
  rm(Temp)
}
rm(i)

for (i in 1:nrow(Outbreaks)){
  start_date = as.Date(Outbreaks[[i,2]])  #https://stackoverflow.com/questions/18508633/get-only-the-value-of-an-element-in-an-r-data-frame-without-the-index
  end_date = as.Date(Outbreaks[[i,3]])
  weeks_leght = as.integer(round(difftime(end_date,start_date,units = c("weeks"))))
  Temp <- data.frame(Week = seq.int(1,weeks_leght,1), date_range(start_date,end_date)) #It may give an error here if last week COVID-19 data is not yet available
  Temp$volume <- Temp$volume/1000000000 # Volumen en 1x10^9 = 1 Billion (American)
  Temp$Cumulative <- cumsum(Temp$ret.closing.prices)
  assign(paste0(year(as.Date(Outbreaks[[i,2]])),"_", as.character(Outbreaks[i,1])),Temp)
  outbreaks_list[i] <- list(Temp)
  rm(Temp)
}
rm(i)

Comparison_Crises <- Reduce(function(x, y) merge(x, y, by = "Week", all=TRUE), crises_list)
for (i in 1:nrow(Crises)){
  names(Comparison_Crises)[(i*3)-1] = paste0(year(as.Date(Crises[[i,2]])),"_",as.character(Crises[i,1]),"_volume")
  names(Comparison_Crises)[i*3] = paste0(year(as.Date(Crises[[i,2]])),"_",as.character(Crises[i,1]),"_change")
  names(Comparison_Crises)[(i*3)+1] = paste0(year(as.Date(Crises[[i,2]])),"_",as.character(Crises[i,1]),"_cumsum")
}
rm(i)
Comparison_Crises <- rbind.data.frame(c(0),Comparison_Crises)

Comparison_Outbreaks <- Reduce(function(x, y) merge(x, y, by = "Week", all=TRUE), outbreaks_list)
for (i in 1:nrow(Outbreaks)){
  names(Comparison_Outbreaks)[(i*3)-1] = paste0(year(as.Date(Outbreaks[[i,2]])),"_",as.character(Outbreaks[i,1]),"_volume")
  names(Comparison_Outbreaks)[i*3] = paste0(year(as.Date(Outbreaks[[i,2]])),"_",as.character(Outbreaks[i,1]),"_change")
  names(Comparison_Outbreaks)[(i*3)+1] = paste0(year(as.Date(Outbreaks[[i,2]])),"_",as.character(Outbreaks[i,1]),"_cumsum")
}
rm(i)
Comparison_Outbreaks <- rbind.data.frame(c(0),Comparison_Outbreaks)

#first_weeks <- as.integer(round(difftime(Sys.Date(),as.Date("2020-02-19"),units = c("weeks")))) + 2
first_weeks = 8

Comparison_Crises_Volume_All <- melt(Comparison_Crises[2:nrow(Comparison_Crises),c(1,seq(2, ncol(Comparison_Crises), by=3))],id.vars = "Week")
Comparison_Crises_Changes_All <- melt(Comparison_Crises[,c(1,seq(3, ncol(Comparison_Crises), by=3))],id.vars = "Week")
Comparison_Crises_Cumulative_All <- melt(Comparison_Crises[,seq(1, ncol(Comparison_Crises), by=3)],id.vars = "Week")

Comparison_Outbreaks_Volume_All <- melt(Comparison_Outbreaks[2:nrow(Comparison_Outbreaks),c(1,seq(2, ncol(Comparison_Outbreaks), by=3))],id.vars = "Week")
Comparison_Outbreaks_Changes_All <- melt(Comparison_Outbreaks[,c(1,seq(3, ncol(Comparison_Outbreaks), by=3))],id.vars = "Week")
Comparison_Outbreaks_Cumulative_All <- melt(Comparison_Outbreaks[,seq(1, ncol(Comparison_Outbreaks), by=3)],id.vars = "Week")

#Crises Plots
Crises_Changes_All <- ggplot(Comparison_Crises_Changes_All,aes(Week,value,colour=gsub("_change","",variable))) +
  geom_line() + 
  theme_ft_rc(plot_title_size = 14,
              axis_title_face = "bold", 
              axis_title_just = "m",
              axis_title_size = 10,
  )+
  ggtitle("Weekly Changes During Crises (SP500) - Complete Period")+
  labs(y="Change (%)")+
  labs(colour = "Crisis:")+
  annotate(geom="text", x=99, y=-27, label='r/ETFs_Europe', color='white', angle=0, fontface='bold', size=5, alpha=0.5) #Crises
#print(Crises_Changes_All)

Crises_Changes_Begin <- ggplot(Comparison_Crises_Changes_All,aes(Week,value,colour=gsub("_change","",variable))) +
  geom_line() + 
  theme_ft_rc(plot_title_size = 14,
              axis_title_face = "bold", 
              axis_title_just = "m",
              axis_title_size = 10,
  )+
  ggtitle(paste0("Weekly Changes During Crises (SP500) - First ",first_weeks, " Weeks"))+
  labs(y="Change (%)")+
  labs(colour = "Crisis:") +
  xlim(0, first_weeks)+
  annotate(geom="text", x=7.3, y=-40, label='r/ETFs_Europe', color='white', angle=0, fontface='bold', size=5, alpha=0.5) #Crises
#print(Crises_Changes_Begin)

Crises_Cumulative_All <- ggplot(Comparison_Crises_Cumulative_All,aes(Week,value,colour=gsub("_cumsum","",variable))) +
  geom_line() + 
  theme_ft_rc(plot_title_size = 14,
              axis_title_face = "bold", 
              axis_title_just = "m",
              axis_title_size = 10,
  )+
  ggtitle("Cumulative Changes During Crises (SP500) - Complete Period")+
  labs(y="Cumulative Change (%)")+
  labs(colour = "Crisis:")+
  annotate(geom="text", x=99, y=-60, label='r/ETFs_Europe', color='white', angle=0, fontface='bold', size=5, alpha=0.5) #Crises
#print(Crises_Cumulative_All)

Crises_Cumulative_Begin <- ggplot(Comparison_Crises_Cumulative_All,aes(Week,value,colour=gsub("_cumsum","",variable))) +
  geom_line() + 
  theme_ft_rc(plot_title_size = 14,
              axis_title_face = "bold", 
              axis_title_just = "m",
              axis_title_size = 10,
  )+
  ggtitle(paste0("Cumulative Changes During Crises (SP500) - First ",first_weeks, " Weeks"))+
  labs(y="Cumulative Change (%)")+
  labs(colour = "Crisis:") +
  xlim(0, first_weeks)+
  ylim(-34, 4)+
  annotate(geom="text", x=7.15, y=-34, label='r/ETFs_Europe', color='white', angle=0, fontface='bold', size=5, alpha=0.5) #Crises
#print(Crises_Crises_Cumulative_Begin)

Crises_Volume_All <- ggplot(Comparison_Crises_Volume_All,aes(Week,value,colour=gsub("_volume","",variable))) +
  geom_line() + 
  theme_ft_rc(plot_title_size = 14,
              axis_title_face = "bold", 
              axis_title_just = "m",
              axis_title_size = 10,
  )+
  ggtitle("Volume During Crises (SP500) - Complete Period")+
  labs(y="Volume in Billions - Log. Scale ")+
  labs(colour = "Crisis:")+
  scale_y_continuous(trans = log10_trans()) +
  annotate(geom="text", x=99, y=2, label='r/ETFs_Europe', color='white', angle=0, fontface='bold', size=5, alpha=0.5) #Crises
#print(Crises_Volume_All)

Crises_Volume_Begin <- ggplot(Comparison_Crises_Volume_All,aes(Week,value,colour=gsub("_volume","",variable))) +
  geom_line() + 
  theme_ft_rc(plot_title_size = 14,
              axis_title_face = "bold", 
              axis_title_just = "m",
              axis_title_size = 10,
  )+
  ggtitle(paste0("Volume During Crises (SP500) - First ",first_weeks, " Weeks"))+
  labs(y="Volume in Billions - Log. Scale ")+
  labs(colour = "Crisis:") +
  xlim(0, first_weeks)+
  scale_y_continuous(trans = log10_trans())+
  annotate(geom="text", x=7.3, y=2, label='r/ETFs_Europe', color='white', angle=0, fontface='bold', size=5, alpha=0.5) #Crises
#print(Crises_Volume_Begin)
  

#Outbreaks Plots (Only Complete Periods, no need of zoom in)
Outbreaks_Changes_All <- ggplot(Comparison_Outbreaks_Changes_All,aes(Week,value,colour=gsub("_change","",variable))) +
  geom_line() + 
  theme_ft_rc(plot_title_size = 14,
              axis_title_face = "bold", 
              axis_title_just = "m",
              axis_title_size = 10,
  )+
  ggtitle("Weekly Changes During Outbreaks (SP500)")+
  labs(y="Change (%)")+
  labs(colour = "Dip:")+
  annotate(geom="text", x=16, y=-15, label='r/ETFs_Europe', color='white', angle=0, fontface='bold', size=5, alpha=0.5) #Outbreaks
#print(Outbreaks_Changes_All)

Outbreaks_Cumulative_All <- ggplot(Comparison_Outbreaks_Cumulative_All,aes(Week,value,colour=gsub("_cumsum","",variable))) +
  geom_line() + 
  theme_ft_rc(plot_title_size = 14,
              axis_title_face = "bold", 
              axis_title_just = "m",
              axis_title_size = 10,
  )+
  ggtitle("Cumulative Changes During Outbreaks (SP500)")+
  labs(y="Cumulative Change (%)")+
  labs(colour = "Dip:")+
  annotate(geom="text", x=16, y=-33, label='r/ETFs_Europe', color='white', angle=0, fontface='bold', size=5, alpha=0.5) #Outbreaks
#print(Outbreaks_Cumulative_All)

Outbreaks_Volume_All <- ggplot(Comparison_Outbreaks_Volume_All,aes(Week,value,colour=gsub("_volume","",variable))) +
  geom_line() + 
  theme_ft_rc(plot_title_size = 14,
              axis_title_face = "bold", 
              axis_title_just = "m",
              axis_title_size = 10,
  )+
  ggtitle("Volume During Outbreaks (SP500)")+
  labs(y="Volume in Billions - Log. Scale ")+
  labs(colour = "Dip:")+
  scale_y_continuous(trans = log10_trans()) +
  annotate(geom="text", x=16, y=300, label='r/ETFs_Europe', color='white', angle=0, fontface='bold', size=5, alpha=0.5) #Outbreaks
#print(Outbreaks_Volume_All)

#Plots export
ggsave(paste0("plots/",Sys.Date(),"_Crises_Changes_All.jpeg"),plot = Crises_Changes_All,width = 14, height = 8, dpi = 600, units = c("in"), limitsize = FALSE)
ggsave(paste0("plots/",Sys.Date(),"_Crises_Changes_Begin.jpeg"),plot = Crises_Changes_Begin,width = 14, height = 8, dpi = 600, units = c("in"), limitsize = FALSE)
ggsave(paste0("plots/",Sys.Date(),"_Crises_Cumulative_All.jpeg"),plot = Crises_Cumulative_All,width = 14, height = 8, dpi = 600, units = c("in"), limitsize = FALSE)
ggsave(paste0("plots/",Sys.Date(),"_Crises_Cumulative_Begin.jpeg"),plot = Crises_Cumulative_Begin,width = 14, height = 8, dpi = 600, units = c("in"), limitsize = FALSE)
ggsave(paste0("plots/",Sys.Date(),"_Crises_Volume_All.jpeg"),plot = Crises_Volume_All,width = 14, height = 8, dpi = 600, units = c("in"), limitsize = FALSE)
ggsave(paste0("plots/",Sys.Date(),"_Crises_Volume_Begin.jpeg"),plot = Crises_Volume_Begin,width = 14, height = 8, dpi = 600, units = c("in"), limitsize = FALSE)

ggsave(paste0("plots/",Sys.Date(),"_Outbreaks_Changes_All.jpeg"),plot = Outbreaks_Changes_All,width = 14, height = 8, dpi = 600, units = c("in"), limitsize = FALSE)
ggsave(paste0("plots/",Sys.Date(),"_Outbreaks_Cumulative_All.jpeg"),plot = Outbreaks_Cumulative_All,width = 14, height = 8, dpi = 600, units = c("in"), limitsize = FALSE)
ggsave(paste0("plots/",Sys.Date(),"_Outbreaks_Volume_All.jpeg"),plot = Outbreaks_Volume_All,width = 14, height = 8, dpi = 600, units = c("in"), limitsize = FALSE)

dev.new()
multiplot(Crises_Changes_All, Crises_Cumulative_All, Crises_Volume_All)
dev.new()
multiplot(Crises_Changes_Begin, Crises_Cumulative_Begin, Crises_Volume_Begin) 
dev.new()
multiplot(Outbreaks_Changes_All, Outbreaks_Cumulative_All, Outbreaks_Volume_All)
