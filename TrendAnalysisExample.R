library(tidyverse)
library(ggplot2)
library(png)
library(grid)
library(Kendall)
library(ggimage)
library(cowplot)


# Change the path below to your local path
sdt<- read.csv("C:/R/DataGroup/TrendExample/sdtTrend.csv") 
  
  
SDTimage = "C:/R/DataGroup/TrendExample/SDT2.png"
  


# Full Record:


# Run Mann-Kendall Test

mk_full<-MannKendall(sdt$SD_Mean)


## Save trend test results as variables to use in plot below 


# tau and p variables

mktau_full = round(mk_full$tau[1],3)
mk_p_full = round(mk_full$sl[1],3)

    ## This is in case p is very low, will show up as "<0.001" instead of some ugly decimal)

    mk_p_full<-ifelse(mk_p_full<0.001,"<0.001",mk_p_full)

# create trend test result statement: 

sig_full<- if_else(mktau_full > .5 & mk_p_full< 0.05, "Water clarity is significantly increasing",
                   if_else(mktau_full < -0.5 & mk_p_full<0.05, "Water clarity is significantly decreasing",
                           if_else(mktau_full < 0.5 & mktau_full >0 & mk_p_full<0.05, "Water clarity is significantly increasing",
                                   if_else(mktau_full < 0 & mktau_full >-.5 & mk_p_full<0.05, "Water clarity is significantly decreasing",
                                           "Water clarity is stable or too variable to detect a trend"))))






plot_full =
  ggplot()+
  geom_point(data=subset(sdt, !is.na(SD_Mean)),aes(x=Year,y=SD_Mean),size=0)+
  scale_y_reverse(lim=c(8,0))+
  theme_bw()+
  theme(panel.background = element_rect(color = "black", size=0.5))+
  scale_x_continuous(position = "top", limits=c(1970,2023))+
  labs(title = "Full Data Record")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.minor = element_blank())+
  geom_text(aes( x=1970, y=.35, label=paste0("Trend Test Result (", expression("\u03C4"), " = ",
            mktau_full,", ", "p = ",mk_p_full,"):")),          
            color="blue", 
            size=2.5 , # fontface="italic", 
            hjust = 0)+
  geom_text(aes( x=1970, y=1.1, label=sig_full),                  
            color="blue", 
            size=3 ,  fontface="bold", 
            hjust = 0)+
  ylab("Depth (m)")+
  xlab("Year")+
  geom_image(data = sdt, aes(x = Year, y = SD_Mean, image=SDTimage), size=.06)+
  theme(legend.position = "bottom", 
        plot.caption = element_text(vjust = -1,lineheight = 3,size = 9,color = "grey30", hjust = 0), 
        plot.margin=unit(c(1,1,1,1), "cm"))+
  theme(plot.margin = margin(0.2, 0.3, 0.5, 0.2, "cm"),
        plot.title = element_text(size=11),
        axis.title=element_text(size=8,face="bold"))+  #top, right, bottom, left.
  geom_smooth(data = sdt,aes(Year, SD_Mean),color = "red", se = FALSE)#5), span = .2



## optionally save plot as separate file 
# ggsave("C:/R/DataGroup/TrendExample/Full.png",plot_full, width = 4, height = 2.5, units = "in")



###########################################################

# Trend analysis for 1970-2000 only

### MK: 

sdt_1970_2000 <- sdt %>% filter(Year <=2000) 



mk_7000<-MannKendall(sdt_1970_2000$SD_Mean)
mktau_7000 = round(mk_7000$tau[1],3)
mk_p_7000 = round(mk_7000$sl[1],3)

mk_p_7000<-ifelse(mk_p_7000<0.001,"<0.001",mk_p_7000)


# Trend Results:
sig_7000<- if_else(mktau_7000 > .5 & mk_p_7000< 0.05, "Water clarity is significantly increasing",
                   if_else(mktau_7000 < -0.5 & mk_p_7000<0.05, "Water clarity is significantly decreasing",
                           if_else(mktau_7000 < 0.5 & mktau_7000 >0 & mk_p_7000<0.05, "Water clarity is significantly increasing",
                                   if_else(mktau_7000 < 0 & mktau_7000 >-.5 & mk_p_7000<0.05, "Water clarity is significantly decreasing",
                                           "Water clarity is stable or too variable to detect a trend"))))



plot_1970_2000 =
  ggplot()+
  geom_point(data=subset(sdt_1970_2000, !is.na(SD_Mean)),aes(x=Year,y=SD_Mean),size=0)+
  scale_y_reverse(lim=c(8,0))+
  theme_bw()+
  theme(panel.background = element_rect(color = "black", size=0.5))+
  scale_x_continuous(position = "top", limits=c(1970,2023))+
  labs(title = "1970-2000")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.minor = element_blank())+
  geom_text(aes( x=1970, y=.35, label=paste0("Trend Test Result (", expression("\u03C4"), " = ",mktau_7000,", ", 
                                             "p = ",mk_p_7000,"):")),         , 
            color="blue", size=2.5 ,   hjust = 0)+ # fontface="italic", 
  
  geom_text(aes( x=1970, y=1.1, label=sig_7000), color="blue", size=3 ,  fontface="bold",hjust = 0)+
  ylab("Depth (m)")+    xlab("Year")+
  geom_image(data = sdt_1970_2000, aes(x = Year, y = SD_Mean, image=SDTimage), size=.06)+
  theme(legend.position = "bottom", 
        plot.caption = element_text(vjust = -1,lineheight = 3,size = 9,color = "grey30", hjust = 0), 
        plot.margin=unit(c(1,1,1,1), "cm"))+
  theme(plot.margin = margin(0.2, 0.2, 0.5, 0.2, "cm"),
        plot.title = element_text(size=11),
        axis.title=element_text(size=8,face="bold"))+  #top, right, bottom, left.
  geom_smooth(data = sdt_1970_2000,aes(Year, SD_Mean),color = "red", se = FALSE)#5), span = .2




# ggsave("C:/R/DataGroup/TrendExample/1970-2000.png", plot_1970_2000, width = 4, height = 2.5, units = "in")





###########################################################

# Trend analysis for 2000-2022


sdt_2000_2022 <- sdt %>% filter(Year >2000) 

### MK: 


mk_0022<-MannKendall(sdt_2000_2022$SD_Mean)
mktau_0022 = round(mk_0022$tau[1],3)
mk_p_0022 = round(mk_0022$sl[1],3)

mk_p_0022<-ifelse(mk_p_0022<0.001,"<0.001",mk_p_0022)


# Trend Results:
sig_0022<- if_else(mktau_0022 > .5 & mk_p_0022< 0.05, "Water clarity is significantly increasing",
                   if_else(mktau_0022 < -0.5 & mk_p_0022<0.05, "Water clarity is significantly decreasing",
                           if_else(mktau_0022 < 0.5 & mktau_0022 >0 & mk_p_0022<0.05, "Water clarity is significantly increasing",
                                   if_else(mktau_0022 < 0 & mktau_0022 >-.5 & mk_p_0022<0.05, "Water clarity is significantly decreasing",
                                           "Water clarity is stable or too variable to detect a trend"))))



plot_2000_2022 =
  ggplot()+
  geom_point(data=subset(sdt_2000_2022, !is.na(SD_Mean)),aes(x=Year,y=SD_Mean),size=0)+
  scale_y_reverse(lim=c(8,0))+
  theme_bw()+
  theme(panel.background = element_rect(color = "black", size=0.5))+
  scale_x_continuous(position = "top", limits=c(1970,2023))+
  labs(title = "2000-2022")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.minor = element_blank())+
  geom_text(aes( x=1970, y=.35, label=paste0("Trend Test Result (", expression("\u03C4"), " = ",mktau_0022,", ", 
                                             "p = ",mk_p_0022,"):")),         , 
            color="blue", size=2.5 ,   hjust = 0)+ # fontface="italic", 
  
  geom_text(aes( x=1970, y=1.1, label=sig_0022), color="blue", size=3 ,  fontface="bold",hjust = 0)+
  ylab("Depth (m)")+    xlab("Year")+
  geom_image(data = sdt_2000_2022, aes(x = Year, y = SD_Mean, image=SDTimage), size=.06)+
  theme(legend.position = "bottom", 
        plot.caption = element_text(vjust = -1,lineheight = 3,size = 9,color = "grey30", hjust = 0), 
        plot.margin=unit(c(1,1,1,1), "cm"))+
  theme(plot.margin = margin(0.2, 0.3, 0.5, 0.2, "cm"),
        plot.title = element_text(size=11),
        axis.title=element_text(size=8,face="bold"))+  #top, right, bottom, left.
  geom_smooth(data = sdt_2000_2022,aes(Year, SD_Mean),color = "red", se = FALSE)#5), span = .2




# ggsave("C:/R/DataGroup/TrendExample/2000-2022.png", plot_2000_2022,width = 4, height = 2.5, units = "in")










###########################################################

# 2001-2012


sdt_2001_2012 <- sdt %>% filter(Year >2000 & Year <=2012) 


### MK: 


mk_0112<-MannKendall(sdt_2001_2012$SD_Mean)
mktau_0112 = round(mk_0112$tau[1],3)
mk_p_0112 = round(mk_0112$sl[1],3)

mk_p_0112<-ifelse(mk_p_0112<0.001,"<0.001",mk_p_0112)


# Trend Results:
sig_0112<- if_else(mktau_0112 > .5 & mk_p_0112< 0.05, "Water clarity is significantly increasing",
                   if_else(mktau_0112 < -0.5 & mk_p_0112<0.05, "Water clarity is significantly decreasing",
                           if_else(mktau_0112 < 0.5 & mktau_0112 >0 & mk_p_0112<0.05, "Water clarity is significantly increasing",
                                   if_else(mktau_0112 < 0 & mktau_0112 >-.5 & mk_p_0112<0.05, "Water clarity is significantly decreasing",
                                           "Water clarity is stable or too variable to detect a trend"))))



plot_2001_2012 =
  ggplot()+
  geom_point(data=subset(sdt_2001_2012, !is.na(SD_Mean)),aes(x=Year,y=SD_Mean),size=0)+
  scale_y_reverse(lim=c(8,0))+
  theme_bw()+
  theme(panel.background = element_rect(color = "black", size=0.5))+
  scale_x_continuous(position = "top", limits=c(1970,2023))+
  labs(title = "2001-2012")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.minor = element_blank())+
  geom_text(aes( x=1970, y=.35, label=paste0("Trend Test Result (", expression("\u03C4"), " = ",mktau_0112,", ", 
                                             "p = ",mk_p_0112,"):")),         , 
            color="blue", size=2.5 ,   hjust = 0)+ # fontface="italic", 
  
  geom_text(aes( x=1970, y=1.1, label=sig_0112), color="blue", size=3 ,  fontface="bold",hjust = 0)+
  ylab("Depth (m)")+    xlab("Year")+
  geom_image(data = sdt_2001_2012, aes(x = Year, y = SD_Mean, image=SDTimage), size=.06)+
  theme(legend.position = "bottom", 
        plot.caption = element_text(vjust = -1,lineheight = 3,size = 9,color = "grey30", hjust = 0), 
        plot.margin=unit(c(1,1,1,1), "cm"))+
  theme(plot.margin = margin(0.2, 0.2, 0.5, 0.2, "cm"),
        plot.title = element_text(size=11),
        axis.title=element_text(size=8,face="bold"))+  #top, right, bottom, left.
  geom_smooth(data = sdt_2001_2012,aes(Year, SD_Mean),color = "red", se = FALSE)#5), span = .2




# ggsave("C:/R/DataGroup/TrendExample/2001-2012.png",plot_2001_2012, width = 4, height = 2.5, units = "in")











###########################################################

# Random

sdt_rand <- sdt %>% 
  slice_sample(prop = .5)  # selects random portion of dataset based on decimal (0.5 = 50%, 0.25 = 25%, etc.)


### MK: 


mk_rand<-MannKendall(sdt_rand$SD_Mean)
mktau_rand = round(mk_rand$tau[1],3)
mk_p_rand = round(mk_rand$sl[1],3)

sdt_2001_2012 <- sdt %>% filter(Year >2000 & Year <=2012) 


# Trend Results:
sig_rand<- if_else(mktau_rand > .5 & mk_p_rand< 0.05, "Water clarity is significantly increasing",
                   if_else(mktau_rand < -0.5 & mk_p_rand<0.05, "Water clarity is significantly decreasing",
                           if_else(mktau_rand < 0.5 & mktau_rand >0 & mk_p_rand<0.05, "Water clarity is significantly increasing",
                                   if_else(mktau_rand < 0 & mktau_rand >-.5 & mk_p_rand<0.05, "Water clarity is significantly decreasing",
                                           "Water clarity is stable or too variable to detect a trend"))))




plot_rand50=
  ggplot()+
  geom_point(data=subset(sdt_rand, !is.na(SD_Mean)),aes(x=Year,y=SD_Mean),size=0)+
  scale_y_reverse(lim=c(8,0))+
  theme_bw()+
  theme(panel.background = element_rect(color = "black", size=0.5))+
  scale_x_continuous(position = "top", limits=c(1970,2023))+
  labs(title = "Random Sub-set of Sample Years (50%)")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.minor = element_blank())+
  geom_text(aes( x=1970, y=.35, label=paste0("Trend Test Result (", expression("\u03C4"), " = ",mktau_rand,", ", 
                                             "p = ",mk_p_rand,"):")),         , 
            color="blue", size=2.5 ,   hjust = 0)+ # fontface="italic", 
  
  geom_text(aes( x=1970, y=1.1, label=sig_rand), color="blue", size=3 ,  fontface="bold",hjust = 0)+
  ylab("Depth (m)")+    xlab("Year")+
  geom_image(data = sdt_rand, aes(x = Year, y = SD_Mean, image=SDTimage), size=.06)+
  theme(legend.position = "bottom", 
        plot.caption = element_text(vjust = -1,lineheight = 3,size = 9,color = "grey30", hjust = 0), 
        plot.margin=unit(c(1,1,1,1), "cm"))+
  theme(plot.margin = margin(0.2, 0.3, 0.5, 0.2, "cm"),
        plot.title = element_text(size=11),
        axis.title=element_text(size=8,face="bold"))+  #top, right, bottom, left.
  geom_smooth(data = sdt_rand,aes(Year, SD_Mean),color = "red", se = FALSE)#5), span = .2




# ggsave("C:/R/DataGroup/TrendExample/Random50.png", plot_rand50, width = 4, height = 2.5, units = "in")









###########################################################

# Random 25%

sdt_rand25 <- sdt %>% 
  slice_sample(prop = 0.25)


### MK: 


mk_rand25<-MannKendall(sdt_rand25$SD_Mean)
mktau_rand25 = round(mk_rand25$tau[1],3)
mk_p_rand25 = round(mk_rand25$sl[1],3)

mk_p_rand25<-ifelse(mk_p_rand25<0.001,"<0.001",mk_p_rand25)



# Trend Results:
sig_rand25<- if_else(mktau_rand25 > .5 & mk_p_rand25< 0.05, "Water clarity is significantly increasing",
                     if_else(mktau_rand25 < -0.5 & mk_p_rand25<0.05, "Water clarity is significantly decreasing",
                             if_else(mktau_rand25 < 0.5 & mktau_rand25 >0 & mk_p_rand25<0.05, "Water clarity is significantly increasing",
                                     if_else(mktau_rand25 < 0 & mktau_rand25 >-.5 & mk_p_rand25<0.05, "Water clarity is significantly decreasing",
                                             "Water clarity is stable or too variable to detect a trend"))))




plot_rand25=
  ggplot()+
  geom_point(data=subset(sdt_rand25, !is.na(SD_Mean)),aes(x=Year,y=SD_Mean),size=0)+
  scale_y_reverse(lim=c(8,0))+
  theme_bw()+
  theme(panel.background = element_rect(color = "black", size=0.5))+
  scale_x_continuous(position = "top", limits=c(1970,2023))+
  labs(title = "Random Sub-set of Sample Years (25%)")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.minor = element_blank())+
  geom_text(aes( x=1970, y=.35, label=paste0("Trend Test Result (", expression("\u03C4"), " = ",mktau_rand25,", ", 
                                             "p = ",mk_p_rand25,"):")),         , 
            color="blue", size=2.5 ,   hjust = 0)+ # fontface="italic", 
  
  geom_text(aes( x=1970, y=1.1, label=sig_rand25), color="blue", size=3 ,  fontface="bold",hjust = 0)+
  ylab("Depth (m)")+    xlab("Year")+
  geom_image(data = sdt_rand25, aes(x = Year, y = SD_Mean, image=SDTimage), size=.06)+
  theme(legend.position = "bottom", 
        plot.caption = element_text(vjust = -1,lineheight = 3,size = 9,color = "grey30", hjust = 0), 
        plot.margin=unit(c(1,1,1,1), "cm"))+
  theme(plot.margin = margin(0.2, 0.2, 0.5, 0.2, "cm"),
        plot.title = element_text(size=11),
        axis.title=element_text(size=8,face="bold"))+  #top, right, bottom, left.
  geom_smooth(data = sdt_rand25,aes(Year, SD_Mean),color = "red", se = FALSE)#5), span = .2




# ggsave("C:/R/DataGroup/TrendExample/Random25.png", plot_rand25, width = 4, height = 2.5, units = "in")


################################################################################################

# Create Plot:

## This saves a text label as a graphic for a master title for the grouped plot created below.

title <- ggdraw() + 
  draw_label("Secchi Disk Transparency: Actual Lake, ME",fontface = 'bold', x = 0, hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 7))


# Create a plot grid (with cowplot::plot_grid) of all plots, 
# arrange into # of columns you want and assign labels if you want to:

plotg<- plot_grid(plot_full, plot_1970_2000, plot_2000_2022,
                  plot_2001_2012, plot_rand50, plot_rand25, ncol = 2, 
                  labels = c("A","B","C","D","E","F"))


# Create final image with title and plot grid. 
cow_final<- plot_grid(title, plotg, ncol = 1 , rel_heights = c(0.1, 1)) 

#view final plot:
cow_final


# save final image to file
save_plot("C:/R/DataGroup/TrendExample/TrendPlotGrid.png", cow_final, base_height = 8, base_width = 8)

