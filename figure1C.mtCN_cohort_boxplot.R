rm(list=ls())#clear Global Environment
# setwd("D:/桌面/散点+箱线图+小提琴图+辅助线+显著性")
# Load packages
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(ggsignif) # Significance Brackets for 'ggplot2'
library(gghalves) # Compose Half-Half Plots Using Your Favourite Geoms
library(dplyr)
library(patchwork)
library(readxl)
# Load data
# df <- read.table("data.txt",header = 1)
# df$group <- factor(df$group,levels = c("A","B","C","D"))
dfID1020 <- read_excel("D:/biosoft/1000thal/千人/ID对应(1020+409)2023.12.22.xlsx", sheet = "1020")
dfID409 <- read_excel("D:/biosoft/1000thal/千人/ID对应(1020+409)2023.12.22.xlsx", sheet = "409")

phe_child <- readxl::read_excel("D:/biosoft/1000thal/千人/RNO.1 Basic statistics of 1020 β-thalassemia patients.xlsx", sheet = "Sheet1") %>% 
  select(ID,Clinical_staging,Transfusion_Dependence)


mtCN1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\1020mtDNA_out\\1020.mtCN_mean.tsv", header=T,sep ="\t") %>% 
  mutate(group = "Thalassemia")%>% 
  select(MT_mean_coverage,NUC_mean_coverage,mtCN_mean,group) 



# %>% 
#   left_join(dfID1020,by=c("ID"="HID")) %>% 
#   left_join(phe_child,by=c("ID.y"="ID")) %>% 
#   # filter(Clinical_staging=="TM") %>% 
#   select(MT_mean_coverage,mtCN_mean,group)

mtCN409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\1020mtDNA_out\\409.mtCN_mean.tsv", header=T,sep ="\t") %>% 
  mutate(group = "Carriers") %>% 
  select(MT_mean_coverage,NUC_mean_coverage,mtCN_mean,group)


mtCN58 <- read_excel("D:/biosoft/1000thal/mtDNA/58norm_people/finalout/58mtCN.xlsx") %>% 
  mutate(group = "Healthy") %>% 
  select(MT_mean_coverage,NUC_mean_coverage,mtCN_mean,group)




input <- rbind(mtCN1020,mtCN409,mtCN58)
input$group <- factor(input$group, 
                      levels = c("Thalassemia", "Carriers", "Healthy"))
### 绘图 ###
P1 <- ggplot(input,aes(x=group,y=mtCN_mean))+
  
  geom_half_violin(aes(fill=group),position=position_nudge(x=0.12),side="r",width=0.2,color=NA)+
  geom_half_boxplot(aes(fill=group),position=position_nudge(x=-0.12),,width=0.08,outlier.color=NA,side="l")+
  geom_jitter(aes(color=group),shape=16,size=3,width=0.1,alpha=0.5)+
  
  
  
  geom_hline(yintercept = 130, linetype = 2, color = "red",linewidth=0.8)+
  geom_hline(yintercept = 295, linetype = 2, color = "red",linewidth=0.8)+
  geom_signif(comparisons = list(c("Thalassemia","Carriers"),c("Thalassemia","Healthy")),
              map_signif_level = T, 
              test = t.test, 
              y_position = c(600,700),
              tip_length = c(0.06,0.06),
              size=0.5,color="black",textsize = 7)+
  # scale_y_continuous(limits = c(-20,140),breaks = c(0,40,80,120))+
  theme_bw()+
  theme(
    legend.justification = c(1, 1),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    axis.text.x = element_text(color = "black", size = 12, face = 'bold'),
    axis.title.x = element_text(size = 15, face = 'bold'),
    axis.text.y = element_text(color = "black", size = 12, face = 'bold'),
    axis.title.y = element_text(size = 15, face = 'bold'),
    axis.line = element_line(colour = "black", size=0.7)
  ) +
  # theme(panel.grid = element_blank(),
  #       panel.border = element_rect(size = 0.5),
  #       axis.text.x = element_text(color = "black", size = 12,face = 'bold'),
  #       axis.title.x = element_text(size = 15,face = 'bold'),
  #       axis.text.y = element_text(color = "black",size = 12,face = 'bold'),
  #       axis.title.y = element_text(size = 15,face = 'bold'),
  #       legend.position = "none",
  #       axis.ticks = element_line(color="black",linewidth = 0.5)
  # )+
  labs(x=NULL,y="mtCN,raw")+
  scale_fill_manual(
    values = c("Thalassemia" = "#e95f5c", 
               "Carriers" = "#EDBE6C", 
               "Healthy" = "#4DAB8F")
  ) +
  scale_color_manual(
    values = c("Thalassemia" = "#e95f5c", 
               "Carriers" = "#EDBE6C", 
               "Healthy" = "#4DAB8F")
  )
P1
# ggsave(P1,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figur4A_1.mtCN across cohorts.pdf",
# device = pdf,width = 8,height = 6,dpi = 600)

# require(viridis)#色彩Adjust的R包
# require(hrbrthemes)#Load主题包
P11 <- ggplot(input,aes(x=group,y=MT_mean_coverage))+
  
  geom_half_violin(aes(fill=group),position=position_nudge(x=0.12),side="r",width=0.2,color=NA)+
  geom_half_boxplot(aes(fill=group),position=position_nudge(x=-0.12),,width=0.08,outlier.color=NA,side="l")+
  geom_jitter(aes(fill=group),shape=21,size=3,width=0.1,alpha=0.5)+
  
  
  
  geom_hline(yintercept = 8000, linetype = 2, color = "red",linewidth=1)+
  geom_hline(yintercept = 2000, linetype = 2, color = "red",linewidth=1)+
  geom_signif(comparisons = list(c("Thal","Carrier")),
              map_signif_level = T, 
              test = t.test, 
              y_position = c(20000),
              tip_length = c(0.06,0.06),
              size=1,color="black",textsize = 7)+
  # scale_y_continuous(limits = c(-20,140),breaks = c(0,40,80,120))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1.5),
        axis.text.x = element_text(color = "black", size = 12,face = 'bold'),
        axis.title.x = element_text(size = 15,face = 'bold'),
        axis.text.y = element_text(color = "black",size = 12,face = 'bold'),
        axis.title.y = element_text(size = 15,face = 'bold'),
        legend.position = "none",
        # axis.ticks = element_line(color="black",linewidth = 1.2)
  )+
  labs(x=NULL,y="MT mean coverage")+
  scale_fill_manual(values = c("#79ceb8","#e95f5c","#EDBE6C"))
P11
# ggsave(P11,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figur4A_1.MT_mean_coverage_across_cohorts.pdf",
#        device = pdf,width = 8,height = 6,dpi = 600)


density_plot2 <- ggplot() +
  geom_density(data = mtCN1020, aes(x = mtCN_mean, color = "Thalassemia"), linewidth=1.4) +
  geom_density(data = mtCN409, aes(x = mtCN_mean, color = "Carriers"), linewidth=1.4) +
  geom_density(data = mtCN58, aes(x = mtCN_mean, color = "Healthy"), linewidth=1.4) +
  labs(title = NULL, x = "mtCN,raw", y = "density") +
  theme_bw() +
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    legend.text = element_text(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    axis.text.x = element_text(color = "black", size = 12, face = 'bold'),
    axis.title.x = element_text(size = 15, face = 'bold'),
    axis.text.y = element_text(color = "black", size = 12, face = 'bold'),
    axis.title.y = element_text(size = 15, face = 'bold'),
    axis.line = element_line(colour = "black", size=0.7)
  ) +
  guides(color = guide_legend(title = NULL)) +
  scale_y_continuous(expand = c(0,0), labels = scales::percent) +
  scale_x_continuous(limits = c(0, 600)) +
  scale_color_manual(
    values = c("Thalassemia" = "#e95f5c", 
               "Carriers" = "#EDBE6C", 
               "Healthy" = "#4DAB8F")
  )

density_plot2



P12 <- ggplot(input,aes(x=group,y=NUC_mean_coverage))+
  
  geom_half_violin(aes(fill=group),position=position_nudge(x=0.12),side="r",width=0.2,color=NA)+
  geom_half_boxplot(aes(fill=group),position=position_nudge(x=-0.12),,width=0.08,outlier.color=NA,side="l")+
  geom_jitter(aes(fill=group),shape=21,size=3,width=0.1,alpha=0.5)+
  
  
  
  geom_hline(yintercept = 80, linetype = 2, color = "red",linewidth=1)+
  geom_hline(yintercept = 30, linetype = 2, color = "red",linewidth=1)+
  geom_signif(comparisons = list(c("Thal","Carrier")),
              map_signif_level = T, 
              test = t.test, 
              y_position = c(120),
              tip_length = c(0.06,0.06),
              size=1,color="black",textsize = 7)+
  # scale_y_continuous(limits = c(-20,140),breaks = c(0,40,80,120))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1.5),
        axis.text.x = element_text(color = "black", size = 12,face = 'bold'),
        axis.title.x = element_text(size = 15,face = 'bold'),
        axis.text.y = element_text(color = "black",size = 12,face = 'bold'),
        axis.title.y = element_text(size = 15,face = 'bold'),
        legend.position = "none",
        # axis.ticks = element_line(color="black",linewidth = 1.2)
  )+
  labs(x=NULL,y="NUC mean coverage")+
  scale_fill_manual(values = c("#79ceb8","#e95f5c"))
P12
# ggsave(P12,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figur4A_1.NUC_mean_coverage_across_cohorts.pdf",
#        device = pdf,width = 8,height = 6,dpi = 600)

P2 <- ggplot(input, aes(x = mtCN_mean, fill = group)) +
  geom_density(alpha = 0.9)+
  labs(title = NULL, x = "mean mtCN", y = "Density") +
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1.5),
        axis.text.x = element_text(color = "black", size = 12,face = 'bold'),
        axis.title.x = element_text(size = 15,face = 'bold'),
        axis.text.y = element_text(color = "black",size = 12,face = 'bold'),
        axis.title.y = element_text(size = 15,face = 'bold'),
        # legend.position = "none",
        # axis.ticks = element_line(color="black",linewidth = 1.2)
        )+
  # scale_fill_manual(values = c("#79ceb8","#e95f5c"))
  scale_fill_manual(values = c("#79ceb8","#e95f5c"),name = NULL)+
  scale_y_discrete(expand = expansion(add = c(0, 0)))+ # 使用discrete scale
  theme(legend.position = c(0.8,0.9),legend.background = element_rect(fill = 'white', colour = 'black'))  #Modify图例Position
P2
#

# Composite plot
P3 <- P1+P2

P3

P4 <- P1+density_plot2
P4
# ggsave(P4,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure4A.mtCN across cohorts.pdf",
#        device = pdf,width = 8,height = 6,dpi = 600)





