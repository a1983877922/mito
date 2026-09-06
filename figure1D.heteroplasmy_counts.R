rm(list=ls())
library(dplyr)
library(tidyverse)
library(ggrepel)
library(patchwork)
library(readxl)

## Load cached, skip recomputation
# load("D:\\biosoft\\1000thal\\mtDNA\\R script/figure1D.counts_per_heteroplasmy.RData")

##
hl1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020.mt.ano.filter.hl2zero") %>% 
  filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
  mutate(V3=paste(V2,V4,V5,sep = "_")) %>%
  filter(rowSums(. >= 0.05) > 0) %>%
  distinct(.keep_all = TRUE,V3)

hl409<- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\409/409.mt.ano.filter.hl2zero") %>% 
  filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
  mutate(V3=paste(V2,V4,V5,sep = "_")) %>%
  filter(rowSums(. >= 0.05) > 0) %>%
  distinct(.keep_all = TRUE,V3)


hl58<- read.table("D:\\biosoft\\1000thal\\mtDNA\\58norm_people\\finalout/58norm.mt.filter.hl2zero") %>% 
  filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
  mutate(V3=paste(V2,V4,V5,sep = "_")) %>%
  filter(rowSums(. >= 0.05) > 0) %>%
  distinct(.keep_all = TRUE,V3)


# #将大于等于0.95的值变为0
# hl1020[ , 7:1026][hl1020[ , 7:1026] <= 0.05] <- 0

hl1020_sub <- subset(hl1020,select=-c(1:6))
hl409_sub <- subset(hl409,select=-c(1:6))
hl58_sub <- subset(hl58,select=-c(1:5))

long_column <- unlist(hl1020_sub)
long_column2 <- unlist(hl409_sub)
long_column3 <- unlist(hl58_sub)

hl1020_long <- data.frame(Column = long_column ) %>% 
  filter(Column != 0) %>% 
  mutate(group="Thalassemia")

hl409_long <- data.frame(Column = long_column2 ) %>% 
  filter(Column != 0) %>% 
  mutate(group="Carriers")

hl58_long <- data.frame(Column = long_column3 ) %>% 
  filter(Column != 0) %>% 
  mutate(group="Healthy")

input <- rbind(hl1020_long,hl409_long,hl58_long)
input$group <- factor(input$group, 
                      levels = c("Thalassemia", "Carriers", "Healthy"))




input_percent <- input %>%
  count(Column, group) %>%
  group_by(group) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup()

input <- input %>%
  mutate(Column2 = round(Column, 3)) %>%
  count(Column2, group) %>%
  group_by(group) %>%
  mutate(per = n / case_when(
    group == "Thalassemia" ~ 1020,
    group == "Carriers" ~ 409,
    group == "Healthy" ~ 58
  ) * 100) %>%
  ungroup()
    
    
# test1 <- input_percent %>% 
#   filter(group=="Healthy")
# 
# 
# sum(test1$percent)



p1 <- ggplot(input_percent, aes(x = Column, y = percent, fill = group, color = group)) +
  geom_bar(stat = "identity", width = 0.001) +
  scale_y_continuous(limits = c(0, 6), 
                     breaks = seq(0, 6, by = 1)) +
  scale_fill_manual(values = c("Thalassemia" = "#e95f5c", 
                               "Carriers" = "#EDBE6C", 
                               "Healthy" = "#4DAB8F")) +
  scale_color_manual(values = c("Thalassemia" = "#e95f5c", 
                                "Carriers" = "#EDBE6C", 
                                "Healthy" = "#4DAB8F")) +
  labs(x = "Heteroplasmy", y = "Percentage (%)", fill = "Group", color = "Group") +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 12, face = 'bold'),
    axis.title.x = element_text(size = 15, face = 'bold'),
    axis.text.y = element_text(size = 12, face = 'bold'),
    axis.title.y = element_text(size = 15, face = 'bold'),
    axis.line = element_line(linewidth = 0.5, colour = "black"),
    legend.direction = "vertical",
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = "white", color = "black"))
p1


pp1 <- ggplot(input, aes(x = Column2, y = per, fill = group, color = group)) +
  geom_bar(stat = "identity", width = 0.001) +
  # scale_y_continuous(limits = c(0, 6), 
  #                    breaks = seq(0, 6, by = 1)) +
  scale_fill_manual(values = c("Thalassemia" = "#e95f5c", 
                               "Carriers" = "#EDBE6C", 
                               "Healthy" = "#4DAB8F")) +
  scale_color_manual(values = c("Thalassemia" = "#e95f5c", 
                                "Carriers" = "#EDBE6C", 
                                "Healthy" = "#4DAB8F")) +
  labs(x = "Heteroplasmy Levels", y = "mean Counts ", fill = "Group", color = "Group") +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 12, face = 'bold'),
    axis.title.x = element_text(size = 15, face = 'bold'),
    axis.text.y = element_text(size = 12, face = 'bold'),
    axis.title.y = element_text(size = 15, face = 'bold'),
    axis.line = element_line(linewidth = 0.5, colour = "black"),
    legend.direction = "vertical",
    legend.position = c(0.93, 0.93),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = "white", color = "black"))
pp1
# p1 <- ggplot(hl1020_long, aes(x = Column)) +
#   geom_bar(color = "#50AC93", fill="#50AC93",width = 0.001) +  # Set柱子颜色and边框颜色
#   scale_y_continuous(limits = c(0, 4000), breaks = seq(0, 4000, by = 1000)) + 
#   labs(x = "Heteroplasmy", y = "Counts") +  # Add标题and轴标签
# theme_classic() +  # 使用简洁主题
#   # scale_x_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, by = 0.05)) +  # 限制 x 轴范围
#   theme(
#     # axis.ticks.length.x = unit(0.03,'cm'),
#     # axis.ticks.y = element_blank(),
#     axis.text.x = element_text(size=12,face = 'bold'),
#     axis.title.x = element_text(size = 15,face = 'bold'),
#     axis.text.y = element_text(size = 12,face = 'bold'),
#     axis.title.y = element_text(size = 15,face = 'bold'),
#     axis.line = element_line(linewidth=0.5, colour = "black"),
#     # panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"),
#     legend.direction = "vertical")
# p1


ppp <-  p1 + 
  scale_x_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, by = 0.05)) +  # 限制 x 轴范围
  # xlim(0,0.1,breaks = seq(0, 0.1, by = 0.05)) +  
  theme(legend.position = 'none',
        panel.border = element_rect(fill=NA,color="black", linewidth=0.5, linetype="solid")) +
  labs(title = "",x = "",y = "") 

ppp



ppp2 <-  pp1 + 
  scale_x_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, by = 0.05)) +  # 限制 x 轴范围
  # xlim(0,0.1,breaks = seq(0, 0.1, by = 0.05)) +  
  theme(legend.position = 'none',
        panel.border = element_rect(fill=NA,color="black", linewidth=0.5, linetype="solid")) +
  labs(title = "",x = "",y = "") 

ppp2
# p2 <- ggplot(input, aes(x = Column, fill = group, color = group)) +
# geom_bar(width = 0.001) +  # Set柱子width
#   # scale_y_continuous(limits = c(0, 6000), breaks = seq(0, 6000, by = 1000)) + 
#   scale_fill_manual(values = c("Thalassemia" = "#e95f5c", 
#                                "Carriers" = "#EDBE6C", 
#                                "Healthy" = "#4DAB8F")) +
#   scale_color_manual(values = c("Thalassemia" = "#e95f5c", 
#                                 "Carriers" = "#EDBE6C", 
#                                 "Healthy" = "#4DAB8F")) +
#   labs(x = "Heteroplasmy", y = "Counts") +  # Add标题and轴标签
# theme_classic() +  # 使用简洁主题
#   theme(
#     # axis.ticks.length.x = unit(0.03,'cm'),
#     # axis.ticks.y = element_blank(),
#     axis.text.x = element_text(size=12,face = 'bold'),
#     axis.title.x = element_text(size = 15,face = 'bold'),
#     axis.text.y = element_text(size = 12,face = 'bold'),
#     axis.title.y = element_text(size = 15,face = 'bold'),
#     axis.line = element_line(linewidth=1, colour = "black"),
#     # panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"),
#     legend.direction = "vertical")+
#   annotate("segment", x = 0, xend = 0.1, y = 5200, yend = 5200,linewidth = 0.5)+
#   # annotate("segment", x = 0, xend = 0, y = 5100, yend = 5200,linewidth = 0.5)+
#   annotate("segment", x = 0, xend = 0, y = 5100, yend = 5200,linewidth = 0.5)+
#   annotate("segment", x = 0.1, xend = 0.1, y = 5100, yend = 5200,linewidth = 0.5)+
#   annotate("segment", x = 0.05, xend = 0.05, y = 5200, yend = 5800,linewidth = 0.5)+
#   annotate("segment", x = 0.05, xend = 0.55, y = 5800, yend = 5800,linewidth = 0.5)+
#   annotate("segment", x = 0.55, xend = 0.55, y = 4500, yend = 5800,linewidth = 0.5)+
#   annotate("segment", x = 0.2, xend = 0.93, y = 4500, yend = 4500,linewidth = 0.5)+
#   annotate("segment", x = 0.2, xend = 0.2, y = 4400, yend = 4500,linewidth = 0.5)+
#   annotate("segment", x = 0.93, xend = 0.93, y = 4400, yend = 4500,linewidth = 0.5)+
#   inset_element(ppp, 0.2, 0.05, 0.9, 0.75, on_top = TRUE)

p2 <- ggplot(input, aes(x = Column2, y = per, fill = group, color = group)) +
  geom_bar(stat = "identity", width = 0.001) +
  # scale_y_continuous(limits = c(0, 6),
  #                      breaks = seq(0, 6, by = 1)) +
  scale_fill_manual(values = c("Thalassemia" = "#e95f5c", 
                               "Carriers" = "#EDBE6C", 
                               "Healthy" = "#4DAB8F")) +
  scale_color_manual(values = c("Thalassemia" = "#e95f5c", 
                                "Carriers" = "#EDBE6C", 
                                "Healthy" = "#4DAB8F")) +
  labs(x = "Heteroplasmy Levels", y = "mean variants Counts per Heteroplasmy") +  # Add标题and轴标签
  theme_classic() +  # 使用简洁主题
  theme(
    # axis.ticks.length.x = unit(0.03,'cm'),
    # axis.ticks.y = element_blank(),
    axis.text.x = element_text(size=12,face = 'bold'),
    axis.title.x = element_text(size = 15,face = 'bold'),
    axis.text.y = element_text(size = 12,face = 'bold'),
    axis.title.y = element_text(size = 15,face = 'bold'),
    axis.line = element_line(linewidth=0.7, colour = "black"),
    # panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"),
    legend.direction = "vertical")+
  annotate("segment", x = 0, xend = 0.1, y = 3000, yend = 3000,linewidth = 0.5)+
  # annotate("segment", x = 0, xend = 0, y = 5100, yend = 5200,linewidth = 0.5)+
  annotate("segment", x = 0, xend = 0, y = 2900, yend = 3000,linewidth = 0.5)+
  annotate("segment", x = 0.1, xend = 0.1, y = 2900,, yend = 3000,linewidth = 0.5)+
  annotate("segment", x = 0.05, xend = 0.05, y = 3000, yend = 4000,linewidth = 0.5)+
  annotate("segment", x = 0.05, xend = 0.55, y = 4000, yend = 4000,linewidth = 0.5)+
  annotate("segment", x = 0.55, xend = 0.55, y = 3200, yend = 4000,linewidth = 0.5)+
  annotate("segment", x = 0.5, xend = 0.6, y = 3200, yend = 3200,linewidth = 0.5)+
  # annotate("segment", x = 0.2, xend = 0.2, y = 4.4, yend = 4.5,linewidth = 0.5)+
  # annotate("segment", x = 0.93, xend = 0.93, y = 4.4, yend = 4.5,linewidth = 0.5)+
  inset_element(ppp2, 0.2, 0.05, 0.9, 0.7, on_top = TRUE)

p2
# ggsave(p2 ,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure1D.mean variants Counts per Heteroplasmy.pdf",
#        device = pdf,width = 8,height = 8,dpi = 600)

pp2 <- ggplot(input_percent, aes(x = Column, y = percent, fill = group, color = group)) +
  geom_bar(stat = "identity", width = 0.001) +
  scale_y_continuous(limits = c(0, 6),
                     breaks = seq(0, 6, by = 1)) +
  scale_fill_manual(values = c("Thalassemia" = "#e95f5c", 
                               "Carriers" = "#EDBE6C", 
                               "Healthy" = "#4DAB8F")) +
  scale_color_manual(values = c("Thalassemia" = "#e95f5c", 
                                "Carriers" = "#EDBE6C", 
                                "Healthy" = "#4DAB8F")) +
  labs(x = "Heteroplasmy Levels", y = "Percentage of Counts per Heteroplasmy(%)") +  # Add标题and轴标签
  theme_classic() +  # 使用简洁主题
  theme(
    # axis.ticks.length.x = unit(0.03,'cm'),
    # axis.ticks.y = element_blank(),
    axis.text.x = element_text(size=12,face = 'bold'),
    axis.title.x = element_text(size = 15,face = 'bold'),
    axis.text.y = element_text(size = 12,face = 'bold'),
    axis.title.y = element_text(size = 15,face = 'bold'),
    axis.line = element_line(linewidth=0.7, colour = "black"),
    # panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"),
    legend.direction = "vertical")+
  annotate("segment", x = 0, xend = 0.1, y = 5.2, yend = 5.2,linewidth = 0.5)+
  # annotate("segment", x = 0, xend = 0, y = 5100, yend = 5200,linewidth = 0.5)+
  annotate("segment", x = 0, xend = 0, y = 5.1, yend = 5.2,linewidth = 0.5)+
  annotate("segment", x = 0.1, xend = 0.1, y = 5.1, yend = 5.2,linewidth = 0.5)+
  annotate("segment", x = 0.05, xend = 0.05, y = 5.2, yend = 5.8,linewidth = 0.5)+
  annotate("segment", x = 0.05, xend = 0.55, y = 5.8, yend = 5.8,linewidth = 0.5)+
  annotate("segment", x = 0.55, xend = 0.55, y = 4.5, yend = 5.8,linewidth = 0.5)+
  annotate("segment", x = 0.5, xend = 0.6, y = 4.5, yend = 4.5,linewidth = 0.5)+
  # annotate("segment", x = 0.2, xend = 0.2, y = 4.4, yend = 4.5,linewidth = 0.5)+
  # annotate("segment", x = 0.93, xend = 0.93, y = 4.4, yend = 4.5,linewidth = 0.5)+
  inset_element(ppp, 0.2, 0.05, 0.9, 0.7, on_top = TRUE)

pp2

## Save所有数据框
setwd("D:\\biosoft\\1000thal\\mtDNA\\R script/")
save.image("figure1D.counts_per_heteroplasmy.RData")
