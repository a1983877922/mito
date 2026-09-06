rm(list=ls())
library(dplyr)
library(tidyverse)
library(ggrepel)
library(patchwork)
# 


## Load cached, skip recomputation
# load("D:\\biosoft\\1000thal\\mtDNA\\R script/mutation_per_sample.RData")

header <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\header.1020",sep = "\t")
header409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\409/header.409",sep = "\t")
sampID <- header[1,7:1026]
sampID409 <- header409[1,7:415]
sampID58 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\58norm_people\\finalout/58.header",header = F) %>%
  t() %>%
  as.data.frame()
  # pull()
# hl1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020.mt.ano.filter.hl2zero") %>% 
#   filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
#   mutate(V3=paste(V2,V4,V5,sep = "_")) %>%
#   # filter(rowSums(. >= 0.05) > 0) %>%
#   filter(rowSums(across(7:ncol(.), ~ . >= 0.05)) > 0) %>%  # 只对第7列到最后一列进行Filter
#   distinct(.keep_all = TRUE,V3) %>% 
#   filter(!grepl(",",V5))
# 
# hl409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\409/409.mt.ano.filter.hl2zero") %>% 
#   filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
#   mutate(V3=paste(V2,V4,V5,sep = "_")) %>%
#   # filter(rowSums(. >= 0.05) > 0) %>%
#   filter(rowSums(across(7:ncol(.), ~ . >= 0.05)) > 0) %>%  # 只对第7列到最后一列进行Filter
#   distinct(.keep_all = TRUE,V3) %>% 
#   filter(!grepl(",",V5))

# hl58<- read.table("D:\\biosoft\\1000thal\\mtDNA\\58norm_people\\finalout/58norm.mt.filter.hl2zero") %>% 
#   filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
#   mutate(V3=paste(V2,V4,V5,sep = "_")) %>%
#   # filter(rowSums(. >= 0.05) > 0) %>%
#   filter(rowSums(across(6:ncol(.), ~ . >= 0.05)) > 0) %>%  # 只对第7列到最后一列进行Filter
#   distinct(.keep_all = TRUE,V3) %>% 
#   filter(!grepl(",",V5))
hl1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020.mt.ano.filter.ft2onefilt")
hl409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\409.mt.ano.filter.ft2onefilt") 
hl58 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\58.mt.ano.filter.ft2onefilt") 


indel_hl1020 <- hl1020 %>%  filter(nchar(V4) != 1 | nchar(V5) != 1)%>% 
  filter(!(str_detect(V5, "^\\w,\\w$")|str_detect(V5, "^\\w,\\w,\\w$")))

count_mutant_per_sample0.1 <- colSums(hl1020[7:1026] >= 0.1 & hl1020[7:1026] <= 0.9)
# count_mutant_per_sample0.9 <- c(colSums(hl1020[7:1026] >0.9 ))


count_mutant_per_sample409_0.1 <- colSums(hl409[7:415] >= 0.1 & hl409[7:415] <= 0.9)
# count_mutant_per_sample409_0.9 <- c(colSums(hl409[7:415] >0.9 ))

count_mutant_per_sample58_0.1 <- colSums(hl58[6:63] >= 0.1 & hl58[6:63] <= 0.9)
# Create一个新的数据框来存储计数信息
# mut_per <- data.frame(
#   sampleID = t(sampID),
#   counts_mutations_0.95 = count_mutant_per_sample0.95,
#   counts_mutations_0.05 = count_mutant_per_sample0.05
# ) %>% 
#   mutate(
#     interval95=cut(counts_mutations_0.95, breaks = seq(0, 50, by = 5)),
#     interval05=cut(counts_mutations_0.05, breaks = seq(0, 55, by = 5))
#   )

mut_per <- data.frame(
  sampleID = t(sampID),  # Assume sampID are一个矩阵或数据框，转置后作为列
  counts_mutations_0.1 = count_mutant_per_sample0.1  # Mutation计数数据
) %>% 
  mutate(
    interval01 = cut(counts_mutations_0.1, breaks = seq(0, 16, by = 1), right = FALSE, include.lowest = TRUE)
  )


mut_per409 <- data.frame(
  sampleID = t(sampID409),
  counts_mutations409_0.1 = count_mutant_per_sample409_0.1
) %>% 
  mutate(
    interval01=cut(counts_mutations409_0.1, breaks = seq(0, 16, by = 1), right = FALSE, include.lowest = TRUE)
  )

mut_per58 <- data.frame(
  sampleID = t(sampID58),
  counts_mutations58_0.1 = count_mutant_per_sample58_0.1
) %>% 
  mutate(
    interval01=cut(counts_mutations58_0.1, breaks = seq(0, 16, by = 1), right = FALSE, include.lowest = TRUE)
  )

# hom_mut_per <- mut_per %>% 
#   group_by(interval95) %>% 
#   mutate(sample_counts = sum(n()),
#          homoplasmic="hom" ) %>% 
#   select(interval95,sample_counts,homoplasmic) %>% 
#   distinct() %>% 
#   rename(seq = interval95)

het_mut_per1 <- mut_per %>% 
  group_by(interval01) %>% 
  mutate(sample_counts = n(),
         homoplasmic="het") %>% 
  ungroup() 

het_mut_per <- het_mut_per1[,c("interval01", "sample_counts", "homoplasmic")] %>% 
  group_by(interval01) %>% 
  distinct() %>% 
  rename(seq = interval01)%>% 
  filter(!is.na(seq))
##409
het_mut_per1_409 <- mut_per409 %>% 
  group_by(interval01) %>% 
  mutate(sample_counts = n(),
         homoplasmic="het") %>% 
  ungroup() 

het_mut_per409 <- het_mut_per1_409[,c("interval01", "sample_counts", "homoplasmic")] %>% 
  group_by(interval01) %>% 
  distinct() %>% 
  rename(seq = interval01)%>% 
  filter(!is.na(seq))

##58
het_mut_per1_58 <- mut_per58 %>% 
  group_by(interval01) %>% 
  mutate(sample_counts = n(),
         homoplasmic="het") %>% 
  ungroup() 

het_mut_per58 <- het_mut_per1_58[,c("interval01", "sample_counts", "homoplasmic")] %>% 
  group_by(interval01) %>% 
  distinct() %>% 
  rename(seq = interval01)%>% 
  filter(!is.na(seq))
#
# het_mut_per$sample_counts[5] <- het_mut_per$sample_counts[5] + het_mut_per$sample_counts[6]
# 
# het_mut_per <- het_mut_per %>% filter(!is.na(seq))

# input <- full_join(hom_mut_per ,het_mut_per,by=c("interval95"="interval05")) %>% 
#   rename(seq = interval95,
#          Hom =sample_counts.x,
#          Het = sample_counts.y) %>% 
#   mutate(across(everything(), ~ ifelse(is.na(.), 0, .))) %>% 
#   arrange(seq)
#   

# input2 <- rbind(het_mut_per,hom_mut_per)

### 使用ggplot2Plot双柱状图 ###
# P1 <- ggplot(input2, aes(x = seq, y = sample_counts, fill = homoplasmic)) +
# geom_bar(stat = "identity", position = "dodge", width = 0.7) +
# scale_fill_manual(values = c("#FA7F6F", "#82B0D2"),
#                   labels=c('Het (HL ~ [0.05,0.95))', 'Hom (HL >= 0.95)'),
#                   name="Homoplasmic level") +
# geom_point(aes(color = homoplasmic), size = 2) +
# geom_line(aes(color = homoplasmic, group = homoplasmic), size = 1.2) +
# scale_color_manual(values = c('#1e8b9b','#2878B5'),
#                    labels=c('Het (HL ~ [0.05,0.95))', 'Hom (HL >= 0.95)'),
#                    name="Homoplasmic level") +
# labs(title = NULL,
#      x = "Number of mtDNA mutations per sample",
#      y = "Number of samples") +
# theme_classic()+
# theme(
#   axis.text.x = element_text(size=12,face = 'bold'),
#   axis.title.x = element_text(size = 15,face = 'bold'),
#   axis.text.y = element_text(size = 12,face = 'bold'),
#   axis.title.y = element_text(size = 15,face = 'bold'),
#   axis.line = element_line(linewidth=1, colour = "black"),
#   panel.grid.major = element_blank(), # Remove主网格线
#   panel.grid.minor = element_blank(), # Remove次网格线
#   # panel.border = element_rect(color="black", fill=NA, size=2),
#   legend.position = c(0.87,0.87)
# )

mean(mut_per$counts_mutations_0.1)
mean(mut_per409$counts_mutations409_0.1)
mean(mut_per58$counts_mutations58_0.1)
### 使用ggplot2Plot双柱状图 ###
P1 <- ggplot(het_mut_per, aes(x = seq, y = sample_counts, fill = "#FA7F6F")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("#FA7F6F", "#82B0D2"),
                    labels=c('Het (HL ~ (0.1,0.9))', 'Hom (HL >= 0.9)'),
                    name="Homoplasmic level") +
  geom_point(aes(color = homoplasmic), size = 2) +
  geom_line(aes(color = homoplasmic, group = homoplasmic), size = 1.2) +
  scale_color_manual(values = c('#1e8b9b','#2878B5'),
                     labels=c('Het (HL ~ (0.1,0.9))', 'Hom (HL >= 0.9)'),
                     name="Homoplasmic level") +
  labs(title = NULL,
       x = "Number of mtDNA mutations per sample",
       y = "Number of samples") +
  theme_classic()+
  theme(
    axis.text.x = element_text(size=8,face = 'bold'),
    axis.title.x = element_text(size = 15,face = 'bold'),
    axis.text.y = element_text(size = 12,face = 'bold'),
    axis.title.y = element_text(size = 15,face = 'bold'),
    # axis.line = element_line(linewidth=1, colour = "black"),
    panel.grid.major = element_blank(), # Remove主网格线
    panel.grid.minor = element_blank(), # Remove次网格线
    # panel.border = element_rect(color="black", fill=NA, size=2),
    legend.position = c(0.87,0.87)
  )+
  scale_y_continuous(limits = c(0,800),expand = c(0, 0)
                     # breaks = c(0,40,80,120)
  )


P1
# ggsave(P1,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure3D.Distribution of mtDNA mutations cutoff202512.pdf",
#        device = pdf,width = 8,height = 7,dpi = 600)


### 使用ggplot2Plot双柱状图 ###
P2 <- ggplot(het_mut_per409, aes(x = seq, y = sample_counts, fill = "#FA7F6F")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("#FA7F6F", "#82B0D2"),
                    labels=c('Het (HL ~ (0.1,0.9))', 'Hom (HL >= 0.9)'),
                    name="Homoplasmic level") +
  geom_point(aes(color = homoplasmic), size = 2) +
  geom_line(aes(color = homoplasmic, group = homoplasmic), size = 1.2) +
  scale_color_manual(values = c('#1e8b9b','#2878B5'),
                     labels=c('Het (HL ~ (0.1,0.9))', 'Hom (HL >= 0.9)'),
                     name="Homoplasmic level") +
  labs(title = NULL,
       x = "Number of mtDNA mutations per sample",
       y = "Number of samples") +
  theme_classic()+
  theme(
    axis.text.x = element_text(size=8,face = 'bold'),
    axis.title.x = element_text(size = 15,face = 'bold'),
    axis.text.y = element_text(size = 12,face = 'bold'),
    axis.title.y = element_text(size = 15,face = 'bold'),
    # axis.line = element_line(linewidth=1, colour = "black"),
    panel.grid.major = element_blank(), # Remove主网格线
    panel.grid.minor = element_blank(), # Remove次网格线
    # panel.border = element_rect(color="black", fill=NA, size=2),
    legend.position = c(0.87,0.87)
  )+
  scale_y_continuous(limits = c(0,300),expand = c(0, 0)
                     # breaks = c(0,40,80,120)
  )


P2
# ggsave(P2,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure1D.Distribution of mtDNA mutations409 cutoff202512.pdf",
#        device = pdf,width = 8,height = 7,dpi = 600)

# ##Save所有数据框
# setwd("D:\\biosoft\\1000thal\\mtDNA\\R script/")
# save.image("mutation_per_sample.RData")

P3 <- ggplot(het_mut_per58, aes(x = seq, y = sample_counts, fill = "#FA7F6F")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("#FA7F6F", "#82B0D2"),
                    labels=c('Het (HL ~ (0.1,0.9))', 'Hom (HL >= 0.9)'),
                    name="Homoplasmic level") +
  geom_point(aes(color = homoplasmic), size = 2) +
  geom_line(aes(color = homoplasmic, group = homoplasmic), size = 1.2) +
  scale_color_manual(values = c('#1e8b9b','#2878B5'),
                     labels=c('Het (HL ~ (0.1,0.9))', 'Hom (HL >= 0.9)'),
                     name="Homoplasmic level") +
  labs(title = NULL,
       x = "Number of mtDNA mutations per sample",
       y = "Number of samples") +
  theme_classic()+
  theme(
    axis.text.x = element_text(size=8,face = 'bold'),
    axis.title.x = element_text(size = 15,face = 'bold'),
    axis.text.y = element_text(size = 12,face = 'bold'),
    axis.title.y = element_text(size = 15,face = 'bold'),
    # axis.line = element_line(linewidth=1, colour = "black"),
    panel.grid.major = element_blank(), # Remove主网格线
    panel.grid.minor = element_blank(), # Remove次网格线
    # panel.border = element_rect(color="black", fill=NA, size=2),
    legend.position = c(0.87,0.87)
  )+
  scale_y_continuous(limits = c(0,40),expand = c(0, 0)
                     # breaks = c(0,40,80,120)
  )


P3
# ggsave(P3,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure1D.Distribution of mtDNA mutations norm58 cutoff202512.pdf",
#        device = pdf,width = 8,height = 7,dpi = 600)
