rm(list=ls())
library(dplyr)
library(broom)
library(data.table)
library(ggpubr) # 继承ggplot语法
library(patchwork) # Composite plot包
library(ggsci) #配色包
library(tidyr)
library(dplyr)
## Load cached, skip recomputation
# load("D:\\biosoft\\1000thal\\mtDNA\\R script/Mitochondria_Mutational_Burden.RData")
header1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\header.1020",sep = "\t")
header409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\409/header.409",sep = "\t")

sampID51child <- read.table("D:\\biosoft\\1000thal\\mtDNA\\norm_people/51norm_childs.ID",sep = "\t") %>% t()
header51child <- c("CHROM","POS","ID","REF","ALT",sampID51child)

sampID51parent <- read.table("D:\\biosoft\\1000thal\\mtDNA\\norm_people/51norm_parents.ID",sep = "\t") %>% t()
header51parent<- c("CHROM","POS","ID","REF","ALT",sampID51parent)

# sampID1020 <- header1020[7:1026]
# sampID409 <- header409[7:415]


hl1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020.mt.ano.filter.hl2zero") %>% 
  filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
  mutate(V3=paste(V2,V4,V5,sep = "_")) %>%
  # filter(rowSums(. >= 0.05) > 0) %>%
  filter(rowSums(across(7:ncol(.), ~ . >= 0.05)) > 0) %>%  # 只对第7列到最后一列进行Filter
  distinct(.keep_all = TRUE,V3) %>% 
  filter(!grepl(",",V5))

colnames(hl1020) <- header1020

hl409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut/409/409.mt.ano.filter.hl2zero") %>% 
  filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
  mutate(V3=paste(V2,V4,V5,sep = "_")) %>%
  # filter(rowSums(. >= 0.05) > 0) %>%
  filter(rowSums(across(7:ncol(.), ~ . >= 0.05)) > 0) %>%  # 只对第7列到最后一列进行Filter
  distinct(.keep_all = TRUE,V3) %>% 
  filter(!grepl(",",V5))

colnames(hl409) <- header409
###
hl51child <- read.table("D:\\biosoft\\1000thal\\mtDNA\\norm_people/norm.childs.mt.filter.hl2zero") %>% 
  filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
  mutate(V3=paste(V2,V4,V5,sep = "_")) %>%
  # filter(rowSums(. >= 0.05) > 0) %>%
  filter(rowSums(across(6:ncol(.), ~ . >= 0.05)) > 0) %>%  # 只对第7列到最后一列进行Filter
  distinct(.keep_all = TRUE,V3) %>% 
  filter(!grepl(",",V5))

colnames(hl51child) <- header51child

hl51child[hl51child=="."] <- "0"

hl51child <- hl51child %>% mutate(across(6, as.numeric))  # 使用列Position


###
hl51parent <- read.table("D:\\biosoft\\1000thal\\mtDNA\\norm_people/norm.parents.mt.filter.hl2zero") %>% 
  filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
  mutate(V3=paste(V2,V4,V5,sep = "_")) %>%
  # filter(rowSums(. >= 0.05) > 0) %>%
  filter(rowSums(across(6:ncol(.), ~ . >= 0.05)) > 0) %>%  # 只对第7列到最后一列进行Filter
  distinct(.keep_all = TRUE,V3) %>% 
  filter(!grepl(",",V5))

colnames(hl51parent) <- header51parent

hl51parent[hl51parent=="."] <- "0"

hl51parent <- hl51parent %>% mutate(across(6, as.numeric))  # 使用列Position



# mtDNA gene features
mtgff3 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020bedout\\mt.gff3.bed",sep = "\t")
mtgff3$V2 <- mtgff3$V2 + 1
mtgff3$"with" <- mtgff3$V3 - mtgff3$V2 + 1
mtgff3[1,6] <- "D_loop"
mtgff3[39,6] <- "D_loop"
colnames(mtgff3) <- c("chrom","start","end","id","strand","gene","with")

##
hl1020_het <- hl1020 %>%
  mutate(across(7:ncol(.), ~ case_when(
    . <= 0.1 ~ 0,
    . >= 0.9 ~ 0,#
    TRUE ~ 1
  )))

hl409_het <- hl409 %>%
  mutate(across(7:ncol(.), ~ case_when(
    . <= 0.1 ~ 0,
    . >= 0.9 ~ 0,#
    TRUE ~ 1
  )))



hl51child_het <- hl51child %>%
  mutate(across(6:ncol(.), ~ case_when(
    . <= 0.1 ~ 0,
    . >= 0.9 ~ 0,#
    TRUE ~ 1
  )))

hl51parent_het <- hl51parent %>%
  mutate(across(6:ncol(.), ~ case_when(
    . <= 0.1 ~ 0,
    . >= 0.9 ~ 0,#
    TRUE ~ 1
  )))
##
setDT(hl1020_het)
setDT(hl409_het)
setDT(hl51child_het)
setDT(hl51parent_het)
setDT(mtgff3)
#
hl1020_het[mtgff3, GENE := i.gene, on = .(POS >= start, POS <= end)] 

hl1020_het$GENE[is.na(hl1020_het$GENE)] <- "intergenic"

hl409_het[mtgff3, GENE := i.gene, on = .(POS >= start, POS <= end)] 

hl409_het$GENE[is.na(hl409_het$GENE)] <- "intergenic"
###
hl51child_het[mtgff3, GENE := i.gene, on = .(POS >= start, POS <= end)] 

hl51child_het$GENE[is.na(hl51child_het$GENE)] <- "intergenic"

hl51parent_het[mtgff3, GENE := i.gene, on = .(POS >= start, POS <= end)] 

hl51parent_het$GENE[is.na(hl51parent_het$GENE)] <- "intergenic"

#
df_long1020 <- hl1020_het %>%
  pivot_longer(
    cols = -c(CHROM,POS,ID,REF,ALT,INFO,GENE),  # 排除GENEandPOS列
    names_to = "Sample",   # Sample列名将存入Sample列
    values_to = "Count"    # 计数值将存入Count列
  ) %>% 
  dplyr::select(-c(CHROM,POS,ID,REF,ALT,INFO))

df_long409 <- hl409_het %>%
  pivot_longer(
    cols = -c(CHROM,POS,ID,REF,ALT,INFO,GENE),  # 排除GENEandPOS列
    names_to = "Sample",   # Sample列名将存入Sample列
    values_to = "Count"    # 计数值将存入Count列
  ) %>% 
  dplyr::select(-c(CHROM,POS,ID,REF,ALT,INFO))

df_long51child <- hl51child_het %>%
  pivot_longer(
    cols = -c(CHROM,POS,ID,REF,ALT,GENE),  # 排除GENEandPOS列
    names_to = "Sample",   # Sample列名将存入Sample列
    values_to = "Count"    # 计数值将存入Count列
  ) %>% 
  dplyr::select(-c(CHROM,POS,ID,REF,ALT))

df_long51parent <- hl51parent_het %>%
  pivot_longer(
    cols = -c(CHROM,POS,ID,REF,ALT,GENE),  # 排除GENEandPOS列
    names_to = "Sample",   # Sample列名将存入Sample列
    values_to = "Count"    # 计数值将存入Count列
  ) %>% 
  dplyr::select(-c(CHROM,POS,ID,REF,ALT))

### 按SampleandGene汇总计数 ###
df_summary1020 <- df_long1020 %>%
  group_by(Sample, GENE) %>%
  summarise(Total_Count = sum(Count, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(GROUP = "Thalassemia")

df_summary409 <- df_long409 %>%
  group_by(Sample, GENE) %>%
  summarise(Total_Count = sum(Count, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(GROUP = "Carriers")

df_summary51child <- df_long51child %>%
  group_by(Sample, GENE) %>%
  summarise(Total_Count = sum(Count, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(GROUP = "Healthy Offspring")

df_summary51parent <- df_long51parent %>%
  group_by(Sample, GENE) %>%
  summarise(Total_Count = sum(Count, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(GROUP = "Healthy Parents")

#
bed <- c("ATP6", "ATP8", "COX1", "COX2", "COX3", "CYTB", "D_loop", "ND1",
         "ND2", "ND3", "ND4", "ND4L", "ND5", "ND6", "RNR1", "RNR2", "TRNA",
         "TRNC", "TRND", "TRNE", "TRNF", "TRNG", "TRNH", "TRNI", "TRNK",
         "TRNL1", "TRNL2", "TRNM", "TRNN", "TRNP", "TRNQ", "TRNR", "TRNS1",
         "TRNS2", "TRNT", "TRNV", "TRNW", "TRNY","intergenic")

bedleng <- c(681,207,1542,684,784,1141,1122,956,1042,346,1378,297,1812,525,954,
             1559,69,66,68,69,71,68,69,69,70,75,71,68,73,68,72,65,69,59,66,69,68,66,87)

mt <- data.frame(
  GENE=bed,
  LEN=bedleng
)

df_sum1020 <- left_join(df_summary1020,mt,by="GENE") %>% 
  mutate(burden = Total_Count  / LEN * 1000 )

df_sum409 <- left_join(df_summary409,mt,by="GENE") %>% 
  mutate(burden = Total_Count  / LEN * 1000 )


df_sum51child <- left_join(df_summary51child,mt,by="GENE") %>% 
  mutate(burden = Total_Count  / LEN * 1000 )

df_sum51parent <- left_join(df_summary51parent,mt,by="GENE") %>% 
  mutate(burden = Total_Count  / LEN * 1000 )

df_sum51nrom <- rbind(df_summary51child,df_summary51parent) %>% 
  mutate(GROUP = "Healthy Normal") %>% 
  left_join(mt,by="GENE") %>% 
  mutate(burden = Total_Count  / LEN * 1000 )

#
coding_bed <- c("D_loop","ND1","ND2","COX1","COX2","ATP8","ATP6","COX3","ND3","ND4L","ND4","ND5","ND6","CYTB")

dfmerge <- rbind(df_sum1020,df_sum409) %>% 
  filter(GENE %in% coding_bed)

dfmerge_norm <- rbind(df_sum51child,df_sum51parent) %>% 
  filter(GENE %in% coding_bed)

dfmerge_all1 <- rbind(df_sum1020,df_sum409,df_sum51child) %>% 
  filter(GENE %in% coding_bed)

dfmerge_all2 <- rbind(df_sum1020,df_sum409,df_sum51nrom) %>% 
  filter(GENE %in% coding_bed)



dfmerge$GROUP <- factor(
  dfmerge$GROUP,
  
  levels = c(
    "Thalassemia",        # 第一级：轻度
    "Carriers"    # 第二级：中度
  ),
  ordered = TRUE                   # Set为有序因子
)




dfmerge_all1$GROUP <- factor(
  dfmerge_all1$GROUP,
  
  levels = c(
    "Thalassemia",        # 第一级：轻度
    "Carriers",    # 第二级：中度
    "Healthy Offspring"         # 第三级：重度
  ),
  ordered = TRUE                   # Set为有序因子
)

dfmerge_all2$GROUP <- factor(
  dfmerge_all2$GROUP,
  
  levels = c(
    "Thalassemia",        # 第一级：轻度
    "Carriers",    # 第二级：中度
    "Healthy Normal"         # 第三级：重度
  ),
  ordered = TRUE                   # Set为有序因子
)



# Create一个函数来进行两两Compare
compare_tmb <- function(data) {
  ### 使用成对 t test或非参数test ###
  pairwise_results <- data %>%
    group_by(GENE) %>%
    do({
      temp_data <- .
      
      ### 获取不同组别的名称 ###
      groups <- unique(temp_data$GROUP)
      
      ### 确保至少有两个组别 ###
      if (length(groups) >= 2) {
        ### 对每个组别进行成对 t test ###
        pairwise_t_test <- tidy(t.test(burden ~ GROUP, data = temp_data, alternative = 'two.sided'))
      } else {
        ### 如果只有一个组别，Return一个空数据框（避免 NULL） ###
        pairwise_t_test <- tibble(
          estimate = NA_real_,
          estimate1 = NA_real_,
          estimate2 = NA_real_,
          statistic = NA_real_,
          p.value = NA_real_,
          parameter = NA_real_,
          conf.low = NA_real_,
          conf.high = NA_real_,
          method = NA_character_,
          alternative = NA_character_
        )
      }
      
      ### ReturnResults（确保are数据框） ###
      pairwise_t_test
    })
  
  return(pairwise_results)
}

compare_tmb2 <- function(data) {
  ### 使用成对 t test或非参数test ###
  pairwise_results <- data %>%
    group_by(GENE) %>%
    do({
      temp_data <- .
      
      ### 获取不同组别的名称 ###
      groups <- unique(temp_data$GROUP)
      
      ### 确保至少有两个组别 ###
      if (length(groups) >= 2) {
        ### 对每个组别进行成对 t test ###
        pairwise_t_test <- tidy(t.test(Total_Count ~ GROUP, data = temp_data, alternative = 'two.sided'))
      } else {
        ### 如果只有一个组别，Return一个空数据框（避免 NULL） ###
        pairwise_t_test <- tibble(
          estimate = NA_real_,
          estimate1 = NA_real_,
          estimate2 = NA_real_,
          statistic = NA_real_,
          p.value = NA_real_,
          parameter = NA_real_,
          conf.low = NA_real_,
          conf.high = NA_real_,
          method = NA_character_,
          alternative = NA_character_
        )
      }
      
      ### ReturnResults（确保are数据框） ###
      pairwise_t_test
    })
  
  return(pairwise_results)
}

diff_TMB <- compare_tmb(dfmerge)
diff_TMB2 <- compare_tmb2(dfmerge)

diff_TMB_norm <- compare_tmb(dfmerge_norm)

##





p1 <- ggboxplot(dfmerge, x = "GENE", y = "burden", width = 0.6, color = "black",fill="GROUP",
                palette = c("#e95f5c","#EDBE6C"),
                xlab = F, #不显示x轴的label
                bxp.errorbar=T,bxp.errorbar.width=0.4, #Adderrorbar
                size=1, #箱型图边线的粗细
                outlier.shape=NA, #不显示outlier
                legend = "right") +
  labs(x="Mitochondria Genes",
       y="Mitochondria Mutational Burden(kb)") +
  theme_bw()+
  theme(legend.position = c(0.9,0.8),legend.justification = c(0.8, 0.8),
        legend.background = element_rect(fill = 'white', colour = 'black'), #Modify图例Position
        panel.grid.major.y = element_blank(),  # 去掉主要的横向网格线
        panel.grid.minor.y = element_blank(), # 去掉次要的横向向网格线
        plot.title = element_text(hjust = 0.5),
        axis.ticks.length.x = unit(0.05,'cm'),
        axis.text.x = element_text(angle = 60, hjust = 1,size=12,face = 'bold'),
        axis.title.x = element_text(size = 15,face = 'bold'),
        axis.text.y = element_text(size = 12,face = 'bold'),
        axis.title.y = element_text(size = 15,face = 'bold'),
        axis.line = element_line(linewidth=1, colour = "black"),
        # axis.ticks = element_line(color = "black", linewidth = 1),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.5)
  )+
  # scale_y_continuous(limits = c(0,8),
  #                    # breaks = c(0,40,80,120)
  # )+
  scale_fill_manual(name = "Cohort Group", values = c("#e95f5c","#EDBE6C"), labels = c("Thalssemia", "Carriers"))

p1


p11 <- p1 + stat_compare_means(aes(group = GROUP),
                               method = "t.test",
                               label="p.signif",
                               hide.ns=T,
                               show.legend = F)



p11

# ggsave("D:\\biosoft\\1000thal\\mtDNA\\figure/figure2E.Mitochondria_Mutational_Burden_thalvscarr cutoff.pdf", width= 12 , height= 8)

##
p2 <- ggboxplot(dfmerge_norm, x = "GENE", y = "burden", width = 0.6, color = "black",fill="GROUP",
                palette = c("#2F7FC1","#98CB8D"),
                xlab = F, #不显示x轴的label
                bxp.errorbar=T,bxp.errorbar.width=0.4, #Adderrorbar
                size=1, #箱型图边线的粗细
                outlier.shape=NA, #不显示outlier
                legend = "right") +
  labs(x="Mitochondria Genes",
       y="Mitochondria Mutational Burden(kb)") +
  theme_bw()+
  theme(legend.position = c(0.9,0.8),legend.justification = c(0.8, 0.8),
        legend.background = element_rect(fill = 'white', colour = 'black'), #Modify图例Position
        panel.grid.major.y = element_blank(),  # 去掉主要的横向网格线
        panel.grid.minor.y = element_blank(), # 去掉次要的横向向网格线
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1,size=12,face = 'bold'),
        axis.title.x = element_text(size = 15,face = 'bold'),
        axis.text.y = element_text(size = 12,face = 'bold'),
        axis.title.y = element_text(size = 15,face = 'bold'),
        axis.line = element_line(linewidth=1, colour = "black"),
        # axis.ticks = element_line(color = "black", linewidth = 1),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.5)
  )+
  scale_fill_manual(name = "Cohort Group", values = c("#2F7FC1","#98CB8D"), labels = c("Healthy Offspring", "Healthy Parents"))

p2


p21 <- p2 + stat_compare_means(aes(group = GROUP),
                               method = "t.test",
                               label="p.signif",
                               hide.ns=T,
                               show.legend = F)



p21
# ggsave("D:\\biosoft\\1000thal\\mtDNA\\figure/figure2E.Mitochondria_Mutational_Burden_nrom_trios cutoff.pdf", width= 12 , height= 8)


###
p3 <- ggboxplot(dfmerge_all1, x = "GENE", y = "burden", width = 0.6, color = "black",fill="GROUP",
                palette = c("#e95f5c","#EDBE6C","#4DAB8F"),
                xlab = F, #不显示x轴的label
                bxp.errorbar=T,bxp.errorbar.width=0.4, #Adderrorbar
                size=1, #箱型图边线的粗细
                outlier.shape=NA, #不显示outlier
                legend = "right") +
  labs(x="Mitochondria Genes",
       y="Mitochondria Mutational Burden(kb)") +
  theme_bw()+
  theme(legend.position = c(0.9,0.8),legend.justification = c(0.8, 0.8),
        legend.background = element_rect(fill = 'white', colour = 'black'), #Modify图例Position
        panel.grid.major.y = element_blank(),  # 去掉主要的横向网格线
        panel.grid.minor.y = element_blank(), # 去掉次要的横向向网格线
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1,size=12,face = 'bold'),
        axis.title.x = element_text(size = 15,face = 'bold'),
        axis.text.y = element_text(size = 12,face = 'bold'),
        axis.title.y = element_text(size = 15,face = 'bold'),
        axis.line = element_line(linewidth=1, colour = "black"),
        # axis.ticks = element_line(color = "black", linewidth = 1),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.5)
  )+
  scale_fill_manual(name = "Cohort Group", values = c("#e95f5c","#EDBE6C","#4DAB8F"), labels = c("Thalssemia", "Carriers", "Healthy Normal"))

p3


p31 <- p3 + stat_compare_means(aes(group = GROUP),
                               label="p.signif",
                               hide.ns=T,
                               show.legend = F)



p31


# ggsave("D:\\biosoft\\1000thal\\mtDNA\\figure/figure2E.Mitochondria_Mutational_Burden cutoff.pdf", width= 12 , height= 8)

###
p4 <- ggboxplot(dfmerge_all2, x = "GENE", y = "burden", width = 0.6, color = "black",fill="GROUP",
                palette = c("#e95f5c","#EDBE6C","#4DAB8F"),
                xlab = F, #不显示x轴的label
                bxp.errorbar=T,bxp.errorbar.width=0.4, #Adderrorbar
                size=1, #箱型图边线的粗细
                outlier.shape=NA, #不显示outlier
                legend = "right") +
  labs(x="Mitochondria Genes",
       y="Mitochondria Mutational Burden(kb)") +
  theme_bw()+
  theme(legend.position = c(0.9,0.8),legend.justification = c(0.8, 0.8),
        legend.background = element_rect(fill = 'white', colour = 'black'), #Modify图例Position
        panel.grid.major.y = element_blank(),  # 去掉主要的横向网格线
        panel.grid.minor.y = element_blank(), # 去掉次要的横向向网格线
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1,size=12,face = 'bold'),
        axis.title.x = element_text(size = 15,face = 'bold'),
        axis.text.y = element_text(size = 12,face = 'bold'),
        axis.title.y = element_text(size = 15,face = 'bold'),
        axis.line = element_line(linewidth=1, colour = "black"),
        # axis.ticks = element_line(color = "black", linewidth = 1),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.5)
  )+
  scale_y_continuous(limits = c(0,10),
                     # breaks = c(0,40,80,120)
  )+
  scale_fill_manual(name = "Cohort Group", values = c("#e95f5c","#EDBE6C","#4DAB8F"), labels = c("Thalssemia", "Carriers", "Healthy Normal"))

p4

p_values2 <- dfmerge_all2 %>%
  group_by(GENE) %>%
  summarise(
    p_Thal_vs_Carrier = t.test(burden[GROUP == "Thalassemia"], 
                               burden[GROUP == "Carriers"])$p.value,
    p_Thal_vs_Normal = t.test(burden[GROUP == "Thalassemia"], 
                              burden[GROUP == "Healthy Normal"])$p.value
  )

### 将 p 值转换为显著性符号 ###
p_values2 <- p_values2 %>%
  mutate(
    sig_Thal_vs_Carrier = case_when(
      p_Thal_vs_Carrier < 0.0001 ~ "****",
      p_Thal_vs_Carrier < 0.001 ~ "***",
      p_Thal_vs_Carrier < 0.01 ~ "**",
      p_Thal_vs_Carrier < 0.05 ~ "*",
      TRUE ~ "ns"
    ),
    sig_Thal_vs_Normal = case_when(
      p_Thal_vs_Normal < 0.0001 ~ "****",
      p_Thal_vs_Normal < 0.001 ~ "***",
      p_Thal_vs_Normal < 0.01 ~ "**",
      p_Thal_vs_Normal < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  )

p_values2 <- p_values2 %>%
  mutate(
    combined_sig = paste0(sig_Thal_vs_Carrier, "/", sig_Thal_vs_Normal)
  ) 

p_values2[p_values2=="ns/ns"] <- ""
# Merge到图中
p41 <-p4 + 
  geom_text(
    data = p_values2,
    aes(x = GENE, 
        y = 9,  # Adjusty轴Position
        label = combined_sig),
    vjust = 0,
    size = 4  # 可Adjust文字大小
  )

p41


# ggsave("D:\\biosoft\\1000thal\\mtDNA\\figure/figure2E.Mitochondria_Mutational_Burden_thalvsnomal.pdf", width= 12 , height= 8)





## Save所有数据框
setwd("D:\\biosoft\\1000thal\\mtDNA\\R script/")
save.image("Mitochondria_Mutational_Burden.RData")
