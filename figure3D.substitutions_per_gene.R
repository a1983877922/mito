rm(list=ls())
library(dplyr)
library(tidyr)
library(tidyverse)
library(broom)
library(ggplot2)
library(ggrepel)
library(data.table)
## Load cached, skip recomputation
# load("D:\\biosoft\\1000thal\\mtDNA\\R script/substitutions.RData")
header1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\header.1020",sep = "\t")
header409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\409/header.409",sep = "\t")

# hl1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020.mt.ano.filter.hl2zero") %>% 
#   filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
#   mutate(V3=paste(V2,V4,V5,sep = "_")) %>%
#   # filter(rowSums(. >= 0.05) > 0) %>%
#   filter(rowSums(across(7:ncol(.), ~ . >= 0.05)) > 0) %>%  # 只对第7列到最后一列进行Filter
#   distinct(.keep_all = TRUE,V3) %>% 
#   filter(!grepl(",",V5))

hl1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020.mt.ano.filter.ft2onefilt")
hl409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\409.mt.ano.filter.ft2onefilt") 

colnames(hl1020) <- header1020

# hl409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\409/409.mt.ano.filter.hl2zero") %>% 
#   filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
#   mutate(V3=paste(V2,V4,V5,sep = "_")) %>%
#   # filter(rowSums(. >= 0.05) > 0) %>%
#   filter(rowSums(across(7:ncol(.), ~ . >= 0.05)) > 0) %>%  # 只对第7列到最后一列进行Filter
#   distinct(.keep_all = TRUE,V3) %>% 
#   filter(!grepl(",",V5))
colnames(hl409) <- header409

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
  ))) %>% 
  filter(rowSums(.[7:ncol(.)] == 1) > 0)

hl409_het <- hl409 %>%
  mutate(across(7:ncol(.), ~ case_when(
    . <= 0.1 ~ 0,
    . >= 0.9 ~ 0,#
    TRUE ~ 1
  ))) %>% 
  filter(rowSums(.[7:ncol(.)] == 1) > 0)

##
mtgff3_PART1 <- mtgff3 %>% 
  filter(!(gene %in% c("ATP8","ND4L")))
mtgff3_PART2 <- mtgff3 %>% 
  filter(gene %in% c("ATP8","ND4L"))
##
setDT(hl1020_het)
setDT(hl409_het)
setDT(mtgff3)
setDT(mtgff3_PART1)
setDT(mtgff3_PART2)
#
hl1020_het_spec <- hl1020_het
hl409_het_spec <- hl409_het

#
hl1020_het[mtgff3_PART1, GENE := i.gene, on = .(POS >= start, POS <= end)] 

hl1020_het$GENE[is.na(hl1020_het$GENE)] <- "intergenic"


hl1020_het_spec[mtgff3_PART2, GENE := i.gene, on = .(POS >= start, POS <= end)] 

hl1020_het_spec<- hl1020_het_spec %>% 
  filter(GENE %in% c("ATP8","ND4L"))


input_combind_1020 <- hl1020_het %>% 
  filter(!(ID %in% hl1020_het_spec$ID)) %>% 
  rbind(.,hl1020_het_spec) %>% 
  dplyr::select(CHROM, POS, ID, REF, ALT,INFO,GENE) %>% 
  arrange(POS) %>% 
  unique()
write.table(input_combind_1020 ,"D:/biosoft/1000thal/mtDNA/185variants.tsv",sep = "\t",row.names = F)

###
hl409_het[mtgff3, GENE := i.gene, on = .(POS >= start, POS <= end)] 

hl409_het$GENE[is.na(hl409_het$GENE)] <- "intergenic"


hl409_het_spec[mtgff3_PART2, GENE := i.gene, on = .(POS >= start, POS <= end)] 

hl409_het_spec<- hl409_het_spec %>% 
  filter(GENE %in% c("ATP8","ND4L"))

input_combind_409 <- rbind(hl409_het,hl409_het_spec) %>% 
  dplyr::select(CHROM, POS, ID, REF, ALT,GENE) %>% 
  arrange(POS) %>% 
  unique()
#
# coding_bed <- c("ND1","ND2","COX1","COX2","ATP6","COX3","ND3","ND4","ND5","ND6","CYTB")
# coding_bed1 <- c("ATP8","ND4L")

bed <- c("ATP6","COX3","ND2","ND5","TRNA","TRNF","TRNK","TRNN","TRNS1","TRNW","ATP8","CYTB","ND3",
         "ND6","TRNC","TRNG","TRNL1","TRNP","TRNS2","TRNY","COX1","D_loop","ND4","RNR1","TRND","TRNH",
         "TRNL2","TRNQ","TRNT","COX2","ND1","ND4L","RNR2","TRNE","TRNI","TRNM","TRNR","TRNV")

coding_bed <- c("ATP6","COX3","ND2","ND5","ATP8","CYTB","ND3","ND6","COX1","ND4","COX2","ND1","ND4L")

nocoding_bed <- c("TRNA", "TRNV", "TRNF","TRNK","TRNN","TRNS1","TRNW","intergenic",
                  "TRNC","TRNG","TRNL1","TRNP","TRNS2","TRNY","D_loop","RNR1","TRND","TRNH","TRNR",
                  "TRNL2","TRNQ","TRNT","RNR2","TRNE","TRNI","TRNM")
rRNA_bed <- c("RNR1","RNR2")
TRNA_bed <- c("TRNF", "TRNV", "TRNL1",  "TRNI", "TRNQ", "TRNM", "TRNW", "TRNA", "TRNN", "TRNC", "TRNY", 
  "TRNS1", "TRND",  "TRNK", "TRNG", "TRNR", "TRNH", "TRNS2", "TRNL2", "TRNE", "TRNT", "TRNP")

# counts_type <- function(df1,genename="ATP6"){
#   df <- df1 %>%
#     dplyr::select(CHROM, POS, ID, REF, ALT) %>% 
#     mutate(change = str_c(REF, ALT)) %>%  # 先Add change 列
#     filter(nchar(change) == 2)            # 再Filter长度为 2 的行
#   df <- as.data.frame(df)  # Cast to data.frame
#   
#   
#   change <- which(df$change == 'AT')
#   df[change,'type1'] <- 'A>T|T>A'; df[change,'type2'] <- 'tv'
#   change <- which(df$change == 'AG')
#   df[change,'type1'] <- 'A>G|T>C'; df[change,'type2'] <- 'ti'
#   change <- which(df$change == 'AC')
#   df[change,'type1'] <- 'A>C|T>G'; df[change,'type2'] <- 'tv'
#   
#   change <- which(df$change == 'TA')
#   df[change,'type1'] <- 'A>T|T>A'; df[change,'type2'] <- 'tv'
#   change <- which(df$change == 'TG')
#   df[change,'type1'] <- 'A>C|T>G'; df[change,'type2'] <- 'tv'
#   change <- which(df$change == 'TC')
#   df[change,'type1'] <- 'A>G|T>C'; df[change,'type2'] <- 'ti'
#   
#   change <- which(df$change == 'GA')
#   df[change,'type1'] <- 'G>A|C>T'; df[change,'type2'] <- 'ti'
#   change <- which(df$change == 'GT')
#   df[change,'type1'] <- 'G>T|C>A'; df[change,'type2'] <- 'tv'
#   change <- which(df$change == 'GC')
#   df[change,'type1'] <- 'G>C|C>G'; df[change,'type2'] <- 'tv'
#   
#   change <- which(df$change == 'CA')
#   df[change,'type1'] <- 'G>T|C>A'; df[change,'type2'] <- 'tv'
#   change <- which(df$change == 'CT')
#   df[change,'type1'] <- 'G>A|C>T'; df[change,'type2'] <- 'ti'
#   change <- which(df$change == 'CG')
#   df[change,'type1'] <- 'G>C|C>G'; df[change,'type2'] <- 'tv'
#   
#   # df_ti <- length(which(df$type2 == 'ti'))
#   # df_tv <- length(which(df$type2 == 'tv'))
#   # 
#   # df_at <- length(which(df$type1 == 'A>T|T>A'))
#   # df_ag <- length(which(df$type1 == 'A>G|T>C'))
#   # df_ac <- length(which(df$type1 == 'A>C|T>G'))
#   # df_ga <- length(which(df$type1 == 'G>A|C>T'))
#   # df_gt <- length(which(df$type1 == 'G>T|C>A'))
#   # df_gc <- length(which(df$type1 == 'G>C|C>G'))   
#   
#   df_substitute <- df %>% 
#     group_by(type1) %>% 
#     mutate(counts = n()) %>% 
#     distinct(type1, counts) %>% 
#     ungroup() %>% 
#     mutate(
#       genes = genename,
#       proportion = counts / sum(counts)
#     )
#   return(df_substitute)
# }
### 定义Substitution规则 ###
change_rules <- list(
  'AT' = c('A>T|T>A', 'tv'),
  'AG' = c('A>G|T>C', 'ti'),
  'AC' = c('A>C|T>G', 'tv'),
  'TA' = c('A>T|T>A', 'tv'),
  'TG' = c('A>C|T>G', 'tv'),
  'TC' = c('A>G|T>C', 'ti'),
  'GA' = c('G>A|C>T', 'ti'),
  'GT' = c('G>T|C>A', 'tv'),
  'GC' = c('G>C|C>G', 'tv'),
  'CA' = c('G>T|C>A', 'tv'),
  'CT' = c('G>A|C>T', 'ti'),
  'CG' = c('G>C|C>G', 'tv')
)
### 1020 ###
df1020 <- input_combind_1020 %>%
  # dplyr::select(CHROM, POS, ID, REF, ALT) %>% 
  mutate(change = str_c(REF, ALT)) %>%  # 先Add change 列
  filter(nchar(change) == 2)            # 再Filter长度为 2 的行
df1020 <- as.data.frame(df1020) %>%   # Cast to data.frame
  mutate(OXPHS= case_when(
    GENE %in% rRNA_bed ~ "rRNA",
    GENE %in% TRNA_bed ~ "tRNA",
    GENE %in% coding_bed ~ "OXPHS",
    GENE == "D_loop" ~ "D_loop",
    TRUE ~ "intergenic"
  )
)

### Iterate over所有可能的 change 组合 ###
for (change_str in names(change_rules)) {
  change_idx <- which(df1020$change == change_str)
  if (length(change_idx) > 0) {
    df1020[change_idx, 'type1'] <- change_rules[[change_str]][1]
    df1020[change_idx, 'type2'] <- change_rules[[change_str]][2]
  }
}

df_substitute1020 <- df1020 %>% 
  group_by(GENE) %>% 
  mutate(counts = n()) %>% 
  distinct(type1, counts) %>% 
  # ungroup() %>% 
  mutate(
    proportion = counts / sum(counts)
  )

# data_type_sum1020 <- df_substitute1020 %>% 
#   group_by(type1) %>% 
#   mutate(sum = sum(counts))%>% 
#   distinct(type1, sum) %>% 
#   ungroup() %>% 
#   rename(counts=sum) %>% 
#   mutate(
#     GENE = "Overall",
#     proportion = counts / sum(counts)
#   ) %>% 
#   select(GENE, type1, counts, proportion)  # Adjust列顺序
data_type_sum1020 <- df_substitute1020 %>% 
  group_by(type1) %>% 
  summarise(counts = sum(counts)) %>%  # 直接汇总
  ungroup() %>% 
  mutate(
    GENE = "Overall",
    proportion = counts / sum(counts)
  ) %>% 
  select(GENE, type1, counts, proportion)
all_data_type1020 <- rbind(df_substitute1020,data_type_sum1020)

### 409 ###
df409 <- input_combind_409 %>%
  # dplyr::select(CHROM, POS, ID, REF, ALT) %>% 
  mutate(change = str_c(REF, ALT)) %>%  # 先Add change 列
  filter(nchar(change) == 2)            # 再Filter长度为 2 的行
df409 <- as.data.frame(df409) %>%   # Cast to data.frame
  mutate(OXPHS= case_when(
    GENE %in% rRNA_bed ~ "rRNA",
    GENE %in% TRNA_bed ~ "tRNA",
    GENE %in% coding_bed ~ "OXPHS",
    GENE == "D_loop" ~ "D_loop",
    TRUE ~ "intergenic"
  )
  )

### Iterate over所有可能的 change 组合 ###
for (change_str in names(change_rules)) {
  change_idx <- which(df409$change == change_str)
  if (length(change_idx) > 0) {
    df409[change_idx, 'type1'] <- change_rules[[change_str]][1]
    df409[change_idx, 'type2'] <- change_rules[[change_str]][2]
  }
}

df_substitute409 <- df409 %>% 
  group_by(GENE) %>% 
  mutate(counts = n()) %>% 
  distinct(type1, counts) %>% 
  # ungroup() %>% 
  mutate(
    proportion = counts / sum(counts)
  )

# data_type_sum409 <- df_substitute409 %>% 
#   group_by(type1) %>% 
#   mutate(sum = sum(counts))%>% 
#   distinct(type1, sum) %>% 
#   ungroup() %>% 
#   rename(counts=sum) %>% 
#   mutate(
#     GENE = "Overall",
#     proportion = counts / sum(counts)
#   ) %>% 
#   select(GENE, type1, counts, proportion)  # Adjust列顺序

data_type_sum409 <- df_substitute409 %>% 
  group_by(type1) %>% 
  summarise(counts = sum(counts)) %>%  # 直接汇总
  ungroup() %>% 
  mutate(
    GENE = "Overall",
    proportion = counts / sum(counts)
  ) %>% 
  select(GENE, type1, counts, proportion)

all_data_type409 <- rbind(df_substitute409,data_type_sum409)

### 卡方test ###
input1020 <- all_data_type1020 %>% 
  mutate(group = "Thal")

input409 <- all_data_type409 %>% 
  mutate(group = "Carrier")

input_all <- rbind(input1020,input409)
##
index <- which(input_all$type1=='A>T|T>A')
input_all[index,'type2']<- 'tv'

index <- which(input_all$type1 == 'A>G|T>C')
input_all[index,'type2'] <- 'ti'

index <- which(input_all$type1 == 'A>C|T>G')
input_all[index,'type2'] <- 'tv'

index <- which(input_all$type1 == 'G>A|C>T')
input_all[index,'type2'] <- 'ti'
index <- which(input_all$type1 == 'G>T|C>A')
input_all[index,'type2'] <- 'tv'
index <- which(input_all$type1 == 'G>C|C>G')
input_all[index,'type2'] <- 'tv'

### p ###

calculate_p_value <- function(i) {
  # Create一个空的数据框用于存储Results
  chip_df <- data.frame(gene = character(), pvalue = numeric(), stringsAsFactors = FALSE)
  
  ### 从 input_all 中Filter出SpecifyGene的数据 ###
  input1 <- input_all %>% filter(GENE == i)
  
  ### 对数据进行汇总 ###
  input1_summary <- input1 %>%
    group_by(group, type2) %>%
    summarise(total_counts = sum(counts, na.rm = TRUE), .groups = 'drop') # Add na.rm 参数以忽略 NA
  
  ### 将汇总后的数据转换成矩阵 ###
  matrix_data <- input1_summary %>%
    pivot_wider(names_from = type2, values_from = total_counts, values_fill = 0) # 使用 values_fill = 0 填充缺失值
  
  ### 转换成标准的矩阵形式 ###
  matrix_output <- as.matrix(matrix_data[, -1]) # 排除第一列（group列）
  
  ### Check矩阵输出 ###
  print(paste("Matrix for", i))
  print(matrix_output)
  
  ### 如果矩阵少于两行或两列，补充缺失的行或列 ###
  if (nrow(matrix_output) < 2) {
    matrix_output <- rbind(matrix_output, matrix(0, nrow = 2 - nrow(matrix_output), ncol = ncol(matrix_output)))
  }
  if (ncol(matrix_output) < 2) {
    matrix_output <- cbind(matrix_output, matrix(0, nrow = nrow(matrix_output), ncol = 2 - ncol(matrix_output)))
  }
  
  ### 执行 Fisher 精确test并获取 p 值 ###
  if(all(is.finite(matrix_output)) && all(matrix_output >= 0)) { # Check所有值都are非负的有限值
    p <- fisher.test(matrix_output)
    ### 将 p 值Add到 chip_df 数据框中 ###
    chip_df <- rbind(chip_df, data.frame(gene = i, pvalue = p$p.value))
  } else {
    stop(paste("Matrix contains invalid values for gene", i))
  }
  
  return(chip_df)
}

p_df <- data.frame()
for (i in coding_bed) {
  print(i)
  df <- calculate_p_value(i)
  p_df <- rbind(p_df, df)
}

##
calculate_tv_ti <- function(i) {
  # Create一个空的数据框用于存储Results
  tvti_df <- data.frame(gene = character(), value = numeric(), stringsAsFactors = FALSE)
  
  ### 从 input_all 中Filter出SpecifyGene的数据 ###
  input1 <- input_all %>% filter(GENE == i,group=="Thal")
  
  ### 对数据进行汇总 ###
  input1_summary <- input1 %>%
    group_by(type2) %>%
    summarise(total_counts = sum(counts))
  
  ### 将汇总后的数据转tv/ti ###
  # matrix_data <- input1_summary %>%
  #   pivot_wider(names_from = type2, values_from = total_counts) %>% 
  #   mutate(GENE=i,
  #          tv_ti=ifelse(is.na(tv),ti,ti / tv))
  matrix_data <- input1_summary %>%
    pivot_wider(names_from = type2, values_from = total_counts) %>% 
    ### 如果 tv 列不存在，则Create并赋值为 0 ###
    mutate(tv = ifelse("tv" %in% colnames(.), tv, 0)) %>%
    ### 计算 tv_ti，如果 tv=0 则设为 NA ###
    mutate(GENE = i,
           tv_ti = ifelse(tv == 0, NA_real_, ti / tv))
  
  return(matrix_data)
}
calculate_tv_ti409 <- function(i) {
  # Create一个空的数据框用于存储Results
  tvti_df <- data.frame(gene = character(), value = numeric(), stringsAsFactors = FALSE)
  
  ### 从 input_all 中Filter出SpecifyGene的数据 ###
  input1 <- input_all %>% filter(GENE == i,group=="Carrier")
  
  ### 对数据进行汇总 ###
  input1_summary <- input1 %>%
    group_by(type2) %>%
    summarise(total_counts = sum(counts))
  
  ### 将汇总后的数据转tv/ti ###
  matrix_data <- input1_summary %>%
    pivot_wider(names_from = type2, values_from = total_counts) %>% 
    ### 如果 tv 列不存在，则Create并赋值为 0 ###
    mutate(tv = ifelse("tv" %in% colnames(.), tv, 0)) %>%
    ### 计算 tv_ti，如果 tv=0 则设为 NA ###
    mutate(GENE = i,
           tv_ti = ifelse(tv == 0, NA_real_, ti / tv))
  
  return(matrix_data)
}

tv_ti_df<- data.frame()
for (i in c(coding_bed,"Overall")) {
  print(i)
  df <- calculate_tv_ti(i)
  tv_ti_df<- rbind(tv_ti_df,df)
}

tv_ti_df409<- data.frame()
for (i in c(coding_bed,"Overall")) {
  print(i)
  df409 <- calculate_tv_ti409(i)
  tv_ti_df409<- rbind(tv_ti_df409,df409)
}

### plot ###
all_data_type1 <- all_data_type1020 %>% left_join(tv_ti_df,by="GENE") %>% 
  filter(GENE %in% c(coding_bed,"Overall"))

P1 <- ggplot(all_data_type1, aes(x = proportion, y = GENE, fill = type1)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("red", "#aed83c", "#13b433", "#5db0da", "#fabf00", "#f2c5c5"),name = NULL) +
  labs(x = "Proportion of substitutions", y = "Coding GENE", title = "Mutational spectrum of the coding GENE of hypermutated mitochondrial genomes") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(
    # axis.ticks.length.x = unit(0.03,'cm'),
    # axis.ticks.y = element_blank(),
    axis.text.x = element_text(size=12,face = 'bold'),
    axis.title.x = element_text(size = 15,face = 'bold'),
    axis.text.y = element_text(size = 12,face = 'bold'),
    axis.title.y = element_text(size = 15,face = 'bold'),
    axis.line = element_line(linewidth=1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 0.5),
    legend.direction = "vertical")+
  scale_y_discrete(expand = expansion(add = c(0, 0)))   # 使用discrete scale
  # annotate("text", x = 1.05 , y = 1,label = "18.3",colour="black")+
  # annotate("text", x = 1.05 , y = 2,label = "18.0",colour="black")+
  # annotate("text", x = 1.05 , y = 3,label = "23.4",colour="red")+
  # annotate("text", x = 1.05 , y = 4,label = "18.5",colour="black")+
  # annotate("text", x = 1.05 , y = 5,label = "20.8",colour="red")+
  # annotate("text", x = 1.05 , y = 6,label = "7.2",colour="black")+
  # annotate("text", x = 1.05 , y = 7,label = "8.2",colour="black")+
  # annotate("text", x = 1.05 , y = 8,label = "5.5",colour="black")+
  # annotate("text", x = 1.05 , y = 9,label = "14.0",colour="black")+
  # annotate("text", x = 1.05 , y = 10,label = "10.1",colour="black")+
  # annotate("text", x = 1.05 , y = 11,label = "N/A",colour="red")+
  # annotate("text", x = 1.05 , y = 12,label = "7.6",colour="black")+
  # annotate("text", x = 1.05 , y = 13,label = "8.0",colour="black")+
  # annotate("text", x = 1.05 , y = 14,label = "10.4",colour="black")



P1

ggsave(P1,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure1E.substitutions_cutoff202512.pdf",
       device = pdf,width = 8,height = 6,dpi = 600)


input_type <- df1020 %>% 
  mutate(
    OXPHS = factor(OXPHS, levels = c("OXPHS", "D_loop", "tRNA", "rRNA", "intergenic"))
  ) %>% 
  group_by(OXPHS,type1) %>% 
  mutate(counts = n()) %>% 
  distinct(OXPHS,type1,counts) %>% 
  group_by(OXPHS) %>% 
  arrange(counts, .by_group = TRUE) %>%  # 按 counts 升序排列
  ungroup() %>%
  mutate(type1 = factor(type1, levels = unique(type1)))  # 固定因子顺序

P2 <- ggplot(input_type,aes(x=OXPHS,y=counts,fill=type1))+
  geom_bar(stat = 'identity', 
           ### 柱状图Position并排: ###
           position = 'dodge', #使用position=position_dodge(width=0.9),可使组内柱子间隔,自行试一下。
           width = 0.8,      #Set柱子width,使变量之间分开
           color='black')+        
  geom_text(aes(label=counts),size=4,
            position = position_dodge(width = 0.8), #相应的Annotationwidth也Adjust
            vjust=-0.3)+    #调节Annotationheight
  labs(x=NULL,y = "Counts of heteroplasmic variants")+
  theme_classic()+  
  # theme(axis.text = element_text(colour = 'black'))+
  theme(
    # axis.ticks.length.x = unit(0.03,'cm'),
    # axis.ticks.y = element_blank(),
    # axis.text.x = element_text(size=12,face = 'bold'),
    # axis.title.x = element_text(size = 15,face = 'bold'),
    # axis.text.y = element_text(size = 12,face = 'bold'),
    # axis.title.y = element_text(size = 15,face = 'bold'),
    # axis.line = element_line(linewidth=1, colour = "black"),
    # axis.ticks = element_line(color = "black", size = 0.5),
    legend.position = c(0.95, 0.95),  # 图例放在右上角（坐标范围：0~1）
    legend.justification = c(1, 1),    # 对齐方式（右上角）
    legend.title = element_blank(),    # 确保图例标题为空
    legend.direction = "vertical")+
  scale_fill_manual(values = c("red", "#aed83c", "#13b433", "#5db0da", "#fabf00", "#f2c5c5"))+
  scale_y_continuous(expand = c(0, 0),limits = c(0,80))
  
P2 
  
  
  
ggsave(P2,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure1E.substitutions_type1_cutoff202512.pdf",
       device = pdf,width = 8,height = 6,dpi = 600)

  
  
  

##### Plot409标准化后的堆砌柱状图#####

P3 <- ggplot(all_data_type409, aes(x = proportion, y = GENE, fill = type1)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("red", "#aed83c", "#13b433", "#5db0da", "#fabf00", "#f2c5c5"),name = NULL) +
  labs(x = "Proportion of substitutions", y = "Coding Genes", title = "Mutational spectrum of the coding genes of hypermutated mitochondrial genomes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(
    # axis.ticks.length.x = unit(0.03,'cm'),
    # axis.ticks.y = element_blank(),
    axis.text.x = element_text(size=12,face = 'bold'),
    axis.title.x = element_text(size = 15,face = 'bold'),
    axis.text.y = element_text(size = 12,face = 'bold'),
    axis.title.y = element_text(size = 15,face = 'bold'),
    axis.line = element_line(linewidth=1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 0.5),
    legend.direction = "vertical")+
  scale_y_discrete(expand = expansion(add = c(0, 0)))+ # 使用discrete scale
  annotate("text", x = 1.05 , y = 1,label = "19.0",colour="black")+
  annotate("text", x = 1.05 , y = 2,label = "38.0",colour="red")+
  annotate("text", x = 1.05 , y = 3,label = "10.8",colour="black")+
  annotate("text", x = 1.05 , y = 4,label = "23.0",colour="red")+
  annotate("text", x = 1.05 , y = 5,label = "14.5",colour="black")+
  annotate("text", x = 1.05 , y = 6,label = "6.9",colour="black")+
  annotate("text", x = 1.05 , y = 7,label = "8.5",colour="black")+
  annotate("text", x = 1.05 , y = 8,label = "5.7",colour="black")+
  annotate("text", x = 1.05 , y = 9,label = "11.0",colour="black")+
  annotate("text", x = 1.05 , y = 10,label = "15.4",colour="black")+
  annotate("text", x = 1.05 , y = 11,label = "13.0",colour="black")+
  annotate("text", x = 1.05 , y = 12,label = "7.6",colour="black")+
  annotate("text", x = 1.05 , y = 13,label = "41.0",colour="red")+
  annotate("text", x = 1.05 , y = 14,label = "10.5",colour="black")



P3

ggsave(P3,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure3D.substitutions409_202512.pdf",
       device = pdf,width = 8,height = 6,dpi = 600)


## Save所有数据框
setwd("D:\\biosoft\\1000thal\\mtDNA\\R script/")
save.image("substitutions.RData")

