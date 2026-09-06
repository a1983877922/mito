rm(list=ls())#clear Global Environment
library(openxlsx)
library(rstatix)
library(ggpubr)
library(data.table)
library(purrr)
library(dplyr)
## Load cached, skip recomputation
# load("D:\\biosoft\\1000thal\\mtDNA\\R script/figur3D.mutation_per_family.RData")
header1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\header.1020",sep = "\t")
header409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\409/header.409",sep = "\t")
sampID1020 <- header1020[7:1026]
sampID409 <- header409[7:415]
sampID58 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\58norm_people\\finalout/58.header",header = F) %>%
  # t() %>% 
  # as.data.frame() %>% 
  pull()
#
hl1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020.mt.ano.filter.hl2zero") %>% 
  filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
  mutate(V3=paste(V2,V4,V5,sep = "_")) %>%
  # filter(rowSums(. >= 0.05) > 0) %>%
  filter(rowSums(across(7:ncol(.), ~ . >= 0.05)) > 0) %>%  # 只对第7列到最后一列进行Filter
  distinct(.keep_all = TRUE,V3) %>% 
  filter(!grepl(",",V5))

hl409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\409\\409.mt.ano.filter.hl2zero") %>% 
  filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
  mutate(V3=paste(V2,V4,V5,sep = "_")) %>%
  # filter(rowSums(. >= 0.05) > 0) %>%
  filter(rowSums(across(7:ncol(.), ~ . >= 0.05)) > 0) %>%  # 只对第7列到最后一列进行Filter
  distinct(.keep_all = TRUE,V3) %>% 
  filter(!grepl(",",V5))

hl58 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\58norm_people\\finalout/58norm.mt.filter.hl2zero") %>% 
  filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
  mutate(V3=paste(V2,V4,V5,sep = "_")) %>%
  # filter(rowSums(. >= 0.05) > 0) %>%
  filter(rowSums(across(7:ncol(.), ~ . >= 0.05)) > 0) %>%  # 只对第7列到最后一列进行Filter
  distinct(.keep_all = TRUE,V3) %>% 
  filter(!grepl(",",V5))

ft1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020.mt.ano.filter.ft2one") %>% 
  filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
  mutate(V3=paste(V2,V4,V5,sep = "_")) 

ft409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\409/409.mt.ano.filter.ft2one") %>% 
  filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
  mutate(V3=paste(V2,V4,V5,sep = "_")) 

ft58 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\58norm_people\\finalout/58.mt.ano.filter.ft2one") %>% 
  filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
  mutate(V3=paste(V2,V4,V5,sep = "_")) 

ft1020filt <- ft1020 %>%
  ### 步骤1：处理strand_bias ###
  mutate_all(~ ifelse(grepl("strand_bias", ., fixed = TRUE), 0, .)) 

ft409filt <- ft409 %>%
  ### 步骤1：处理strand_bias ###
  mutate_all(~ ifelse(grepl("strand_bias", ., fixed = TRUE), 0, .)) 

ft58filt <- ft58 %>%
  ### 步骤1：处理strand_bias ###
  mutate_all(~ ifelse(grepl("strand_bias", ., fixed = TRUE), 0, .)) 

df1 <- subset(ft1020filt,select = 7:ncol(ft1020filt))
df2 <- subset(ft409filt,select = 7:ncol(ft409filt))
df3 <- subset(ft58filt,select = 6:ncol(ft58filt))

freq_table <- df1 %>%
  ### 将数据框转换为长格式 ###
  tidyr::pivot_longer(everything(), values_to = "值") %>%
  ### 计数每个值的频数 ###
  count(值, name = "频数") %>%
  ### 按频数降序排列 ###
  arrange(desc(频数))

freq_table2 <- df2 %>%
  ### 将数据框转换为长格式 ###
  tidyr::pivot_longer(everything(), values_to = "值") %>%
  ### 计数每个值的频数 ###
  count(值, name = "频数") %>%
  ### 按频数降序排列 ###
  arrange(desc(频数))

freq_table3 <- df3 %>%
  ### 将数据框转换为长格式 ###
  tidyr::pivot_longer(everything(), values_to = "值") %>%
  ### 计数每个值的频数 ###
  count(值, name = "频数") %>%
  ### 按频数降序排列 ###
  arrange(desc(频数))

# Inspect results
freq_table
freq_table2
freq_table3

ft1020filt <- ft1020 %>%
  mutate(across(7:last_col(), ~ {
    ### 先将列转换为字符 ###
    x <- as.character(.)
    
    ### 使用case_when处理 ###
    result <- case_when(
      grepl("strand_bias", x, fixed = TRUE) ~ "0",
      x %in% c("ForceCalledHomoplasmy", "PASS") ~ "1",
      x %in% c("1", "0") ~ x,
      TRUE ~ "0"
    )
    
    ### 转换为数值，处理可能的NA ###
    as.numeric(result)
  }))

ft409filt <- ft409 %>%
  mutate(across(7:last_col(), ~ {
    ### 先将列转换为字符 ###
    x <- as.character(.)
    
    ### 使用case_when处理 ###
    result <- case_when(
      grepl("strand_bias", x, fixed = TRUE) ~ "0",
      x %in% c("ForceCalledHomoplasmy", "PASS") ~ "1",
      x %in% c("1", "0") ~ x,
      TRUE ~ "0"
    )
    
    ### 转换为数值，处理可能的NA ###
    as.numeric(result)
  }))

ft58filt <- ft58 %>%
  mutate(across(6:last_col(), ~ {
    ### 先将列转换为字符 ###
    x <- as.character(.)
    
    ### 使用case_when处理 ###
    result <- case_when(
      grepl("strand_bias", x, fixed = TRUE) ~ "0",
      x %in% c("ForceCalledHomoplasmy", "PASS") ~ "1",
      x %in% c("1", "0") ~ x,
      TRUE ~ "0"
    )
    
    ### 转换为数值，处理可能的NA ###
    as.numeric(result)
  }))

mergedf <- subset(hl1020,select = 3) %>% 
  left_join(ft1020filt,by="V3") %>% 
  select(V1, V2, V3, everything())

mergedf2 <- subset(hl409,select = 3) %>% 
  left_join(ft409filt,by="V3") %>% 
  select(V1, V2, V3, everything())

mergedf3 <- subset(hl58,select = 3) %>% 
  left_join(ft58filt,by="V3") %>% 
  select(V1, V2, V3, everything())

### 只处理第7列到最后一列 ###
result <- mergedf %>%
  ### 保留前6列不变 ###
  select(1:6) %>%
  ### 将第7列到最后一列对应相乘 ###
  bind_cols(
    map2_dfc(
      mergedf %>% select(7:last_col()),
      hl1020 %>% select(7:last_col()),
      ~ .x * .y
    )
  )

result2 <- mergedf2 %>%
  ### 保留前6列不变 ###
  select(1:6) %>%
  ### 将第7列到最后一列对应相乘 ###
  bind_cols(
    map2_dfc(
      mergedf2 %>% select(7:last_col()),
      hl409 %>% select(7:last_col()),
      ~ .x * .y
    )
  )

result3 <- mergedf3 %>%
  ### 保留前6列不变 ###
  select(1:5) %>%
  ### 将第7列到最后一列对应相乘 ###
  bind_cols(
    map2_dfc(
      mergedf3 %>% select(6:last_col()),
      hl58 %>% select(6:last_col()),
      ~ .x * .y
    )
  )

colname1020 <- c("chrom","pos","id","ref","alt","info",sampID1020)  
colnames(result) <- colname1020

colname409 <- c("chrom","pos","id","ref","alt","info",sampID409)  
colnames(result2) <- colname409

colname58<- c("chrom","pos","id","ref","alt",sampID58)  
colnames(result3) <- colname58

# write.table(result,"D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020.mt.ano.filter.ft2onefilt",sep = "\t",row.names = F,quote = F,col.names = FALSE)
# write.table(result2,"D:\\biosoft\\1000thal\\mtDNA\\genemut\\409.mt.ano.filter.ft2onefilt",sep = "\t",row.names = F,quote = F,col.names = FALSE)
# write.table(result3,"D:\\biosoft\\1000thal\\mtDNA\\genemut\\58.mt.ano.filter.ft2onefilt",sep = "\t",row.names = F,quote = F,col.names = FALSE)
