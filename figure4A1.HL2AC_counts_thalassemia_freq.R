rm(list=ls())
library(dplyr)

## Load cached, skip recomputation
# load("D:\\biosoft\\1000thal\\mtDNA\\R script/count_HL2AC&freq.RData")

header <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\header.1020",sep = "\t")
sampID <- header[1,7:1026]
# hl1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020.mt.ano.filter.hl2zero") %>% 
#   filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
#   mutate(V3=paste(V2,V4,V5,sep = "_")) %>%
#   filter(rowSums(. >= 0.05) > 0) %>%
#   distinct(.keep_all = TRUE,V3)
# 
# hl409<- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\409/409.mt.ano.filter.hl2zero") %>% 
#   filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
#   mutate(V3=paste(V2,V4,V5,sep = "_")) %>%
#   filter(rowSums(. >= 0.05) > 0) %>%
#   distinct(.keep_all = TRUE,V3)
# 
# hl58<- read.table("D:\\biosoft\\1000thal\\mtDNA\\58norm_people\\finalout/58norm.mt.filter.hl2zero") %>% 
#   filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
#   mutate(V3=paste(V2,V4,V5,sep = "_")) %>%
#   filter(rowSums(. >= 0.05) > 0) %>%
#   distinct(.keep_all = TRUE,V3)
# bed <- c("ATP6","COX3","ND2","ND5","TRNA","TRNF","TRNK","TRNN","TRNS1","TRNW","ATP8","CYTB","ND3",
#          "ND6","TRNC","TRNG","TRNL1","TRNP","TRNS2","TRNY","COX1","D_loop","ND4","RNR1","TRND","TRNH",
#          "TRNL2","TRNQ","TRNT","COX2","ND1","ND4L","RNR2","TRNE","TRNI","TRNM","TRNR","TRNV")
hl1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020.mt.ano.filter.ft2onefilt")
hl409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\409.mt.ano.filter.ft2onefilt") 
hl58<- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\58.mt.ano.filter.ft2onefilt") 
### 提取的各GeneVariant位点的表格的list ###
calculate_frequency <- function(df,cutoff1=0.05,cutoff2=0.95) {

  ### 将.Substitution为0 ###
  for (i in 1:nrow(df)) {
    for (j in 7:ncol(df)) {
      if (df[i,j] == ".") {
        df[i,j] <- 0
      }
    }
  }
  ### 提取hl 纯矩阵 ###
  dfsub <- subset(df, select = 7:ncol(df)) 
  dfsub2 <- subset(df, select = 1:6)
  
  ### 计算每行中大于0.05 小于0.95的数量 ###
  counts_per_row <- rowSums(dfsub > cutoff1 & dfsub < cutoff2)
  
  ### 计算每行中0的Frequency ###
  frequency_per_row <- counts_per_row / 1020

  ### 将FrequencyAdd到原始数据框的最后一列 ###
  dfsub2$Fre <- frequency_per_row
  
  colnames(dfsub2) <- c("CHROM","POS","ID","REF","ALT","ANNO","Freq")
  ### Return更新后的数据框 ###
  return(dfsub2)
}


### 计算AC_HET AC_HOM andgnomAD数据匹配 ###
calculate_AC <- function(df,cutoff2=0.95,cutoff1=0.01) {
  
  ### 将.Substitution为0 ###
  for (i in 1:nrow(df)) {
    for (j in 7:ncol(df)) {
      if (df[i,j] == ".") {
        df[i,j] <- 0
      }
    }
  }
  ### 提取hl 纯矩阵 ###
  dfsub <- subset(df, select = 7:ncol(df)) 
  dfsub2 <- subset(df, select = 1:6)
  
  ### 计算每行中大于0.95的数量 ###
  hom_per_row <- rowSums(dfsub >= cutoff2)
  het_per_row <- rowSums(dfsub > cutoff1 & dfsub < cutoff2  )
  max_per_row <- apply(dfsub, 1, max)
  # # 计算每行中0的Frequency
  # frequency_per_row <- counts_per_row / 1020
  
  ### 将FrequencyAdd到原始数据框的最后一列 ###
  dfsub2$AC_hom <- hom_per_row
  dfsub2$AC_het <- het_per_row
  dfsub2$max_HL <- max_per_row
  
  colnames(dfsub2) <- c("CHROM","POS","ID","REF","ALT","ANNO","AC_hom","AC_het","max_HL")
  ### Return更新后的数据框 ###
  return(dfsub2)
}
calculate_AC58 <- function(df,cutoff2=0.95,cutoff1=0.01) {
  
  ### 将.Substitution为0 ###
  for (i in 1:nrow(df)) {
    for (j in 7:ncol(df)) {
      if (df[i,j] == ".") {
        df[i,j] <- 0
      }
    }
  }
  ### 提取hl 纯矩阵 ###
  dfsub <- subset(df, select = 6:ncol(df)) 
  dfsub2 <- subset(df, select = 1:5)
  
  ### 计算每行中大于0.95的数量 ###
  hom_per_row <- rowSums(dfsub >= cutoff2)
  het_per_row <- rowSums(dfsub > cutoff1 & dfsub < cutoff2  )
  max_per_row <- apply(dfsub, 1, max)
  # # 计算每行中0的Frequency
  # frequency_per_row <- counts_per_row / 1020
  
  ### 将FrequencyAdd到原始数据框的最后一列 ###
  dfsub2$AC_hom <- hom_per_row
  dfsub2$AC_het <- het_per_row
  dfsub2$max_HL <- max_per_row
  
  colnames(dfsub2) <- c("CHROM","POS","ID","REF","ALT","AC_hom","AC_het","max_HL")
  ### Return更新后的数据框 ###
  return(dfsub2)
}


# dfsum_0.95 <- calculate_frequency(hl1020,0.95)
# dfsum_0.05 <- calculate_frequency(hl1020,cutoff1=0.05,cutoff2=0.95)
dfsum_0.1 <- calculate_frequency(hl1020,cutoff1=0.1,cutoff2=0.9)

# dfsum_AC0.05 <- calculate_AC(hl1020,cutoff2=0.95,cutoff1=0.05)
dfsum_AC0.1 <- calculate_AC(hl1020,cutoff2=0.9,cutoff1=0.1)

df409_AC0.1 <- calculate_AC(hl409,cutoff2=0.9,cutoff1=0.1)
df58_AC0.1 <- calculate_AC58(hl58,cutoff2=0.9,cutoff1=0.1)

sorted_df01 <- dfsum_0.1 %>%arrange(POS)
# sorted_df005 <- dfsum_0.05 %>%arrange(POS)

sorted_df01$ID <- paste(sorted_df01$POS, sorted_df01$REF,sorted_df01$ALT, sep="_")
# sorted_df005$ID <- paste(sorted_df005$POS, sorted_df005$REF,sorted_df005$ALT, sep="_")


# #
unique_rows01 <- sorted_df01 %>%
  distinct(ID, .keep_all = TRUE) %>%
  filter(Freq != 0)

# unique_rows005 <- sorted_df005 %>%
#   distinct(ID, .keep_all = TRUE) %>% 
#   filter(Freq != 0)

#
##
# dfsum_AC0.05_sorted <- dfsum_AC0.05 %>%
#   filter(!is.na(POS)) %>% 
#   arrange(POS) %>% 
#   mutate(ID = paste(POS,REF,ALT,sep = "_"),
#          AF_hom = AC_hom / 1020 ,
#          AF_het = AC_het / 1020) %>% 
#   distinct(ID, .keep_all = TRUE) %>% 
#   filter(! (AC_het == 0 & AC_hom == 0))

dfsum_AC0.1_sorted <- dfsum_AC0.1 %>%
  filter(!is.na(POS)) %>% 
  arrange(POS) %>% 
  mutate(ID = paste(POS,REF,ALT,sep = "_"),
         AF_hom = AC_hom / 1020 ,
         AF_het = AC_het / 1020) %>% 
  distinct(ID, .keep_all = TRUE) %>% 
  filter(! (AC_het == 0 & AC_hom == 0))

df409_AC0.1_sorted <- df409_AC0.1 %>%
  filter(!is.na(POS)) %>% 
  arrange(POS) %>% 
  mutate(ID = paste(POS,REF,ALT,sep = "_"),
         AF_hom = AC_hom / 409 ,
         AF_het = AC_het / 409) %>% 
  distinct(ID, .keep_all = TRUE) %>% 
  filter(! (AC_het == 0 & AC_hom == 0))

df58_AC0.1_sorted <- df58_AC0.1 %>%
  filter(!is.na(POS)) %>% 
  arrange(POS) %>% 
  mutate(ID = paste(POS,REF,ALT,sep = "_"),
         AF_hom = AC_hom / 58 ,
         AF_het = AC_het / 58) %>% 
  distinct(ID, .keep_all = TRUE) %>% 
  filter(! (AC_het == 0 & AC_hom == 0))
# test <- dfsum_AC0.1_sorted %>%
#   filter(!grepl(",",ALT)) %>%
#   filter(AC_het !=0)

heteroplamic409 <- df409_AC0.1_sorted %>%
  filter(!grepl(",",ALT)) %>%
  filter(AC_het !=0)

heteroplamic58 <- df58_AC0.1_sorted %>%
  filter(!grepl(",",ALT)) %>%
  filter(AC_het !=0)

# write.table(heteroplamic409,file = "D:\\biosoft\\1000thal\\mtDNA\\genemut\\409.heteroplamic variants.tsv",sep = "\t",
#             row.names = F,na = "",quote = F)
# write.table(heteroplamic58,file = "D:\\biosoft\\1000thal\\mtDNA\\genemut\\58.heteroplamic variants.tsv",sep = "\t",
#             row.names = F,na = "",quote = F)
## Save所有数据框
# setwd("D:\\biosoft\\1000thal\\mtDNA\\R script/")
# save.image("count_HL2AC&freq.RData")

### Write ###
# write.table(dfsum_AC0.05_sorted,file = "D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020.AC0.05.tsv",sep = "\t",
#             row.names = F,na = "",quote = F)
# 
# write.table(dfsum_AC0.1_sorted,file = "D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020.AC0.1_202512.tsv",sep = "\t",
#             row.names = F,na = "",quote = F)
# 
# write.table(unique_rows01,file = "D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020.Freq0.1_202512.tsv",sep = "\t",
#             row.names = F,na = "",quote = F)
# write.table(unique_rows005,file = "D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020.Freq0.05.tsv",sep = "\t",
#             row.names = F,na = "",quote = F)
