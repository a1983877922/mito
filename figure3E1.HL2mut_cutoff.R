rm(list=ls())
library(dplyr)
library(data.table)
library(janitor)
library(tibble)
### 最终Results ###
# dfsum <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\mut_profile.tsv",sep = "\t",header = T)

# AC <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\merge.AC0.05.tsv",sep = "\t",header = T) 
# hl1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020.mt.ano.filter.hl2zero") %>% 
#   filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
#   mutate(V3=paste(V2,V4,V5,sep = "_")) %>%
#   filter(rowSums(select(., V7:ncol(.)) >= 0.05) > 0) %>% 
#   # filter(rowSums(. >= 0.05) > 0) %>%
#   distinct(.keep_all = TRUE,V3) %>% 
#   filter(!grepl(",",V5))
hl1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020.mt.ano.filter.ft2onefilt")
hl409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\409.mt.ano.filter.ft2onefilt") 
hl58<- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\58.mt.ano.filter.ft2onefilt") 

header1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\header.1020",sep = "\t")
colnames(hl1020) <- header1020

# hl409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut/409/409.mt.ano.filter.hl2zero") %>% 
#   filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
#   mutate(V3=paste(V2,V4,V5,sep = "_")) %>%
#   # filter(rowSums(. >= 0.05) > 0) %>%
#   filter(rowSums(across(7:ncol(.), ~ . >= 0.05)) > 0) %>%  # 只对第7列到最后一列进行Filter
#   distinct(.keep_all = TRUE,V3) %>% 
#   filter(!grepl(",",V5))

header409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\409/header.409",sep = "\t")
colnames(hl409) <- header409

# hl58<- read.table("D:\\biosoft\\1000thal\\mtDNA\\58norm_people\\finalout/58norm.mt.filter.hl2zero") %>% 
#   filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
#   mutate(V3=paste(V2,V4,V5,sep = "_")) %>%
#   # filter(rowSums(. >= 0.05) > 0) %>%
#   filter(rowSums(across(6:ncol(.), ~ . >= 0.05)) > 0) %>%  # 只对第7列到最后一列进行Filter
#   distinct(.keep_all = TRUE,V3) %>% 
#   filter(!grepl(",",V5))
sampID58 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\58norm_people\\finalout/58.header",header = F) %>%
  # t() %>% 
  # as.data.frame() %>% 
  pull()
colnames(hl58) <- c("CHROM","POS","ID","REF","ALT",sampID58)
# mtDNA gene features
mtgff3 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020bedout\\mt.gff3.bed",sep = "\t")
mtgff3$V2 <- mtgff3$V2 + 1
mtgff3$"with" <- mtgff3$V3 - mtgff3$V2 + 1
feature_start <- mtgff3$V2
feature_width <- mtgff3$"with"
mtgff3[1,6] <- "D_loop"
mtgff3[39,6] <- "D_loop"
colnames(mtgff3) <- c("chrom","start","end","id","strand","gene","with")




# bed <- c("ATP6", "ATP8", "COX1", "COX2", "COX3", "CYTB", "D_loop", "ND1", 
#          "ND2", "ND3", "ND4", "ND4L", "ND5", "ND6", "RNR1", "RNR2", "TRNA", 
#          "TRNC", "TRND", "TRNE", "TRNF", "TRNG", "TRNH", "TRNI", "TRNK", 
#          "TRNL1", "TRNL2", "TRNM", "TRNN", "TRNP", "TRNQ", "TRNR", "TRNS1", 
#          "TRNS2", "TRNT", "TRNV", "TRNW", "TRNY")

coding_bed <- c("ATP6","COX3","ND2","ND5","ATP8","CYTB",
                "ND3","ND6","COX1","ND4","COX2","ND1","ND4L")


bed <- c("ATP6","COX3","ND2","ND5","TRNA","TRNF","TRNK","TRNN","TRNS1","TRNW","ATP8","CYTB","ND3",
         "ND6","TRNC","TRNG","TRNL1","TRNP","TRNS2","TRNY","COX1","D_loop","ND4","RNR1","TRND","TRNH",
         "TRNL2","TRNQ","TRNT","COX2","ND1","ND4L","RNR2","TRNE","TRNI","TRNM","TRNR","TRNV")

rate <- c("transcript_ablation","splice_acceptor_variant","splice_donor_variant","stop_gained",
          "frameshift_variant","stop_lost","start_lost","transcript_amplification","feature_elongation",
          "feature_truncation","inframe_insertion","inframe_deletion","missense_variant","protein_altering_variant")

###
hl1020_filt <- hl1020 
hl409_filt <- hl409 
hl58_filt <- hl58
###
# hl1020_filt <- hl1020 %>% filter(! POS %in% filter)
# hl409_filt <- hl409 %>% filter(! POS %in% filter)

# hl1020_filt <- hl1020_filt %>%
#   mutate(geo = case_when(
#     grepl(",", ALT) ~ "Multiallelic",  # 如果ALTcontains逗号
#     nchar(ALT) > nchar(REF) ~ "dup",   # 如果ALT字符数大于REF
#     nchar(ALT) < nchar(REF) ~ "indel", # 如果ALT字符数小于REF
#     TRUE ~ "snp"                       # 其他情况
#   )) %>% 
#   relocate(geo, .after = 1)  # 将geo列移动到第1列之后(即第2Position)
# 
# hl409_filt <- hl409_filt %>%
#   mutate(geo = case_when(
#     grepl(",", ALT) ~ "Multiallelic",  # 如果ALTcontains逗号
#     nchar(ALT) > nchar(REF) ~ "dup",   # 如果ALT字符数大于REF
#     nchar(ALT) < nchar(REF) ~ "indel", # 如果ALT字符数小于REF
#     TRUE ~ "snp"                       # 其他情况
#   )) %>% 
#   relocate(geo, .after = 1)  # 将geo列移动到第1列之后(即第2Position)

#

hl1020_filt2het <- hl1020_filt %>%
  mutate(across(7:ncol(.), ~ case_when(
    . < 0.1 ~ 0,
    . > 0.9 ~ 0,#
    TRUE ~ 1
  )))

hl409_filt2het <- hl409_filt %>%
  mutate(across(7:ncol(.), ~ case_when(
    . < 0.1 ~ 0,
    . > 0.9 ~ 0,#
    TRUE ~ 1
  )))

hl58_filt2het <- hl58_filt %>%
  mutate(across(6:ncol(.), ~ case_when(
    . < 0.1 ~ 0,
    . > 0.9 ~ 0,#
    TRUE ~ 1
  )))

setDT(hl1020_filt2het)
setDT(hl409_filt2het)
setDT(hl58_filt2het)
setDT(mtgff3)

hl1020_filt2het[mtgff3, GENE := i.gene, on = .(POS >= start, POS <= end)] 
hl409_filt2het[mtgff3, GENE := i.gene, on = .(POS >= start, POS <= end)] 
hl58_filt2het[mtgff3, GENE := i.gene, on = .(POS >= start, POS <= end)] 

recolname <- function(df){
  ### 获取所有列名 ###
  all_cols <- names(df)
  
  ### 确定GENE列的Position ###
  gene_pos <- which(all_cols == "GENE")
  
  ### 重新排列列顺序：CHROM, GENE, 其他列(排除CHROMandGENE) ###
  new_order <- c("CHROM", "GENE", all_cols[!all_cols %in% c("CHROM", "GENE")])
  
  ### 应用新的列顺序 ###
  df <- df[, ..new_order]
}


### 获取所有列名 ###
# all_cols <- names(hl1020_filt2het)
# 
# # 确定GENE列的Position
# gene_pos <- which(all_cols == "GENE")
# 
# # 重新排列列顺序：CHROM, GENE, 其他列(排除CHROM和GENE)
# new_order <- c("CHROM", "GENE", all_cols[!all_cols %in% c("CHROM", "GENE")])
# 
# # 应用新的列顺序
# hl1020_filt2het <- hl1020_filt2het[, ..new_order]
hl1020_filt2het <- recolname(hl1020_filt2het)
hl409_filt2het <- recolname(hl409_filt2het)
hl58_filt2het <- recolname(hl58_filt2het)

hl1020_filt2het2mut <- hl1020_filt2het %>% 
  group_by(GENE) %>% 
  summarise(across(7:last_col(), \(x) sum(x, na.rm = TRUE)))
  # summarise(across(8:ncol(.)-1, sum, na.rm = TRUE))

hl409_filt2het2mut <- hl409_filt2het %>% 
  group_by(GENE) %>% 
  summarise(across(7:last_col(), \(x) sum(x, na.rm = TRUE)))
  # summarise(across(8:416, sum, na.rm = TRUE))

hl58_filt2het2mut <- hl58_filt2het %>% 
  group_by(GENE) %>% 
  summarise(across(6:last_col(), \(x) sum(x, na.rm = TRUE)))
# summarise(across(8:416, sum, na.rm = TRUE))

hl1020_filt2het2mut$GENE[38] <- "intergenic"
hl409_filt2het2mut$GENE[38] <- "intergenic"
hl58_filt2het2mut$GENE[28] <- "intergenic"

mut2het1020 <- t(hl1020_filt2het2mut) %>% 
  as.data.frame() %>% 
  row_to_names(row_number = 1) %>%   # 将第一行Set为列名
  rownames_to_column("SampleID")   # 将Row names转为列

mut2het409 <- t(hl409_filt2het2mut) %>% 
  as.data.frame() %>% 
  row_to_names(row_number = 1) %>%   # 将第一行Set为列名
  rownames_to_column("SampleID")   # 将Row names转为列

mut2het58 <- t(hl58_filt2het2mut) %>% 
  as.data.frame() %>% 
  row_to_names(row_number = 1) %>%   # 将第一行Set为列名
  rownames_to_column("SampleID")   # 将Row names转为列


final2het1020 <- mut2het1020 %>%
  mutate(across(
    .cols = -SampleID,  # 对所有列操作（或Specify列如 `-SampleID`）
    .fns = ~ case_when(
      . == "0" ~ NA_character_,  # 字符 "0" → NA
      . == "1" ~ "single",       # 字符 "1" → "single"
      is.na(.) ~ NA_character_,  # 保留原有 NA
      TRUE ~ "multi"             # 其他字符 → "multi"
    )
  ))


final2het409 <- mut2het409 %>%
  mutate(across(
    .cols = -SampleID,  # 对所有列操作（或Specify列如 `-SampleID`）
    .fns = ~ case_when(
      . == "0" ~ NA_character_,  # 字符 "0" → NA
      . == "1" ~ "single",       # 字符 "1" → "single"
      is.na(.) ~ NA_character_,  # 保留原有 NA
      TRUE ~ "multi"             # 其他字符 → "multi"
    )
  ))

final2het58 <- mut2het58 %>%
  mutate(across(
    .cols = -SampleID,  # 对所有列操作（或Specify列如 `-SampleID`）
    .fns = ~ case_when(
      . == "0" ~ NA_character_,  # 字符 "0" → NA
      . == "1" ~ "single",       # 字符 "1" → "single"
      is.na(.) ~ NA_character_,  # 保留原有 NA
      TRUE ~ "multi"             # 其他字符 → "multi"
    )
  ))

### Write ###
# write.table(final2het1020,file = "D:\\biosoft\\1000thal\\mtDNA\\genemut\\mut_profile2het1020cutoff202512.tsv",sep = "\t",
#             row.names = F,na = "",quote = F)
# 
# write.table(final2het409,file = "D:\\biosoft\\1000thal\\mtDNA\\genemut\\mut_profile2het409cutoff202512.tsv",sep = "\t",
#             row.names = F,na = "",quote = F)
# write.table(final2het58,file = "D:\\biosoft\\1000thal\\mtDNA\\genemut\\mut_profile2het58cutoff202512.tsv",sep = "\t",
#             row.names = F,na = "",quote = F)
