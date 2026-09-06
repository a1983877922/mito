rm(list=ls())#clear Global Environment
# Load packages
library(readxl)
library(tidyr)
library(data.table)
library(tibble)
library(dplyr)
#####
header1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\header.1020",sep = "\t")
header409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\409/header.409",sep = "\t")
sampID1020 <- header1020[7:1026]
sampID409 <- header409[7:415]
hl254 <- read_excel("D:\\biosoft\\1000thal\\mtDNA\\manuscript/Table 5.All of 254 heteroplasmic variants in β-thalassemia.xlsx")
#
# hl1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020.mt.ano.filter.hl2zero") %>% 
#   filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
#   mutate(V3=paste(V2,V4,V5,sep = "_")) %>%
#   # filter(rowSums(. >= 0.05) > 0) %>%
#   filter(rowSums(across(7:ncol(.), ~ . >= 0.05)) > 0) %>%  # 只对第7列到最后一列进行Filter
#   distinct(.keep_all = TRUE,V3) %>% 
#   filter(!grepl(",",V5))

ad1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\1020mtDNA_out\\vcfout/1020.mt.ano.filter.ad2zero") %>% 
  filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
  mutate(V3=paste(V2,V4,V5,sep = "_")) %>%
  # filter(rowSums(. >= 0.05) > 0) %>%
  # filter(rowSums(across(7:ncol(.), ~ . >= 0.05)) > 0) %>%  # 只对第7列到最后一列进行Filter
  distinct(.keep_all = TRUE,V3) %>% 
  filter(!grepl(",",V5))

# hl409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\409\\409.mt.ano.filter.hl2zero") %>% 
#   filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
#   mutate(V3=paste(V2,V4,V5,sep = "_")) %>%
#   # filter(rowSums(. >= 0.05) > 0) %>%
#   filter(rowSums(across(7:ncol(.), ~ . >= 0.05)) > 0) %>%  # 只对第7列到最后一列进行Filter
#   distinct(.keep_all = TRUE,V3) %>% 
#   filter(!grepl(",",V5))
hl1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020.mt.ano.filter.ft2onefilt")
hl409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\409.mt.ano.filter.ft2onefilt") 

colname1020 <- c("chrom","pos","id","ref","alt","info",sampID1020)
colname409 <- c("chrom","pos","id","ref","alt","info",sampID409)
colnames(hl1020) <- colname1020
colnames(ad1020) <- colname1020
colnames(hl409) <- colname409


# #将大于等于0.05的值变为0
hl1020[ , 7:1026][hl1020[ , 7:1026] <= 0.1] <- 0
hl409[ , 7:415][hl409[ , 7:415] <= 0.1] <- 0

hl1020[ , 7:1026][hl1020[ , 7:1026] >= 0.9] <- 0
hl409[ , 7:415][hl409[ , 7:415] >= 0.9] <- 0
hl1020_raw <- hl1020
# mtDNA gene features
mtgff3 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020bedout\\mt.gff3.bed",sep = "\t")
mtgff3$V2 <- mtgff3$V2 + 1
mtgff3$"with" <- mtgff3$V3 - mtgff3$V2 + 1
mtgff3[1,6] <- "D_loop"
mtgff3[39,6] <- "D_loop"
colnames(mtgff3) <- c("chrom","start","end","id","strand","gene","with")
coding_bed <- c("ND1","ND2","COX1","COX2","ATP6","COX3","ND3","ND4","ND5","ND6","CYTB","ATP8","ND4L")
##
setDT(hl1020)
setDT(hl409)
setDT(mtgff3)
#
hl1020[mtgff3, GENE := i.gene, on = .(pos >= start, pos <= end)] 
hl409[mtgff3, GENE := i.gene, on = .(pos >= start, pos <= end)]

hl1020$GENE[is.na(hl1020$GENE)] <- "intergenic"
hl409$GENE[is.na(hl409$GENE)] <- "intergenic"
###
child_info <- subset(hl1020,select = c("chrom","pos","id","ref","alt","info"))
parents_info <- subset(hl409,select = c("chrom","pos","id","ref","alt","info"))


all_id <- full_join(child_info,parents_info,by="id") %>% 
  mutate(chrom = coalesce(chrom.x, chrom.y),
         pos = coalesce(pos.x,pos.y),
         ref = coalesce(ref.x,ref.y),
         alt = coalesce(alt.x,alt.y),
         info = coalesce(info.x,info.y)) %>% 
  select(c(chrom,pos,id,ref,alt,info))

re_colname1020 <- colname1020[7:1026]

bed_df <- data.frame(Chrom = character(),
                     Start = numeric(),
                     End = numeric(),
                     GENE = character(),
                     HL = numeric(),
                     Sample = character(),
                     id = character(),
                     info = character(),
                     type = character(),
                     stringsAsFactors = FALSE)

re_colname409 <- colname409[7:415]

bed_df2 <- data.frame(Chrom = character(),
                      Start = numeric(),
                      End = numeric(),
                      GENE = character(),
                      HL = numeric(),
                      Sample = character(),
                      id=character(),
                      info = character(),
                      type = character(),
                      stringsAsFactors = FALSE)
####
for (i in 1:length(re_colname1020)) {
  child <- as.character(re_colname1020[i])
  #
  child_col <- subset(hl1020,select = c(child,"GENE"))
  child_df <- cbind(child_info,child_col)
  
  snv_df <- full_join(child_df,all_id,by="id") %>% 
    mutate(chrom = coalesce(chrom.x, chrom.y),
           pos = coalesce(pos.x,pos.y),
           ref = coalesce(ref.x,ref.y),
           alt = coalesce(alt.x,alt.y),
           info = coalesce(info.x,info.y)) %>% 
    select(-c(chrom.x,pos.x,ref.x,alt.x,info.x,
              chrom.y,pos.y,ref.y,alt.y,info.y)) %>% 
    mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>% 
    filter(!grepl(",", id)) %>% 
    # filter(!(nchar(alt) != 1 | nchar(ref) != 1)) %>% 
    filter(!!sym(child)>0.05) %>% 
    filter(!!sym(child)<0.95)
  
  bed_output <- snv_df %>%
    mutate(
      Chrom = "mtDNA",                  # BED 第1列：染色体
      Start = pos - 1,                # BED 第2列：起始位置（0-based）
      End = pos + nchar(alt) - 1,     # BED 第3列：结束位置（计算变异长度）
      GENE = GENE,  # BED 第4列：名称（自定义格式）
      HL = !!sym(child),           # BED 第5列：分值（这里用HL值）
      Sample = child,                  # BED 第6列：链（默认未知）
      id = id,
      info=info ,
      type = ifelse(nchar(alt) == 1 & nchar(ref) == 1,"SNV","INDEL") %>% as.character()
    ) %>%
    select(Chrom, Start, End, GENE, HL, Sample,id,info,type)  # 标准BED6格式
  
  bed_df <- bind_rows(
    bed_df,  # 原有数据（如果有）
    bed_output
  )
}


# for (i in 1:length(re_colname409)) {
#   parents <- as.character(re_colname409[i])
#   
#   parents_col <- subset(hl409,select = c(parents,"GENE"))
#   parents_df <- cbind(parents_info,parents_col)
#   
#   snv_df <- full_join(parents_df,all_id,by="id") %>% 
#     mutate(chrom = coalesce(chrom.x, chrom.y),
#            pos = coalesce(pos.x,pos.y),
#            ref = coalesce(ref.x,ref.y),
#            alt = coalesce(alt.x,alt.y),
#            info = coalesce(info.x,info.y)) %>% 
#     select(-c(chrom.x,pos.x,ref.x,alt.x,info.x,
#               chrom.y,pos.y,ref.y,alt.y,info.y)) %>% 
#     mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>% 
#     filter(!grepl(",", id)) %>% 
#     # filter(!(nchar(alt) != 1 | nchar(ref) != 1)) %>% 
#     filter(!!sym(parents)>0.05) %>% 
#     filter(!!sym(parents)<0.95)
#   
#   bed_output2 <- snv_df %>%
#     mutate(
#       Chrom = "mtDNA",                  # BED 第1列：染色体
#       Start = pos - 1,                # BED 第2列：起始位置（0-based）
#       End = pos + nchar(alt) - 1,     # BED 第3列：结束位置（计算变异长度）
# GENE = GENE,  # BED 第4列：名称（自定义格式）
#       HL = !!sym(parents),           # BED 第5列：分值（这里用HL值）
#       Sample = parents,                    # BED 第6列：链（默认未知）
#       id = id,
#       info=info ,
#       type = ifelse(nchar(alt) == 1 & nchar(ref) == 1,"SNV","INDEL") %>% as.character()
#     ) %>%
#     select(Chrom, Start, End, GENE, HL, Sample,id,info,type)  # 标准BED6格式
#   
#   bed_df2 <- bind_rows(
# bed_df2,  # 原有数据（如果有）
#     bed_output2
#   )
#   
#   
# }


# write.table(bed_df,"D:/biosoft/1000thal/mtDNA/1020 HL per sample.tsv",sep = "\t")
bed2freq <- bed_df %>%
  group_by(id) %>%
  mutate(sum = n()) %>%
  distinct(End,id,sum)%>%
  filter(sum >10)


# bed2pos1 <- bed_df %>% 
#   filter(id == "14755_A_C") %>% 
#   select(Sample,HL) %>% 
#   rename(HL14755 = HL ,
#          HID = Sample)
# 
# bed2pos2 <- bed_df %>% 
#   filter(id == "14775_T_C") %>% 
#   select(Sample,HL) %>% 
#   rename(HL14775 = HL ,
#          HID = Sample)
# 
# bed2pos3 <- bed_df %>% 
#   filter(id == "16179_CAA_C") %>% 
#   select(Sample,HL) %>% 
#   rename(HL16179 = HL ,
#          HID = Sample)
# bed2pos4 <- bed_df %>% 
#   filter(id == "16183_A_C") %>% 
#   select(Sample,HL) %>% 
#   rename(HL16183_A_C = HL ,
#          HID = Sample)
# bed2pos5 <- bed_df %>% 
#   filter(id == "16183_A_ACCC") %>% 
#   select(Sample,HL) %>% 
#   rename(HL16183_A_ACCC = HL ,
#          HID = Sample)
# bed2pos6 <- bed_df %>% 
#   filter(id == "16183_A_ACCCC") %>% 
#   select(Sample,HL) %>% 
#   rename(HL16183_A_ACCCC = HL ,
#          HID = Sample)
# bed2pos7 <- bed_df %>% 
#   filter(id == "16183_A_AC") %>% 
#   select(Sample,HL) %>% 
#   rename(HL16183_A_AC = HL ,
#          HID = Sample)
# 
# 
# merge_df <- t(sampID1020) %>% 
#   as.data.frame()%>% 
#   rename(HID=V1) %>% 
#   left_join(bed2pos1,by="HID") %>% 
#   left_join(bed2pos2,by="HID") %>% 
#   left_join(bed2pos3,by="HID") %>% 
#   left_join(bed2pos4,by="HID") %>% 
#   left_join(bed2pos5,by="HID") %>% 
#   left_join(bed2pos6,by="HID") %>% 
#   left_join(bed2pos7,by="HID") 


merge_df2 <- hl1020_raw %>% 
  select(-c("chrom","pos","ref","alt","info")) %>% 
  t() %>% 
  as.data.frame() %>% 
  ### 使用 setNames Set列名并Remove第一行 ###
  { 
    data <- .
    colnames(data) <- data[1, ]
    data <- data[-1, ]
    data
  } %>% 
  rownames_to_column("HID") %>% 
  ### 转换所有列为数值型（因为转置后可能变成字符型） ###
  mutate(across(-HID, as.numeric)) %>% 
  ### 新建所有行的Mean列 ###
  mutate(mean_all = rowMeans(select(., -HID), na.rm = TRUE))%>% 
  select(HID,mean_all)

merge_df2pos <- hl1020_raw %>% 
  select(-c("chrom","pos","ref","alt","info")) %>% 
  t() %>% 
  as.data.frame() %>% 
  ### 使用 setNames Set列名并Remove第一行 ###
  { 
    data <- .
    colnames(data) <- data[1, ]
    data <- data[-1, ]
    data
  } %>% 
  rownames_to_column("HID") %>% 
  ### 转换所有列为数值型（因为转置后可能变成字符型） ###
  mutate(across(-HID, as.numeric)) %>% 
  ### 新建所有行的Mean列 ###
  select(HID,"14766_C_T","16179_CAA_C", "16192_C_CT","16183_A_C")



merge_df3 <- hl1020_raw %>%
  filter(id %in% hl254$ID) %>% 
  select(-c("chrom","pos","ref","alt","info")) %>% 
  t() %>% 
  as.data.frame() %>% 
  ### 使用 setNames Set列名并Remove第一行 ###
  { 
    data <- .
    colnames(data) <- data[1, ]
    data <- data[-1, ]
    data
  } %>% 
  rownames_to_column("HID") %>% 
  ### 转换所有列为数值型（因为转置后可能变成字符型） ###
  mutate(across(-HID, as.numeric)) %>% 
  ### 新建所有行的Mean列 ###
  mutate(mean_het = rowMeans(select(., -HID), na.rm = TRUE)) %>% 
  select(HID,mean_het)



merge_df <- hl1020_raw %>% 
  filter(id %in% bed2freq$id) %>% 
  select(-c("chrom","pos","ref","alt","info")) %>% 
  t() %>% 
  as.data.frame() %>% 
  ### 使用 setNames Set列名并Remove第一行 ###
  { 
    data <- .
    colnames(data) <- data[1, ]
    data <- data[-1, ]
    data
  } %>% 
  rownames_to_column("HID") %>% 
  ### 转换所有列为数值型（因为转置后可能变成字符型） ###
  mutate(across(-HID, as.numeric)) %>% 
  ### 新建所有行的Mean列 ###
  mutate(mean_COM = rowMeans(select(., -HID), na.rm = TRUE)) %>% 
  ### 新建adfandaded列的Mean ###
  mutate(mean_CYTB = rowMeans(select(., "14766_C_T","16179_CAA_C", "16192_C_CT","16183_A_C"), na.rm = TRUE)) %>% 
  left_join(merge_df2,by="HID") %>% 
  left_join(merge_df3,by="HID") %>% 
  select(c("HID","mean_COM", "mean_CYTB" ,"mean_het", "mean_all" )) %>% 
  mutate(across(-1, ~ -log(.x ))) 

merge_ad2 <- ad1020%>% 
  select(-c("chrom","pos","ref","alt","info")) %>% 
  t() %>% 
  as.data.frame() %>% 
  ### 使用 setNames Set列名并Remove第一行 ###
  { 
    data <- .
    colnames(data) <- data[1, ]
    data <- data[-1, ]
    data
  } %>% 
  rownames_to_column("HID") %>% 
  ### 转换所有列为数值型（因为转置后可能变成字符型） ###
  mutate(across(-HID, ~ {
    sapply(., function(cell) {
      if(cell == "0" || is.na(cell)) {
        return(0)  # 直接Return数值0
      } else {
        nums <- as.numeric(strsplit(cell, ",")[[1]])
        return(nums[2] / (nums[1] + nums[2]))
      }
    })
  })) %>% 
  ### 新建所有行的Mean列 ###
  mutate(mean_all = rowMeans(select(., -HID), na.rm = TRUE)) %>% 
  select(HID,mean_all)

merge_ad2pos <- ad1020%>% 
  select(-c("chrom","pos","ref","alt","info")) %>% 
  t() %>% 
  as.data.frame() %>% 
  ### 使用 setNames Set列名并Remove第一行 ###
  { 
    data <- .
    colnames(data) <- data[1, ]
    data <- data[-1, ]
    data
  } %>% 
  rownames_to_column("HID") %>% 
  ### 转换所有列为数值型（因为转置后可能变成字符型） ###
  mutate(across(-HID, ~ {
    sapply(., function(cell) {
      if(cell == "0" || is.na(cell)) {
        return(0)  # 直接Return数值0
      } else {
        nums <- as.numeric(strsplit(cell, ",")[[1]])
        return(nums[2] / (nums[1] + nums[2]))
      }
    })
  })) %>% 
  select(HID,"14766_C_T","16179_CAA_C", "16192_C_CT","16183_A_C") %>% 
  rename_with(~ paste0(., "_ad"), .cols = -1) 

merge_ad3 <- ad1020 %>% 
  filter(id %in% hl254$ID) %>% 
  select(-c("chrom","pos","ref","alt","info")) %>% 
  t() %>% 
  as.data.frame() %>% 
  ### 使用 setNames Set列名并Remove第一行 ###
  { 
    data <- .
    colnames(data) <- data[1, ]
    data <- data[-1, ]
    data
  } %>% 
  rownames_to_column("HID") %>% 
  ### 转换所有列为数值型（因为转置后可能变成字符型） ###
  mutate(across(-HID, ~ {
    sapply(., function(cell) {
      if(cell == "0" || is.na(cell)) {
        return(0)  # 直接Return数值0
      } else {
        nums <- as.numeric(strsplit(cell, ",")[[1]])
        return(nums[2] / (nums[1] + nums[2]))
      }
    })
  })) %>% 
  ### 新建所有行的Mean列 ###
  mutate(mean_het = rowMeans(select(., -HID), na.rm = TRUE)) %>% 
  select(HID,mean_het)


merge_ad <- ad1020 %>% 
  filter(id %in% bed2freq$id) %>% 
  select(-c("chrom","pos","ref","alt","info")) %>% 
  t() %>% 
  as.data.frame() %>% 
  { 
    colnames(.) <- .[1, ]
    .[-1, , drop = FALSE]
  } %>% 
  rownames_to_column("HID") %>% 
  ### 一次性处理所有数值列 ###
  mutate(across(-HID, ~ {
    sapply(., function(cell) {
      if(cell == "0" || is.na(cell)) {
        return(0)  # 直接Return数值0
      } else {
        nums <- as.numeric(strsplit(cell, ",")[[1]])
        return(nums[2] / (nums[1] + nums[2]))
      }
    })
  })) %>% 
  ### 直接转换为数值型，避免重复操作 ###
  mutate(across(-HID, as.numeric)) %>% 
  ### 新建所有行的Mean列 ###
  mutate(mean_COM = rowMeans(select(., -HID), na.rm = TRUE)) %>% 
  ### 新建特定列的Mean ###
  mutate(mean_CYTB = rowMeans(select(., any_of(c("14766_C_T","16179_CAA_C", "16192_C_CT","16183_A_C"))), na.rm = TRUE)) %>% 
  left_join(merge_ad2, by = "HID") %>% 
  left_join(merge_ad3, by = "HID") %>% 
  rename_with(~ paste0(., "_ad"), .cols = -1) %>% 
  select(c("HID","mean_COM_ad", "mean_CYTB_ad" ,"mean_het_ad", "mean_all_ad" )) %>% 
  mutate(across(-1, ~ -log(.x ))) 

###

raw_df <- hl1020_raw %>% 
  select(-c("chrom","pos","ref","alt","info")) %>% 
  t() %>% 
  as.data.frame() %>% 
  ### 使用 setNames Set列名并Remove第一行 ###
  { 
    data <- .
    colnames(data) <- data[1, ]
    data <- data[-1, ]
    data
  } %>% 
  rownames_to_column("HID") %>% 
  ### 转换所有列为数值型（因为转置后可能变成字符型） ###
  mutate(across(-HID, as.numeric)) %>% 
  select("HID","14766_C_T","16179_CAA_C", "16192_C_CT","16183_A_C") %>% 
  mutate(across(-1, ~ -log(.x ))) 

raw_ad <- ad1020 %>% 
  filter(id %in% bed2freq$id) %>% 
  select(-c("chrom","pos","ref","alt","info")) %>% 
  t() %>% 
  as.data.frame() %>% 
  { 
    colnames(.) <- .[1, ]
    .[-1, , drop = FALSE]
  } %>% 
  rownames_to_column("HID") %>% 
  ### 一次性处理所有数值列 ###
  mutate(across(-HID, ~ {
    sapply(., function(cell) {
      if(cell == "0" || is.na(cell)) {
        return(0)  # 直接Return数值0
      } else {
        nums <- as.numeric(strsplit(cell, ",")[[1]])
        return(nums[2] / (nums[1] + nums[2]))
      }
    })
  })) %>% 
  ### 直接转换为数值型，避免重复操作 ###
  mutate(across(-HID, as.numeric)) %>% 
  select("HID","14766_C_T","16179_CAA_C", "16192_C_CT","16183_A_C") %>% 
  rename_with(~ paste0(., "_ad"), .cols = -1) %>% 
  mutate(across(-1, ~ -log(.x ))) 


merge_pos <- left_join(merge_ad2pos,merge_df2pos,by="HID") 
merge_raw <- left_join(raw_ad,raw_df,by="HID")

write.table(merge_df,"D:/biosoft/1000thal/mtDNA/gwas/mean HL per sample.tsv",sep = "\t")
write.table(merge_ad,"D:/biosoft/1000thal/mtDNA/gwas/mean ad per sample.tsv",sep = "\t")
# write.table(merge_raw ,"D:/biosoft/1000thal/mtDNA/gwas/log raw ad and hl per sample.tsv",sep = "\t")
write.table(merge_pos  ,"D:/biosoft/1000thal/mtDNA/gwas/raw ad and hl per sample.tsv",sep = "\t")

