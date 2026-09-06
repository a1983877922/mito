rm(list=ls())
library(dplyr)
library(xlsx)
## Load cached, skip recomputation
# load("D:\\biosoft\\1000thal\\mtDNA\\R script/maternal_inherit.RData")
ped164 <- readxl::read_excel("D:\\biosoft\\1000thal\\mtDNA\\Maternal inheritance/232ped.xlsx",sheet = "164TRIOS_MOTHER")
ped68 <- readxl::read_excel("D:\\biosoft\\1000thal\\mtDNA\\Maternal inheritance/232ped.xlsx",sheet = "68SINGON")
header1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\header.1020",sep = "\t")
header409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\409/header.409",sep = "\t")
sampID1020 <- header1020[7:1026]
sampID409 <- header409[7:415]

# df1020AC0.05 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020.AC0.05.tsv",sep = "\t",header=T) %>%
#   filter(!grepl(",",ALT))

# %>% 
# filter(AC_het !=0 )

df1020AC0.1 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020.AC0.1_202512.tsv",sep = "\t",header=T) %>%
  filter(!grepl(",",ALT))%>%
  filter(AC_het !=0 )
# colnames(df1020AC0.05) <- c("CHROM", "POS", "ID", "REF", "ALT", "ANNO", "AC_hom1020", "AC_het1020", "max_HL1020","AF_hom1020", "AF_het1020")

# hl1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020.mt.ano.filter.hl2zero") %>% 
#   filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
#   mutate(V3=paste(V2,V4,V5,sep = "_")) %>%
#   # filter(rowSums(. >= 0.05) > 0) %>%
#   filter(rowSums(across(7:ncol(.), ~ . >= 0.05)) > 0) %>%  # 只对第7列到最后一列进行Filter
#   distinct(.keep_all = TRUE,V3) %>% 
#   filter(!grepl(",",V5))

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
colnames(hl409) <- colname409

### 将小于等于0.05的值变为0,大于等于0.95的值变为0 ###
# hl1020[ , 7:1026][hl1020[ , 7:1026] <= 0.05] <- 0
# hl409[ , 7:415][hl409[ , 7:415] <= 0.05] <- 0
# hl1020[ , 7:1026][hl1020[ , 7:1026] >= 0.95] <- 0
# hl409[ , 7:415][hl409[ , 7:415] >= 0.95] <- 0

### 将小于等于0.1的值变为0,大于等于0.9的值变为0 ###
hl1020[ , 7:1026][hl1020[ , 7:1026] <= 0.1] <- 0
hl409[ , 7:415][hl409[ , 7:415] <= 0.1] <- 0
hl1020[ , 7:1026][hl1020[ , 7:1026] >= 0.9] <- 0
hl409[ , 7:415][hl409[ , 7:415] >= 0.9] <- 0



ped <- rbind(ped164,ped68)

#
child_info <- subset(hl1020,select = c("chrom","pos","id","ref","alt","info"))
mother_info <- subset(hl409,select = c("chrom","pos","id","ref","alt","info"))

all_id <- full_join(child_info,mother_info,by="id") %>% 
  mutate(chrom = coalesce(chrom.x, chrom.y),
         pos = coalesce(pos.x,pos.y),
         ref = coalesce(ref.x,ref.y),
         alt = coalesce(alt.x,alt.y),
         info = coalesce(info.x,info.y)) %>% 
  dplyr::select(c(chrom,pos,id,ref,alt,info))

#
all_inherit_df <- all_id
all_dnm_df <- all_id
all_dnm_df2 <- all_id

for (i in 1:nrow(ped)) {
  child <- as.character(ped[i,'HID'])
  mother <- as.character(ped[i,'MHID'])
  
  #
  child_col <- subset(hl1020,select = child)
  child_df <- cbind(child_info,child_col)
  
  mother_col <- subset(hl409,select = mother)
  mother_df <- cbind(mother_info,mother_col)
  
  
  inherit_df <- full_join(child_df,mother_df,by="id") %>% 
    mutate(chrom = coalesce(chrom.x, chrom.y),
           pos = coalesce(pos.x,pos.y),
           ref = coalesce(ref.x,ref.y),
           alt = coalesce(alt.x,alt.y),
           info = coalesce(info.x,info.y)) %>% 
    dplyr::select(-c(chrom.x,pos.x,ref.x,alt.x,info.x,
                     chrom.y,pos.y,ref.y,alt.y,info.y)) %>% 
    mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>% 
    # filter((!!sym(child) > 0.1 & !!sym(mother) < 0.9) | ( !!sym(child) < 0.9 & !!sym(mother) > 0.1)
    filter(!!sym(child) != 0 & !!sym(mother) != 0)
  
  
  
  
  all_inherit_df <- left_join(all_inherit_df,inherit_df,by="id") %>% 
    dplyr::select(-c(chrom.y,pos.y,ref.y,alt.y,info.y)) %>% 
    rename(chrom = chrom.x,
           pos = pos.x,
           ref = ref.x,
           alt = alt.x,
           info = info.x)
  
  dnm_df<- full_join(child_df,mother_df,by="id") %>% 
    mutate(chrom = coalesce(chrom.x, chrom.y),
           pos = coalesce(pos.x,pos.y),
           ref = coalesce(ref.x,ref.y),
           alt = coalesce(alt.x,alt.y),
           info = coalesce(info.x,info.y)) %>% 
    dplyr::select(-c(chrom.x,pos.x,ref.x,alt.x,info.x,
                     chrom.y,pos.y,ref.y,alt.y,info.y)) %>% 
    mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>% 
    # filter((!!sym(mother)  <= 0.1 & !!sym(child) > 0.1) | (!!sym(mother)  >= 0.9 & !!sym(child) < 0.9))
    filter((!!sym(mother)  == 0 & !!sym(child) != 0) )
  
  all_dnm_df <- left_join(all_dnm_df,dnm_df,by="id") %>% 
    dplyr::select(-c(chrom.y,pos.y,ref.y,alt.y,info.y)) %>% 
    rename(chrom = chrom.x,
           pos = pos.x,
           ref = ref.x,
           alt = alt.x,
           info = info.x)
  dnm_df2<- full_join(child_df,mother_df,by="id") %>% 
    mutate(chrom = coalesce(chrom.x, chrom.y),
           pos = coalesce(pos.x,pos.y),
           ref = coalesce(ref.x,ref.y),
           alt = coalesce(alt.x,alt.y),
           info = coalesce(info.x,info.y)) %>% 
    dplyr::select(-c(chrom.x,pos.x,ref.x,alt.x,info.x,
                     chrom.y,pos.y,ref.y,alt.y,info.y)) %>% 
    mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>% 
    # filter(!(!!sym(child) ==  0 & !!sym(mother) ==  0)) %>% 
    # filter(!(!!sym(child) >=  0.95 & !!sym(mother) >=  0.95)) %>% 
    # filter(!(!!sym(child) ==  0 & c(!!sym(mother) <  0.95  & !!sym(mother) > 0.05 ))) 
    filter((!!sym(mother)  <= 0.05 & !!sym(child) >= 0.1) | (!!sym(mother)  >= 0.95 & !!sym(child) <= 0.9))
  
  all_dnm_df2 <- left_join(all_dnm_df2,dnm_df2,by="id") %>% 
    dplyr::select(-c(chrom.y,pos.y,ref.y,alt.y,info.y)) %>% 
    rename(chrom = chrom.x,
           pos = pos.x,
           ref = ref.x,
           alt = alt.x,
           info = info.x)
  
}



### 获取除了前6列外的所有列名 ###
cols_to_check <- colnames(all_inherit_df)[7:ncol(all_inherit_df)]


# ! (逻辑非) 可以反转这个向量，使TRUE变为FALSE，FALSE变为TRUE
# inherit_cleaned <- all_inherit_df[!rowSums(is.na(all_inherit_df[, cols_to_check])) == length(cols_to_check), ]
# inherit_cleaned_filter <- inherit_cleaned %>% 
#   filter(!grepl(",", id))


dnm_cleaned <- all_dnm_df[!rowSums(is.na(all_dnm_df[, cols_to_check])) == length(cols_to_check), ]

dnm_cleaned_filter <- dnm_cleaned %>% 
  filter(!grepl(",", id))


dnm_cleaned_filter_id <- dnm_cleaned_filter %>% 
  select(id) %>% 
  unlist()

allvariant <- df1020AC0.1 %>% 
  mutate(
    dnm = ifelse(ID %in% dnm_cleaned_filter_id ,"yes","no")
  ) 
dnm_cleaned2 <- all_dnm_df2[!rowSums(is.na(all_dnm_df2[, cols_to_check])) == length(cols_to_check), ]

dnm_cleaned_filter2 <- dnm_cleaned2 %>%
  filter(!grepl(",", id))



inherit_cleaned <- all_inherit_df[!rowSums(is.na(all_dnm_df[, cols_to_check])) == length(cols_to_check), ]

inherit_cleaned_filter <- inherit_cleaned %>%
  filter(!grepl(",", id))


inherit_final <- allvariant  %>%
  filter(dnm == "no")



# write.table(inherit_cleaned_filter,'D:\\biosoft\\1000thal/mtDNA/Maternal inheritance/inherit.tsv',sep = "\t",quote = F,row.names = F,na = "")
# write.table(inherit_final,'D:\\biosoft\\1000thal/mtDNA/Maternal inheritance/inherit_final202512.tsv',sep = "\t",quote = F,row.names = F,na = "")
# write.table(dnm_cleaned_filter,'D:\\biosoft\\1000thal/mtDNA/Maternal inheritance/dnm.tsv',sep = "\t",quote = F,row.names = F,na = "")
# write.table(dnm_cleaned_filter2,'D:\\biosoft\\1000thal/mtDNA/Maternal inheritance/dnm_abs.tsv',sep = "\t",quote = F,row.names = F,na = "")
# 
# write.table(dnm_cleaned_filter,'D:\\biosoft\\1000thal/mtDNA/Maternal inheritance/dnm_cutoff202512.tsv',sep = "\t",quote = F,row.names = F,na = "")

## Save所有数据框
setwd("D:\\biosoft\\1000thal\\mtDNA\\R script/")
save.image("maternal_inherit.RData")
