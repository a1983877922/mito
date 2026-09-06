rm(list=ls())
library(dplyr)
library(tidyverse)
library(ggrepel)
library(patchwork)
library(readxl)
library(writexl)
library(openxlsx)
library(rstatix)
library(ggpubr)
library(data.table)
# 
## Load cached, skip recomputation
# load("D:\\biosoft\\1000thal\\mtDNA\\R script/figur3D.mutation_per_family.RData")
header1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\header.1020",sep = "\t")
header409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\409/header.409",sep = "\t")
sampID1020 <- header1020[7:1026]
sampID409 <- header409[7:415]
sampID58 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\58norm_people\\finalout/58.header",header = F) %>%
  t() %>% 
  as.data.frame()
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


hl58<- read.table("D:\\biosoft\\1000thal\\mtDNA\\58norm_people\\finalout/58norm.mt.filter.hl2zero") %>%
  filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
  mutate(V3=paste(V2,V4,V5,sep = "_")) %>%
  # filter(rowSums(. >= 0.05) > 0) %>%
  filter(rowSums(across(6:ncol(.), ~ . >= 0.05)) > 0) %>%  # 只对第7列到最后一列进行Filter
  distinct(.keep_all = TRUE,V3) %>%
  filter(!grepl(",",V5))
# hl1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020.mt.ano.filter.ft2onefilt")
# hl409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\409.mt.ano.filter.ft2onefilt") 
# hl58 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\58.mt.ano.filter.ft2onefilt")  

colname1020 <- c("chrom","pos","id","ref","alt","info",sampID1020)
colname409 <- c("chrom","pos","id","ref","alt","info",sampID409)
colname58 <- c("chrom","pos","id","ref","alt",sampID58)
colnames(hl1020) <- colname1020
colnames(hl409) <- colname409
colnames(hl58) <- colname58

### 将大于等于0.05的值变为0 ###
hl1020[ , 7:1026][hl1020[ , 7:1026] <= 0.05] <- 0
hl409[ , 7:415][hl409[ , 7:415] <= 0.05] <- 0
hl58[ , 6:63][hl58[ , 6:63] <= 0.05] <- 0

hl1020[ , 7:1026][hl1020[ , 7:1026] >= 0.95] <- 0
hl409[ , 7:415][hl409[ , 7:415] >= 0.95] <- 0
hl58[ , 6:63][hl58[ , 6:63] >= 0.95] <- 0


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
setDT(hl58)
setDT(mtgff3)

#
hl1020[mtgff3, GENE := i.gene, on = .(pos >= start, pos <= end)] 
hl409[mtgff3, GENE := i.gene, on = .(pos >= start, pos <= end)]
hl58[mtgff3, GENE := i.gene, on = .(pos >= start, pos <= end)]


hl1020$GENE[is.na(hl1020$GENE)] <- "intergenic"
hl409$GENE[is.na(hl409$GENE)] <- "intergenic"
hl58$GENE[is.na(hl58$GENE)] <- "intergenic"
###
child_info <- subset(hl1020,select = c("chrom","pos","id","ref","alt","info"))
parents_info <- subset(hl409,select = c("chrom","pos","id","ref","alt","info"))
norms_info <- subset(hl58,select = c("chrom","pos","id","ref","alt"))

all_id <- full_join(child_info,parents_info,norm_info,by="id") %>% 
  mutate(chrom = coalesce(chrom.x, chrom.y),
         pos = coalesce(pos.x,pos.y),
         ref = coalesce(ref.x,ref.y),
         alt = coalesce(alt.x,alt.y),
         info = coalesce(info.x,info.y)) %>% 
  select(c(chrom,pos,id,ref,alt,info))
re_colname1020 <- colname1020[7:1026]
count_df <- data.frame(id = NA, counts_snv = NA,counts_indel = NA, counts_ox = NA, counts_ncr = NA,stringsAsFactors = FALSE)

re_colname409 <- colname409[7:415]
count_df2 <- data.frame(id = NA, counts_snv = NA,counts_indel = NA,counts_ox = NA,counts_ncr = NA,stringsAsFactors = FALSE)

re_colname58 <- colname58[6:63]
count_df3 <- data.frame(id = NA, counts_snv = NA,counts_indel = NA,counts_ox = NA,counts_ncr = NA,stringsAsFactors = FALSE)

####
for (i in 1:length(re_colname1020)) {
  child <- as.character(re_colname1020[i])
  ### 向数据框中Add一行空数据 ###
  count_df <- rbind(count_df, data.frame(id = NA, counts_snv = NA, counts_indel = NA,counts_ox = NA,counts_ncr = NA, stringsAsFactors = FALSE))
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
    filter(!(nchar(alt) != 1 | nchar(ref) != 1)) %>% 
    filter(!!sym(child)>0.05) %>% 
    filter(!!sym(child)<0.95)
  
  indel_df <- full_join(child_df,all_id,by="id") %>% 
    mutate(chrom = coalesce(chrom.x, chrom.y),
           pos = coalesce(pos.x,pos.y),
           ref = coalesce(ref.x,ref.y),
           alt = coalesce(alt.x,alt.y),
           info = coalesce(info.x,info.y)) %>% 
    select(-c(chrom.x,pos.x,ref.x,alt.x,info.x,
              chrom.y,pos.y,ref.y,alt.y,info.y)) %>% 
    mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>% 
    filter(!grepl(",", id)) %>% 
    filter(nchar(alt) != 1 | nchar(ref) != 1) %>% 
    filter(!!sym(child)>0.05) %>% 
    filter(!!sym(child)<0.95)
  
  OXPHOS_df <- full_join(child_df,all_id,by="id") %>% 
    mutate(chrom = coalesce(chrom.x, chrom.y),
           pos = coalesce(pos.x,pos.y),
           ref = coalesce(ref.x,ref.y),
           alt = coalesce(alt.x,alt.y),
           info = coalesce(info.x,info.y)) %>% 
    select(-c(chrom.x,pos.x,ref.x,alt.x,info.x,
              chrom.y,pos.y,ref.y,alt.y,info.y)) %>% 
    mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>% 
    filter(!grepl(",", id)) %>% 
    filter(GENE %in% coding_bed) %>% 
    filter(!!sym(child)>0.05) %>% 
    filter(!!sym(child)<0.95)
  
  NCR_df <- full_join(child_df,all_id,by="id") %>% 
    mutate(chrom = coalesce(chrom.x, chrom.y),
           pos = coalesce(pos.x,pos.y),
           ref = coalesce(ref.x,ref.y),
           alt = coalesce(alt.x,alt.y),
           info = coalesce(info.x,info.y)) %>% 
    select(-c(chrom.x,pos.x,ref.x,alt.x,info.x,
              chrom.y,pos.y,ref.y,alt.y,info.y)) %>% 
    mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>% 
    filter(!grepl(",", id)) %>% 
    filter(GENE == "D_loop") %>% 
    filter(!!sym(child)>0.05) %>% 
    filter(!!sym(child)<0.95)
  
  count_df$id[i] <- child
  count_df$counts_snv[i] <- nrow(snv_df)
  count_df$counts_indel[i] <- nrow(indel_df) 
  count_df$counts_ox[i] <- nrow(OXPHOS_df)
  count_df$counts_ncr[i] <- nrow(NCR_df)
}


for (i in 1:length(re_colname409)) {
  parents <- as.character(re_colname409[i])
  ### 向数据框中Add一行空数据 ###
  count_df2 <- rbind(count_df2, data.frame(id = NA, counts_snv = NA, counts_indel = NA,counts_ox = NA,counts_ncr = NA, stringsAsFactors = FALSE))
  #
  parents_col <- subset(hl409,select = c(parents,"GENE"))
  parents_df <- cbind(parents_info,parents_col)
  
  snv_df <- full_join(parents_df,all_id,by="id") %>% 
    mutate(chrom = coalesce(chrom.x, chrom.y),
           pos = coalesce(pos.x,pos.y),
           ref = coalesce(ref.x,ref.y),
           alt = coalesce(alt.x,alt.y),
           info = coalesce(info.x,info.y)) %>% 
    select(-c(chrom.x,pos.x,ref.x,alt.x,info.x,
              chrom.y,pos.y,ref.y,alt.y,info.y)) %>% 
    mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>% 
    filter(!grepl(",", id)) %>% 
    filter(!(nchar(alt) != 1 | nchar(ref) != 1)) %>% 
    filter(!!sym(parents)>0.05) %>% 
    filter(!!sym(parents)<0.95)
  
  indel_df <- full_join(parents_df,all_id,by="id") %>% 
    mutate(chrom = coalesce(chrom.x, chrom.y),
           pos = coalesce(pos.x,pos.y),
           ref = coalesce(ref.x,ref.y),
           alt = coalesce(alt.x,alt.y),
           info = coalesce(info.x,info.y)) %>% 
    select(-c(chrom.x,pos.x,ref.x,alt.x,info.x,
              chrom.y,pos.y,ref.y,alt.y,info.y)) %>% 
    mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>% 
    filter(!grepl(",", id)) %>% 
    filter(nchar(alt) != 1 | nchar(ref) != 1) %>% 
    filter(!!sym(parents)>0.05) %>% 
    filter(!!sym(parents)<0.95)
  
  OXPHOS_df <- full_join(parents_df,all_id,by="id") %>% 
    mutate(chrom = coalesce(chrom.x, chrom.y),
           pos = coalesce(pos.x,pos.y),
           ref = coalesce(ref.x,ref.y),
           alt = coalesce(alt.x,alt.y),
           info = coalesce(info.x,info.y)) %>% 
    select(-c(chrom.x,pos.x,ref.x,alt.x,info.x,
              chrom.y,pos.y,ref.y,alt.y,info.y)) %>% 
    mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>% 
    filter(!grepl(",", id)) %>% 
    filter(GENE %in% coding_bed) %>% 
    filter(!!sym(parents)>0.05) %>% 
    filter(!!sym(parents)<0.95)
  
  NCR_df <- full_join(parents_df,all_id,by="id") %>% 
    mutate(chrom = coalesce(chrom.x, chrom.y),
           pos = coalesce(pos.x,pos.y),
           ref = coalesce(ref.x,ref.y),
           alt = coalesce(alt.x,alt.y),
           info = coalesce(info.x,info.y)) %>% 
    select(-c(chrom.x,pos.x,ref.x,alt.x,info.x,
              chrom.y,pos.y,ref.y,alt.y,info.y)) %>% 
    mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>% 
    filter(!grepl(",", id)) %>% 
    filter(GENE == "D_loop") %>% 
    filter(!!sym(parents)>0.05) %>% 
    filter(!!sym(parents)<0.95)
  
  count_df2$id[i] <- parents
  count_df2$counts_snv[i] <- nrow(snv_df)
  count_df2$counts_indel[i] <- nrow(indel_df) 
  count_df2$counts_ox[i] <- nrow(OXPHOS_df)
  count_df2$counts_ncr[i] <- nrow(NCR_df)
}


for (i in 1:length(re_colname58)) {
  norms <- as.character(re_colname58[i])
  ### 向数据框中Add一行空数据 ###
  count_df3 <- rbind(count_df3, data.frame(id = NA, counts_snv = NA, counts_indel = NA,counts_ox = NA,counts_ncr = NA, stringsAsFactors = FALSE))
  #
  norms_col <- subset(hl58,select = c(norms,"GENE"))
  norms_df <- cbind(norms_info,norms_col)
  
  snv_df <- full_join(norms_df,all_id,by="id") %>% 
    mutate(chrom = coalesce(chrom.x, chrom.y),
           pos = coalesce(pos.x,pos.y),
           ref = coalesce(ref.x,ref.y),
           alt = coalesce(alt.x,alt.y)) %>% 
    select(-c(chrom.x,pos.x,ref.x,alt.x,
              chrom.y,pos.y,ref.y,alt.y)) %>% 
    mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>% 
    filter(!grepl(",", id)) %>% 
    filter(!(nchar(alt) != 1 | nchar(ref) != 1)) %>% 
    filter(!!sym(norms)>0.05) %>% 
    filter(!!sym(norms)<0.95)
  
  indel_df <- full_join(norms_df,all_id,by="id") %>% 
    mutate(chrom = coalesce(chrom.x, chrom.y),
           pos = coalesce(pos.x,pos.y),
           ref = coalesce(ref.x,ref.y),
           alt = coalesce(alt.x,alt.y)) %>% 
    select(-c(chrom.x,pos.x,ref.x,alt.x,
              chrom.y,pos.y,ref.y,alt.y)) %>% 
    mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>% 
    filter(!grepl(",", id)) %>% 
    filter(nchar(alt) != 1 | nchar(ref) != 1) %>% 
    filter(!!sym(norms)>0.05) %>% 
    filter(!!sym(norms)<0.95)
  
  OXPHOS_df <- full_join(norms_df,all_id,by="id") %>% 
    mutate(chrom = coalesce(chrom.x, chrom.y),
           pos = coalesce(pos.x,pos.y),
           ref = coalesce(ref.x,ref.y),
           alt = coalesce(alt.x,alt.y)) %>% 
    select(-c(chrom.x,pos.x,ref.x,alt.x,
              chrom.y,pos.y,ref.y,alt.y)) %>% 
    mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>% 
    filter(!grepl(",", id)) %>% 
    filter(GENE %in% coding_bed) %>% 
    filter(!!sym(norms)>0.05) %>% 
    filter(!!sym(norms)<0.95)
  
  NCR_df <- full_join(norms_df,all_id,by="id") %>% 
    mutate(chrom = coalesce(chrom.x, chrom.y),
           pos = coalesce(pos.x,pos.y),
           ref = coalesce(ref.x,ref.y),
           alt = coalesce(alt.x,alt.y)) %>% 
    select(-c(chrom.x,pos.x,ref.x,alt.x,
              chrom.y,pos.y,ref.y,alt.y)) %>% 
    mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>% 
    filter(!grepl(",", id)) %>% 
    filter(GENE == "D_loop") %>% 
    filter(!!sym(norms)>0.05) %>% 
    filter(!!sym(norms)<0.95)
  
  count_df3$id[i] <- norms
  count_df3$counts_snv[i] <- nrow(snv_df)
  count_df3$counts_indel[i] <- nrow(indel_df) 
  count_df3$counts_ox[i] <- nrow(OXPHOS_df)
  count_df3$counts_ncr[i] <- nrow(NCR_df)
}

count_df_new <- count_df %>% 
  mutate(counts_other = counts_snv + counts_indel - counts_ox )

count_df_new2 <- count_df2 %>% 
  mutate(counts_other = counts_snv + counts_indel - counts_ox )

count_df_new3 <- count_df3 %>% 
  mutate(counts_other = counts_snv + counts_indel - counts_ox )

count_df1020 <- count_df %>% 
  mutate(group = "Thalassemia") %>% 
  mutate(
    interval_snv = cut(counts_snv,breaks = seq(-1, 45), by = 2),
    interval_indel = cut(counts_indel,breaks = seq(-1, 45), by = 2),
    interval_OX = cut(counts_ox,breaks = seq(-1, 45), by = 2)
  ) %>% 
  filter(!is.na(id))

count_df409 <- count_df2 %>% 
  mutate(group = "Carriers") %>% 
  mutate(
    interval_snv = cut(counts_snv,breaks = seq(-1, 45), by = 2),
    interval_indel = cut(counts_indel,breaks = seq(-1, 45), by = 2),
    interval_OX = cut(counts_ox,breaks = seq(-1, 45), by = 2)
  ) %>% 
  filter(!is.na(id))

count_df58 <- count_df3 %>% 
  mutate(group = "Healthy") %>% 
  mutate(
    interval_snv = cut(counts_snv,breaks = seq(-1, 45), by = 2),
    interval_indel = cut(counts_indel,breaks = seq(-1, 45), by = 2),
    interval_OX = cut(counts_ox,breaks = seq(-1, 45), by = 2)
  ) %>% 
  filter(!is.na(id))

count_merge <- rbind(count_df1020 ,count_df409,count_df58)
count_snv <- count_merge %>% 
  group_by(group,interval_snv) %>% 
  mutate(sample_counts = n(),
         type="snv") %>%
  ungroup() %>% 
  select(interval_snv,sample_counts,type,group) %>% 
  distinct() %>% 
  rename(interval = interval_snv)

count_indel <- count_merge %>% 
  group_by(group,interval_indel) %>% 
  mutate(sample_counts = n(),
         type="indel") %>%
  ungroup() %>% 
  select(interval_indel,sample_counts,type,group) %>% 
  distinct() %>% 
  rename(interval = interval_indel)

count_ox <- count_merge %>% 
  group_by(group,interval_OX) %>% 
  mutate(sample_counts = n(),
         type="OXPHOS") %>%
  ungroup() %>% 
  select(interval_OX,sample_counts,type,group) %>% 
  distinct() %>% 
  rename(interval = interval_OX)
#
input <- rbind(count_snv,count_indel,count_ox) 
input$group <- factor(input$group, 
                      levels = c("Thalassemia", "Carriers", "Healthy"))
### Create一个新的列 interval2，初始值与 interval 相同 ###
input$interval2 <- as.character(input$interval)

### Define a function来解析区间并提取右端点的数字 ###
extract_right_endpoint <- function(interval) {
  as.numeric(sub("\\((.*?),(.*)\\]", "\\2", interval))
}

### 提取每个区间的右端点数字 ###
right_endpoints <- sapply(as.character(input$interval), extract_right_endpoint)

# # 使用 ifelse 函数来赋值
# input$interval2 <- ifelse(right_endpoints >= 12, '>11', as.character(sub("\\((.*?),(.*)\\]", "\\2", input$interval)))
# 
# input$interval2 <- factor(input$interval2, levels = c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10','11', '>11'))
input$interval2 <- factor(right_endpoints)


input1 <- input %>% 
  group_by(interval2,type,group) %>% 
  mutate(sample_counts2 = sum(sample_counts))


snv_input <- input1 %>% 
  filter(type=="snv") %>% 
  group_by(group) %>% 
  mutate(percent = sample_counts / sum(sample_counts)) 
# %>% 
#   filter(interval2 %in% c('0', '1', '2', '3', '4', '5', '6'))

indel_input <- input1 %>% 
  filter(type=="indel") %>% 
  group_by(group) %>% 
  mutate(percent = sample_counts / sum(sample_counts))

OXPHOS_input <- input1 %>% 
  filter(type=="OXPHOS") %>% 
  group_by(group) %>% 
  mutate(percent = sample_counts / sum(sample_counts))

# write.table(count_df,file = "D:\\biosoft\\1000thal\\mtDNA\\1020mutations per sample.tsv",row.names = F,quote = F,sep = "\t")
# write.table(count_df2,file = "D:\\biosoft\\1000thal\\mtDNA\\409mutations per sample.tsv",row.names = F,quote = F,sep = "\t")


### 使用ggplot2Plot双柱状图 ###
P1 <- ggplot(snv_input, aes(x = interval2, y = percent, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("#e95f5c","#EDBE6C","#4DAB8F"),
                    labels=c('Thalassemia', 'Carriers',"Healthy"),
                    name="") +
  labs(title = NULL,
       x = "Heteroplasmy count",
       y = "Percentage") +
  annotate("text", x=7, y=0.08, label="SNV",
           size=8, color="black", angle=270, fontface="bold") + # Add右侧垂直标题
  theme_classic()+
  theme(
    axis.text.x = element_text(size=10,face = 'bold'),
    axis.title.x = element_text(size = 15,face = 'bold'),
    axis.text.y = element_text(size = 10,face = 'bold'),
    axis.title.y = element_text(size = 15,face = 'bold'),
    axis.line = element_line(linewidth=1, colour = "black"),
    panel.grid.major = element_blank(), # Remove主网格线
    panel.grid.minor = element_blank(), # Remove次网格线
    # panel.border = element_rect(color="black", fill=NA, size=2),
    legend.position = c(0.87,0.87)
  )+
  # scale_y_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), labels = scales::percent) +
  scale_x_discrete(expand = c(0,0), limits = as.character(0:7))  # Setx轴范围为0-18
  # scale_x_discrete(expand = c(0,0))



P1

# ggsave(P1,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure2A2a.snv_heteropasmy_counts.pdf",
#        device = pdf,width = 12,height = 8,dpi = 600)




P2 <- ggplot(indel_input, aes(x = interval2, y = percent, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("#e95f5c","#EDBE6C","#4DAB8F"),
                    labels=c('Thalassemia', 'Carriers',"Healthy"),
                    name="") +
  labs(title = NULL,
       x = NULL,
       y = "Percentage") +
  theme_classic()+
  annotate("text", x=6, y=0.4, label="INDEL",
           size=8, color="black", angle=270, fontface="bold") + # Add右侧垂直标题
  
  theme(plot.title = element_blank(), # 隐藏默认标题
        plot.margin = margin(t=10, r=40, b=10, l=10))+ # 增加右侧边距以容纳标题
  theme(
    axis.text.x = element_text(size=10,face = 'bold'),
    axis.title.x = element_text(size = 15,face = 'bold'),
    axis.text.y = element_text(size = 10,face = 'bold'),
    axis.title.y = element_text(size = 15,face = 'bold'),
    axis.line = element_line(linewidth=1, colour = "black"),
    panel.grid.major = element_blank(), # Remove主网格线
    panel.grid.minor = element_blank(), # Remove次网格线
    # panel.border = element_rect(color="black", fill=NA, size=2),
    legend.position = c(0.87,0.87)
  )+
  # scale_y_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), labels = scales::percent) +
  scale_x_discrete(expand = c(0,0), limits = as.character(0:6))  # Setx轴范围为0-18
  # scale_x_discrete(expand = c(0,0))

P2


# ggsave(P2,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure2A2b.indel_heteropasmy_counts.pdf",
#        device = pdf,width = 12,height = 8,dpi = 600)
##

P3 <- ggplot(OXPHOS_input, aes(x = interval2, y = percent, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("#e95f5c","#EDBE6C","#4DAB8F"),
                    labels=c('Thalassemia', 'Carriers',"Healthy"),
                    name="") +
  labs(title = NULL,
       x = NULL,
       y = "Percentage") +
  theme_classic()+
  annotate("text", x=10, y=0.15, label="OXPHOS",
           size=6, color="black", angle=0, fontface="bold") + # Add右侧垂直标题
  
  theme(plot.title = element_blank(), # 隐藏默认标题
        plot.margin = margin(t=10, r=40, b=10, l=10))+ # 增加右侧边距以容纳标题
  theme(
    axis.text.x = element_text(size=10,face = 'bold'),
    axis.title.x = element_text(size = 15,face = 'bold'),
    axis.text.y = element_text(size = 10,face = 'bold'),
    axis.title.y = element_text(size = 15,face = 'bold'),
    axis.line = element_line(linewidth=1, colour = "black"),
    panel.grid.major = element_blank(), # Remove主网格线
    panel.grid.minor = element_blank(), # Remove次网格线
    # panel.border = element_rect(color="black", fill=NA, size=2),
    legend.position = c(0.87,0.87)
  )+
  # scale_y_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), labels = scales::percent) +
  scale_x_discrete(expand = c(0,0))

P3

# ggsave(P3,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure2A2b.OXPHOS_heteropasmy_counts.pdf",
#        device = pdf,width = 12,height = 8,dpi = 600)
P2/P1+plot_layout(guides='collect')+
  plot_annotation(tag_levels = 'a')&  theme(legend.position='top')

pdf("D:\\biosoft\\1000thal\\mtDNA\\figure/figure2A2.mutations_heteroplasmy_counts.pdf", width = 10, height = 8)
P2/P1+plot_layout(guides='collect')+
  plot_annotation(tag_levels = 'a')&  theme(legend.position='top')
dev.off()


## Save所有数据框
setwd("D:\\biosoft\\1000thal\\mtDNA\\R script/")
save.image("figur3D.mutation_per_family.RData")


