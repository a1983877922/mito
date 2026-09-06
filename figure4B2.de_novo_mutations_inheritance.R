rm(list=ls())
library(dplyr)
library(tidyverse)
library(ggrepel)
library(patchwork)
library(readxl)
library(data.table)
#
# dnm <- read.table('D:\\biosoft\\1000thal/mtDNA/Maternal inheritance/dnm.tsv',sep = "\t",header=T)
dnm <- read.table('D:\\biosoft\\1000thal/mtDNA/Maternal inheritance/dnm_cutoff202512.tsv',sep = "\t",header=T)
# dnm_abs <- read.table('D:\\biosoft\\1000thal/mtDNA/Maternal inheritance/dnm_abs.tsv',sep = "\t",header=T)
# hl254 <- read.table('D:\\biosoft\\1000thal/mtDNA/254variants.tsv',sep = "\t",header=T)
hl185 <- read.table('D:\\biosoft\\1000thal/mtDNA/254variants.tsv',sep = "\t",header=T)


missing_values_per_row <- rowSums(is.na(dnm))
# missing_values_per_row2 <- rowSums(is.na(dnm_abs))

dnm$num_family <- (464 - missing_values_per_row )/2
# dnm_abs$num_family <- (464 - missing_values_per_row2 )/2

dnm <- dnm %>%
  mutate(color = case_when(
    num_family == 1 ~ "#9BBBE1",
    num_family > 1 & num_family < 10 ~ "#E6B745",
    num_family >= 10 ~ "#B54764"
  )) %>% 
  mutate(family_freq = case_when(
    num_family == 1 ~ "Low",
    num_family > 1 & num_family < 10 ~ "median",
    num_family >= 10 ~ "High"
  ))  
variants_df <- dnm %>% 
  filter(num_family >=7)
# dnm_abs <- dnm_abs %>%
#   mutate(color = case_when(
#     num_family == 1 ~ "#9BBBE1",
#     num_family > 1 & num_family < 10 ~ "#E6B745",
#     num_family >= 10 ~ "#B54764"
#   )) %>% 
#   mutate(family_freq = case_when(
#     num_family == 1 ~ "Low",
#     num_family > 1 & num_family < 10 ~ "median",
#     num_family >= 10 ~ "High"
#   )) 


# mtDNA gene features
mtgff3 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020bedout\\mt.gff3.bed",sep = "\t")
mtgff3$V2 <- mtgff3$V2 + 1
mtgff3$"with" <- mtgff3$V3 - mtgff3$V2 + 1
mtgff3[1,6] <- "D_loop"
mtgff3[39,6] <- "D_loop"
colnames(mtgff3) <- c("chrom","start","end","id","strand","gene","with")


#
setDT(mtgff3)
setDT(dnm)
#
# dnm[mtgff3, GENE := i.gene, on = .(pos >= V2, pos <= V3)] 
dnm[mtgff3, GENE := i.gene, on = .(pos >= start, pos <= end)] 
dnm$GENE[is.na(dnm$GENE)] <- "Intergenic"


coding_bed <- c("ATP6","COX3","ND2","ND5","ATP8","CYTB",
                "ND3","ND6","COX1","ND4","COX2","ND1","ND4L")
# # Create一个空向量用于存储Results
# dnm$GENE <- NA
# 
# # Iterate over df 的每一行
# for (i in seq_len(nrow(dnm))) {
# # 获取当前行的 pos 值
#   pos <- dnm$pos[i]
#   
# # 在 wf 中查找 start <= pos <= end 的行
#   match_row <- mtgff3 %>% 
#     filter(V2 <= pos & pos <= V3)
#   
# # 如果找到了匹配的行（即 match_row 不为空）
#   if (nrow(match_row) > 0) {
# # 将匹配行的 tag 值赋给 df 的新列 tag
#     dnm$GENE[i] <- match_row$V6[1] # 取第一个匹配的 tag 值
#   }
# }
dnm <- as.data.frame(dnm)
dnm$OXPHOS <- NA
for (i in 1:nrow(dnm)) {
  if (dnm[i,"GENE"] %in% coding_bed)
  {
    dnm[i,"OXPHOS"] <- "YES"
  } else{dnm[i,"OXPHOS"] <- "NO"}
}

###

ox_dnm <- dnm %>% 
  filter(GENE == "D_loop") %>% 
  select(id) %>% 
  pull()
###
inherit <- hl185 %>% 
  filter(!ID %in% dnm$id) %>% 
  filter(GENE %in% coding_bed) 

oxphox <- hl185 %>% 
  filter(GENE %in% coding_bed) 
######
# dnm_abs$GENE <- NA

### Iterate over df 的每一行 ###
# for (i in seq_len(nrow(dnm_abs))) {
# # 获取当前行的 pos 值
#   pos <- dnm_abs$pos[i]
#   
# # 在 wf 中查找 start <= pos <= end 的行
#   match_row <- mtgff3 %>% 
#     filter(V2 <= pos & pos <= V3)
#   
# # 如果找到了匹配的行（即 match_row 不为空）
#   if (nrow(match_row) > 0) {
# # 将匹配行的 tag 值赋给 df 的新列 tag
#     dnm_abs$GENE[i] <- match_row$V6[1] # 取第一个匹配的 tag 值
#   }
# }
# 
# dnm_abs$OXPHOS <- NA
# for (i in 1:nrow(dnm_abs)) {
#   if (dnm_abs[i,"GENE"] %in% coding_bed)
#   {
#     dnm_abs[i,"OXPHOS"] <- "YES"
#   } else{dnm_abs[i,"OXPHOS"] <- "NO"}
# }


summarized <- dnm %>%
  group_by(GENE) %>%
  summarize(
    sum_pos=n(),
    sum_family1=sum(family_freq != "Low", na.rm = TRUE),
    sum_family2=sum(num_family >=4, na.rm = TRUE)
    ) %>% 
  # summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
  arrange(desc(sum_pos))

# summarized_abs <- dnm_abs %>%
#   group_by(GENE) %>%
#   summarize(
#     sum_pos=n(),
#     sum_family1=sum(family_freq != "Low", na.rm = TRUE),
#     sum_family2=sum(num_family >=5, na.rm = TRUE)
#     ) %>% 
#   # summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
#   arrange(desc(sum_pos))

#

# summarized$GENE[is.na(summarized$GENE)] <- "Intergenic Region"
# summarized_abs$GENE[is.na(summarized_abs$GENE)] <- "Intergenic Region"

summarized$major_group <- ifelse(summarized$GENE %in% coding_bed, 
                                 "OXPHOS", 
                                 "NOT OXPHOS")
# summarized_abs$major_group <- ifelse(summarized_abs$GENE %in% coding_bed, 
#                                  "OXPHOS", 
#                                  "NOT OXPHOS")
sum(summarized$sum_pos[summarized$major_group == "OXPHOS"])
sum(summarized$sum_pos[summarized$major_group == "NOT OXPHOS"])

summarized <- summarized %>% 
  arrange(desc(major_group))

# summarized_abs <- summarized_abs %>% 
#   arrange(desc(major_group))

# 1. 定义新的Gene顺序
new_order <- c("COX1", "CYTB", "ND5","ATP6", "COX2", "ND1", "ND4",
               "Gap", "D_loop",  "RNR1", "TRNT",  "TRNR")
# new_order <- c("ND2", "COX1", "CYTB", "ND5","ATP6",  "COX3",  "COX2", "ND1", "ND4",
#   "Gap", "D_loop", "Intergenic", "RNR1", "TRNT", "TRND", "TRNR")
# new_order <- c("CYTB", "ND5", "ND2", "COX3", "ND1",  "ND4", "COX2", "ND3","Gap", 
#                "D_loop","Intergenic Region" )
# 5. 定义自定义的x轴标签，隐藏"Gap"标签

custom_labels <- c("COX1", "CYTB", "ND5","ATP6",   "COX2", "ND1", "ND4",
                   "", "D_loop",  "RNR1", "TRNT",  "TRNR")
# 2. 将 GENE 列转换为因子，并Set其水平为新定义的顺序
summarized$GENE <- factor(summarized$GENE, levels = new_order)
# summarized_abs$GENE <- factor(summarized_abs$GENE, levels = new_order)
#   
### Assume summarized are你的数据框 ###
# Plot柱状图
summarized_long <- summarized %>%
  pivot_longer(
    cols = c(sum_pos, sum_family1, sum_family2),  # 要转换的列
    names_to = "type",                   # 新列名（存放原来的列名）
    values_to = "value"                           # 新列名（存放数值）
  )
summarized_long$type <- factor(summarized_long$type, 
                                        levels = c("sum_pos", "sum_family1", "sum_family2"))

p <- ggplot(summarized, aes(x = GENE, y = sum_pos, fill = GENE %in% coding_bed)) +
  geom_col(width = 0.8,colour = "black") + # Set柱子width
  labs(title = "", x = "", y = "Numbers of De novo variants in differents gene rigons") + # Add标题and轴标签
  theme_classic() + # 使用简约主题
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)) + # Adjustx轴文本避免重叠
    scale_fill_manual(values = c("TRUE" = "#66C2A9", "FALSE" = "#FC8D62"), 
                      labels = c("OXPHOS", "NOT OXPHOS"),
                      name = "") + # Custom colorsand图例
  geom_text(aes(label = ifelse(sum_pos == 0, "", sum_pos)), vjust = -0.5, size = 2.5) + # 在柱子上方标注y值
  scale_y_continuous(limits=c(0, 25), expand = c(0, 0)) +
  scale_x_discrete(limits = new_order, labels = custom_labels)+ # 自定义x轴标签
  geom_segment(aes(x = 1, xend = 9, y = 20, yend = 20), color = "black", size = 0.5)+
  annotate("text", x = 6, y = 21, label = "OXPHOS GENE(67%)")
  
  
p
# ggsave(p ,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure5.counts_dnm.pdf",
#        device = pdf,width = 8,height = 8,dpi = 600)

p2 <- ggplot(summarized, aes(x = GENE, y = sum_family1, fill = GENE %in% coding_bed)) +
  geom_col(width = 0.8,colour = "black") + # Set柱子width
  labs(title = "", x = "", y = "Numbers of De novo variants in differents gene rigons\n (in multiple family)") + # Add标题and轴标签
  theme_classic() + # 使用简约主题
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)) + # Adjustx轴文本避免重叠
  scale_fill_manual(values = c("TRUE" = "#66C2A9", "FALSE" = "#FC8D62"), 
                    labels = c("OXPHOS", "NOT OXPHOS"),
                    name = "") + # Custom colorsand图例
  geom_text(aes(label = ifelse(sum_family1 == 0, "", sum_family1)), vjust = -0.5, size = 2.5) + # 在柱子上方标注y值
  scale_y_continuous(limits=c(0, 10), expand = c(0, 0)) +
  scale_x_discrete(limits = new_order, labels = custom_labels)+ # 自定义x轴标签
  geom_segment(aes(x = 1, xend = 9, y = 7, yend = 7), color = "black", size = 0.5)+
  annotate("text", x = 5, y = 8, label = "OXPHOS GENE(53%)")


p2
# ggsave(p2 ,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure5.counts_dnm_mul_family.pdf",
#        device = pdf,width = 8,height = 8,dpi = 600)

p3 <- ggplot(summarized, aes(x = GENE, y = sum_family2, fill = GENE %in% coding_bed)) +
  geom_col(width = 0.8,colour = "black") + # Set柱子width
  labs(title = "", x = "", y = "Numbers of De novo variants in differents gene rigons\n (in more than five family)") + # Add标题and轴标签
  theme_classic() + # 使用简约主题
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)) + # Adjustx轴文本避免重叠
  scale_fill_manual(values = c("TRUE" = "#66C2A9", "FALSE" = "#FC8D62"), 
                    labels = c("OXPHOS", "NOT OXPHOS"),
                    name = "") + # Custom colorsand图例
  geom_text(aes(label = ifelse(sum_family2 == 0, "", sum_family2)), vjust = -0.5, size = 2.5) + # 在柱子上方标注y值
  scale_y_continuous(limits=c(0, 15), expand = c(0, 0)) +
  scale_x_discrete(limits = new_order, labels = custom_labels)+ # 自定义x轴标签
  geom_segment(aes(x = 1, xend = 12, y = 13, yend = 13), color = "black", size = 0.5)+
  annotate("text", x = 6, y = 14, label = "OXPHOS GENE(94%)")


p3
# ggsave(p3 ,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure5.counts_dnm_high_family.pdf",
#        device = pdf,width = 8,height = 8,dpi = 600)
p4 <- ggplot(summarized_long, aes(x = GENE, y = value, fill = type)) +
  geom_col(width = 0.6, colour = "black", position = position_dodge(0.8)) + # Addposition_dodge分组
  labs(title = "", x = "", y = "Numbers of De novo variants in different gene regions") + # Add标题and轴标签
  theme_classic() + # 使用简约主题
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)) + # Adjustx轴文本避免重叠
  scale_fill_manual(
    values = c("sum_pos" = "#66C2A9", "sum_family1" = "#FC8D62", "sum_family2" = "#FAFD62"),  # 三个颜色对应三个variable_type
    labels = c("sum_pos" = "Total", "sum_family1" = "≥2 Families", "sum_family2" = "≥4 Families"),
    name = "Variant Type"
  ) +
  # Add数据标签
  geom_text(aes(label = ifelse(value == 0, "", value)), 
            position = position_dodge(0.8), 
            vjust = -0.5, size = 2.5) + 
  scale_y_continuous(limits = c(0, 17), expand = c(0, 0)) +
  scale_x_discrete(limits = new_order, labels = custom_labels)+ # 自定义x轴标签
  geom_segment(aes(x = 1, xend = 7, y = 15, yend = 15), color = "black", size = 0.5)+
  annotate("text", x = 5, y = 16, label = "OXPHOS GENE(51.28%)")
print(p4)
# ggsave(p4 ,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure5.counts_dnm_high_family202512.pdf",
#        device = pdf,width = 8,height = 8,dpi = 600)

