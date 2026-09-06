rm(list=ls())
library(dplyr)
library(tidyverse)
library(ggrepel)
library(patchwork)
library(readxl)
library(data.table)
##
## Load cached, skip recomputation
# load("D:\\biosoft\\1000thal\\mtDNA\\R script/figure3A.diff_maternal.RData")
df1020AC01 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020.AC0.1_202512.tsv",sep = "\t",header=T)   %>%
  filter(!grepl(",",ALT)) 
  # filter(AC_het != 0)
# df1020AC005 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020.AC0.05.tsv",sep = "\t",header=T) %>%
#   filter(!grepl(",",ALT)) 
  # filter(AC_het != 0)
colnames(df1020AC01) <- c("CHROM", "POS", "ID", "REF", "ALT", "ANNO", "AC_hom1020", "AC_het1020", "max_HL1020","AF_hom1020", "AF_het1020")
# colnames(df1020AC005) <- c("CHROM", "POS", "ID", "REF", "ALT", "ANNO", "AC_hom1020", "AC_het1020", "max_HL1020","AF_hom1020", "AF_het1020")

# all(df1020AC01$ID %in% df1020AC005$ID)



# inherit <- read.table('D:\\biosoft\\1000thal/mtDNA/Maternal inheritance/inherit_final.tsv',sep = "\t",header=T) %>% 
#   mutate(type = ifelse(nchar(ALT) == 1 & nchar(REF) == 1,"snv","indel"))

# inherit005 <- read.table('D:\\biosoft\\1000thal/mtDNA/Maternal inheritance/inherit.tsv',sep = "\t",header=T) 

# dnm005 <- read.table('D:\\biosoft\\1000thal/mtDNA/Maternal inheritance/dnm.tsv',sep = "\t",header=T)
# dnm01 <- read.table('D:\\biosoft\\1000thal/mtDNA/Maternal inheritance/dnm_abs.tsv',sep = "\t",header=T)
gnad <- read.table("D:\\biosoft\\1000thal\\mtDNA\\MTdatabase/gnomAD/gnomad.genomes.v3.1.sites.chrM.reduced_annotations.tsv",sep = "\t",header=T) %>% 
  mutate(ID=paste(position,ref,alt,sep = "_"))

colnames(gnad) <- c("chromosome", "position", "ref", "alt", "filters", "AC_hom_gnomAD", 
                    "AC_het_gnomAD", "AF_hom_gnomAD", "AF_het_gnomAD", "AN", "max_observed_heteroplasmy", "id")


UKB <- read_xlsx("D:\\biosoft\\1000thal\\mtDNA\\MTdatabase/gnomAD/ukb_aou.xlsx",sheet = "UKB")
AOU <- read_xlsx("D:\\biosoft\\1000thal\\mtDNA\\MTdatabase/gnomAD/ukb_aou.xlsx",sheet = "AoU")
##
# Fix separator in IDs
UKB$variant <- gsub(":", "_", UKB$variant)
AOU$variant <- gsub(":", "_", AOU$variant)

UKB$variant <- gsub(",", "_", UKB$variant)
AOU$variant <- gsub(",", "_", AOU$variant)

UKB$variant <- gsub("^chrM_", "", UKB$variant)
AOU$variant <- gsub("^chrM_", "", AOU$variant)

UKB_subset <- subset(UKB,select = variant ) %>% 
  mutate(group1 = "UKB") %>% 
  distinct() %>% 
  rename(id = variant)

AOU_subset <- subset(AOU,select = variant ) %>% 
  mutate(group2 = "AOU") %>% 
  distinct() %>% 
  rename(id = variant)

gnad_subset <- subset(gnad,select = id) %>% 
  mutate(group3 = "gnad") %>%
  distinct()



# mtDNA gene features
mtgff3 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020bedout\\mt.gff3.bed",sep = "\t")
mtgff3$V2 <- mtgff3$V2 + 1
mtgff3$"with" <- mtgff3$V3 - mtgff3$V2 + 1
mtgff3[1,6] <- "D_loop"
mtgff3[39,6] <- "D_loop"
colnames(mtgff3) <- c("chrom","start","end","id","strand","gene","with")

coding_bed <- c("ATP6", "ATP8", "COX1", "COX2", "COX3", "CYTB",  "ND1",
                "ND2", "ND3", "ND4", "ND4L", "ND5", "ND6")


setDT(df1020AC01)
setDT(mtgff3)
#
df1020AC01[mtgff3, GENE := i.gene, on = .(POS >= start, POS <= end)] 

df1020AC01$GENE[is.na(df1020AC01$GENE)] <- "intergenic"


df1020 <- df1020AC01 %>% 
  as.data.frame() %>% 
  mutate( OXPHOS = ifelse( GENE %in% coding_bed,"yes","no"))

df1020_nohet <- df1020 %>% 
  filter(AC_het1020 ==0) %>% 
  rename(id =ID) %>% 
  left_join(gnad_subset,by="id") %>%
  left_join(AOU_subset,by="id") %>%
  left_join(UKB_subset,by="id") %>%
  mutate(YESNO = ifelse(is.na(group1) & is.na(group2) & is.na(group3), "no", "yes"))
  # left_join(df1020,by="id") %>%
  # select(c("id","AF_het1020","AC_het1020","YESNO","OXPHOS"))

df1020_yeshet <- df1020 %>% 
  filter(AC_het1020 !=0) %>% 
  rename(id =ID) %>% 
  left_join(gnad_subset,by="id") %>%
  left_join(AOU_subset,by="id") %>%
  left_join(UKB_subset,by="id") %>%
  mutate(YESNO = ifelse(is.na(group1) & is.na(group2) & is.na(group3), "no", "yes"))
# left_join(df1020,by="id") %>%
# select(c("id","AF_het1020","AC_het1020","YESNO","OXPHOS"))

df1020_nohet_snv <- df1020_nohet %>% filter(nchar(ALT) == 1 & nchar(REF) == 1)

df1020_nohet_indel <- df1020_nohet %>% filter(nchar(ALT) != 1 | nchar(REF) != 1)

df1020_yeshet_snv <- df1020_yeshet %>% filter(nchar(ALT) == 1 & nchar(REF) == 1)

df1020_yeshet_indel <- df1020_yeshet%>% filter(nchar(ALT) != 1 | nchar(REF) != 1)

### homo ###

novel_homo_snv <- df1020_nohet_snv %>% filter(YESNO == "no")
known_homo_snv <- df1020_nohet_snv %>% filter(YESNO == "yes")

novel_homo_indel <- df1020_nohet_indel %>% filter(YESNO == "no")
known_homo_indel<- df1020_nohet_indel %>% filter(YESNO == "yes")


snv_novel_homo <- nrow(novel_homo_snv)
snv_known_homo <- nrow(known_homo_snv)
indel_novel_homo <- nrow(novel_homo_indel)
indel_known_homo <- nrow(known_homo_indel)

### hetero ###

novel_hetero_snv <- df1020_yeshet_snv %>% filter(YESNO == "no")
known_hetero_snv <- df1020_yeshet_snv %>% filter(YESNO == "yes")

novel_hetero_indel <- df1020_yeshet_indel %>% filter(YESNO == "no")
known_hetero_indel<- df1020_yeshet_indel %>% filter(YESNO == "yes")


snv_novel_hetero <- nrow(novel_hetero_snv)
snv_known_hetero <- nrow(known_hetero_snv)
indel_novel_hetero <- nrow(novel_hetero_indel)
indel_known_hetero <- nrow(known_hetero_indel)


### 2 af ###
af1_indel <- df1020_yeshet_indel %>% 
  filter(AF_het1020 <= 0.01) 
af2_indel <- df1020_yeshet_indel %>% 
  filter(AF_het1020 > 0.01 & AF_het1020 < 0.05) 
af5_indel <- df1020_yeshet_indel %>% 
  filter(AF_het1020 >= 0.05) 

af1_snv <- df1020_yeshet_snv %>% 
  filter(AF_het1020 <= 0.01) 
af2_snv <- df1020_yeshet_snv %>%  
  filter(AF_het1020 > 0.01 & AF_het1020 < 0.05) 
af5_snv <- df1020_yeshet_snv %>%  
  filter(AF_het1020 >= 0.05) 

### COUNTS af2 ###

novel_af1_indel <- af1_indel %>% filter(YESNO == "no")
known_af1_indel<- af1_indel %>% filter(YESNO == "yes")
novel_af1_snv <- af1_snv %>% filter(YESNO == "no")
known_af1_snv <- af1_snv %>% filter(YESNO == "yes")

novel_af2_indel <- af2_indel %>% filter(YESNO == "no")
known_af2_indel<- af2_indel %>% filter(YESNO == "yes")
novel_af2_snv <- af2_snv %>% filter(YESNO == "no")
known_af2_snv <- af2_snv %>% filter(YESNO == "yes")

novel_af5_indel <- af5_indel %>% filter(YESNO == "no")
known_af5_indel<- af5_indel %>% filter(YESNO == "yes")
novel_af5_snv <- af5_snv %>% filter(YESNO == "no")
known_af5_snv <- af5_snv %>% filter(YESNO == "yes")

### is known ###
snv_novel_af1 <- nrow(novel_af1_snv)
indel_novel_af1 <- nrow(novel_af1_indel)
snv_known_af1 <- nrow(known_af1_snv)
indel_known_af1 <- nrow(known_af1_indel)


snv_novelaf2 <- nrow(novel_af2_snv)
indel_novel_af2 <- nrow(novel_af2_indel)
snv_known_af2 <- nrow(known_af2_snv)
indel_known_af2 <- nrow(known_af2_indel)


snv_novel_af5 <- nrow(novel_af5_snv)
indel_novel_af5 <- nrow(novel_af5_indel)
snv_known_af5 <- nrow(known_af5_snv)
indel_known_af5 <- nrow(known_af5_indel)

#####

# Create the data frame with an empty category for spacing
input_snv <- data.frame(
  Category = c(rep(c("Homoplasmic","Heteroplasmic"), each = 2),
               "",  # This creates the space
               rep(c("AF <= 1%", "AF = 1%~5%", "AF >= 5%"), each = 2)),
  Frequency = c(rep(c("Novel", "Known"), time = 2),
                "Novel",  # This creates the space
                rep(c("Novel", "Known"), time= 3)),
  
  Count = c(snv_novel_homo, snv_known_homo, 
            snv_novel_hetero, snv_known_hetero,
            0,   # Zero values for the empty category (won't show in plot)
            snv_novel_af1, snv_known_af1,
            snv_novelaf2, snv_known_af2,
            snv_novel_af5, snv_known_af5)
)

input_indel<- data.frame(
  Category = c(rep(c("Homoplasmic","Heteroplasmic"), each = 2),
               "",  # This creates the space
               rep(c("AF <= 1%", "AF = 1%~5%", "AF >= 5%"), each = 2)),
  Frequency = c(rep(c("Novel", "Known"), time = 2),
                "Novel",  # This creates the space
                rep(c("Novel", "Known"), time= 3)),
  Count = c(indel_novel_homo,indel_known_homo,
            indel_novel_hetero,indel_known_hetero,
            0,   # Zero values for the empty category (won't show in plot)
            indel_novel_af1,indel_known_af1,
            indel_novel_af2,indel_known_af2,
            indel_novel_af5,indel_known_af5)
)






# Set factor levels with empty string for spacing
input_snv$Category <- factor(
  input_snv$Category,
  levels = c("Homoplasmic", "Heteroplasmic", "", "AF <= 1%", "AF = 1%~5%", "AF >= 5%")
)

input_indel$Category <- factor(
  input_snv$Category,
  levels = c("AF >= 5%","AF = 1%~5%","AF <= 1%", "",  "Heteroplasmic","Homoplasmic")
)

### Adjust ggplot，跳过空字符串的标签 ###
P1 <- ggplot(input_snv, aes(x = Category, y = Count, fill = Frequency)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(
    values = c("#ddc7eb", "#be95d9"),
    name = "SNV",
    guide = guide_legend(label = FALSE)
  ) +
  geom_text(aes(label = ifelse(Count < 200 & Count >= 5, Count, "")), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            fontface = "bold",
            color = "white") +
  # # 单独为小数值Addnudge（微调）
  geom_text(data = input_snv %>% filter(Count > 0 & Count < 5),
            aes(x = Category,
                y = ave(Count, Category, FUN = function(x) cumsum(x) - 0.5*x),
                label = Count),
            position = position_nudge(y = 3),  # 这里使用nudge_y
            size = 3.5,
            fontface = "bold",
            color = "black") +

  # scale_x_discrete(
  #   breaks = c("Homoplasmic", "Heteroplasmic", "","AF <= 1%", "AF = 1%~5%", "AF >= 5%"), # Skip empty string
  #   labels = c("Homoplasmic", "Heteroplasmic", "","AF ≤ 1%", "AF = 1%~5%", "AF ≥ 5%")
  # ) +
  labs(title = NULL, x = "Non-reference allele frequency", y = "Number of SNVs") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = 'bold'),
    axis.title.x = element_text(size = 15, face = 'bold'),
    axis.text.y = element_text(size = 12, face = 'bold'),
    axis.title.y = element_text(size = 15, face = 'bold'),
    axis.line = element_line(linewidth = 1, colour = "black"),
    legend.position = c(0.5, 0.9),
    legend.direction = "vertical"
  ) +
  scale_y_continuous(expand=c(0,0))+
  # Add箭头
  annotate("segment",
           x = 2.7, xend = 3.2,  # 第三个类别的Position
           y = 100, yend = 100,  # 从y轴下方开始向上画
           arrow = arrow(type = "closed", length = unit(0.2, "cm")),
           color = "black") +
  # Add dashed rectangle around the second bar
  annotate("rect",
           xmin = 1.5,  # Center of second bar (bar positions are at 1, 2, 3, etc.)
           xmax = 2.5,
           ymin = 0,
           ymax = 178, # Or specific height
           fill = NA,
           color = "black",
           linetype = "11",
           linewidth = 1) +
  coord_cartesian(ylim = c(0, 200))

P1





# 2. Adjust ggplot，跳过空字符串的标签

P2 <- ggplot(input_indel, aes(x = Category, y = Count, fill = Frequency)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(
    values = c("#c4d4ed", "#8eacde"),
    name = "INDEL",
     # guide = guide_legend(label = FALSE)
  ) +
  geom_text(aes(label = ifelse(Count >= 4, Count, "")), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            fontface = "bold",
            color = "white") +
  # # 单独为小数值Addnudge（微调）
  # geom_text(data = input_snv %>% filter(Count > 0 & Count < 5),
  #           aes(x = Category,
  #               y = ave(Count, Category, FUN = function(x) cumsum(x) - 0.5*x),
  #               label = Count),
  #           position = position_nudge(y = 3),  # 这里使用nudge_y
  #           size = 3.5,
  #           fontface = "bold",
  #           color = "black") +
  ### 为小数值（height<=4）在外部标注 - 使用position_nudge ###
  geom_text(aes(label = ifelse(Count > 0 & Count < 4, Count, "")),
            position = position_stack(vjust = 0.5),
            size = 3.5,
            fontface = "bold",
            color = "black") +
  # scale_x_discrete(
  #   breaks = c("Homoplasmic", "Heteroplasmic", "","AF <= 1%", "AF = 1%~5%", "AF >= 5%"), # Skip empty string
  #   labels = c("Homoplasmic", "Heteroplasmic", "","AF ≤ 1%", "AF = 1%~5%", "AF ≥ 5%")
  # ) +
  labs(title = NULL, x = "Non-reference allele frequency", y = "Number of INDELs") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = 'bold'),
    axis.title.x = element_text(size = 15, face = 'bold'),
    axis.text.y = element_text(size = 12, face = 'bold'),
    axis.title.y = element_text(size = 15, face = 'bold'),
    axis.line = element_line(linewidth = 1, colour = "black"),
    legend.position = c(0.5, 0.9),
    legend.direction = "vertical"
  ) +
  coord_cartesian(ylim = c(0, 20))+
  # Add箭头
  annotate("segment",
           x = 4.2, xend = 3.7,  # 第三个类别的Position
           y = 10, yend = 10,  # 从y轴下方开始向上画
           arrow = arrow(type = "closed", length = unit(0.2, "cm")),
           color = "black") +
  # Add dashed rectangle around the second bar
  annotate("rect",
           xmin = 4.5,  # Center of second bar (bar positions are at 1, 2, 3, etc.)
           xmax = 5.5,
           ymin = 0,
           ymax = 11.5, # Or specific height
           fill = NA,
           color = "black",
           linetype = "11",
           linewidth = 1) +
  scale_y_continuous(position = "right",expand=c(0,0))

P2







### 使用 patchwork 来Composite plot，并收集图例 ###
P3 <- (P1 | P2) + plot_layout(guides = 'collect',axis_titles = "collect") & theme(legend.position='top') 

P3

ggsave(P3,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/Main/figure3A.Non-reference allele frequency202512.pdf",
       device = pdf,width = 8,height = 9,dpi = 600)

#




#####
#
# dnm_merge <- dnm %>% 
#   left_join(gnad_subset,by="id") %>% 
#   left_join(AOU_subset,by="id") %>% 
#   left_join(UKB_subset,by="id") %>% 
#   mutate(YESNO = ifelse(is.na(group1) & is.na(group2) & is.na(group3), "no", "yes")) %>% 
#   left_join(df1020AC0.05,,by=c("id"="ID")) %>%
#   select(c("id","AF_het1020","AC_het1020","YESNO"))
# 
# inherit_merge <- inherit %>% 
#   left_join(gnad_subset,by="id") %>% 
#   left_join(AOU_subset,by="id") %>% 
#   left_join(UKB_subset,by="id") %>% 
#   mutate(YESNO = ifelse(is.na(group1) & is.na(group2) & is.na(group3), "no", "yes")) %>% 
#   left_join(df1020AC0.05,,by=c("id"="ID")) %>%
#   select(c("id","AF_het1020","AC_het1020","YESNO"))

dnm_indel <- dnm %>% filter(nchar(alt) != 1 | nchar(ref) != 1) %>%
  left_join(gnad_subset,by="id") %>%
  left_join(AOU_subset,by="id") %>%
  left_join(UKB_subset,by="id") %>%
  mutate(YESNO = ifelse(is.na(group1) & is.na(group2) & is.na(group3), "no", "yes")) %>%
  left_join(df1020,,by=c("id"="ID")) %>%
  select(c("id","AF_het1020","AC_het1020","YESNO","OXPHOS"))

#
dnm_snv <- anti_join(dnm, dnm_indel) %>%
  left_join(gnad_subset,by="id") %>%
  left_join(AOU_subset,by="id") %>%
  left_join(UKB_subset,by="id") %>%
  mutate(YESNO = ifelse(is.na(group1) & is.na(group2) & is.na(group3), "no", "yes")) %>%
  left_join(df1020,,by=c("id"="ID")) %>%
  select(c("id","AF_het1020","AC_het1020","YESNO","OXPHOS"))
#
#
inherit_indel <- inherit %>% filter(nchar(ALT) != 1 | nchar(REF) != 1) %>%
  left_join(gnad_subset,by=c("ID"= "id")) %>%
  left_join(AOU_subset,by=c("ID"= "id")) %>%
  left_join(UKB_subset,by=c("ID"= "id")) %>%
  mutate(YESNO = ifelse(is.na(group1) & is.na(group2) & is.na(group3), "no", "yes"))%>%
  left_join(df1020,,by="ID") %>%
  select(c("ID","AF_het1020","AC_het1020","YESNO","OXPHOS"))
#
#
inherit_snv <- inherit %>% filter(type == "snv") %>%
  left_join(gnad_subset,by=c("ID"= "id")) %>%
  left_join(AOU_subset,by=c("ID"= "id")) %>%
  left_join(UKB_subset,by=c("ID"= "id")) %>%
  mutate(YESNO = ifelse(is.na(group1) & is.na(group2) & is.na(group3), "no", "yes"))%>%
  left_join(df1020,,by="ID") %>%
  select(c("ID","AF_het1020","AC_het1020","YESNO","OXPHOS"))

# inherit_snv <- anti_join(inherit,inherit_indel) %>%
#   left_join(gnad_subset,by="id") %>%
#   left_join(AOU_subset,by="id") %>%
#   left_join(UKB_subset,by="id") %>%
#   mutate(YESNO = ifelse(is.na(group1) & is.na(group2) & is.na(group3), "no", "yes")) %>%
#   left_join(df1020AC0.05,,by=c("id"="ID")) %>%
#   select(c("id","AF_het1020","AC_het1020","YESNO"))

#
# notdb_dnm_merge <- dnm_merge %>% filter(YESNO == "no")
# indb_dnm_merge<- dnm_merge %>% filter(YESNO == "yes")
# notdb_inherit_merge <- inherit_merge %>% filter(YESNO == "no")
# indb_inherit_merge <- inherit_merge%>% filter(YESNO == "yes")


### is known ###
notdb_dnm_indel <- dnm_indel %>% filter(YESNO == "no")
indb_dnm_indel<- dnm_indel %>% filter(YESNO == "yes")
notdb_dnm_snv <- dnm_snv %>% filter(YESNO == "no")
indb_dnm_snv <- dnm_snv %>% filter(YESNO == "yes")

notdb_inherit_indel <- inherit_indel %>% filter(YESNO == "no")
indb_inherit_indel <- inherit_snv %>% filter(YESNO == "yes")
notdb_inherit_snv <- inherit_indel %>% filter(YESNO == "no")
indb_inherit_snv <- inherit_snv %>% filter(YESNO == "yes")

##
# af1_dnm_merge <- dnm_merge %>% filter(AF_het1020 <= 0.01)
# notdb_af1_dnm_merge <- af1_dnm_merge %>% filter(YESNO == "no")
# intdb_af1_dnm_merge <- af1_dnm_merge %>% filter(YESNO == "yes")
# 
# af2_dnm_merge <- dnm_merge %>% filter(AF_het1020 < 0.05 & AF_het1020 > 0.01)
# notdb_af2_dnm_merge <- af2_dnm_merge %>% filter(YESNO == "no")
# intdb_af2_dnm_merge <- af2_dnm_merge %>% filter(YESNO == "yes")
# 
# af5_dnm_merge  <- dnm_merge %>% filter(AF_het1020 >= 0.05)
# notdb_af5_dnm_merge <- af5_dnm_merge %>% filter(YESNO == "no")
# intdb_af5_dnm_merge <- af5_dnm_merge %>% filter(YESNO == "yes")
# 
# af1_inherit_merge <- inherit_merge %>% filter(AF_het1020 <= 0.01)
# notdb_af1_inherit_merge <- af1_inherit_merge %>% filter(YESNO == "no")
# intdb_af1_inherit_merge <- af1_inherit_merge %>% filter(YESNO == "yes")
# 
# af2_inherit_merge <- inherit_merge %>% filter(AF_het1020 < 0.05 & AF_het1020 > 0.01)
# notdb_af2_inherit_merge <- af2_inherit_merge %>% filter(YESNO == "no")
# intdb_af2_inherit_merge <- af2_inherit_merge %>% filter(YESNO == "yes")
# 
# af5_inherit_merge <- inherit_merge %>% filter(AF_het1020 >= 0.05)
# notdb_af5_inherit_merge <- af5_inherit_merge %>% filter(YESNO == "no")
# intdb_af5_inherit_merge <- af5_inherit_merge %>% filter(YESNO == "yes")


#COUNTS KNOWN
# Novel_dnm <- nrow(notdb_dnm_merge)
# Known_dnm <- nrow(indb_dnm_merge)
# Novel_inherit <- nrow(notdb_inherit_merge)
# Known_inherit <- nrow(indb_inherit_merge)
snv_Novel_dnm <- nrow(notdb_dnm_snv)
snv_Known_dnm <- nrow(indb_dnm_snv)
indel_Novel_dnm <- nrow(notdb_dnm_indel)
indel_Known_dnm <- nrow(indb_dnm_indel)

snv_Novel_inherit<- nrow(notdb_inherit_snv)
snv_Known_inherit<- nrow(indb_inherit_snv)
indel_Novel_inherit<- nrow(notdb_inherit_indel)
indel_Known_inherit<- nrow(indb_inherit_indel)

##COUNTS AF
# Novel_AF1_dnm <- nrow(notdb_af1_dnm_merge)
# Known_AF1_dnm <- nrow(intdb_af1_dnm_merge)
# 
# Novel_AF2_dnm <- nrow(notdb_af2_dnm_merge)
# Known_AF2_dnm <- nrow(intdb_af2_dnm_merge)
# 
# Novel_AF5_dnm <- nrow(notdb_af5_dnm_merge)
# Known_AF5_dnm <- nrow(intdb_af5_dnm_merge)
# 
# 
# Novel_inherit <- nrow(notdb_inherit_merge)
# Known_inherit <- nrow(indb_inherit_merge)
# 
# snv_AF1_dnm <- nrow(af1_dnm_snv)
# snv_AF2_dnm <- nrow(af2_dnm_snv)
# snv_AF5_dnm <- nrow(af5_dnm_snv)
# indel_AF1_dnm <- nrow(af1_dnm_indel)
# indel_AF2_dnm <- nrow(af2_dnm_indel)
# indel_AF5_dnm <- nrow(af5_dnm_indel)
# 
# 
# snv_AF1_inherit <- nrow(af1_inherit_snv)
# snv_AF2_inherit <- nrow(af2_inherit_snv)
# snv_AF5_inherit <- nrow(af5_inherit_snv)
# indel_AF1_inherit <- nrow(af1_inherit_indel)
# indel_AF2_inherit <- nrow(af2_inherit_indel)
# indel_AF5_inherit <- nrow(af5_inherit_indel)

####
# input_snv<- data.frame(
#   Category = rep(c("Het all samples","Heteroplasmic", "AF <= 1%", "AF = 1%~5%", "AF >= 5%"), each = 2),
#   Frequency = rep(c("Novel", "Known in gnomAD"), times = 5),
#   Count = c(snv_Het_homoplasmic_Novel,snv_Het_homoplasmic_Known,
#             snv_Heteroplasmic_Novel,snv_Heteroplasmic_Known,snv_AF1_Novel,snv_AF1_Known,
#             snv_AF15_Novel,snv_AF15_Known,snv_AF5_Novel,snv_AF5_Known)
# )



input_snv<- data.frame(
  Category = rep(c("Dnm","Inherit"), each = 2),
  Frequency = rep(c("Novel", "Known"), times = 2),
  Count = c(snv_Novel_dnm,snv_Known_dnm,
            snv_Novel_inherit,snv_Known_inherit)
)


input_indel <- data.frame(
  Category = rep(c("Dnm","Inherit"), each = 2),
  Frequency = rep(c("Novel", "Known"), times = 2),
  Count = c(indel_Novel_dnm,indel_Known_dnm,
            indel_Novel_inherit,indel_Known_inherit)
)

# Plot图形
P1 <- ggplot(input_snv, aes(x = Category, y = Count, fill = Frequency)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("#ddc7eb", "#be95d9"),name = "SNV",
                    guide = guide_legend(label = FALSE)
  ) +
  labs(title = NULL, x = "Non-reference allele frequency", y = "Number of SNVs") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.5, 0.9)
  ) +
  coord_cartesian(ylim = c(0, 300))+
  scale_y_continuous(expand=c(0,0))+
  theme(
    # axis.ticks.length.x = unit(0.03,'cm'),
    # axis.ticks.y = element_blank(),
    axis.text.x = element_text(size=12,face = 'bold'),
    axis.title.x = element_text(size = 15,face = 'bold'),
    axis.text.y = element_text(size = 12,face = 'bold'),
    axis.title.y = element_text(size = 15,face = 'bold'),
    axis.line = element_line(linewidth=1, colour = "black"),
    legend.direction = "vertical")
P1


P2 <- ggplot(input_indel, aes(x = Category, y = Count, fill = Frequency)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("#c4d4ed", "#8eacde"),name = "INDEL",
                    # guide = guide_legend(label = FALSE)
  ) +
  labs(title = NULL, x = "Non-reference allele frequency", y = "Number of Indels") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.3, 0.9)
  ) +
  # scale_y_continuous(labels = scales::comma) +
  coord_cartesian(ylim = c(0, 300))+
  scale_y_continuous(position = "right",expand=c(0,0))+
  theme(
    # axis.ticks.length.x = unit(0.03,'cm'),
    # axis.ticks.y = element_blank(),
    axis.text.x = element_text(size=12,face = 'bold'),
    axis.title.x = element_text(size = 15,face = 'bold'),
    axis.text.y = element_text(size = 12,face = 'bold'),
    axis.title.y = element_text(size = 15,face = 'bold'),
    axis.line = element_line(linewidth=1, colour = "black"),
    legend.direction = "vertical")

P2

### 使用 patchwork 来Composite plot，并收集图例 ###
P3 <- (P1 | P2) + plot_layout(guides = 'collect',axis_titles = "collect") & theme(legend.position='top') 

P3
# ggsave(P3,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/Main/figure3A.Non-reference allele frequency.pdf",
#        device = pdf,width = 8,height = 9,dpi = 600)



### is oxphos ###
noOX_dnm_indel <- dnm_indel %>% filter(OXPHOS == "no")
inOX_dnm_indel<- dnm_indel %>% filter(OXPHOS == "yes")
noOX_dnm_snv <- dnm_snv %>% filter(OXPHOS == "no")
inOX_dnm_snv <- dnm_snv %>% filter(OXPHOS == "yes")

noOX_inherit_indel <- inherit_indel %>% filter(OXPHOS == "no")
inOX_inherit_indel <- inherit_indel %>% filter(OXPHOS == "yes")
noOX_inherit_snv <- inherit_snv %>% filter(OXPHOS == "no")
inOX_inherit_snv <- inherit_snv %>% filter(OXPHOS == "yes")

snv_NoOX_dnm <- nrow(noOX_dnm_snv)
snv_OXPH_dnm <- nrow(inOX_dnm_snv)
indel_NoOX_dnm <- nrow(noOX_dnm_indel)
indel_OXPH_dnm <- nrow(inOX_dnm_indel)

snv_NoOX_inherit<- nrow(noOX_inherit_snv)
snv_OXPH_inherit<- nrow(inOX_inherit_snv)
indel_NoOX_inherit<- nrow(noOX_inherit_indel)
indel_OXPH_inherit<- nrow(inOX_inherit_indel)


### 2 af ###
af1_indel <- df1020 %>% filter(nchar(ALT) != 1 | nchar(REF) != 1) %>% 
  filter(AF_het1020 <= 0.01) 
af2_indel <- df1020 %>% filter(nchar(ALT) != 1 | nchar(REF) != 1) %>% 
  filter(AF_het1020 > 0.01 & AF_het1020 < 0.05) 
af5_indel <- df1020 %>% filter(nchar(ALT) != 1 | nchar(REF) != 1) %>% 
  filter(AF_het1020 >= 0.05) 

af1_snv <- df1020 %>% filter(nchar(ALT) == 1 & nchar(REF) == 1) %>% 
  filter(AF_het1020 <= 0.01) 
af2_snv <- df1020 %>% filter(nchar(ALT) == 1 & nchar(REF) == 1) %>%  
  filter(AF_het1020 > 0.01 & AF_het1020 < 0.05) 
af5_snv <- df1020 %>% filter(nchar(ALT) == 1 & nchar(REF) == 1) %>%  
  filter(AF_het1020 >= 0.05) 
### COUNTS af2 ###
noOX_af1_indel <- af1_indel %>% filter(OXPHOS == "no")
inOX_af1_indel<- af1_indel %>% filter(OXPHOS == "yes")
noOX_af1_snv <- af1_snv %>% filter(OXPHOS == "no")
inOX_af1_snv <- af1_snv %>% filter(OXPHOS == "yes")

noOX_af2_indel <- af2_indel %>% filter(OXPHOS == "no")
inOX_af2_indel<- af2_indel %>% filter(OXPHOS == "yes")
noOX_af2_snv <- af2_snv %>% filter(OXPHOS == "no")
inOX_af2_snv <- af2_snv %>% filter(OXPHOS == "yes")

noOX_af5_indel <- af5_indel %>% filter(OXPHOS == "no")
inOX_af5_indel<- af5_indel %>% filter(OXPHOS == "yes")
noOX_af5_snv <- af5_snv %>% filter(OXPHOS == "no")
inOX_af5_snv <- af5_snv %>% filter(OXPHOS == "yes")


snv_NoOX_af1 <- nrow(noOX_af1_snv)
indel_NoOX_af1 <- nrow(noOX_af1_indel)
snv_inOX_af1 <- nrow(inOX_af1_snv)
indel_inOX_af1 <- nrow(inOX_af1_indel)


snv_NoOX_af2 <- nrow(noOX_af2_snv)
indel_NoOX_af2 <- nrow(noOX_af2_indel)
snv_inOX_af2 <- nrow(inOX_af2_snv)
indel_inOX_af2 <- nrow(inOX_af2_indel)


snv_NoOX_af5 <- nrow(noOX_af5_snv)
indel_NoOX_af5 <- nrow(noOX_af5_indel)
snv_inOX_af5 <- nrow(inOX_af5_snv)
indel_inOX_af5 <- nrow(inOX_af5_indel)


######
input_snv2<- data.frame(
  Category = rep(c("Dnm","Inherit", "AF <= 1%", "AF = 1%~5%", "AF >= 5%"), each = 2),
  Frequency = rep(c("Non-OXPHOS", "OXPHOS"),  times = 5),
  Count = c(snv_NoOX_dnm,snv_OXPH_dnm,snv_NoOX_inherit,snv_OXPH_inherit,
            snv_NoOX_af1,snv_inOX_af1,snv_NoOX_af2,snv_inOX_af2,snv_NoOX_af5,snv_inOX_af5)
)


input_indel2<- data.frame(
  Category = rep(c("Dnm","Inherit", "AF <= 1%", "AF = 1%~5%", "AF >= 5%"), each = 2),
  Frequency = rep(c("Non-OXPHOS", "OXPHOS"),  times = 5),
  Count = c(indel_NoOX_dnm,indel_OXPH_dnm,indel_NoOX_inherit,indel_OXPH_inherit,
            indel_NoOX_af1,indel_inOX_af1,indel_NoOX_af2,indel_inOX_af2,indel_NoOX_af5,indel_inOX_af5)
)



# Plot图形
P11 <- ggplot(input_snv2, aes(x = Category, y = Count, fill = Frequency)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("#f9e5b5", "#f1c252"),name = "SNV",
                    guide = guide_legend(label = FALSE)
  ) +
  labs(title = NULL, x = "Non-reference allele frequency", y = "Number of SNVs") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.5, 0.9)
  ) +
  coord_cartesian(ylim = c(0, 400))+
  theme(
    # axis.ticks.length.x = unit(0.03,'cm'),
    # axis.ticks.y = element_blank(),
    axis.text.x = element_text(size=12,face = 'bold'),
    axis.title.x = element_text(size = 15,face = 'bold'),
    axis.text.y = element_text(size = 12,face = 'bold'),
    axis.title.y = element_text(size = 15,face = 'bold'),
    axis.line = element_line(linewidth=1, colour = "black"),
    legend.direction = "vertical")
P11


P22 <- ggplot(input_indel2, aes(x = Category, y = Count, fill = Frequency)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("#D0E5CB", "#91C286"),name = "INDEL",
                    # guide = guide_legend(label = FALSE)
  ) +
  labs(title = NULL, x = "Non-reference allele frequency", y = "Number of Indels") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.3, 0.9)
  ) +
  # scale_y_continuous(labels = scales::comma) +
  coord_cartesian(ylim = c(0, 250))+
  scale_y_continuous(position = "right")+
  theme(
    # axis.ticks.length.x = unit(0.03,'cm'),
    # axis.ticks.y = element_blank(),
    axis.text.x = element_text(size=12,face = 'bold'),
    axis.title.x = element_text(size = 15,face = 'bold'),
    axis.text.y = element_text(size = 12,face = 'bold'),
    axis.title.y = element_text(size = 15,face = 'bold'),
    axis.line = element_line(linewidth=1, colour = "black"),
    legend.direction = "vertical")

P22




# P1 | P2 + plot_layout(guides = 'collect')

# combined_plot <- (P1 | P2) + 
#   plot_layout(guides = 'collect') +
#     plot_annotation(
#         theme = theme(
#             legend.position = c(0.05, 0.95)  # Set图例Position为左上角
#           )
#       )
### 使用 patchwork 来Composite plot，并收集图例 ###
P33 <- (P11 | P22) + plot_layout(guides = 'collect',axis_titles = "collect") & theme(legend.position='top') 

P33
# ggsave(P33,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure3A.Non-reference allele frequency in OXPHOS.pdf",
#        device = pdf,width = 8,height = 8,dpi = 600)

## Save所有数据框
# setwd("D:\\biosoft\\1000thal\\mtDNA\\R script/")
# save.image("figure3A.diff_maternal.RData")
