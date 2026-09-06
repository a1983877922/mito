rm(list=ls())#clear Global Environment
# Load必要的包
library(clusterProfiler)
library(aPEAR)
library(org.Hs.eg.db)  # 人类Gene组数据库
library(ggplot2)
library(dplyr)

### 读取Gene count据 ###
genes <- read.table("D:\\biosoft\\1000thal\\mtDNA\\gwas\\human_mito_genes\\mito_genes_hg38.tsv", 
                    header = TRUE) %>%
  arrange(hg38_Chromosome) %>%
  mutate(Gene_Length = hg38_End - hg38_Start + 1,
         hg38_Chromosome = paste0("chr", hg38_Chromosome))


background_genes <- genes$Symbol  # Assume你的数据框中有Gene_Symbol列
### 你的significant Genes列表 ###
significant_genes <- c("AK2", "ATAD3A", "ATAD3B", "BOLA1", "CASP9", "KMO", "MTARC2",
                       "TDRKH", "COMTD1", "NUDT13", "BCO2", "CAT", "SDHAF2", "TIMM10B",
                       "TMEM126B", "ALDH1L2", "ATP23", "CRY1", "DIABLO", "GATC", "MMAB",
                       "MRPL42", "USP30", "CARS2", "MIPEP", "BCL2L2", "DHRS4", "BCL2A1",
                       "C15orf48", "C15orf61", "CYP11A1", "MTFMT", "ACSM2A", "CA5A",
                       "CMC2", "DNAJA3", "HSDL1", "MLYCD", "NME3", "NME4", "PAM16",
                       "PDPR", "ACACA", "C1QBP", "CISD3", "LYRM9", "MRM1", "MRPL45",
                       "MYO19", "SLC25A35", "SPATA20", "BCL2", "FAM210A", "RBFA", "CLPP",
                       "CPT1C", "NDUFA3", "QTRT1", "RDH13", "TIMM44", "UQCRFS1", "BCL2L11",
                       "BCS1L", "BOK", "BOLA3", "C2orf69", "CASP8", "CMPK2", "DBI",
                       "FAHD2A", "MCEE", "MRPL44", "NDUFA10", "NEU4", "COX4I2", "MRPS26",
                       "ATP5PF", "ATP5PO", "CBR3", "DNAJC28", "BCL2L13", "BID", "BIK",
                       "COMT", "GCAT", "NDUFA6", "ACAA1", "C3orf33", "SUCLG2", "CCDC58",
                       "CBR4", "MRPL1", "CCDC127", "CKMT2", "MCCC2", "MRPS27", "PRELID2",
                       "MRPS36", "BPHL", "HSD17B8", "MCCD1", "MRPL18", "MRPS18B", "TOMM6",
                       "VARS2", "ARF5", "CHCHD2", "CHCHD3", "FAM185A", "NDUFA5", "SLC25A13",
                       "BNIP3L", "C8orf82", "CYP11B1", "DECR1", "LYPLA1", "PDP1", "TMEM65",
                       "GLDC", "MRRF", "STOM", "ABCB7", "ACOT9", "AIFM1", "ALAS2", "APOOL",
                       "ARMCX2", "CA5B", "PDHA1", "TMLHE", "TRMT2B")


up_genes <- c("ATAD3B", "BOLA1", "CASP9", "NUDT13", "BCO2", "CAT", "SDHAF2", 
             "TMEM126B", "ALDH1L2", "DIABLO", "GATC", "MMAB", "MRPL42", "USP30", 
             "CARS2", "MIPEP", "BCL2L2", "BCL2A1", "C15orf48", "C15orf61", 
             "CYP11A1", "MTFMT", "ACSM2A", "CA5A", "CMC2", "HSDL1", "MLYCD", 
             "PDPR", "C1QBP", "LYRM9", "SPATA20", "BCL2", "FAM210A", "RBFA", 
             "CLPP", "CPT1C", "BCL2L11", "BCS1L", "BOK", "BOLA3", "C2orf69", 
             "CASP8", "CMPK2", "MCEE", "MRPL44", "COX4I2", "MRPS26", "ATP5PF", 
             "ATP5PO", "CBR3", "BCL2L13", "BID", "BIK", "GCAT", "ACAA1", "C3orf33", 
             "SUCLG2", "CCDC58", "CBR4", "CCDC127", "CKMT2", "MRPS27", "PRELID2", 
             "BPHL", "MRPL18", "TOMM6", "ARF5", "CHCHD3", "SLC25A13", "BNIP3L", 
             "C8orf82", "LYPLA1", "PDP1", "TMEM65", "STOM", "ABCB7", "ACOT9", 
             "AIFM1", "ALAS2", "APOOL", "ARMCX2", "CA5B", "PDHA1", "TMLHE", 
             "TRMT2B")

down_genes <- c("AK2", "ATAD3A", "KMO", "MTARC2", "TDRKH", "COMTD1", "TIMM10B", 
               "ATP23", "CRY1", "DHRS4", "DNAJA3", "NME3", "NME4", "PAM16", 
               "ACACA", "CISD3", "MRM1", "MRPL45", "MYO19", "SLC25A35", "NDUFA3", 
               "QTRT1", "RDH13", "TIMM44", "UQCRFS1", "DBI", "FAHD2A", "NDUFA10", 
               "NEU4", "DNAJC28", "COMT", "NDUFA6", "MRPL1", "MCCC2", "MRPS36", 
               "HSD17B8", "MCCD1", "MRPS18B", "VARS2", "CHCHD2", "FAM185A", 
               "NDUFA5", "CYP11B1", "DECR1", "GLDC", "MRRF")

go_result <- enrichGO(
  gene = sig_genes,
  OrgDb = org.Hs.eg.db,
  keyType = "SYMBOL",
  ont = "BP",
  pvalueCutoff = 0.05
)


### Method 1：GOEnrichment分析（生物过程） ###
ego_bp <- enrichGO(gene = significant_genes,
                   universe = genes$Symbol,  # 背景Gene集
                   OrgDb = org.Hs.eg.db,
                   keyType = "SYMBOL",
                   ont = "BP",  # 生物过程
                   pvalueCutoff = 0.05,
                   pAdjustMethod = "none",
                   qvalueCutoff = 0.2,
                   readable = TRUE)

### 方法2：GOEnrichment分析（细胞组分） ###
ego_cc <- enrichGO(gene = significant_genes,
                   universe = genes$Symbol,
                   OrgDb = org.Hs.eg.db,
                   keyType = "SYMBOL",
                   ont = "CC",  # 细胞组分
                   pvalueCutoff = 0.05,
                   pAdjustMethod = "BH",
                   qvalueCutoff = 0.2,
                   readable = TRUE)

### 方法3：GOEnrichment分析（分子功能） ###
ego_mf <- enrichGO(gene = significant_genes,
                   universe = genes$Symbol,
                   OrgDb = org.Hs.eg.db,
                   keyType = "SYMBOL",
                   ont = "MF",  # 分子功能
                   pvalueCutoff = 0.05,
                   pAdjustMethod = "BH",
                   qvalueCutoff = 0.2,
                   readable = TRUE)














# PlotGO图
# 1. 点图（默认显示前20个Enrichment项）
dotplot(ego_bp, showCategory = 20, title = "GO Biological Process")

# 2. 条形图
barplot(ego_bp, showCategory = 15, title = "GO Biological Process")

# 3. 有向without环图（DAG）
plotGOgraph(ego_bp)

# 4. Enrichment图（cnetplot）
cnetplot(ego_bp, categorySize = "pvalue", 
         showCategory = 10,
         circular = FALSE, 
         colorEdge = TRUE)

# 5. Heatmap样式的Enrichment图
heatplot(ego_bp, showCategory = 15)

### 如果你想要更美观的图形，可以自定义ggplot2主题 ###

custom_plot <- dotplot(ego_bp, showCategory = 15) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  labs(title = "GO Enrichment Analysis (Biological Process)",
       x = "Gene Ratio",
       y = "GO Terms")

print(custom_plot)

# Save图形
# ggsave("D:\\biosoft\\1000thal\\mtDNA\\figure/figure6F.GO_enrichment_plot.pdf", 
#        plot = custom_plot,
#        width = 10, 
#        height = 8, 
#        dpi = 300)








pathwayClusters <- function(
    enrichment,methods=aPEAR.methods,verbose=FALSE){
  if (!methods::is(enrichment, 'data.frame')) {
    stop(paste0('Unrecognized data type for parameter "enrichment": ',
                paste(class(enrichment),collapse=','),'.Pleaseprovideadata.frame.'))}
  if (verbose) message('Validating parameters...')
  methods<-prepareMethods(methods,...)  
  if (verbose) message('Validating enrichment data...')
  data<-prepareEnrichment(enrichment,methods=methods,verbose=verbose,requireTheme=FALSE) 
  sim <- similarity(values = data$genes, method = methods$similarity, verbose = verbose)
  clusters<-pathwayClusters(sim=sim,minClusterSize=methods$minClusterSize,method=methods$cluster,
                              verbose=verbose)
  clusterNames<-clusterName(enrichment=data$enrichment,
                              sim=sim,clusters=clusters,method=methods$clusterName,verbose=verbose)
  clusters <- clusters %>% 
    merge(clusterNames, by = 'ClusterID') %>%
    .[,list(Pathway,Cluster)]
  return(list(clusters=clusters,similarity=sim))}



### 自定义画网络图的函数 ###

plotPathClusters <- function(enrichment,sim,
                             clusters,theme = aPEAR.theme,verbose = FALSE) {
  if (verbose) message('Validating theme parameters...')
  if (verbose) message('Preparing enrichment data for plotting...')
  data <- prepareEnrichment(enrichment, theme = theme, requireTheme = TRUE, verbose = verbose)
  # Merge with cluster data  
  enrichment <- data$enrichment %>% 
    data.table::as.data.table() %>%
    .[ , list(Pathway = Description, Color, Size) ] %>%
    merge(clusters) %>%
    .[ , ClusterSize := .N, by = Cluster ]
  if (verbose) message('Creating the enrichment graph...')
  graph <- enrichmentGraph(sim, clusters, theme)
  # Add enrichment data to the node coordinates
  nodes <- merge(graph$nodes, enrichment)
  # Filter edges to make sure we keep only those that have coordinates
  edges <- graph$edges  
  edges <- edges[ from %in% nodes[ , Pathway ] & to %in% nodes[ , Pathway ] ]
  .plotNodesAndEdges(nodes = nodes, edges = edges, theme = theme)}




# pathwayClusters <- function(enrichment,methods=aPEAR.methods,verbose=FALSE){
#   if (!methods::is(enrichment, 'data.frame')) {
#     stop(paste0('Unrecognized data type for parameter "enrichment": ',
#                 paste(class(enrichment),collapse=','),'.Pleaseprovideadata.frame.'))}
#   if (verbose) message('Validating parameters...')
#   methods<-prepareMethods(methods,...)
#   if (verbose) message('Validating enrichment data...')
#   data<-prepareEnrichment(enrichment,methods=methods,verbose=verbose,requireTheme=FALSE)
#   sim <- similarity(values = data$genes, method = methods$similarity, verbose = verbose)
#   clusters<-pathwayClusters(sim=sim,minClusterSize=methods$minClusterSize,method=methods$cluster,verbose=verbose)
#   clusterNames<-clusterName(enrichment=data$enrichment,sim=sim,clusters=clusters,method=methods$clusterName,verbose=verbose)
#   clusters <- clusters %>%
#     merge(clusterNames, by = 'ClusterID') %>%
#     .[,list(Pathway,Cluster)]
#   return(list(clusters=clusters,similarity=sim))}






