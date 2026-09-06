# Mitochondrial DNA Analysis Pipeline

R analysis scripts for a whole-genome sequencing (WGS) study of mitochondrial DNA (mtDNA)
variation in a 1,000-person β-thalassemia cohort (1,020 pediatric probands + 409 parental
samples) plus a 58-person healthy control cohort. All scripts are organized by manuscript
figure and were used to produce the analyses for the associated submission.

> The manuscript is under peer review. The data underlying these analyses are not
> redistributed with this repository; the scripts expect the directory layout described in
> [Data layout](#data-layout) below.

## Cohort

| Cohort | n | Description |
|---|---|---|
| Thalassemia | 1,020 | Pediatric β-thalassemia probands (β⁰/β⁰, β⁰/β⁺, β⁺/β⁺, β⁰/HPFH, etc.) |
| Carriers | 409 | Unaffected parents of the 1,020 probands |
| Healthy | 58 | Healthy controls without thalassemia genotypes |

Public reference panels (UK Biobank, All of Us, gnomAD v3.1) are pulled for comparative
analyses in several figures.

## File layout

Scripts are named `figure{N}{panel}.{topic}.R` matching the manuscript figure number.
A single non-figure utility script is included at the top level:

| File | Manuscript figure | Topic |
|---|---|---|
| `figure1B.mtDNA_coverage.R`               | Figure 1B  | mtDNA / nuclear DNA coverage scatter & density |
| `figure1C.mtCN_cohort_boxplot.R`          | Figure 1C  | mtCN boxplot across cohorts |
| `figure1CAB.mtDNA_coverage.R`             | Figure 1C-A-B | mtDNA coverage composite (Thalassemia / Carriers / Healthy) |
| `figure1D.heteroplasmy_counts.R`          | Figure 1D  | Per-heteroplasmy level variant counts |
| `figure1E.mutations_per_sample.R`         | Figure 1E  | Mutations per sample distribution |
| `figure2A.mtCN_variable_associations_mantel.R` | Figure 2A | mtCN associations (Mantel test) |
| `figure2B.mtDNA_correction_model.R`       | Figure 2B  | mtDNA coverage correction model |
| `figure2C.mtCN_HbF_thalassemia_phenotype.R` | Figure 2C | mtCN–HbF–phenotype associations |
| `figure2DE.mutations_phenotype_cutoff.R`  | Figure 2D-E | Mutations × phenotype (cutoff-based) |
| `figure3A.maternal_difference_cutoff.R`   | Figure 3A  | Maternal-inheritance heteroplasmy differences (cutoff) |
| `figure3B.mtDNA_atlas_409.R`              | Figure 3B  | mtDNA atlas across 409 family trios |
| `figure3C.pie_cutoff.R`                   | Figure 3C  | Variant composition (pie, cutoff-based) |
| `figure3D.substitutions_per_gene.R`       | Figure 3D  | Substitution spectrum per gene |
| `figure3E1.HL2mut_cutoff.R`               | Figure 3E-1 | Heteroplasmic mutation cutoff |
| `figure3E2.oncoplot_mut2maf_cutoff.R`     | Figure 3E-2 | Oncoplot of mut2maf (cutoff) |
| `figure3F.mutations_per_sample_plot.R`    | Figure 3F  | Per-sample mutation landscape |
| `figure3G.mitochondrial_mutational_burden.R` | Figure 3G | Mitochondrial mutational burden |
| `figure4A1.HL2AC_counts_thalassemia_freq.R` | Figure 4A-1 | HL2-AC counts × thalassemia frequency |
| `figure4A2.HL_difference.R`               | Figure 4A-2 | HL difference comparison |
| `figure4B1.maternal_inheritance.R`        | Figure 4B-1 | Maternal inheritance analysis |
| `figure4B2.de_novo_mutations_inheritance.R` | Figure 4B-2 | De novo mutation inheritance |
| `figure4C.DNM_density_dloop_mt14766.R`    | Figure 4C  | DNM density in D-loop and mt14766 |
| `figure4D.maternal_heteroplasmy_phenotype.R` | Figure 4D | Maternal heteroplasmy × phenotype |
| `figure4E.mt14766_phenotype.R`            | Figure 4E  | mt14766 phenotype analysis |
| `figure4F.mt_gene_nonsynonymous_mutations.R` | Figure 4F | mt gene non-synonymous mutation spectrum |
| `figure5A1B.heteroplasmy_phenotype_ttest.R` | Figure 5A-1-B | Heteroplasmy × phenotype (t-test, panel A1+B) |
| `figure5A2.heteroplasmy_phenotype_ttest.R` | Figure 5A-2 | Heteroplasmy × phenotype (t-test, panel A2) |
| `figure5C.mt_heatmap.R`                   | Figure 5C  | mt gene heatmap |
| `figure6.heteroplasmy_common_variants.R`  | Figure 6   | Heteroplasmy of common variants |
| `figure6AB.AC_DAF_difference.R`           | Figure 6A-B | AC and DAF difference |
| `figure6C.genetic_load_volcano.R`         | Figure 6C  | Volcano plot of differential genetic load |
| `figure6D.DAF_heatmap.R`                  | Figure 6D  | DAF heatmap |
| `figure6E.variants_stackplot.R`           | Figure 6E  | Variant stackplot |
| `figure6F.GO_analysis.R`                  | Figure 6F  | GO enrichment analysis |
| `strand_bias_correction.R`                | (utility)  | Strand-bias correction for mtDNA variant calls |

## Data layout

Each script reads from a hard-coded local path rooted at `D:/biosoft/1000thal/`. To
reproduce the analyses, set up the following directory layout (paths can be edited
at the top of each script):

```
D:/biosoft/1000thal/
├── 千人/                                  # Phenotype sheets
│   ├── ID对应(1020+409)2023.12.22.xlsx
│   ├── RNO.1 Basic statistics of 1020 β-thalassemia patients.xlsx
│   └── RNO.4.1 Basic information of the 409 parental samples.xlsx
└── mtDNA/
    ├── 1020mtDNA_out/                     # Per-cohort mtCN tables
    │   ├── 1020.mtCN_mean.tsv
    │   ├── 409.mtCN_mean.tsv
    │   └── norm.mtCN_mean.tsv
    ├── 58norm_people/finalout/58mtCN.xlsx # Healthy cohort mtCN
    ├── genemut/                           # Per-position mutation tables
    │   ├── 1020.AC0.1_202512.tsv
    │   ├── 409/409.mt.ano.filter.hl2zero
    │   └── ...
    ├── MTdatabase/gnomAD/                 # Public reference panels
    ├── Maternal inheritance/              # Trio-level inheritance calls
    ├── gwas/human_mito_genes/             # GWAS-ready per-gene tables
    └── figure/                            # Output PDF destination
```

## Dependencies

R (≥ 4.2) with the following packages:

```
readxl, writexl, openxlsx,
dplyr, tibble, tidyr, data.table, purrr,
ggplot2, ggsignif, ggpubr, gghalves, ggrepel, ggsci, ggh4x,
patchwork, scales,
ggstatsplot, pairwiseComparisons,
paletteer, ComplexHeatmap, circlize, maftools,
pheatmap, plotly, vcfR, rstatix, broom
```

Install with:

```r
install.packages(c(
  "readxl","writexl","openxlsx","dplyr","tibble","tidyr","data.table","purrr",
  "ggplot2","ggsignif","ggpubr","gghalves","ggrepel","ggsci","ggh4x",
  "patchwork","scales","ggstatsplot","paletteer","ComplexHeatmap","circlize",
  "maftools","pheatmap","plotly","vcfR","rstatix","broom"
))
BiocManager::install(c("ComplexHeatmap","circlize"))
remotes::install_github("IndrajeetPatil/ggstatsplot")
remotes::install_github("r-lib/pairwiseComparisons")
```

## Reproducing a figure

Each script is self-contained — it loads its own data, runs its own statistics, and
saves its own PDF. To re-run a single figure:

```bash
Rscript figure3B.mtDNA_atlas_409.R
```

PDF output is written to `D:/biosoft/1000thal/mtDNA/figure/` by default.

## Conventions

- **mtCN** = mitochondrial copy number (mtDNA / nucDNA depth ratio)
- **HL** = heteroplasmy level (alternate-allele fraction at a position)
- **DAF** = derived allele frequency
- **AC / AN** = allele count / allele number
- **DNM** = de novo mutation
- **OXPHOS** = oxidative phosphorylation complex (used for the 13 protein-coding genes)

The cohort labels (`Thalassemia`, `Carriers`, `Healthy`) are reused as factor levels
and color legends across figures.

## Status

Code accompanies a manuscript currently under peer review. File names reflect the
manuscript figure numbering; paths inside each script point to the original working
tree and should be edited before running on a new machine.
