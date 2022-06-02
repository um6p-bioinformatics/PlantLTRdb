# PlantLTRdb
PlantLTRdb: An interactive database of plant LTR-Retrotransposons

This repository contains: 

1- The perl script (LTR-RT-gene-chimera.pl) used to identify LTR-RTs gene chimera based on LTR-retrotransposons localization in genome sequence. The Perl script compared the LTR-RT start and end with gene start and end within the genome. LTR-RT was considered a LTR-RT gene chimera if it was located within the gene start and end coordinates provided by the gene annotation in the GFF files.

2- R scripts (Statistical_Correlation_analysis.r & Normal_distribution.r) used to perform the correlation analysis to reveal the relationship between plant genome size, LTR-RT insertion age, and LTR-RT length.
