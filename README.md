# SGEN-ME: Shared Genetic Architecture Map for Multimorbidity Explorer 

## Objective

Multimorbidity, the co-occurrence of multiple diseases in an individual, presents a complex health challenge. Shared genetic mechanisms are thought to play a role in driving these multimorbidity patterns. However, the conjoint genetic mechanisms underlying diverse diseases remain incompletely understood. Investigating these shared genetic mechanisms is crucial for uncovering patterns of disease co-occurrence and identifying potential biological pathways that link them. Existing approaches often focus on single SNP-disease associations, which limit the ability to capture the combined effects of genetic variants. Furthermore, these approaches primarily examine univariate genetic and phenotype relationships, leaving the multivariate interactions between diseases and biomolecules underexplored. Addressing these gaps requires robust tools that integrate genetic, molecular, and phenotype data to better explore and visualize the shared genetic architecture of multimorbidity.

## Methods

We developed SGEN-ME, a discovery platform on large population-scale genetic and phenotypic biobank data to investigate shared genetic architecture underlying multimorbidity. This platform integrates genetic and phenotypic data from large-scale biobanks to construct a robust knowledge base. Using effect sizes provided by OmicsPred for each SNP across various molecular traits, molecular profiles were predicted for individuals in the Vanderbilt BioVU Biobank. Phenome-wide association studies (PheWAS) were then conducted to identify univariate relationships between disease phenotypes and molecular profiles. Beyond univariate analysis, SGEN-ME incorporates novel interactive tools that emphasize multivariate interactions, offering researchers a framework to explore how shared genetic mechanisms and biological pathways drive disease co-occurrence. Key features include a unique real-time biological pathway clustering framework based on multimorbidity similarity, offering unique insights into how shared genetic architecture drives patterns of disease co-occurrence.

## Results

A Type 2 diabetes (T2D) case study demonstrates SGEN-ME’s capabilities in uncovering shared genetic architecture. The platform highlights well-known overlaps, such as the connection between T2D and hypercholesterolemia through lipid metabolic processes involving LDL cholesterol. It also reveals less-explored but evidence-supported links, such as the MAPK signaling pathway connecting T2D and non-melanoma skin cancer. These findings showcase SGEN-ME’s ability to uncover shared genetic variants and enriched pathways that drive multimorbidity. These findings highlight SGEN-ME’s dual capacity to validate known relationships while identifying novel shared genetic mechanisms that underpin disease co-occurrence. By linking genetic similarities across phenotypes, SGEN-ME provides a robust framework for exploring shared genetic factors, prioritizing pathways for further investigation, and advancing multimorbidity research by bridging genetic and phenotypic data.

The current version is accessible at: [application](https://prod.tbilab.org/content/99395bec-6a59-4526-b1cf-e973e774626), which is still under development.

![Figure 1: Overview of SGEN-ME](./www/Figure1.png)