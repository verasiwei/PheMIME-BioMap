# PheMIME-BioMap: An Interactive Knowledge Base and Network Visualization for Exploring Shared Biomolecular Mechanisms in Disease Multimorbidity

## Objective

Multimorbidity, the co-occurrence of multiple diseases in an individual, presents a complex health challenge. These patterns, often represented as networks of disease-disease phenotypes co-occurrences derived from electronic health record (EHR) data, are frequently driven by underlying shared molecular mechanisms. Understanding these shared molecular mechanisms is crucial for improving diagnosis, treatment, and identifying drug repurposing opportunities. Despite their importance, the shared biomolecular mechanisms underlying complex multimorbidity networks remain underexplored. Large-scale EHR data, combined with multi-omics profiles predicted from genetic biobank data, provides a unique opportunity to comprehensively characterize multimorbidity by linking disease phenotypes to underlying molecular mechanisms. While these data have provided valuable insights, there remains a significant challenge in fully exploring and visualizing the complex molecular interactions driving these co-occurrences. Addressing this challenge requires robust knowledge bases and advanced tools for data integration and visualization. 

## Methods

To address this challenge, we developed PheMIME-BioNet, a comprehensive knowledge base built on large population-scale biobank data. This platform integrates genetic scores from the OmicsPred study on the UK Biobank and applies these scores to predict the multi-omics traits in the Vanderbilt BioVU Biobank. Using PheWAS on these predicted multi-omics traits and phenotype data from EHR, it constructs a large-scale phenotype-biomolecule network, enabling researchers to analyze shared molecular pathways across multiple diseases. Beyond constructing the knowledge base, we developed novel visualization tools, enabling researchers to efficiently identify shared and exclusive biomolecular patterns, facilitating insights into multimorbidity.

## Results

PheMIME-BioNet enables researchers to select disease phenotypes of interest, and also allows researchers to select highly co-occurring diseases or upload consistent disease-disease co-occurrence data, such as those from PheMIME. Through network-based analysis and visualization, PheMIME-BioNet identifies common biological pathways and clusters them based on disease phenotype similarities. Its advanced interactive visualization capabilities enable researchers to identify common pathways across disease-disease co-occurrence network and provide insights into potential drug repurposing targets. By leveraging large-scale population biobank data, PheMIME-BioNet addresses a critical gap in multimorbidity research, facilitating the integration of biomolecular interactions with disease co-occurrence patterns for better characterization and interpretation.


The current version is accessible at: [application](https://prod.tbilab.org/content/99395bec-6a59-4526-b1cf-e973e774626), which is still under development.

![Figure 1: Overview of PheMIME-BioMap](./www/Figure1.png)