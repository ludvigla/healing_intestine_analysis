# Healing intestine analysis

This repo contains rmarkdown notebooks used for the study "The spatial transcriptomic landscape of the healing intestine following damage", by M. Parigi et al.


## Installation

The notebook requires Seurat v3.1.5 and sctransform v0.2.1. You can get the correct packages using the `versions` R package.

```
install.packages("versions")
install.versions("Seurat", "3.1.5")
```

## Data

This repo includes spaceranger output files required for analysis. High resolution H&E images can be found on [GEO](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE169749).
