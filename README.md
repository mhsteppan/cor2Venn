# cor2venn

This package visualizes a correlation matrix as a Venn diagram. The overlap of circles represents the shared variance (R squared) between variables. Using additional features (e.g. cor2dist) the distance between two variables is proportionate to their correlation (close = positive correlation; distant = negative correlation). 

## Installation

```R 
library(devtools)
install_github("mhsteppan/cor2venn")
```

## Examples

### Example on genetic correlations across psychiatric disorders

```R 
cormat<- abdellaoui
fit <- cor2venn(cormat)
plot(fit$p)

```

Source: Abdellaoui, A., Smit, D. J., van den Brink, W., Denys, D., & Verweij, K. J. (2021). Genomic relationships across psychiatric disorders including substance use disorders. Drug and alcohol dependence, 220, 108535.



![Screenshot](image1.png)
