# cor2Venn

This package visualizes a correlation matrix as a Venn diagram. The overlap of circles represents the shared variance (R squared) between variables. Using additional features (e.g. cor2dist) the distance between variables is proportionate to their correlation (close = positive correlation; distant = negative correlation). The problem is optimized using Quasi-Newton optimization. Computation time increases exponentially the more variables are included. 

## Installation

```R 
library(devtools)
install_github("mhsteppan/cor2Venn")
```



## Examples

### Example on the g factor of intelligence

```R 
cormat<-ids2cormat
fit<-cor2Venn(ids2cormat)


mode<-c("Verbal Reasoning","Verbal Reasoning","Long-term memory","Long-term memory","Visual short-term memory","Visual short-term memory","Auditory short-term memory","Auditory short-term memory","Processing speed","Processing speed","Visual processing","Visual processing","Abstract reasoning","Abstract reasoning")

p<-cor2Vennplot(fit,manualfill=mode,manualalphafill = 0.2,labelfill=mode)

ggsave(p.file="ids2cor2venn.png")

```


### Example on the Big Five

Openpsychometrics.org published a dataset on N=19,719 individuals responding to a Big Five questionnaire. The raw data is used here. First the correlation matrix is calculated, then the visualization is fitted to the correlation matrix. Due to the fact that some items are inversely coded, the parameter Recode = TRUE is used, so that the highest correlation is always positive.

```R 
cormat<- cor(big5,use="pairwise.complete.obs")
fit <- cor2VennSolve(cormat)
plot(fit$p)

```

![Screenshot](ids2cor2venn2.png)


### Example on genetic correlations across psychiatric disorders

Abdellaoui et al. published genetic correlations across psychiatric disorders including substance use disorders. Genetic correlations represent the proportion of variance that two traits share due to genetic causes. Therefore, for visualization purposes we use squared = FALSE. 

```R 
cormat<- abdellaoui
fit <- cor2Venn(cormat)
plot(fit$p)

```

Source: Abdellaoui, A., Smit, D. J., van den Brink, W., Denys, D., & Verweij, K. J. (2021). Genomic relationships across psychiatric disorders including substance use disorders. Drug and alcohol dependence, 220, 108535.



![Screenshot](image1.png)


## How to cite 

Steppan, M. (2022). cor2Venn: visualizing correlation matrices as Venn diagrams. R package version 0.0.1

