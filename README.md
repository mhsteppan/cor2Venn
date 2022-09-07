# cor2Venn

This package visualizes a correlation matrix as a Venn diagram. The overlap of circles represents the shared variance (R squared) between variables. Using additional features (e.g. cor2dist) the distance between variables is proportionate to their correlation (close = positive correlation; distant = negative correlation). The problem is optimized using Quasi-Newton optimization. Computation time increases exponentially the more variables are included. 

## Installation

```R 
library(devtools)
install_github("mhsteppan/cor2Venn")
library(cor2Venn)
```



## Examples



### Example on the Big Five

Openpsychometrics.org published a dataset on N=19,719 individuals responding to a Big Five questionnaire. The raw data is used here. First the correlation matrix is calculated, then the visualization is fitted to the correlation matrix. Due to the fact that some items are inversely coded, the parameter Recode = TRUE is used, so that the highest correlation is always positive.

```R 
cormat<- cor(big5,use="pairwise.complete.obs")
fitbig5 <- cor2Venn(cormat,recode=TRUE)

p<-cor2Vennplot(fitbig5,manualalphafill=0.2,fillmode="mclust",density = T)
p<-p+scale_fill_discrete(labels=c("Extraversion","Neuroticism","Agreeableness","Conscientiousness","Openness"))

```
![Screenshot](big5cor2venn.png)


### Example on genetic correlations across psychiatric disorders - the p' factor 

Abdellaoui et al. published genetic correlations across psychiatric disorders including substance use disorders. Genetic correlations represent the proportion of variance that two traits share due to genetic causes. Therefore, for visualization purposes we use squared = FALSE. 

```R 
cormat<- abdellaouietal
fitrg <- cor2Venn(cormat, Rsquared=F,cor2dist=T)
p<-cor2Vennplot(fitrg,manualalphafill=0.2,density=T)
p<-p+scale_fill_gradient(high="red",low="yellow")
ggsave(p,file="abdellaouicor2venn.png")

```

![Screenshot](abdellaouicor2venn.png)

Source: Abdellaoui, A., Smit, D. J., van den Brink, W., Denys, D., & Verweij, K. J. (2021). Genomic relationships across psychiatric disorders including substance use disorders. Drug and alcohol dependence, 220, 108535.



### Example on the g' factor of intelligence

This is an example on 14 subtests of the intelligence and development test (IDS-2). The visualization shows a 'positive manifold', i.e. that all subtests share variance with each other. Some subtests (e.g. reasoning) are more central than others (e.g. memory).  

```R 
cormat<-ids2cormat
fit<-cor2Venn(ids2cormat)


mode<-c("Verbal Reasoning","Verbal Reasoning","Long-term memory","Long-term memory","Visual short-term memory","Visual short-term memory","Auditory short-term memory","Auditory short-term memory","Processing speed","Processing speed","Visual processing","Visual processing","Abstract reasoning","Abstract reasoning")

p<-cor2Vennplot(fit,manualfill=mode,manualalphafill = 0.2,labelfill=mode)

ggsave(p,file="ids2cor2venn.png")

```

![Screenshot](ids2cor2venn.png)



## How to cite 

Steppan, M. (2022). cor2Venn: visualizing correlation matrices as Venn diagrams. R package version 0.0.1

