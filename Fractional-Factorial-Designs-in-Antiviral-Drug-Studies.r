#First, we create a dataframe based on Table(II) from A to E and two replications.


```{r}
experiment <- data.frame(
   Run = 1:34,
   A = rep(c(1,-1,-1,-1,-1,1,1,1,1,1,1,-1,-1,-1,-1,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1)),
   A2 = rep(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,0,1,1,0,1,1,0,1,1,0,1,1,0,1)),
   B = rep(c(-1,1,-1,-1,-1,1,1,1,-1,-1,-1,1,1,1,-1,1,-1,0,1,-1,0,1,0,1,-1,1,-1,0,0,1,-1,1,-1,0)),
   B2 = rep(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,0,1,0,1,1,1,1,0,0,1,1,1,1,0)),
   C = rep(c(-1,-1,1,-1,-1,1,-1,-1,1,1,-1,1,-1,1,1,1,-1,0,1,0,1,-1,-1,0,1,1,-1,0,1,-1,0,0,1,-1)),
   C2 = rep(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,0,1,1,1,0,1,1,1,0,1,1,0,0,1,1)),
   D = rep(c(-1,-1,-1,1,-1,-1,1,-1,1,-1,1,1,1,-1,1,1,-1,0,1,0,1,-1,1,-1,0,0,1,-1,-1,0,1,1,-1,0)),
   D2 = rep(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,0,1,1,1,1,0,0,1,1,1,0,1,1,1,0)),
   E = rep(c(-1,-1,-1,-1,1,-1,-1,1,-1,1,1,-1,1,1,1,1,-1,0,1,1,-1,0,0,1,-1,0,1,-1,1,-1,0,-1,0,1)),
   E2 = rep(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,0,0,1,1,0,1,1,1,1,0,1,0,1)),
   Replicate1 = c(69.8,66.4,83.0,16.2,46.1,68.6,6.8,15.6,11.1,19.8,3.7,5.8,2.6,42.2,1.8,3.1,78.6,13.3,3.4,21.4,8.6,18.0,7.3,17.9,52.9,13.2,2.1,73.4,19.6,59.1,1.4,7.3,22.3,14.1 ),
   Replicate2 = c(72.0,67.4,68.6,23.4,33.6,65.5,7.2,19.1,7.0,20.3,4.7,3.9,4.0,23.2,5.2,3.4,81.9,16.7,3.8,25.2,4.4,27.3,2.4,23.7,54.3,8.8,4.5,73.9,14.6,41.7,2.6,4.8,24.0,18.3))
   
experiment

```
#We ordered the replication of the experiments with the replecation 1 starting first and them replication2, respectively.
```{r}
library(tidyverse)

etch_data <- experiment %>% 
   pivot_longer(Replicate1:Replicate2,
                names_to = "Replicate",
                names_prefix = "Replicate",
                values_to = "a") %>% 
   relocate(Replicate) %>% 
   arrange(Replicate)
etch_data
```

```{r}
etch_aov <- aov(a~ A + B + C +D+E+A2+B2+C2+D2+E2+A:B+A:C+A:D+A:E+B:C+B:D+B:E+C:D+C:E+D:E , data = etch_data)
anova(etch_aov)
```


```{r}
par(mfrow=c(2,2))
 plot(etch_aov)
```





```{r}
 etch_lm <- lm(a ~ A + B + C +D+E+A2+B2+C2+D2+E2+A:B+A:C+A:D+A:E+B:C+B:D+B:E+C:D+C:E+D:E, data = etch_data)
summary(etch_lm)

```
```{r}
anova(etch_lm)
```
```{r}
filter_lm <- lm(a ~ A*B, data = etch_data)
summary(filter_lm)
```

```{r}
 library(rsm)
par(mfrow=c(1,2))

 contour(filter_lm , B ~ A, image = TRUE, main = "(a)")

png("grapha")
```

```{r}
filter_lm1 <- lm(a ~  A+B+A:C+C+D+E, data = etch_data)
summary(filter_lm1)
```
```{r}
par(mfrow=c(1,2))
contour(filter_lm1 , C ~ A, image = TRUE, main = "(b)")
```


```{r}
filter_lm2 <- lm(a ~ D*E, data = etch_data)
summary(filter_lm2)
```
```{r}
par(mfrow=c(1,2))
 contour(filter_lm2 , E ~ D, image = TRUE, main = "(C)")
```
Column B

```{r}
experiment1 <- data.frame(
   Run = 1:34,
   A = rep(c(1,-1,-1,-1,-1,1,1,1,1,1,1,-1,-1,-1,-1,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1)),
   A2 = rep(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,0,1,1,0,1,1,0,1,1,0,1,1,0,1)),
   B = rep(c(-1,1,-1,-1,-1,1,1,1,-1,-1,-1,1,1,1,-1,1,-1,0,1,-1,0,1,0,1,-1,1,-1,0,0,1,-1,1,-1,0)),
   B2 = rep(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,0,1,0,1,1,1,1,0,0,1,1,1,1,0)),
   C = rep(c(-1,-1,1,-1,-1,1,-1,-1,1,1,-1,1,-1,1,1,1,-1,0,1,0,1,-1,-1,0,1,1,-1,0,1,-1,0,0,1,-1)),
    C2 = rep(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,0,1,1,1,0,1,1,1,0,1,1,0,0,1,1)),
   D = rep(c(-1,-1,-1,1,-1,-1,1,-1,1,-1,1,1,1,-1,1,1,-1,0,1,0,1,-1,1,-1,0,0,1,-1,-1,0,1,1,-1,0)),
   D2 = rep(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,0,1,1,1,1,0,0,1,1,1,0,1,1,1,0)),
   E = rep(c(-1,-1,-1,-1,1,-1,-1,1,-1,1,1,-1,1,1,1,1,-1,0,1,1,-1,0,0,1,-1,0,1,-1,1,-1,0,-1,0,1)),
   E2 = rep(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,0,0,1,1,0,1,1,1,1,0,1,0,1)),
   Replicate1 = c(sqrt(69.8),sqrt(66.4),sqrt(83),sqrt(16.2),sqrt(46.1),sqrt(68.6),sqrt(6.8),sqrt(15.6),sqrt(11.1),sqrt(19.8),sqrt(3.7),sqrt(5.8),sqrt(2.6),sqrt(42.2), sqrt(1.8),sqrt(3.1),sqrt(78.6),sqrt(13.3),sqrt(3.4),sqrt(21.4),sqrt(8.6),sqrt(18.0),sqrt(7.3),sqrt(17.9),sqrt(52.9),sqrt(13.2),sqrt(2.1),sqrt(73.4),sqrt(19.6),sqrt(59.1),sqrt(1.4),sqrt(7.3),sqrt(22.3),sqrt(14.1)),
   Replicate2 = c(sqrt(72.0),sqrt(67.4),sqrt(68.6),sqrt(23.4),sqrt(33.6),sqrt(65.5),sqrt(7.2),sqrt(19.1),sqrt(7.0),sqrt(20.3),sqrt(4.7),sqrt(3.9),sqrt(4.0),sqrt(23.2),sqrt(5.2),sqrt(3.4),sqrt(81.9),sqrt(16.7),sqrt(3.8),sqrt(25.2),sqrt(4.4),sqrt(27.3),sqrt(2.4),sqrt(23.7),sqrt(54.3),sqrt(8.8),sqrt(4.5),sqrt(73.9),sqrt(14.6),sqrt(41.7),sqrt(2.6),sqrt(4.8),sqrt(24.0),sqrt(18.3)))
   
experiment1
```

```{r}
library(tidyverse)

etch_data1 <- experiment1 |> 
   pivot_longer(Replicate1:Replicate2,
                names_to = "Replicate",
                names_prefix = "Replicate",
                values_to = "EtchRate") |> 
   relocate(Replicate) |> 
   arrange(Replicate)
etch_data1
```






```{r}
etch_aov1 <- aov(EtchRate ~ A + B + C +D+E+A2+B2+C2+D2+E2+A:B+A:C+A:D+A:E+B:C+B:D+B:E+C:D+C:E+D:E, data = etch_data1)
anova(etch_aov1)
```



```{r}
par(mfrow=c(2,2))
 plot(etch_aov1)
```
```{r}
 etch_lm1 <- lm(EtchRate ~ A + B + C +D+E+A2+B2+C2+D2+E2+A:B+A:C+A:D+A:E+B:C+B:D+B:E+C:D+C:E+D:E, data = etch_data1)
summary(etch_lm1)

```
```{r}
anova(etch_lm1)
```

Column C

```{r}
experiment2 <- data.frame(
   Run = 1:34,
   A = rep(c(1,-1,-1,-1,-1,1,1,1,1,1,1,-1,-1,-1,-1,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1)),
   A2 = rep(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,0,1,1,0,1,1,0,1,1,0,1,1,0,1)),
   B = rep(c(-1,1,-1,-1,-1,1,1,1,-1,-1,-1,1,1,1,-1,1,-1,0,1,-1,0,1,0,1,-1,1,-1,0,0,1,-1,1,-1,0)),
   B2 = rep(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,0,1,0,1,1,1,1,0,0,1,1,1,1,0)),
   C = rep(c(-1,-1,1,-1,-1,1,-1,-1,1,1,-1,1,-1,1,1,1,-1,0,1,0,1,-1,-1,0,1,1,-1,0,1,-1,0,0,1,-1)),
    C2 = rep(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,0,1,1,1,0,1,1,1,0,1,1,0,0,1,1)),
   D = rep(c(-1,-1,-1,1,-1,-1,1,-1,1,-1,1,1,1,-1,1,1,-1,0,1,0,1,-1,1,-1,0,0,1,-1,-1,0,1,1,-1,0)),
   D2 = rep(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,0,1,1,1,1,0,0,1,1,1,0,1,1,1,0)),
   E = rep(c(-1,-1,-1,-1,1,-1,-1,1,-1,1,1,-1,1,1,1,1,-1,0,1,1,-1,0,0,1,-1,0,1,-1,1,-1,0,-1,0,1)),
   E2 = rep(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,0,0,1,1,0,1,1,1,1,0,1,0,1)),
   Replicate1 = c(sqrt(69.8),sqrt(66.4),sqrt(83),sqrt(16.2),sqrt(46.1),sqrt(68.6),sqrt(6.8),sqrt(15.6),sqrt(11.1),sqrt(19.8),sqrt(3.7),sqrt(5.8),sqrt(2.6),NaN, sqrt(1.8),sqrt(3.1),sqrt(78.6),sqrt(13.3),sqrt(3.4),sqrt(21.4),sqrt(8.6),sqrt(18.0),sqrt(7.3),sqrt(17.9),sqrt(52.9),sqrt(13.2),sqrt(2.1),sqrt(73.4),sqrt(19.6),sqrt(59.1),sqrt(1.4),sqrt(7.3),sqrt(22.3),sqrt(14.1)),
   Replicate2 = c(sqrt(72.0),sqrt(67.4),sqrt(68.6),sqrt(23.4),sqrt(33.6),sqrt(65.5),sqrt(7.2),sqrt(19.1),sqrt(7.0),sqrt(20.3),sqrt(4.7),sqrt(3.9),sqrt(4.0),sqrt(23.2),sqrt(5.2),sqrt(3.4),sqrt(81.9),sqrt(16.7),sqrt(3.8),sqrt(25.2),sqrt(4.4),sqrt(27.3),sqrt(2.4),sqrt(23.7),sqrt(54.3),sqrt(8.8),sqrt(4.5),sqrt(73.9),sqrt(14.6),sqrt(41.7),sqrt(2.6),sqrt(4.8),sqrt(24.0),sqrt(18.3)))
   
experiment2
```
```{r}
library(tidyverse)

etch_data2 <- experiment2 |> 
   pivot_longer(Replicate1:Replicate2,
                names_to = "Replicate",
                names_prefix = "Replicate",
                values_to = "EtchRate") |> 
   relocate(Replicate) |> 
   arrange(Replicate)
etch_data2
```
```{r}
etch_aov2 <- aov(EtchRate ~ A + B + C +D+E+A2+B2+C2+D2+E2+A:B+A:C+A:D+A:E+B:C+B:D+B:E+C:D+C:E+D:E, data = etch_data2)
anova(etch_aov2)
```
```{r}
par(mfrow=c(2,2))
 plot(etch_aov2)
```

```{r}
 etch_lm2 <- lm(EtchRate ~ A + B + C +D+E+A2+B2+C2+D2+E2+A:B+A:C+A:D+A:E+B:C+B:D+B:E+C:D+C:E+D:E, data = etch_data2)
summary(etch_lm2)

```
```{r}
anova(etch_lm2)
```


