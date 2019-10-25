Pset 1
================
Maria Neely
10/24/2019

``` r
#2b. only include years 2009 and 2010
clean_dset <- x %>% filter(year == 2009 | year == 2010)

#2a. select relevant variables 

clean_dset <- clean_dset %>% 
  select(t_slength, slength, salary_real, expend, fips)

#2c omit all missing values
clean_dset <- na.omit(clean_dset)

#2d standardize input features using function scale()
clean_dset[c(1:4)] <- scale(clean_dset[c(1:4)])

#2e get data into workable form
state_ids <- x %>% select(fips, stateabv) %>% distinct()
dset_with_state <- merge(clean_dset, state_ids, by = "fips")
dset_with_state <- dset_with_state[-c(1)]

clean_dset<- clean_dset[,1:4]
```

    ## Warning: package 'seriation' was built under R version 3.5.2

![](pset_1_files/figure-markdown_github/problem%203-1.png)

1.  After assessing the Visual Assessment of Tendency (VAT), it is all one muddled color, suggesting there is no apparent natural pattern in our data between salary, expenditure, total length of sessions (special and regular) and length of regular sessions. This does not mean that there are no clusters within the data. Rather, it shows on precursory analysis that these variables are dissimilar. Further analysis is needed to see if there are in fact clusters in our data.

``` r
rownames(dset_with_state) <- dset_with_state[,5]
dset_with_state <- dset_with_state[-5]
y <- dist(dset_with_state)
hc_complete <- hclust(y, method="complete"); plot(hc_complete, hang=-1)
rect.hclust(hc_complete, k =3)
```

![](pset_1_files/figure-markdown_github/problem%204-1.png)

``` r
#cut the trees
cuts <- cutree(hc_complete, 
               k = c(2,3,4))


### Compare tables to see where the clusters lie
table_2_v_3 <- table('2 Clusters' = cuts[,1],
                     '3 Clusters' = cuts[,2])

table3_v_4 <- table('3 Clusters' = cuts[,2], 
                    '4 Clusters' = cuts[,3])

table_2_v_3
```

    ##           3 Clusters
    ## 2 Clusters  1  2  3
    ##          1 41  0  0
    ##          2  0  1  6

``` r
table3_v_4
```

    ##           4 Clusters
    ## 3 Clusters  1  2  3  4
    ##          1 41  0  0  0
    ##          2  0  1  0  0
    ##          3  0  0  4  2

1.  After looking at our dendrogram (and graphing the clusters), we see that California lies alone in a cluster, and most other states are clustered together. There is another cluster including Massachusetts, New York, Pennsylvania, Ohio, Illinois and Michigan, which makes logical sense because these states' legislatures have special characteristics, such as term limits, which according to Squire (2007) increases professionalism (New York, Michigan and Ohio), and extremely high salaries (Pennsylvania, Illinois, Michigan and Ohio). Given the continuous variables we worked with, we can say there is a range of legislative professionalism throughout states, rather than legislative professionalism being a binary of either professional or not professional. We see similar results after examining tables comparing having 2 or 3 clusters, or having 3 or 4 clusters. When comparing having 2 clusters or 3 clusters, we see that there is the largest cluster is cluster 1, and the second largest cluster is cluster 3, indicating 3 clusters is more appropriate than 2 clusters. Furthermore, when comparing having 3 clusters or 4 clusters, we once again see here is the largest cluster is cluster 1, and the second largest cluster is cluster 3, indicating once again 3 clusters may be most appropriate.

``` r
#fit kmeans algorithm 
library(clValid)
set.seed(4545)


kmeans <- kmeans(y, 
                 centers = 2,
                 nstart = 15)
kmeans
```

    ## K-means clustering with 2 clusters of sizes 4, 44
    ## 
    ## Cluster means:
    ##         AL       AK       AZ       AR       CA       CO       CT       DE
    ## 1 6.180794 4.916976 4.353007 6.270389 3.770741 4.582782 5.118508 5.812728
    ## 2 1.313320 1.639660 2.287063 1.247765 7.566708 1.783392 1.205268 1.500295
    ##         FL       GA       HI       ID       IL       IN       IA       KS
    ## 1 5.374902 6.357367 5.107842 5.594597 4.414618 5.451008 5.535756 5.711664
    ## 2 2.126072 1.285879 1.530415 1.201434 2.234353 1.082043 1.098381 1.331308
    ##         KY       LA       ME       MD       MA       MI       MN       MS
    ## 1 5.779002 5.636328 6.279251 5.132699 3.043353 3.614834 5.656249 5.044040
    ## 2 1.116576 1.106716 1.207869 1.391105 4.882742 3.005405 1.168378 2.079712
    ##         MO       MT       NE       NV       NH       NM       NY       NC
    ## 1 5.196780 6.490008 5.373304 6.091309 5.982135 6.764328 2.687914 5.087340
    ## 2 1.316998 1.343922 1.236726 1.578718 1.502669 1.580906 6.309395 1.482119
    ##         ND       OH       OK       OR       PA       RI       SC       SD
    ## 1 6.625512 3.624916 5.250080 5.527680 2.925670 5.798307 5.247063 6.641824
    ## 2 1.413285 2.939838 1.293024 1.092817 3.706023 1.087239 1.393840 1.428848
    ##         TN       TX       UT       VT       VA       WA       WV      WY
    ## 1 5.975776 5.840890 6.807747 6.421457 6.171305 5.031560 6.061621 7.17727
    ## 2 1.117375 1.694376 1.564858 1.317748 1.233826 1.443474 1.144182 1.91503
    ## 
    ## Clustering vector:
    ## AL AK AZ AR CA CO CT DE FL GA HI ID IL IN IA KS KY LA ME MD MA MI MN MS MO 
    ##  2  2  2  2  1  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  1  2  2  2  2 
    ## MT NE NV NH NM NY NC ND OH OK OR PA RI SC SD TN TX UT VT VA WA WV WY 
    ##  2  2  2  2  2  1  2  2  2  2  2  1  2  2  2  2  2  2  2  2  2  2  2 
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1]  450.0427 1114.3191
    ##  (between_SS / total_SS =  66.0 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"    
    ## [5] "tot.withinss" "betweenss"    "size"         "iter"        
    ## [9] "ifault"

``` r
kmeans$cluster
```

    ## AL AK AZ AR CA CO CT DE FL GA HI ID IL IN IA KS KY LA ME MD MA MI MN MS MO 
    ##  2  2  2  2  1  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  1  2  2  2  2 
    ## MT NE NV NH NM NY NC ND OH OK OR PA RI SC SD TN TX UT VT VA WA WV WY 
    ##  2  2  2  2  2  1  2  2  2  2  2  1  2  2  2  2  2  2  2  2  2  2  2

``` r
kmeans$centers
```

    ##         AL       AK       AZ       AR       CA       CO       CT       DE
    ## 1 6.180794 4.916976 4.353007 6.270389 3.770741 4.582782 5.118508 5.812728
    ## 2 1.313320 1.639660 2.287063 1.247765 7.566708 1.783392 1.205268 1.500295
    ##         FL       GA       HI       ID       IL       IN       IA       KS
    ## 1 5.374902 6.357367 5.107842 5.594597 4.414618 5.451008 5.535756 5.711664
    ## 2 2.126072 1.285879 1.530415 1.201434 2.234353 1.082043 1.098381 1.331308
    ##         KY       LA       ME       MD       MA       MI       MN       MS
    ## 1 5.779002 5.636328 6.279251 5.132699 3.043353 3.614834 5.656249 5.044040
    ## 2 1.116576 1.106716 1.207869 1.391105 4.882742 3.005405 1.168378 2.079712
    ##         MO       MT       NE       NV       NH       NM       NY       NC
    ## 1 5.196780 6.490008 5.373304 6.091309 5.982135 6.764328 2.687914 5.087340
    ## 2 1.316998 1.343922 1.236726 1.578718 1.502669 1.580906 6.309395 1.482119
    ##         ND       OH       OK       OR       PA       RI       SC       SD
    ## 1 6.625512 3.624916 5.250080 5.527680 2.925670 5.798307 5.247063 6.641824
    ## 2 1.413285 2.939838 1.293024 1.092817 3.706023 1.087239 1.393840 1.428848
    ##         TN       TX       UT       VT       VA       WA       WV      WY
    ## 1 5.975776 5.840890 6.807747 6.421457 6.171305 5.031560 6.061621 7.17727
    ## 2 1.117375 1.694376 1.564858 1.317748 1.233826 1.443474 1.144182 1.91503

``` r
kmeans$size
```

    ## [1]  4 44

``` r
#Scatterplots to better see what is going on visually

ggplot(clean_dset, 
       aes(x=salary_real, y=expend)) + 
  geom_point(aes(color = kmeans$cluster)) + 
  xlab("Salary") +
  ylab("Expenditures") + 
  theme_bw()
```

![](pset_1_files/figure-markdown_github/problem%205-1.png)

``` r
ggplot(clean_dset, 
       aes(x=salary_real, y=t_slength)) + 
  geom_point(aes(color = kmeans$cluster)) + 
  xlab("Salary") +
  ylab("Total Session Length (regular and special)") + 
  theme_bw()
```

![](pset_1_files/figure-markdown_github/problem%205-2.png)

``` r
ggplot(clean_dset, 
       aes(x=slength, y=t_slength)) + 
  geom_point(aes(color = kmeans$cluster)) + 
  xlab("Regular Session Length") +
  ylab("Total Session Length (regular and special)") + 
  theme_bw()
```

![](pset_1_files/figure-markdown_github/problem%205-3.png)

1.  Given our kmeans output, we have two clusters of 4 and 44 states. However, the sum of squares by cluster is only 66%, indicating that only 66% of the data is well grouped. When looking at the kmeans method visually, we see some distinction between clusters 1 and 2 when comparing salary vs. total session lengths (cluster 1 being in the upper right section of the graph, cluster 2 being in the lower left section of the graph). When comparing Salary vs. Expenditures and regular session length vs. total session length, there is less distinction between the two clusters. Thus, our graphs may substantiate that while there are two clusters, they may not necessarily by well grouped.

``` r
# Load a few extra libraries
library(mixtools)
```

    ## mixtools package, version 1.1.0, Released 2017-03-10
    ## This package is based upon work supported by the National Science Foundation under Grant No. SES-0518772.

``` r
library(plotGMM)
```

    ## Warning: package 'plotGMM' was built under R version 3.5.2

``` r
#GMMs
set.seed(7355) 
gmm_expend <- normalmixEM(clean_dset$expend, 
                    k = 2) 
```

    ## number of iterations= 15

``` r
gmm_salary <- normalmixEM(clean_dset$salary_real, 
                    k = 2) 
```

    ## number of iterations= 54

``` r
gmm_tlength <- normalmixEM(clean_dset$t_slength, 
                    k = 2) 
```

    ## number of iterations= 27

``` r
gmm_slength <- normalmixEM(clean_dset$slength, 
                    k = 2) 
```

    ## number of iterations= 86

``` r
gmm_expend$loglik
```

    ## [1] -35.89249

``` r
gmm_salary$loglik
```

    ## [1] -56.74103

``` r
gmm_tlength$loglik
```

    ## [1] -50.7358

``` r
gmm_slength$loglik
```

    ## [1] -54.41568

``` r
gmm_expend$sigma
```

    ## [1] 0.247148 1.550297

``` r
gmm_salary$sigma
```

    ## [1] 0.3423934 1.0131739

``` r
gmm_tlength$sigma
```

    ## [1] 0.3777076 1.1879633

``` r
gmm_slength$sigma
```

    ## [1] 0.4844234 1.3478919

``` r
gmm_expend$mu
```

    ## [1] -0.3356998  1.3208741

``` r
gmm_salary$mu
```

    ## [1] -0.5823506  0.8414661

``` r
gmm_tlength$mu
```

    ## [1] -0.4018569  1.3626499

``` r
gmm_slength$mu
```

    ## [1] -0.317918  1.443654

1.  Given the output for our GMMs for each variable, we see that the expenditure GMM has the highest log likelihood statistic (-35.89) out of the 4 GMMs, leading us to believe this model best captures the clusters. Additionally, for the expenditure GMM, the cluster 1 sigma statistic is the smallest amongst our 4 GMMs, indicating the smallest variation between distances within cluster 1, further substantiating the model's accuracy.

![](pset_1_files/figure-markdown_github/problem%207-1.png)![](pset_1_files/figure-markdown_github/problem%207-2.png)![](pset_1_files/figure-markdown_github/problem%207-3.png)![](pset_1_files/figure-markdown_github/problem%207-4.png)

    ## number of iterations= 31

    ## number of iterations= 263

    ## number of iterations= 130

    ## number of iterations= 195

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](pset_1_files/figure-markdown_github/problem%207-5.png)

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](pset_1_files/figure-markdown_github/problem%207-6.png)

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](pset_1_files/figure-markdown_github/problem%207-7.png)

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](pset_1_files/figure-markdown_github/problem%207-8.png)

1.  One high-level takeaway from these visualizations is that cluster one is much denser than cluster 2. In the dendrogram, we see many more states branching off of the first leg of the tree than the second, indicating a higher density. Similarly, in the GMM histograms, there is a higher density of observations in towards lower x values and in the kmeans scatterplots, there is a high density of points in the lower left corner of the graphs compared to the upper corner of the graph.

We can also make determinations on the number of clusters based on these visualizations. As stated above, we see for all three methods that cluster 1 has less intra-cluster variation than cluster 2. Some of this large intra-cluster variation in cluster 2 may be due to an outlier. When we graph the dendrogram with 3 clusters, California stands alone, which makes sense because its state government is extremely unique. Furthermore, we can see California standing alone in the histograms graphed for our four Gaussian Mixture models. Because California is an outlier, we see the second cluster (the blue line) is misfit, and has extreme variance in order to fit the outlier. Similarly, when comparing the scatterplots for the kmeans method, we see an outlier in the top right corner of graphs comparing regular session length vs total session length and salary vs expenditure, perhaps once again indicating California is an outlier. Thus, with California being an extreme outlier, two clusters may not be sufficient to capture the groupings of the data. Perhaps, we should cluster the data by k = 3. This is supported by the fact that the three clusters seen in our dendrogram make sense logically, given the special components in state legislatures of the smaller clusters.

``` r
# check internal validation
#total session length
library(mclust)
```

    ## Warning: package 'mclust' was built under R version 3.5.2

    ## Package 'mclust' version 5.4.5
    ## Type 'citation("mclust")' for citing this R package in publications.

    ## 
    ## Attaching package: 'mclust'

    ## The following object is masked from 'package:mixtools':
    ## 
    ##     dmvnorm

    ## The following object is masked from 'package:purrr':
    ## 
    ##     map

``` r
int <- as.matrix(clean_dset)

internal <- clValid(int, 2:10, 
                    clMethods = c("kmeans", "hierarchical","model"), 
                    validation = "internal"); summary(internal)
```

    ## 
    ## Clustering Methods:
    ##  kmeans hierarchical model 
    ## 
    ## Cluster sizes:
    ##  2 3 4 5 6 7 8 9 10 
    ## 
    ## Validation Measures:
    ##                                  2       3       4       5       6       7       8       9      10
    ##                                                                                                   
    ## kmeans       Connectivity   8.5683 11.0183 18.1651 20.1651 23.6810 25.8476 36.4726 44.8750 45.4024
    ##              Dunn           0.1726  0.2597  0.2456  0.2456  0.1214  0.1214  0.1871  0.1846  0.2515
    ##              Silhouette     0.6390  0.6054  0.4824  0.4611  0.3328  0.3210  0.3169  0.2854  0.3249
    ## hierarchical Connectivity   6.0869  6.9536 13.1345 15.1345 20.7563 22.9230 28.1726 30.1171 40.5512
    ##              Dunn           0.3598  0.4340  0.2902  0.2902  0.2836  0.2836  0.2451  0.2451  0.1930
    ##              Silhouette     0.6920  0.6619  0.5199  0.4989  0.3776  0.3658  0.2921  0.2831  0.2624
    ## model        Connectivity  18.7095 23.7964 33.3683 60.1651 69.0651 54.4433 51.8206 63.7619 62.1766
    ##              Dunn           0.0833  0.0855  0.0554  0.0280  0.0391  0.0532  0.0935  0.0879  0.0928
    ##              Silhouette     0.4230  0.3854  0.2157  0.0962  0.0473  0.1822  0.2957  0.2091  0.2132
    ## 
    ## Optimal Scores:
    ## 
    ##              Score  Method       Clusters
    ## Connectivity 6.0869 hierarchical 2       
    ## Dunn         0.4340 hierarchical 3       
    ## Silhouette   0.6920 hierarchical 2

``` r
#FOR CONNECTIVITY WANT SMALL VALUES
par(mfrow = c(2, 2))

plot(internal, legend = TRUE,
     type = "l",
     main = " ")

#test k=3
kmeans <- kmeans(y, 
                 centers = 3,
                 nstart = 15)
kmeans
```

    ## K-means clustering with 3 clusters of sizes 3, 7, 38
    ## 
    ## Cluster means:
    ##         AL       AK       AZ        AR       CA       CO       CT       DE
    ## 1 6.794833 5.679157 4.782580 6.9496629 3.647087 5.099267 5.753066 6.520385
    ## 2 3.032982 2.282659 1.625985 3.1121819 6.179227 1.518167 1.963880 2.664468
    ## 3 1.076155 1.547286 2.429294 0.9828675 7.732166 1.865143 1.118407 1.343459
    ##         FL       GA       HI       ID       IL        IN        IA
    ## 1 6.151739 7.025016 5.841277 6.177902 5.134079 6.0893104 6.1757274
    ## 2 3.026429 3.127192 2.123736 2.333953 1.668418 2.2576645 2.3127588
    ## 3 1.984383 1.027441 1.457359 1.062372 2.339179 0.9300614 0.9409285
    ##         KS        KY        LA        ME       MD       MA       MI
    ## 1 6.264206 6.4117840 6.3131933 6.9182082 5.848602 2.820684 4.316917
    ## 2 2.489303 2.6383935 2.5512592 3.0306934 2.100897 3.325459 1.666803
    ## 3 1.189644 0.9089807 0.9063789 0.9550996 1.302298 5.138784 3.212599
    ##          MN       MS       MO       MT       NE       NV       NH       NM
    ## 1 6.3461379 5.469706 5.831673 7.116887 5.969718 6.778116 6.513725 7.414532
    ## 2 2.4968569 1.973366 1.993751 3.256627 2.271709 3.226180 2.751444 3.596305
    ## 3 0.9872957 2.143706 1.244310 1.077515 1.107843 1.339769 1.348544 1.294722
    ##         NY       NC       ND       OH       OK        OR       PA
    ## 1 2.300681 5.624249 7.263326 4.045041 5.918110 6.2002591 3.900893
    ## 2 4.530379 1.990873 3.380349 1.567944 2.086043 2.4426110 2.284859
    ## 3 6.572378 1.440888 1.137741 3.177416 1.198336 0.9077794 3.870289
    ##          RI       SC       SD        TN       TX       UT       VT
    ## 1 6.4235461 5.805148 7.277562 6.6515845 6.528384 7.459889 7.039115
    ## 2 2.6005169 2.171256 3.383243 2.8136966 3.119541 3.590999 3.153897
    ## 3 0.8830922 1.307973 1.155821 0.8793935 1.486688 1.278107 1.065056
    ##          VA       WA        WV       WY
    ## 1 6.8603498 5.773203 6.7301981 7.842367
    ## 2 3.0336790 2.124743 2.8499489 3.964394
    ## 3 0.9778091 1.353849 0.9065859 1.623488
    ## 
    ## Clustering vector:
    ## AL AK AZ AR CA CO CT DE FL GA HI ID IL IN IA KS KY LA ME MD MA MI MN MS MO 
    ##  3  3  2  3  1  2  3  3  3  3  3  3  2  3  3  3  3  3  3  3  1  2  3  2  3 
    ## MT NE NV NH NM NY NC ND OH OK OR PA RI SC SD TN TX UT VT VA WA WV WY 
    ##  3  3  3  3  3  1  3  3  2  3  3  2  3  3  3  3  3  3  3  3  3  3  3 
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 218.5312 194.1290 496.2951
    ##  (between_SS / total_SS =  80.3 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"    
    ## [5] "tot.withinss" "betweenss"    "size"         "iter"        
    ## [9] "ifault"

``` r
kmeans$cluster
```

    ## AL AK AZ AR CA CO CT DE FL GA HI ID IL IN IA KS KY LA ME MD MA MI MN MS MO 
    ##  3  3  2  3  1  2  3  3  3  3  3  3  2  3  3  3  3  3  3  3  1  2  3  2  3 
    ## MT NE NV NH NM NY NC ND OH OK OR PA RI SC SD TN TX UT VT VA WA WV WY 
    ##  3  3  3  3  3  1  3  3  2  3  3  2  3  3  3  3  3  3  3  3  3  3  3

``` r
kmeans$centers
```

    ##         AL       AK       AZ        AR       CA       CO       CT       DE
    ## 1 6.794833 5.679157 4.782580 6.9496629 3.647087 5.099267 5.753066 6.520385
    ## 2 3.032982 2.282659 1.625985 3.1121819 6.179227 1.518167 1.963880 2.664468
    ## 3 1.076155 1.547286 2.429294 0.9828675 7.732166 1.865143 1.118407 1.343459
    ##         FL       GA       HI       ID       IL        IN        IA
    ## 1 6.151739 7.025016 5.841277 6.177902 5.134079 6.0893104 6.1757274
    ## 2 3.026429 3.127192 2.123736 2.333953 1.668418 2.2576645 2.3127588
    ## 3 1.984383 1.027441 1.457359 1.062372 2.339179 0.9300614 0.9409285
    ##         KS        KY        LA        ME       MD       MA       MI
    ## 1 6.264206 6.4117840 6.3131933 6.9182082 5.848602 2.820684 4.316917
    ## 2 2.489303 2.6383935 2.5512592 3.0306934 2.100897 3.325459 1.666803
    ## 3 1.189644 0.9089807 0.9063789 0.9550996 1.302298 5.138784 3.212599
    ##          MN       MS       MO       MT       NE       NV       NH       NM
    ## 1 6.3461379 5.469706 5.831673 7.116887 5.969718 6.778116 6.513725 7.414532
    ## 2 2.4968569 1.973366 1.993751 3.256627 2.271709 3.226180 2.751444 3.596305
    ## 3 0.9872957 2.143706 1.244310 1.077515 1.107843 1.339769 1.348544 1.294722
    ##         NY       NC       ND       OH       OK        OR       PA
    ## 1 2.300681 5.624249 7.263326 4.045041 5.918110 6.2002591 3.900893
    ## 2 4.530379 1.990873 3.380349 1.567944 2.086043 2.4426110 2.284859
    ## 3 6.572378 1.440888 1.137741 3.177416 1.198336 0.9077794 3.870289
    ##          RI       SC       SD        TN       TX       UT       VT
    ## 1 6.4235461 5.805148 7.277562 6.6515845 6.528384 7.459889 7.039115
    ## 2 2.6005169 2.171256 3.383243 2.8136966 3.119541 3.590999 3.153897
    ## 3 0.8830922 1.307973 1.155821 0.8793935 1.486688 1.278107 1.065056
    ##          VA       WA        WV       WY
    ## 1 6.8603498 5.773203 6.7301981 7.842367
    ## 2 3.0336790 2.124743 2.8499489 3.964394
    ## 3 0.9778091 1.353849 0.9065859 1.623488

``` r
kmeans$size
```

    ## [1]  3  7 38

![](pset_1_files/figure-markdown_github/problem%208-1.png)

9a. Given the fit on our internal validation graphs, we see that the Gaussian Mixture Model has the worst fit across connectivity, silhouette and dunn validation methods. The kmeans and hierarchical models have a similar fit visually, indicating they are more internally valid methods.

9b. Given this internal validation, the hierarchical approach is optimal, due to its optimal internal validation scores. The hierarchical approach had the lowest connectivity index, the highest silhouette index and the highest dunn index. Furthermore, the optimal value for k is 2, given that for both the silhouette and connectivity indices are optimal at k=2. Interestingly, the dunn index is optimal at k=3, indicating there may be a possibility that k=3 clustering may be a possibility.

9c. Perhaps, because of the discrepancy of optimal number of clusters between the dunn index versus the silhouette and connectivity indices, it may make sense to select a technically "sub-optimal" partitioning method. This is because each model has underlying assumptions that may not fit our data set. For example, the GMM assumes cluster shapes can be arbitrarily drawn, indicating it is used best when observations may be far apart in Euclidean space, but close probabilistically. The kmeans method, on the other hand, assumes circular clusters, and that each observation belongs to one cluster and one cluster only. The hierarchical structure assumes that there are underlying natural groupings in the data. In the case of our data, the hierarchical structure assumption may be too strong given that our initial ODI showed no natural patterns, thus indicating our data is dissimilar. Thus, perhaps kmeans, although technically "sub-optimal", may be the best for our data. There is further proof for this when re-running the kmeans method with 3 clusters (or k=3). Now, 80.3% of our data is well grouped (vs 66% for k=2), indicating 3 clusters under the kmeans method is a more accurate.
