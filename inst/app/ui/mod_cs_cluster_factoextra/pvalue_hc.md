Clusters can be found in a data set by chance due to clustering noise or 
sampling error. By conducting multiscale bootstrap resampling, we could find hierarchical 
clustering above specified P-Values.

**Algorithm:**

1. Generated thousands of bootstrap samples by randomly sampling elements of 
the data.

2. Compute hierarchical clustering on each bootstrap copy

3. For each cluster:

  * compute the **bootstrap probability (BP) value** which corresponds to the 
  frequency that the cluster is identified in bootstrap copies.

  * Compute the **approximately unbiased (AU) probability values** (p-values) by
  multiscale bootstrap resampling.

  
**Notice on Cluster dendrogram with AU/BP values:**

  Values on the dendrogram are AU p-values (Red, left), BP values (green, right),
and clusterlabels (grey, bottom). 

**Rule to judge:**

Clusters with AU > = 95% are indicated by the rectangles and are considered to be strongly supported by data.
