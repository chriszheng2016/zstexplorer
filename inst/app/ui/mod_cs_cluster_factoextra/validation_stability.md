**Stability measures**, a special version of internal measures, which evaluates the consistency of a clustering result by comparing it with the clusters obtained after each column is removed, one at a time.

  * The average proportion of non-overlap (APN)

  The APN measures the average proportion of observations not placed in the same cluster by clustering based on the full data and clustering based on the data with a single column removed.

  * The average distance (AD)

    The AD measures the average distance between observations placed in the same cluster under both cases (full data set and removal of one column).

  * The average distance between means (ADM)

    The ADM measures the average distance between cluster centers for observations placed in the same cluster under both cases.

  * The figure of merit (FOM)

    The FOM measures the average intra-cluster variance of the deleted column, where the clustering is based on the remaining (undeleted) columns.

  The APN, AD, and ADM are all based on the cross-classification table of the original clustering on the full data with the clustering based on the removal of one column.

  **Rule to judge:**

  The values of APN, ADM and FOM ranges from 0 to 1, with smaller value corresponding with highly consistent clustering results. AD has a value between 0 and infinity, and smaller values are also preferred.

