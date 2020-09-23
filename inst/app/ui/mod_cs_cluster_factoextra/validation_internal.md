**Internal measures**, which uses intrinsic information in the data
to assess the quality of the clustering.

  * Silhouette coefficient

    The silhouette measures how well an observation is clustered and
    it estimates the average distance between clusters.

  **Rule to judge:** Silhouette coefficient should be maximized
  * Dunn index

  The Dunn Index is the ratio of the smallest distance between
  observations not in the same cluster to the largest intra-cluster
  distance.

  **Rule to judge:** The Dunn Index has a value between zero and
  infinity, and should be maximized

  * Connectivity

    The connectivity indicates the degree of connectedness of the
    clusters, as determined by the k-nearest neighbors.

  It corresponds to what extent items are placed in the same
  cluster as their nearest neighbors in the data space.

  **Rule to judge:** The connectivity has a value between 0 and
  infinity and should be minimized.

