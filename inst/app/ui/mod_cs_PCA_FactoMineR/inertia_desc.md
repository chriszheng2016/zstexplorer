The eigenvalues measure the amount of variation retained by each principal component. Eigenvalues are large for the first PCs and small for the subsequent PCs. That is, the first PCs corresponds to the directions with the maximum amount of variation in the data set.

Eigenvalues can be used to determine the number of principal components to
retain after PCA:

* An eigenvalue > 1 indicates that PCs account for more variance than
accounted by one of the original variables in standardized data. This
is commonly used as a cutoff point for which PCs are retained. This
holds true only when the data are standardized.

* You can also limit the number of component to that number that
accounts for a certain fraction of the total variance. For example, if
you are satisfied with 70% of the total variance explained then use the
number of components to achieve that.
