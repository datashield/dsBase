# Calculates the coordinates of the centroid of each n nearest neighbours

This function calculates the coordinates of the centroids for each n
nearest neighbours.

## Usage

``` r
heatmapPlotDS(x, y, k, noise, method.indicator)
```

## Arguments

- x:

  the name of a numeric vector, the x-variable.

- y:

  the name of a numeric vector, the y-variable.

- k:

  the number of the nearest neighbours for which their centroid is
  calculated if the `method.indicator` is equal to 1 (i.e. deterministic
  method).

- noise:

  the percentage of the initial variance that is used as the variance of
  the embedded noise if the `method.indicator` is equal to 2 (i.e.
  probabilistic method).

- method.indicator:

  a number equal to either 1 or 2. If the value is equal to 1 then the
  'deterministic' method is used. If the value is set to 2 the
  'probabilistic' method is used.

## Value

a list with the x and y coordinates of the centroids if the
deterministic method is used or the x and y coordinated of the noisy
data if the probabilistic method is used.

## Details

The function finds the n-1 nearest neighbours of each data point in a
2-dimensional space. The nearest neighbours are the data points with the
minimum Euclidean distances from the point of interest. Each point of
interest and its n-1 nearest neighbours are then used for the
calculation of the coordinates of the centroid of those n points.
Centroid here is referred to the centre of mass, i.e. the x-coordinate
of the centroid is the average value of the x-coordinates of the n
nearest neighbours and the y-coordinate of the centroid is the average
of the y-coordinates of the n nearest neighbours. The coordinates of the
centroids return to the client side function and can be used for the
plot of non-disclosive graphs (e.g. scatter plots, heatmap plots,
contour plots, etc).

## Author

Demetris Avraam for DataSHIELD Development Team
