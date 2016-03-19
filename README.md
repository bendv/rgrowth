# rgrowth
Post-disturbance regrowth monitoring using Landsat Time Series (LTS).

`bfastSpatial` is required for `rgrowth`. Both can be installed using `devtools`:

```r
library(devtools)
install_github('dutri001/bfastSpatial')
install_github('bendv/rgrowth')
library(rgrowth)
```

If you use `rgrowth`, please cite the following paper:

DeVries, B., M. Decuyper, J. Verbesselt, A. Zeileis, M. Herold and S. Joseph. 2015. Tracking disturbance-regrowth dynamics in the tropics using structural change detection and Landsat time series. Remote Sensing of Environment 169:320-334. DOI: [10.1016/j.rse.2015.08.020](http://doi.org/10.1016/j.rse.2015.08.020).

A short tutorial is available [here](http://bendv.github.io/rgrowth). At the moment, the tutorial only covers single-pixel time series. Check back later for updates to the tutorial, including a mapping component!
