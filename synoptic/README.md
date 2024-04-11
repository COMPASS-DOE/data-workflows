

From within `synoptic/data`, to mirror files up to COMPASS HPC:
```
rsync -av --exclude=".*" L1 <user>@compass.pnl.gov:/compass/datasets/fme_data_release/sensor_data/Level1/
```
