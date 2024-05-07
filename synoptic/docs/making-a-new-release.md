# Steps to make a new data release

1. Make sure you're up-to-date with the latest version of `main`.

2. Add new raw files from the Dropbox folder(s) into
`./synoptic/data/Raw/`. Right now, this should include files from both
the TEMPEST and synoptic Dropbox shares. Basically, bring your raw data
files up to date.

3. IMPORTANT NOTE: some of the raw data files have bad timestamps
(usually, from when a datalogger is first installed) and I have edited
those out by hand. So you do NOT want to start with the entire Dropbox
archive folder, but rather from the raw files of the previous release.
These are archived with each release.

4. Set the release version in `driver.R` (or in the individual Quarto
files; see below).

5. Make sure there's a README for your release number in
`./synoptic/metadata/L1_metadata/`. The L1 step will error if a file
named `README_vXXX.txt` doesn't exist there, where "XXX" is the version
number you set in step 4. Make sure that the citation and changelog
sections of this document are up to date.

6. Occasionally, check with site PIs to see if the site-specific contact
information should be updated, and if so, update the various site files
in `./synoptic/metadata/L1_metadata`.

7. From the `./synoptic` folder, run `reset("data/")` (this function
should be sourced from `helpers.R`). This will clean out any previous
files.

8. Run the processing pipeline. If you use `driver.R` it will be
relatively fast, because highly parallelized, but you don't get
informative html logs, because the parallel processes can't write to
output. If you want the full, detailed logs, run without parallelism
(either by rendering the Quarto files one by one, or changing the driver
script). Of course, this is much slower.

9. Double-check the final release README file.

10. You may want to clean up the resulting L1 folder; for example,
remove (`find ./ -name ".DS_Store" | xargs rm`) hidden files created by MacOS.

11. Push the data to the COMPASS HPC. For example:

```
rsync -av --exclude=".*" L1/ <user>@compass.pnl.gov:/compass/datasets/fme_data_release/sensor_data/Level1/v1-0/
```

(Follow the same procedure for `Raw`, `L0`, and `Logs` outputs.)

12. Upload to the Google Drive, renaming the folder to the correct
version number, again for `L1`, `Raw`, `L0`, and `Logs`.

13. Let everyone know!
