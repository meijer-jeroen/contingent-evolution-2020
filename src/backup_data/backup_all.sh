#!/bin/sh
# Makes a copy of the Virtual Microbe dataset, retaining only the data used in analysis and figures.

# data needed for SI 1
for dir in ../../../emergent-ecologies/CF_fixDI/CF*

do
  SOURCEDIR=$dir/
  TARGETDIR="../../data/raw/main_experiment/$(basename $dir)"
  echo source $SOURCEDIR
  echo target $TARGETDIR

  # copy dataset
  rsync -arv --include-from=minimal_data.txt --exclude='*' --recursive  $SOURCEDIR  $TARGETDIR

done



# data for Figure 2 lineage markers and grid
for dir in ../../../emergent-ecologies/CF_fixDI/CF_fixDI_freenon_evo202 ../../../emergent-ecologies/CF_fixDI/CF_fixDI_freenon_evo404 ../../../emergent-ecologies/CF_fixDI/CF_fixDI_freenon_evo901

do
  #echo $dir
  SOURCEDIR=$dir/
  TARGETDIR="../../data/raw/main_experiment/$(basename $dir)"
  echo source $SOURCEDIR
  echo target $TARGETDIR

  # copy dataset
  rsync -arv --include-from=extended_data_fig2.txt --exclude='*' --recursive  $SOURCEDIR  $TARGETDIR

 #prune large grid files down to temporal resolution of 5000 time steps
 #gridprune2 100

 # remove plot/grid views/metabolite files. Couldn't figure out how to not expand into these with rsync --include-from

 # make a tarball
done



# data for Figure 3 Lineage removal
for dir in ../../../emergent-ecologies/remove_lineage_CF_fixDI/*
do
  SOURCEDIR=$dir/
  TARGETDIR="../../data/raw/remove_lineage/$(basename $dir)"
  echo source $SOURCEDIR
  echo target $TARGETDIR

  rsync -arv --include-from=data_remove_lineage.txt --exclude='*' --recursive $SOURCEDIR $TARGETDIR
done



# data for Supplement Transcription costs
for dir in ../../../emergent-ecologies/trans_costs/CF*
do
  # echo $dir
  SOURCEDIR=$dir/
  TARGETDIR="../../data/raw/trans_costs/$(basename $dir)"
  echo source $SOURCEDIR
  echo target $TARGETDIR

  #copy dataset
  rsync -arv --include-from=minimal_data.txt --exclude='*' --recursive $SOURCEDIR $TARGETDIR

done

