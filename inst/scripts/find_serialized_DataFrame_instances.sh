#!/bin/sh
#
# Run this script in batch mode with:
#
#   path/to/find_serialized_DataFrame_instances.sh >find_serialized_DataFrame_instances.log 2>&1 &
#
# Once the script has completed, extract the list of packages with old
# DataFrame instances with:
#
#   grep '^File .* [1-9][0-9]*$' find_serialized_DataFrame_instances.log | \
#     sed 's/^File //' | cut -d '/' -f 1 | sort | uniq
#
# This script was successfully run on nebbiolo1 (BioC 3.15, 2076 packages in
# manifest) on Jan 13, 2022:
# - log file: find_serialized_DataFrame_instances-20220113.log (located in
#   this folder)
# - 2076 input packages (2064 processed, 12 skipped)
# - took about 25 min
# - attached or loaded 1191 packages (325 attached and 866 loaded) as
#   reported by sessionInfo()
# - required about 8.5 Gb of RAM
# - found 24 packages with old DataFrame instances (run the above grep
#   command on the log file to extract the list)

set -e  # exit immediately if a simple command returns a non-zero status

# Settings for 3.15 software packages on nebbiolo1:
MEAT0_DIR="/home/biocbuild/bbs-3.15-bioc/MEAT0"
Rscript="/home/biocbuild/bbs-3.15-bioc/R/bin/Rscript"

SKIPPED_PKGS="c('BaalChIP', 'BiGGR', 'ChIPpeakAnno', 'CytoTree', 'gwascat', 'isobar', 'mirIntegrator', 'oposSOM', 'PFP', 'ROntoTools', 'SLGI', 'SNPhood')"
FILTER="\\\\bDataFrame\\\\b"
CODES_DEST_FILE="find_serialized_DataFrame_instances_codes.rds"

R_EXPR="library(updateObject)"
R_EXPR="$R_EXPR;setwd('$MEAT0_DIR')"
R_EXPR="$R_EXPR;all_pkgs <- dir()"
R_EXPR="$R_EXPR;length(all_pkgs)"
R_EXPR="$R_EXPR;SKIPPED_PKGS <- $SKIPPED_PKGS"
R_EXPR="$R_EXPR;filter <- '$FILTER'"
R_EXPR="$R_EXPR;system.time(codes <- updateAllPackageObjects(all_pkgs, skipped_pkgs=SKIPPED_PKGS, filter=filter, dry.run=TRUE))"
R_EXPR="$R_EXPR;saveRDS(codes, file='$CODES_DEST_FILE')"
R_EXPR="$R_EXPR;sessionInfo()"
R_EXPR="$R_EXPR;gc()"
R_EXPR="$R_EXPR;table(codes)"
R_EXPR="$R_EXPR;sum(codes > 0)"
R_EXPR="$R_EXPR;sum(codes > 0) / length(codes)"

$Rscript -e "$R_EXPR"

echo "DONE."
