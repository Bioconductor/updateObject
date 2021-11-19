#!/bin/sh
#
# Run this script in batch mode with:
#
#   path/to/find_serialized_DataFrame_instances.sh >find_serialized_DataFrame_instances.log 2>&1 &
#

set -e  # exit immediately if a simple command returns a non-zero status

# Settings for 3.15 software packages on nebbiolo1:
MEAT0_DIR="/home/biocbuild/bbs-3.15-bioc/MEAT0"
Rscript="/home/biocbuild/bbs-3.15-bioc/R/bin/Rscript"

SKIPPED_PKGS="c('BaalChIP', 'BiGGR', 'ChIPpeakAnno', 'CytoTree', 'GeneAnswers', 'gwascat', 'isobar', 'mirIntegrator', 'oposSOM', 'PFP', 'ROntoTools', 'SLGI', 'SNPhood')"
FILTER="\\\\bDataFrame\\\\b"

R_EXPR="library(updateObject)"
R_EXPR="$R_EXPR;setwd('$MEAT0_DIR')"
R_EXPR="$R_EXPR;all_pkgs <- dir()"
R_EXPR="$R_EXPR;length(all_pkgs)"
R_EXPR="$R_EXPR;SKIPPED_PKGS <- $SKIPPED_PKGS"
R_EXPR="$R_EXPR;filter <- '$FILTER'"
R_EXPR="$R_EXPR;system.time(codes <- updateAllPackageObjects(all_pkgs, skipped_pkgs=SKIPPED_PKGS, filter=filter, dry.run=TRUE))"
R_EXPR="$R_EXPR;sessionInfo()"
R_EXPR="$R_EXPR;gc()"
R_EXPR="$R_EXPR;table(codes)"
R_EXPR="$R_EXPR;sum(codes > 0)"
R_EXPR="$R_EXPR;sum(codes > 0) / length(codes)"

$Rscript -e "$R_EXPR"

echo "DONE."
