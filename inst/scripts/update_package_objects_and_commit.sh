#!/bin/bash
#
# Usage:
#   path/to/update_package_objects_and_commit.sh [--push] [branch] [repo_path]
#

#-------------------------- User-controlled settings --------------------------

export commit_msg="Pass serialized S4 instances thru updateObject()"
export new_date="2021-11-20"

filter="\\\\bDataFrame\\\\b"

# Settings for 3.15 packages on nebbiolo1:
Rscript="/home/biocbuild/bbs-3.15-bioc/R/bin/Rscript"
BBS_HOME="/home/biocbuild/BBS"

# -----------------------------------------------------------------------------

python_script1="$BBS_HOME/utils/clone_or_pull_repo.py"
python_script2="$BBS_HOME/utils/small_version_bumps.py"

print_usage()
{
	cat <<-EOD
	Usage:
	    $0 [--push] [branch] [repo_path]
	EOD
	exit 1
}

if [ "$1" == "--push" ]; then
	branch="$2"
	repo_path="$3"
else
	branch="$1"
	repo_path="$2"
fi

if [ "$branch" == "" ] || [ "$repo_path" == "" ]; then
	print_usage
fi

/usr/bin/python3 "$python_script1" --branch "$branch" "$repo_path"

echo ""

R_EXPR="suppressPackageStartupMessages(library(updateObject))"
R_EXPR="$R_EXPR;filter <- '$filter'"
R_EXPR="$R_EXPR;code <- updatePackageObjects('$repo_path', filter=filter)"
R_EXPR="$R_EXPR;exit_status <- if (code > 0) 0L else 2L"
R_EXPR="$R_EXPR;quit(save='no', status=exit_status)"
$Rscript --vanilla -e "$R_EXPR"
exit_status="$?"

echo ""

if [ "$exit_status" == "1" ]; then
	echo "Rscript returned an error! (see above)"
	exit 1
fi

if [ "$exit_status" == "2" ]; then
	echo "NOTHING TO UPDATE."
	exit 2
fi

if [ "$1" == "--push" ]; then
	/usr/bin/python3 "$python_script2" --push "$branch" "$repo_path"
else
	/usr/bin/python3 "$python_script2" "$branch" "$repo_path"
fi

echo "DONE."
