#!/usr/bin/perl
# Does casual-state reconstruction on multiple single-line data files named
# things like [tag]_[number]
# Four arguments:
# first, the tag at the start of the file name
# second, the starting number for the files (inclusive)
# third, the ending number for the files (inclusive)
# fourth, the history-length to pass to the reconstruction program

$reconstructor = "CSSR"; # executable name for the reconstruction program
$alpha = "alpha"; # Name of the alphabet file, in the same directory as the
# data files

die "Usage: $0 tag start_number end_number history\n" unless
    (@ARGV == 4);

$tag = shift(@ARGV); # Common beginning of data file names
$start_number = shift(@ARGV); # Number of the file to begin on
$end_number = shift(@ARGV); # Number of the last file to run
$history = shift(@ARGV); # Length of history to examine

for ($i = $start_number; $i < $end_number+1; $i++) {
    $filename = $tag . "_" . $i;
    print "$reconstructor $alpha $filename $history\n";
    system("$reconstructor $alpha $filename $history");
}

