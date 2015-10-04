#!/usr/bin/perl

# Script to run CSSR with BIC length selection on multiple files

# invokes bic-selector.pl, which in turn invokes the likelihood-under-CSM.pl
# script, and, of course, CSSR

# Designed for use with fMRI data with x, y, z coordinates
# Name format assumed to be
# [base]_x[X]_y[Y]_z[Z].[extension]



# Arguments:
# min x, max x, min y, max y, min z, max z, alphabet file

use strict; # To avoid bugs due to mis-spelling variable, require declarations
use Perl6::Slurp; # To read whole files into strings

my $debug=0;

# Print usage message if the # of parameters is wrong
my $usage = "<name base> <extension> <alphabet file> <min x> <max x> <min y> <max y> <min z> <max z>";
die "Usage: $0 $usage\n" unless (@ARGV==9);

my $namebase = shift(@ARGV);
my $extension = shift(@ARGV);
my $alphabetfile = shift(@ARGV);
my $min_x = shift(@ARGV);
my $max_x = shift(@ARGV);
my $min_y = shift(@ARGV);
my $max_y = shift(@ARGV);
my $min_z = shift(@ARGV);
my $max_z = shift(@ARGV);

my $bic_command = "bic-selector.pl";

for (my $x = $min_x; $x <= $max_x; $x++) {
    for (my $y = $min_y; $y <= $max_y; $y++) {
	for (my $z = $min_z; $z <= $max_z; $z++) {
	    my $datafile = $namebase . "_" . "x$x" . "_" . "y$y" . "_" . "z$z" . ".$extension";
	    system("$bic_command $alphabetfile $datafile");
	}
    }
}
