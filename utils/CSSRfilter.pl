#!/usr/bin/perl

# Turns CSSR into a STDIN to STDOUT filter

# Command line gives the size of the alphabet, which is presumed to run from 0
# through the integers (up to at most 9 of course)
# Next argument should be the size level to use in the hypothesis test
# Each line of STDIN is a _separate_ time series (over the common alphabet) and
# is processed separately by CSSR
# The resulting state sequences are written to STDOUT
# Output: Each line begins with the total number of states for that reconstructed machine, followed by a space, and then the sequence of visited states, identified by numbers and separated by spaces; every line ends in a newline character
# The buffer is flushed after every line
# Currently all of this is done through writing and reading files to /tmp/, but
# ultimately CSSR's source should be re-written to make it work in filter mode

use Perl6::Slurp; # To read whole files into strings
use File::Path; # To delete directories

# Defaults
$tmpbase = "/tmp";
$alphabetname = "alpha";
$timename = join("_",localtime());
  # This is the current time; inserted into temporary file names so that
  # multiple copies of this could be run in the same directory without
  # interference (unless launched within the same second!)
$tsname = "ts";
$bic_selector = "bic-selector.pl";
$verbose = 0;
$debug = 0;

### Do the settings processing
# Temporarily: command line, fixed order
unless (@ARGV==2) { die "I need two arguments, bub\n"; }
$alphasize = shift(@ARGV); 
$siglevel = shift(@ARGV);


# Write alphabet file to $tmpbase/$alphabetname or die trying
$alpha_string = "";
for ($i=0;$i<$alphasize;$i++) {
    $alpha_string .= $i;
}
$the_alphabet = "$tmpbase/$alphabetname.$timename";
open(ALPHABET,">",$the_alphabet) ||
    die "Can't open $the_alphabet: $!";
print ALPHABET "$alpha_string\n";
close(ALPHABET) ||
    die "Couldn't close $the_alphabet: $!";
print "Wrote alphabet $alpha_string to $the_alphabet\n" if $verbose;

# To deal with buffering, need to make STDOUT a "hot" handle
  # See http://perl.plover.com/FAQs/Buffering.html
#my $ofh = select STDOUT;
#$| = 1;
#select $ofh;


# Loop over STDIN
$counter=1;
while(<STDIN>) {
    chomp;
    $the_time_series = "$tmpbase/$tsname.$timename.$counter";
    open(TSFILE,">",$the_time_series) ||
	die ("Could not open $the_time_series: $!");
    print TSFILE "$_";
    close(TSFILE) || die ("Could not close $the_time_series_file: $!");
    # System call to run bic-selector with appropriate alphabet, data file, etc.
    print "CSSR says: $_ written to $the_time_series\n" if $verbose;
    system("$bic_selector --s $siglevel $the_alphabet $the_time_series");
    # Print state sequence of the selected model to STDOUT
    # First, get the state series
    $the_state_series = $the_time_series . "_BIC_picked_state_series";
    $state_series = slurp($the_state_series);
    $state_series =~ s/;/ /g;
    # Need to also get the number of states
    $selected_info_file = $the_time_series . "_BIC_picked_info";
    $selected_info = slurp($selected_info_file);
    if ($selected_info =~ m/Number of Inferred States:\s*(\d+)/) {
      $num_states = $1;
    } else {
      die "Can't get the number of states from string matching in $selected_info_file\n";
    }
    print "$num_states $state_series\n";
    # clean up the time series file now (unless $debug)
    foreach $file (glob("$the_time_series*")) {
	if ($file =~ m/unselected/) {
	    print "There is a directory called $file\n" if $verbose;
	    File::Path::remove_tree $file unless $debug;
	    # [[Test for success here!]]
	} else {
	    print "There is a file called $file\n" if $verbose;
	    print "It must die\n" if $verbose;
	    unless ($debug) {
		unlink $file || die("$file defies my might and I perish: $!");
	    }
	}
    }
    $counter++;
}


# final file clean-up (unless $debug)
print "Fat lady singing here, boss\n" if $verbose;
unlink $the_alphabet || die("Could not delete $the_alphabet: $!");
print "She's sung; encore?\n" if $verbose;
close(STDOUT); # Terminate output
