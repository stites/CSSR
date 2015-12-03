#!/usr/bin/perl

# Program which produces a long data-file consisting of a IID tosses of
# a biased coin, bias set by input.
# The output alphabet is 0, 1.
# The output symbols are unspaced.
# To be used in conjunction with Kris's state-inference program.

# First argument is the probability of a 1, the second argument is the number
# of repetitions
# Output is to std-out, use redirects to put in files.

srand; # Initialize random number generator

$success_rate = shift(@ARGV); # Take first argument, make it success-rate
$repetitions = shift(@ARGV); # Number of times to spit it out

for ($i = 1; $i <= $repetitions; $i++) {
    if ((rand) <= $success_rate) {
	   print "1";
    }
    else {
	   print "0";
    }
}
print "\n";
