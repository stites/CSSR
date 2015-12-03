#!/usr/bin/perl

# Program which produces a long data-file consisting of a long repetition
# of the same symbol, unspaced.
# To be used in conjunction with Kris's state-inference program.

# First argument is the symbol, the second argument is the number of
# repetitions
# Output is to std-out, use redirects to put in files.
# Note that if the output "symbol" is itself a string, it will produce copies
# of that template.

$output_symbol = shift(@ARGV); # Take first argument, make it output symbol
$repetitions = shift(@ARGV); # Number of times to spit it out

for ($i = 1; $i <= $repetitions; $i++) {
    print "$output_symbol";
}
print "\n";
