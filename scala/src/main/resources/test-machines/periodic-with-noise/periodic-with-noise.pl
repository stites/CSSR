#!/opt/gnu/bin/perl

# Program which produces a long data-file consisting of a long repetition
# of the same symbol, unspaced, with bits flipped with a fixed probability.
# To be used in conjunction with Kris's state-inference program.

# First argument is the template, the second is the probability of error, the
# third argument is the number of repetitions of the template
# Output is to std-out, use redirects to put in files.
# Assumes that the template is in the 0,1 alphabet.
# Note that if the output "symbol" is itself a string, it will produce copies
# of that template.

$output_symbol = shift(@ARGV); # Take first argument, make it output symbol
$noise_level = shift(@ARGV); # Second argument is probability of error
$repetitions = shift(@ARGV); # Number of times to spit it out

$template_length = length($output_symbol);

print "input template has length $template_length\n";

for ($i = 1; $i <= $repetitions; $i++) {
    for ($j = 0; $j < $template_length; $j++) {
	   if ((rand) > $noise-level) {
		  $k = $output_symbol[$j];
	   }
	   else {
		  $k = my_not($output_symbol[$j]);
	   }
	   print "$k";
    }
}

print "\n";


sub my_not {
    if ($_[0] = 0) {
	   return(1);
	   print "I am returning a 1! \n";
    } else {
	   return(0);
	   print "I am returning a 0! \n";
    }
}
