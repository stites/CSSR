#!/usr/bin/perl

# Calculate either the likelihood or the (natural) log-likelihood of a time
# series under a causal state model

##### ARGUMENTS/USAGE in this order:
# -log [optional; if present, returns natural log of the likelihood]
# <machine name base> [required; name of file run through CSSR to get machine]
# <data file> [required; name of file containing time series]

# Read in a causal state model in the format produced by CSSR
# Read in a data string (additional file), possibly with multiple lines

### Mechanics:
# Needs an initial state distribution.  Currently taken from the machine.
# We also have an array of "current state, for each possible starting state",
# which initially is just the identity.
# At each time step, for each j from 0 to num_states-1, look up
#  a. probability of emitting the next symbol, given the state is s[j]
#  b. the state transitioned to, given the state is s[j]
# Multiply the running likelihoods by the probabilities, update the states
# At the end, sum all the running likelihoods and output

# Changes to make:
## Better format of output numbers!!!
## Add options re initial states:
##### fixed initial state
##### arbitrary distribution vector
## Improve the handling of the dwindling number of states over time.
#### Can we keep track only of the POSSIBLE current states, so as not to re-
#### compute many probabilities?
## Allow time series to be redirected from standard input?
## Use the Perl6::Slurp package to simplify getting stuff from machine?


# We need high precision floating-point numbers, and there's a package to
# do just that.
require Math::BigFloat;
use Math::BigFloat qw(:constant);
Math::BigFloat->accuracy(40);


$state_probs_from_machine = 1;

$describe_machine = 0; # For debugging
$debug=0;

$usage = "<file CSSR was run on to get machine> <file with new time series>";
die "Usage: $0 [-log] $usage\n" unless ((@ARGV==2) || ((@ARGV==3) && ($ARGV[0] eq "-log")));
if (@ARGV==3) {
  shift(@ARGV);
  $log = 1;
}
$machine = shift(@ARGV);
$datafile = shift(@ARGV);

# Read in machine
# Taken from generate-string-from-CSM.pl
# In turn taken from ~/bin/test-machines/inferred-versus-true
# but modified to handle larger alphabet using hash-of-hashes

# Open the _results file for reading
$ResultsFileName = $machine . "_results";
open (RESULTS, $ResultsFileName) || die "$0: Couldn't open $ResultsFileName: $!\n";
# Declare an object of the BigFloat type for probability calculations, which
# we'll recycle, rather than repeatedly declaring big fancy things
$prob = new Math::BigFloat;
while (<RESULTS>) {
  # Get a line
  # Remember that by default the input line is read into $_, which is also
  # the default operand for chomp and split
  chomp;
  @ResultsLine = split;
  # Default splitting on whitespace
  # If the first word on the line is "State" then we're getting a state
  # number
  # i.e. the line looks like "State number: X"
  if ($ResultsLine[0] eq "State") {
    $state = $ResultsLine[2];
    push(@StatesList,$state);
  } elsif ($ResultsLine[0] eq "distribution:") {
    # These lines look like "distribution: P(A)=X  P(B)=Y", where X is prob of A, etc.
    while (@ResultsLine) {
      $word = shift(@ResultsLine);
      if ($word =~ m/P\((\w)\)/) {
	# then we know the symbol whose probability we're getting at the next
	# number
	$symbol = $1;
      }
      # We omit zero probability events for concision
      if (($word eq "1") || ($word =~ m/0\.\d+/)) {
	$prob = $word;
	$EmitProb{$state}{$symbol} = $prob;
      }
    }
  } elsif ($ResultsLine[0] eq "transitions:") {
    # These lines look like "transitions: T(A)=M T(B)=N", etc.
    # There is probably a more elegant way of doing this, using global
    # regexp matching on $_ or something, but it suffices for the present
    while (@ResultsLine) {
      $word = shift(@ResultsLine);
      if ($word =~ m/T\((\w)\)/) {
	# then we know the symbol whose trnaisition we're getting at the
	# next number
	$symbol = $1;
      }
      # Numbers by themselves on these lines give the transitioned-to states
      if ($word =~ m/\d+/) {
	$new_state = $word;
	$Transition{$state}{$symbol} = $new_state;
      }
    }
  } elsif ($ResultsLine[0] eq "P(state):") {
    $prob = $ResultsLine[1];
    $StateProb{$state} = $prob;
  }
}
# Continue scanning until file is ended
close(RESULTS) || die "$0: Can't close $ResultsFileName: $!\n";

if ($describe_machine) { 
  foreach $state (@StatesList) {
    print "State $state: probability $StateProb{$state}\n";
    foreach $symbol (keys %{ $EmitProb{$state} } ) {
      print "\t Pr($symbol) = $EmitProb{$state}{$symbol} goes to $Transition{$state}{$symbol}\n"
    }
  }
}

# Set initial state probabilities
# Currently only the default, of taking them from the machine, is implemented
# Also, let's set up the BigFloats we'll use to keep track of conditional
# likelihoods
if ($state_probs_from_machine > 0) {
  foreach $state (keys(%StateProb) ) {
    $initial_state_prob[$state] = $StateProb{$state};
    $cond_like[$state] = new Math::BigFloat(1.0);
  }
}
$num_states = keys(%StateProb);
print "Number of states is $num_states\n" if ($debug);


open(IN, "$datafile") || die "$0: Can't open $datafile for reading: $!\n";
while (<IN>) {
  chomp;
  $t = length($_);
  @time_series = unpack("A1" x $t, $_); # Per Perl Cookbook Recipe 1.1
  for ($j = 0; $j < $num_states; $j++) {
    $cond_state[$j] = $j;
    $cond_like[$j]->bone(); # Reset the conditional likelihood to 1.0
    print "Set $j th starting state to be $j\n" if ($debug);
  }
  for ($i=0; $i < $t; $i++) {
    $symbol = $time_series[$i];
    print "Symbol is $symbol\n" if ($debug);
    for ($j=0; $j < $num_states; $j++) {
      # Ignore paths through the machine which have died out because the initial
      # state was incompatible with the eventual sequence
      # It seems to me that we should be able to not look up the same cond.
      # probabilities via array access for all the starting states once the
      # process has synchronized.  Maybe do some structure which keeps the
      # list of possible states?
      # Or perhaps a hash.
      # cond_state{$j} = possible states at current time given start
      # in state $j
      # then loop over values(%cond_state) to get emission probability,
      # successor
      if ($cond_state[$j] != -1) {
	$prob = $EmitProb{$cond_state[$j]}{$symbol};
	print "\t Starting from state $j, cond prob is $emit_prob\n"
	  if ($debug);
	# Check whether we've hit an impossibility, given our guessed start
	if ($prob > 0) {
	  $new_state = $Transition{$cond_state[$j]}{$symbol};
	  print "\t\t and new state is $new_state\n" if ($debug>0);
	} else {
	  $new_state = -1;
	}
	# Invoke our super-accurate multiplication, to the default precision
	$cond_like[$j]->bmul($prob);
	print "\t\t finally the likelihood to data is $cond_like[$j]\n"
	  if ($debug);
	$cond_state[$j] = $new_state;
      }
    }
  }
  # Should check here that all the conditional states have become the same,
  # otherwise not synchronized yet and issue a warning (not a die)
  # Doesn't do this yet
  $total_like = 0.0;
  $killed_starting_states = 0;
  for ($j=0;$j<$num_states;$j++) {
    print "Likelihood conditional on starting in state $j is $cond_like[$j]\n"
      if ($debug);
    $total_like += $cond_like[$j] * $initial_state_prob[$j];
    $killed_starting_states++ if ($cond_state[$j] == -1);
  }
  $readable_total_like = $total_like->bsstr(); # Scientific notation
  if ($log) {
    $total_like->blog(); # i.e. take natural log
    $readable_total_like = $total_like->bstr(); # Less scientific notation
  }
  print "$readable_total_like";
  print "  ALL initial states impossible"
    if ($killed_starting_states == $num_states);
  print "\n";
}
close(IN) || die "$0: Can't close $datafile: $!\n";
