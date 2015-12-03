#!/usr/bin/perl

# Program which produces a time series simulating the "flip" transducer
# of Holmes and Isbell ("Looping Suffix Tree-Based Inference of Partially
# Observable Hidden State", ICML 2006)
# They present the model as a transducer with two hidden states, three
# inputs or actions, "u", "r" and "l", and two outputs or observations,
# "0" and "1".  (They do not name their states.)  In the first state,
# the inputs l and u loop back, and produce a 0, but the action r goes
# to the other state and produces a 1.  In that state, r and u loop
# back, emitting 0s, and l switches states, producing a 1.
# They claim to run CSSR on this, with the "actions [i.e., inputs] chosen
# at random".  If one did that and just observed the outputs, both
# states would produce 0 with probability 2/3, 1 with probability 1/3,
# and this would just be a biased coin, so their simulations would make
# no sense at all.  I think they must give it input-output pairs to
# predict.  Then we have a five-symbol alphabet, "l0", "l1", "r0", "r1"
# and "u0", with probabilities and transitions as follows:
# In state A:
### "l0" -> A, prob. 1/3
### "l1" forbidden
### "r0" forbidden
### "r1" -> B, prob. 1/3
### "u0" -> a, prob. 1/3
# In state B:
### "l0" forbidden
### "l1" -> A prob. 1/3
### "r0" -> B prob. 1/3
### "r1"  forbidden
### "u0" -> B prob. 1/3
# To accomodate CSSR, I will use a five-letter alphabet, where lower-case
# letters are accompanied by 0s, upper-case by 1s, i.e.
# "l0" <-> "l", "l1" <-> "L", "r0" <-> "r", "r1" <-> "R", "u0" <-> "u"
# Thus, the suffixes l, L, r and R are all resolving, and only u* is
# ambiguous.
# We pick an initial state equiprobably.

# The first input is the number of steps to simulate.  That is the only
# input.
# The output symbols are unspaced.
# Output is to files named TMD_foo.
# To be used in conjunction with Kris's state-inference program.


srand; # Initialize random number generator
$number_states = 2; # Three states
$FileNamesBase = "flip";

$repetitions = shift(@ARGV); # Make first argument number of steps to simulate

# Pick a state at random
$r = rand;
if ($r <= 0.5) {$current_state = "A";} else {$current_state = "B";};

for ($i = 1; $i <= $repetitions; $i++) {
    ($observation, $next_state) = make_a_move($current_state);
    # $observation = make_a_measurement($next_state,$current_state);
    $observed_sequence .= $observation;
    # print "$observation";
    $current_state = $next_state;
    $state_sequence .= $current_state;
}

$TimeSeriesFileName = $FileNamesBase . "_timeseq";
open(TIMESERIES, ">$TimeSeriesFileName") || die "Can't make $TimeSeriesFileName: $!";
# Print out the series of observed symbols, unspaced, followed by a newline
print TIMESERIES "$observed_sequence\n";
close(TIMESERIES) || die "Couldn't close $TimeSeriesFileName: $!";

$StateSeriesFileName = $FileNamesBase . "_stateseq";
open(STATESERIES, ">$StateSeriesFileName") || die "Can't make $StateSeriesFileName: $!";
# Print out the sequence of states, unspaced, followed by a newline
print STATESERIES "$state_sequence\n";
close(STATESERIES) || die "Couldn't close $StateSeriesFileName: $!";


sub make_a_move {
    $old_state = $_[0];
    $r = (rand);
    if ($r <= (1/3)) {
      $symbol = "u";
      $new_state = $old_state;
    } elsif ($r <= (2/3)) {
      $new_state = "B";
      if ($old_state eq "A") {
	$symbol = "R";
      } else {
	$symbol = "r";
      }
    } else {
      $new_state = "A";
      if ($old_state eq "A") {
	$symbol = "l";
      } else {
	$symbol = "L";
      }
    }
    return(($symbol,$new_state));
  }
