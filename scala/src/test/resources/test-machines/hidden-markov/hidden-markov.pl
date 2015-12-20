#!/opt/gnu/bin/perl

# Program which produces a long data-file simulating a hidden Markov model.
# The model is constructed so that each state can be unambiguously identified
# on the basis of the end of the observed history.
# The topology is as follows:
# Any 0 takes us to state A.
# From A, a 1 takes us to B.
# From B, a 1 takes us to C.
# From C, a 1 takes us back to C.
# Thus, A is the set of histories {{.}*0}
# B is the set {{.}*01}
# C is the set {{.}*11}
# The probability of emitting a is, respectively, .5, .3, and .8.
# We always begin in state A.
# The first input is the number of steps to simulate.  That is the only
# input.
# The output symbols are unspaced.
# Output is to std-out, use redirects to put in files.
# To be used in conjunction with Kris's state-inference program.


srand; # Initialize random number generator
$number_states = 3; # Three states

#Give the values of the parameter for going to the left
$emit_one[0] = .5;
$emit_one[1] = .3;
$emit_one[2] = .8;

$repetitions = shift(@ARGV); # Make first argument number of steps to simulate

# Always start in state 1
$current_state = 0;

for ($i = 1; $i <= $repetitions; $i++) {
    $next_state = make_a_move($current_state);
    $observation = make_a_measurement($next_state,$current_state);
    print "$observation";
    $current_state = $next_state;
}
print "\n";


sub make_a_move {
    $new_state = $_[0];
    $r = (rand);
    if ($r <= $emit_one[$new_state]) {
	   # Emitting a 1 always moves us to the next state forward
	   $new_state++;
    }
    else {
	   # And emitting a zero moves us back
	   $new_state--;
    }
    # But we need to keep in the range 0--2
    # And note that a 1, in state 2, keeps us in 2, whereas a 0, in state 0,
    # keeps us in 0
    if ($new_state < 0) {
	   $new_state = 0;
    }
    if ($new_state > $number_states) {
	   $new_state = $number_states;
    }
    return($new_state);
}

sub make_a_measurement {
    # Input is new state, old state
    $new_state = $_[0];
    $old_state = $_[1];
    if ($new_state == 0) {
	   # We only go to state 0 on a zero (and vice-versa)
	   $measure = 0;
    }
    elsif ($new_state == $number_states) {
	   # And we only go to the final state on a 1
	   $measure = 1;
    }
    else {
	   # And the only time we see a 0 is in going to state 0
	   $measure = 1;
    }
    return($measure);
}
