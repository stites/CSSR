#!/usr/bin/perl

# Program which produces a long data-file simulating a hidden Markov model. The
# underlying Markov chain is drift on a ring with three sites: the three
# locations have probabilities (.1, .9) and (.5, .5) and (.8, .3) of going left
# or going right.  We always start in the center (.5, .5) state. Each left move
# is observed as a 0; each right move is observed as a 1.
#
# The correct state can be identified by counting the number of 0s since the
# beginning of the string.
#
# The first input is the number of steps to simulate.  That is the only input.
# The output symbols are unspaced.
#
# Output is to std-out, use redirects to put in files.
#
# To be used in conjunction with Kris's state-inference program.


srand; # Initialize random number generator
$number_states = 3; # Three-point ring

#Give the values of the parameter for going to the left
$go_left[0] = .1;
$go_left[1] = .5;
$go_left[2] = .8;

$repetitions = shift(@ARGV); # Make first argument number of steps to simulate

# Always start in state 1
$current_state = 1;

for ($i = 1; $i <= $repetitions; $i++) {
    $next_state = make_a_move($current_state);
    $observation = make_a_measurement($next_state - $current_state);
    print "$observation";
    $current_state = $next_state;
}
print "\n";


sub make_a_move {
    $new_location = $_[0];
    $r = (rand);
    if ($r <= $go_left[$new_location]) {
     # Going to the left decrements the state by one
     $new_location--;
    }
    else {
     # And going to the right increases it
     $new_location++;
    }
    # But we need to keep in the range 0--2
    $new_location = $new_location % $number_states;
    return($new_location);
}

sub make_a_measurement {
    # Input is new state minus old state
    $difference = $_[0];
    $measure = 0;
    if ($difference <= 0) {
     # If we moved left, give 0
     $measure = 0;
    }
    else {
     # If we moved right, give 1
     $measure = 1;
    }
    return($measure);
}
