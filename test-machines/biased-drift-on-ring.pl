#!/usr/bin/perl

# Program which produces a long data-file simulating a first-order Markov
# chain, which in turn models drift on a ring with a preferred direction.
# The first input is the number of states.  The second is the probability of
# moving forward, the third the probability of moving backwards.  The fourth
# and final input is the number of steps to simulate.
# The output symbols are unspaced.
# Output is to std-out, use redirects to put in files.
# To be used in conjunction with Kris's state-inference program.


srand; # Initialize random number generator

$number_states = shift(@ARGV); # Make first argument number of sites on ring
$go_forward = shift(@ARGV); # Make 2nd arg. prob. of going forward
$go_backward = shift(@ARGV); # Make 3rd arg. prob. of going back
$repetitions = shift(@ARGV); # Number of steps to simulate

# Pick a starting state uniformly
$current_state = int(rand $number_states);

for ($i = 1; $i <= $repetitions; $i++) {
    print "$current_state";
    $current_state = make_a_move($current_state);
}
print "\n";


sub make_a_move {
    $new_location = $_[0];
    $r = (rand);
    if ($r <= $go_forward) {
	   $new_location++;
    }
    elsif ($r <= ($go_forward + $go_backward)) {
	   $new_location--;
    }
    else {
	   +$new_location;
    }
    $new_location = $new_location % $number_states;
    return($new_location);
}
