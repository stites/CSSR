#!/usr/bin/perl

# Program which simulates a simple finite-state machine, with binary
# output, where the states are strictly periodic, and each state can
# produce either symbol, though with differing probability of 1, except
# for the first state, which always produces 0.  Thus it has a synchronizing
# word: (p-1) consecutive 1s, p being the period.


# Input: number of states, number of time-steps

$num_states = shift(@ARGV);
$time_steps = shift(@ARGV);

$state = 0;



for ($i = 0; $i < $time_steps; $i++) {
  $r = rand();
  $symbol = 1;
  $symbol = 0 if (($r < 0.5) && ($state > 0));
  $symbol = 0 if ($state == 0);
  print $symbol;
  $state++;
  $state %= $num_states;
}
print "\n";
