#!/usr/bin/perl

# Simple nondeterministic source
# 2 HMM states
# In state 0, always emit "1"; stay in 0 w/prob 1/2 and goto 1 w/prob 1/2
# In state 1, emit "1" and stay in 1 w/prob 1/2, emit "0" and goto 0 otherwise

srand; # Initialize random number generator
$number_states = 2; # Two states

#Give the values of the parameter for emitting a 1
$Goto_One[0] = .5; # Prob that next state = 1
$Goto_One[1] = .5;
for ($i = 0; $i < $number_states; $i++) {
}

$repetitions = shift(@ARGV); # Make first argument number of steps to simulate

# Always start in state 1
$current_state = 0;

for ($i = 1; $i <= $repetitions; $i++) {
    $state_sequence .= $current_state;
    $StateCounts[$current_state]++;
    ($observation, $next_state) = make_a_move($current_state);
    $observed_sequence .= $observation;
    # print "$observation";
    $current_state = $next_state;
}
print "\n";

# Make the state probabilities, Cmu, hmu
for ($i = 0; $i < $number_states; $i++) {
    $State_Probs[$i] = $StateCounts[$i]/$repetitions;
    if ($State_Probs[$i]) {
	$Cmu -= $State_Probs[$i] * log_2($State_Probs[$i]);
	$hmu += $State_Probs[$i] * binary_entropy($Emit_Zero[$i]);
    }
}
    

$FileNamesBase = "SNS";

$TimeSeriesFileName = $FileNamesBase . "_timeseq";
open(TIMESERIES, ">$TimeSeriesFileName") || die "Can't make $TimeSeriesFileName: $!";
# Print out the series of observed symbols, unspaced, followed by a newline
print TIMESERIES "$observed_sequence\n";
close(TIMESERIES) || die "Couldn't close $TimeSeriesFileName: $!";
# system("gzip $TimeSeriesFileName");

sub make_a_move {
    $old_state = $_[0];
    $r = (rand);
    if ($r <= $Goto_One[$old_state]) {
	$new_state = 1;
	if ($old_state == 0) {
	    $symbol = 1;
	} elsif ($old_state == 1) {
	    $symbol = 1;
	}
    }
    else {
	$new_state = 0;
	if ($old_state == 0) {
	    $symbol = 1;
	} elsif ($old_state == 1) {
	    $symbol = 0;
	}
    }
    return(($symbol, $new_state));
}

# sub make_a_measurement {
#    # Input is new state, old state
#    $new_state = $_[0];
#    $old_state = $_[1];
#    if ($old_state == 0) {
#	if ($new_state == 0) {
#	    $measure = 0;
#	} elsif ($new_state == 1) {
#	    $measure = 1;
#	}
#    } elsif ($old_state == 1) {
#	if ($new_state == 0 {
#	}
#    if ($new_state == 0) {
#	   # We only go to state 0 on a zero (and vice-versa)
#	   $measure = 0;
#    }
#    elsif ($new_state == ($number_states - 1)) {
#	   # And we only go to the final state on a 1
#	   $measure = 1;
#    }
#    else {
#	   # And the only time we see a 0 is in going to state 0
#	   $measure = 1;
#    }
#    return($measure);
# }

sub binary_entropy {
    # Input is probability of emitting a zero
    # Output is the entropy of the binary distribution in bits
    $prob_zero = $_[0];
    $prob_one = 1.0 - $prob_zero;
    # Avoid 0 log 0 by testing for it explicitly!
    if (($prob_zero == 0.0) || ($prob_zero == 1.0)) {
	$entropy = 0.0;
    } else {
	$entropy = $prob_zero * log_2($prob_zero);
	$entropy += $prob_one * log_2($prob_one);
    }
    return(-$entropy);
}  

sub log_2 {
    # Input is a positive real number
    # Output is its base-two logarithm
    return(log($_[0])/log(2.0));
}
