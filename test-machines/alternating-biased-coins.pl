#!/usr/bin/perl

# Program which simulates alternating between flipping a coin which produces
# 1 with p = .75 and one with p = 0.25.

srand; # Initialize random number generator
$number_states = 2; # two states

#Give the values of the parameter for emitting a 1
$Emit_One[0] = 0.75;
$Emit_One[1] = 0.25;
for ($i = 0; $i < $number_states; $i++) {
    $Emit_Zero[$i] = 1.0 - $Emit_One[$i];
    $Alphabetic_Label[$i] = chr(65+$i);
}

$Zero_Trans[0] = 1;
$Zero_Trans[1] = 0;

$One_Trans[0] = 1;
$One_Trans[1] = 0;


$repetitions = shift(@ARGV); # Make first argument number of steps to simulate

# Always start in state 1
$current_state = 0;

for ($i = 1; $i <= $repetitions; $i++) {
    $StateCounts[$current_state]++;
    ($observation, $next_state) = make_a_move($current_state);
    $observed_sequence .= $observation;
    # print "$observation";
    $current_state = $next_state;
    $state_sequence .= $Alphabetic_Label[$current_state];
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
    

$FileNamesBase = "ABC";
$MachineFileName = $FileNamesBase . "_machine";
open(MACHINE, ">$MachineFileName") || die "Can't make $MachineFileName: $!";
print MACHINE "Reference number: $FileNamesBase\n";
print MACHINE "Number of states: $number_states\n";
print MACHINE "Statistical complexity: $Cmu\n";
print MACHINE "Entropy rate: $hmu\n";
for ($i = 0; $i < $number_states; $i++) {
    print MACHINE "State $Alphabetic_Label[$i]:\n";
    print MACHINE "\t Probability $State_Probs[$i]\n";
    print MACHINE "\t Emits 0 with prob $Emit_Zero[$i] and goes to $Alphabetic_Label[$Zero_Trans[$i]]\n";
    print MACHINE "\t Emits 1 with prob $Emit_One[$i] and goes to $Alphabetic_Label[$One_Trans[$i]]\n";
    print MACHINE "\n";
}
close(MACHINE) || die "Couldn't close $MachineFileName: $!";

$TimeSeriesFileName = $FileNamesBase . "_timeseq";
open(TIMESERIES, ">$TimeSeriesFileName") || die "Can't make $TimeSeriesFileName: $!";
# Print out the series of observed symbols, unspaced, followed by a newline
print TIMESERIES "$observed_sequence\n";
close(TIMESERIES) || die "Couldn't close $TimeSeriesFileName: $!";
# system("gzip $TimeSeriesFileName");

$StateSeriesFileName = $FileNamesBase . "_stateseq";
open(STATESERIES, ">$StateSeriesFileName") || die "Can't make $StateSeriesFileName: $!";
# Print out the sequence of states, unspaced, followed by a newline
print STATESERIES "$state_sequence\n";
close(STATESERIES) || die "Couldn't close $StateSeriesFileName: $!";
system("gzip $StateSeriesFileName");


sub make_a_move {
    $old_state = $_[0];
    $r = (rand);
    if ($r <= $Emit_One[$old_state]) {
	$symbol = 1;
	# We enter this if we've produced a 1
	$new_state = $One_Trans[$old_state];
    }
    else {
	$symbol = 0;
	$new_state = $Zero_Trans[$old_state];
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
