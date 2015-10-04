#!/usr/bin/perl

# Program which produces a long data-file simulating a tricky hidden Markov
# model.
# The model is constructed so that each state can be unambiguously identified
# on the basis of the end of the observed history, _if_ we look at futures
# of over length 1, _and_ we insist on deterministic states.
# The topology is as follows:
# Any 0 takes us to state A.
# From A, a 1 takes us to B.
# From B, a 1 takes us to C.
# From C, a 1 takes us back to C.
# Thus, A is the set of histories {{.}*0}
# B is the set {{.}*01}
# C is the set {{.}*11}
# The probability of emitting a 1 is, respectively, .5, .5, and .8.
# We always begin in state A.
# The first input is the number of steps to simulate.  That is the only
# input.
# The output symbols are unspaced.
# Output is to files named TMD_foo.
# To be used in conjunction with Kris's state-inference program.


srand; # Initialize random number generator
$number_states = 3; # Three states

#Give the values of the parameter for emitting a 1
$Emit_One[0] = .5;
$Emit_One[1] = .5;
$Emit_One[2] = .8;
for ($i = 0; $i < $number_states; $i++) {
    $Emit_Zero[$i] = 1.0 - $Emit_One[$i];
    $Alphabetic_Label[$i] = chr(65+$i);
    $Zero_Trans[$i] = 0;
}

$One_Trans[0] = 1;
$One_Trans[1] = 2;
$One_Trans[2] = 2;


$repetitions = shift(@ARGV); # Make first argument number of steps to simulate

# Always start in state 1
$current_state = 0;

for ($i = 1; $i <= $repetitions; $i++) {
    $StateCounts[$current_state]++;
    ($observation, $next_state) = make_a_move($current_state);
    # $observation = make_a_measurement($next_state,$current_state);
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
    

$FileNamesBase = "TMD";
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

$DotFileName = $FileNamesBase . ".dot";
open(DOT, ">$DotFileName") || die "Can't make $DotFileName: $!";
$GraphName = $FileNamesBase;
$GraphName =~ s/:/_/g;
print DOT "digraph $GraphName {\n";
print DOT "size = \"6,8.5\";\n";
print DOT "ratio = \"fill\";\n";
print DOT "node [shape = circle];\n";
print DOT "node [fontsize = 24];\n";
print DOT "edge [fontsize = 24];\n";
for ($i = 0; $i < $number_states; $i++) {
    $Zero_Successor = $Zero_Trans[$i];
    $One_Successor = $One_Trans[$i];
    $ZeroProb = sprintf "%.3f", $Emit_Zero[$i];
    $OneProb = sprintf "%.3f", $Emit_One[$i];
    if ($ZeroProb > 0) {
	print DOT "$Alphabetic_Label[$i] -> $Alphabetic_Label[$Zero_Successor] [label = \"0  |  $ZeroProb\"];\n";
    }
    if ($OneProb > 0) {
	print DOT "$Alphabetic_Label[$i] -> $Alphabetic_Label[$One_Successor] [label = \"1  |  $OneProb\"];\n"
	}
}
print DOT "}\n";
close(DOT)|| die "Couldn't close $DotFileName: $!";
$PSFileName = $FileNamesBase . ".ps";
system("dot -Tps $DotFileName -o $PSFileName");
# I really ought to check whether dot could run successfully, but I don't
# know, and can't easily find out, what it returns.


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
    if ($r <= $Emit_One[$old_state]) {
	# We enter this if we've produced a 1
	$symbol = 1;
	$new_state = $One_Trans[$old_state];
    }
    else {
	# We've emitted a zero, and must go back to state 0
	$symbol = 0;
	$new_state = $Zero_Trans[$old_state];
    }
    return(($symbol,$new_state));
}

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
