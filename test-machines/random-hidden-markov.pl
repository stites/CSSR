#!/opt/gnu/bin/perl

# Program which generates a random deterministic hidden Markov model, then
# runs it.

# The user inputs the number of states and the number of steps to simulate,
# in that order.

# The program generates an HMM with that many states, always with a binary
# observational alphabet.
# For each state, the model first picks the probability that the state emits
# a 0, uniformly in [0, 1].  It then picks, uniformly among the hidden states,
# the state gone to on a zero, and (independently, and again uniformly) the
# state gone to on a 1.  These three pieces of information are recorded
# for each state.
# We also record (but of course this isn't independent) the entropy of the
# next symbol conditional on the current state.
# After generating all states, we should check that no two are duplicates,
# i.e., no two have _both_ the same output prob. and the same transitions.
# If this is done, the HMM states are the causal states (since each
# corresponds to a unique future distribution).
# We then pick a state (uniformly) and simulate the motion, recording
# both the observable sequence and the state sequence.
# From the state sequence, we calculate the (empirical) distribution over
# states and its entropy (= Cmu).  We also give the weighted average entropy
# of the next symbol given the current state = hmu.

# We produce a number of output files.
# The first gives all transition information for all states, plus Cmu and hmu.
# The second starts in a random (uniformly chosen) state and produces
# output for the given length of time.
# The third gives the state sequence.

# The first input is the number of states to use.  The second input is
# the number of steps to simulate.
# The output symbol sequences are unspaced.
# The state-information output probably ought to be dot-formatted
# Output is to file named by the run-time.
# To be used in conjunction with Kris's state-inference program.


srand; # Initialize random number generator

#

$number_states = shift(@ARGV); # Make first arg number of states to use

# Most state information is recorded in a series of arrays.
# The first says what the prob of emitting a zero is for each state.
# The second says what state is gone to on a zero.
# The third  what state is gone to on a one.
# The fourth records the entropy of the next symbol for each state.
# @State_Counts records the number of times each state has
# been visited
# @State_Probs records the state probabilities

for ($i = 0; $i < $number_states; $i++) {
    $Emit_Zero[$i] = rand; # i.e. Prob of 0 = rand. no. in [0, 1)
    $Emit_One[$i] = 1.0 - $Emit_Zero[$i]; # Technically redundant but often
    # convenient to have pre-calculated and anyway small
    $Zero_Trans[$i] = int(rand $number_states); # state gone to on 0
    $One_Trans[$i] = int(rand $number_states); # state gone to on 1
    # 
    $Entropy_Rate[$i] = binary_entropy($Emit_Zero[$i]);
    $Alphabetic_Label[$i] = chr(65 + $i); # Give the state an alphabetic label,
    # beginning with "A"
    $State_Counts[$i] = $State_Probs[$i] = 0;
}

$repetitions = shift(@ARGV); # Make second argument number of steps to simulate

# Always start in state 1
$current_state = int(rand $number_states); # Pick a random state to start in


for ($i = 1; $i <= $repetitions; $i++) {
    $State_Counts[$current_state]++; # Increment the state counts
    $observation = make_a_measurement($current_state);
    # Figure out the next symbol to show
    $next_state = make_a_move($current_state,$observation);
    # See which state that symbol puts us in
    $observed_sequence .= $observation; # Append observed to sequence thereof
    $state_sequence .= $Alphabetic_Label[$current_state]; # Append state to
	# sequence thereof
    $current_state = $next_state; # Move to next state
}

$NumberStatesVisited = 0;
$OneStateVisited = 0;

# Generate state probabilities and calculate hmu
for ($i = 0; $i < $number_states; $i++) {
    $State_Probs[$i] = $State_Counts[$i]/($repetitions);
    if ($State_Counts[$i] > 0) {$NumberStatesVisited++};
    if ($State_Counts[$i] == ($repetitions)) {
	$OneStatedVisited = 1;
	# We're keeping track of how many states we've actually seen
	print "Only ever in state $i\n";
    }
    # Since the number of states visited just equals the number of symbols -1
    $hmu += $State_Probs[$i] * $Entropy_Rate[$i]
}


# Calculate Cmu
if ($OneStateVisited == 1)
{
    $Cmu = 0;
} else {
    for ($i = 0; $i < $number_states; $i++) {
	if ($State_Counts[$i] != 0) {
            # Never-visited states do not contribute to means
	    $Cmu -= $State_Probs[$i] * log_2($State_Probs[$i]);
	}
    }
}

# Now deal with the output
# We make five files, with the following extensions:
# _machine [Machine in words, plus Cmu, hmu]
# .dot [Dot file]
# _timeseq [time series]
# _stateseq [state series]
# _statealpha [state alphabet]

$FileNamesBase = "HMM_with_" . $number_states . "_States_" . $NumberStatesVisited . "_Active_" . gmtime;
$FileNamesBase =~ s/\s/_/g;
# File name base is the Greenwich Mean Time

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
system("gzip  $PSFileName");
# I really ought to check whether dot could run successfully, but I don't
# know, and can't easily find out, what it returns.

$TimeSeriesFileName = $FileNamesBase . "_timeseq";
open(TIMESERIES, ">$TimeSeriesFileName") || die "Can't make $TimeSeriesFileName: $!";
# Print out the series of observed symbols, unspaced, followed by a newline
print TIMESERIES "$observed_sequence\n";
close(TIMESERIES) || die "Couldn't close $TimeSeriesFileName: $!";
system("gzip $TimeSeriesFileName");

$StateSeriesFileName = $FileNamesBase . "_stateseq";
open(STATESERIES, ">$StateSeriesFileName") || die "Can't make $StateSeriesFileName: $!";
# Print out the sequence of states, unspaced, followed by a newline
print STATESERIES "$state_sequence\n";
close(STATESERIES) || die "Couldn't close $StateSeriesFileName: $!";
system("gzip $StateSeriesFileName");


# Generate the state alphabet (for posterity)
$StateAlphaFileName = $FileNamesBase . "_statealpha";
open(STATEALPHA, ">$StateAlphaFileName") || die "Can't make $StateAlphaFileName: $!";
# List the alphabetic labels in order, unspaced, followed by a newline
for ($i = 0; $i < $number_states; $i++) {
    print STATEALPHA $Alphabetic_Label[$i];
}
print STATEALPHA "\n";
close(STATEALPHA) || die "Couldn't close $StateAlphaFileName: $!";




sub make_a_move {
    # Inputs: present state, next symbol
    # Output: next state
    $current_state = $_[0];
    $measurement = $_[1];
    if ($measurement == 0) {
	$next_state = $Zero_Trans[$current_state];
    } else {
	$next_state = $One_Trans[$current_state];
    }
    return($next_state);
}

sub make_a_measurement {
    # Input is current state
    # Output is new observational symbol
    $present_state = $_[0];
    $random_choice = rand;
    if ($random_choice <= $Emit_Zero[$present_state]) {
	   $measure = 0;
    }
    else {
	   $measure = 1;
    }
    return($measure);
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
