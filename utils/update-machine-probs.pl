#!/usr/bin/perl

# Program which takes a deterministic probabilisti automaton, as generated
# by CSSR, and updates its state-transition probabilities from new
# data.
# Outputs the new machine.

# The user needs to say where information about the input machine may be
# found, a little about the process which produced it, and the name of
# the new data file.  Optionally, the user also says which state to start
# machine in; otherwise the program must synchronize, and won't start updating
# probabilities until has.  The program will output the final state, to aide
# in recursive use.

# This assumes a binary, 01 alphabet, but that can and should be generalized.

# To be used in conjunction with CSSR


# First argument is the name-base for the inferred-machine files
## This will generally be the "_results" output file from CSSR, but it
## may have been renamed
# The second argument the number of data points used in inferring it
# The third argument is the new data file
# The fourth argument, if present, is the number of the start-state (counting from zero) 


### This should work, double-check effect of pop
unless ((@ARGV == 4) || (@ARGV == 3)) die "Usage: $0 machine_name data_size new_data [starting_state_number]\n";
if (@ARGV == 4) {
    $start_state = pop(@ARGV);
    $state_known = 1;
} else {
    $state_known = 0;
}
$machine_file_name = shift(@ARGV);
$data_size = shift(@ARGV);
$new_data = shift(@ARGV);

@Alphabet = ("0", "1");


$InferredNamesBase = $machine_file_name; # Shouldn't need any of these
$TimeSeqFileName = $InferredNamesBase;
$CausalSeqFileName = $CausalNamesBase . "_stateseq";
$InferredSeqFileName = $InferredNamesBase . "_state_series";
$ResultsFileName = $InferredNamesBase;
$MachineFileName = $CausalNamesBase . "_machine";

# Build transition information

# First build the inferred state transition information

# Open the _results file for reading
open (RESULTS, $machine_file_name) || die "Couldn't open $ResultsFileName: $!";
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
	$InferredState = $ResultsLine[2];
	push(@InferredStatesList,$InferredState);
    } elsif ($ResultsLine[0] eq "distribution:") {
	# These lines look like "distribution: P(0) = X P(1) = Y", where X is
	# prob of 0, etc.
	# So X is word 3 (from 0), and Y is word 6
	$InferredZeroProb{$InferredState} = $ResultsLine[3];
	$InferredOneProb{$InferredState} = $ResultsLine[6];
    } elsif ($ResultsLine[0] eq "transitions:") {
	# These lines look like "transitions: T(0) = X T(0) = Y"
	$InferredZeroTrans{$InferredState} = $ResultsLine[3];
	$InferredOneTrans{$InferredState} = $ResultsLine[6];
    }
    # Oops!  We need to get the state probabilities here too, and it seems
    # the code this is copied from didn't need to do that originally
    $InferredStateProb{$InferredState} = ;
}
# Continue scanning until file is ended
close(RESULTS) || die "Can't close $ResultsFileName: $!";

# Handle the occasional "NULL" meaning no such state in the output
$InferredZeroTrans{"NULL"} = "NULL";
$InferredOneTrans{"NULL"} = "NULL";

#### Let's change from the rather ugly, binary-only format to one which will
#### work more generally.
#### This would be a hash of hashes
foreach $state (@InferredStates) {
    $Transitions{$state}{0} = $InferredZeroTrans{$state};
    $Transitions{$state}{1} = $InferredOneTrans{$state};
    $Prob{$state}{0} = $InferedZeroProb{$state};
    $Prob{$state}{1} = $InferredOneProb{$state};
}

# Convert read-in probabilities to counts
foreach $state (@InferredStatesList) {
    $StateCount{$state} = $InferredStateProb{$state} * $data_size;
    $TranisitionCounts{$state} = ${$state}{0} * $StateCount{$state};
    $TransitionOneCount{$state} = $InferredOneProb{$state} * $StateCount{$state};
}

# Read in the new data
open (DATA, $new_data) ||
    die "$0 says: Can't open $new_data for reading: $!\n";
while (<DATA>) {
    chomp;
    $dataseq = $_;
}
# Prepare the list of states from synchronization (if we don't know the start
# state)
# To simplify life later, we will actually use a HASH rather than a list
# (because then we can just use "keys" to get all the distinct values, which
# is harder with a list
unless ($state_known == 1) {
    foreach $state (@InferredStatesList) {
	$PossibleStates{$state} = 1;
    }
    $current_state = NULL;
} else {
    $current_state = $start_state;
}

$synchronization_count = 0; # This tracks how many steps it's taken us
# to synchronize
# Now step through the data
for ($i = 0; $i < length($dataseq); $i++) {
    $symbol = substr($dataseq,$i,1);
    # Do we know what state we're in?
    if ($state_known == 1) {
	# If we do know what state we're in, then update the transition counts
	# for that state
	$StateCount{$current_state}++;
	increment_count($current_state,$symbol);
	# update the state
	$new_state = make_transition($current_state, $symbol);
	# Are we in trouble?
	if ($current_state eq "NULL") {
	    # If so, die
	    die "$0: An apparently imposible transition has occurred at position $i, when we thought we were in state $current_state\n";
	} else {
	    # If not, update the state
	    $current_state = $new_state;
	}
    }
    # If we do not know the state, try to synchronize
    else {
	$synchronization_count++;
	foreach $state (keys(%PossibleStates)) {
	    $new_state = make_transition($state,$symbol);
	    unless ($new_state eq "NULL") {
		$NewPossibleStates{$new_state} = 1;
	    }
	}
	@new_states = keys(%NewPossibleStates);
	if (@new_states  > 2) {
	    %PossibleStates = %NewPossibleStates;
	} elsif (@new_states  == 1) {
	    $state_known = 1;
	    $current_state = $new_states[0];
	    $StateCount{$current_state}++;
	} else {
	    die "$0: Failure to synchronize. At position $i, we none of the states still considered possible %PossibleStates had a valid transition\n";
	}
    }
}

# Make sure that we do in fact have new data to report
if ($state_known == 0) {
    die "$0: Failure to synchronize. Reached the end of the new data and never managed to figure out the state\n";
}

# Prepare to output
# First, convert counts back in to probabilities
# Do all probabilities as ratios of individual terms to sums (rather than
# trying anything fancy)
# Then, output in the same CSSR format as you read in
# But append to the end of the file the total effective data
# size = $data_size + length($dataseq) - $synchronization_count
$combined_data_size = $data_size + length($dataseq) - $ynchronization_count;

	


# HERE ENDETH THE PROGRAM

sub log_2 {
    # Input is a positive real number
    # Output is its base-two logarithm
    return(log($_[0])/log(2.0));
}

sub increment_count {
    $state = $_[0];
    $symbol = $_[1];
    if ($symbol eq "0") {
	$TransitionZeroCount{$state}++;
    }
    if ($symbol eq "1") {
	$TransitionOneCount{$state}++;
    }
}

sub make_transition {
    $state = $_[0];
    $symbol = $_[1];
    if ($symbol eq "0") {
	$new_state = $InferredZeroTrans{$state};
    }
    if ($symbol eq "1") {
	




sub calc_relative_entropy {

# Calculate the entropy of the inferred states' distribution for
# strings of length $string_length relative to that of the causal states.

# While we're at it, calculate the total-variation distance between
# the distributions.

# Entropy of supposed distribution Q relative to true distribution P is
# sum_{i}{P_i log(P_i/Q_i)}, with 0log(0/q) = 0 and plog(p/0) = infinity.
# The interpretation is that a code based on Q requires H(P) + D(Q||P)
# bits per symbol, whereas a true code based on P needs just H(P).
# This means we get infinitely penalized for saying that something isn't
# possible when it is, but oh well.

    @LStrings = ();
    $num_strings = 2**$string_length;

# Generate all possible strings of length L
    for ($i = 0; $i < $num_strings; $i++) {
	# Make $string = the binary version of $i
	$string = binary_of_number($i);
	# Add $string to the list of all L-strings
	@LStrings = (@LStrings, $string);
    }

# For each string of length L,
    foreach $string (@LStrings) {
	# Calculate the true, inferred and empirical probabilities
	$WordCausal = &causal_string_prob($string);
	$WordInf = &inferred_string_prob($string);
	$WordEmp = empirical_string_prob($string);

	if ($WordCausal > 0) {
	    if ($WordInf == 0) {
		# Set the relative entropy to infinity
		$RelativeEntropy = "Infinity";
	    }
	    elsif ($RelativeEntropy ne "Infinity") {
		$KLTerm = log_2($WordCausal/$WordInf);
		$KLTerm *= $WordCausal;
		$RelativeEntropy += $KLTerm;
	    }
	}
	if ($WordEmp > 0) {
	    if ($WordInf == 0) {
		# Set the relative entropy to infinite
		$EmpiricalDivergence = "Infinity";
	    }
	    elsif ($EmpiricalDivergence ne "Infinity") {
		$DivTerm += log_2($WordEmp) - log_2($WordInf);
		$DivTerm *= $WordEmp;
		$EmpiricalDivergence += $DivTerm;
	    }
	}
	$TotalVariation += abs($WordCausal - $WordInf);
	$EmpiricalVariation += abs($WordEmp - $WordInf);
	$SamplingFluctuation += abs($WordEmp - $WordCausal);
    }
    @Values = ($RelativeEntropy,$TotalVariation);
    push(@Values,$EmpiricalDivergence);
    push(@Values,$EmpiricalVariation);
    push(@Values,$SamplingFluctuation);
    return(@Values);
}

sub state_label {
    # Take in a number and return the corresponding letter symbol
    # on the assumption that number 0 corresponds to A, and so on up through
    # ASCII
    $state_number = $_[0];
    $state_symbol = chr(65 + $state_number);
    return($state_symbol);
}

sub binary_of_number {
    # Function that takes an integer, presumed less than 2**$string_length,
    # and gives its binary representation
    $number = $_[0];
    $binary_string = "";
    for ($j = $string_length-1;$j >= 0; $j--) {
	# Count down the powers of two
	$TwoPower = 2**$j;
	if ($number >= $TwoPower) {
	    # If the number is greater than that power,
	    # then subtract the power and record a 1 there
	    $binary_string .= "1";
	    $number -= $TwoPower;
	} else {
	    # Otherwise, record a zero and go on the next lower power
	    $binary_string .= "0";
	}
    }
    return($binary_string);
}

sub causal_string_prob {
    # Takes a string and returns its probability under the true machine.
    my($string,$prob,$word_length);
    $string = $_[0];

    $word_length = length($string);

    foreach $Causal (@CausalStatesList) {
	# For each causal state, find the probability of that string if started
	# from that state; record in $StringCondCausal
	$StringCondCausal = 1;
	$CurrentState = $Causal;
	for ($i = 0; $i < $word_length; $i++) {
	    $CurrentSymbol = substr($string,$i,1);
	    if ($CurrentSymbol == 0) {
		$StringCondCausal *= $CausalZeroProb{$CurrentState};
		$CurrentState = $CausalZeroTrans{$CurrentState};
	    } else {
		$StringCondCausal *= $CausalOneProb{$CurrentState};
		$CurrentState = $CausalOneTrans{$CurrentState};
	    }
	}
	# Average over all states
	$prob += $StringCondCausal * $CausalProb{$Causal};
    }
    return($prob);
}

sub inferred_string_prob {
    # Calculate the probability of a string under the inferred machine
    my($string,$prob,$i,$CurrentState,$word_length);
    $string = $_[0];
    $word_length = length($string);

    foreach $Inferred (keys %InferredProb) {
	$StringCondInferred = 1;
	# For each inferred state,
	# Caclulate the probability of that string if started from that
	# state
	$CurrentState = $Inferred;
	for ($i = 0; $i < $word_length; $i++) {
	    $CurrentSymbol = substr($string,$i,1);
	    if ($CurrentSymbol == 0) {
		$StringCondInferred *= $InferredZeroProb{$CurrentState};
		$CurrentState = $InferredZeroTrans{$CurrentState};
	    } else {
		$StringCondInferred *= $InferredOneProb{$CurrentState};
		$CurrentState = $InferredOneTrans{$CurrentState};
	    }
	}
	# Record in temporary $StringCondInferred
	# Average over all states
	$prob += $StringCondInferred * $InferredProb{$Inferred};
    }
    return($prob);
}

sub empirical_string_prob {
    # Calculate the empirical probability of a string
    my($string,$prob,$word_length);
    $string = $_[0];
    $word_length = length($string);
    
    $denominator = $time_seq_length - $word_length;
    $prob = 0;
    $prob = $EmpWord{$string}/$denominator if ($EmpWord{$string});
    # %EmpWord is defined only for keys that actually occur in the data.
    return($prob);
}


sub initialize_cond_prob_thingies {
    # Initialized %ConditionalCount and %CondProb
    foreach $Inferred (@InferredStatesList) {
	$HaveInferred{$Inferred} = 0;
	$InferredCount{$Inferred} = 0;
	$InferredProb{$Inferred} = 0;
	$rec = {};
	$rec2 = {};
	$ConditionalCount{$Inferred} = $rec;
	$CondProb{$Inferred} = $rec2;
	foreach $Causal (@CausalStatesList) {
	    ($key, $Count) = ($Causal, 0);
	    ($key2, $Prob) = ($Causal, 0);
	    $rec->{$key} = $Count;
	    $rec2->{$key2} = $Prob;
	}
    }

    foreach $Causal (@CausalStatesList) {
	$HaveSeen{$Causal} = 0;
    }
}

sub scan_for_cond_probs{
    # Scan over sequence
    for ($i = 0; $i < $TotalCount; $i++) {
	$Inferred = $InferredSequence[$i];
	$Causal = substr($CausalSequence,$i,1);
	# Get the characters at the ith position (counting from 0) of the
	# series.
	# Leave the series alone --- too much work to be constantly lopping
	# things off them
	# If $Inferred = ?, then we've not synchronized yet, and rather than
	# deal with synchronization states, we'll just ignore that position
	if ($Inferred ne "\?") {
	    $ConditionalCount{$Inferred}{$Causal}++;
	    $InferredCount{$Inferred}++;
	    # Record that we have seen this causal state
	    if ($HaveSeen{$Causal} == 0) {
		push(@ObservedCausalStates,$Causal);
		$HaveSeen{$Causal} = 1;
	    }
	    if ($HaveInferred{$Inferred} == 0) {
		push(@SeenInferred,$Inferred);
		$HaveInferred{$Inferred} = 1;
	    }
	} else {
	    $TotalCounts--;
	    # Decrement our count of the total number of symbols, since
	    # we're not counting these
	}
    }
}

sub cond_counts_to_cond_probs {
    # Convert counts to probabilities.
    # Since searching through a hash may be slow, I'm combining this with
    # manipulating the probabilities so as to get the conditional entropy for
    # each inferred state and the overall conditional entropy
    my($ConditionalEntropy);

    $ConditionalEntropy = 0;
    
    foreach $Inferred (@InferredStatesList) {
	$InferredProb{$Inferred} = $InferredCount{$Inferred}/$TotalCount;
	$CondEnt{$Inferred} = 0;
	foreach $Causal (@ObservedCausalStates) {
	    # with @ObservedCausalStates ought to
	    if ($InferredCount{$Inferred} > 0) {
		$CondProb{$Inferred}{$Causal} = $ConditionalCount{$Inferred}{$Causal}/$InferredCount{$Inferred};
	    } else {
		$CondProb{$Inferred}{$Causal} = 0;
	    }
	    if ($CondProb{$Inferred}{$Causal} > 0) {
		$CondEnt{$Inferred} -= $CondProb{$Inferred}{$Causal} * log_2($CondProb{$Inferred}{$Causal});
	    }
	}
	$ConditionalEntropy += $InferredProb{$Inferred} * $CondEnt{$Inferred};
    }
    return($ConditionalEntropy);
}
