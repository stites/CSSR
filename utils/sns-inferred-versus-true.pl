#!/usr/bin/perl

# Program which compares the simple nondeterministic source to machines
# inferred from the former's output

# We assume always a binary observational alphabet.


# To be used in conjunction with Kris's state-inference program.


# First argument is the name-base for the inferred-machine files
# The second is the name-base for the true machine files
# Third argument is the string length

$InferredNamesBase = shift(@ARGV);
$CausalNamesBase = shift(@ARGV);
$string_length = shift(@ARGV);


$CausalSeqFileName = $CausalNamesBase . "_stateseq";
$InferredSeqFileName = $InferredNamesBase . "_state_series";
$ResultsFileName = $InferredNamesBase . "_results";

# Build transition information

# First build the inferred state transition information

# Open the _results file for reading
open (RESULTS, $ResultsFileName) || die "Couldn't open $ResultsFileName: $!";
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
    # But we need a state label so use a function
    if ($ResultsLine[0] eq "State") {
	$InferredState = state_label($ResultsLine[2]);
	push(@InferredStatesList,$InferredState);
    } elsif ($ResultsLine[0] eq "distribution:") {
	# These lines look like "distribution: X Y", where X is prob of 0, etc.
	$InferredZeroProb{$InferredState} = $ResultsLine[1];
	$InferredOneProb{$InferredState} = $ResultsLine[2];
    } elsif ($ResultsLine[0] eq "transitions:") {
	# These lines look like "transitions: X Y"
	$InferredZeroTrans{$InferredState} = state_label($ResultsLine[1]);
	$InferredOneTrans{$InferredState} = state_label($ResultsLine[2]);
    }
}
# Continue scanning until file is ended
close(RESULTS) || die "Can't close $ResultsFileName: $!";

# Handle the occasional "-1" meaning no such state in the output
$InferredZeroTrans{state_label(-1)} = state_label(-1);
$InferredOneTrans{state_label(-1)} = state_label(-1);



# Calculation of H[\CausalState|\InferredState]

# Read in causal state sequence
open(CAUSAL_SEQ,$CausalSeqFileName)
    || die "Can't open $CausalSeqFileName: $!";
while (<CAUSAL_SEQ>) {
    chomp;
    $CausalSequence .= $_;
}
close(CAUSAL_SEQ) || die "Can't close $CausalSeqFileName: $!";
# system("gzip $CausalSeqFileName");

# Read in inferred state sequence
open(INFERRED_SEQ,$InferredSeqFileName)
    || die "Can't open $InferredSeqFileName: $!";
while (<INFERRED_SEQ>) {
    chomp;
    $InferredSequence .= $_;
}
close(INFERRED_SEQ) || die "Can't close $InferredSeqFileName: $!";

# We need to know the total number of symbols; also, we need to make sure
# the two series have the same length
$TotalCount = length($InferredSequence);
(length($InferredSequence) == length($CausalSequence)) 
    || die "State sequences are of different lengths!";


# We need the conditional distribution of causal states given inferred
# states
# Store this in a hash of hashes (so I don't have to worry about what
# the alphabet is), call it %ConditionalCount
# Also keep track of how many times each inferred state is seen, in
# $InferredCount

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


# Scan over sequence
for ($i = 0; $i < $TotalCount; $i++) {
    $Inferred = substr($InferredSequence,$i,1);
    $Causal = substr($CausalSequence,$i,1);
    # Get the characters at the ith position (counting from 0) of the series
    # Leave the series alone --- too much work to be constantly lopping things
    # off them
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
	$TotalCount--;
	# Decrement our count of the total number of symbols, since
	# we're not counting these
    }
}

# Convert counts to probabilities.
# Since searching through a hash may be slow, I'm combining this with
# manipulating the probabilities so as to get the conditional entropy for each
# inferred state and the overall conditional entropy

$ConditionalEntropy = 0;

foreach $Inferred (@InferredStatesList) {
    $InferredProb{$Inferred} = $InferredCount{$Inferred}/$TotalCount;
    $CondEnt{$Inferred} = 0;
    foreach $Causal (@ObservedCausalStates) {
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


# Invoke the subroutine to calculate relative entropy/KL-divergence and
# total variation
($KLDiv,$TotVar) = calc_relative_entropy();
$KL_rate = "Infinity";
$KL_rate = $KLDiv/$string_length unless ($KLDiv eq "Infinity");

$EffectiveCausalStates = @ObservedCausalStates;
$EffectiveInferred = @InferredStatesList;

$OutputFileName = $InferredNamesBase . "_comparison";
open(OUT, ">$OutputFileName") || die "Can't write to $OutputFileName: $!\n";
print OUT "Causal States Visited: $EffectiveCausalStates\n";
print OUT "Inferred States Visited: $EffectiveInferred\n";
print OUT "Divergence: $KLDiv bits over $string_length symbols\n";
print OUT "Rate: $KL_rate bits per symbol\n";
print OUT "Variation: $TotVar\n";
print OUT "Equivocation: $ConditionalEntropy bits\n";
close(OUT) || die "Can't close $OutputFileName: $!\n";


# HERE ENDETH THE PROGRAM

sub log_2 {
    # Input is a positive real number
    # Output is its base-two logarithm
    return(log($_[0])/log(2.0));
}


sub calc_relative_entropy
{

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

    foreach $string (@LStrings) {
	# For each hidden state, get the probability of that string if
	# started from that state; average over all states.
	# We have two hidden states, A and B.
	$Acopy = $string;
	$Bcopy = $string;
	$WordHidden{$string} = 0.5 * &hidden_prob($Acopy,"A");
	$WordHidden{$string} += 0.5 * &hidden_prob($Bcopy,"B");
    }


    foreach $string (@LStrings) {
	foreach $Inferred (keys %InferredProb) {
	    $copy = $string;
	    $CondInfProb = &inferred_prob($copy,$Inferred);
	    $WordInferred{$string} +=  $CondInfProb * $InferredProb{$Inferred};
	}
    }

    foreach $string (@LStrings) {
	if ($WordHidden{$string} > 0) {
	    $RelativeEntropy = "Infinity" if ($WordInferred{$string} == 0);
	    if ($RelativeEntropy ne "Infinity") {
		$RelativeEntropy += $WordHidden{$string} * (log_2($WordHidden{$string}) - log_2($WordInferred{$string}));
	    }
	}
	$TotalVariation += abs($WordHidden{$string} - $WordInferred{$string});
    }

    return(($RelativeEntropy,$TotalVariation));
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

sub hidden_prob {
    # Calculates, recursively, the probability of a string
    # under the true distribution, starting from a given (hidden) state.
    # Takes two inputs: the string and the initial state.
    my($string,$state,$symb,$prob);
    $string = $_[0];
    $state = $_[1];
    $symb = chop($string);
    return(1) unless ($symb ne ""); # Null string means we're done!
    $prob = &symbol_prob($symb,$state);
    return(0) if ($prob == 0);
    if (($string ne "") && ($state eq "A")) {
	$prob *= 0.5* (&hidden_prob($string,"A") + &hidden_prob($string,"B"));
    }
    if (($string ne "") && ($state eq "B")) {
	$prob *= &hidden_prob($string,&B_successor($symb));
    }
    return($prob);
}

sub symbol_prob {
    # Function that gives the probability of emitting a symbol from a state
    # Two arguments: symbol and state!
    my($symb,$state,$prob);
    $symb = $_[0];
    $state = $_[1];
    $prob = 0;
    $prob = 1 if (($state eq "A") && ($symb eq "1"));
    $prob = 0.5 if ($state eq "B");
    return($prob);
}

sub B_successor {
    # Gives the state reached from hidden state B on the input symbol
    $symb = $_[0];
    $state = "A" if ($symb eq "0");
    $state = "B" if ($symb eq "1");
    return($state);
}

sub inferred_prob
{
    # Calculates inferred prob. of word starting from a state
    my($string,$state);
    $string = $_[0];
    $state = $_[1];

    $StringCondInferred = 1;
    $word_length = length($string);
    for ($i = 0; $i < $word_length; $i++) {
	$CurrentSymbol = chop($string);
	if ($CurrentSymbol == 0) {
	    $StringCondInferred *= $InferredZeroProb{$state};
	    $state = $InferredZeroTrans{$state};
	} else {
	    $StringCondInferred *= $InferredOneProb{$state};
	    $state = $InferredOneTrans{$state};
	}
    }
    return($StringCondInferred);
}
