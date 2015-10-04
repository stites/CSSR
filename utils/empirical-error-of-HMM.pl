#!/usr/bin/perl

# Program which compares an inferred (deterministic) HMM to a dataset, finding
# the relative entropy and the total variation distance between the empirical
# distribution and the inferred distribution over strings of a given length.

# We assume always a binary observational alphabet.

# To be used in conjunction with Kris's state-inference program.


# First argument is the name-base for the inferred-machine files
# If two arguments, the second is the history length.
# If three arguments, the second is the name of the time series, and the
# third is the history length.

if (@ARGV == 3) {
    $InferredNamesBase = shift(@ARGV);
    $TimeSeriesName = shift(@ARGV);
    $string_length = shift(@ARGV);
} elsif (@ARGV == 2) {
    $InferredNamesBase = shift(@ARGV);
    $TimeSeriesName = $InferredNamesBase;
    $string_length = shift(@ARGV);
} else {
    die "Useage: $0 machine_name [time_series] history_length\n";
}




$TimeSeqFileName = $TimeSeriesName;
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
	$InferredState = $ResultsLine[2];
	push(@InferredStatesList,$InferredState);
    } elsif ($ResultsLine[0] eq "distribution:") {
	# These lines look like "distribution: X Y", where X is prob of 0, etc.
	$InferredZeroProb{$InferredState} = $ResultsLine[1];
	$InferredOneProb{$InferredState} = $ResultsLine[2];
    } elsif ($ResultsLine[0] eq "transitions:") {
	# These lines look like "transitions: X Y"
	$InferredZeroTrans{$InferredState} = $ResultsLine[1];
	$InferredOneTrans{$InferredState} = $ResultsLine[2];
    } elsif ($ResultsLine[0] eq "probability:") {
	$InferredStateProb{$InferredState} = $ResultsLine[1];
    }
}
# Continue scanning until file is ended
close(RESULTS) || die "Can't close $ResultsFileName: $!";

# Handle the occasional "-1" meaning no such state in the output
$InferredZeroTrans{-1} = -1;
$InferredOneTrans{-1} = -1;


# We need to get the original time-series for comparative purposes
open(TIME_SEQ,$TimeSeqFileName) || die "Can't open $TimeSeqFileName: $!";
while (<TIME_SEQ>) {
    chomp;
    $TimeSeq .= $_;
}
close(TIME_SEQ) || die "Can't close $TimeSeqFileName: $!";


# Get the empirical probabilities
$time_seq_length = length($TimeSeq);
for ($i = 0; $i < ($time_seq_length - $string_length); $i++) {
    $string = substr($TimeSeq,$i,$string_length);
    $EmpWord{$string}++;
}






# Invoke the subroutine to calculate relative entropy/KL-divergence and
# total variation
($EmpDiv,$EmpVar) = calc_relative_entropy();
$EmpDiv_rate = "Infinity";
$EmpDiv_rate = $EmpDiv/$string_length unless ($EmpDiv eq "Infinity");

$OutputFileName = $InferredNamesBase . "_empirical_errors";
open(OUT, ">$OutputFileName") || die "Can't write to $OutputFileName: $!\n";
print OUT "Empirical Divergence: $EmpDiv bits\n";
print OUT "Empirical Divergence Rate: $EmpDiv_rate bits per symbol\n";
print OUT "Empirical Variation: $EmpVar\n";
close(OUT) || die "Can't close $OutputFileName: $!\n";


# HERE ENDETH THE PROGRAM

sub log_2 {
    # Input is a positive real number
    # Output is its base-two logarithm
    return(log($_[0])/log(2.0));
}



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
	$WordInf = &inferred_string_prob($string);
	$WordEmp = empirical_string_prob($string);

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
	$EmpiricalVariation += abs($WordEmp - $WordInf);
    }
    @Values = ($EmpiricalDivergence,$EmpiricalVariation);
    return(@Values);
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

sub inferred_string_prob {
    # Calculate the probability of a string under the inferred machine
    my($string,$prob,$i,$CurrentState,$word_length);
    $string = $_[0];
    $word_length = length($string);

    foreach $Inferred (@InferredStatesList) {
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
	$prob += $StringCondInferred * $InferredStateProb{$Inferred};
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


