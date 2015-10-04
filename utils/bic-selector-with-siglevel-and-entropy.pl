#!/usr/bin/perl

# Script to select a history length and significance level for CSSR using BIC


####################### INCOMPLETE ####################


# Invokes the likelihood-under-CSM.pl script, and, of course, CSSR

#### Arguments (in required order):
# --verbose [optional; flag; print extra diagnostics while running]
# --entropy [optional; flag; estimate entropy rate to improve history range]
# --siglevel [optional; flag; sweep significance levels]
# min_sig [optional; real; lowest significance level to check]
# max_sig [optional; real; highest significance level to check]
# sig_steps [optional; integer; number of signifiance levels to check]
# alpha_file [required; name of alphabet file]
# data_file [required; name of data file]


### Explanations
# The basic operation is to run CSSR at multiple history lengths, calculating
# the BIC score at each, and select the machine with the best BIC.  The
# selected model is renamed to indicated that it was picked, and the losers
# are moved to a new directory for safe-keeping.
# With --siglevel, it loops over significance levels from large to small,
# reducing the level by the same factor at each step.
# The --entropy flag changes the way the maximum checked history length is set.
# This is always done using the Marton-Shields formula.  The default behavior
# is to use the alphabet size as a conservative over-estimate of the entropy
# rate.  With the --entropy flag, the entropy rate estimate from L=1 (at the
# first significance level checked, if applicable) is used.  This should be a
# bit more optimistic and adaptive.

# If the --verbose flag is set, prints out tables giving no. states,
# likelihood, BIC and AIC vs. history length, and a verbal message on the
# winning length and significance level and its BIC.

# AIC is calculated but not, currently, used (except in that table).
# Experience suggest that BIC tends to favor the right structure by a larger
# margin than does AIC, and so should be more reliable.  Option to select by
# AIC (or other IC?) may be added later.

### Details on calculates
# The BIC and AIC penalties depend on the number of parameters, call that k.
# The BIC penalty is (1/2)k log N, where N is the number of data points.
# The AIC penalty is just k.
# For a fixed CSM structure, each state has a distinct probability of emitting
# each symbol; these probabilities must sum to one for each state.
# So, for a _fixed_ structure, k = (# states) * (#symbols - 1).
# This is what is used in the calculations below.
# This is theoretically somewhat dubious, because of course CSSR can infer many
# introducing extra degrees of freedom not accounted for here.  It's hard to
# translate this into a number of parameters, though.


# CHANGES TO MAKE:
# Different choices for maximum history length
#     - manually
#     - running at L=1 to get entropy rate estimate, Marton-Shields
# Default alphabet file
# Issue warnings if CSSR dies, can't synchronize, etc.
# Improve file renaming, now done "by hand" multiple times --- subroutine?
# Option to select by AIC, maximum likelihood, others...
# Options re handling of unselected files: delete, change destination,
#    compress, some combination


use strict; # To avoid bugs due to mis-spelling variable, require declarations
use Perl6::Slurp; # To read whole files into strings


my $debug=0;
my $verbose = 0;
my $min_alpha = 0.0001;
my $max_alpha = 0.0001;
my $alpha_steps = 1;
my $alpha_factor = 1;
my $entropy = 0;

# Print usage message if the # of parameters is wrong
my $usage = "--verbose [--entropy] [--alpha min max steps] <alphabet file> <data file>";
if ((@ARGV==3) || (@ARGV==7)) {
  if (shift(@ARGV) eq "--verbose") {
    $verbose = 1;
  } else {
    die "Unrecognized option.\nUsage: $0 $usage\n";
  }
}
if (@ARGV==6) {
  if (shift(@ARGV) eq "--alpha") {
    $min_alpha = shift(@ARGV);
    $max_alpha = shift(@ARGV);
    $alpha_steps = shift(@ARGV);
    if ($min_alpha > $max_alpha) {
      die "Minimum sig. level must be =< maximum.\nUsage: $0 $usage\n";
    }
  }
  else {
    die "Unrecognized option.\nUsage: $0 $usage\n";
  }
}
die "Usage: $0 $usage\n" unless (@ARGV==2);
my $alphabetfile = shift(@ARGV);
my $datafile = shift(@ARGV);

# Declare the arrays up here, so that they're globally accessible
my @loglike;
my @num_states;
my @bic_scores;
my @aic_scores;

# Get the alphabet size, needed to compute effective parameter numbers
my $alphabet = length(slurp($alphabetfile)) -1;
printf stderr ("%d\n",$alphabet) if ($debug);

# Get the data size
my $datalength = length(slurp($datafile)) -1;
printf stderr ("%d\n", $datalength) if ($debug);

# Calculate maximum history length
# Marton and Shields: nonparametric probability estimates over sequences
# of increasing length are only consistent if the string-length is
# bounded above by log(N)/h, h being entropy rate.  Our history length =
# string length - 1.  Below, uses log(alphabet) as an estimate of the entropy
# rate, which will almost always be too high
my $max_history_length = log($datalength)/log($alphabet) - 1;

# Length at which best score is obtained
my $best_bic_length = 1;
my $best_aic_length = 1;
# alphas at which the best score is obtained
my $best_bic_alpha = $max_alpha;
my $best_aic_alpha = $max_alpha;
# Actual best score to date
# Loglikelihood for a discrete model is the log of a probability, so < 0,
# so the penalized loglikelihood is also < 0.  I give these initial
# _positive_ values so make it clear that they haven't been initialized
# yet.
my $best_bic = 1;
my $best_aic = 1;
# Define these out here so that they can be accessed later


# Loop CSSR over alpha values
# This proceeds in steps of equal logarithmic size from the max down to the min
$alpha_factor = ($min_alpha/$max_alpha)**(1/$alpha_steps);
print("Going from $min_alpha to $max_alpha in $alpha_steps multiplicative steps of $alpha_factor\n") if ($verbose);

for (my $alpha = $max_alpha; $alpha >= $min_alpha; $alpha *= $alpha_factor) {
  # Loop CSSR over history lengths
  for (my $L = 1; $L <= $max_history_length; $L++) {
    # Run CSSR
    system("CSSR $alphabetfile $datafile $L -s $alpha");
    # Should really check here that it ran properly CSSR's output files all
    # take the datefile's name as their base, we're going to want to compare
    # different values of L and alpha so we'll rename the outputs to indicate
    # this control setting.  We may also want to compare the state sequence
    # files, so let's rename and save them, too; and why not the .dot file
    # while we're at it?
    my $machine_results_file = $datafile . "_results";
    my $labeled_machine = $datafile . "_alpha-$alpha". "_L-$L";
    my $machine_info_file = $datafile . "_info";
    my $labeled_info_file = $datafile . "_alpha-$alpha". "_L-$L" . "_info";
    my $stateseq_file = $datafile . "_state_series";
    my $dot_file = $datafile . "_inf.dot";
    rename($machine_results_file,"$labeled_machine" . "_results") || die "Couldn't move results file: $!";
    rename($machine_info_file,$labeled_info_file) || die "Couldn't move info file: $!";
    rename($stateseq_file,"$labeled_machine" . "_state_series") || die "Couldn't move state series file: $!";
    rename($dot_file, "$labeled_machine" . "_inf.dot") || die "Couldn't move dot file: $!";
    # Read the whole info file into a string
    my $current_info = slurp $labeled_info_file;
    # Declare these variables here for later accessibility
    my $bic_correction;
    my $aic_correction;

    # Check to see if the CSSR had the "observed string is impossible" problem,
    # if so don't bother calculating a likelihood
    if ($current_info =~ m/Relative Entropy:\sinf/) {
      undef $loglike[$L];
    } else {
      # Use `` to capture the output of the system call
      $loglike[$L] = `likelihood-under-CSM.pl -log $labeled_machine $datafile`;
      # Extract the number of states from the info file
      if ($current_info =~ m/Number of Inferred States:\s*(\d+)/) {
	$num_states[$L] = $1;
      } else {
	print STDERR "$current_info\n";
	die "Can't get the number of states from string matching!\n";
      }
      # Calculate BIC correction factor.  See top of file for explanation
      my $eff_parameters = $num_states[$L] * ($alphabet - 1);
      $bic_correction = 0.5* $eff_parameters * log($datalength);
      $aic_correction =  $eff_parameters;
      $bic_scores[$L] = $loglike[$L] - $bic_correction;
      $aic_scores[$L] = $loglike[$L] - $aic_correction;
      # Check whether the current BIC score beats the best to date
      # Real BIC scores are negative, so a positive value for the best yet means
      # it hasn't been initialized yet
      if (($best_bic > 0) || ($bic_scores[$L] > $best_bic)) {
	$best_bic_length = $L;
	$best_bic_alpha = $alpha;
	$best_bic = $bic_scores[$L];
	print "Best BIC now $best_bic at length $best_bic_length\n" if $debug;
      }
      # Similarly for AIC
      if (($best_aic > 0) || ($aic_scores[$L] > $best_aic)) {
	$best_aic_length = $L;
	$best_aic_alpha = $alpha;
	$best_aic = $aic_scores[$L];
	print "Best AIC now $best_aic at length $best_aic_length\n" if $debug;
      }

    } 
    # Print L, number of states, log likelihood, BIC, AIC
    printf("%8f %5d %5d %8.2f %8.2f %8.2f\n", $alpha, $L, $num_states[$L], $loglike[$L], $bic_scores[$L], $aic_scores[$L]) if ($verbose && (defined $loglike[$L]));
  }
}

print "Best BIC score was $best_bic, obtained at length $best_bic_length and significance level $best_bic_alpha\n" if ($debug || $verbose);

# Rename the selected machine to something indicating it was picked by BIC,
# and suppress the history length used (since that will facilitate uniform
# reference to the files)
# Change over everything --- results, info, state series (needed for MI
# calculations) and dot file (why not?)

my $from_name =  $datafile . "_alpha-$best_bic_alpha" . "_L-$best_bic_length";
my $to_name = $datafile . "_BIC_picked";

rename($from_name . "_results", $to_name . "_results") || die "Couldn't move results file: $!";
rename($from_name . "_info", $to_name . "_info") || die "Couldn't move info file: $!";
rename($from_name . "_state_series", $to_name . "_state_series") || die "Couldn't move state series file: $!";
rename($from_name . "_inf.dot", $to_name . "_inf.dot") || die "Couldn't move dot file: $!";

# clean-up of files from non-selected machines by moving them into a
# new directory
my $storage_directory = $datafile . "_unselected";
mkdir $storage_directory || die "Can't make directory $storage_directory: $!\n";
for (my $alpha = $max_alpha; $alpha >= $min_alpha; $alpha *= $alpha_factor) {
  for (my $L = 1; $L <= $max_history_length; $L++) {
    my $labeled_machine = $datafile . "_alpha-$alpha". "_L-$L";
    unless (($L == $best_bic_length) && ($alpha == $best_bic_alpha)) {
      rename ($labeled_machine . "_results", $storage_directory . "/". $labeled_machine . "_results") || die "Couldn't move results file to storage: $!\n" ;
      rename ($labeled_machine . "_info", $storage_directory . "/". $labeled_machine . "_info") || die "Couldn't move info file to storage: $!\n" ;
      rename ($labeled_machine . "_state_series", $storage_directory . "/". $labeled_machine . "_state_series") || die "Couldn't move state series file to storage: $!\n" ;
      rename ($labeled_machine . "_inf.dot", $storage_directory . "/". $labeled_machine . "_inf.dot") || die "Couldn't move dot file to storage: $!\n" ;
    }
  }
}
