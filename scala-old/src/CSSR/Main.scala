package CSSR

import CSSR.parse.Parse

object Main {
  def main(
            alphabet:String,
            dataSource:String,
            maxLength:Integer,
            sigLevel:Int = 0.5,
            useMultiline:Boolean = false, // unused
            useChiSquared:Boolean = false // unused
          ): Unit = {
    val parsetree:Parse = Parse(alphabet, dataSource)
  }
}

class Main {
  int max_length;
  char* data_file;
  char* alpha_file;
  HashTable2 *alphaHash;
  bool isMulti = false;
  bool stateRemoved = false; //dummy
  Machine* machine;
  double sigLevel = SIGLEVEL;
  bool isSigLevel = false;
  bool isChi = false;

  // read in info from command line
  // check for proper arguments
  if(argc !=4 )
  {
    if (argc == 5)
      FiveArgs(argv, isMulti, isSigLevel, sigLevel, isChi);
    else if (argc == 6)
      SixArgs(argv, isMulti, isSigLevel, sigLevel, isChi);
    else if (argc == 7)
      SevenArgs(argv, isMulti, isSigLevel, sigLevel, isChi);
    else
      PrintError();
  }

  PrintCopyrightInfo();

  //set arguments
  max_length = atoi(argv[3]);
  data_file = argv[2];
  alpha_file = argv[1];

  // if no significance level is set, use default
  // (should be set already, just to be careful)
  if(!isSigLevel)
    sigLevel = SIGLEVEL;
  else
    cout << "Significance level set to " << sigLevel <<".\n";

  //create parse tree to store all strings in data
  ParseTree parsetree(max_length);

  //if using multi-line input, read in data and enter
  //tree one line at a time
  if(isMulti)
  {
    parsetree.ReadProcessMultiLine(alpha_file, data_file);
    cout << "Multi-line option is set.\n"
    << "Max line length is "<< MAX_LINE_SIZE
    << "\n";
  }

  //otherwise do data read first, then enter in tree
  else
  {
    //read in data and alphabet from files
    parsetree.ReadInput(alpha_file, data_file);
    //enter data in tree
    parsetree.FillTree();
  }

  //make hash table of alpha symbols and indices
  alphaHash = parsetree.MakeAlphaHash();

  //create array of states
  AllStates allstates(parsetree.getAlphaSize(), sigLevel, isChi);

  //calculate frequency of occurence of symbols
  allstates.InitialFrequencies(parsetree);

  //check all possible strings up to max
  //length and compare distributions
  for(int k = 1; k <= max_length; k++)
  allstates.CalcNewDist(k, parsetree);

  //remove shorter strings
  stateRemoved = allstates.DestroyShortHists(max_length, parsetree);

  //remove all non-recurring states
  allstates.CheckConnComponents(parsetree);

  //check futures longer than 1,
  //by using determinism of states
  allstates.Determinize(parsetree);

  //remove all non-recurring states (again, since there may be new ones)
  allstates.CheckConnComponents(parsetree);

  //store transitions from state to state
  allstates.StoreTransitions(parsetree.getMaxLength(), parsetree.getAlpha());

  //calculate distribution/frequency of states
  allstates.GetStateDistsMulti(parsetree, data_file, alphaHash, isMulti);

  //calculate information values
  machine = new Machine(&allstates);
  machine->CalcRelEnt(parsetree, alphaHash, isMulti);
  machine->CalcRelEntRate(parsetree, alphaHash, isMulti);
  machine->CalcCmu();
  machine->CalcEntRate();
  machine->CalcVariation(parsetree, alphaHash, isMulti);

  //print out states
  allstates.PrintOut(data_file, parsetree.getAlpha());

  //print out machine and calculationsf
  machine->PrintOut(data_file, alpha_file, data_file, max_length, sigLevel, isMulti, isChi, parsetree.getAlphaSize());
  machine->PrintDot(data_file, parsetree.getAlpha());

  delete machine;
  return 1;
}


