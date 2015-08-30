/////////////////////////////////////////////////////////////////////////////
//Title:	Main.cpp (for program CSSR)
//Author:	Kristina Klinkner
//Date:		July 23, 2003
//Description:	Creates separate causal states for each history of data
//		with a singular probability distribution.  History length
//		increases incrementally until cutoff point is reached.  Then
//              removes transient states, determinizes remaining states, and
//              calculates various metrics for the resulting state machine.
//              Outputs a file of states, a file of state sequences, a dot
//              file, and an information file with the metrics.
//
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
//
//    Copyright (C) 2002 Kristina Klinkner
//    This file is part of CSSR
//
//    CSSR is free software; you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation; either version 2 of the License, or
//    (at your option) any later version.
//
//    CSSR is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with CSSR; if not, write to the Free Software
//    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//////////////////////////////////////////////////////////////////////////////


#include "Main.h"

///////////////////////////////////////////////////////////////////////////////

int main(int argc, char *argv[])
  //argv[1] is Alphabet File, 
  //argv[2] is Datafile, 
  //argv[3] is maximum  length of string
  //argv[4] is the (optional) flag for changing the significance level
  //argv[5] is multiline version, if used
  // argv[6] is use of chi-squared test (optional)
{
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

  //read in info from command line
  //check for proper arguments
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

  //if no significance level is set, use default
  //(should be set already, just to be careful)
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
