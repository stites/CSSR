all: main.o hash.o states.o allStates.o parsetree.o g_array.o hash2.o machine.o transtable.o test.o
	c++ -O -o CSSR main.o hash.o states.o allStates.o parsetree.o g_array.o hash2.o machine.o transtable.o test.o

main.o:	Main.cpp Main.h Common.h AllStates.h Machine.h
	c++ -O -c -o main.o Main.cpp

hash.o: Hash.cpp Hash.h States.h Common.h States.cpp
	c++ -O -c -o hash.o Hash.cpp

states.o: States.cpp States.h Common.h ParseTree.h G_Array.h Hash.h
	c++ -O -c -o states.o States.cpp

allStates.o: AllStates.cpp AllStates.h States.h Common.h ParseTree.h G_Array.h
	c++ -O -c -o allStates.o AllStates.cpp

parsetree.o: ParseTree.cpp ParseTree.h Common.h G_Array.h
	c++ -O -c -o parsetree.o ParseTree.cpp

g_array.o: G_Array.cpp G_Array.h Common.h
	c++ -O -c -o g_array.o G_Array.cpp

hash2.o: Hash2.cpp Hash2.h Common.h
	c++ -O -c -o hash2.o Hash2.cpp

machine.o: Machine.cpp Machine.h States.h Common.h Hash2.h Hash.h
	c++ -O -c -o machine.o Machine.cpp

transtable.o: TransTable.cpp TransTable.h Common.h States.h
	c++ -O -c -o transtable.o TransTable.cpp

test.o: Common.h Test.h
	c++ -O -c -o test.o Test.cpp