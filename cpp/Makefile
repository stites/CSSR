.PHONY: all clean cleanobj

CC          = g++
CFLAGS      = -Wextra -Wall -O2
CFLAGS_MAIN = -g $(CFLAGS)
MAIN        = CSSR
SOURCES     = main.o hash.o states.o allStates.o parsetree.o g_array.o hash2.o machine.o transtable.o test.o

# Header files
STATE_H     = States.h Hash.h Common.h
GARRAY_H    = G_Array.h Common.h
HASH2_H     = Hash2.h Common.h
PARSETREE_H = ParseTree.h $(GARRAY_H)

all: $(MAIN)

$(MAIN): $(SOURCES)
	$(CC) $(CFLAGS_MAIN) -o $@ $(SOURCES)

clean: cleanobj
	rm $(MAIN)

cleanobj:
	rm *.o

main.o: Main.cpp Main.h Common.h AllStates.h Machine.h
	$(CC) $(CFLAGS) -c -o $@ $<

hash.o: Hash.cpp States.cpp $(STATE_H)
	$(CC) $(CFLAGS) -c -o $@ $<

states.o: States.cpp $(PARSETREE_H) $(STATE_H)
	$(CC) $(CFLAGS) -c -o $@ $<

# no Hash.h required
allStates.o: AllStates.cpp AllStates.h $(PARSETREE_H) $(STATE_H)
	$(CC) $(CFLAGS) -c -o $@ $<

parsetree.o: ParseTree.cpp $(PARSETREE_H)
	$(CC) $(CFLAGS) -c -o $@ $<

g_array.o: G_Array.cpp $(GARRAY_H)
	$(CC) $(CFLAGS) -c -o $@ $<

hash2.o: Hash2.cpp $(HASH2_H)
	$(CC) $(CFLAGS) -c -o $@ $<

machine.o: Machine.cpp Machine.h $(HASH2_H) $(STATE_H)
	$(CC) $(CFLAGS) -c -o $@ $<

# no Hash.h required
transtable.o: TransTable.cpp TransTable.h $(STATE_H)
	$(CC) $(CFLAGS) -c -o $@ $<

test.o: Test.cpp Test.h Common.h
	$(CC) $(CFLAGS) -c -o $@ $<

