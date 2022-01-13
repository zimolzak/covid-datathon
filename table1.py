import sys

with open(sys.argv[1]) as file:
    printing = False
    for L in file:
        if "*t1" in L:
            printing = True
        if "t1*" in L:
            printing = False
            print(L)
        if printing:
            print(L, end='')
