import sys

START = "*t1"
END = "t1*"

with open(sys.argv[1]) as file:
    printing = False
    for L in file:
        if START in L:
            printing = True
            L = L.replace(START, '')
        if END in L:
            printing = False
            print("...\n")
        if printing:
            print(L, end='')
