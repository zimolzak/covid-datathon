from sys import argv
fn = argv[1]
h = {}
with open(fn, 'r') as fh:
    for line in fh:
        n_commas = line.count(',')
        if n_commas in h:
            h[n_commas] += 1
        else:
            h[n_commas] = 1
print(h)
