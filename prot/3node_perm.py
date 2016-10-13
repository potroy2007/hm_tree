import itertools

perms = list(itertools.permutations(["MPP", "CMP", "CLP"]))

trees = list()

for e in perms:
    trees.append("({},{}){};".format(*e))

with open ("trees.tre", "w+") as o:
    for t in trees:
        o.write(t + "\n")
