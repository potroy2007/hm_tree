import itertools

perms = list(itertools.permutations(["MPP", "CMP", "CLP", "GMP"]))

trees = ["(({}){},{}){};".format(*x) for x in perms]

with open ("4nodetrees.tre", "w+") as o1:
    for t in trees:
        o1.write(t + "\n")
