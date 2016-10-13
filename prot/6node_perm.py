import itertools

perms = list(itertools.permutations(["MPP", "CMP", "CLP", "GMP", "MEP", "CD8"]))

trees = ["((({}){}){},({}){}){};".format(*x) for x in perms]
trees += ["(({},{}){},({}){}){};".format(*x) for x in perms]

with open ("6nodetrees.tre", "w+") as o:
    for t in trees:
        o.write(t + "\n")
