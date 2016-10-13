import itertools

perms = list(itertools.permutations(["MPP", "CMP", "CLP", "GMP", "MEP"]))

trees = ["(({}){},({}){}){};".format(*x) for x in perms]
trees += ["(({},{}){},{}){};".format(*x) for x in perms]

with open ("5nodetrees.tre", "w+") as o:
    for t in trees:
        o.write(t + "\n")
