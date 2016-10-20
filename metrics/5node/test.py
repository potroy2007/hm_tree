import itertools

perms = list(itertools.permutations(["CMP", "EryA", "GMP", "MEP"]))

trees = ["((({},{}){}){})MPP;".format(*x) for x in perms]
trees += ["((({}){},{}){})MPP;".format(*x) for x in perms]
trees += ["((({}){}){},{})MPP;".format(*x) for x in perms]
trees += ["(({},{}){},{})MPP;".format(*x) for x in perms]
trees += ["(({}){},({}){})MPP;".format(*x) for x in perms]

print(len(trees))

with open ("test.tre", "w+") as o:
    for t in trees:
        o.write(t + "\n")
        
