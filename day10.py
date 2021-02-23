with open("input10.txt", "r") as fp:
    lines = [int(line.rstrip()) for line in fp.readlines()]

sol = {0:1}
for line in sorted(lines):
    sol[line] = 0
    if line - 1 in sol:
        sol[line]+=sol[line-1]
    if line - 2 in sol:
        sol[line]+=sol[line-2]
    if line - 3 in sol:
        sol[line]+=sol[line-3]

print(sol[max(lines)])
