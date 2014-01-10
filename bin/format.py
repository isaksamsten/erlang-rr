import sys
attributes = {}
inputs = []
outputs = None
header = True
lines = []


def handle_attr(attr):
    split = attr.split()
    attributes[split[1]] = "numeric" if attr.endswith("]") else "categoric"


def handle_in(i):
    global inputs
    split = i.replace(", ", ",").split()
    inputs = split[1].split(",")


def handle_outputs(o):
    global outputs
    outputs = o.split()[1]

    
def assemble_header(o):
    global attributes
    o.write(",".join(map(lambda x: attributes[x], inputs))+",class\r\n")
    o.write(",".join(inputs) + "," + outputs + "\r\n")

with open(sys.argv[1]) as f:
    print "***", sys.argv[1], "***"
    o = open(sys.argv[2], "w")
    for line in f.readlines():
        if header:
            line = line.strip()
            if line.startswith("@relation"):
                continue;
            elif line.startswith("@attribute"):
                handle_attr(line)
            elif line.startswith("@inputs"):
                handle_in(line)
            elif line.startswith("@output") or line.startswith("@outputs"):
                handle_outputs(line)
            elif line.startswith("@data"):
                assemble_header(o)
                header = False
        else:
            o.write(line.replace("<null>", "?").strip() + "\r\n")
