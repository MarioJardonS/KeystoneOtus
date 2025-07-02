import sys

file = sys.argv[1]
newfile = "corrected" + file

file = open(file)
newfile = open(newfile , "a")

for line in file:
    newline = ""
    for i in range(len(line)):
        if line[i] == " ":
            newline = newline + "_"
        else:
            newline = newline + line[i]
        
        
    newfile.write(newline)
    
file.close()    
newfile.close()

