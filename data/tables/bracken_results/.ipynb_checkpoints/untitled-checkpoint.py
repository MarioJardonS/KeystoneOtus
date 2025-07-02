import sys

file = sys.argv[1]
newfile = "corrected" + file

file = open(file)
newfile = open(newfile , "a")

for line in file:
    i = 0
    newline = ""
    while int(line[i]) == FALSE:
        if line[i] == " ":
            newline = newline + "_"
        else:
            newline = newline + line[i]
        
        i = i + 1
        
    newline = newline + line[i:]
    
file.close()    
newfile.close()

