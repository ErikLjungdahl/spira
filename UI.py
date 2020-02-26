import subprocess
import re
import sys

def main():
	is_start = True
	is_finished = False
	fp_ceptre = sys.argv[1]
	fp_game = sys.argv[2]
	cmd = [fp_ceptre, fp_game]
	p = subprocess.Popen(cmd, stdout=subprocess.PIPE, bufsize=1)
	for line in iter(p.stdout.readline, ""):

		# For removing the starting rows
		if(line == "#trace ...\n") :
			is_start = False

		# Removes the ?- line
		elif(re.match('\?-',line)) :
			print("")

		# Checks for the winner and print the name + won
		elif(re.match('\{qui',line)):
			is_finished = True
			print(re.search(r"(?<=\bwin\s)(\w+)", line).group() + " won\n")

		elif(re.match(r'\d*: ',line)):
			print(modify(line).rstrip())

		# Prints the line as it is but remove start and end
		elif( dont_remove(line) and not is_start and not is_finished) : 
			print(line.rstrip())
			
	p.stdout.close()
	p.wait()

# Modifies the (s (s z)) -> int
def modify(line):
	tmp = re.sub(r"\([sz].*?\)", lambda m: str(m.group().count("s")), line)
	return tmp.replace(")", "").replace(" z"," 0")

# Where we add everything case we wont write out the line
def dont_remove(line):
	if (re.match(r"Final",line)):
		return False
	else : 
		return True

main()
