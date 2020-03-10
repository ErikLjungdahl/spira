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
		if line == "#trace ...\n":
			is_start = False
		# Removes the ?- line
		elif re.match(r'\?-',line):
			print(line.rstrip()+"\n")
		# Checks for the winner and print the name + won
		elif re.match(r'\{qui',line):
			print(re.search(r"(?<=\bwin\s)(\w+)", line).group() + " won\n")
			print(line)
			is_finished = True
		# changes the (s (s (s z)))
		elif re.match(r'\d*: ',line):
			print(modify(line).rstrip())
		# Prints the line as it is but remove start and end
		elif keep(line) and not is_start and not is_finished:
			print(line.rstrip())
	p.stdout.close()
	p.wait()

# Modifies the (s (s z)) -> int.
def modify(line):
	tmp = re.sub(r"\([sz].*?\)", lambda m: str(m.group().count("s")), line)
	return tmp.replace(r')', "").replace(r'\sz'," 0")

# Add every case we don't write out.
def keep(line):
	if re.match(r"Trace:",line):
		return True
    return False

main()
