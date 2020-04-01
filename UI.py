import subprocess
import re
import sys

def main():
	is_start = True
	is_finished = False
	fp_ceptre = sys.argv[1]
	fp_game = sys.argv[2]
	dic = create_dict_move(fp_game)
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
		elif re.match(r'{qui',line):
			#print(re.search(r"(x?<=\bwin\s)(\w+)", line).group() + " won\n")
			print(line)
			is_finished = True
		# changes the (s (s (s z))) + gives name to parameters
		elif re.match(r'\d*: ',line):
			print(modify(line, dic).rstrip())
		# Prints the line as it is but remove start and end
		elif keep(line) and not is_start and not is_finished:
			print(line.rstrip())
	p.stdout.close()
	p.wait()

# Modifies the (s (s z)) -> int.
# Also gives the names to each kolumn/parameter/dont know what it is called
def modify(line, dic):
	tmp = re.sub(r"\([sz].*?\)", lambda m: str(m.group().count("s")), line)
	t = tmp.replace(")","").replace("(","").replace(" z"," 0")
	list_t = t.split(" ")
	list_t[1] = list_t[1].split("/")[0]
	#print(list_t)
	dic_list = dic.get(list_t[1])
	st = list_t[0] + " "
	for i,elem in enumerate(list_t[2:]):
		kol_name = dic_list[i]
		if not kol_name == "_":
			st = st + kol_name + ": " + elem + "  "
	return st

# Add every case we don't write out.
def keep(line):
	if re.match(r"Trace:",line):
		return True
	return False

# creates a dict on all interactive plays that exist
def create_dict_move(filepath):
	dic = {}
	with open(filepath) as f:
		for row in f:
			row = row.strip()
			if(re.match(r'%%', row)):
				print(row)
				list_row = row.split(" ")[1:]
				dic[list_row[0]] = list_row[1:]
				print(dic)
	return dic

main()
