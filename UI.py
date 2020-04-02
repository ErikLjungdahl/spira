import subprocess
import re
import sys
from sys import stdin
import os

def main():
	is_start = True
	is_finished = False
	is_time_to_choose = True
	fp_ceptre = sys.argv[1]
	fp_game = sys.argv[2]
	line_pointer = 0
	dic = create_dict_move(fp_game)
	cmd = [fp_ceptre, fp_game]
	p = subprocess.Popen(cmd, stdout=subprocess.PIPE)
	print("BEFORE LOOP")
	for line in iter(p.stdout.readline, ""):
		# For removing the starting rows
		if line == "#trace ...\n":
			is_start = False
		# Removes the ?- line
		elif re.match(r'\?-',line) and is_time_to_choose:
			print(line.rstrip()+"\n")
			is_time_to_choose = False
			line_pointer = create_board(line_pointer)
		# Checks for the winner and print the name + won
		elif re.match(r'{qui',line):
			#print(re.search(r"(x?<=\bwin\s)(\w+)", line).group() + " won\n")
			print(line)
			is_finished = True
		# changes the (s (s (s z))) + gives name to parameters
		elif re.match(r'\d*: ',line):
			print(modify(line, dic).rstrip())
			is_time_to_choose = True
		# Prints the line as it is but remove start and end
		elif keep(line) and not is_start and not is_finished:
			print(line.rstrip())
		
	print("BEFORE CLOSE")
	p.stdout.close()
	p.wait()



def create_board(line_pointer, ):
	print("----------------------------")

	line = open("log.txt").readlines()

	for i,l in enumerate (line[line_pointer::]):
		if(re.match('---- {\(stage play\)',l)) : 
			print(i, l)
			line_pointer += i+1

	print("---------------------------- line_pointer",line_pointer)

	return line_pointer









# Modifies the (s (s z)) -> int.
# Also gives the names to each kolumn/parameter/dont know what it is called
def modify(line, dic):
	tmp = re.sub(r"\([sz].*?\)", lambda m: str(m.group().count("s")), line)
	t = tmp.replace(")","").replace("(","").replace(" z"," 0")
	list_t = t.split(" ")
	if not re.match(r'0',list_t[0]):
		dic_list = dic.get(list_t[1])
		st = list_t[0] + " "
		for i,elem in enumerate(list_t[2:]):
			kol_name = dic_list[i]
			if not kol_name == "_":
				st = st + kol_name + ": " + elem + "  "
		return st
	return "0:"

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
				print("cdm row:",row)
				list_row = row.split(" ")[1:]
				dic[list_row[0]] = list_row[1:]
				print("cdm dic:",dic)
	return dic

main()
