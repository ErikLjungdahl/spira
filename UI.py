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

	# Prints initial game plan
	line = open("log.txt").readlines()[1]
	dicti = line_to_coord(line)
	print_board(dicti)
	
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

	p.stdout.close()
	p.wait()



def create_board(line_pointer):
	print("")

	line = open("log.txt").readlines()

	for i,l in enumerate (line[line_pointer::]):
		if(re.match('---- {\(stage play\)',l)) : 
			dic = line_to_coord(l)
			print_board(dic)
			line_pointer += i+1

	print("")

	return line_pointer


def print_board(dic_of_positions):
	#print(dic_of_positions)
	xMax, yMax = get_boardsize(dic_of_positions)
	mat = [["_" for j in range(yMax+1)] for i in range(xMax+1)]
	for key,val in dic_of_positions.items():
		if key != "free":
			for x,y in val:
				mat[x][y] = key[0].upper()
	print_matrix(mat)


def get_boardsize(dic):
	xMax, yMax = 0, 0
	for key,val in dic.items():
		for x,y in val:
			xMax = x if xMax < x else xMax
			yMax = y if yMax < y else yMax
	return xMax, yMax


	
def print_matrix(mat):
	for row in reversed(mat):
		r = "| "
		for elem in row:
			r = r+elem+" "
		r += "|"
		print (r)
			



def line_to_coord(line):
	list_lines = line.split(",")
	positions = {}

	for l in list_lines:
		line = clean_numbers(l).replace("}","").replace("{","")
		line = line.split(" ")[1:] #Removes the blank spot at start
		key = line[0]
		if(key == "occupied" or key == "tile"):
			player = line[1]
			xPos = int(line[-2])
			yPos = int(line[-1])
			if player in positions:
				positions[player].append( (xPos,yPos) )
			else:
				positions[player] = [(xPos,yPos)]
		elif(key == "free"):
			xPos = int(line[-2])
			yPos = int(line[-1])
			if key in positions:
				positions[key].append( (xPos,yPos) )
			else:
				positions[key] = [(xPos,yPos)]
	return positions


def clean_numbers(line):
	tmp = re.sub(r"\([sz].*?\)", lambda m: str(m.group().count("s")), line)
	t = tmp.replace(")","").replace("(","").replace(" z"," 0")
	return t


# Modifies the (s (s z)) -> int.
# Also gives the names to each kolumn/parameter/dont know what it is called
def modify(line, dic):
	t = clean_numbers(line)
	list_t = t.split(" ")
	if not re.match(r'0',list_t[0]):
		dic_list = dic.get(list_t[1])
		st = list_t[0] + " "
		for i,elem in enumerate(list_t[2:]):
			#print(i, dic_list)
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
				#print("cdm row:",row)
				list_row = row.split(" ")[1:]
				dic[list_row[0]] = list_row[1:]
				#print("cdm dic:",dic)
	return dic

main()

#line = "---- {(stage play), (turn bob), (occupied alice (s z) z), (occupied bob (s z) (s z)), (occupied alice z (s z)), (occupied bob z (s (s z))), (occupied alice z z), (free (s (s z)) (s (s z))), (free (s (s z)) (s z)), (free (s (s z)) z), (free (s z) (s (s z)))}"
#dic = line_to_coord(line)
#print_board(dic)
