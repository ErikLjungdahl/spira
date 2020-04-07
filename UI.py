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
	open("log.txt", "w").close()
	p = subprocess.Popen(cmd, stdout=subprocess.PIPE)
	for line in iter(p.stdout.readline, ""):
		# For removing the starting rows
		if line == "#trace ...\n":
			create_initial_board(dic)
			is_start = False
		# Removes the ?- line
		elif re.match(r'\?-',line) and is_time_to_choose:
			print(line.rstrip()+"\n")
			line_pointer = create_board(line_pointer, dic, "stage play")
		# Checks for the winner and print the name + won
		elif re.match(r'{qui',line):
			#print(re.search(r"(x?<=\bwin\s)(\w+)", line).group() + " won\n")
			line_pointer = create_board(line_pointer, dic, "stage win")
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


# pretoken_play
def create_initial_board(dic):
	file = open("log.txt")
	line = file.readlines()[1]
	print("")
	dic = line_to_coord(line,dic)
	print_board(dic)
	print("")

def create_board(line_pointer, dic, match):
	line = open("log.txt").readlines()
	print("")
	for i,l in enumerate (line[line_pointer::]):
		
		if(re.match('---- {\('+match,l)) : 
			dic = line_to_coord(l,dic)
			print_board(dic)
			line_pointer += i+1

	print("")
	return line_pointer



def print_board(dic_of_positions):
	xMax, yMax = get_boardsize(dic_of_positions)
	mat = [["_" for j in range(yMax+1)] for i in range(xMax+1)]
	#print(dic_of_positions)
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
	rez = [[mat[j][i] for j in range(len(mat))] for i in range(len(mat[0]))]
	size = len(rez)-1
	#print("   " + "__"*(size+1))
	for i,row in enumerate(reversed(rez)):
		r = str(size)
		size = size - 1
		for elem in row:
			r = r + "|" + elem
		r += "|"
		print (r)

	last_row = " "
	size = len(rez[0])
	for i in range(size):
		last_row = last_row + " " + str(i)
	print(last_row)



def line_to_coord(line, dic):
	list_lines = line.split(",")
	positions = {}

	for l in list_lines:
		line = clean_numbers(l).replace("}","").replace("{","")
		line = line.split(" ")[1:] #Removes the blank spot at start
		key = line[0]
		if(key == "occupied" or key == "tile"):
			get_xy(line, dic)
			player = line[1]
			if not player == "free" :
				player = line[2]
			xPos = int(line[-2])
			yPos = int(line[-1])
			if player in positions:
				positions[player].append( (xPos,yPos) )
			else:
				positions[player] = [(xPos,yPos)]
		elif(key == "free"):
			get_xy(line, dic)
			xPos = int(line[-2])
			yPos = int(line[-1])
			if key in positions:
				positions[key].append( (xPos,yPos) )
			else:
				positions[key] = [(xPos,yPos)]
	
	return positions


def get_xy(line, dic):
	x, y = 0, 0

	return x,y



def clean_numbers(line):
	tmp = re.sub(r"\([sz].*?\)", lambda m: str(m.group().count("s")), line)
	t = tmp.replace(")","").replace("(","").replace(" z"," 0")
	return t


# Modifies the (s (s z)) -> int.
# Also gives the names to each kolumn/parameter/dont know what it is called
def modify(line, dic):
	t = clean_numbers(line)
	list_t = t.split(" ")

	## EXPERIMENTAL (ULTIMATE FULHACK OF DOOM)
	if "coord" in list_t:
		ind = list_t.index("coord")
		list_t[ind] = (list_t[ind+1] + "/" + list_t[ind+2]).strip()
		list_t = list_t[:ind+1]

	#print("AFTER",list_t)
	if not re.match(r'0',list_t[0]):
		dic_list = dic.get(list_t[1])
		st = list_t[0] + " "
		for i,elem in enumerate(list_t[2:]):
			#print(i, dic_list, list_t)
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
				list_row = row.split(" ")[1:]
				dic[list_row[0]] = list_row[1:]
	return dic

main()



