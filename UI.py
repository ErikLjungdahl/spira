import subprocess
import re
import sys
from sys import stdin
import os

def main(fp_ceptre, fp_game):
	""" Main function that open a pipe process.
	It reads each line from the output to the terminal and parse it
	to make it more readable and clear.

	:param fp_ceptre - Filepath and run command to the Ceptre-bin file.
	:param fp_game   - Filepath to the .hs file to run.
	"""
	is_start = True
	is_finished = False
	is_time_to_choose = True
	line_pointer = 0
	dic = create_dict_move(fp_game)
	cmd = [fp_ceptre, fp_game]
	#open("log.txt", "w").close()
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
		elif re.match(r"Trace:",line) and not is_start and not is_finished:
			print(line.rstrip())

	p.stdout.close()
	p.wait()


def create_initial_board(dic):
	""" Creates the initial state of the board
	:param dic - Dictionary of players and there piece positions.
	"""
	file = open("log.txt")
	line = file.readlines()[1]
	print("")
	dic_of_positions = line_to_coord(line,dic)
	print_board(dic_of_positions)
	print("")

def create_board(line_pointer, dic, match):
	""" Creates the board for the updated state after user pick 
	an alternative move to go to.

	:param line_pointer - Point at the line to start reading from in "log.txt".
	:param dic          - Dictionary of players and there piece positions.
	:param match        - The line to match regex on in "log.txt".
	"""
	line = open("log.txt").readlines()
	print("")
	for i,l in enumerate (line[line_pointer::]):
		
		if(re.match('---- {\('+match,l)) : 
			dic_of_positions = line_to_coord(l,dic)
			print_board(dic_of_positions)
			line_pointer += i+1

	print("")
	return line_pointer



def print_board(dic_of_positions):
	""" Creates the board in a matrix and pass it to print_matrix to print.
	Takes the dictionary and loop through each player (key) and 
	add the players first letter in uppercase as Piece

	:param dic_of_positions - Dictionary of players and there piece positions.

	"""
	xMax, yMax = get_boardsize(dic_of_positions)
	mat = [["_" for j in range(yMax+1)] for i in range(xMax+1)]
	for key,val in dic_of_positions.items():
		if key != "free":
			for x,y in val:
				mat[x][y] = key[0].upper()
	print_matrix(mat)


def get_boardsize(dic_of_positions):
	""" Returns the size of the board with an max value of x and y
	
	:param dic_of_positions - Dictionary of players and there piece positions.
	"""
	xMax, yMax = 0, 0
	for key,val in dic_of_positions.items():
		for x,y in val:
			xMax = x if xMax < x else xMax
			yMax = y if yMax < y else yMax
	return xMax, yMax
 

	
def print_matrix(mat):
	""" Prints the board nicely with indexing columns/rows, this
	makes is easier to read the board.

	:param mat - Matrix of characters (players pieces) 
	"""
	rez = [[mat[j][i] for j in range(len(mat))] for i in range(len(mat[0]))]
	size = len(rez)-1
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
	""" Parse a line from the log.txt into a dictionary of positions,
	it creates a key for each player and one for the "free".
	Then it adds all position that each key allready posess.
	
	:param line - A single line from log.txt that we will parse
	:param dic  - Dictionary of players and there piece positions.
	"""
	list_lines = line.split(",")
	positions = {}

	for l in list_lines:
		line = clean_numbers(l).replace("}","").replace("{","")
		line = line.split(" ")[1:] #Removes the blank spot at start
		key = line[0]
		if(key == "occupied" or key == "tile"):
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
			xPos = int(line[-2])
			yPos = int(line[-1])
			if key in positions:
				positions[key].append( (xPos,yPos) )
			else:
				positions[key] = [(xPos,yPos)]
	
	return positions



def clean_numbers(line):
	""" Clean up the successors of zero so basicly we parse a line from the 
	output and reamoves the "s (s (s (z))))" to a simple 4.

	:param line - one line from the outputted ceptre lines in the terminal.
	"""
	tmp = re.sub(r"\([sz].*?\)", lambda m: str(m.group().count("s")), line)
	t = tmp.replace(")","").replace("(","").replace(" z"," 0")
	return t


# Modifies the (s (s z)) -> int.
# Also gives the names to each kolumn/parameter/dont know what it is called
def modify(line, dic):
	""" Modfies one line of the output.
	This method take the dictionary that contains the output values for
	the different stages in ceptre and returns a string that is simplified and
	contain each label.

	:param line - One single line of the ceptre output.
	:param dic  - Dictionary of players and there piece positions.
	"""
	t = clean_numbers(line)
	list_t = t.split(" ")

	# creates a display of "Row/Col" instead of two different row: 1  col: 1
	if "coord" in list_t:
		ind = list_t.index("coord")
		list_t[ind] = (list_t[ind+1] + "/" + list_t[ind+2]).strip()
		list_t = list_t[:ind+1]

	# Reads a line and att each label to the value
	if not re.match(r'0',list_t[0]):
		dic_list = dic.get(list_t[1])
		st = list_t[0] + " "
		for i,elem in enumerate(list_t[2:]):
			#print(i, dic_list, list_t)
			kol_name = dic_list[i]
			if not kol_name == "_":
				st = st + kol_name + ": " + elem + "  "
		return st
	return "0: quit"

# creates a dict on all interactive plays that exist
def create_dict_move(filepath):
	""" Creates the dictionary for the output names. it reads
	the line starting with %% in the ceptre file to then use 
	it to give everything names

	:param filepath - Filepath to the ceptre file.
	"""
	dic = {}
	with open(filepath) as f:
		for row in f:
			row = row.strip()
			if(re.match(r'%%', row)):
				list_row = row.split(" ")[1:]
				dic[list_row[0]] = list_row[1:]
	return dic

# Runs the game
if __name__ == "__main__":
	fp_ceptre = sys.argv[1]
	fp_game = sys.argv[2]
	main(fp_ceptre, fp_game)