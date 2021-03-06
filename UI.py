from subprocess import Popen, PIPE
import re
import sys
from sys import stdin
import os


def main(fp_ceptre, fp_game):
	""" Main function that open a pipe process.
	It reads each line from the output to the terminal and parse it
	to make it more readable and clear.

	Parameters:
	fp_ceptre (String) - Filepath and run command to the Ceptre-bin file.
	fp_game (String)   - Filepath to the .hs file to run.
	"""
	is_start = True
	is_finished = False
	line_pointer = 0
	dic = create_dict_move(fp_game)
	cmd = [fp_ceptre, fp_game]
	p = Popen(cmd, stdout=PIPE)

	for line in iter(p.stdout.readline, ""):
		# For removing the starting rows
		if line == "#trace ...\n":
			create_initial_board(dic)
			is_start = False
		# Removes the ?- line
		elif re.match(r'\?-',line):
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


def create_initial_board(dic):
	""" Creates the initial state of the board

	Parameters:
	dic (dictionary) - Dictionary of players and there piece positions.
	"""
	os.system("clear")
	log_file = open("log.txt")
	line = log_file.readlines()[1]
	dic_of_positions = line_to_coord(line,dic)
	print_board(dic_of_positions)


def create_board(line_pointer, dic, match):
	""" Creates the board for the updated state after user pick 
	an alternative move to go to.
	
	Parameters:
	line_pointer (int) - Point at the line to start reading from in "log.txt".
	dic (dictionary)   - Dictionary of players and there piece positions.
	match (String)     - The line to match regex on in "log.txt".

	Returns:
	line_pointer (int) - Pointing at the line after last read one in "log.txt".
	"""
	os.system("clear")
	line = open("log.txt").readlines()
	for i,l in enumerate (line[line_pointer::]):
		if(re.match('---- {\('+match,l)) : 
			dic_of_positions = line_to_coord(l,dic)
			print_board(dic_of_positions)
			line_pointer += i+1

	return line_pointer


def print_board(dic_of_positions):
	""" Creates the board in a matrix and pass it to print_matrix to print.
	Takes the dictionary and loop through each player (key) and 
	add the players first letter in uppercase as Piece
	
	Parameters:
	dic_of_positions (dictionary) - Dictionary of players and there piece positions.
	"""
	xMax, yMax = get_boardsize(dic_of_positions)
	mat = [["\33[0m"+"_" for j in range(yMax+1)] for i in range(xMax+1)]
	i = 0
	for key,val in dic_of_positions.items():
		if key != "free":
			for x,y in val:
				if i == 0:
					mat[x][y] = '\33[31m' + key[0].upper()
				if i == 1:
					mat[x][y] = '\33[32m' + key[0].upper()
				if i == 2:
					mat[x][y] = '\33[33m' + key[0].upper()
		i += 1

	print_matrix(mat)


def get_boardsize(dic_of_positions):
	""" Returns the size of the board with an max value of x and y
	
	Parameters:
	dic_of_positions (dictionarys) - Dictionary of players and there piece positions.

	Returns:
	xMax (int) - Max value on the x-axis
	yMax (int) - Max value on the y-axis
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

	Parameters:
	mat (dictionary) - Matrix of characters (players pieces)
	"""
	rez = [[mat[j][i] for j in range(len(mat))] for i in range(len(mat[0]))]
	size = len(rez)-1
	print("")
	for i,row in enumerate(reversed(rez)):
		r = str(size)
		size = size - 1
		for elem in row:
			r = r + "\33[0m|" + elem
		r += "\33[0m" + "|"
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
	
	Parameters:
	line (String)    - A single line from log.txt that we will parse
	dic (dictionary) - Dictionary of players and there piece positions.

	Returns:
	position (dictionary) - Positions of the players pieces.
	"""
	list_lines = line.split(",")
	positions = {}
	for l in list_lines:
		line = clean_numbers(l).replace("}","").replace("{","").replace("\n","")
		line = line.split(" ")[1:] #Removes the blank spot at start
		key = line[0]
		if isCoordinates(line) :
			player = line[1]
			#if key == "free":
			#	player = key
			#elif not line[1] == "free":
			#	player = line[1]
			#else :
			#	player = line[1]
			xPos, yPos = int(line[-2]), int(line[-1])
			if player in positions:
				positions[player].append( (xPos,yPos) )
			else:
				positions[player] = [(xPos,yPos)]
	return positions


def isCoordinates(line):
	""" Checks if a line contains coordinate pair.

	Parameters:
	line (String) - A single line from log.txt that we will parse

	Returns:
	(boolean) - True if we have a pair of ints or more.
	"""
	total_coords = 0
	for elem in line:
		if elem.isdigit():
			total_coords += 1
 	if total_coords == 2:
 		return True
 	else:
 		return False



def clean_numbers(line):
	""" Clean up the successors of zero so basicly we parse a line from the 
	output and reamoves the "s (s (s (z))))" to a simple 4.
	
	Parameters:
	line (String) - one line from the outputted ceptre lines in the terminal.

	Returns:
	new_line (String) - Cleaned string with ints instead of (s z).
	"""
	tmp_line = re.sub(r"\([sz].*?\)", lambda m: str(m.group().count("s")), line)
	new_line = tmp_line.replace(")","").replace("(","").replace(" z"," 0")
	return new_line


def modify(line, dic):
	""" Modfies one line of the output.
	This method take the dictionary that contains the output values for
	the different stages in ceptre and returns a string that is simplified and
	contain each label.

	Parameters:
	line (String)    - One single line of the ceptre output.
	dic (dictionary) - Dictionary of players and there piece positions.

	Returns:
	st (String) - New string with labels
	"""
	tmp = clean_numbers(line)
	list_t = tmp.split(" ")

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
			kol_name = dic_list[i]
			if not kol_name == "_":
				st = st + kol_name + ": " + elem + "  "
		return st
	return "0: quit"


def create_dict_move(filepath):
	""" Creates the dictionary for the output names. it reads
	the line starting with %% in the ceptre file to then use 
	it to give everything names
	
	Parameters:
	filepath (String) - Filepath to the ceptre file.

	Returns:
	dic (dictionary) - dictionary of players labels.
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