import re
import sys

# Inventar nome irado = preCcessor ??

def remove_comments_linebreak(line, code):
    oneSpaceLine = re.sub(r'(  +)', ' ', line)

    slash_position = oneSpaceLine.find("//")
    if slash_position != -1:
        code += oneSpaceLine[:slash_position]
    elif oneSpaceLine.find('#include') == -1:
        code += oneSpaceLine.replace('\n', ' ')
    else:
        code += oneSpaceLine
    return code

def expand_include():
    return

def expand_define():
    return


def call_error():
    print("Usage: python preCcessor.py <input_file.c>")
    print("The output file with be: <input_file>")
    sys.exit(1)


if __name__ == '__main__':
    if len(sys.argv) != 2:
        call_error()

    input_file = sys.argv[1]

    if input_file[-2:] != ".c":
        call_error()

    output_file = input_file[0:-2] + "-pp.c"


    print("preCcessor is running...")
    print(input_file)
    print(output_file)

    input = open(input_file, "r")
    output = open(output_file, "w")

    newCode = ''
    for line in input:
        newCode = remove_comments_linebreak(line, newCode)
        expand_define()
        expand_include

    output.write(newCode)

    print("preCcessor finished with success!")

    input.close()
    output.close()
