import re
import sys

#This is the PreCCessor, a simple pre-processor for C language.
#It will remove comments, linebreaks, multiple spaces and desnecessary spaces.
#It will also expand #include and #define


def remove_inline_comments(line):

    slash_position = line.find("//")
    if slash_position != -1:
        aux = line[:slash_position]
    else:
        aux = line
    return aux

def remove_multiline_comments(code):
    aux = code
    return aux


def remove_linebreak(code):
    # destrincha linha por linha, checa #include or #define, se houver ele adiciona \n ao final da linha
    aux = ''
    for line in code.splitlines():
        if line.find('#include') == -1 and line.find('#define') == -1:
            aux += line
        else:
            aux += line + '\n'
    return aux


def remove_multiple_spaces(code):
     aux = re.sub(r'(  +)', ' ', code) # remove multiple spaces
     return aux

def remove_desnecessary_space(code):
    # aux = re.sub(r'([a-zA-Z0-9])(\s+)([a-zA-Z0-9])', r'\1\3', code) # remove spaces between letters and numbers
    aux = re.sub(r'\s*([;:`\'|,!"(){}=\-<>\[\]])\s*', r'\1', code) # remove spaces after ;(){}=-
    return aux

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
    input = open(input_file, "r")
    output = open(output_file, "w")


    newCode = ''
    for line in input:
        line = remove_inline_comments(line)
        newCode += line
    newCode = remove_linebreak(newCode)
    newCode = remove_multiple_spaces(newCode)
    newCode = remove_desnecessary_space(newCode)

    

    expand_define()
    expand_include()
    output.write(newCode)

    print("preCcessor finished with success!")

    input.close()
    output.close()





'''
    order:
    1. import define, include and macros (recursive)
    2. expand define and macros (recursive) 
    3. remove inline comments
    4. remove linebreaks
    5. remove 'multiline' comments
    6. remove multiple spaces
    7. remove desnecessary spaces
'''