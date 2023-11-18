import re
import sys

#This is the PreCCessor, a simple pre-processor for C language.
#It will remove comments, linebreaks, multiple spaces and desnecessary spaces.
#It will also expand #include and #define

def process_include_directives(text):
    include_directives = re.compile(r'#include\s*[<](.*?)[>]').findall(text)    
    include_locals = re.compile(r'#include\s*["](.*?)["]').findall(text)
    print(include_directives)
    print(include_locals)

    shoud_continue = True

    for match in include_directives:
        try:
            filepath = '/usr/include/' + match
            with open(filepath, 'r') as include_file:
                include_content = include_file.read()
                text = text.replace(f'#include <{match}>', include_content)
                include_directives.remove(match)
        except FileNotFoundError:
            include_directives.remove(match)
            print(f"Warning: File '{match}' not found in include directive.")            


    for match in include_locals:
        try:
            with open(match, 'r') as include_file:
                include_locals.remove(match)
                include_content = include_file.read()
                text = text.replace(f'#include "{match}"', include_content)
        except FileNotFoundError:
            include_locals.remove(match)
            print(f"Warning: File '{match}' not found in local directory.")
    
    if (len(include_directives) + len(include_locals)) == 0:
        shoud_continue = False

    return [text, shoud_continue]


def remove_inline_comments(code):
    aux = ''
    for line in code.splitlines(True):
        if line.find('//') != -1:
            aux += line[0:line.find('//')]
        else:
            aux += line
    return aux


def remove_multiline_comments(code):
    opencomment = code.find('/*')
    closecomment = code.find('*/')
    if opencomment == -1 and closecomment == -1:
        return code
    else:
        aux = code[0:opencomment] + code[closecomment+2:]
        return remove_multiline_comments(aux)


def remove_linebreak(code):
    # destrincha linha por linha, checa #include or #define, se houver ele adiciona \n ao final da linha
    aux = ''
    for line in code.splitlines():
        if line.find('#') == -1:
            aux += line
        else:
            aux += line + '\n'
    return aux


def remove_multiple_spaces(code):
     aux = re.sub(r'(  +)', ' ', code) # remove multiple spaces
     aux = re.sub(r'(\t+)', '', aux) # remove multiple spaces
     return aux

def remove_desnecessary_space(code):
    aux = ''
    for line in code.splitlines(True):
        aux += re.sub(r'\s*([;:`\'|,!"(){}=\-<>\[\]])\s*', r'\1', line) # remove spaces after ;(){}=-
        aux+= '\n'
    return aux[:-21]

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


    aux_code = input.read()
    aux_code = remove_inline_comments(aux_code)
    aux_code = remove_linebreak(aux_code)
    aux_code = remove_multiline_comments(aux_code)
    aux_code = remove_multiple_spaces(aux_code)
    aux_code = remove_desnecessary_space(aux_code)
    # [aux_code, still_include] = process_include_directives(aux_code)


    output.write(aux_code)

    print("preCcessor finished with success!")

    input.close()
    output.close()


'''
    order:
    1. remove inline comments
    2. import define, include and macros (recursive)
    3. expand define and macros (recursive) 
    4. remove linebreaks
    5. remove 'multiline' comments
    6. remove multiple spaces
    7. remove desnecessary spaces
'''