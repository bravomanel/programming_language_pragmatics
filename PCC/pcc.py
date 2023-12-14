import re
import sys

#This is the PreCCessor, a simple pre-processor for the C language.
#It will remove inline and multiline comments, linebreaks, multiple and/or desnecessary spaces.
#It will also handle the #include and #define

def remove_inline_comments(code):
    aux = ''
    for line in code.splitlines(True):
        if line.find('//') != -1:
            aux += line[0:line.find('//')] + '\n'
        else:
            aux += line
    return aux

def process_define(text):
    define_directives = re.compile(r'#define\s(\w+)\s(.*)').findall(text)
    for match in define_directives:
        text = text.replace(f'#define {match[0]} {match[1]}', '')
        text = re.sub(r'\b' + match[0] + r'\b', match[1], text)
    return text

def remove_linebreak(code):
    aux = ''
    for line in code.splitlines():
        if line.find('#') == -1:
            aux += line
        else:
            aux += line + '\n'
    return aux

def remove_multiline_comments(code):
    opencomment = code.find('/*')
    closecomment = code.find('*/')
    if opencomment == -1 and closecomment == -1:
        return code
    else:
        aux = code[0:opencomment] + code[closecomment+2:]
        return remove_multiline_comments(aux)



def remove_multiple_spaces(code):
    aux = re.sub(r'(  +)', ' ', code)
    aux = re.sub(r'(\t)', '', aux)
    return aux

def remove_desnecessary_space(code):
    aux = ''
    for line in code.splitlines(True):
        if line.find('#') != -1:
            aux += re.sub(r'\s+#', '#', line)
        else:
            aux += re.sub(r'\s*([;:`|,!(){}=\-<>\[\]])\s*', r'\1', line)
    return aux

def process_include(text):
    include_directives = re.compile(r'#include\s*[<](.*?)[>]').findall(text)    
    include_locals = re.compile(r'#include\s*["](.*?)["]').findall(text)
    included_any = False

    for match in include_directives:
        try:
            filepath = '/usr/include/' + match
            with open(filepath, 'r') as include_file:
                include_content = include_file.read()
                text = text.replace(f'#include <{match}>', include_content)
                included_any = True
                include_directives.remove(match)
        except FileNotFoundError:
            print(f"Warning: File '{match}' not found in include directive.")            


    for match in include_locals:
        try:
            with open(match, 'r') as include_file:
                include_locals.remove(match)
                include_content = include_file.read()
                included_any = True
                text = text.replace(f'#include "{match}"', include_content)
        except FileNotFoundError:
            print(f"Warning: File '{match}' not found in local directory.")
    
    if included_any == False:
        return text
    else:
        return process_include(text)


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

    output_file = input_file[0:-2] + "-pcc.c"

    print("PreCCessor is running...")
    input = open(input_file, "r")
    output = open(output_file, "w")


    aux_code = input.read()
    aux_code = remove_inline_comments(aux_code)
    aux_code = process_define(aux_code)
    aux_code = remove_linebreak(aux_code)
    aux_code = remove_multiline_comments(aux_code)
    aux_code = remove_multiple_spaces(aux_code)
    aux_code = remove_desnecessary_space(aux_code)
    aux_code = process_include(aux_code)
    
    output.write(aux_code)

    print("PreCCessor finished with success!")

    input.close()
    output.close()