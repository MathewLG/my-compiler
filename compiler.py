'''
    Mathew Gabriel Lopez Garcia
    A01635001

    Proyecto final para la clase: Diseño de Compiladores.
'''

#Imports y dependencias
import sys
import ply.lex as lex
import ply.yacc as yacc
sys.path.insert(0, "..")

#-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*--*-*-*-*-*-*-*-*-*
#----------------------Paso 1. Tokenizacion para el analisis lexico---------------------------------------------------

#Definicion de tokens del lenguaje
reserved = {
    "int": "INTDCL",
    "float": "FLOATDCL",
    "print": "PRINT",
    "boolean": "BOOLDCL",
    "true": "BOOLVAL",
    "false": "BOOLVAL",
    "if": "IF",
    "else": "ELSE", 
    "elif" : "ELIF", 
    "for" : "FOR",
    "while" : "WHILE"
}



tokens = [
    'NAME', 'INUMBER', 'FNUMBER', 'EQUALS', 'NOT_EQUALS', 'GREATER_EQUAL', 'LESS_EQUAL', 'INCREMENT', 'DECREMENT'
]
tokens.extend(reserved.values())

literals = ['=', '+', '-', ';', '(', ')', '{', '}', '<', '>', '*', '/', '^']

def t_NAME(t):
    r'[a-zA-Z_]+[a-zA-Z0-9]*' #r'[a-eg-hj-oq-z]'
    if t.value in reserved:
        t.type = reserved[t.value]
    return t


def t_FNUMBER(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t


def t_INUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t


def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    
    t.lexer.skip(1)


t_ignore = " \t"
t_EQUALS = r'=='
t_NOT_EQUALS = r'!='
t_GREATER_EQUAL = r'>='
t_LESS_EQUAL = r'<='
t_INCREMENT = r'\+\+'
t_DECREMENT = r'--'

#------------------------------Paso 2. Construccion del lexer ---------------------------------------------------
lexer = lex.lex()


#------------------------------ Paso 3. Analisis sintactico (parseo) --------------------------------------------------------


# ----------- Creacion de una clase Node para la creacion del arbol sintactico abstracto.
class Node:
    
    # childrens = None
    # type = None

    def __init__(self):
        self.childrens = []
        self.type = ''
        self.val = ''

    def print(self, lvl = 0):
        f = open("output.txt", "w")
        r = (' ' * lvl) + self.type + ":" + str(self.val)
        print(r)
        #print(self.childrens)
        for c in self.childrens:
            c.print(lvl+1)
        

# Creamos un diccionario que constituye la tabla de simbolos de nuestro compilador
symbolsTable = {
    "table" : {},
    "parent" : None,
}
#Inicializacion de un arbol sintactico abstracto vacio. 
abstractTree = None


'''
     *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ----------- El punto de entrada del programa: Un programa esta hecho de statements. 
      Se instancia un arbol abstracto. Se asigna un nodo raiz al arbol. 
'''
def p_prog(p):
    'prog : stmts'
    global abstractTree
    abstractTree = Node()
    abstractTree.type = 'root'
    abstractTree.childrens.extend(p[1])


#Un statement puede tener mas statements a su derecha. 
def p_statements_recursion(p):
    '''stmts : statement stmts
             | statement '''
    stmt = p[1]
    if len(p) == 3: #Si hay recursion por la derecha
        stmts = [ stmt ]
        stmts.extend(p[2])
        p[0] = stmts
    else: 
        p[0] = [ stmt ]

#La declaracion de numeros enteros consiste en: int <NOMBRE> ;
#En este punto, se declara la variable, pero no se le asigna un valor. 
#Por defecto, el valor de los numeros enteros no asignados es 0. 
def p_dcl_declare_int(p):
    'statement : INTDCL NAME ";"'
    symbolsTable["table"][p[2]] = { "type": "INT", "value":0}
    n = Node()
    n.type = "INT_DLC"
    n.val = p[2]
    p[0] = n

#La declaracion de numeros flotantes consiste en: float <NOMBRE> ;
#En este punto, se declara la variable, pero no se le asigna un valor. 
#Por defecto, el valor de los numeros flotantes no asignados es 0. 
def p_statement_declare_float(p):
    'statement : FLOATDCL NAME ";"'
    symbolsTable["table"][p[2]] = { "type": "FLOAT", "value":0 }
    n = Node()
    n.type = "FLOAT_DLC"
    n.val = p[2]
    p[0] = n

#La declaracion de numeros flotantes consiste en: boolean <NOMBRE> ;
#En este punto, se declara la variable, pero no se le asigna un valor. 
#Por defecto, el valor de los numeros flotantes no asignados es False. 
def p_statement_declare_bool(p):
    'statement : BOOLDCL NAME ";"'
    symbolsTable["table"][p[2]] = { "type": "BOOLEAN", "value": False }
    n = Node()
    n.type = "BOOL_DLC"
    n.val = p[2]
    p[0] = n

#La declaracion de un print consiste en: print <expresion> ;
def p_statement_print(p):
    'statement : PRINT expression ";"'
    n = Node()
    n.type = 'PRINT'
    n.childrens.append(p[2])
    p[0] = n

def p_statement_if(p):
    'statement : IF "(" boolexp ")" "{" stmts "}"'
    n = Node()
    n.type = 'IF'
    n2 = Node()
    n2.childrens = p[6]
    n.childrens.append(p[3])
    n.childrens.append(n2)
    p[0] = n

def p_statement_for(p):
    'statement : FOR "(" start ";" comparison ";" forcontrol ")" "{" stmts "}"'
    n = Node()
    n.type = 'FOR'
    p[0] = n
    start = Node()
    start.val = p[3].childrens[0]
    n.childrens.append(start) #Start value
    n.childrens.append(p[5]) #Boolean condition
    n.childrens.append(p[7]) #Increment
    codeblock = Node()
    codeblock.childrens = p[10]
    n.childrens.append(codeblock)
    p[0] = n

def p_forcontrol(p):
    '''forcontrol : NAME INCREMENT
                | NAME DECREMENT '''
    n = Node()
    p[0] = n

def p_comparison(p):
    '''comparison : NAME "<" NAME 
                | NAME "<" INUMBER
                | NAME ">" NAME
                | NAME ">" INUMBER '''
    n = Node()
    n.val = str(str(p[1]) + " " + str(p[2]) + " " + str(p[3]))
    p[0] = n

def p_start_for(p):
    'start : NAME "=" INUMBER'
    n = Node()
    n.type = "STARTFOR"
    n.val = p[1]
    n.childrens.append(p[3])
    p[0] = n
    
def p_statement_while(p):
    'statement : WHILE "(" boolexp ")" "{" stmts "}"'
    n = Node()
    n.type = "WHILE"
    codeblock = Node()
    codeblock.childrens = p[6]
    n.childrens.append(p[3])
    n.childrens.append(codeblock)
    p[0] = n

def p_statement_elif(p):
    'statement : ELIF "{" stmts "}"'
    n = Node()
    n.type = 'ELIF'
    n2 = Node()
    n2.childrens = p[3]
    n.childrens.append(n2)
    p[0] = n

def p_statement_else(p):
    'statement : ELSE "{" stmts "}"'
    n = Node()
    n.type = 'ELSE'
    n2 = Node()
    n2.childrens = p[3]
    n.childrens.append(n2)
    p[0] = n

def p_statement_assign(p):
    'statement : NAME "=" expression ";"'
    if p[1] not in symbolsTable["table"]:
        print ( "You must declare a variable before using it")
    n = Node()
    n.type = 'ASIGN'
    ##n.childrens.append(p[1])
    if p[1] in symbolsTable["table"]:
        n1 = Node()
        n1.type = 'ID'
        n1.val = p[1]
        n.childrens.append(n1)
        symbolsTable["table"][p[1]]["value"] = p[3]
    else: 
        print("Error undeclared variable")


    n.childrens.append(p[3])
    p[0] = n

def p_expression_group(p):
    "expression : '(' expression ')'"
    p[0] = p[2]

def p_expression_binop(p):
    '''expression : expression '+' expression
                  | expression '-' expression
                  | expression '*' expression
                  | expression '/' expression
                  | expression '^' expression'''
    if p[2] == '+':
        n = Node()
        n.type = '+'
        n.childrens.append(p[1])
        n.childrens.append(p[3])
        p[0] = n
    elif p[2] == '-':
        n = Node()
        n.type = '-'
        n.childrens.append(p[1])
        n.childrens.append(p[3])
        p[0] = n
    elif p[2] == '*':
        n = Node()
        n.type = '*'
        n.childrens.append(p[1])
        n.childrens.append(p[3])
        p[0] = n
    elif p[2] == '/':
        n = Node()
        n.type = '/'
        n.childrens.append(p[1])
        n.childrens.append(p[3])
        p[0] = n
    elif p[2] == '^':
        n = Node()
        n.type = '^'
        n.childrens.append(p[1])
        n.childrens.append(p[3])
        p[0] = n

def p_unary_operator(p):
    "expression : INUMBER INCREMENT"
    n = Node()
    n.type = 'UNARYOP'
    n.val = int(p[1]) + 1
    p[0] = n

def p_expression_inumber(p):
    "expression : INUMBER"
    n = Node()
    n.type = 'INUMBER'
    n.val = int(p[1])
    p[0] = n



def p_expression_fnumber(p):
    "expression : FNUMBER"
    n = Node()
    n.type = 'FNUMBER'
    n.val = float(p[1])
    p[0] = n


def p_expression_boolval(p):
    "expression : boolexp"
    p[0] = p[1]

def p_bool_expression(p):
    "boolexp : BOOLVAL"
    n = Node()
    n.type = 'BOOLVAL'
    n.val = (p[1] == 'true')
    p[0] = n


def p_expression_name(p):
    "expression : NAME"
    if p[1] in symbolsTable["table"]:
        n = Node()
        n.type = 'ID'
        n.val = p[1]
        p[0] = n



def p_error(p):
    if p:
        print("Syntax error at '%s'" % p.value)
    else:
        print("Syntax error at EOF")



parser = yacc.yacc()


f = open("code.txt")
content = f.read()
yacc.parse(content)


abstractTree.print()
varCounter = 0
labelCounter = 0
def genTAC(node):
    #sys.stdout = open("output.txt", "a")
    global varCounter
    global labelCounter
    if node == None:
        return ""
    if ( node.type == "ASIGN" ):
        print(node.childrens[0].val  + " := " + genTAC(node.childrens[1]) )
    elif ( node.type == "INUMBER"):
        return str(node.val)
    elif ( node.type == "ID"):
        return str(symbolsTable["table"][node.val]["value"].val)
    elif ( node.type == "+"):
        tempVar = "t" + str(varCounter)
        varCounter = varCounter +1
        print( tempVar + " := " + genTAC(node.childrens[0]) + " + " + genTAC(node.childrens[1]))
        return tempVar
    elif ( node.type == "-"):
        tempVar = "t" + str(varCounter)
        varCounter = varCounter +1
        print( tempVar + " := " + genTAC(node.childrens[0]) + " - " + genTAC(node.childrens[1]))
        return tempVar
    elif ( node.type == "*"):
        tempVar = "t" + str(varCounter)
        varCounter = varCounter +1
        print( tempVar + " := " + genTAC(node.childrens[0]) + " * " + genTAC(node.childrens[1]))
        return tempVar
    elif ( node.type == "/"):
        tempVar = "t" + str(varCounter)
        varCounter = varCounter +1
        print( tempVar + " := " + genTAC(node.childrens[0]) + " / " + genTAC(node.childrens[1]))
        return tempVar
    elif ( node.type == "^"):
        tempVar = "t" + str(varCounter)
        varCounter = varCounter +1
        print( tempVar + " := " + genTAC(node.childrens[0]) + " ^ " + genTAC(node.childrens[1]))
        return tempVar
    elif ( node.type == "PRINT"):
        print( "PRINT " + genTAC(node.childrens[0]))
    elif ( node.type == "IF" ):
        tempVar = "t" + str(varCounter)
        varCounter = varCounter +1
        print ( tempVar + " := !" + str(node.childrens[0].val))
        tempLabel = "l" + str(labelCounter)
        labelCounter = labelCounter + 1
        print ( "gotoLabelIf " + tempVar + " " + tempLabel)
        genTAC(node.childrens[1])
        print ( tempLabel)
    elif ( node.type == "WHILE" ):
        print("WHILE")
        tempVar = "t" + str(varCounter)
        varCounter = varCounter +1
        print ( tempVar + " := !" + str(node.childrens[0].val))
        tempLabel = "l" + str(labelCounter)
        labelCounter = labelCounter + 1
        print ( "gotoLabelWhile " + tempVar + " " + tempLabel)
        print("Statements")
        genTAC(node.childrens[1])
        print ( tempLabel)
    elif ( node.type == "FOR" ):
        print("FOR")
        tempVar = "t" + str(varCounter)
        varCounter = varCounter +1
        print ( tempVar + " := " + str(node.childrens[0].val))
        tempVar2 = "t" + str(varCounter)
        varCounter = varCounter +1
        print( tempVar2 + " = " + str(node.childrens[1].val))
        tempLabel = "l" + str(labelCounter)
        labelCounter = labelCounter + 1
        print ( "gotoLabelFor " + tempVar + " " + tempLabel)
        print("Statements")
        genTAC(node.childrens[1])
        print ( tempLabel)
    else:
        for child in node.childrens:
            genTAC(child)
    
f = open("output.txt", "w")
print("\ntac:\n")
genTAC(abstractTree)

#Some examples
# for ( i = 0; i < 3; i++){
#     stamentes
# }
# i := 0
# t1 = i < 3
# t0 = !t1
# gotoLabelif t0 Label1

# staments
# i = i + 1
# Label1



# while ( condicion ) {
#     staments
# }
# WHILE
# t1 = condicion
# t0 = !t1
# gotoLabelif t0 Label1

# staments

# Label1
