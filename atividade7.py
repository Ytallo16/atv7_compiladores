import ply.lex as lex
import ply.yacc as yacc
import sys

# Estrutura para armazenar variáveis
class Variable:
    def __init__(self, name, value=None, type=None):
        self.name = name
        self.value = value
        self.type = type

class Calculator:
    # Constantes de tipos
    NUMB_TYPE = 1
    BOOL_TYPE = 2
    
    def __init__(self):
        self.symtable = {}  # Tabela de símbolos
        self.var_temp_count = 0
        
        # Build the lexer and parser
        self.lexer = lex.lex(module=self)
        self.parser = yacc.yacc(module=self)
        
    # Tokens do lexer
    tokens = (
        'FLOAT',
        'ID',
        'INTEIRO',
        'BOOLEAN',
        'TRUE',
        'FALSE',
        'LESS_THAN',
        'GREATER_THAN',
        'EQUAL_EQUAL',
        'AND',
        'OR',
        'NOT'
    )
    
    # Literais (caracteres únicos)
    literals = ['+', '-', '*', '/', '(', ')', '=', ';']
    
    # Regras do lexer
    def t_NUMBER(self, t):
        r'inteiro'
        t.value = self.NUMB_TYPE
        return t
    
    def t_BOOLEAN(self, t):
        r'boolean'
        t.value = self.BOOL_TYPE
        return t
    
    def t_TRUE(self, t):
        r'true'
        t.value = True
        return t
    
    def t_FALSE(self, t):
        r'false'
        t.value = False
        return t
    
    def t_LESS_THAN(self, t):
        r'<'
        return t
    
    def t_GREATER_THAN(self, t):
        r'>'
        return t
    
    def t_EQUAL_EQUAL(self, t):
        r'=='
        return t
    
    def t_AND(self, t):
        r'and'
        return t
    
    def t_OR(self, t):
        r'or'
        return t
    
    def t_NOT(self, t):
        r'not'
        return t
    
    def t_FLOAT(self, t):
        r'\d*\.\d+|\d+'
        t.value = float(t.value)
        return t
    
    def t_ID(self, t):
        r'[a-zA-Z_][a-zA-Z0-9_]*'
        return t
    
    # Caracteres ignorados
    t_ignore = ' \t\n'
    
    def t_error(self, t):
        print(f"Illegal character '{t.value[0]}'")
        t.lexer.skip(1)
    
    # Precedência de operadores
    precedence = (
        ('left', 'OR'),
        ('left', 'AND'),
        ('right', 'NOT'),
        ('left', 'LESS_THAN', 'GREATER_THAN', 'EQUAL_EQUAL'),
        ('left', '+', '-'),
        ('left', '*', '/'),
    )
    
    # Funções auxiliares para manipulação de variáveis
    def get_var_value(self, name):
        if name in self.symtable:
            return self.symtable[name].value
        raise NameError(f"Variable '{name}' not found")
    
    def get_var_type(self, name):
        if name in self.symtable:
            return self.symtable[name].type
        raise NameError(f"Variable type not found for '{name}'")
    
    def set_var_value(self, name, value):
        if name in self.symtable:
            self.symtable[name].value = value
        else:
            self.symtable[name] = Variable(name, value)
    
    def declare_var(self, name, type):
        if name not in self.symtable:
            self.symtable[name] = Variable(name, 0, type)
        else:
            raise NameError(f"Variable '{name}' already declared")
    
    def newtemp(self):
        self.var_temp_count += 1
        return f"t{self.var_temp_count}"
    
    # Regras gramaticais
    def p_program(self, p):
        '''program : 
                  | program stmt ';' '''
        if len(p) > 1:
            print(f"Result: {p[2].get('code', '')}")
    
    def p_stmt(self, p):
        '''stmt : expr
                | ID '=' expr
                | declaration'''
        if len(p) == 2:  # expr
            p[0] = p[1]
        elif len(p) == 4:  # ID = expr
            var_name = p[1]
            expr = p[3]
            if self.get_var_type(var_name) == expr['type']:
                self.set_var_value(var_name, expr['val'])
                p[0] = {
                    'code': f"{expr['code']};\n{var_name}={expr['place']}",
                    'place': var_name,
                    'val': expr['val'],
                    'type': expr['type']
                }
            else:
                raise TypeError("Type mismatch in assignment")
    
    def p_declaration(self, p):
        '''declaration : type ID'''
        self.declare_var(p[2], p[1])
        p[0] = {'code': '', 'place': p[2], 'type': p[1]}
    
    def p_type(self, p):
        '''type : NUMBER
                | BOOLEAN'''
        p[0] = p[1]
    
    def p_expr(self, p):
        '''expr : expr '+' term
                | expr '-' term
                | expr LESS_THAN term
                | expr GREATER_THAN term
                | expr EQUAL_EQUAL term
                | expr AND term
                | expr OR term
                | term'''
        if len(p) == 2:
            p[0] = p[1]
        else:
            temp = self.newtemp()
            
            # Tratamento de operações numéricas
            if p[1]['type'] == self.NUMB_TYPE and p[3]['type'] == self.NUMB_TYPE:
                if p[2] == '+':
                    val = p[1]['val'] + p[3]['val']
                elif p[2] == '-':
                    val = p[1]['val'] - p[3]['val']
                elif p[2] == '<':
                    val = p[1]['val'] < p[3]['val']
                    result_type = self.BOOL_TYPE
                elif p[2] == '>':
                    val = p[1]['val'] > p[3]['val']
                    result_type = self.BOOL_TYPE
                elif p[2] == '==':
                    val = p[1]['val'] == p[3]['val']
                    result_type = self.BOOL_TYPE
                else:
                    raise TypeError(f"Invalid numeric operation {p[2]}")
                
                p[0] = {
                    'val': val,
                    'type': result_type if 'result_type' in locals() else self.NUMB_TYPE,
                    'place': temp,
                    'code': f"{p[1]['code']};\n{p[3]['code']};\n{temp}={p[1]['place']}{p[2]}{p[3]['place']}"
                }
            
            # Tratamento de operações booleanas
            elif p[1]['type'] == self.BOOL_TYPE and p[3]['type'] == self.BOOL_TYPE:
                if p[2] == 'and':
                    val = p[1]['val'] and p[3]['val']
                elif p[2] == 'or':
                    val = p[1]['val'] or p[3]['val']
                elif p[2] == '==':
                    val = p[1]['val'] == p[3]['val']
                else:
                    raise TypeError(f"Invalid boolean operation {p[2]}")
                
                p[0] = {
                    'val': val,
                    'type': self.BOOL_TYPE,
                    'place': temp,
                    'code': f"{p[1]['code']};\n{p[3]['code']};\n{temp}={p[1]['place']}{p[2]}{p[3]['place']}"
                }
            else:
                raise TypeError("Type mismatch in operation")
    
    def p_term(self, p):
        '''term : term '*' factor
                | term '/' factor
                | NOT factor
                | '-' factor
                | factor'''
        if len(p) == 2:
            p[0] = p[1]
        elif len(p) == 3:  # Unary operations (- or not)
            temp = self.newtemp()
            if p[1] == 'not' and p[2]['type'] == self.BOOL_TYPE:
                p[0] = {
                    'val': not p[2]['val'],
                    'type': self.BOOL_TYPE,
                    'place': temp,
                    'code': f"{p[2]['code']};\n{temp}=not {p[2]['place']}"
                }
            elif p[1] == '-' and p[2]['type'] == self.NUMB_TYPE:
                p[0] = {
                    'val': -p[2]['val'],
                    'type': self.NUMB_TYPE,
                    'place': temp,
                    'code': f"{p[2]['code']};\n{temp}=minus {p[2]['place']}"
                }
            else:
                raise TypeError("Invalid unary operation")
        else:  # Binary operations (* /)
            if p[1]['type'] == self.NUMB_TYPE and p[3]['type'] == self.NUMB_TYPE:
                temp = self.newtemp()
                if p[2] == '*':
                    val = p[1]['val'] * p[3]['val']
                else:  # p[2] == '/'
                    if p[3]['val'] == 0:
                        raise ZeroDivisionError("Division by zero")
                    val = p[1]['val'] / p[3]['val']
                
                p[0] = {
                    'val': val,
                    'type': self.NUMB_TYPE,
                    'place': temp,
                    'code': f"{p[1]['code']};\n{p[3]['code']};\n{temp}={p[1]['place']}{p[2]}{p[3]['place']}"
                }
            else:
                raise TypeError(f"Invalid types for {p[2]} operation")
    
    def p_factor(self, p):
        '''factor : '(' expr ')'
                 | FLOAT
                 | TRUE
                 | FALSE
                 | ID'''
        if len(p) == 4:  # ( expr )
            p[0] = p[2]
        elif isinstance(p[1], float):  # FLOAT
            p[0] = {
                'val': p[1],
                'type': self.NUMB_TYPE,
                'code': '',
                'place': str(p[1])
            }
        elif p[1] is True or p[1] is False:  # Boolean literals
            p[0] = {
                'val': p[1],
                'type': self.BOOL_TYPE,
                'code': '',
                'place': str(p[1]).lower()
            }
        else:  # ID
            p[0] = {
                'val': self.get_var_value(p[1]),
                'type': self.get_var_type(p[1]),
                'code': '',
                'place': p[1]
            }
    
    def p_error(self, p):
        if p:
            print(f"Syntax error at '{p.value}'")
        else:
            print("Syntax error at EOF")
    
    def parse(self, text):
        try:
            return self.parser.parse(text, lexer=self.lexer)
        except Exception as e:
            print(f"Parsing error: {e}")
            return None

def main():
    # Nome fixo do arquivo de entrada
    filename = "input.txt"
    
    try:
        with open(filename, 'r') as file:
            calc = Calculator()
            text = file.read()
            calc.parse(text)
    except FileNotFoundError:
        print(f"Error: File '{filename}' not found")
        sys.exit(1)
    except Exception as e:
        print(f"Error: {str(e)}")
        sys.exit(1)

if __name__ == "__main__":
    main()