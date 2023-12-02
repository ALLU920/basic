#############################
# IMPORTS
#############################

from string_with_arrows import *

#############################
# CONSTANTS
#############################

DIGITS = '0123456789'

#############################
# ERRORS
#############################

class Error:
    def __init__(self, pos_start, pos_end, error_name,details):
        self.pos_start = pos_start
        self.pos_end = pos_end 
        self.error_name = error_name
        self.details = details

    # string method just constructs a string
    def as_string(self):
        result = f'{self.error_name}: {self.details}'
        result += f'\nFile {self.pos_start.fn}, line {self.pos_start.ln + 1}'  # adds the filename and line to error
        return result
    
# sub class for this error class
class IllegalCharError(Error):
    def __init__(self, pos_start, pos_end, details):
        super().__init__(pos_start, pos_end, 'Illegal Character', details)

class InvalidSyntaxError(Error):
    def __init__(self, pos_start, pos_end, details=''):
        super().__init__(pos_start, pos_end, 'Invalid Syntax Error', details)

    
#############################
# POSITION                                           # keeps track of the line,column no. and index for better 
#############################                        # error detection 
class Position:
    def __init__(self, idx, ln, col, fn, ftxt):
        self.idx = idx
        self.ln = ln
        self.col = col 
        self.fn = fn
        self.ftxt = ftxt

    def advance(self, current_char=None):
        self.idx +=1
        self.col += 1

        #check for a new line and reset pos to 0
        if current_char == '\n':
            self.ln += 1
            self.col = 0

        return self
    
    # copy method that creates a copy of the position
    def copy(self):
        return Position(self.idx, self.ln, self.col, self.fn, self.ftxt)



#############################
# TOKENS 
#############################

# Constants for all the token types
TT_INT = 'INT'
TT_FLOAT = 'FLOAT'
TT_PLUS = 'PLUS'
TT_MINUS = 'MINUS'
TT_MUL = 'MUL'
TT_DIV = 'DIV'
TT_LPAREN = 'LPAREN'
TT_RPAREN = 'RPAREN'
TT_EOF = 'EOF'

class Token:
    def __init__(self, type_, value=None, pos_start=None, pos_end=None):
        self.type = type_
        self.value = value

        if pos_start:
            self.pos_start = pos_start.copy()
            # if end is not given assume its + 1
            self. pos_end = pos_start.copy()
            self.pos_end.advance()


        if pos_end:
            self.pos_end = pos_end

    # representation method that prints the value and the type if the token has a value otherwise just the type
    def __repr__(self):
        if self.value:return f'{self.type}:{self.value}'
        return f'{self.type}'
    
######################################
# LEXER
######################################

class Lexer:
    def __init__(self, fn, text):
        self.fn = fn
        self.text = text
        self.pos = Position(-1, 0, -1, fn, text)     # keeps track of current position 
        self.current_char = None                     # keeps track of current character 
        self.advance()                               # increments to zero

    # advance method that advances to the next character in the text
    def advance(self):
        self.pos.advance(self.current_char)
        self.current_char = self.text[self.pos.idx] if self.pos.idx < len(self.text) else None  # checks for length of text

    # make_tokens method
    def make_tokens(self):
        tokens = []                                  # list to store the tokens 

        while self.current_char != None:
            if self.current_char in ' \t':           # if character is space or tab advance
                self.advance()
            elif self.current_char in DIGITS:
                tokens.append(self.make_number())    # the function make number makes integer or float token
            elif self.current_char == '+':
                tokens.append(Token(TT_PLUS, pos_start=self.pos))
                self.advance()
            elif self.current_char == '-':
                tokens.append(Token(TT_MINUS, pos_start=self.pos))
                self.advance()
            elif self.current_char == '*':
                tokens.append(Token(TT_MUL, pos_start=self.pos))
                self.advance()
            elif self.current_char == '/':
                tokens.append(Token(TT_DIV, pos_start=self.pos))
                self.advance()
            elif self.current_char == '(':
                tokens.append(Token(TT_LPAREN, pos_start=self.pos))
                self.advance()
            elif self.current_char == ')':
                tokens.append(Token(TT_RPAREN, pos_start=self.pos))
                self.advance()
            else:
                # return some error for illegal character
                pos_start = self.pos.copy()
                char = self.current_char
                self.advance()
                return [], IllegalCharError(pos_start, self.pos, "'" + char + "'")


        tokens.append(Token(TT_EOF, pos_start=self.pos))   # end of file token 
        return tokens, None
    
    # make number function that will return either an integer or float token
    def make_number(self):
        num_str = ''
        dot_count = 0                                # dot count is to check if its float or not 
        pos_start = self.pos.copy()


        while self.current_char != None and self.current_char in DIGITS + '.':
            if self.current_char == '.':
                if dot_count == 1: break             # if the character is a dot add it to count and number string
                dot_count+=1                         # and if it is 1 break. Because number doesnt have more than
                num_str += '.'                       # two dots
            else:
                num_str += self.current_char
            self.advance()

        # we check dot count to decide if its integer or float
        if dot_count == 0:
            return Token(TT_INT, int(num_str), pos_start, self.pos)
        else:
            return Token(TT_FLOAT, float(num_str), pos_start, self.pos)
                     
########################################
# NODES                                               # we need to define nodes as we generate tree 
########################################

class NumberNode:
    def __init__(self, tok):
        self.tok = tok

        self.pos_start = self.tok.pos_start
        self.pos_end = self.tok.pos_end
    
    # representation method that returns the token in a string
    def __repr__(self):
        return f'{self.tok}'
    
class BinOpNode:
    def __init__(self, left_node, op_tok, right_node):
        self.left_node = left_node
        self.op_tok = op_tok
        self.right_node = right_node

        self.pos_start = self.left_node.pos_start
        self.pos_end = self.right_node.pos_end


    # represents node in a string
    def __repr__(self):
        return f'({self.left_node}, {self.op_tok}, {self.right_node})'

class UnaryOpNode:
    def __init__(self, op_tok, node):
        self.op_tok = op_tok
        self.node = node

        self.pos_start = self.op_tok.pos_start
        self.pos_end = node.pos_end


    def __repr__(self):
        return f'({self.op_tok}, {self.node})'
    
########################################
# PARSE RESULT
########################################

class ParseResult:
    def __init__(self):
        self.error = None
        self.node = None

    def register(self, res):
        if isinstance(res, ParseResult):
            if res.error : self.error = res.error
            return res.node
        
        return res 

        

    def success(self, node):
        self.node = node 
        return self


    def failure(self, error):
        self.error = error
        return self




########################################
# PARSER
########################################

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.tok_idx = -1                             # keeps track of tokens
        self.advance()

    def advance(self, ):
        self.tok_idx +=1
        if self.tok_idx < len(self.tokens):
            self.current_tok = self.tokens[self.tok_idx]
        return self.current_tok 

    def parse(self):
        res = self.expr()
        if not res.error and self.current_tok.type != TT_EOF:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end, 
                "Expected '+', '-', '*' or '/'"
            ))
        return res 

######################################################
  
    def factor(self):
        res = ParseResult()
        tok = self.current_tok

        if tok.type in (TT_PLUS, TT_MINUS):
            res.register(self.advance())
            factor = res.register(self.factor())
            if res.error: return res
            return res.success(UnaryOpNode(tok, factor))
        

        elif tok.type in (TT_INT, TT_FLOAT):
            res.register(self.advance())
            return res.success(NumberNode(tok))
        
        elif tok.type == TT_LPAREN:
            res.register(self.advance())
            expr = res.register(self.expr())
            if res.error: return res
            if self.current_tok.type == TT_RPAREN:
                res.register(self.advance())
                return res.success(expr)
            else:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected ')'"
                ))
        
        # if you dont have a response node

        return res.failure(InvalidSyntaxError(
            tok.pos_start, tok.pos_end,
            "Expected int or float"
        ))

    def term(self):
        return self.bin_op(self.factor, (TT_MUL, TT_DIV))

    def expr(self):
        return self.bin_op(self.term, (TT_PLUS, TT_MINUS))

    # this is used by both the functions above
    def bin_op(self, func, ops):
        res = ParseResult()
        left = res.register(func())                  # register takes in parse result and takes out the node from
                                                      # it and not the entire parse result.
        if res.error: return res                      # if error return this 
        while self.current_tok.type in ops:
            op_tok = self.current_tok
            res.register(self.advance())
            right = res.register(func())
            if res.error: return res                  # if error returns it early 
            left = BinOpNode(left, op_tok, right)

        return res.success(left)

########################################
# VALUES
########################################

#stores numbers and operates them on other numbers 
class Number:
    def __init__(self, value):
        self.value = value
        self.set_pos()

    def set_pos(self, pos_start = None, pos_end=None):
        self.pos_start = pos_start
        self.pos_end = pos_end
        return self 
    
    #takes another numbers and adds it 
    def added_to(self, other):
        if isinstance(other, Number):
            return Number(self.value + other.value)
        
    #takes another numbers and subtracts it 
    def subbed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value - other.value)
        
    #takes another numbers and multiplies it 
    def multed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value * other.value)
        
    #takes another numbers and divides it 
    def divided_by(self, other):
        if isinstance(other, Number):
            return Number(self.value / other.value)
        
    def __repr__(self):
        return str(self.value)
        


########################################
# INTERPRETER
########################################

class Interpreter:
    #visit processes a node and visits all the child nodes
    def visit(self, node):
        method_name = f'visit_{type(node).__name__}'  # aurtomatically creates visit_binOPNode etc
        method = getattr(self, method_name, self.no_visit_method)
        return method(node)
    
    def no_visit_method(self, node):
        raise Exception(f'No visit_{type(node).__name__} method defined')
    
    ############################################################

    def visit_NumberNode(self, node):
        return Number(node.tok.value).set_pos(node.pos_start, node.pos_end)

    def visit_BinOpNode(self, node):
        
        #need to visit left and right node of bin op node
        left = self.visit(node.left_node)
        right = self.visit(node.right_node)

        if node.op_tok.type == TT_PLUS:
            result = left.added_to(right)
        elif node.op_tok.type == TT_MINUS:
            result = left.subbed_by(right)
        elif node.op_tok.type == TT_MUL:
            result = left.multed_by(right)
        elif node.op_tok.type == TT_DIV:
            result = left.divided_by(right)

        return result.set_pos(node.pos_start, node.pos_end)
    
    def visit_UnaryOpNode(self, node):
        number = self.visit(node.node)

        if node.op_tok.type == TT_MINUS:
            number = number.multed_by(Number(-1))

        return number.set_pos(node.pos_start, node.pos_end)



    

########################################
# RUN                                                 # just takes the text and runs it 
########################################
 
def run(fn, text):
    # generate rokens 
    lexer = Lexer(fn, text)
    tokens, error = lexer.make_tokens()  

    #checking for error before making tree  
    if error: return None, error

    # Generate AST (ABSTRACT SYNTAX TREE)

    parser = Parser(tokens)
    ast = parser.parse()
    if ast.error: return None, ast.error

    # Run program 
    interpreter = Interpreter()
    result = interpreter.visit(ast.node)

    return result, None




    
