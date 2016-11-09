#include <iostream>
#include <string>
#include <sstream>
#include <memory>
#include <cctype>
#include <vector>
#include <map>
#include <fstream>
#include <algorithm>
#include <iomanip>

/*
 * PASCAL GRAMMAR RULES
 *
 * program: PROGRAM variable SEMI block DOT
 * block: declarations compound_statement
 * declarations: VAR (variable_declaration SEMI)+
 *             | empty
 * variable_declaration:  ID (COMMA ID)* COLON type_spec
 * type_spec: INTEGER
 * compound_statement: BEGIN statement_list END
 * statement_list: statement
 *               | statement SEMI statement_list
 * statement: compound_statement
 *          | assignment_statement
 *          | empty
 * assignment_statement: variable ASSIGN expr
 * empty:
 * expr: term ((PLUS | MINUS) term)*
 * term: factor ((MUL | INTEGER_DIV | FLOAT_DIV) factor)*
 * factor: PLUS factor
 *       | MINUS factor
 *       | INTEGER
 *       | LPAREN expr RPAREN
 *       | variable
 * variable: ID
 *
 */

/*
 * EXAMPLE PASCAL PROGRAM
 *
 * BEGIN
 *   BEGIN
 *     number := 2;
 *     a := number;
 *     b := 10 * a + 10 * number / 4;
 *     c := a - - b;
 *   END;
 *   x := 11;
 * END.
 *
 */

struct Tokens {
    enum Type {
        Integer,
        Real,
        Plus,
        Minus,
        Multiply,
        IntDivide,
        FloatDivide,
        LParen,
        RParen,
        Assign,
        Semicolon,
        Colon,
        Comma,
        Program,
        Var,
        Begin,
        End,
        Dot,
        ID,
        IntegerConst,
        RealConst,
        EndOfFile,
    };

    static std::string typeToString(Type t) {
        switch (t) {
        case Integer:
            return "INTEGER";
        case Real:
            return "REAL";
        case Plus:
            return "PLUS";
        case Minus:
            return "MINUS";
        case Multiply:
            return "MULT";
        case IntDivide:
            return "DIV";
        case FloatDivide:
            return "FLOAT_DIV";
        case LParen:
            return "LPAREN";
        case RParen:
            return "RPAREN";
        case Assign:
            return "ASSIGN";
        case Semicolon:
            return "SEMI";
        case Begin:
            return "BEGIN";
        case End:
            return "END";
        case Dot:
            return "DOT";
        case ID:
            return "ID";
        case EndOfFile:
            return "EOF";
        case Colon:
            return "COLON";
        case Comma:
            return "COMMA";
        case Program:
            return "PROGRAM";
        case Var:
            return "VAR";
        case IntegerConst:
            return "INT_CONST";
        case RealConst:
            return "REAL_CONST";
        }
        return "";
    }
};

struct ReservedKeywords {
    static const std::map<std::string, Tokens::Type> keywordMap;
};

const std::map<std::string, Tokens::Type> ReservedKeywords::keywordMap = {
    {"program", Tokens::Program},
    {"begin", Tokens::Begin},
    {"end", Tokens::End},
    {"div", Tokens::IntDivide},
    {"var", Tokens::Var},
    {"integer", Tokens::Integer},
    {"real", Tokens::Real},
};

template <typename K, typename V>
std::ostream& operator<< (std::ostream& os, const std::map<K, V>& m) {
    os << "{ ";
    auto it = begin(m);
    if (it != end(m)) {
        os << it->first << ": " << it->second;
    }
    while (++it != end(m)) {
        os << ", " << it->first << ": " << it->second;
    }
    return (os << " }");
}

class Token {
public:
    static Token fromReservedKeywordOrId(const std::string& str) {
        std::string lowerStr = str;
        std::transform(begin(lowerStr), end(lowerStr), begin(lowerStr), ::tolower);

        auto it = ReservedKeywords::keywordMap.find(lowerStr);
        if (it != ReservedKeywords::keywordMap.end()) {
            return Token(it->second);
        }
        return Token(Tokens::ID, lowerStr);
    }

public:
    Token():_type(Tokens::EndOfFile) {}
    Token(Tokens::Type type):_type(type) {}
    Token(Tokens::Type type, const std::string& value):_type(type),_value(value) {
    }

    std::string description() const {
        std::stringstream ss;
        ss << "Token(" << Tokens::typeToString(_type) << ", " << _value << ")";

        return ss.str();
    }

    Tokens::Type type() const {
        return _type;
    }

    std::string value() const {
        return _value;
    }

    template <typename T>
    T value() const {
        T ret;
        std::stringstream ss(_value);
        ss >> ret;
        return ret;
    }

    int valueAsInt() const {
        return value<int>();
    }

    bool isTypeOf(Tokens::Type type) const {
        return _type == type;
    }

    template <typename T, typename... Targs>
    bool isAnyTypeOf(T type, Targs... types) const {
        return isTypeOf(type) || isAnyTypeOf(types...);
    }

    bool isAnyTypeOf(Tokens::Type type) const {
        return isTypeOf(type);
    }

private:
    Tokens::Type _type;
    std::string _value;
};

struct interpreter_result_t {
    double value;
};

std::ostream& operator<< (std::ostream& os, const interpreter_result_t& res) {
    return (os << res.value);
}

class Lexer {
public:
    Lexer(const std::string& text):
        _text(text),_pos(0)
    {
        _currentChar = text[_pos];
        _eof = false;
    }

    Lexer(const Lexer& lexer):
        _text(lexer._text),_pos(0)
    {
        _currentChar = _text[_pos];
        _eof = false;
    }

    void trace() const {
        constexpr size_t max_chars = 72;
        constexpr size_t from_end = 16;
        constexpr size_t padding = 0;

        std::cout << "_pos: " << _pos << ", _currentChar: '" << _currentChar << "'" << std::endl;

        std::string error_text(max_chars + padding, ' ');
        std::string error_pos(max_chars + padding, ' ');

        int e_pos = std::min<int>(_text.size(), _pos + from_end);
        int s_pos = std::max<int>(0, e_pos - max_chars);

        auto start_pos = next(begin(_text), s_pos);
        auto end_pos = next(start_pos, e_pos - s_pos);

        std::fill_n(begin(error_text), padding, ' ');
        std::copy(start_pos, end_pos, next(begin(error_text), padding));
        error_pos[padding + ((_pos - s_pos) % max_chars)] = '^';

        std::cout << error_text << std::endl;
        std::cout << error_pos << std::endl;
        std::cout.flush();
    };

    void error() {
        throw std::runtime_error("Invalid character");
    }

    void advance() {
        _pos += 1;
        if (_pos > _text.size() - 1) {
            _currentChar = 0;
            _eof = true;
        } else {
            _currentChar = _text[_pos];
        }
    }

    void skipComment() {
        while (_currentChar != '}') {
            advance();
        }
        advance();
    }

    void skipWhitespace() {
        while (!_eof && isspace(_currentChar)) {
            advance();
        }
    }

    Token number() {
        std::stringstream ss;

        while (!_eof && isdigit(_currentChar)) {
            ss << _currentChar;
            advance();
        }

        Tokens::Type tokenType = Tokens::RealConst;

        if (_currentChar == '.') {
            ss << _currentChar;
            advance();

            while (!_eof && isdigit(_currentChar)) {
                ss << _currentChar;
                advance();
            }

            tokenType = Tokens::RealConst;
        }

        auto token = Token(tokenType, ss.str());
        //std::cout << token.description() << std::endl;
        return token;
    }

    char peek() {
        auto peek_pos = _pos + 1;
        if (peek_pos > _text.size() - 1) {
            return 0;
        }
        return _text[peek_pos];
    }

    Token getNextToken() {
        while (!_eof) {
            if (isspace(_currentChar)) {
                skipWhitespace();
                continue;
            }

            if (isalpha(_currentChar) || _currentChar == '_') {
                return _id();
            }

            if (_currentChar == ':' && peek() == '=') {
                advance();
                advance();
                return Token(Tokens::Assign);
            }

            if (_currentChar == ';') {
                advance();
                return Token(Tokens::Semicolon);
            }

            if (_currentChar == '.') {
                advance();
                return Token(Tokens::Dot);
            }

            if (_currentChar == '{') {
                advance();
                skipComment();
                continue;
            }

            if (isdigit(_currentChar)) {
                return number();
            }

            if (_currentChar == ':') {
                advance();
                return Token(Tokens::Colon);
            }

            if (_currentChar == ',') {
                advance();
                return Token(Tokens::Comma);
            }

            if (_currentChar == '+') {
                advance();
                return Token(Tokens::Plus);
            }

            if (_currentChar == '-') {
                advance();
                return Token(Tokens::Minus);
            }

            if (_currentChar == '*') {
                advance();
                return Token(Tokens::Multiply);
            }

            if (_currentChar == '(') {
                advance();
                return Token(Tokens::LParen);
            }

            if (_currentChar == ')') {
                advance();
                return Token(Tokens::RParen);
            }

            if (_currentChar == '/') {
                advance();
                return Token(Tokens::FloatDivide);
            }

            error();
        }

        return Token(Tokens::EndOfFile);
    }

private:
    Token _id() {
        std::stringstream ss;
        while (!_eof && (isalnum(_currentChar) || _currentChar == '_')) {
            ss << _currentChar;
            advance();
        }

        return Token::fromReservedKeywordOrId(ss.str());
    }

private:
    std::string _text;
    int _pos;
    Token _currentToken;
    char _currentChar;
    bool _eof;
};

class VisitorNode;
class AST;
class BinOp;
class UnaryOp;
class Num;
class Compound;
class Assign;
class Var;
class NoOp;
class Program;
class Block;
class VarDecl;
class Type;

class NodeVisitor {
public:
    virtual void visit(const VisitorNode& node) = 0;
    virtual void visit(const BinOp& node) = 0;
    virtual void visit(const UnaryOp& node) = 0;
    virtual void visit(const Num& node) = 0;
    virtual void visit(const Compound& node) = 0;
    virtual void visit(const Assign& node) = 0;
    virtual void visit(const Var& node) = 0;
    virtual void visit(const NoOp& node) = 0;
    virtual void visit(const Program& node) = 0;
    virtual void visit(const Block& node) = 0;
    virtual void visit(const VarDecl& node) = 0;
    virtual void visit(const Type& node) = 0;
};

class VisitorNode {
public:
    virtual void accept(NodeVisitor& v) const {
        v.visit(*this);
    }
};

class AST: public VisitorNode {
public:
    AST() {}
    virtual ~AST() {}

    virtual std::string description() const {
        return "AST(empty)";
    }

    virtual void accept(NodeVisitor& v) const {
        v.visit(*this);
    }
};

class BinOp: public AST {
public:
    BinOp(std::unique_ptr<AST> left, Tokens::Type op, std::unique_ptr<AST> right):
        _left(std::move(left)),_op(op),_right(std::move(right)) {
    }

    const AST& getLeft() const {
        return *_left;
    }

    const AST& getRight() const {
        return *_right;
    }

    const Tokens::Type& getOp() const {
        return _op;
    }

    virtual std::string description() const {
        std::stringstream ss;
        ss << "BinOp(" << _left->description() << ", ";
        ss << Tokens::typeToString(_op) << ", ";
        ss << _right->description() << ")";
        return ss.str();
    }

    virtual void accept(NodeVisitor& v) const {
        v.visit(*this);
    }
private:
    std::unique_ptr<AST> _left;
    std::unique_ptr<AST> _right;
    Tokens::Type _op;
};

class UnaryOp: public AST {
public:
    UnaryOp(Tokens::Type op, std::unique_ptr<AST> expr):
        _op(op),_expr(std::move(expr)) {
    }

    Tokens::Type getOp() const {
        return _op;
    }

    const AST& getExpr() const {
        return *_expr;
    }

    virtual void accept(NodeVisitor& v) const {
        v.visit(*this);
    }

private:
    Tokens::Type _op;
    std::unique_ptr<AST> _expr;
};

class Num: public AST {
public:
    Num(Token token):_value(0) {
        if (token.type() == Tokens::IntegerConst) {
            _value = token.valueAsInt();
        } else {
            _value = token.value<double>();
        }
    }

    double getValue() const {
        return _value;
    }

    virtual std::string description() const {
        std::stringstream ss;
        ss << "Num(" << _value << ")";
        return ss.str();
    }

    virtual void accept(NodeVisitor& v) const {
        v.visit(*this);
    }
private:
    double _value;
};

class Compound: public AST {
public:
    typedef std::vector<std::unique_ptr<AST> >::const_iterator const_iterator;
public:
    Compound():_children() {}
    Compound(std::vector<std::unique_ptr<AST> > nodes):
        _children(std::move(nodes)) {
    }

    const_iterator begin() const {
        return _children.begin();
    }

    const_iterator cbegin() const {
        return _children.cbegin();
    }

    const_iterator end() const {
        return _children.end();
    }

    const_iterator cend() const {
        return _children.end();
    }

    virtual void accept(NodeVisitor& v) const {
        v.visit(*this);
    }
private:
    std::vector<std::unique_ptr<AST> > _children;
};

class Assign: public AST {
public:
    Assign(std::unique_ptr<Var> left, Tokens::Type op, std::unique_ptr<AST> right):
        _left(std::move(left)),_right(std::move(right)),_op(op) {
    }

    const Var& getLeft() const {
        return *_left;
    }

    const AST& getRight() const {
        return *_right;
    }

    Tokens::Type getOp() const {
        return _op;
    }

    virtual void accept(NodeVisitor& v) const {
        v.visit(*this);
    }
private:
    std::unique_ptr<Var> _left;
    std::unique_ptr<AST> _right;
    Tokens::Type _op;
};

class Var: public AST {
public:
    Var(Token token):_token(token) {}

    Token getToken() const {
        return _token;
    }

    std::string getValue() const {
        return _token.value();
    }

    virtual void accept(NodeVisitor& v) const {
        v.visit(*this);
    }
private:
    Token _token;
};

class NoOp: public AST {
    virtual void accept(NodeVisitor& v) const {
        v.visit(*this);
    }
};

class Program: public AST {
public:
    Program(const std::string& name, std::unique_ptr<Block> block):
        _name(name), _block(std::move(block))
    {
    }

    const std::string& getName() const {
        return _name;
    }

    const Block& getBlock() const {
        return *_block;
    }

    virtual void accept(NodeVisitor& v) const {
        v.visit(*this);
    }

private:
    std::string _name;
    std::unique_ptr<Block> _block;
};

class Block: public AST {
public:
    Block(std::vector<std::unique_ptr<AST>> declarations, std::unique_ptr<AST> compoundStatement):
        _declarations(std::move(declarations)), _compoundStatement(std::move(compoundStatement))
    {
    }

    const std::vector<std::unique_ptr<AST>>& getDeclarations() const {
        return _declarations;
    }

    const AST& getCompoundStatement() const {
        return *_compoundStatement;
    }

    virtual void accept(NodeVisitor& v) const {
        v.visit(*this);
    }

private:
    std::vector<std::unique_ptr<AST>> _declarations;
    std::unique_ptr<AST> _compoundStatement;
};

class VarDecl: public AST {
public:
    VarDecl(std::unique_ptr<Var> var, std::unique_ptr<Type> type):
        _var(std::move(var)), _type(std::move(type))
    {
    }

    const Var& getVar() const {
        return *_var;
    }

    const Type& getType() const {
        return *_type;
    }

    virtual void accept(NodeVisitor& v) const {
        v.visit(*this);
    }

private:
    std::unique_ptr<Var> _var;
    std::unique_ptr<Type> _type;
};

class Type: public AST {
public:
    Type(Token token):_token(token) {
    }

    Token getToken() const {
        return _token;
    }

    virtual void accept(NodeVisitor& v) const {
        v.visit(*this);
    }

private:
    Token _token;
};

class Parser {
public:
    Parser(Lexer lexer):_lexer(lexer) {
        _currentToken = _lexer.getNextToken();
    }

    Parser(const Parser& parser):_lexer(parser._lexer) {
        _currentToken = _lexer.getNextToken();
    }

    void error() {
        _lexer.trace();
        throw std::runtime_error("Invalid syntax");
    }

    void eat(Tokens::Type type) {
        //std::cout << _currentToken.description() << std::endl;
        if (_currentToken.type() == type) {
            auto nextToken = _lexer.getNextToken();

            _currentToken = nextToken;
        } else {
            std::cerr << "ERROR: Expected " << Tokens::typeToString(type) << " but got " << Tokens::typeToString(_currentToken.type()) << std::endl;
            error();
        }
    }

    std::unique_ptr<AST> factor() {
        Token token = _currentToken;
        if (token.type() == Tokens::Plus) {
            eat(Tokens::Plus);
            return std::make_unique<UnaryOp>(token.type(), factor());
        } else if (token.type() == Tokens::Minus) {
            eat(Tokens::Minus);
            return std::make_unique<UnaryOp>(token.type(), factor());
        } else if (token.type() == Tokens::IntegerConst) {
            eat(Tokens::IntegerConst);
            return std::make_unique<Num>(token);
        } else if (token.type() == Tokens::RealConst) {
            eat(Tokens::RealConst);
            return std::make_unique<Num>(token);
        } else if (token.type() == Tokens::LParen) {
            eat(Tokens::LParen);
            auto node = expr();
            eat(Tokens::RParen);
            return node;
        }
        return variable();
    }

    std::unique_ptr<AST> term() {
        auto node = factor();

        while (_currentToken.isAnyTypeOf(Tokens::Multiply, Tokens::IntDivide, Tokens::FloatDivide)) {
            Token token = _currentToken;
            if (token.type() == Tokens::Multiply) {
                eat(Tokens::Multiply);
            } else if (token.type() == Tokens::IntDivide) {
                eat(Tokens::IntDivide);
            } else if (token.type() == Tokens::FloatDivide) {
                eat(Tokens::FloatDivide);
            }

            node = std::make_unique<BinOp>(std::move(node), token.type(), std::move(factor()));
        }

        return node;
    }

    std::unique_ptr<AST> expr() {
        auto node = term();

        while (_currentToken.isAnyTypeOf(Tokens::Plus, Tokens::Minus)) {
            Token token = _currentToken;
            if (token.type() == Tokens::Plus) {
                eat(Tokens::Plus);
            } else if (token.type() == Tokens::Minus) {
                eat(Tokens::Minus);
            }
            node = std::make_unique<BinOp>(std::move(node), token.type(), std::move(term()));
        }

        return node;
    }

    std::unique_ptr<Program> program() {
        eat(Tokens::Program);
        auto var_node = variable();
        eat(Tokens::Semicolon);
        auto block_node = block();
        auto program_node = std::make_unique<Program>(var_node->getValue(), std::move(block_node));
        eat(Tokens::Dot);
        return program_node;
    }

    std::unique_ptr<AST> compoundStatement() {
        eat(Tokens::Begin);
        auto nodes = statementList();
        eat(Tokens::End);

        std::unique_ptr<AST> compound = std::make_unique<Compound>(std::move(nodes));
        return compound;
    }

    std::vector<std::unique_ptr<AST> > statementList() {
        std::vector<std::unique_ptr<AST> > results;
        results.push_back(std::move(statement()));

        while (_currentToken.type() == Tokens::Semicolon) {
            eat(Tokens::Semicolon);
            results.push_back(statement());
        }

        if (_currentToken.type() == Tokens::ID) {
            error();
        }

        return results;
    }

    std::unique_ptr<AST> statement() {
        std::unique_ptr<AST> node;
        if (_currentToken.type() == Tokens::Begin) {
            node = compoundStatement();
        } else if (_currentToken.type() == Tokens::ID) {
            node = assignStatement();
        } else {
            node = empty();
        }
        return node;
    }

    std::unique_ptr<AST> assignStatement() {
        auto left = variable();
        auto token = _currentToken;
        eat(Tokens::Assign);
        auto right = expr();
        std::unique_ptr<AST> node = std::make_unique<Assign>(std::move(left), token.type(), std::move(right));

        return node;
    }

    std::unique_ptr<Var> variable() {
        auto node = std::make_unique<Var>(_currentToken);
        eat(Tokens::ID);

        return node;
    }

    std::unique_ptr<AST> parse() {
        std::unique_ptr<AST> node = program();
        if (_currentToken.type() != Tokens::EndOfFile) {
            error();
        }
        return node;
    }

    std::unique_ptr<NoOp> empty() {
        return std::make_unique<NoOp>();
    }

    std::unique_ptr<Block> block() {
        auto declaration_nodes = declarations();
        auto compound_statement_nodes = compoundStatement();
        auto node = std::make_unique<Block>(std::move(declaration_nodes), std::move(compound_statement_nodes));
        return node;
    }

    std::vector<std::unique_ptr<AST>> declarations() {
        std::vector<std::unique_ptr<AST>> decls;
        if (_currentToken.type() == Tokens::Var) {
            eat(Tokens::Var);
            while (_currentToken.type() == Tokens::ID) {
                auto var_decl = variableDeclaration();
                for (auto it = begin(var_decl); it != end(var_decl); it++) {
                    decls.push_back(std::move(*it));
                }
                eat(Tokens::Semicolon);
            }
        }
        return decls;
    }

    std::vector<std::unique_ptr<VarDecl>> variableDeclaration() {
        auto var = std::make_unique<Var>(_currentToken);

        std::vector<std::unique_ptr<Var>> var_nodes;
        var_nodes.push_back(std::move(var));
        eat(Tokens::ID);

        while (_currentToken.type() == Tokens::Comma) {
            eat(Tokens::Comma);
            auto node = std::make_unique<Var>(_currentToken);
            var_nodes.push_back(std::move(node));
            eat(Tokens::ID);
        }

        eat(Tokens::Colon);

        auto type_node = typeSpec();

        std::vector<std::unique_ptr<VarDecl>> var_decls;

        for (auto it = begin(var_nodes); it != end(var_nodes); it++) {
            auto decl = std::make_unique<VarDecl>(std::move(*it), std::make_unique<Type>(type_node->getToken()));
            var_decls.push_back(std::move(decl));
        }

        return var_decls;
    }

    std::unique_ptr<Type> typeSpec() {
        Token token = _currentToken;
        if (_currentToken.type() == Tokens::Integer) {
            eat(Tokens::Integer);
        } else {
            eat(Tokens::Real);
        }

        auto node = std::make_unique<Type>(token);
        return node;
    }

private:
    Lexer _lexer;
    Token _currentToken;
};

class Interpreter: public NodeVisitor {
public:
    Interpreter(Parser& parser):_parser(parser) {
    }

    virtual void visit(const VisitorNode& node) {
        throw std::runtime_error("visitor method must be overridden for type");
    }

    virtual void visit(const Num& node) {
        _result.value = node.getValue();
    }

    virtual void visit(const UnaryOp& node) {
        node.getExpr().accept(*this);

        if (node.getOp() == Tokens::Minus) {
            _result.value = -_result.value;
        }
    }

    virtual void visit(const BinOp& node) {
        node.getLeft().accept(*this);
        auto left = _result.value;

        node.getRight().accept(*this);
        auto right = _result.value;

        auto type = node.getOp();
        if (type == Tokens::Plus) {
            _result.value = left + right;
        } else if (type == Tokens::Minus) {
            _result.value = left - right;
        } else if (type == Tokens::Multiply) {
            _result.value = left * right;
        } else if (type == Tokens::IntDivide) {
            _result.value = (int) (left / right);
        } else if (type == Tokens::FloatDivide) {
            _result.value = left / right;
        }
    }

    virtual void visit(const Compound& node) {
        for (auto it = std::begin(node); it != std::end(node); it++) {
            it->get()->accept(*this);
        }
    }

    virtual void visit(const Assign& node) {
        auto name = node.getLeft().getValue();
        node.getRight().accept(*this);
        _globalScope[name] = _result.value;
    }

    virtual void visit(const Var& node) {
        auto name = node.getValue();
        _result.value = _globalScope[name];
    }

    virtual void visit(const NoOp& node) {
    }

    virtual void visit(const Program& node) {
        node.getBlock().accept(*this);
    }

    virtual void visit(const Block& node) {
        for (auto it = begin(node.getDeclarations()); it != end(node.getDeclarations()); it++) {
            (*it)->accept(*this);
        }
        node.getCompoundStatement().accept(*this);
    }

    virtual void visit(const VarDecl& node) {
    }

    virtual void visit(const Type& node) {
    }

    interpreter_result_t interpret() {
        _result = interpreter_result_t { 0 };
        auto node = _parser.parse();
        node->accept(*this);
        return _result;
    }

    void printGlobalScope() const {
        std::cout << _globalScope << std::endl;
    }

private:
    Parser _parser;
    interpreter_result_t _result;
    std::map<std::string, double> _globalScope;
};

int main(int argc, char** argv) {

    if (argc != 2) {
        std::cout << "Usage: " << argv[0] << " FILENAME" << std::endl;
        return 1;
    }

    std::ifstream file(argv[1]);
    if (!file) {
        std::cerr << "Unable to open file '" << argv[1] << "'" << std::endl;
        return 1;
    }

    std::stringstream ss;
    std::string s;
    while (file) {
        std::getline(file, s);
        ss << s;
    }

    Lexer lexer(ss.str());
    Parser parser(lexer);
    Interpreter interpreter(parser);
    interpreter.interpret();
    interpreter.printGlobalScope();

    return 0;
}

