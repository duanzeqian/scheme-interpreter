/**
 * @file parser.cpp
 * @brief Parsing implementation for Scheme syntax tree to expression tree conversion 解析
 * 
 * This file implements the parsing logic that converts syntax trees into
 * expression trees that can be evaluated.
 * primitive operations, and function applications.
 */

#include "RE.hpp"
#include "Def.hpp"
#include "syntax.hpp"
#include "value.hpp"
#include "expr.hpp"
#include <map>
#include <string>
#include <iostream>

#define mp make_pair
using std::string;
using std::vector;
using std::pair;

extern std::map<std::string, ExprType> primitives;
extern std::map<std::string, ExprType> reserved_words;


Expr syntax_To_Expr(const Syntax &stx,Assoc &env);
bool isKeywordRedefined(const std::string& keyword, Assoc& env) {
    Value keyword_value = find(keyword, env);
    return (keyword_value.get() != nullptr);
}
Expr handleRedefinedKeyword(const std::string& keyword, const std::vector<Syntax>& stxs, Assoc& env) {
    vector<Expr> args;
    for(size_t i = 1; i < stxs.size(); ++i) {
        args.push_back(syntax_To_Expr(stxs[i], env));
    }
    return Expr(new Apply(new Var(keyword), args));
}

/**
 * @brief Default parse method (should be overridden by subclasses)
 */
Expr Syntax::parse(Assoc &env) {
    throw RuntimeError("Unimplemented parse method");
}

Expr Number::parse(Assoc &env) {
    return Expr(new Fixnum(n));
}

Expr RationalSyntax::parse(Assoc &env) {
    //TODO: complete the rational parser
    return Expr(new RationalNum(numerator,denominator));
}

Expr SymbolSyntax::parse(Assoc &env) {
    return Expr(new Var(s));
}

Expr StringSyntax::parse(Assoc &env) {
    return Expr(new StringExpr(s));
}

Expr TrueSyntax::parse(Assoc &env) {
    return Expr(new True());
}

Expr FalseSyntax::parse(Assoc &env) {
    return Expr(new False());
}

Expr syntax_To_Expr(const Syntax &stx,Assoc &env)
{
    if(auto num = dynamic_cast<Number*>(stx.get()))return Expr(new Fixnum(num->n));
    else if(auto rat = dynamic_cast<RationalSyntax*>(stx.get())) return Expr(new RationalNum(rat->numerator, rat->denominator));
    else if(auto sym = dynamic_cast<SymbolSyntax*>(stx.get())) return Expr(new Var(sym->s));
    else if(auto str = dynamic_cast<StringSyntax*>(stx.get())) return Expr(new StringExpr(str->s));
    else if(dynamic_cast<TrueSyntax*>(stx.get())) return Expr(new True());
    else if(dynamic_cast<FalseSyntax*>(stx.get())) return Expr(new False());
    else if(auto lst = dynamic_cast<List*>(stx.get())) return lst->parse(env);
    else throw RuntimeError("Unknown syntax type");
}

Expr List::parse(Assoc &env) {
    if (stxs.empty()) {
        return Expr(new Quote(Syntax(new List())));
    }
    //TODO: check if the first element is a symbol
    //If not, use Apply function to package to a closure;
    //If so, find whether it's a variable or a keyword;
    SymbolSyntax *id = dynamic_cast<SymbolSyntax*>(stxs[0].get());
    if (id == nullptr) {
        //TODO: TO COMPLETE THE LOGIC(Apply)
        vector<Expr> args;
        for(size_t i=1; i<stxs.size(); ++i) args.push_back(syntax_To_Expr(stxs[i],env));
        return Expr(new Apply(syntax_To_Expr(stxs[0],env),args));//package it as a function
    }else{
    string op = id->s;
    Value match_value = find(op, env);
    if (match_value.get() != nullptr) {
        //TODO: TO COMPLETE THE PARAMETER PARSER LOGIC(op is a variable in the environment)
        vector<Expr> args;
        for(size_t i=1; i<stxs.size(); ++i) args.push_back(syntax_To_Expr(stxs[i],env));
        return Expr(new Apply(new Var(op),args));
    }
    if (primitives.count(op) != 0) {
        vector<Expr> parameters;
        //TODO: TO COMPLETE THE PARAMETER PARSER LOGIC
        for(size_t i=1; i<stxs.size(); ++i) parameters.push_back(syntax_To_Expr(stxs[i],env));

        ExprType op_type = primitives[op];
        if (op_type == E_PLUS) {
            if (parameters.size() == 2) {
                return Expr(new Plus(parameters[0], parameters[1])); 
            } else {
                return Expr(new PlusVar(parameters));
            }
        } else if (op_type == E_MINUS) {
            //TODO: TO COMPLETE THE LOGIC
            if (parameters.size() == 0) throw RuntimeError("Wrong number of arguments for -");
            if (parameters.size() == 2) {
                return Expr(new Minus(parameters[0], parameters[1])); 
            } else {
                return Expr(new MinusVar(parameters));
            }
        } else if (op_type == E_MUL) {
            //TODO: TO COMPLETE THE LOGIC
            if (parameters.size() == 2) {
                return Expr(new Mult(parameters[0], parameters[1])); 
            } else {
                return Expr(new MultVar(parameters));
            }
        }  else if (op_type == E_DIV) {
            //TODO: TO COMPLETE THE LOGIC
            if (parameters.size() == 0) throw RuntimeError("Wrong number of arguments for /");
            if (parameters.size() == 2) {
                return Expr(new Div(parameters[0], parameters[1])); 
            } else {
                return Expr(new DivVar(parameters));
            }
        } else if (op_type == E_MODULO) {
            if (parameters.size() != 2) {
                throw RuntimeError("Wrong number of arguments for modulo");
            }
            return Expr(new Modulo(parameters[0], parameters[1]));
        } else if (op_type == E_LIST) {
            return Expr(new ListFunc(parameters));
        } else if (op_type == E_LT) {
            //TODO: TO COMPLETE THE LOGIC
            if (parameters.size() <= 1) throw RuntimeError("Wrong number of arguments for <");
            if (parameters.size() == 2) {
                return Expr(new Less(parameters[0], parameters[1])); 
            } else {
                return Expr(new LessVar(parameters));
            }
        } else if (op_type == E_LE) {
            //TODO: TO COMPLETE THE LOGIC
            if (parameters.size() <= 1) throw RuntimeError("Wrong number of arguments for <=");
            if (parameters.size() == 2) {
                return Expr(new LessEq(parameters[0], parameters[1])); 
            } else {
                return Expr(new LessEqVar(parameters));
            }
        } else if (op_type == E_EQ) {
            //TODO: TO COMPLETE THE LOGIC
            if (parameters.size() <= 1) throw RuntimeError("Wrong number of arguments for =");
            if (parameters.size() == 2) {
                return Expr(new Equal(parameters[0], parameters[1])); 
            } else {
                return Expr(new EqualVar(parameters));
            }
        } else if (op_type == E_GE) {
            //TODO: TO COMPLETE THE LOGIC
            if (parameters.size() <= 1) throw RuntimeError("Wrong number of arguments for >=");
            if (parameters.size() == 2) {
                return Expr(new GreaterEq(parameters[0], parameters[1])); 
            } else {
                return Expr(new GreaterEqVar(parameters));
            }
        } else if (op_type == E_GT) {
            //TODO: TO COMPLETE THE LOGIC
            if (parameters.size() <= 1) throw RuntimeError("Wrong number of arguments for >");
            if (parameters.size() == 2) {
                return Expr(new Greater(parameters[0], parameters[1])); 
            } else {
                return Expr(new GreaterVar(parameters));
            }
        } else if (op_type == E_AND) {
            return Expr(new AndVar(parameters));
        } else if (op_type == E_OR) {
            return Expr(new OrVar(parameters));
        } else if (op_type == E_CAR) {
            if (parameters.size() == 1) {
                return Expr(new Car(parameters[0]));
            } else {
                throw RuntimeError("Wrong number of arguments for car");
            }
        } else if (op_type == E_CDR) {
            if (parameters.size() == 1) {
                return Expr(new Cdr(parameters[0]));
            } else {
                throw RuntimeError("Wrong number of arguments for cdr");
            }
        } else if (op_type == E_CONS) {
            if (parameters.size() == 2) {
                return Expr(new Cons(parameters[0], parameters[1]));
            } else {
                throw RuntimeError("Wrong number of arguments for cons");
            }
        } 
        else if (op_type == E_EXPT) {
            if (parameters.size() == 2) {
                return Expr(new Expt(parameters[0], parameters[1]));
            } else {
                throw RuntimeError("Wrong number of arguments for expt");
            }
        }
        else if (op_type == E_SETCAR) {
            if (parameters.size() == 2) {
                return Expr(new SetCar(parameters[0], parameters[1]));
            } else {
                throw RuntimeError("Wrong number of arguments for set-car!");
            }
        } else if (op_type == E_SETCDR) {
            if (parameters.size() == 2) {
                return Expr(new SetCdr(parameters[0], parameters[1]));
            } else {
                throw RuntimeError("Wrong number of arguments for set-cdr!");
            }
        }
        else if (op_type == E_NOT) {
            if (parameters.size() == 1) {
                return Expr(new Not(parameters[0]));
            } else {
                throw RuntimeError("Wrong number of arguments for not");
            }
        }
        else if (op_type == E_EQQ) {
            if (parameters.size() == 2) {
                return Expr(new IsEq(parameters[0], parameters[1]));
            } else {
                throw RuntimeError("Wrong number of arguments for eq?");
            }
        } else if (op_type == E_BOOLQ) {
            if (parameters.size() == 1) {
                return Expr(new IsBoolean(parameters[0]));
            } else {
                throw RuntimeError("Wrong number of arguments for boolean?");
            }
        } else if (op_type == E_INTQ) {
            if (parameters.size() == 1) {
                return Expr(new IsFixnum(parameters[0]));
            } else {
                throw RuntimeError("Wrong number of arguments for number?");
            }
        } else if (op_type == E_NULLQ) {
            if (parameters.size() == 1) {
                return Expr(new IsNull(parameters[0]));
            } else {
                throw RuntimeError("Wrong number of arguments for null?");
            }
        } else if (op_type == E_PAIRQ) {
            if (parameters.size() == 1) {
                return Expr(new IsPair(parameters[0]));
            } else {
                throw RuntimeError("Wrong number of arguments for pair?");
            }
        } else if (op_type == E_PROCQ) {
            if (parameters.size() == 1) {
                return Expr(new IsProcedure(parameters[0]));
            } else {
                throw RuntimeError("Wrong number of arguments for procedure?");
            }
        } else if (op_type == E_SYMBOLQ) {
            if (parameters.size() == 1) {
                return Expr(new IsSymbol(parameters[0]));
            } else {
                throw RuntimeError("Wrong number of arguments for symbol?");
            }
        } else if (op_type == E_LISTQ) {
            if (parameters.size() == 1) {
                return Expr(new IsList(parameters[0]));
            } else {
                throw RuntimeError("Wrong number of arguments for list?");
            }
        } else if (op_type == E_STRINGQ) {
            if (parameters.size() == 1) {
                return Expr(new IsString(parameters[0]));
            } else {
                throw RuntimeError("Wrong number of arguments for string?");
            }
        }
        else if (op_type == E_DISPLAY) {
            if (parameters.size() == 1) {
                return Expr(new Display(parameters[0]));
            } else {
                throw RuntimeError("Wrong number of arguments for display");
            }
        }
        else if (op_type == E_VOID) {
            if (parameters.size() == 0) {
                return Expr(new MakeVoid());
            } else {
                throw RuntimeError("Wrong number of arguments for void");
            }
        } else if (op_type == E_EXIT) {
            if (parameters.size() == 0) {
                return Expr(new Exit());
            } else {
                throw RuntimeError("Wrong number of arguments for exit");
            }
        }
         else {
            //TODO: TO COMPLETE THE LOGIC
            throw RuntimeError("Unknown primitive: " + op);
        }
    }

    if (reserved_words.count(op) != 0) {
        if(isKeywordRedefined(op,env)) return handleRedefinedKeyword(op,stxs,env);
    	switch (reserved_words[op]) {
			//TODO: TO COMPLETE THE reserve_words PARSER LOGIC
            case E_BEGIN:
            {
                vector<Expr> exps;
                for(size_t i=1; i<stxs.size(); ++i) exps.push_back(syntax_To_Expr(stxs[i],env));
                return Expr(new Begin(exps));
            }
            case E_QUOTE:
            {
                if(stxs.size() == 2) return Expr(new Quote(stxs[1]));
                else throw RuntimeError("Wrong number of arguments for Quote");
            }
            case E_IF:
            {
                if(stxs.size() == 4) return Expr(new If(syntax_To_Expr(stxs[1],env),syntax_To_Expr(stxs[2],env),syntax_To_Expr(stxs[3],env)));
                else throw RuntimeError("Wrong number of arguments for If");
            }
            case E_COND:
            {
                vector<vector<Expr> > clauses;
                for(size_t i=1; i<stxs.size(); ++i)
                {
                    if(auto lst = dynamic_cast<List*>(stxs[i].get()))
                    {
                        vector<Expr> clause;
                        for(auto &stx:lst->stxs) clause.push_back(syntax_To_Expr(stx,env));
                        clauses.push_back(clause);
                    }
                    else throw(RuntimeError("Wrong typename in Cond"));
                }
                return Expr(new Cond(clauses));
            }
            case E_LAMBDA:
            {
                if(stxs.size() < 3) throw RuntimeError("Wrong number of arguments for lambda");
                vector<string> parameters;
                if(auto lst = dynamic_cast<List*>(stxs[1].get()))
                {
                    for(auto &stx:lst->stxs)
                    {
                        if(auto sym = dynamic_cast<SymbolSyntax*>(stx.get())) 
                            parameters.push_back(sym->s);
                    }
                }
                Assoc lambda_env = env; 
                for(const auto& param : parameters) lambda_env = extend(param, VoidV(), lambda_env);//bind outside environment
                vector<Expr> exps;
                for(size_t i=2; i<stxs.size(); ++i) exps.push_back(syntax_To_Expr(stxs[i], lambda_env));
                return Expr(new Lambda(parameters, Expr(new Begin(exps))));
            }
            case E_DEFINE:
            {
                if(stxs.size() < 3) throw RuntimeError("Wrong number of arguments for define");
                //check if it is the simple form of function(name)
                if(auto lst = dynamic_cast<List*>(stxs[1].get()))
                {
                    if(!lst->stxs.empty())
                    {
                        if(SymbolSyntax* funcNameSym = dynamic_cast<SymbolSyntax*>(lst->stxs[0].get()))//get the name of the function
                        {
                            string funcName = funcNameSym->s;
                            //if(primitives.count(funcName) || reserved_words.count(funcName)) throw RuntimeError("Define: cannot redefine primitive or reserved word");
                            vector<string> parameters;
                            for(size_t i=1; i<lst->stxs.size(); ++i)
                            {
                                SymbolSyntax* paramSym = dynamic_cast<SymbolSyntax*>(lst->stxs[i].get());
                                if (paramSym != nullptr) parameters.push_back(paramSym->s);
                                else throw RuntimeError("Invalid parameter in function definition");
                            }
                            vector<Expr> exps;
                            for(size_t i=2; i<stxs.size(); ++i) exps.push_back(syntax_To_Expr(stxs[i],env));
                            Expr body=exps[0];//size=1
                            if(exps.size() > 1) body = Expr(new Begin(exps));
                            //create a new lambda and package it with Define
                            Expr lambda = Expr(new Lambda(parameters,body));
                            return Expr(new Define(funcName,lambda));
                        }
                    }
                }
                //now it must be a variable
                SymbolSyntax* varSym = dynamic_cast<SymbolSyntax*>(stxs[1].get());
                if(varSym == nullptr) throw RuntimeError("Invalid variable name in Define");
                string varName = varSym->s;
                //if(primitives.count(varName) || reserved_words.count(varName)) throw RuntimeError("Define: cannot redefine primitive or reserved word");
                Expr expr=syntax_To_Expr(stxs[2],env);//size=3
                if(stxs.size() > 3)
                {
                    vector<Expr> exprs;
                    for(size_t i=2; i<stxs.size(); ++i) exprs.push_back(syntax_To_Expr(stxs[i],env));
                    expr = Expr(new Begin(exprs));
                }
                return Expr(new Define(varName, expr));
            }
            case E_LET:
            {
                if(stxs.size() < 3) throw RuntimeError("Wrong number of arguments for Let");
                vector<string> parameters;
                vector<Syntax> arguments;
                if(auto lst = dynamic_cast<List*>(stxs[1].get()))
                {
                    for(auto &binding_stx : lst->stxs)
                    {
                        if(auto binding_lst = dynamic_cast<List*>(binding_stx.get()))
                        {
                            if(binding_lst->stxs.size() != 2) throw RuntimeError("Wrong number of arguments in single Let binding");
                            SymbolSyntax* var_sym = dynamic_cast<SymbolSyntax*>(binding_lst->stxs[0].get());
                            if(var_sym == nullptr) throw RuntimeError("Invalid variable name in Let binding");
                            parameters.push_back(var_sym->s);
                            arguments.push_back(binding_lst->stxs[1]);
                        }
                        else throw RuntimeError("Invalid binding syntax in Let");
                    }
                }
                else throw RuntimeError("Wrong parameter list in Let");
                vector<Syntax> body_stxs;
                for(size_t i=2; i<stxs.size(); ++i) body_stxs.push_back(stxs[i]);
                if(body_stxs.empty()) body_stxs.push_back(Syntax(new SymbolSyntax("void")));
                List* lambda_list = new List();
                lambda_list->stxs.push_back(Syntax(new SymbolSyntax("lambda")));
                List* param_list = new List();
                for(const auto& param : parameters) param_list->stxs.push_back(Syntax(new SymbolSyntax(param)));
                lambda_list->stxs.push_back(Syntax(param_list));
                for(const auto& body_stx : body_stxs) lambda_list->stxs.push_back(body_stx);
                
                //((lambda (params...) body) args...)
                List* apply_list = new List();
                apply_list->stxs.push_back(Syntax(lambda_list));
                for(const auto& arg : arguments) apply_list->stxs.push_back(arg);
                return syntax_To_Expr(Syntax(apply_list), env);
            }
            case E_LETREC:
            {
                if(stxs.size() < 3) throw RuntimeError("Wrong number of arguments for Letrec");
                vector<pair<string,Expr>> bindings;
                if(auto lst = dynamic_cast<List*>(stxs[1].get()))
                {
                    for(auto &stx:lst->stxs)
                    {
                        if(auto sym_lst = dynamic_cast<List*>(stx.get()))
                        {
                            if(sym_lst->stxs.size() != 2) throw RuntimeError("Wrong number of arguments in single Letrec");
                            SymbolSyntax* sym = dynamic_cast<SymbolSyntax*>(sym_lst->stxs[0].get());
                            if(sym == nullptr) throw RuntimeError("Invalid variable name in single Letrec");
                            bindings.push_back(std::make_pair(sym->s,syntax_To_Expr(sym_lst->stxs[1],env)));
                        }
                        else throw RuntimeError("Wrong typename in Lambda");
                    }
                }
                else throw RuntimeError("Wrong parameter list in Letrec");
                vector<Expr> body;
                for(size_t i=2; i<stxs.size(); ++i) body.push_back(syntax_To_Expr(stxs[i],env));
                Expr expr = body[0];
                if(body.size() > 1) expr = Expr(new Begin(body));
                return Expr(new Letrec(bindings,expr));
            }
            case E_SET:
            {
                if(stxs.size() != 3) throw RuntimeError("Wrong number of arguments for set!");
                SymbolSyntax* varSym = dynamic_cast<SymbolSyntax*>(stxs[1].get());
                if(varSym == nullptr) throw RuntimeError("Invalid variable name in set!");
                string varName = varSym->s;
                Expr expr = syntax_To_Expr(stxs[2],env);
                return Expr(new Set(varName,expr));
            }
        	default:
            	throw RuntimeError("Unknown reserved word: " + op);
    	}
    }

    //default: use Apply to be an expression
    //TODO: TO COMPLETE THE PARSER LOGIC
    //now op is a function name(variable name)
    vector<Expr> args;
    for(size_t i=1; i<stxs.size(); ++i) args.push_back(syntax_To_Expr(stxs[i],env));
    return Expr(new Apply(Expr(new Var(op)),args));
}
}
