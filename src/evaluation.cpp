/**
 * @file evaluation.cpp
 * @brief Expression evaluation implementation for the Scheme interpreter
 * @author luke36
 * 
 * This file implements evaluation methods for all expression types in the Scheme
 * interpreter. Functions are organized according to ExprType enumeration order
 * from Def.hpp for consistency and maintainability.
 */

#include "value.hpp"
#include "expr.hpp" 
#include "RE.hpp"
#include "syntax.hpp"
#include <cstring>
#include <vector>
#include <map>
#include <climits>

extern std::map<std::string, ExprType> primitives;
extern std::map<std::string, ExprType> reserved_words;

Value Fixnum::eval(Assoc &e) { // evaluation of a fixnum
    return IntegerV(n);
}

Value RationalNum::eval(Assoc &e) { // evaluation of a rational number
    return RationalV(numerator, denominator);
}

Value StringExpr::eval(Assoc &e) { // evaluation of a string
    return StringV(s);
}

Value True::eval(Assoc &e) { // evaluation of #t
    return BooleanV(true);
}

Value False::eval(Assoc &e) { // evaluation of #f
    return BooleanV(false);
}

Value MakeVoid::eval(Assoc &e) { // (void)
    return VoidV();
}

Value Exit::eval(Assoc &e) { // (exit)
    return TerminateV();
}

Value Unary::eval(Assoc &e) { // evaluation of single-operator primitive
    return evalRator(rand->eval(e));
}

Value Binary::eval(Assoc &e) { // evaluation of two-operators primitive
    return evalRator(rand1->eval(e), rand2->eval(e));
}

Value Variadic::eval(Assoc &e) { // evaluation of multi-operator primitive
    // TODO: TO COMPLETE THE VARIADIC CLASS
    std::vector <Value> e_args;
    for(auto& arg : rands) e_args.push_back(arg->eval(e));
    return evalRator(e_args);
}

Value Var::eval(Assoc &e) { // evaluation of variable
    // TODO: TO identify the invalid variable
    // We request all valid variable just need to be a symbol,you should promise:
    //The first character of a variable name cannot be a digit or any character from the set: {.@}
    //If a string can be recognized as a number, it will be prioritized as a number. For example: 1, -1, +123, .123, +124., 1e-3
    //Variable names can overlap with primitives and reserve_words
    //Variable names can contain any non-whitespace characters except #, ', ", `, but the first character cannot be a digit
    //When a variable is not defined in the current scope, your interpreter should output RuntimeError
    Value matched_value = find(x, e);//user variable in current scope environment
    //Variable names can overlap with primitives and reserve_words, so we should check it first
    if (matched_value.get() == nullptr) {
        if (primitives.count(x)) {
             static std::map<ExprType, std::pair<Expr, std::vector<std::string>>> primitive_map = {
                    {E_VOID,     {new MakeVoid(), {}}},
                    {E_EXIT,     {new Exit(), {}}},
                    {E_BOOLQ,    {new IsBoolean(new Var("parm")), {"parm"}}},
                    {E_INTQ,     {new IsFixnum(new Var("parm")), {"parm"}}},
                    {E_NULLQ,    {new IsNull(new Var("parm")), {"parm"}}},
                    {E_PAIRQ,    {new IsPair(new Var("parm")), {"parm"}}},
                    {E_PROCQ,    {new IsProcedure(new Var("parm")), {"parm"}}},
                    {E_SYMBOLQ,  {new IsSymbol(new Var("parm")), {"parm"}}},
                    {E_STRINGQ,  {new IsString(new Var("parm")), {"parm"}}},
                    {E_DISPLAY,  {new Display(new Var("parm")), {"parm"}}},
                    {E_PLUS,     {new PlusVar({}),  {}}},
                    {E_MINUS,    {new MinusVar({}), {}}},
                    {E_MUL,      {new MultVar({}),  {}}},
                    {E_DIV,      {new DivVar({}),   {}}},
                    {E_MODULO,   {new Modulo(new Var("parm1"), new Var("parm2")), {"parm1","parm2"}}},
                    {E_EXPT,     {new Expt(new Var("parm1"), new Var("parm2")), {"parm1","parm2"}}},
                    {E_EQQ,      {new EqualVar({}), {}}},
                    {E_EQ,       {new EqualVar({}), {}}},
                    {E_LT,       {new LessVar({}), {}}},
                    {E_LE,       {new LessEqVar({}), {}}},
                    {E_GE,       {new GreaterEqVar({}), {}}},
                    {E_GT,       {new GreaterVar({}), {}}},
                    {E_CONS,     {new Cons(new Var("parm1"), new Var("parm2")), {"parm1","parm2"}}},
                    {E_CAR,      {new Car(new Var("parm")), {"parm"}}},
                    {E_CDR,      {new Cdr(new Var("parm")), {"parm"}}},
                    {E_NOT,      {new Not(new Var("parm")), {"parm"}}},
                    {E_LIST,     {new ListFunc({}), {}}},
                    {E_LISTQ,    {new IsList(new Var("parm")), {"parm"}}},
                    {E_SETCAR,   {new SetCar(new Var("parm1"), new Var("parm2")), {"parm1","parm2"}}},
                    {E_SETCDR,   {new SetCdr(new Var("parm1"), new Var("parm2")), {"parm1","parm2"}}},
                    {E_AND,      {new AndVar({}), {}}},
                    {E_OR,       {new OrVar({}), {}}}
            };//check if it is an operation

            auto it = primitive_map.find(primitives[x]);
            //TOD0:to PASS THE parameters correctly;
            //COMPLETE THE CODE WITH THE HINT IN IF SENTENCE WITH CORRECT RETURN VALUE
            if (it != primitive_map.end()) {
                //TODO
                return ProcedureV(it->second.second,it->second.first,e);
                /*
                in value.cpp:
                Value ProcedureV(const std::vector<std::string> &xs, const Expr &e, const Assoc &env) {
                    return Value(new Procedure(xs, e, env));
                }
                */
            }
      }
      throw(RuntimeError("Invalid variable name in Var::eval : " + x));
    }
    return matched_value;
}

Value Plus::evalRator(const Value &rand1, const Value &rand2) { // +
    //TODO: To complete the addition logic
    if((rand1->v_type == V_RATIONAL || rand1->v_type == V_INT) && (rand2->v_type == V_RATIONAL || rand2->v_type == V_INT))
    {
        Rational augend(0,1),addend(0,1);
        if(rand1->v_type == V_RATIONAL) augend = *dynamic_cast<Rational*>(rand1.get());
        else augend = Rational(dynamic_cast<Integer*>(rand1.get())->n,1);
        if(rand2->v_type == V_RATIONAL) addend = *dynamic_cast<Rational*>(rand2.get());
        else addend = Rational(dynamic_cast<Integer*>(rand2.get())->n,1);
        Rational sum = augend + addend;
        return RationalV(sum.numerator,sum.denominator);
    }
    throw(RuntimeError("Wrong typename in +"));
}

Value Minus::evalRator(const Value &rand1, const Value &rand2) { // -
    //TODO: To complete the substraction logic
    if((rand1->v_type == V_RATIONAL || rand1->v_type == V_INT) && (rand2->v_type == V_RATIONAL || rand2->v_type == V_INT))
    {
        Rational minuend(0,1),subtrahend(0,1);
        if(rand1->v_type == V_RATIONAL) minuend = *dynamic_cast<Rational*>(rand1.get());
        else minuend = Rational(dynamic_cast<Integer*>(rand1.get())->n,1);
        if(rand2->v_type == V_RATIONAL) subtrahend = *dynamic_cast<Rational*>(rand2.get());
        else subtrahend = Rational(dynamic_cast<Integer*>(rand2.get())->n,1);
        Rational difference = minuend - subtrahend;
        return RationalV(difference.numerator,difference.denominator);
    }
    throw(RuntimeError("Wrong typename in -"));
}

Value Mult::evalRator(const Value &rand1, const Value &rand2) { // *
    //TODO: To complete the Multiplication logic
    if((rand1->v_type == V_RATIONAL || rand1->v_type == V_INT) && (rand2->v_type == V_RATIONAL || rand2->v_type == V_INT))
    {
        Rational multiplier(0,1),multiplicand(0,1);
        if(rand1->v_type == V_RATIONAL) multiplier = *dynamic_cast<Rational*>(rand1.get());
        else multiplier = Rational(dynamic_cast<Integer*>(rand1.get())->n,1);
        if(rand2->v_type == V_RATIONAL) multiplicand = *dynamic_cast<Rational*>(rand2.get());
        else multiplicand = Rational(dynamic_cast<Integer*>(rand2.get())->n,1);
        Rational product = multiplier * multiplicand;
        return RationalV(product.numerator,product.denominator);
    }
    throw(RuntimeError("Wrong typename in *"));
}

Value Div::evalRator(const Value &rand1, const Value &rand2) { // /
    //TODO: To complete the dicision logic
    if((rand1->v_type == V_RATIONAL || rand1->v_type == V_INT) && (rand2->v_type == V_RATIONAL || rand2->v_type == V_INT))
    {
        Rational dividend(0,1),divisor(0,1);
        if(rand1->v_type == V_RATIONAL) dividend = *dynamic_cast<Rational*>(rand1.get());
        else dividend = Rational(dynamic_cast<Integer*>(rand1.get())->n,1);
        if(rand2->v_type == V_RATIONAL) divisor = *dynamic_cast<Rational*>(rand2.get());
        else divisor = Rational(dynamic_cast<Integer*>(rand2.get())->n,1);
        if(divisor.numerator == 0) throw(RuntimeError("Division by zero"));
        Rational quotient = dividend / divisor;
        return RationalV(quotient.numerator,quotient.denominator);
    }
    throw(RuntimeError("Wrong typename in /"));
}

Value Modulo::evalRator(const Value &rand1, const Value &rand2) { // modulo
    if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
        int dividend = dynamic_cast<Integer*>(rand1.get())->n;
        int divisor = dynamic_cast<Integer*>(rand2.get())->n;
        if (divisor == 0) {
            throw(RuntimeError("Division by zero"));
        }
        return IntegerV(dividend % divisor);
    }
    throw(RuntimeError("modulo is only defined for integers"));
}

Value PlusVar::evalRator(const std::vector<Value> &args) { // + with multiple args
    //TODO: To complete the addition logic
    if(args.size() == 0) return IntegerV(0);//special judge
    else if(args.size() == 1)
    {
        if(args[0]->v_type == V_INT || args[0]->v_type == V_RATIONAL) return args[0];
        else throw(RuntimeError("Wrong typename in +(multiple)"));
    }
    Rational sum(0,1),addend(0,1);
    for(size_t i=0; i<args.size(); ++i)
    {
        if(!i)
        {
            if(args[i]->v_type == V_RATIONAL) sum = *dynamic_cast<Rational*>(args[i].get());
            else if(args[i]->v_type == V_INT) sum = Rational(dynamic_cast<Integer*>(args[i].get())->n,1);
            else throw(RuntimeError("Wrong typename in +(multiple)"));
        }
        else
        {
            if(args[i]->v_type == V_RATIONAL) addend = *dynamic_cast<Rational*>(args[i].get());
            else if(args[i]->v_type == V_INT) addend = Rational(dynamic_cast<Integer*>(args[i].get())->n,1);
            else throw(RuntimeError("Wrong typename in +(multiple)"));
            sum += addend;
        }
    }
    return RationalV(sum.numerator,sum.denominator);
}

Value MinusVar::evalRator(const std::vector<Value> &args) { // - with multiple args
    //TODO: To complete the substraction logic
    if(args.size() == 0) throw RuntimeError("Wrong number of arguments for -");
    else if(args.size() == 1)
    {
        if(args[0]->v_type == V_INT) return IntegerV(-dynamic_cast<Integer*>(args[0].get())->n);
        else if(args[0]->v_type == V_RATIONAL)
        {
            Rational r = *dynamic_cast<Rational*>(args[0].get());
            return RationalV(-r.numerator,r.denominator);
        }
        else throw RuntimeError("Wrong typename in -(multiple)");
    }
    Rational difference(0,1),subtrahend(0,1);
    for(size_t i=0; i<args.size(); ++i)
    {
        if(!i)
        {
            if(args[i]->v_type == V_RATIONAL) difference = *dynamic_cast<Rational*>(args[i].get());
            else if(args[i]->v_type == V_INT) difference = Rational(dynamic_cast<Integer*>(args[i].get())->n,1);
            else throw(RuntimeError("Wrong typename in -(multiple)"));
        }
        else
        {
            if(args[i]->v_type == V_RATIONAL) subtrahend = *dynamic_cast<Rational*>(args[i].get());
            else if(args[i]->v_type == V_INT) subtrahend = Rational(dynamic_cast<Integer*>(args[i].get())->n,1);
            else throw(RuntimeError("Wrong typename in -(multiple)"));
            difference -= subtrahend;
        }
    }
    return RationalV(difference.numerator,difference.denominator);
}

Value MultVar::evalRator(const std::vector<Value> &args) { // * with multiple args
    //TODO: To complete the multiplication logic
    if(args.size() == 0) return IntegerV(1);//special judge
    else if(args.size() == 1)
    {
        if(args[0]->v_type == V_INT || args[0]->v_type == V_RATIONAL) return args[0];
        else throw(RuntimeError("Wrong typename in *(multiple)"));
    }
    Rational product(0,1),multiplicand(0,1);
    for(size_t i=0; i<args.size(); ++i)
    {
        if(!i)
        {
            if(args[i]->v_type == V_RATIONAL) product = *dynamic_cast<Rational*>(args[i].get());
            else if(args[i]->v_type == V_INT) product = Rational(dynamic_cast<Integer*>(args[i].get())->n,1);
            else throw(RuntimeError("Wrong typename in *(multiple)"));
        }
        else
        {
            if(args[i]->v_type == V_RATIONAL) multiplicand = *dynamic_cast<Rational*>(args[i].get());
            else if(args[i]->v_type == V_INT) multiplicand = Rational(dynamic_cast<Integer*>(args[i].get())->n,1);
            else throw(RuntimeError("Wrong typename in *(multiple)"));
            product *= multiplicand;
        }
    }
    return RationalV(product.numerator,product.denominator);
}

Value DivVar::evalRator(const std::vector<Value> &args) { // / with multiple args
    //TODO: To complete the divisor logic
    if(args.size() == 0) throw RuntimeError("Wrong number of arguments for /");
    else if(args.size() == 1)
    {
        if(args[0]->v_type == V_INT)
        {
            if(dynamic_cast<Integer*>(args[0].get())->n == 0) throw RuntimeError("Division by zero");
            return RationalV(1,dynamic_cast<Integer*>(args[0].get())->n);
        }
        else if(args[0]->v_type == V_RATIONAL)
        {
            Rational r = *dynamic_cast<Rational*>(args[0].get());
            if(r.numerator == 0) throw RuntimeError("Division by zero");
            return RationalV(r.denominator,r.numerator);
        }
        else throw RuntimeError("Wrong typename in /(multiple)");
    }
    Rational quotient(0,1),divisor(0,1);
    for(size_t i=0; i<args.size(); ++i)
    {
        if(!i)
        {
            if(args[i]->v_type == V_RATIONAL) quotient = *dynamic_cast<Rational*>(args[i].get());
            else if(args[i]->v_type == V_INT) quotient = Rational(dynamic_cast<Integer*>(args[i].get())->n,1);
            else throw(RuntimeError("Wrong typename in /(multiple)"));
        }
        else
        {
            if(args[i]->v_type == V_RATIONAL) divisor = *dynamic_cast<Rational*>(args[i].get());
            else if(args[i]->v_type == V_INT) divisor = Rational(dynamic_cast<Integer*>(args[i].get())->n,1);
            else throw(RuntimeError("Wrong typename in /(multiple)"));
            if(divisor.numerator == 0) throw(RuntimeError("Division by zero"));
            quotient /= divisor;
        }
    }
    return RationalV(quotient.numerator,quotient.denominator);
}

Value Expt::evalRator(const Value &rand1, const Value &rand2) { // expt
    if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
        int base = dynamic_cast<Integer*>(rand1.get())->n;
        int exponent = dynamic_cast<Integer*>(rand2.get())->n;
        
        if (exponent < 0) {
            throw(RuntimeError("Negative exponent not supported for integers"));
        }
        if (base == 0 && exponent == 0) {
            throw(RuntimeError("0^0 is undefined"));
        }
        
        long long result = 1;
        long long b = base;
        int exp = exponent;
        
        while (exp > 0) {
            if (exp % 2 == 1) {
                result *= b;
                if (result > INT_MAX || result < INT_MIN) {
                    throw(RuntimeError("Integer overflow in expt"));
                }
            }
            b *= b;
            if (b > INT_MAX || b < INT_MIN) {
                if (exp > 1) {
                    throw(RuntimeError("Integer overflow in expt"));
                }
            }
            exp /= 2;
        }
        
        return IntegerV((int)result);
    }
    throw(RuntimeError("Wrong typename in expt"));
}

//A FUNCTION TO SIMPLIFY THE COMPARISON WITH INTEGER AND RATIONAL NUMBER
int compareNumericValues(const Value &v1, const Value &v2) {
    if (v1->v_type == V_INT && v2->v_type == V_INT) {
        int n1 = dynamic_cast<Integer*>(v1.get())->n;
        int n2 = dynamic_cast<Integer*>(v2.get())->n;
        return (n1 < n2) ? -1 : (n1 > n2) ? 1 : 0;
    }
    else if (v1->v_type == V_RATIONAL && v2->v_type == V_INT) {
        Rational* r1 = dynamic_cast<Rational*>(v1.get());
        int n2 = dynamic_cast<Integer*>(v2.get())->n;
        int left = r1->numerator;
        int right = n2 * r1->denominator;
        return (left < right) ? -1 : (left > right) ? 1 : 0;
    }
    else if (v1->v_type == V_INT && v2->v_type == V_RATIONAL) {
        int n1 = dynamic_cast<Integer*>(v1.get())->n;
        Rational* r2 = dynamic_cast<Rational*>(v2.get());
        int left = n1 * r2->denominator;
        int right = r2->numerator;
        return (left < right) ? -1 : (left > right) ? 1 : 0;
    }
    else if (v1->v_type == V_RATIONAL && v2->v_type == V_RATIONAL) {
        Rational* r1 = dynamic_cast<Rational*>(v1.get());
        Rational* r2 = dynamic_cast<Rational*>(v2.get());
        int left = r1->numerator * r2->denominator;
        int right = r2->numerator * r1->denominator;
        return (left < right) ? -1 : (left > right) ? 1 : 0;
    }
    throw RuntimeError("Wrong typename in numeric comparison");
}

Value Less::evalRator(const Value &rand1, const Value &rand2) { // <
    //TODO: To complete the less logic
    if((rand1->v_type == V_RATIONAL || rand1->v_type == V_INT) && (rand2->v_type == V_RATIONAL || rand2->v_type == V_INT))
    {
        Rational lhs(0,1),rhs(0,1);
        if(rand1->v_type == V_RATIONAL) lhs = *dynamic_cast<Rational*>(rand1.get());
        else lhs = Rational(dynamic_cast<Integer*>(rand1.get())->n,1);
        if(rand2->v_type == V_RATIONAL) rhs = *dynamic_cast<Rational*>(rand2.get());
        else rhs = Rational(dynamic_cast<Integer*>(rand2.get())->n,1);
        return BooleanV(lhs<rhs);
    }
    throw(RuntimeError("Wrong typename in <"));
}

Value LessEq::evalRator(const Value &rand1, const Value &rand2) { // <=
    //TODO: To complete the lesseq logic
    if((rand1->v_type == V_RATIONAL || rand1->v_type == V_INT) && (rand2->v_type == V_RATIONAL || rand2->v_type == V_INT))
    {
        Rational lhs(0,1),rhs(0,1);
        if(rand1->v_type == V_RATIONAL) lhs = *dynamic_cast<Rational*>(rand1.get());
        else lhs = Rational(dynamic_cast<Integer*>(rand1.get())->n,1);
        if(rand2->v_type == V_RATIONAL) rhs = *dynamic_cast<Rational*>(rand2.get());
        else rhs = Rational(dynamic_cast<Integer*>(rand2.get())->n,1);
        return BooleanV(lhs<=rhs);
    }
    throw(RuntimeError("Wrong typename in <="));
}

Value Equal::evalRator(const Value &rand1, const Value &rand2) { // =
    //TODO: To complete the equal logic
    if((rand1->v_type == V_RATIONAL || rand1->v_type == V_INT) && (rand2->v_type == V_RATIONAL || rand2->v_type == V_INT))
    {
        Rational lhs(0,1),rhs(0,1);
        if(rand1->v_type == V_RATIONAL) lhs = *dynamic_cast<Rational*>(rand1.get());
        else lhs = Rational(dynamic_cast<Integer*>(rand1.get())->n,1);
        if(rand2->v_type == V_RATIONAL) rhs = *dynamic_cast<Rational*>(rand2.get());
        else rhs = Rational(dynamic_cast<Integer*>(rand2.get())->n,1);
        return BooleanV(lhs==rhs);
    }
    throw(RuntimeError("Wrong typename in ="));
}

Value GreaterEq::evalRator(const Value &rand1, const Value &rand2) { // >=
    //TODO: To complete the greatereq logic
    if((rand1->v_type == V_RATIONAL || rand1->v_type == V_INT) && (rand2->v_type == V_RATIONAL || rand2->v_type == V_INT))
    {
        Rational lhs(0,1),rhs(0,1);
        if(rand1->v_type == V_RATIONAL) lhs = *dynamic_cast<Rational*>(rand1.get());
        else lhs = Rational(dynamic_cast<Integer*>(rand1.get())->n,1);
        if(rand2->v_type == V_RATIONAL) rhs = *dynamic_cast<Rational*>(rand2.get());
        else rhs = Rational(dynamic_cast<Integer*>(rand2.get())->n,1);
        return BooleanV(lhs>=rhs);
    }
    throw(RuntimeError("Wrong typename in >="));
}

Value Greater::evalRator(const Value &rand1, const Value &rand2) { // >
    //TODO: To complete the greater logic
    if((rand1->v_type == V_RATIONAL || rand1->v_type == V_INT) && (rand2->v_type == V_RATIONAL || rand2->v_type == V_INT))
    {
        Rational lhs(0,1),rhs(0,1);
        if(rand1->v_type == V_RATIONAL) lhs = *dynamic_cast<Rational*>(rand1.get());
        else lhs = Rational(dynamic_cast<Integer*>(rand1.get())->n,1);
        if(rand2->v_type == V_RATIONAL) rhs = *dynamic_cast<Rational*>(rand2.get());
        else rhs = Rational(dynamic_cast<Integer*>(rand2.get())->n,1);
        return BooleanV(lhs>rhs);
    }
    throw(RuntimeError("Wrong typename in >"));
}

Value LessVar::evalRator(const std::vector<Value> &args) { // < with multiple args
    //TODO: To complete the less logic
    if(args.size() == 0) return BooleanV(true);
    Rational lhs(0,1),rhs(0,1);
    bool result = true;
    for(size_t i=0; i<args.size(); ++i)
    {
        if(!i)
        {
            if(args[i]->v_type == V_RATIONAL) lhs = *dynamic_cast<Rational*>(args[i].get());
            else if(args[i]->v_type == V_INT) lhs = Rational(dynamic_cast<Integer*>(args[i].get())->n,1);
            else throw(RuntimeError("Wrong typename in <(multiple)"));
        }
        else
        {
            if(args[i]->v_type == V_RATIONAL) rhs = *dynamic_cast<Rational*>(args[i].get());
            else if(args[i]->v_type == V_INT) rhs = Rational(dynamic_cast<Integer*>(args[i].get())->n,1);
            else throw(RuntimeError("Wrong typename in <(multiple)"));
            if(lhs >= rhs) result = false;
            if(!result) continue;//don't break in case some of the data after the false occurs are invalid
            lhs = rhs;
        }
    }
    return BooleanV(result);
}

Value LessEqVar::evalRator(const std::vector<Value> &args) { // <= with multiple args
    //TODO: To complete the lesseq logic
    if(args.size() == 0) return BooleanV(true);
    Rational lhs(0,1),rhs(0,1);
    bool result = true;
    for(size_t i=0; i<args.size(); ++i)
    {
        if(!i)
        {
            if(args[i]->v_type == V_RATIONAL) lhs = *dynamic_cast<Rational*>(args[i].get());
            else if(args[i]->v_type == V_INT) lhs = Rational(dynamic_cast<Integer*>(args[i].get())->n,1);
            else throw(RuntimeError("Wrong typename in <=(multiple)"));
        }
        else
        {
            if(args[i]->v_type == V_RATIONAL) rhs = *dynamic_cast<Rational*>(args[i].get());
            else if(args[i]->v_type == V_INT) rhs = Rational(dynamic_cast<Integer*>(args[i].get())->n,1);
            else throw(RuntimeError("Wrong typename in <=(multiple)"));
            if(lhs > rhs) result = false;
            if(!result) continue;//don't break in case some of the data after the false occurs are invalid
            lhs = rhs;
        }
    }
    return BooleanV(result);
}

Value EqualVar::evalRator(const std::vector<Value> &args) { // = with multiple args
    //TODO: To complete the equal logic
    if(args.size() == 0) return BooleanV(true);
    Rational lhs(0,1),rhs(0,1);
    bool result = true;
    for(size_t i=0; i<args.size(); ++i)
    {
        if(!i)
        {
            if(args[i]->v_type == V_RATIONAL) lhs = *dynamic_cast<Rational*>(args[i].get());
            else if(args[i]->v_type == V_INT) lhs = Rational(dynamic_cast<Integer*>(args[i].get())->n,1);
            else throw(RuntimeError("Wrong typename in =(multiple)"));
        }
        else
        {
            if(args[i]->v_type == V_RATIONAL) rhs = *dynamic_cast<Rational*>(args[i].get());
            else if(args[i]->v_type == V_INT) rhs = Rational(dynamic_cast<Integer*>(args[i].get())->n,1);
            else throw(RuntimeError("Wrong typename in =(multiple)"));
            if(!(lhs == rhs)) result = false;
            if(!result) continue;//don't break in case some of the data after the false occurs are invalid
            lhs = rhs;
        }
    }
    return BooleanV(result);
}

Value GreaterEqVar::evalRator(const std::vector<Value> &args) { // >= with multiple args
    //TODO: To complete the greatereq logic
    if(args.size() == 0) return BooleanV(true);
    Rational lhs(0,1),rhs(0,1);
    bool result = true;
    for(size_t i=0; i<args.size(); ++i)
    {
        if(!i)
        {
            if(args[i]->v_type == V_RATIONAL) lhs = *dynamic_cast<Rational*>(args[i].get());
            else if(args[i]->v_type == V_INT) lhs = Rational(dynamic_cast<Integer*>(args[i].get())->n,1);
            else throw(RuntimeError("Wrong typename in >=(multiple)"));
        }
        else
        {
            if(args[i]->v_type == V_RATIONAL) rhs = *dynamic_cast<Rational*>(args[i].get());
            else if(args[i]->v_type == V_INT) rhs = Rational(dynamic_cast<Integer*>(args[i].get())->n,1);
            else throw(RuntimeError("Wrong typename in >=(multiple)"));
            if(lhs < rhs) result = false;
            if(!result) continue;//don't break in case some of the data after the false occurs are invalid
            lhs = rhs;
        }
    }
    return BooleanV(result);
}

Value GreaterVar::evalRator(const std::vector<Value> &args) { // > with multiple args
    //TODO: To complete the greater logic
    if(args.size() == 0) return BooleanV(true);
    Rational lhs(0,1),rhs(0,1);
    bool result = true;
    for(size_t i=0; i<args.size(); ++i)
    {
        if(!i)
        {
            if(args[i]->v_type == V_RATIONAL) lhs = *dynamic_cast<Rational*>(args[i].get());
            else if(args[i]->v_type == V_INT) lhs = Rational(dynamic_cast<Integer*>(args[i].get())->n,1);
            else throw(RuntimeError("Wrong typename in >(multiple)"));
        }
        else
        {
            if(args[i]->v_type == V_RATIONAL) rhs = *dynamic_cast<Rational*>(args[i].get());
            else if(args[i]->v_type == V_INT) rhs = Rational(dynamic_cast<Integer*>(args[i].get())->n,1);
            else throw(RuntimeError("Wrong typename in >(multiple)"));
            if(lhs <= rhs) result = false;
            if(!result) continue;//don't break in case some of the data after the false occurs are invalid
            lhs = rhs;
        }
    }
    return BooleanV(result);
}

Value Cons::evalRator(const Value &rand1, const Value &rand2) { // cons
    //TODO: To complete the cons logic
    return PairV(rand1,rand2);
}

Value ListFunc::evalRator(const std::vector<Value> &args) { // list function
    //TODO: To complete the list logic
    Value lst = NullV();
    for(auto it=args.rbegin(); it!=args.rend(); ++it) lst = PairV(*it,lst);
    //1 -> 2 -> 3 -> '(), so we should construct with the opposite order with (*it,lst)
    return lst;
}

Value IsList::evalRator(const Value &rand) { // list?
    //TODO: To complete the list? logic
    if(rand->v_type == V_NULL) return BooleanV(true);
    else if(rand->v_type == V_PAIR) return IsList::evalRator(dynamic_cast<Pair*>(rand.get())->cdr);//check if rand.cdr is Pair or Null
    else return BooleanV(false);
    //a valid list should guarantee one of the following:
    //<1> '() [Null]
    //<2> pair(?,list)
}

Value Car::evalRator(const Value &rand) { // car
    //TODO: To complete the car logic
    if(rand->v_type == V_PAIR) return dynamic_cast<Pair*>(rand.get())->car;
    throw(RuntimeError("Wrong typename in Car"));
}

Value Cdr::evalRator(const Value &rand) { // cdr
    //TODO: To complete the cdr logic
    if(rand->v_type == V_PAIR) return dynamic_cast<Pair*>(rand.get())->cdr;//it can print the whole cdr parts until the last element
    throw(RuntimeError("Wrong typename in Cdr"));
}

Value SetCar::evalRator(const Value &rand1, const Value &rand2) { // set-car!
    //TODO: To complete the set-car! logic
    if (rand1->v_type != V_PAIR) throw RuntimeError("Wrong typename in Set-Car!");
    Pair* pair = dynamic_cast<Pair*>(rand1.get());
    pair->car = rand2;
    return VoidV();
}

Value SetCdr::evalRator(const Value &rand1, const Value &rand2) { // set-cdr!
   //TODO: To complete the set-cdr! logic
   if (rand1->v_type != V_PAIR) throw RuntimeError("Wrong typename in Set-Cdr!");
    Pair* pair = dynamic_cast<Pair*>(rand1.get());
    pair->cdr = rand2;
    return VoidV();
}

Value IsEq::evalRator(const Value &rand1, const Value &rand2) { // eq?
    // 检查类型是否为 Integer
    if ((rand1->v_type == V_INT || rand1->v_type == V_RATIONAL) && (rand2->v_type == V_INT || rand2->v_type == V_RATIONAL)) {
        int res = compareNumericValues(rand1,rand2);
        if(res == 0) return BooleanV(true);
        else return BooleanV(false);
    }
    // 检查类型是否为 Boolean
    else if (rand1->v_type == V_BOOL && rand2->v_type == V_BOOL) {
        return BooleanV((dynamic_cast<Boolean*>(rand1.get())->b) == (dynamic_cast<Boolean*>(rand2.get())->b));
    }
    // 检查类型是否为 Symbol
    else if (rand1->v_type == V_SYM && rand2->v_type == V_SYM) {
        return BooleanV((dynamic_cast<Symbol*>(rand1.get())->s) == (dynamic_cast<Symbol*>(rand2.get())->s));
    }
    // 检查类型是否为 Null 或 Void
    else if ((rand1->v_type == V_NULL && rand2->v_type == V_NULL) ||
             (rand1->v_type == V_VOID && rand2->v_type == V_VOID)) {
        return BooleanV(true);
    } else {
        return BooleanV(rand1.get() == rand2.get());
    }
}

Value IsBoolean::evalRator(const Value &rand) { // boolean?
    return BooleanV(rand->v_type == V_BOOL);
}

Value IsFixnum::evalRator(const Value &rand) { // number?
    return BooleanV(rand->v_type == V_INT || rand->v_type == V_RATIONAL);
}

Value IsNull::evalRator(const Value &rand) { // null?
    return BooleanV(rand->v_type == V_NULL);
}

Value IsPair::evalRator(const Value &rand) { // pair?
    return BooleanV(rand->v_type == V_PAIR);
}

Value IsProcedure::evalRator(const Value &rand) { // procedure?
    return BooleanV(rand->v_type == V_PROC);
}

Value IsSymbol::evalRator(const Value &rand) { // symbol?
    return BooleanV(rand->v_type == V_SYM);
}

Value IsString::evalRator(const Value &rand) { // string?
    return BooleanV(rand->v_type == V_STRING);
}

Value Begin::eval(Assoc &e) {
    //TODO: To complete the begin logic
    if(es.empty()) return VoidV();
    Value res = VoidV();
    for(auto &exp:es) res=exp->eval(e);
    return res;
}

Value Check_List(std::vector<Syntax> &stxs);
Value Build_List(const std::vector<Syntax> &stxs, size_t start, size_t end);
Value Syntax_to_Value(const Syntax &syn);

Value Check_List(std::vector<Syntax> &stxs)
{
    size_t siz = stxs.size();
    for(size_t i=0; i<siz; ++i)
    {
        if(auto sym = dynamic_cast<SymbolSyntax*>(stxs[i].get()))
        {
            if(sym->s == ".")//cut here
            {
                Value car = Build_List(stxs,0,i);//car must be proper(or Null)
                Value cdr = Syntax_to_Value(stxs[i+1]);//cor should be done again
                if(car->v_type == V_NULL) return cdr;
                else//set the cdr of the last element of car from Null to cdr,then return the whole(i.e. the new car)
                {
                    Value tmp = car;
                    Pair* last_pair = nullptr;
                    while(true)
                    {
                        Pair* pair = dynamic_cast<Pair*>(tmp.get());
                        tmp = pair->cdr;
                        last_pair = pair;
                        if(tmp->v_type != V_PAIR) break;
                    }
                    if(last_pair) last_pair->cdr=cdr;
                    return car;
                }
            }
        }
    }
    return Build_List(stxs,0,siz);//if the list doesn't have any dot
}

Value Build_List(const std::vector<Syntax> &stxs,size_t start,size_t end)//the [start,end) section of stxs
{
    if(start >= end) return NullV();//has finished
    Value car = Syntax_to_Value(stxs[start]);
    Value cdr = Build_List(stxs,start+1,end);
    return PairV(car,cdr);//It forms a proper list (a b c),actually (a . (b . (c . ())))
}

Value Syntax_to_Value(const Syntax &syn)
{
    if(auto num = dynamic_cast<Number*>(syn.get())) return IntegerV(num->n);
    else if(auto rat = dynamic_cast<RationalSyntax*>(syn.get())) return RationalV(rat->numerator,rat->denominator);
    else if(dynamic_cast<TrueSyntax*>(syn.get())) return BooleanV(true);
    else if(dynamic_cast<FalseSyntax*>(syn.get())) return BooleanV(false);
    else if(auto sym = dynamic_cast<SymbolSyntax*>(syn.get())) return SymbolV(sym->s);
    else if(auto str = dynamic_cast<StringSyntax*>(syn.get())) return StringV(str->s);
    else if(auto args = dynamic_cast<List*>(syn.get())) return Check_List(args->stxs);//including List and Pair
    /*
    Scheme Pair显示规则
    正规列表：(a . (b . (c . ()))) → 显示为 (a b c)
    非正规列表：(a . (b . c)) → 显示为 (a b . c)
    简单点对：(a . b) → 显示为 (a . b)

    关键规则总结
    递归检查cdr：从最外层Pair开始，递归检查每个cdr的类型
    空列表终止：遇到()时，当前层级列表结束
    非Pair终止：遇到非Pair值时，必须显示点符号
    省略规则：只有当所有中间cdr都是Pair且最后一个cdr是()时，才能省略所有点符号
    */
    else throw(RuntimeError("Wrong typename"));
}

Value Quote::eval(Assoc& e) {
    //TODO: To complete the quote logic
    return Syntax_to_Value(s);
}

Value AndVar::eval(Assoc &e) { // and with short-circuit evaluation
    //TODO: To complete the and logic
    if(rands.empty()) return BooleanV(true);
    Value last_element = BooleanV(true);
    for(auto expr:rands)
    {
        Value val = expr->eval(e);
        last_element = val;
        if(val->v_type == V_BOOL && !(dynamic_cast<Boolean*>(val.get())->b)) return BooleanV(false);
    }
    return last_element;//return the value of the last element
}

Value OrVar::eval(Assoc &e) { // or with short-circuit evaluation
    //TODO: To complete the or logic
    if(rands.empty()) return BooleanV(false);
    Value last_element = BooleanV(false);
    for(auto expr:rands)
    {
        Value val = expr->eval(e);
        last_element = val;
        if(val->v_type == V_BOOL && !(dynamic_cast<Boolean*>(val.get())->b)) continue;//if false,continue
        return val;//return the first element that is not #f
    }
    return last_element;//return the value of the last element
}

Value Not::evalRator(const Value &rand) { // not
    //TODO: To complete the not logic
    if(rand->v_type == V_BOOL && !dynamic_cast<Boolean*>(rand.get())->b) return BooleanV(true);
    else return BooleanV(false);
}
//请特别注意，在 scheme 中，任何不同于 #f 的值（包括不同类型的值,包括 '()）都会被视作 #t

Value If::eval(Assoc &e) {
    //TODO: To complete the if logic
    Value cond_val = cond->eval(e);
    if(cond_val->v_type == V_BOOL && !(dynamic_cast<Boolean*>(cond_val.get())->b)) return alter->eval(e);
    else return conseq->eval(e);
}

Value Cond::eval(Assoc &env) {
    //TODO: To complete the cond logic
    for(auto &clause:clauses)
    {
        if(auto sym = dynamic_cast<Var*>(clause[0].get()))
        {
            if(sym->x == "else")
            {
                if(clause.size()==1) return VoidV();
                else return clause.back()->eval(env);
            }
        }

        Value tmp = clause[0]->eval(env);
        if(tmp->v_type != V_BOOL || dynamic_cast<Boolean*>(tmp.get())->b)//true
        {
            if(clause.size()==1) return tmp;
            else return clause.back()->eval(env);
        }
    }
    return VoidV();
}

Value Lambda::eval(Assoc &env) { 
    //TODO: To complete the lambda logic
    return ProcedureV(x,e,env);//closure
}

Value Apply::eval(Assoc &e) {
    Value proc_value = rator->eval(e);
    
    if (proc_value->v_type != V_PROC) throw RuntimeError("Attempt to apply a non-procedure");

    //TODO: TO COMPLETE THE CLOSURE LOGIC
    Procedure* clos_ptr = dynamic_cast<Procedure*>(proc_value.get());
    
    //TODO: TO COMPLETE THE ARGUMENT PARSER LOGIC
    std::vector<Value> args;
    for(auto &arg:rand) args.push_back(arg->eval(e));
    if (auto varNode = dynamic_cast<Variadic*>(clos_ptr->e.get())) {
        //TODO
        return varNode->evalRator(args);//if varNode,then we shouldn't judge it with the original number of arguments!!!
    }
    
    if (args.size() != clos_ptr->parameters.size()) throw RuntimeError("Wrong number of arguments");
    //TODO: TO COMPLETE THE PARAMETERS' ENVIRONMENT LOGIC
    Assoc param_env = clos_ptr->env;
    for(size_t i=0; i<args.size(); ++i) param_env = extend(clos_ptr->parameters[i],args[i],param_env);//bind args with parameters in new environment
    if(auto MultiExpr = dynamic_cast<Begin*>(clos_ptr->e.get())) return MultiExpr->eval(param_env);//multiple expressions,do them all like Begin does
    else return clos_ptr->e->eval(param_env);
}

Value Define::eval(Assoc &env) {
    //TODO: To complete the define logic
    //if(primitives.count(var) || reserved_words.count(var)) throw(RuntimeError("Invalid variable name in Define::eval"));
    Value matched_value = find(var,env);
    if(matched_value.get() == nullptr) env = extend(var,VoidV(),env);
    //a new element in env, namely recursion,so we create a Placeholder
    Value val = e->eval(env);//avoid mutual citation
    modify(var,val,env);//calculate
    return VoidV();
}

Value Let::eval(Assoc &env) {
    //TODO: To complete the let logic
    std::vector<Value> values;
    for(auto &binding:bind) values.push_back(binding.second->eval(env));
    Assoc new_env = env;
    for(size_t i=0; i<bind.size(); ++i) new_env = extend(bind[i].first,values[i],new_env);
    return body->eval(new_env);
}

Value Letrec::eval(Assoc &env)
{
    //TODO: To complete the letrec logic
    Assoc new_env = env;
    for(auto &binding:bind) new_env = extend(binding.first,VoidV(),new_env);//placeholder
    for(auto &binding:bind)
    {
        Value val = binding.second->eval(new_env);
        modify(binding.first, val,new_env);
    }
    return body->eval(new_env);
}

Value Set::eval(Assoc &env) {
    //TODO: To complete the set logic
    Value new_value = e->eval(env);
    Value loc = find(var,env);
    if(loc.get() == nullptr) throw(RuntimeError("Undefined variable : " + var));
    modify(var,new_value,env);
    return VoidV();
}

Value Display::evalRator(const Value &rand) { // display function
    if (rand->v_type == V_STRING) {
        String* str_ptr = dynamic_cast<String*>(rand.get());
        std::cout << str_ptr->s;
    } else {
        rand->show(std::cout);
    }
    
    return VoidV();
}
